# pexlif2verilog

- [Overview](#overview)
- [Compiling expressions](#compiling-expressions)
	- [Simplifying expressions](#simplifying-expressions)
	- [Translating to verilog](#translating-to-verilog)
- [Compiling update_fns](#compiling-update_fns)
- [Compiling pexlifs](#compiling-pexlifs)
	- [Precompiled pexlifs](#precompiled-pexlifs)
	- [Inlining pexlifs](#inlining-pexlifs)
	- [Simplifying variables](#simplifying-variables)
- [Generating code](#generating-code)
- [What doesn't work...](#what-doesnt-work)
- [What could be optimized](#what-could-be-optimized)

## Overview

The code is split into the following files (in vague order of inclusion).

- `ast.fl` - type definitions for `simple_wexpr`, `precompiled_pexlifs` and the verilog AST
- `utils.fl` - useful constants and utility functions (type size)
- `wexpr2swexpr.fl` - converts a `wexpr` into `simple_wexpr`
- `swexpr2vexpr.fl` - converts a `simple_wexpr` into a `VerilogExpression`
- `precompiled_pexlifs.fl` - verilog code for flip-flops and latches, tools to recognize pexlifs matching those constructs.
- `variables.fl` - tools for finding internal variables and replacing variables in statements
- `pexlif2verilog.fl` - brings it together, compiles a pexlif to a verilog file
- `verilog2string.fl` - prints a verilog file to valid code
- `main.fl` - the main function
- `tests.fl` - unit tests and tests for the whole compiler.


Most functions are documented, so running `main.fl` and using fl's help system can help shine light on unfortunate naming choices.

## Compiling expressions

There is a pretty good correspondance between wexpr and verilog expressions, so most of the compiling is fairly straitforward. However, a few construct don't translate well at all:

- Verilog only allows slicing of identifiers, not arbitrary expressions. We can't write `(a + b)[2:0]`.
	- On the right-hand side this can be solved by adding a new temporary variable (these are the `VV_Slice` variables)
	- On the left hand side we can transform the wexpr to avoid the problem: the only valid left hand side constructs are `W_VAR`, `W_EXPLICIT_VAR`, `W_CAT` and `W_SLICE`. So slices on the left-hand side only contains variables (which are fine), other slices or concatenations. With smart index manipulation we can merge nested slices into one and swap `W_SLICE ... (W_CAT ...)` into `W_CAT (W_SLICE ...)`. Doing this recursively ensures our left and side is in a correct form: an (eventually single element) concatenation of either variable or slices of variables. This is reflected in the type `VerilogLHS` used to represent left-hand sides.
- RAM operations are non-trivial:
	- `W_RAM_READ` can be represented by a slice (verilog allows slicing at a variable address as long as it is constant width). This is reprensented by the expression `VE_OffsetSlice {msb::VerilogExpression} {width::int} {var::VerilogVariable}`. It may also need to introduce a new slice variable (we again use `VV_Slice` here).
	- `W_RAM_WRITE` is harder, as it returns the new RAM. doing it in one expression would mean slicing an arbitrary width (from 0 to the address and from the address + 1 to the max address). We can thankfully go around this using blocking assignement in an always block:
		```Verilog
		always begin
			new_ram = old_ram;
			new_ram[addr -: offset] = new_data;
		end
		```
		This construct introduces a new variable for left hand side slicing (`VV_Ram`), and is represented by the statement `VS_Write_RAM {lhs::VerilogVariable} {ram::VerilogExpression {msb::VerilogExpression} {offset::int} {data::VerilogExpression}`.

### Simplifying expressions

The first step of compiling expression is to transform them into a simpler `simple_wexpr` type (often abbreviated `swexpr`). This is done in the file `wexpr2swexpr.fl`. The new types removes many redundent type constructs:

- `W_VAR` and `W_EXPLICIT_VAR` are unified in `SW_VAR`
- `W_CONST` and `W_NAMED_CONST` are unified in `SW_CONST`
- Binary operations are combined into a single `SW_BINOP` constructor taking the operation as argument.
- `W_ZX _ expr` is transformed into `SW_CAT [SW_CONST sz 0, expr]`
- `W_SX _ expr` is transformed into `SW_CAT [SW_SLICE msb expr, ... SW_SLICE msb expr, expr]`
- `W_SLICE` and `W_NAMED_SLICE` are unified in `SW_SLICE`
- `W_UPDATE_NAMED_SLICE` is changed into a concatenation of slices (from either the old expression or the new one). This is done by `transform_update_named_slice`.
- read and write ram are largely unchanged.

In addition, some important simplifications are done (by the mutually recursive `clean_slice` and `clean_cat` functions):
- Nested slices are transformed into a single slice.
- Nested concatenations are flattened.
- Slices of concatenation are changed into concatenations of slices
These simplification are only really needed for the left-hand side, but we do them all the time as they can reduce the number of slices of complex expressions.

This file also performs some simple optimisations. They aren't really needed and can be removed if buggy.
- slices of constants or X are known so precalculated (if not, this will create an extra variable to store the constant before slicing it)
- calculating operations between constants
- simplifying operations when one of the terms is absorbant/neutral (ex multiplication by 1 or 0)

### Translating to verilog

The next step is to transform simple_wexpr into VerilogLHS or VerilogExpression. This is dones in the file `swexpr2vexpr.fl`.


Compiling left-hand sides is pretty simple as it doesn't introduce any new variables/ The simplification to simple_wexpr ensure a valid left-hand side is either:
- a variable or a slice of a variable
- a concatenation of the above

So translating it is straigtforward. The function `swexpr2vlhs` takes an extra argument path which is used when inlining pexlifs (to avoid conflicts between callee and caller variables)

Compiling right hand side can introduce new variable (for slicing complex expressions or writing ram), so the `swexp2vexpr` function also takes a list of statement as arguments (they are used to check the number of variables introduced when allocating a new one), and returns a new list of statement (including the new variable assignements) as well as the compiled expression.

Thanks to the simplification this is also fairly easy to translate. Some non-trivial points:
- A slice of a variable can be kept as is, for more complex expression we need a new variable. To avoid creating to many of them (non-continous slices, SX and swapping slice and concatenations tend to introduced many slices of the same expression), we first check if we haven't aldready got a statement allocating this very same variable (in `get_or_create_slice_var`).
- As mentionned above, `SW_WRITE_RAM` is transformed into new a dedicated statement `VS_Write_RAM` with a new `VV_RAM` variable, and the new expression is simply the new variable).

## Compiling update_fns


This is done by `update_fn2vstmt` in the file `pexlif2verilog`.
- `W_UPDATE_FN` poses no problems at this point, compile the left-hand side to VerilogLHS and the right hand side to a VerilogExpression using the functions for compiling expression, and make a new assign statement out of them.
- `W_PHASE_DELAY` is untranslatable as is. There is no construct in (synthesizeble) verilog that mimics its behavior. The simple solution is to not translate it, instead rely on recognizing known flip-flops and latches and swithcing them out for precompiled code. This is done at the pexlif level.

  An old idea was to use twin-edge flip-flops to represent this, with a new clock signal. However it doesn't really have the right semantics and it adds some complexity, so it was scrapped (and the unifinished code removed in commit [1cb59c4efee5697d0e28d48fc62ed9d4a4a917d6](https://github.com/cjhseger/FP_HW/commit/1cb59c4efee5697d0e28d48fc62ed9d4a4a917d6)).

## Compiling pexlifs

This is done by two mutually recursive functions in the `pexlif2verilog.fl` file. We compile a pexlif into:

- a single main `VerilogModule`
- a list of other modules
- a list of required precompiled code (flip-flops...)

Essentially, with no inlining we try to compile each pexlif into its separate module. Leaf contain actual code while non-leaf just contain module instanciations (calls). Pexlifs are extremly nested so to avoid creating an indecent amount of them we can inline them significantly.

The two functions take common arguments:

- the predicate determining which pexlifs will be inlined (`inline_pexlif`)
- the path to the current module (used when inlining, see below)
- the list of previously encountered modules (`modules`) (this allows to both ensure new modules have a unique name and avoids duplicate modules). The new updated version of this list is returned.
- the list of encountered precompiled strucutre (`includes`) which will be needed. The new updated version of this list is returned.

Essentially, we need two functions to compile an individual pexlif or a list of children
with information from the parent:

- `pexlif2verilog` compiles an individual pexlif into a VerilogModule.
	- Module input and outputs are generated from the pexlifs `fa_inps` and `fa_outs` list (THIS PART IS BUGGY/UNFINISHED!)
	- Module statements are generated by `update_fn2vstmt` for leafs, or by `iter_children` for non-leaf (in which case new modules/includes may also be generated)
	- The statements are cleaned up a bit by removing extra variable (see [Simplifying variables](#simplifying-variables))
	- Module internals are generated from the statements (as new variables can be added/old ones removed, we can't uses pexlif's `ints`)
	- The module name is set to main, as it is assumed to be the toplevel module.
- `iter_children` recursively runs through all the children of a pexlif. It needs to know which variables are defined in the parent (fa_inps, fa_outs, ints) to link them to their children (this is the `context` argument). For each child:
	- checks if it matches a precompiled pexlif (flip-flop, latch). If so, add the precompiled pexlif to the `include` list and return the matching module instanciation as statement
	- else compile it with `pexlif2verilog`. If it should be inlined, grab it's statements and return them (adding new assign statements for i/o). If not, find a name for it, add it to the list of modules, and create a matching module instanciation statement.

### Precompiled pexlifs

The best way to avoid dealing with `W_PHASE_DELAY` is to recognize structures that use them and precompile them. To that end the compiler can recongnize a few common pexlifs (assuming they are build using hfl's builtin functions) : re_ff, re_ff_en, re_ff_reset, re_ff_en_reset, fe_ff, ah_latch, al_latch. They are listed in the enumerative type `precompiled_pexlifs`. The code that handles them is found in `precompiled_pexlifs.fl`

Recognition is pretty basic (maybe improvable ?), it is done by the `matches_precompiled` function. Right now this only checks the pexlifs name, as well as its inputs and outputs (name and order). This is because most of these structures are overloaded depending on i/o size, so direct comparaison fails.

Precompiled structures are not representable in the small subset of verilog's AST I coded, so their code is stored directly as a string. When compiling, we just keep a list of which precompiled_pexlifs to includes.

Verilog allows parameters (basically like a C++ int templates) for its modules, so this is what we use to overload in size. This is why the `VS_Module_Instanciation` statement has a parameter field while the type defining modules, `VerilogModule` does not.

### Inlining pexlifs

Compiling each pexlif to an individual module can work, but pexlifs are usually very nested, which leads to creating a LOT of modules, most of which only contain a single call to another module. Inlining pexlif can significantly shorten the generated code. There is an HFL function `gen_flatten_pexlif` which does pretty much that, but it returns a pexlif. And pexlif can't contain both `update_fn` and child pexlifs, while our code can merge assign statements and module instanciation statements in a single module.

Deciding which pexlif to inline is left as an input to the final function, but I recommend using the `inline_pexlif` predicate which inlines every pexlif whose name doesn't start with `draw_hier`. As this instruction is used to draw submodules when rendering pexlifs, it seems a logical choice of where to stop inlining.

In order to avoid name conflicts, the `path` argument is used whenever inlining. It is a list of numbers representing the path from the parent (non-inlined) pexlif to the current pexlif. So it is empty for non inlined pexlif and the topmost pexlifs, if is `[1]` for the first child, `[2,1]` for the second child of the first child (assuming they are being inlined). This path is added to variables and module instances as they are compiled. Variable prefix ensures no conflict can occur:

- if no path, the prefix is `v_`
- with a path `[2,1]`, the prefix is `vi1_i2__`

When inlining we also add a bunch of assign statements to set/get input outputs, so inling the pexlif `PINST _ _ _ [("a", ["a_input"])] [("b", ["b_output"])] _ _` will yiel something like

```Verilog
assign vi1_a = v_a_input;
// statements from inlined pexlif go here
assign v_b_output = v1i_b;
```

However, most of these can be simplified.

### Simplifying variables

Both our inlining and pexlif tend to introduce many temporary variables which bloat the code. `pexlif2verilog` can remove a lot of them by parsing the list of statements. If it finds any simple assignements `VS_Assign a_variable another_variable` where `a_variable` or `another_variable` is not an input/output, it can replace one with the other and remove this statement (this is done by `remove_redundant` in the `variables.fl` file)

This is pretty basic and could be pushed further. For example to replace `VS_Assign a_variable a_constant`, but we run into the problem of slices again. One idea I had is to replace single-use variable (one that appear only once on the lhs and once (not in a slice) on the rhs).

This simplification can be turned of with the constant `SIMPLE_VAR_INLINING` in `utils.fl`

## Generating code

This is done in the file `verilog2string.fl`. There shouldn't be too many surprises here. One thing I've added is operator precedence and associativity to avoid placing parentheses where they aren't needed.

We also need to check for invalid characters in variable/module names. Thankfully, verilog allows any string that doesn't include spaces to be a variable. If it's not a valid identifier, it must be preceded with `\` and end with a space.

## What doesn't work...

The big problem I haven't had time to fix is around parsing the pexlif input and outpu list `fa_inps` and `fa_outs`. This is done by `var_assignement` in the file `pexlif2verilog`. While it works on all simple cases, it fails for

- multidimensional vectors (RAM) : this actually crashes the program... The convention I've tried to used for RAM is to flatten it to one dimension, but it is complicated in this case because in order to do that, you need to know the size of all dimensions (which isn't in `fa_inps`, and isn't in the `context` I added either (it only has the full size))
- slices not starting at zero/non continous slices : this will compile incorrectly. `[("a[5:3]", ["a[5:3]"])]` creates a variable of width 3 for the inner pexlif, but then tries to take bits 5-3 which are out of bounds.
- non continous slices are even worse: `[("a[5:3]", ["a[5:3]"]), ("a[8:6]", ["a[8:6]"])]`...

I'm not quite sure how to fix any of these... For problem 2 you can offset by the correct amount but for problem 3 you may have to create a new concatenation for the variable (and fill in the missing bits with ???).

## What could be optimized

The compiler isn't efficient in any means, as it runs through the expression, statements and module lists multiple times, so it might be slow on large pexlifs.

As for the generated code, I think more variables could be removed (in `remove_reundant`) by recognizing more complex assignement (constants, slices), and/or by finding single use variables (assigned once, used once (not in a slice)).
