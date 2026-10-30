# list of (name, usage, help) tripples
# ====================================

set ::command_help {

{IO {} {}}

{{  install_print_function} {install_print_function fn;}
{If fn is a function of type t->string, then this command isntalls
a pretty printer for objects of type t.
Note that in order to pretty print large structures, the function may
actually use printf/fprintf stdout and only return an empty string.}}

{Overloading {} {}}

{{  overload} {overload fn fn1 fn2 ... fnn;}
{Overload fn1, fn2, ..., fnn with the name fn.
As a result, fn is a closed overloaded identifier, which means there will
always only be these n alternatives.
Note that if you try to overload two overlapping functions, the type checker
may fail to resolve the usage. The system should warn for this, but does not
at the moment, so be aware!
}}

{{  open_overload} {open_overload fn fn1 ... fnk}
{Start an open overloading on sysmbol fn with initial choices fn1, ..., fnk.
Later new alternatives can be added.
An open overloaded identifier behaves exactly the same when evaluating
top-level ground terms (i.e., non-functions). However, when defining
a function, an open overloaded identifier is never resolved and is left
unresolved. Only later when this new function is used in a top-level
ground term, will the resolution take place.
To aid the type checker, it is common to give the fn function a most
general type. For example, if you wanted to overload the + function, you
might say 

open_overload { + :: *a -> *a -> *a} i_add f_add;

That way any new addition to the overloading of + must be of such type.
}}

{{  add_open_overload} {add_open_overload fn fn1 fn2 .... fnk;}
{Add new alternatives to the open overloaded identifier fn.}}


{{Visibility} {} {}}

{{  begin_abstype} {begin_abstype;} 
{Start an abstract data type. Only functions exported
(either by explicit export commands or in the end_abstype command)
will be visible outside the asbtract datatype.}}

{{  export} {export foo;}
{Export foo from the current active abstract data type (abstype).}}

{{  end_abstype} {end_abstype fn1 fn2 ... fnn;}
{End the previously started abstract datatype declaration export
functions fn1, fn2, ..., fnn.
Other functions might also be exported if the export command was used for them.
}}

{{tcl-fl interface} {} {}}

{{  export_to_tcl} {export_to_tcl foo;}
Make the fl function 'foo' available in tcl. }

{{  import_from_tcl} {import_from_tcl {foo:: string # int -> int};} {
Make a function defined in tcl ( a proc) become directly callable from fl.
For this to work, the function has to be given its type signature.
Note that VossII allows opaque types whihc basically means fl datatypes
can be sent to tcl and be used in tcl to call back fl functions.
Of course, an opaque type cannot be taken apart or understood inside tcl.
As an example of using export_to_tcl and import_from_tcl, the following
(somewhat contrieved) example is illustrative.


# In a tcl file loaded into fl (e.g., by issuing tcl_eval ["source file.tcl"];)
proc my_tcl_fun {t1 t2} {
    if [expr [tr_size $t1] < [tr_size $t2]] {
        merge_tree $t2 $t1
    } else {
        merge_tree $t1 $t2 
    }
}

// Now you can do
lettype tr = BR tr tr | LF int;     // Declare a tree type

// Size function
letrec tr_size (BR t1 t2) = tr_size t1 + tr_size t2
 /\    tr_size (LF i) = 1
;
export_to_tcl tr_size;  // Make it available in tcl

// Tree merge function
let merge_tree t1 t2 = BR t1 t2;
export_to_tcl merge_tree;   // Make it available in tcl


// Now import the tcl function.
// Note that the type tr is an opaque type in tcl so
// it can only be passed around.
import_from_tcl {my_tcl_fun :: tr -> tr -> tr};

// A simple example:
let tree1 = BR (BR (LF 1) (LF 2)) (LF 3);
let tree2 = LF 4;

// Call the tcl function from fl.
my_tcl_fun tree2 tree1;

}}

{{Memoization} {} {}}

{{  clet} {clet foo a1 a2 ... an = E;}
{Declare a (non-recursive) function foo that takes n arguments.
The function is strict in all its arguments and the function is memoized.}}

{{  cletrec} {cletrec foo a1 a2 ... an = E;}
{Declare a recursive function foo that takes n arguments.
The function is strict in all its arguments and the function is memoized.}}

{{Top-level declarations} {} {}}

{{  let} {let fun arg1 arg2 ... argn = } {
Declare a function of n arguments.
There are a large collection of possible versions of this command.

First, the function can be type cast. E.g., 
  let {f::int list -> int} l = ...

Second, every argument can also be type cast. E.g.,
  let f {x::int list} = ....

Third, pattern-matching can be used. E.g., 
    let f (BR t1 t2) = ....

Fourth, several alternatives can be given with differnt patterns.
Semantically, each pattern is tried until one matches. E.g., 
  let f (BR t1 t2) = ....
  /\  f (LF i) = ....
  ;

If pattern-matching is insufficient, a general predicate can be used. E.g., 
  let f x y assuming (x > 2*y) =...

A function is called by its name followed by some arguments. 
If not enough arguments is given, the result is in itself a function.

If a function is defined with simple argument names, it can be used
(called) with named association. E.g., 
    let f d n = ....
    f [] 12;        // Positional
    f n->12 d->[];  // Named arguments

Finally, arguments can be given default values. E.g., 
    let f l up->T = ....
    f [];           // up will be T
    f [] F;         // up will be F
    f up->F l->[]   // Same as the previous one, but with named arguments.

}}

{{  val} {val (a,b,c) = ....} {
Similar to the let command but take apart a composite result.
As with arguments to the let command, pattern-matching is possible
(in fact it is really the purpose of the construct).
}}

{{  forward_declare} {forward_declare {foo::....};} {
Create a "hole" for a function that will be declared later.
This is an efficient way of creating mutually recursive functions.
A simple (somewhat silly) example could be:

lettype b_expr =     B_TRUE
                   | B_NOT b_expr
                   | B_AND b_expr b_expr
                   | B_GEQ i_expr i_expr
                   | B_EQ i_expr i_expr
//
andlettype i_expr =  I_CONST int
                   | I_ADD i_expr i_expr
                   | I_SUB i_expr i_expr
                   | I_ITE b_expr i_expr i_expr
;

forward_declare {bvalue :: b_expr -> bool};
forward_declare {ivalue :: i_expr -> int};

let ivalue (I_CONST i)   = i
/\  ivalue (I_ADD i1 i2) = ivalue i1 + ivalue i2
/\  ivalue (I_SUB i1 i2) = ivalue i1 - ivalue i2
/\  ivalue (I_ITE c t e) = (bvalue c) => ivalue t | ivalue e
;

let bvalue (B_TRUE)      = T
/\  bvalue (B_NOT b)     = NOT (bvalue b)
/\  bvalue (B_AND b1 b2) = bvalue b1 AND bvalue b2
/\  bvalue (B_GEQ i1 i2) = ivalue i1 >= ivalue i2
/\  bvalue (B_EQ i1 i2)  = ivalue i1 >= ivalue i2
;

let e1 = I_ADD (I_CONST 2) (I_CONST 1);
let e2 = I_SUB (I_CONST 3) (I_CONST 2);
let e3 = I_ITE (B_GEQ e1 e2) (I_CONST 7) (I_CONST 2);

ivalue e3;
}}

{{  non_lazy} {non_lazy foo;} {
Declare the function foo to be a non-lazy function. 
Effectively, this means that when the function is used, it will
(implicitly) depend on all surrounding function arguments.
}}

{{  lettype} {lettype typename = ...}
{Declare a new algebraic data type.
For example:

lettype tree = BR tree tree | LF int;

declares a new type (tree) with two constructors BR and LF.
The BR constructor takes two arguments of type tree and return a tree
and LF takes an integer and returns a tree.
In addition to declare these constructors, lettype also creates destructors
so that pattern-matching can be written. For eaxmple, one migth define

letrec tree_sum (BR t1 t2) = tree_sum t1 + tree_sum t2
  /\   tree_sum (LF i) = i
;

could be used to add up all the integer values in the leaves.

In addition to the constructor/destructor functions created, a read_tree
and write_tree functions are created that allows a tree to be saved to a
file and then read back in.

If some mutually exclusive types needs to be created, the keyword andlettype
can be used. For example, 

lettype b_expr =     B_TRUE
                   | B_NOT b_expr
                   | B_AND b_expr b_expr
                   | B_GEQ i_expr i_expr
                   | B_EQ i_expr i_expr
//
andlettype i_expr =  I_CONST int
                   | I_ADD i_expr i_expr
                   | I_SUB i_expr i_expr
                   | I_ITE b_expr i_expr i_expr
;

Finally, for conveneince, it is possible to name the arguments to
the constructors.  Thus one could also have written:

lettype tree = BR {left::tree} {right::tree} | LF {value::int};

Currently these names are only used for documentation.
}}

{{Parsing} {} {}}

{{  binder} {binder foo;}
{Make foo a binder. As a result, foo a. E is parsed as foo (\a. E).
If multiple arguments are given, each will be bound. Thus:
foo a b c. E will be parsed as foo (\a. foo (\b. foo (\c. E))).}}

{{  binder_with_accumulator} {binder_with_accumulator foo bar;}
{Make foo a binder with accumulator. As a result, foo a. E is parsed as
foo (\a.\bar. E) "a" bar.}}


{{  if_then_else_binder} {if_then_else_binder THEN ELSE;} {
    Introduce two functions that will be parsed as the built-in "=>" and "|".
}}

{{  else_binder} {else_binder ELSE;} {
    Introduce a function that will be parsed as the built-in "|" function.
    Usually not used since if_then_else binder is mostly used.
}}

{{  then_binder} {then_binder THEN;} {
    Introduce a function that will be parsed as the built-in "=>" function.
    Usually not used since if_then_else binder is mostly used.
}}


{{  free_binder} {free_binder foo;}
{Make foo a free binder. As a result, foo s is parsed as foo "s",
i.e., the identifier will be translated into a string.
An example might be:

lettype expr = E_CONST int | E_VAR string;
let Pexpr (E_CONST i) = sprintf "%d" i
 /\ Pexpr (E_VAR name) = sprintf "\"%s\"" name
;
install_print_function Pexpr;

let ' s =
    let is_char c = 
        let ci = ord c in
        ord "a" <= ci AND ci <= ord "z" OR
        ord "A" <= ci AND ci <= ord "Z" 
    in
    is_char (string_hd s) => E_VAR s | E_CONST (sscanf "%d" s)
;
free_binder ';

// Now you can type
'12;
'babc;
}}


{{  infix} {infix 4 foo;} {
Makes foo an infix operator wih precedence 4 that associates to the left.
The precedence can range from 1 to 9.
}}

{{  infixr} {infixr 4 foo;} {
Makes foo an infix operator wih precedence 4 that associates to the right.
The precedence can range from 1 to 9.
}}

{{  nonfix} {nonfix foo;} {
Remove the fixity previously assigned to foo.
}}

{{  prefix} {prefix <int> fun;} {
From now on, parse fun as a prefix operator with precedence <int>.
The precednce can only be 0 or 1.
An example of using it, consider the following code:

lettype b_expr =  E_VAR string
                | E_NOT b_expr
                | E_AND b_expr b_expr
                | E_FORALL string b_expr
;

letrec bvalue env (E_VAR name) = assoc name env
 /\    bvalue env (E_NOT e) = NOT (bvalue env e)
 /\    bvalue env (E_AND e1 e2) = (bvalue env e1) AND (bvalue env e2)
 /\    bvalue env (E_FORALL var b) =
                (bvalue ((var,F):env) b) AND (bvalue ((var,T):env) b)
;

let ~ e = E_NOT e;
prefix 0 ~;
let && a b = E_AND a b;
infix 4 &&;
let || a b = E_NOT (E_AND (E_NOT a) (E_NOT b));
infix 3 ||;
let forall fn s = E_FORALL s (fn (E_VAR s));
binder forall;
let thereis fn s = E_NOT (E_FORALL s (E_NOT (fn (E_VAR s))));
binder thereis;
let ^^ a b = ~a && ~b || a && b;
infix 4 ^^;

// You now can write:
let e = forall a b. thereis c. ~(a^^b) || ~(a^^(c^^b));
bvalue [] e;
}}

{{  posfix} {postfix fun;} {
From now on, parse fun as a postfix operator. E.g.,
    let hours h = 3600*h;
    postfix hours;
    let minutes m = 60*m;
    postfix minutes;
    // Now you can write
    let time = 6 hours + 2 minutes;
}}


{{  sized_binder_with_accumulator} {binder_with_accumulator foo bar;}
{Make foo a sized binder with accumulator.
As a result, if you ive the command:

    sized_binder_with_accumulator foo bar.
then 
    foo i a. E
will be parsed as
    foo i (\a.\bar. E) "a" bar.

NOte that multiple arguments can be used. Thus, as an example,
you could input: foo 12 a b c. E, which is equivalent to
    foo 12 a.
    foo 12 b.
    foo 12 c.
        E
}}


{{  clear_fixities} {clear_fixities;}
{Clear the fixities of every declared identifier.}}

{{  print_fixities} {print_fixities;} {
Print all currently active fixities.
}}

}

