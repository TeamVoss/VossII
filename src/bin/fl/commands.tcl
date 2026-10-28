# list of (name, usage, help) tripples
# ====================================

set ::command_help {


{add_open_overload {add_open_overload fn fn1 fn2 .... fnk;}
{Add new alternatives to the open overloaded identifier fn.}}

{begin_abstype {begin_abstype;} 
{Start an abstract data type. Only functions exported (in the end_abstype
command) will be visible outside the asbtract datatype.}}

{binder {binder foo;}
{Make foo a binder. As a result, foo a. E is parsed as foo (\a. E).
If multiple arguments are given, each will be bound. Thus:
foo a b c. E will be parsed as foo (\a. foo (\b. foo (\c. E))).}}

{binder_with_accumulator {binder_with_accumulator foo bar;}
{Make foo a binder with accumulator. As a result, foo a. E is parsed as
foo (\a.\bar. E) "a" bar.}}

{clear_fixities {clear_fixities;}
{Clear the fixities of every declared identifier.}}

{clet {clet foo a1 a2 ... an = E;}
{Declare a (non-recursive) function foo that takes n arguments.
The function is strict in all its arguments and the function is memoized.}}

{cletrec {cletrec foo a1 a2 ... an = E;}
{Declare a recursive function foo that takes n arguments.
The function is strict in all its arguments and the function is memoized.}}

{else_binder {else_binder ELSE;} {
    Introduce a function that will be parsed as the built-in "|" function.
    Usually not used since if_then_else binder is mostly used.
}}

{end_abstype {end_abstype fn1 fn2 ... fnn;}
{End the previously started abstract datatype declaration and export only
functions fn1, fn2, ..., fnn.}}

{export {export foo;}
{Export foo from the current active abstract data type (abstype).}}

{export_to_tcl {export_to_tcl foo;}
Make the fl function 'foo' available in tcl. }

{if_then_else_binder {if_then_else_binder THEN ELSE;} {
    Introduce two functions that will be parsed as the built-in "=>" and "|".
}}

{install_print_function {install_print_function fn;}
{If fn is a function of type t->string, then this command isntalls
a pretty printer for objects of type t.
Note that in order to pretty print large structures, the function may
actually use printf/fprintf stdout and only return an empty string.}}

{let {let fun arg1 arg2 ... argn = } {
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
    f [] 12;	    // Positional
    f n->12 d->[];  // Named arguments

Finally, arguments can be given default values. E.g., 
    let f l up->T = ....
    f [];	    // up will be T
    f [] F;	    // up will be F
    f up->F l->[]   // Same as the previous one, but with named arguments.

}}


{lettype {lettype typename = ...}
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

For conveneince, it is possible to name the arguments to the constructors.
Thus one could also have written:

lettype tree = BR {left::tree} {right::tree} | LF {value::int};

Currently these names are only used for .....

}}

{open_overload {open_overload fn fn1 ... fnk}
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

{overload {overload fn fn1 fn2 ... fnn;}
{Overload fn1, fn2, ..., fnn with the name fn.
As a result, fn is a closed overloaded identifier, which means there will
always only be these n alternatives.
Note that if you try to overload two overlapping functions, the type checker
may fail to resolve the usage. The system should warn for this, but does not
at the moment, so be aware!
}}

{then_binder {then_binder THEN;} {
    Introduce a function that will be parsed as the built-in "=>" function.
    Usually not used since if_then_else binder is mostly used.
}}

{val {val (a,b,c) = ....} {
Similar to the let command but take apart a composite result.
As with arguments to the let command, pattern-matching is possible
(in fact it is really the purpose of the construct).
}}

}

