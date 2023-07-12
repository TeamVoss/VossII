
pexlif format
=============

The pexlif datatype is used to represent circuits in VossII.
It is basically hierarchy + next-state functions.
Before going into details of the different fields, let start with
a simple example.
COnsider the follong HFL program:

load "idv.fl";
TYPE "byte" 8;

let inc =
    byte_input	i.
    byte_output	o.
    CELL "inc" [
	o <- i '+' '1
];

let test =
    byte_input	a b c.
    byte_output	res.
    byte_internal ap.
    CELL "test" [
	inc a ap,
	res <- ap '-' (b '*' c)
];

let p = flfun2pexlif test;
p;

The (somewhat pretty-printed) result of evaluating p is:

(PINST "test" [SHA->b2a4..,FP->6766..] F
  [(a[7:0],[a[7:0]]),(b[7:0],[b[7:0]]),(c[7:0],[c[7:0]])]
  [(res[7:0],[res[7:0]])]
  [ap[7:0],_$1[7:0]]
i1/:
    (PINST "inc" [SHA->c326..,FP->2111..] F
      [(i[7:0],[a[7:0]])]
      [(o[7:0],[ap[7:0]])]
      []
i1/i1/:
        (PINST "draw_unary_arithm {+1}" [SHA->4b74..,FP->6659..] T
          [(i1[7:0],[i[7:0]])]
          [(o[7:0],[o[7:0]])]
          []
          LEAF [
            o[7:0] <- (i1[7:0] + 0x01)
          ]
        )
    )
i2/:
    (PINST "draw_binary_arithm {*}" [SHA->12da..,FP->1243..] T
      [(i1[7:0],[b[7:0]]),(i2[7:0],[c[7:0]])]
      [(o[7:0],[_$1[7:0]])]
      []
      LEAF [
        o[7:0] <- (i1[7:0] * i2[7:0])
      ]
    )
i3/:
    (PINST "draw_binary_arithm {-}" [SHA->4837..,FP->1114..] T
      [(i1[7:0],[ap[7:0]]),(i2[7:0],[_$1[7:0]])]
      [(o[7:0],[res[7:0]])]
      []
      LEAF [
        o[7:0] <- (i1[7:0] - i2[7:0])
      ]
    )
)

Basically, p is a pexlif with a top-level hierarchy box, named "test", that
contains three instances, named "inc", "draw_binary_arithm {*}", and
"draw_binary_arithm {-}".
The instance "inc" contains only one instance, named "draw_unary_arithm {+1}"
which is a leaf instance in which the actual functionality is captured
(o[7:0] <- (i1[7:0] + 0x01)).

In more details, a PINST has a name, a list of attributes (some of which are
automatically computed!) a boolean signal denoting if the instance is a leaf
or a hierarchical instance.
Then each instance has two lists of (formal,actuals) pairs,
or inside-outside names, for the input connections and outputs connections
resepctively.  The formal (inside name) wire name is always a complete vector
(or singleton if it is of size 1) whereas the actuals (outside names) is
a list of signals and/or constants. So you might see something as
complicated as ("a[7:0}", ["0xf","d[7:6]","e[2:3"]) where the inside
wires a[7],a[6],...,a[0] are connected to 1, 1, 1, 1, d[7], d[6], e[2], e[3].
After the formal_actual input and output lists there is a list of internal
wire vectors. These are the "local" wires used to connect the subhierarchies.
Finally, there is either a P_HIER with a list of PINSTS denoting
more hiearchy, or a P_LEAF that contains either zero-delay next state
functions or phase delay functions.

