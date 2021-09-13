module draw_hier buf1 (
    input a,
    output out
);
wire mid ;
 m1(.a(a), .mid(mid))
 m2(.mid(mid), .out(out))
endmodule
module  (
    input a,
    output mid
);
draw_inverter m1(.a(a), ._tmp(mid))
endmodule
module draw_inverter (
    input a,
    output _tmp
);
assign _tmp = !a ;
endmodule
module  (
    input mid,
    output out
);
draw_inverter m1(.mid(mid), ._tmp(out))
endmodule
module draw_inverter (
    input mid,
    output _tmp
);
assign _tmp = !mid ;
endmodule
