module mbuf1 (
    input wire [7 : 0] a,
    output wire [7 : 0] out
);
reg [7 : 0] mid;
m1_ m1_(.a(a[7:0]), .mid(mid[7:0]));
m2_ m2_(.mid(mid[7:0]), .out(out[7:0]));
endmodule
module m1_ (
    input wire [7 : 0] a,
    output wire [7 : 0] mid
);
mdraw_inverter1_1_ m1_1_(.a(a[7:0]), ._tmp(mid[7:0]));
endmodule
module mdraw_inverter1_1_ (
    input wire [7 : 0] a,
    output wire [7 : 0] _tmp
);
assign _tmp = ~(a[7:0]);
endmodule
module m2_ (
    input wire [7 : 0] mid,
    output wire [7 : 0] out
);
mdraw_inverter1_2_ m1_2_(.mid(mid[7:0]), ._tmp(out[7:0]));
endmodule
module mdraw_inverter1_2_ (
    input wire [7 : 0] mid,
    output wire [7 : 0] _tmp
);
assign _tmp = ~(mid[7:0]);
endmodule
