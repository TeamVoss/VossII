module buf1 (
    input wire [7 : 0] a,
    input wire b,
    output wire [7 : 0] out
);
reg [7 : 0] mid;
m_1 anon_1(.a(a[7:0]), .mid(mid[7:0]));
m_2 anon_2(.b(b), .a(a[7:0]), .mid(mid[7:0]), .out(out[7:0]));
endmodule
module m_1 (
    input wire [7 : 0] a,
    output wire [7 : 0] mid
);
draw_hfl_ZX_8_i12_1_1 anon_1_1(.a(a[7:0]), ._tmp(mid[7:0]));
endmodule
module draw_hfl_ZX_8_i12_1_1 (
    input wire [7 : 0] a,
    output wire [7 : 0] _tmp
);
assign _tmp = {{7{0}}, a[2]};
endmodule
module m_2 (
    input wire b,
    input wire [7 : 0] a,
    input wire [7 : 0] mid,
    output wire [7 : 0] out
);
draw_ite_2_1 anon_2_1(.b(b), .a(a[7:0]), .mid(mid[7:0]), ._tmp(out[7:0]));
endmodule
module draw_ite_2_1 (
    input wire b,
    input wire [7 : 0] a,
    input wire [7 : 0] mid,
    output wire [7 : 0] _tmp
);
assign _tmp = b ? a[7:0] : mid[7:0];
endmodule
