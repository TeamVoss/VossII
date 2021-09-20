module buf1 (
    input wire [7 : 0] a,
    input wire b,
    output wire [7 : 0] out
);
reg [7 : 0] mid;
m_1 anon_1(.b(b), .mid(mid[7:0]));
m_2 anon_2(.a(a[7:0]), .mid(mid[7:0]));
m_3 anon_3(.a(a[7:0]), .mid(mid[7:0]));
m_4 anon_4(.a(a[7:0]), .mid(mid[7:0]));
m_5 anon_5(.mid(mid[7:0]), .out(out[7:0]));
endmodule
module m_1 (
    input wire b,
    output wire [7 : 0] mid
);
draw_hfl_ZX_8_i1_1_1 anon_1_1(.b(b), ._tmp(mid[7:0]));
endmodule
module draw_hfl_ZX_8_i1_1_1 (
    input wire b,
    output wire [7 : 0] _tmp
);
assign _tmp = {{7{0}}, b};
endmodule
module m_2 (
    input wire [7 : 0] a,
    output wire [7 : 0] mid
);
reg [7 : 0] _TmP_1;
draw_input_2_2_1 anon_2_1(._tmp(_TmP_1[7:0]));
draw_binary_arithm__2_2 anon_2_2(.a(a[7:0]), ._TmP_1(_TmP_1[7:0]), ._tmp(mid[7:0]));
endmodule
module draw_input_2_2_1 ( output wire [7 : 0] _tmp
);
assign _tmp = 2;
endmodule
module draw_binary_arithm__2_2 (
    input wire [7 : 0] a,
    input wire [7 : 0] _TmP_1,
    output wire [7 : 0] _tmp
);
assign _tmp = (a[7:0]) << (_TmP_1[7:0]);
endmodule
module m_3 (
    input wire [7 : 0] a,
    output wire [7 : 0] mid
);
draw_unary_arithm_0_3_1 anon_3_1(.a(a[7:0]), ._tmp(mid[7:0]));
endmodule
module draw_unary_arithm_0_3_1 (
    input wire [7 : 0] a,
    output wire [7 : 0] _tmp
);
assign _tmp = (a[7:0]) + (0);
endmodule
module m_4 (
    input wire [7 : 0] a,
    output wire [7 : 0] mid
);
draw_inverter_4_1 anon_4_1(.a(a[7:0]), ._tmp(mid[7:0]));
endmodule
module draw_inverter_4_1 (
    input wire [7 : 0] a,
    output wire [7 : 0] _tmp
);
assign _tmp = ~(a[7:0]);
endmodule
module m_5 (
    input wire [7 : 0] mid,
    output wire [7 : 0] out
);
draw_inverter_5_1 anon_5_1(.mid(mid[7:0]), ._tmp(out[7:0]));
endmodule
module draw_inverter_5_1 (
    input wire [7 : 0] mid,
    output wire [7 : 0] _tmp
);
assign _tmp = ~(mid[7:0]);
endmodule
