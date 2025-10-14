 module _synth_73 (
    input [35 : 0] i1,
    input [35 : 0] i2,
    input i3,
    output [35 : 0] o1
 );
 wire [35 : 0] m1;
 wire m2;
 wire [35 : 0] m3;
 wire [35 : 0] m4;
 m_3 inst_1(.i1(i1[35:0]),
           .i2(m3[35:0]),
           .o1(m4[35:0]));
 m_3 inst_2(.i1(m4[35:0]),
           .i2({35'b00000000000000000000000000000000000, m2}),
           .o1(o1[35:0]));
 m_2 inst_3(.i1(i2[35:0]),
           .o1(m1[35:0]));
 m_1 inst_4(.i1(i3),
           .o1(m2));
 m inst_5(.i1(i3),
         .i2(i2[35:0]),
         .i3(m1[35:0]),
         .o1(m3[35:0]));
 endmodule

 module m_3 (
    input [35 : 0] i1,
    input [35 : 0] i2,
    output [35 : 0] o1
 );
 assign o1 = i1[35:0] + i2[35:0];
 endmodule

 module m_2 (
    input [35 : 0] i1,
    output [35 : 0] o1
 );
 assign o1 = ~(i1[35:0]);
 endmodule

 module m_1 (
    input i1,
    output o1
 );
 assign o1 = ~(i1);
 endmodule

 module m (
    input i1,
    input [35 : 0] i2,
    input [35 : 0] i3,
    output [35 : 0] o1
 );
 assign o1 = i1 ? i2[35:0] : i3[35:0];
 endmodule

