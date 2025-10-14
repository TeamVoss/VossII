 module _synth_13 (
    input i1,
    input i2,
    input i3,
    input i4,
    input i5,
    input [1 : 0] i6,
    output o1
 );
 wire m7;
 wire m1;
 wire m2;
 wire m3;
 wire m4;
 wire m5;
 wire m6;
 m_5 inst_1(.i1(m3),
           .i2(m4),
           .o1(m1));
 m_4 inst_2(.i1(i6[1:0]),
           .o1(m2));
 m_3 inst_3(.i1(i6[1]),
           .o1(m3));
 m_2 inst_4(.i1(i4),
           .i2(i2),
           .o1(m6));
 m_2 inst_5(.i1(i5),
           .i2(i3),
           .o1(m5));
 m_2 inst_6(.i1(m6),
           .i2(m5),
           .o1(m4));
 m_1 inst_7(.i1(m7),
           .i2(i1),
           .o1(o1));
 m inst_8(.i1(m2),
         .i2(m5),
         .i3(m1),
         .o1(m7));
 endmodule

 module m_5 (
    input i1,
    input i2,
    output o1
 );
 assign o1 = i1 & i2;
 endmodule

 module m_4 (
    input [1 : 0] i1,
    output o1
 );
 assign o1 = i1[1:0] == 2'b11;
 endmodule

 module m_3 (
    input i1,
    output o1
 );
 assign o1 = ~(i1);
 endmodule

 module m_2 (
    input i1,
    input i2,
    output o1
 );
 assign o1 = i1 | i2;
 endmodule

 module m_1 (
    input wire i2,
    input wire i1,
    output reg o1
 );
 always @(posedge i2) o1 <= i1;
 endmodule

 module m (
    input i1,
    input i2,
    input i3,
    output o1
 );
 assign o1 = i1 ? i2 : i3;
 endmodule

