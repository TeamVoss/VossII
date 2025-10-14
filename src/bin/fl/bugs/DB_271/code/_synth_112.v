 module _synth_112 (
    input [7 : 0] i1,
    output [2 : 0] o1
 );
 wire m9;
 wire m1;
 wire m2;
 wire m3;
 wire m4;
 wire m5;
 wire m6;
 wire m7;
 wire m8;
 m_8 inst_1(.i1(m7),
           .i2(m6),
           .i3(m5),
           .i4(m4),
           .i5(m3),
           .i6(m2),
           .i7(m1),
           .i8(m9),
           .o1({o1[2], m8, o1[1:0]}));
 m_7 inst_2(.i1(i1[7:0]),
           .o1(m9));
 m_6 inst_3(.i1(i1[7:0]),
           .o1(m1));
 m_5 inst_4(.i1(i1[7:1]),
           .o1(m2));
 m_4 inst_5(.i1(i1[7:1]),
           .o1(m3));
 m_3 inst_6(.i1(i1[7:1]),
           .o1(m4));
 m_2 inst_7(.i1(i1[7:1]),
           .o1(m5));
 m_1 inst_8(.i1(i1[7:0]),
           .o1(m6));
 m inst_9(.i1(i1[7:0]),
         .o1(m7));
 endmodule

 module m_8 (
    input i1,
    input i2,
    input i3,
    input i4,
    input i5,
    input i6,
    input i7,
    input i8,
    output [3 : 0] o1
 );
 assign o1 = i1 ? 4'b1000 : i2 ? 4'b1001 : i3 ? 4'b1010 : i4 ? 4'b1011 : i5 ? 4'b1011 : i6 ? 4'b1011 : i7 ? 4'b1001 : i8 ? 4'b1100 : 4'b0000;
 endmodule

 module m_7 (
    input [7 : 0] i1,
    output o1
 );
 assign o1 = i1[7:0] == 8'b10111100;
 endmodule

 module m_6 (
    input [7 : 0] i1,
    output o1
 );
 assign o1 = i1[7:0] == 8'b10110010;
 endmodule

 module m_5 (
    input [6 : 0] i1,
    output o1
 );
 assign o1 = i1[6:0] == 7'b1001111;
 endmodule

 module m_4 (
    input [6 : 0] i1,
    output o1
 );
 assign o1 = i1[6:0] == 7'b1001101;
 endmodule

 module m_3 (
    input [6 : 0] i1,
    output o1
 );
 assign o1 = i1[6:0] == 7'b1001100;
 endmodule

 module m_2 (
    input [6 : 0] i1,
    output o1
 );
 assign o1 = i1[6:0] == 7'b1001000;
 endmodule

 module m_1 (
    input [7 : 0] i1,
    output o1
 );
 assign o1 = i1[7:0] == 8'b10110100;
 endmodule

 module m (
    input [7 : 0] i1,
    output o1
 );
 assign o1 = i1[7:0] == 8'b10110000;
 endmodule

