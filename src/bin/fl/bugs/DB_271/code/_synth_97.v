 module _synth_97 (
    input i1,
    input [1 : 0] i2,
    output [11 : 0] o1
 );
 wire [11 : 0] m1;
 m_2 inst_1(.i1({i2[1], i2[1], i2[1], i2[1], i2[1], i2[1], i2[1], i2[1:0], i2[1:0], i2[0]}),
           .o1(m1[11:0]));
 m inst_2(.i1(m1[4]),
         .i2(i1),
         .o1(o1[4]));
 m inst_3(.i1(m1[3]),
         .i2(i1),
         .o1(o1[3]));
 m inst_4(.i1(m1[2]),
         .i2(i1),
         .o1(o1[2]));
 m inst_5(.i1(m1[1]),
         .i2(i1),
         .o1(o1[1]));
 m inst_6(.i1(m1[0]),
         .i2(i1),
         .o1(o1[0]));
 m inst_7(.i1(m1[11]),
         .i2(i1),
         .o1(o1[11]));
 m inst_8(.i1(m1[10]),
         .i2(i1),
         .o1(o1[10]));
 m inst_9(.i1(m1[9]),
         .i2(i1),
         .o1(o1[9]));
 m inst_10(.i1(m1[8]),
          .i2(i1),
          .o1(o1[8]));
 m inst_11(.i1(m1[7]),
          .i2(i1),
          .o1(o1[7]));
 m inst_12(.i1(m1[6]),
          .i2(i1),
          .o1(o1[6]));
 m inst_13(.i1(m1[5]),
          .i2(i1),
          .o1(o1[5]));
 endmodule

 module m_2 (
    input [11 : 0] i1,
    output [11 : 0] o1
 );
 assign o1 = i1[11:0];
 endmodule

 module m (
    input i1,
    input i2,
    output o1
 );
 m_1 inst_1(.i1(i1),
           .i2(i2),
           .o1(o1));
 endmodule

 module m_1 (
    input wire i2,
    input wire i1,
    output reg o1
 );
 always @(posedge i2) o1 <= i1;
 endmodule

