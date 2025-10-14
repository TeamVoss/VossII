 module _synth_91 (
    input i1,
    input i2,
    output [1 : 0] o1
 );
 wire [1 : 0] m1;
 m_2 inst_1(.i1({1'b0, i2}),
           .o1(m1[1:0]));
 m inst_2(.i1(m1[1]),
         .i2(i1),
         .o1(o1[1]));
 m inst_3(.i1(m1[0]),
         .i2(i1),
         .o1(o1[0]));
 endmodule

 module m_2 (
    input [1 : 0] i1,
    output [1 : 0] o1
 );
 assign o1 = i1[1:0];
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

