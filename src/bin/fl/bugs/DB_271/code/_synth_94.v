 module _synth_94 (
    input i1,
    output o1
 );
 wire m1;
 m_2 inst_1(.o1(m1));
 m inst_2(.i1(m1),
         .i2(i1),
         .o1(o1));
 endmodule

 module m_2 ( output o1
 );
 assign o1 = 1'b0;
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

