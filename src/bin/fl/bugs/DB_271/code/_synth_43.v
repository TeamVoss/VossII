 module _synth_43 (
    input [1 : 0] i1,
    input i2,
    output [11 : 0] o1,
    output [1 : 0] o2,
    output o3,
    output [14 : 0] o4,
    output o5
 );
 m_3 inst_1(.i1(15'b000000000000000),
           .o1(o4[14:0]));
 m inst_2(.i1(1'b1),
         .o1(o5));
 m_2 inst_3(.i1({i1[1], i1[1], i1[1], i1[1], i1[1], i1[1], i1[1], i1[1:0], i1[1:0], i1[0]}),
           .o1(o1[11:0]));
 m_1 inst_4(.i1({1'b0, i2}),
           .o1(o2[1:0]));
 m inst_5(.i1(1'b0),
         .o1(o3));
 endmodule

 module m_3 (
    input [14 : 0] i1,
    output [14 : 0] o1
 );
 assign o1 = i1[14:0];
 endmodule

 module m_2 (
    input [11 : 0] i1,
    output [11 : 0] o1
 );
 assign o1 = i1[11:0];
 endmodule

 module m_1 (
    input [1 : 0] i1,
    output [1 : 0] o1
 );
 assign o1 = i1[1:0];
 endmodule

 module m (
    input i1,
    output o1
 );
 assign o1 = i1;
 endmodule

