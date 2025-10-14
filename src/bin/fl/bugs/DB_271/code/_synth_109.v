 module _synth_109 (
    input [1 : 0] i1,
    output [9 : 0] o1
 );
 m inst_1(.i1({i1[1], i1[1], i1[1], i1[1], i1[1], i1[1], i1[1], i1[1:0], i1[0]}),
         .o1(o1[9:0]));
 endmodule

 module m (
    input [9 : 0] i1,
    output [9 : 0] o1
 );
 assign o1 = i1[9:0];
 endmodule

