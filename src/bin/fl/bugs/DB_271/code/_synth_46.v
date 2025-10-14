 module _synth_46 (
    input [1 : 0] i1,
    output [11 : 0] o1
 );
 m inst_1(.i1({i1[1], i1[1], i1[1], i1[1], i1[1], i1[1], i1[1], i1[1:0], i1[1:0], i1[0]}),
         .o1(o1[11:0]));
 endmodule

 module m (
    input [11 : 0] i1,
    output [11 : 0] o1
 );
 assign o1 = i1[11:0];
 endmodule

