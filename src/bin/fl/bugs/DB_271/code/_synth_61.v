 module _synth_61 (
    input [35 : 0] i1,
    input i2,
    output [35 : 0] o1
 );
 m inst_1(.i1(i1[35:0]),
         .i2({35'b00000000000000000000000000000000000, i2}),
         .o1(o1[35:0]));
 endmodule

 module m (
    input [35 : 0] i1,
    input [35 : 0] i2,
    output [35 : 0] o1
 );
 assign o1 = i1[35:0] + i2[35:0];
 endmodule

