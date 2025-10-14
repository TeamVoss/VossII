 module _synth_37 (
    input i1,
    input i2,
    input [31 : 0] i3,
    input [33 : 0] i4,
    input [31 : 0] i5,
    input [36 : 0] i6,
    input i7,
    input i8,
    input [9 : 0] i9,
    input i10,
    input i11,
    input i12,
    input [1 : 0] i13,
    input i14,
    input i15,
    input i16,
    output [1 : 0] o1,
    output [31 : 0] o2,
    output [33 : 0] o3,
    output [31 : 0] o4,
    output [36 : 0] o5,
    output [1 : 0] o6,
    output [1 : 0] o7,
    output o8,
    output o9,
    output [9 : 0] o10,
    output o11,
    output o12,
    output o13
 );
 m_5 inst_1(.i1(i6[36:0]),
           .i2(i1),
           .o1(o5[36:0]));
 m_4 inst_2(.i1(i3[31:0]),
           .i2(i1),
           .o1(o2[31:0]));
 m inst_3(.i1(i8),
         .i2(i1),
         .o1(o9));
 m inst_4(.i1(i10),
         .i2(i1),
         .o1(o11));
 m inst_5(.i1(i12),
         .i2(i1),
         .o1(o13));
 m_1 inst_6(.i1(i13[1:0]),
           .i2(i1),
           .o1(o7[1:0]));
 m_1 inst_7(.i1({i16, i15}),
           .i2(i1),
           .o1(o6[1:0]));
 m_4 inst_8(.i1(i5[31:0]),
           .i2(i1),
           .o1(o4[31:0]));
 m_3 inst_9(.i1(i4[33:0]),
           .i2(i1),
           .o1(o3[33:0]));
 m_2 inst_10(.i1(i9[9:0]),
            .i2(i1),
            .o1(o10[9:0]));
 m_1 inst_11(.i1({i2, i14}),
            .i2(i1),
            .o1(o1[1:0]));
 m inst_12(.i1(i11),
          .i2(i1),
          .o1(o12));
 m inst_13(.i1(i7),
          .i2(i1),
          .o1(o8));
 endmodule

 module m_5 (
    input wire i2,
    input wire [36 : 0] i1,
    output reg [36 : 0] o1
 );
 always @(posedge i2) o1[36:0] <= i1[36:0];
 endmodule

 module m_4 (
    input wire i2,
    input wire [31 : 0] i1,
    output reg [31 : 0] o1
 );
 always @(posedge i2) o1[31:0] <= i1[31:0];
 endmodule

 module m_3 (
    input wire i2,
    input wire [33 : 0] i1,
    output reg [33 : 0] o1
 );
 always @(posedge i2) o1[33:0] <= i1[33:0];
 endmodule

 module m_2 (
    input wire i2,
    input wire [9 : 0] i1,
    output reg [9 : 0] o1
 );
 always @(posedge i2) o1[9:0] <= i1[9:0];
 endmodule

 module m_1 (
    input wire i2,
    input wire [1 : 0] i1,
    output reg [1 : 0] o1
 );
 always @(posedge i2) o1[1:0] <= i1[1:0];
 endmodule

 module m (
    input wire i2,
    input wire i1,
    output reg o1
 );
 always @(posedge i2) o1 <= i1;
 endmodule

