//////////////////////////////////////////////////////////////////////
////                                                              ////
////  OR1200's Write-back Mux                                     ////
////                                                              ////
////  This file is part of the OpenRISC 1200 project              ////
////  http://www.opencores.org/cores/or1k/                        ////
////                                                              ////
////  Description                                                 ////
////  CPU's write-back stage of the pipeline                      ////
////                                                              ////
////  To Do:                                                      ////
////   - make it smaller and faster                               ////
////                                                              ////
////  Author(s):                                                  ////
////      - Damjan Lampret, lampret@opencores.org                 ////
////                                                              ////
//////////////////////////////////////////////////////////////////////
////                                                              ////
//// Copyright (C) 2000 Authors and OPENCORES.ORG                 ////
////                                                              ////
//// This source file may be used and distributed without         ////
//// restriction provided that this copyright statement is not    ////
//// removed from the file and that any derivative work contains  ////
//// the original copyright notice and the associated disclaimer. ////
////                                                              ////
//// This source file is free software; you can redistribute it   ////
//// and/or modify it under the terms of the GNU Lesser General   ////
//// Public License as published by the Free Software Foundation; ////
//// either version 2.1 of the License, or (at your option) any   ////
//// later version.                                               ////
////                                                              ////
//// This source is distributed in the hope that it will be       ////
//// useful, but WITHOUT ANY WARRANTY; without even the implied   ////
//// warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR      ////
//// PURPOSE.  See the GNU Lesser General Public License for more ////
//// details.                                                     ////
////                                                              ////
//// You should have received a copy of the GNU Lesser General    ////
//// Public License along with this source; if not, download it   ////
//// from http://www.opencores.org/lgpl.shtml                     ////
////                                                              ////
//////////////////////////////////////////////////////////////////////
//
// CVS Revision History
//
// $Log: not supported by cvs2svn $
// Revision 1.2  2002/03/29 15:16:56  lampret
// Some of the warnings fixed.
//
// Revision 1.1  2002/01/03 08:16:15  lampret
// New prefixes for RTL files, prefixed module names. Updated cache controllers and MMUs.
//
// Revision 1.8  2001/10/21 17:57:16  lampret
// Removed params from generic_XX.v. Added translate_off/on in sprs.v and id.v. Removed spr_addr from dc.v and ic.v. Fixed CR+LF.
//
// Revision 1.7  2001/10/14 13:12:10  lampret
// MP3 version.
//
// Revision 1.1.1.1  2001/10/06 10:18:36  igorm
// no message
//
// Revision 1.2  2001/08/09 13:39:33  lampret
// Major clean-up.
//
// Revision 1.1  2001/07/20 00:46:23  lampret
// Development version of RTL. Libraries are missing.
//
//

// synopsys translate_off
`include "timescale.v"
// synopsys translate_on
`include "or1200_defines.v"

module or1200_wbmux_cm3(
		clk_i_cml_1,
		clk_i_cml_2,
		
	// Clock and reset
	clk, rst,

	// Internal i/f
	wb_freeze, rfwb_op,
	muxin_a, muxin_b, muxin_c, muxin_d,
	muxout, muxreg, muxreg_valid
);


input clk_i_cml_1;
input clk_i_cml_2;
reg  wb_freeze_cml_2;
reg [ 3 - 1 : 0 ] rfwb_op_cml_2;
reg [ 3 - 1 : 0 ] rfwb_op_cml_1;
reg [ 32 - 1 : 0 ] muxin_d_cml_1;
reg [ 32 - 1 : 0 ] muxout_cml_2;
reg [ 32 - 1 : 0 ] muxreg_cml_2;
reg [ 32 - 1 : 0 ] muxreg_cml_1;
reg  muxreg_valid_cml_2;
reg  muxreg_valid_cml_1;



parameter width = `OR1200_OPERAND_WIDTH;

//
// I/O
//

//
// Clock and reset
//
input				clk;
input				rst;

//
// Internal i/f
//
input				wb_freeze;
input	[`OR1200_RFWBOP_WIDTH-1:0]	rfwb_op;
input	[width-1:0]		muxin_a;
input	[width-1:0]		muxin_b;
input	[width-1:0]		muxin_c;
input	[width-1:0]		muxin_d;
output	[width-1:0]		muxout;
output	[width-1:0]		muxreg;
output				muxreg_valid;

//
// Internal wires and regs
//
reg	[width-1:0]		muxout;
reg	[width-1:0]		muxreg;
reg				muxreg_valid;

//
// Registered output from the write-back multiplexer
//

// SynEDA CoreMultiplier
// assignment(s): muxreg, muxreg_valid
// replace(s): wb_freeze, muxout, muxreg, rfwb_op, muxreg_valid
always @(posedge clk or posedge rst) begin
	if (rst) begin
		muxreg <= #1 32'd0;
		muxreg_valid <= #1 1'b0;
	end
	else begin  muxreg_valid <= muxreg_valid_cml_2; muxreg <= muxreg_cml_2; if (!wb_freeze_cml_2) begin
		muxreg <= #1 muxout_cml_2;
		muxreg_valid <= #1 rfwb_op_cml_2[0];
	end end
end

//
// Write-back multiplexer
//

// SynEDA CoreMultiplier
// assignment(s): muxout
// replace(s): rfwb_op, muxin_d
always @(muxin_a or muxin_b or muxin_c or muxin_d_cml_1 or rfwb_op_cml_1) begin
`ifdef OR1200_ADDITIONAL_SYNOPSYS_DIRECTIVES
	case(rfwb_op_cml_1[`OR1200_RFWBOP_WIDTH-1:1]) // synopsys parallel_case infer_mux
`else
	case(rfwb_op_cml_1[`OR1200_RFWBOP_WIDTH-1:1]) // synopsys parallel_case
`endif
		2'b00: muxout = muxin_a;
		2'b01: begin
			muxout = muxin_b;
`ifdef OR1200_VERBOSE
// synopsys translate_off
			$display("  WBMUX: muxin_b %h", muxin_b);
// synopsys translate_on
`endif
		end
		2'b10: begin
			muxout = muxin_c;
`ifdef OR1200_VERBOSE
// synopsys translate_off
			$display("  WBMUX: muxin_c %h", muxin_c);
// synopsys translate_on
`endif
		end
		2'b11: begin
			muxout = muxin_d_cml_1 + 32'h8;
`ifdef OR1200_VERBOSE
// synopsys translate_off
			$display("  WBMUX: muxin_d %h", muxin_d + 4'h8);
// synopsys translate_on
`endif
		end
	endcase
end


always @ (posedge clk_i_cml_1) begin
rfwb_op_cml_1 <= rfwb_op;
muxin_d_cml_1 <= muxin_d;
muxreg_cml_1 <= muxreg;
muxreg_valid_cml_1 <= muxreg_valid;
end
always @ (posedge clk_i_cml_2) begin
wb_freeze_cml_2 <= wb_freeze;
rfwb_op_cml_2 <= rfwb_op_cml_1;
muxout_cml_2 <= muxout;
muxreg_cml_2 <= muxreg_cml_1;
muxreg_valid_cml_2 <= muxreg_valid_cml_1;
end
endmodule

