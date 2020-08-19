//////////////////////////////////////////////////////////////////////
////                                                              ////
////  OR1200's Data Cache top level                               ////
////                                                              ////
////  This file is part of the OpenRISC 1200 project              ////
////  http://www.opencores.org/cores/or1k/                        ////
////                                                              ////
////  Description                                                 ////
////  Instantiation of all IC blocks.                             ////
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
// Revision 1.7.4.2  2003/12/09 11:46:48  simons
// Mbist nameing changed, Artisan ram instance signal names fixed, some synthesis waning fixed.
//
// Revision 1.7.4.1  2003/07/08 15:36:37  lampret
// Added embedded memory QMEM.
//
// Revision 1.7  2002/10/17 20:04:40  lampret
// Added BIST scan. Special VS RAMs need to be used to implement BIST.
//
// Revision 1.6  2002/03/29 15:16:55  lampret
// Some of the warnings fixed.
//
// Revision 1.5  2002/02/11 04:33:17  lampret
// Speed optimizations (removed duplicate _cyc_ and _stb_). Fixed D/IMMU cache-inhibit attr.
//
// Revision 1.4  2002/02/01 19:56:54  lampret
// Fixed combinational loops.
//
// Revision 1.3  2002/01/28 01:16:00  lampret
// Changed 'void' nop-ops instead of insn[0] to use insn[16]. Debug unit stalls the tick timer. Prepared new flag generation for add and and insns. Blocked DC/IC while they are turned off. Fixed I/D MMU SPRs layout except WAYs. TODO: smart IC invalidate, l.j 2 and TLB ways.
//
// Revision 1.2  2002/01/14 06:18:22  lampret
// Fixed mem2reg bug in FAST implementation. Updated debug unit to work with new genpc/if.
//
// Revision 1.1  2002/01/03 08:16:15  lampret
// New prefixes for RTL files, prefixed module names. Updated cache controllers and MMUs.
//
// Revision 1.10  2001/10/21 17:57:16  lampret
// Removed params from generic_XX.v. Added translate_off/on in sprs.v and id.v. Removed spr_addr from ic.v and ic.v. Fixed CR+LF.
//
// Revision 1.9  2001/10/14 13:12:09  lampret
// MP3 version.
//
// Revision 1.1.1.1  2001/10/06 10:18:35  igorm
// no message
//
// Revision 1.4  2001/08/13 03:36:20  lampret
// Added cfg regs. Moved all defines into one defines.v file. More cleanup.
//
// Revision 1.3  2001/08/09 13:39:33  lampret
// Major clean-up.
//
// Revision 1.2  2001/07/22 03:31:53  lampret
// Fixed RAM's oen bug. Cache bypass under development.
//
// Revision 1.1  2001/07/20 00:46:03  lampret
// Development version of RTL. Libraries are missing.
//
//

// synopsys translate_off
`include "timescale.v"
// synopsys translate_on
`include "or1200_defines.v"

//
// Data cache
//
module or1200_ic_top_cm4(
		clk_i_cml_1,
		clk_i_cml_2,
		clk_i_cml_3,
		cmls,
		
	// Rst, clk and clock control
	clk, rst,

	// External i/f
	icbiu_dat_o, icbiu_adr_o, icbiu_cyc_o, icbiu_stb_o, icbiu_we_o, icbiu_sel_o, icbiu_cab_o,
	icbiu_dat_i, icbiu_ack_i, icbiu_err_i,

	// Internal i/f
	ic_en,
	icqmem_adr_i, icqmem_cycstb_i, icqmem_ci_i,
	icqmem_sel_i, icqmem_tag_i,
	icqmem_dat_o, icqmem_ack_o, icqmem_rty_o, icqmem_err_o, icqmem_tag_o,

`ifdef OR1200_BIST
	// RAM BIST
	mbist_si_i, mbist_so_o, mbist_ctrl_i,
`endif

	// SPRs
	spr_cs, spr_write, spr_dat_i
);


input clk_i_cml_1;
input clk_i_cml_2;
input clk_i_cml_3;
input [1:0] cmls;
reg  icbiu_ack_i_cml_1;
reg  ic_en_cml_3;
reg  ic_en_cml_2;
reg  ic_en_cml_1;
reg  icqmem_err_o_cml_3;
reg  icqmem_err_o_cml_2;
reg  icqmem_err_o_cml_1;
reg  spr_write_cml_3;
reg  spr_write_cml_2;
reg  spr_write_cml_1;
reg [ 31 : 0 ] spr_dat_i_cml_3;
reg [ 31 : 0 ] spr_dat_i_cml_2;
reg [ 31 : 0 ] spr_dat_i_cml_1;
reg  tag_v_cml_1;
reg [ 32 - 1 : 0 ] from_icram_cml_3;
reg [ 32 - 1 : 0 ] from_icram_cml_2;
reg [ 32 - 1 : 0 ] from_icram_cml_1;
reg [ 31 : 0 ] saved_addr_cml_3;
reg [ 31 : 0 ] saved_addr_cml_2;
reg [ 31 : 0 ] saved_addr_cml_1;
reg  icfsm_biu_read_cml_3;
reg  icfsm_first_miss_ack_cml_3;
reg  icfsm_first_miss_ack_cml_2;
reg  icfsm_first_miss_ack_cml_1;
reg  tag_comp_3_cml_1;
reg  tag_comp_2_cml_1;
reg  tag_comp_1_cml_1;
reg  tag_comp_0_cml_1;



parameter dw = `OR1200_OPERAND_WIDTH;

//
// I/O
//

//
// Clock and reset
//
input				clk;
input				rst;

//
// External I/F
//
output	[dw-1:0]		icbiu_dat_o;
output	[31:0]			icbiu_adr_o;
output				icbiu_cyc_o;
output				icbiu_stb_o;
output				icbiu_we_o;
output	[3:0]			icbiu_sel_o;
output				icbiu_cab_o;
input	[dw-1:0]		icbiu_dat_i;
input				icbiu_ack_i;
input				icbiu_err_i;

//
// Internal I/F
//
input				ic_en;
input	[31:0]			icqmem_adr_i;
input				icqmem_cycstb_i;
input				icqmem_ci_i;
input	[3:0]			icqmem_sel_i;
input	[3:0]			icqmem_tag_i;
output	[dw-1:0]		icqmem_dat_o;
output				icqmem_ack_o;
output				icqmem_rty_o;
output				icqmem_err_o;
output	[3:0]			icqmem_tag_o;

`ifdef OR1200_BIST
//
// RAM BIST
//
input mbist_si_i;
input [`OR1200_MBIST_CTRL_WIDTH - 1:0] mbist_ctrl_i;
output mbist_so_o;
`endif

//
// SPR access
//
input				spr_cs;
input				spr_write;
input	[31:0]			spr_dat_i;

//
// Internal wires and regs
//
wire				tag_v;
wire	[`OR1200_ICTAG_W-2:0]	tag;
wire	[dw-1:0]		to_icram;
wire	[dw-1:0]		from_icram;
wire	[31:0]			saved_addr;
wire	[3:0]			icram_we;
wire				ictag_we;
wire	[31:0]			ic_addr;
wire				icfsm_biu_read;
reg				tagcomp_miss;
wire	[`OR1200_ICINDXH:`OR1200_ICLS]	ictag_addr;
wire				ictag_en;
wire				ictag_v; 
wire				ic_inv;
wire				icfsm_first_hit_ack;
wire				icfsm_first_miss_ack;
wire				icfsm_first_miss_err;
wire				icfsm_burst;
wire				icfsm_tag_we;
`ifdef OR1200_BIST
//
// RAM BIST
//
wire				mbist_ram_so;
wire				mbist_tag_so;
wire				mbist_ram_si = mbist_si_i;
wire				mbist_tag_si = mbist_ram_so;
assign				mbist_so_o = mbist_tag_so;
`endif

//
// Simple assignments
//
assign icbiu_adr_o = ic_addr;

// SynEDA CoreMultiplier
// assignment(s): ic_inv
// replace(s): spr_write
assign ic_inv = spr_cs & spr_write_cml_3;
assign ictag_we = icfsm_tag_we | ic_inv;

// SynEDA CoreMultiplier
// assignment(s): ictag_addr
// replace(s): spr_dat_i
assign ictag_addr = ic_inv ? spr_dat_i_cml_3[`OR1200_ICINDXH:`OR1200_ICLS] : ic_addr[`OR1200_ICINDXH:`OR1200_ICLS];

// SynEDA CoreMultiplier
// assignment(s): ictag_en
// replace(s): ic_en
assign ictag_en = ic_inv | ic_en_cml_3;
assign ictag_v = ~ic_inv;

//
// Data to BIU is from ICRAM when IC is enabled or from LSU when
// IC is disabled
//
assign icbiu_dat_o = 32'h00000000;

//
// Bypases of the IC when IC is disabled
//

// SynEDA CoreMultiplier
// assignment(s): icbiu_cyc_o
// replace(s): ic_en, icfsm_biu_read
assign icbiu_cyc_o = (ic_en_cml_3) ? icfsm_biu_read_cml_3 : icqmem_cycstb_i;

// SynEDA CoreMultiplier
// assignment(s): icbiu_stb_o
// replace(s): ic_en, icfsm_biu_read
assign icbiu_stb_o = (ic_en_cml_3) ? icfsm_biu_read_cml_3 : icqmem_cycstb_i;
assign icbiu_we_o = 1'b0;

// SynEDA CoreMultiplier
// assignment(s): icbiu_sel_o
// replace(s): ic_en, icfsm_biu_read
assign icbiu_sel_o = (ic_en_cml_3 & icfsm_biu_read_cml_3) ? 4'b1111 : icqmem_sel_i;

// SynEDA CoreMultiplier
// assignment(s): icbiu_cab_o
// replace(s): ic_en
assign icbiu_cab_o = (ic_en_cml_3) ? icfsm_burst : 1'b0;

// SynEDA CoreMultiplier
// assignment(s): icqmem_rty_o
// replace(s): icqmem_err_o
assign icqmem_rty_o = ~icqmem_ack_o & ~icqmem_err_o_cml_1;

// SynEDA CoreMultiplier
// assignment(s): icqmem_tag_o
// replace(s): icqmem_err_o
assign icqmem_tag_o = icqmem_err_o_cml_3 ? `OR1200_ITAG_BE : icqmem_tag_i;

//
// CPU normal and error termination
//

// SynEDA CoreMultiplier
// assignment(s): icqmem_ack_o
// replace(s): icbiu_ack_i, ic_en, icfsm_first_miss_ack
assign icqmem_ack_o = ic_en_cml_1 ? (icfsm_first_hit_ack | icfsm_first_miss_ack_cml_1) : icbiu_ack_i_cml_1;
assign icqmem_err_o = ic_en ? icfsm_first_miss_err : icbiu_err_i;

//
// Select between claddr generated by IC FSM and addr[3:2] generated by LSU
//

// SynEDA CoreMultiplier
// assignment(s): ic_addr
// replace(s): saved_addr, icfsm_biu_read
assign ic_addr = (icfsm_biu_read_cml_3) ? saved_addr_cml_3 : icqmem_adr_i;

//
// Select between input data generated by LSU or by BIU
//
assign to_icram = icbiu_dat_i;

//
// Select between data generated by ICRAM or passed by BIU
//

// SynEDA CoreMultiplier
// assignment(s): icqmem_dat_o
// replace(s): ic_en, from_icram, icfsm_first_miss_ack
assign icqmem_dat_o = icfsm_first_miss_ack_cml_3 | !ic_en_cml_3 ? icbiu_dat_i : from_icram_cml_3;

//
// Tag comparison
//
wire	tag_comp_3;
wire	tag_comp_2;
wire	tag_comp_1;
wire	tag_comp_0;

assign tag_comp_3 = (tag[`OR1200_ICTAG_W-2:15] != saved_addr[31:`OR1200_ICTAGL + 15]);
assign tag_comp_2 = (tag[14:10] != saved_addr[`OR1200_ICTAGL + 14:`OR1200_ICTAGL + 10]);
assign tag_comp_1 = (tag[9:5] != saved_addr[`OR1200_ICTAGL + 9:`OR1200_ICTAGL + 5]);
assign tag_comp_0 = (tag[4:0] != saved_addr[`OR1200_ICTAGL + 4: `OR1200_ICTAGL]);


// SynEDA CoreMultiplier
// assignment(s): tagcomp_miss
// replace(s): tag_v, tag_comp_3, tag_comp_2, tag_comp_1, tag_comp_0
always @(tag or saved_addr or tag_v_cml_1) begin
	//if ((tag != saved_addr[31:`OR1200_ICTAGL]) || !tag_v)
	if ((tag_comp_3_cml_1 | tag_comp_2_cml_1 | tag_comp_1_cml_1 | tag_comp_0_cml_1) || !tag_v_cml_1)
		tagcomp_miss = 1'b1;
	else
		tagcomp_miss = 1'b0;
end

//
// Instantiation of IC Finite State Machine
//
or1200_ic_fsm_cm4 or1200_ic_fsm(
		.clk_i_cml_1(clk_i_cml_1),
		.clk_i_cml_2(clk_i_cml_2),
		.clk_i_cml_3(clk_i_cml_3),
	.clk(clk),
	.rst(rst),
	.ic_en(ic_en),
	.icqmem_cycstb_i(icqmem_cycstb_i),
	.icqmem_ci_i(icqmem_ci_i),
	.tagcomp_miss(tagcomp_miss),
	.biudata_valid(icbiu_ack_i),
	.biudata_error(icbiu_err_i),
	.start_addr(icqmem_adr_i),
	.saved_addr(saved_addr),
	.icram_we(icram_we),
	.biu_read(icfsm_biu_read),
	.first_hit_ack(icfsm_first_hit_ack),
	.first_miss_ack(icfsm_first_miss_ack),
	.first_miss_err(icfsm_first_miss_err),
	.burst(icfsm_burst),
	.tag_we(icfsm_tag_we)
);

//
// Instantiation of IC main memory
//
wire [`OR1200_ICINDXH:2] addr_ic_ram;
assign addr_ic_ram = ic_addr[`OR1200_ICINDXH:2];
or1200_ic_ram_cm4 or1200_ic_ram(
		.clk_i_cml_1(clk_i_cml_1),
		.clk_i_cml_2(clk_i_cml_2),
		.clk_i_cml_3(clk_i_cml_3),
		.cmls(cmls),
	.clk(clk),
	.rst(rst),
`ifdef OR1200_BIST
	// RAM BIST
	.mbist_si_i(mbist_ram_si),
	.mbist_so_o(mbist_ram_so),
	.mbist_ctrl_i(mbist_ctrl_i),
`endif
	.addr(addr_ic_ram),
	.en(ic_en),
	.we(icram_we),
	.datain(to_icram),
	.dataout(from_icram)
);

//
// Instantiation of IC TAG memory
//
wire [31:`OR1200_ICTAGL - 1] ic_tag_datain;
assign ic_tag_datain = {ic_addr[31:`OR1200_ICTAGL], ictag_v};
or1200_ic_tag_cm4 or1200_ic_tag(
		.clk_i_cml_1(clk_i_cml_1),
		.clk_i_cml_2(clk_i_cml_2),
		.clk_i_cml_3(clk_i_cml_3),
		.cmls(cmls),
	.clk(clk),
	.rst(rst),
`ifdef OR1200_BIST
	// RAM BIST
	.mbist_si_i(mbist_tag_si),
	.mbist_so_o(mbist_tag_so),
	.mbist_ctrl_i(mbist_ctrl_i),
`endif
	.addr(ictag_addr),
	.en(ictag_en),
	.we(ictag_we),
	.datain(ic_tag_datain),
	.tag_v(tag_v),
	.tag(tag)
);


always @ (posedge clk_i_cml_1) begin
icbiu_ack_i_cml_1 <= icbiu_ack_i;
ic_en_cml_1 <= ic_en;
icqmem_err_o_cml_1 <= icqmem_err_o;
spr_write_cml_1 <= spr_write;
spr_dat_i_cml_1 <= spr_dat_i;
tag_v_cml_1 <= tag_v;
from_icram_cml_1 <= from_icram;
saved_addr_cml_1 <= saved_addr;
icfsm_first_miss_ack_cml_1 <= icfsm_first_miss_ack;
tag_comp_3_cml_1 <= tag_comp_3;
tag_comp_2_cml_1 <= tag_comp_2;
tag_comp_1_cml_1 <= tag_comp_1;
tag_comp_0_cml_1 <= tag_comp_0;
end
always @ (posedge clk_i_cml_2) begin
ic_en_cml_2 <= ic_en_cml_1;
icqmem_err_o_cml_2 <= icqmem_err_o_cml_1;
spr_write_cml_2 <= spr_write_cml_1;
spr_dat_i_cml_2 <= spr_dat_i_cml_1;
from_icram_cml_2 <= from_icram_cml_1;
saved_addr_cml_2 <= saved_addr_cml_1;
icfsm_first_miss_ack_cml_2 <= icfsm_first_miss_ack_cml_1;
end
always @ (posedge clk_i_cml_3) begin
ic_en_cml_3 <= ic_en_cml_2;
icqmem_err_o_cml_3 <= icqmem_err_o_cml_2;
spr_write_cml_3 <= spr_write_cml_2;
spr_dat_i_cml_3 <= spr_dat_i_cml_2;
from_icram_cml_3 <= from_icram_cml_2;
saved_addr_cml_3 <= saved_addr_cml_2;
icfsm_biu_read_cml_3 <= icfsm_biu_read;
icfsm_first_miss_ack_cml_3 <= icfsm_first_miss_ack_cml_2;
end
endmodule

