//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/*
 *  yosys -- Yosys Open SYnthesis Suite
 *
 *  Copyright (C) 2012  Clifford Wolf <clifford@clifford.at>
 *
 *  Permission to use, copy, modify, and/or distribute this software for any
 *  purpose with or without fee is hereby granted, provided that the above
 *  copyright notice and this permission notice appear in all copies.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 *  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 *  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 *  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 *  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 *  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 *  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

#include "kernel/rtlil.h"
#include "kernel/register.h"
#include "kernel/sigtools.h"
#include "kernel/celltypes.h"
#include "kernel/log.h"
#include <string>

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

struct PexlifDumperConfig
{
	bool icells_mode;
	bool conn_mode;
	bool impltf_mode;
	bool gates_mode;
	bool cname_mode;
	bool param_mode;
	bool attr_mode;
	bool blackbox_mode;
	bool noalias_mode;

	std::string buf_type, buf_in, buf_out;
	std::map<RTLIL::IdString, std::pair<RTLIL::IdString, RTLIL::IdString>> unbuf_types;
	std::string true_type, true_out, false_type, false_out, undef_type, undef_out;

	PexlifDumperConfig() : icells_mode(false), conn_mode(false), impltf_mode(false), gates_mode(false),
			cname_mode(false), param_mode(false), attr_mode(false), blackbox_mode(false), noalias_mode(false) { }
};

struct PexlifDumper
{
	std::ostream &f;
	RTLIL::Module *module;
	RTLIL::Design *design;
	PexlifDumperConfig *config;
	CellTypes ct;

	SigMap sigmap;
	dict<SigBit, int> init_bits;

	PexlifDumper(std::ostream &f, RTLIL::Module *module, RTLIL::Design *design, PexlifDumperConfig *config) :
			f(f), module(module), design(design), config(config), ct(design), sigmap(module)
	{
		for (Wire *wire : module->wires())
			if (wire->attributes.count("\\init")) {
				SigSpec initsig = sigmap(wire);
				Const initval = wire->attributes.at("\\init");
				for (int i = 0; i < GetSize(initsig) && i < GetSize(initval); i++)
					switch (initval[i]) {
						case State::S0:
							init_bits[initsig[i]] = 0;
							break;
						case State::S1:
							init_bits[initsig[i]] = 1;
							break;
						default:
							break;
					}
			}
	}

	vector<shared_str> cstr_buf;
	pool<SigBit> cstr_bits_seen;

	bool is_ff_cell(RTLIL::IdString type)
	{
		return type.in(ID($dff), ID($dffe), ID($sdff), ID($sdffe), ID($sdffce), ID($adff), ID($adffe),
			       ID($dffsr), ID($dffsre), ID($dlatch), ID($adlatch), ID($dlatchsr), ID($sr));
	}


	const char *cstr(RTLIL::IdString id)
	{
		std::string str = RTLIL::unescape_id(id);
		if( str.substr(0,8) == "$paramod" ) {
		    str = str.substr(9);
		}
		for (size_t i = 0; i < str.size(); i++)
			if (str[i] == '\\' || str[i] == '#' || str[i] == '=' || str[i] == '<' || str[i] == '>')
				str[i] = '_';
		cstr_buf.push_back(str);
		return cstr_buf.back().c_str();
	}

	const char *cstr(RTLIL::SigBit sig)
	{
		cstr_bits_seen.insert(sig);

		if (sig.wire == NULL) {
			if (sig == RTLIL::State::S0) return config->false_type == "-" || config->false_type == "+" ? config->false_out.c_str() : "0";
			if (sig == RTLIL::State::S1) return config->true_type == "-" || config->true_type == "+" ? config->true_out.c_str() : "1";
			return config->undef_type == "-" || config->undef_type == "+" ? config->undef_out.c_str() : "x";
		}

		std::string str = RTLIL::unescape_id(sig.wire->name);
		for (size_t i = 0; i < str.size(); i++)
			if (str[i] == '#' || str[i] == '=' || str[i] == '<' || str[i] == '>')
				str[i] = '?';

		if (sig.wire->width != 1)
			str += stringf("[%d]", sig.wire->upto ? sig.wire->start_offset+sig.wire->width-sig.offset-1 : sig.wire->start_offset+sig.offset);

		cstr_buf.push_back(str);
		return cstr_buf.back().c_str();
	}

	const char *cstr_init(RTLIL::SigBit sig)
	{
		sigmap.apply(sig);

		if (init_bits.count(sig) == 0)
			return " 2";

		string str = stringf(" %d", init_bits.at(sig));

		cstr_buf.push_back(str);
		return cstr_buf.back().c_str();
	}

	const char *subckt_or_gate(std::string cell_type)
	{
		if (!config->gates_mode)
			return "subckt";
		if (!design->modules_.count(RTLIL::escape_id(cell_type)))
			return "gate";
		if (design->modules_.at(RTLIL::escape_id(cell_type))->get_bool_attribute("\\blackbox"))
			return "gate";
		return "subckt";
	}

	void dump_const(std::ostream &f, const RTLIL::Const &data, int width, int offset)
	{
		if (width < 0)
			width = data.bits.size() - offset;
		if ((data.flags & RTLIL::CONST_FLAG_STRING) == 0 || width != (int)data.bits.size()) {
			f << stringf("\"0b");
			for (int i = offset+width-1; i >= offset; i--) {
				log_assert(i < (int)data.bits.size());
				switch (data.bits[i]) {
				case RTLIL::S0: f << stringf("0"); break;
				case RTLIL::S1: f << stringf("1"); break;
				case RTLIL::Sx: f << stringf("x"); break; 
				case RTLIL::Sz: f << stringf("z"); break;
				case RTLIL::Sa: f << stringf("-"); break; 
				case RTLIL::Sm: f << stringf("m"); break;
				}
			}
			f << stringf("\"");
		} else {
			f << stringf("\"");
			std::string str = data.decode_string(); 
			for (size_t i = 0; i < str.size(); i++) {
				if (str[i] == '\n')
					f << stringf("\\n");
				else if (str[i] == '\t')
					f << stringf("\\t");
				else if (str[i] < 32)
					f << stringf("\\%03o", str[i]);
				else if (str[i] == '"')
					f << stringf("\\\"");
				else if (str[i] == '\\')
					f << stringf("\\\\");
				else
					f << str[i];
			}
			f << stringf("\"");
		}
	}

	void dump_sigchunk(std::ostream &f, const RTLIL::SigChunk &chunk)
	{
		if (chunk.wire == NULL) {
			dump_const(f, chunk.data, chunk.width, chunk.offset);
		} else {
			const char *name = chunk.wire->name.c_str();
			if( *name == '\\' ) name++;
			if (chunk.width == chunk.wire->width && chunk.offset == 0) {
				if( chunk.width == 1 ) {
				    f << stringf("\"%s\"", name);
				} else {
				    f << stringf("\"%s[%d:0]\"", name, chunk.width-1);
				}
			} else if (chunk.width == 1) 
				f << stringf("\"%s[%d]\"", name, chunk.offset);
			else
				f << stringf("\"%s[%d:%d]\"", name, chunk.offset+chunk.width-1, chunk.offset);
		}
	}

	void dump_sigspec(std::ostream &f, const RTLIL::SigSpec &sig)
	{
		f << stringf("[");
		if (sig.is_chunk()) {
			dump_sigchunk(f, sig.as_chunk());
		} else {
			bool first = true;
			for (auto it = sig.chunks().rbegin(); it != sig.chunks().rend(); ++it) {
				if( !first )
				    f << stringf(", ");
				first = false;
				dump_sigchunk(f, *it);
			}
		}
		f << stringf("]");
	}

	void dump_params(const char *command, dict<IdString, Const> &params)
	{
		for (auto &param : params) {
			f << stringf("%s %s ", command, RTLIL::id2cstr(param.first));
			if (param.second.flags & RTLIL::CONST_FLAG_STRING) {
				std::string str = param.second.decode_string();
				f << stringf("\"");
				for (char ch : str)
					if (ch == '"' || ch == '\\')
						f << stringf("\\%c", ch);
					else if (ch < 32 || ch >= 127)
						f << stringf("\\%03o", ch);
					else
						f << stringf("%c", ch);
				f << stringf("\"\n");
			} else
				f << stringf("%s\n", param.second.as_string().c_str());
		}
	}

	void dump()
	{
		f << stringf("\n");
		f << stringf("let Q%s {attrs::(string#string) list} conns =\n", cstr(module->name));

		std::map<int, RTLIL::Wire*> inputs, outputs, internals;

		int int_cnt = 0;
		for (auto &wire_it : module->wires_) {
			RTLIL::Wire *wire = wire_it.second;
			if (wire->port_input) {
				inputs[wire->port_id] = wire;
			} else if (wire->port_output) {
				outputs[wire->port_id] = wire;
			} else {
				internals[int_cnt] = wire;
				int_cnt++;
			}
		}

		f << stringf("    let inps = [");
		bool start = true;
		for (auto &it : inputs) {
			RTLIL::Wire *wire = it.second;
			if( !start ) f << ",\n                ";
			start = false;
			if( wire->width == 1 ) {
			    f << stringf(" \"%s\"", cstr(wire->name));
			} else {
			    f << stringf(" \"%s[%d:0]\"", cstr(wire->name), wire->width-1);
			}
		}
		f << stringf(" ] in\n");

		f << stringf("    let outs = [");
		start = true;
		for (auto &it : outputs) {
			if( !start ) f << ",\n                ";
			start = false;
			RTLIL::Wire *wire = it.second;
			if( wire->width == 1 ) {
			    f << stringf(" \"%s\"", cstr(wire->name));
			} else {
			    f << stringf(" \"%s[%d:0]\"", cstr(wire->name), wire->width-1);
			}
		}
		f << stringf(" ] in\n");

		f << stringf("    let ints = md_extract_vectors [");
		start = true;
		for (auto &it : internals) {
			RTLIL::Wire *wire = it.second;
			if( !start ) f << ",\n                ";
			start = false;
			if( wire->width == 1 ) {
			    f << stringf(" \"%s\"", cstr(wire->name));
			} else {
			    f << stringf(" \"%s[%d:0]\"", cstr(wire->name), wire->width-1);
			}
		}
		for (auto it = module->memories.begin(); it != module->memories.end(); ++it) {
		    RTLIL::Memory *memory = it->second;
		    if( !start ) f << ",\n                ";
		    start = false;
		    f << stringf("\"%s[%d:%d][%d:0]\"", cstr(memory->name), memory->size+memory->start_offset-1, memory->start_offset, memory->width-1);
		}
		f << stringf(" ] in\n");
		for (auto it = module->memories.begin(); it != module->memories.end(); ++it) {
		    RTLIL::Memory *memory = it->second;
		    const char *name = cstr(memory->name);
		    f << stringf("    let %s = \"%s[%d:%d][%d:0]\" in\n", name, name,
				 memory->size+memory->start_offset-1, memory->start_offset, memory->width-1);
		}

		if (module->get_bool_attribute("\\blackbox")) {
			// CJS: FIX FIX FIX
			std::cerr << "Backboxing not supported yet";
			return;
		}

		if (!config->impltf_mode) {
			if (!config->false_type.empty()) {
				if (config->false_type == "+")
					f << stringf(".names %s\n", config->false_out.c_str());
				else if (config->false_type != "-")
					f << stringf(".%s %s %s=$false\n", subckt_or_gate(config->false_type),
							config->false_type.c_str(), config->false_out.c_str());
			} else
				f << stringf(".names $false\n");
			if (!config->true_type.empty()) {
				if (config->true_type == "+")
					f << stringf(".names %s\n1\n", config->true_out.c_str());
				else if (config->true_type != "-")
					f << stringf(".%s %s %s=$true\n", subckt_or_gate(config->true_type),
							config->true_type.c_str(), config->true_out.c_str());
			} else
				f << stringf(".names $true\n1\n");
			if (!config->undef_type.empty()) {
				if (config->undef_type == "+")
					f << stringf(".names %s\n", config->undef_out.c_str());
				else if (config->undef_type != "-")
					f << stringf(".%s %s %s=$undef\n", subckt_or_gate(config->undef_type),
							config->undef_type.c_str(), config->undef_out.c_str());
			} else
				f << stringf(".names $undef\n");
		}

		f << "    let _mk_fa f = (f, ((assoc f conns) catch [f])) in\n";
		f << "    let _fa_inps = map _mk_fa inps in\n";
		f << "    let _fa_outs = map _mk_fa outs in\n";
		f << "    let _body = [\n        ";
		start = true;
		for (auto &cell_it : module->cells_)
		{
		    RTLIL::Cell *cell = cell_it.second;
		    string src = "";
		    if (cell->attributes.count("\\src")) {
			Const si = cell->attributes.at("\\src");
			src = si.decode_string();
		    }
		    if( !start ) f << stringf(",\n        ");
		    start = false;
		    if ( cell->type == "$or" ||
			 cell->type == "$xor" ||
			 cell->type == "$and" ||
			 cell->type == "$sub" ||
			 cell->type == "$add" ||
			 cell->type == "$mul" ||
			 cell->type == "$div" ||
			 cell->type == "$mod" ||
			 cell->type == "$shl" ||
			 cell->type == "$sshl"
		       )
		    {
			RTLIL::SigSpec y = cell->getPort("\\Y");
			RTLIL::SigSpec a = cell->getPort("\\A");
			RTLIL::SigSpec b = cell->getPort("\\B");
			int y_sz = y.size();
			int a_sz = a.size();
			int b_sz = b.size();
			bool a_signed =
			    cell->parameters.count("\\A_SIGNED") > 0;
			bool b_signed =
			    cell->parameters.count("\\B_SIGNED") > 0;
			f << "_" << cell->type.substr(1);
			f << stringf(" %d \"%s\" ", y_sz, src.c_str());
			dump_sigspec(f, y);
			f << " ";
			if( a_sz > y_sz ) {
			    f.flush();
			    log_error("A size greater than Y size");
			}
			if( b_sz > y_sz ) {
			    f.flush();
			    log_error("B size greater than Y size");
			}
			if( a_sz < y_sz ) {
			    if( a_signed ) {
				f << stringf("(_SignExt %d ", y_sz);
			    } else {
				f << stringf("(_ZeroExt %d ", y_sz);
			    }
			    dump_sigspec(f, a);
			    f << ") ";
			} else {
			    dump_sigspec(f, a);
			    f << " ";
			}
			if( b_sz < y_sz ) {
			    if( b_signed ) {
				f << stringf("(_SignExt %d ", y_sz);
			    } else {
				f << stringf("(_ZeroExt %d ", y_sz);
			    }
			    dump_sigspec(f, b);
			    f << ") ";
			} else {
			    dump_sigspec(f, b);
			    f << " ";
			}
			continue;
		    }
		    if ( cell->type == "$lt" ||
			 cell->type == "$gt" ||
			 cell->type == "$ge" ||
			 cell->type == "$le" ||
			 cell->type == "$eq" ||
			 cell->type == "$eqx" ||
			 cell->type == "$ne" )
		    {
			RTLIL::SigSpec y = cell->getPort("\\Y");
			RTLIL::SigSpec a = cell->getPort("\\A");
			RTLIL::SigSpec b = cell->getPort("\\B");
			int a_sz = a.size();
			int b_sz = b.size();
			bool a_signed =
			    cell->parameters.count("\\A_SIGNED") > 0;
			bool b_signed =
			    cell->parameters.count("\\B_SIGNED") > 0;
			f << "_" << cell->type.substr(1);
			int sz = max(a_sz,b_sz);
			f << stringf(" %d \"%s\" ", sz, src.c_str());
			dump_sigspec(f, y);
			f << " ";
			if( a_sz > sz ) {
			    f.flush();
			    log_error("A size greater than Y size");
			}
			if( b_sz > sz ) {
			    f.flush();
			    log_error("B size greater than Y size");
			}
			if( a_sz < sz ) {
			    if( a_signed ) {
				f << stringf("(_SignExt %d ", sz);
			    } else {
				f << stringf("(_ZeroExt %d ", sz);
			    }
			    dump_sigspec(f, a);
			    f << ") ";
			} else {
			    dump_sigspec(f, a);
			    f << " ";
			}
			if( b_sz < sz ) {
			    if( b_signed ) {
				f << stringf("(_SignExt %d ", sz);
			    } else {
				f << stringf("(_ZeroExt %d ", sz);
			    }
			    dump_sigspec(f, b);
			    f << ") ";
			} else {
			    dump_sigspec(f, b);
			    f << " ";
			}
			continue;
		    }
		    if (cell->type == "$not") {
			    RTLIL::SigSpec out = cell->getPort("\\Y");
			    f << "_not ";
			    f << stringf("%d \"%s\" ", out.size(), src.c_str());
			    dump_sigspec(f, out);
			    f << " ";
			    dump_sigspec(f, cell->getPort("\\A"));
			    continue;
		    }
		    if (cell->type == "$neg") {
			RTLIL::SigSpec y = cell->getPort("\\Y");
			RTLIL::SigSpec a = cell->getPort("\\A");
			int y_sz = y.size();
			int a_sz = a.size();
			bool a_signed =
			    cell->parameters.count("\\A_SIGNED") > 0;
			f << "_" << cell->type.substr(1);
			f << stringf(" %d \"%s\" ", y_sz, src.c_str());
			dump_sigspec(f, y);
			f << " ";
			if( a_sz > y_sz ) {
			    f.flush();
			    log_error("A size greater than Y size");
			}
			if( a_sz < y_sz ) {
			    if( a_signed ) {
				f << stringf("(_SignExt %d ", y_sz);
			    } else {
				f << stringf("(_ZeroExt %d ", y_sz);
			    }
			    dump_sigspec(f, a);
			    f << ") ";
			} else {
			    dump_sigspec(f, a);
			    f << " ";
			}
			continue;
		    }

		    if (cell->type == "$dlatch") {
			RTLIL::SigSpec out = cell->getPort("\\Q");
			f << "_dlatch ";
			f << stringf("%d \"%s\" ", out.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, cell->getPort("\\EN"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\D"));
			continue;
		    }

		    if (cell->type == "$dff") {
			RTLIL::SigSpec out = cell->getPort("\\Q");
			f << "_dff ";
			f << stringf("%d \"%s\" ", out.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, cell->getPort("\\CLK"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\D"));
			continue;
		    }

		    if( cell->type == "$adffe") {
			RTLIL::SigSpec out = cell->getPort("\\Q");
			f << "_adffe ";
			if( cell->hasParam(ID::CLK_POLARITY) && !cell->getParam(ID::CLK_POLARITY).as_bool() )   { f << "F "; } else { f << "T "; }
			if( cell->hasParam(ID::EN_POLARITY) && !cell->getParam(ID::EN_POLARITY).as_bool() )     { f << "F "; } else { f << "T "; }
			if( cell->hasParam(ID::ARST_POLARITY) && !cell->getParam(ID::ARST_POLARITY).as_bool() ) { f << "F "; } else { f << "T "; }
			f << stringf("%d \"%s\" ", out.size(), src.c_str());
			f << " ";
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, cell->getPort("\\ARST"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\EN"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\CLK"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\D"));
			continue;
		    }
		    if( cell->type == "$dffe") {
			RTLIL::SigSpec out = cell->getPort("\\Q");
			f << "_dffe ";
			if( cell->hasParam(ID::CLK_POLARITY) && !cell->getParam(ID::CLK_POLARITY).as_bool() )   { f << "F "; } else { f << "T "; }
			if( cell->hasParam(ID::EN_POLARITY) && !cell->getParam(ID::EN_POLARITY).as_bool() )     { f << "F "; } else { f << "T "; }
			f << stringf("%d \"%s\" ", out.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, cell->getPort("\\EN"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\CLK"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\D"));
			continue;
		    }
		    if( cell->type == "$sdff") {
			RTLIL::SigSpec out = cell->getPort("\\Q");
			f << "_sdff ";
			if( cell->hasParam(ID::CLK_POLARITY) && !cell->getParam(ID::CLK_POLARITY).as_bool() )   { f << "F "; } else { f << "T "; }
			if( cell->hasParam(ID::SRST_POLARITY) && !cell->getParam(ID::SRST_POLARITY).as_bool() ) { f << "F "; } else { f << "T "; }
			f << stringf("%d \"%s\" ", out.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, cell->getPort("\\SRST"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\CLK"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\D"));
			continue;
		    }
		    if( cell->type == "$sdffe") {
			RTLIL::SigSpec out = cell->getPort("\\Q");
			f << "_sdffe ";
			if( cell->hasParam(ID::CLK_POLARITY) && !cell->getParam(ID::CLK_POLARITY).as_bool() )   { f << "F "; } else { f << "T "; }
			if( cell->hasParam(ID::EN_POLARITY) && !cell->getParam(ID::EN_POLARITY).as_bool() )   { f << "F "; } else { f << "T "; }
			if( cell->hasParam(ID::SRST_POLARITY) && !cell->getParam(ID::SRST_POLARITY).as_bool() ) { f << "F "; } else { f << "T "; }
			f << stringf("%d \"%s\" ", out.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, cell->getPort("\\SRST"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\EN"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\CLK"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\D"));
			continue;
		    }
		    if( cell->type == "$sdffce") {
			RTLIL::SigSpec out = cell->getPort("\\Q");
			f << "_sdffce ";
			if( cell->hasParam(ID::CLK_POLARITY) && !cell->getParam(ID::CLK_POLARITY).as_bool() )   { f << "F "; } else { f << "T "; }
			if( cell->hasParam(ID::EN_POLARITY) && !cell->getParam(ID::EN_POLARITY).as_bool() )   { f << "F "; } else { f << "T "; }
			if( cell->hasParam(ID::SRST_POLARITY) && !cell->getParam(ID::SRST_POLARITY).as_bool() ) { f << "F "; } else { f << "T "; }
			f << stringf("%d \"%s\" ", out.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, cell->getPort("\\SRST"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\EN"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\CLK"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\D"));
			continue;
		    }
		    if( cell->type == "$dffsre") {
			RTLIL::SigSpec out = cell->getPort("\\Q");
			f << "_dffsre ";
			if( cell->hasParam(ID::CLK_POLARITY) && !cell->getParam(ID::CLK_POLARITY).as_bool() )   { f << "F "; } else { f << "T "; }
			if( cell->hasParam(ID::SET_POLARITY) && !cell->getParam(ID::SET_POLARITY).as_bool() ) { f << "F "; } else { f << "T "; }
			if( cell->hasParam(ID::CLR_POLARITY) && !cell->getParam(ID::CLR_POLARITY).as_bool() ) { f << "F "; } else { f << "T "; }
			if( cell->hasParam(ID::EN_POLARITY) && !cell->getParam(ID::EN_POLARITY).as_bool() )   { f << "F "; } else { f << "T "; }
			f << stringf("%d \"%s\" ", out.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, cell->getPort("\\CLR"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\SET"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\EN"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\CLK"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\D"));
			continue;
		    }

		    if (cell->type == "$adff") {
			RTLIL::SigSpec out = cell->getPort("\\Q");
			f << "_adff ";
			f << stringf("%d \"%s\" ", out.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, cell->getPort("\\ARST"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\CLK"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\D"));
			continue;
		    }
		    if (cell->type == "$shiftx") {
			RTLIL::SigSpec out = cell->getPort("\\Y");
			RTLIL::SigSpec a = cell->getPort("\\A");
			RTLIL::SigSpec b = cell->getPort("\\B");
			f << "_shr ";
			f << stringf("%d %d %d \"%s\" ", out.size(), a.size(), b.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, a);
			f << " ";
			dump_sigspec(f, b);
			continue;
		    }
		    if (cell->type == "$shr") {
			RTLIL::SigSpec out = cell->getPort("\\Y");
			RTLIL::SigSpec a = cell->getPort("\\A");
			RTLIL::SigSpec b = cell->getPort("\\B");
			f << "_shr ";
			f << stringf("%d %d %d \"%s\" ", out.size(), a.size(), b.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, a);
			f << " ";
			dump_sigspec(f, b);
			continue;
		    }
		    if (cell->type == "$sshr") {
			RTLIL::SigSpec out = cell->getPort("\\Y");
			RTLIL::SigSpec a = cell->getPort("\\A");
			RTLIL::SigSpec b = cell->getPort("\\B");
			f << "_ashr ";
			f << stringf("%d %d %d \"%s\" ", out.size(), a.size(), b.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, a);
			f << " ";
			dump_sigspec(f, b);
			continue;
		    }
		    if (cell->type == "$logic_not") {
			RTLIL::SigSpec out = cell->getPort("\\Y");
			RTLIL::SigSpec inp = cell->getPort("\\A");
			f << "_logic_not ";
			f << stringf("%d \"%s\" ", inp.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, inp);
			continue;
		    }
		    if (cell->type == "$logic_and") {
			RTLIL::SigSpec out = cell->getPort("\\Y");
			RTLIL::SigSpec inp = cell->getPort("\\A");
			f << "_land ";
			f << stringf("%d \"%s\" ", inp.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, cell->getPort("\\A"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\B"));
			continue;
		    }

		    if (cell->type == "$logic_or") {
			RTLIL::SigSpec out = cell->getPort("\\Y");
			RTLIL::SigSpec inp = cell->getPort("\\A");
			f << "_lor ";
			f << stringf("%d \"%s\" ", inp.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, cell->getPort("\\A"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\B"));
			continue;
		    }

		    if (cell->type == "$mux") {
			RTLIL::SigSpec out = cell->getPort("\\Y");
			f << "_mux ";
			f << stringf("%d \"%s\" ", out.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, cell->getPort("\\A"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\B"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\S"));
			continue;
		    }

		    if (cell->type == "$pmux") {
			RTLIL::SigSpec out = cell->getPort("\\Y");
			RTLIL::SigSpec sel = cell->getPort("\\S");
			f << stringf("_pmux %d %d \"%s\" ", out.size(), sel.size(), src.c_str());
			dump_sigspec(f, out);
			f << " ";
			dump_sigspec(f, cell->getPort("\\A"));
			f << " ";
			dump_sigspec(f, cell->getPort("\\B"));
			f << " ";
			dump_sigspec(f, sel);
			continue;
		    }
		    if (cell->type == "$reduce_bool") {
			RTLIL::SigSpec out = cell->getPort("\\Y");
			RTLIL::SigSpec inp = cell->getPort("\\A");
			f << "_logic_reduce_bool ";
			f << stringf("%d \"%s\" ", inp.size(), src.c_str());
			dump_sigspec(f, cell->getPort("\\Y"));
			f << " ";
			dump_sigspec(f, inp);
			continue;
		    }
		    if (cell->type == "$reduce_or") {
			RTLIL::SigSpec out = cell->getPort("\\Y");
			RTLIL::SigSpec inp = cell->getPort("\\A");
			f << "_logic_reduce_or ";
			f << stringf("%d \"%s\" ", inp.size(), src.c_str());
			dump_sigspec(f, cell->getPort("\\Y"));
			f << " ";
			dump_sigspec(f, inp);
			continue;
		    }
		    if (cell->type == "$reduce_and") {
			RTLIL::SigSpec out = cell->getPort("\\Y");
			RTLIL::SigSpec inp = cell->getPort("\\A");
			f << "_logic_reduce_and ";
			f << stringf("%d \"%s\" ", inp.size(), src.c_str());
			dump_sigspec(f, cell->getPort("\\Y"));
			f << " ";
			dump_sigspec(f, inp);
			continue;
		    }

		    if( cell->type == "$memwr" ) {
			const char *mem = cstr(cell->parameters["\\MEMID"].decode_string());
			int width = cell->parameters["\\WIDTH"].as_int();
			int abits = cell->parameters["\\ABITS"].as_int();
			f << stringf("_memwr \"%s\" %d %d %s", src.c_str(), width, abits, mem);
                        f << " ";
                        dump_sigspec(f, cell->getPort("\\EN"));
                        f << " ";
                        dump_sigspec(f, cell->getPort("\\ADDR"));
                        f << " ";
                        dump_sigspec(f, cell->getPort("\\DATA"));
			continue;
		    }

		    if( cell->type == "$meminit" ) {
			const char *mem = cstr(cell->parameters["\\MEMID"].decode_string());
			int width = cell->parameters["\\WIDTH"].as_int();
			int abits = cell->parameters["\\ABITS"].as_int();
			f << stringf("_meminit \"%s\" %d %d %s", src.c_str(), width, abits, mem);
                        f << " ";
                        dump_sigspec(f, cell->getPort("\\DATA"));
			continue;
		    }

		    if( cell->type == "$memrd" ) {
			const char *mem = cstr(cell->parameters["\\MEMID"].decode_string());
			int width = cell->parameters["\\WIDTH"].as_int();
			int abits = cell->parameters["\\ABITS"].as_int();
			f << stringf("_memrd \"%s\" %d %d %s", src.c_str(), width, abits, mem);
                        f << " ";
                        dump_sigspec(f, cell->getPort("\\EN"));
                        f << " ";
                        dump_sigspec(f, cell->getPort("\\ADDR"));
                        f << " ";
                        dump_sigspec(f, cell->getPort("\\DATA"));
			continue;
		    }

		    if( cell->type == "$mem" ) {
			const char *mem = cstr(cell->parameters["\\MEMID"].decode_string());
			int wr_ports = cell->parameters["\\WR_PORTS"].as_int();
			int rd_ports = cell->parameters["\\RD_PORTS"].as_int();
			int mem_size = cell->parameters["\\SIZE"].as_int();
			int mem_width = cell->parameters["\\WIDTH"].as_int();
			int mem_offset = cell->parameters["\\OFFSET"].as_int();
			int mem_abits = cell->parameters["\\ABITS"].as_int();
			f << stringf("_mem \"%s\" \"%s\"\n                ", src.c_str(), mem);
			f << "// wr_ports rd_ports mem_size mem_width mem_offset mem_abits\n                ";
			f << stringf("%d %d %d %d %d %d [\n                ",
				     wr_ports, rd_ports, mem_size, mem_width,
				     mem_offset, mem_abits);
			bool first = true;
			for (auto &conn : cell->connections()) {
			    if( !first ) f << ",\n                ";
			    first = false;
			    if (conn.second.size() == 1)
				    f << stringf("(\"%s\",", cstr(conn.first));
			    else
				    f << stringf("(\"%s[%d:0]\",",
					cstr(conn.first), conn.second.size()-1);
			    dump_sigspec(f, conn.second);
			    f << ")";
			}
			f << stringf("]");
			continue;
		    }

		    // Needed??????
		    if (config->unbuf_types.count(cell->type)) {
			auto portnames = config->unbuf_types.at(cell->type);
			f << stringf(".names00 %s %s\n1 1\n",
				     cstr(cell->getPort(portnames.first)),
				     cstr(cell->getPort(portnames.second)));
			continue;
		    }

		    f << stringf("Q%s [(\"instance\", \"%s\"), (\"src\", \"%s\")] [\n                ",
				 cstr(cell->type), (((cell->name).c_str())+1), src.c_str());

		    bool first = true;
		    for (auto &conn : cell->connections()) {
			int ssz = conn.second.size();
			if( ssz != 0) {
			    if( !first ) f << ",\n                ";
			    first = false;
			    if (ssz == 1)
				f << stringf("(\"%s\",", cstr(conn.first));
			    else
				f << stringf("(\"%s[%d:0]\",", cstr(conn.first), ssz-1);
			    dump_sigspec(f, conn.second);
			    f << ")";
			}
		    }
		    f << stringf("]");

		    if (config->cname_mode)
			    f << stringf(".cname %s\n", cstr(cell->name));
		    if (config->attr_mode)
			    dump_params(".attr", cell->attributes);
		    if (config->param_mode)
			    dump_params(".param", cell->parameters);
		}

		for (auto &conn : module->connections()) {
		    RTLIL::SigSpec out = conn.first;
		    if( !start ) f << stringf(",\n        ");
		    start = false;
		    string src = "";
		    if (out.is_wire() && out.as_wire()->attributes.count("\\src")) {
			Const si = out.as_wire()->attributes.at("\\src");
			src = si.decode_string();
		    }
		    f << stringf("_buf %d \"%s\" ", out.size(), src.c_str());
		    dump_sigspec(f, out);
		    f << " ";
		    dump_sigspec(f, conn.second);
		}
		f << "\n    ] in\n";
		f << stringf("    PINST \"draw_hier %s\" attrs F _fa_inps _fa_outs ints (P_HIER _body)\n;\n\n", cstr(module->name));

	}

	static void dump(std::ostream &f, RTLIL::Module *module, RTLIL::Design *design, PexlifDumperConfig &config)
	{
		PexlifDumper dumper(f, module, design, &config);
		dumper.dump();
	}
};

struct PexlifBackend : public Backend {
	PexlifBackend() : Backend("pexlif", "write design to pexlif file") { }
	virtual void help()
	{
		//   |---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|
		log("\n");
		log("    write_pexlif [options] [filename]\n");
		log("\n");
		log("Write the current design to a pexlif file.\n");
		log("\n");
		log("    -top top_module\n");
		log("        set the specified module as design top module\n");
		log("\n");
		log("    -buf <cell-type> <in-port> <out-port>\n");
		log("        use cells of type <cell-type> with the specified port names for buffers\n");
		log("\n");
		log("    -unbuf <cell-type> <in-port> <out-port>\n");
		log("        replace buffer cells with the specified name and port names with\n");
		log("        a .names statement that models a buffer\n");
		log("\n");
		log("    -true <cell-type> <out-port>\n");
		log("    -false <cell-type> <out-port>\n");
		log("    -undef <cell-type> <out-port>\n");
		log("        use the specified cell types to drive nets that are constant 1, 0, or\n");
		log("        undefined. when '-' is used as <cell-type>, then <out-port> specifies\n");
		log("        the wire name to be used for the constant signal and no cell driving\n");
		log("        that wire is generated. when '+' is used as <cell-type>, then <out-port>\n");
		log("        specifies the wire name to be used for the constant signal and a .names\n");
		log("        statement is generated to drive the wire.\n");
		log("\n");
		log("    -noalias\n");
		log("        if a net name is aliasing another net name, then by default a net\n");
		log("        without fanout is created that is driven by the other net. This option\n");
		log("        suppresses the generation of this nets without fanout.\n");
		log("\n");
		log("    -icells\n");
		log("        do not translate Yosys's internal gates to generic pexlif logic\n");
		log("        functions. Instead create .subckt or .gate lines for all cells.\n");
		log("\n");
		log("    -gates\n");
		log("        print .gate instead of .subckt lines for all cells that are not\n");
		log("        instantiations of other modules from this design.\n");
		log("\n");
		log("    -conn\n");
		log("        do not generate buffers for connected wires. instead use the\n");
		log("        non-standard .conn statement.\n");
		log("\n");
		log("    -attr\n");
		log("        use the non-standard .attr statement to write cell attributes\n");
		log("\n");
		log("    -param\n");
		log("        use the non-standard .param statement to write cell parameters\n");
		log("\n");
		log("    -cname\n");
		log("        use the non-standard .cname statement to write cell names\n");
		log("\n");
		log("    -blackbox\n");
		log("        write blackbox cells with .blackbox statement.\n");
		log("\n");
		log("    -impltf\n");
		log("        do not write definitions for the $true, $false and $undef wires.\n");
		log("\n");
	}
	virtual void execute(std::ostream *&f, std::string filename, std::vector<std::string> args, RTLIL::Design *design)
	{
		std::string top_module_name;
		std::string buf_type, buf_in, buf_out;
		std::string true_type, true_out;
		std::string false_type, false_out;
		PexlifDumperConfig config;

		log_header(design, "Executing pexlif backend.\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++)
		{
			if (args[argidx] == "-top" && argidx+1 < args.size()) {
				top_module_name = args[++argidx];
				continue;
			}
			if (args[argidx] == "-buf" && argidx+3 < args.size()) {
				config.buf_type = args[++argidx];
				config.buf_in = args[++argidx];
				config.buf_out = args[++argidx];
				continue;
			}
			if (args[argidx] == "-unbuf" && argidx+3 < args.size()) {
				RTLIL::IdString unbuf_type = RTLIL::escape_id(args[++argidx]);
				RTLIL::IdString unbuf_in = RTLIL::escape_id(args[++argidx]);
				RTLIL::IdString unbuf_out = RTLIL::escape_id(args[++argidx]);
				config.unbuf_types[unbuf_type] = std::pair<RTLIL::IdString, RTLIL::IdString>(unbuf_in, unbuf_out);
				continue;
			}
			if (args[argidx] == "-true" && argidx+2 < args.size()) {
				config.true_type = args[++argidx];
				config.true_out = args[++argidx];
				continue;
			}
			if (args[argidx] == "-false" && argidx+2 < args.size()) {
				config.false_type = args[++argidx];
				config.false_out = args[++argidx];
				continue;
			}
			if (args[argidx] == "-undef" && argidx+2 < args.size()) {
				config.undef_type = args[++argidx];
				config.undef_out = args[++argidx];
				continue;
			}
			if (args[argidx] == "-icells") {
				config.icells_mode = true;
				continue;
			}
			if (args[argidx] == "-gates") {
				config.gates_mode = true;
				continue;
			}
			if (args[argidx] == "-conn") {
				config.conn_mode = true;
				continue;
			}
			if (args[argidx] == "-cname") {
				config.cname_mode = true;
				continue;
			}
			if (args[argidx] == "-param") {
				config.param_mode = true;
				continue;
			}
			if (args[argidx] == "-attr") {
				config.attr_mode = true;
				continue;
			}
			if (args[argidx] == "-blackbox") {
				config.blackbox_mode = true;
				continue;
			}
			if (args[argidx] == "-impltf") {
				config.impltf_mode = true;
				continue;
			}
			if (args[argidx] == "-noalias") {
				config.noalias_mode = true;
				continue;
			}
			break;
		}
		extra_args(f, filename, args, argidx);

		if (top_module_name.empty())
			for (auto & mod_it:design->modules_)
				if (mod_it.second->get_bool_attribute("\\top"))
					top_module_name = mod_it.first.str();

		std::vector<RTLIL::Module*> mod_list;
		std::vector<RTLIL::Module*>::iterator first;
		RTLIL::Module *my_top_module = NULL;

		design->sort();
		for (auto module_it : design->modules_)
		{
			RTLIL::Module *module = module_it.second;
			if (module->get_bool_attribute("\\blackbox") && !config.blackbox_mode)
				continue;

			if (module->processes.size() != 0)
				log_error("Found unmapped processes in module %s: unmapped processes are not supported in pexlif backend!\n", RTLIL::id2cstr(module->name));

			if (module->name == RTLIL::escape_id(top_module_name)) {
				my_top_module = module;
				top_module_name.clear();
				continue;
			}

			mod_list.push_back(module);

		}

		if (!top_module_name.empty())
			log_error("Can't find top module `%s'!\n", top_module_name.c_str());


		for (auto module : mod_list) {
			RTLIL::IdString id = module->name;
			vector<shared_str> cstr_buf;
			std::string str = RTLIL::unescape_id(id);
			if( str.substr(0,8) == "$paramod" ) {
			    str = str.substr(9);
			}
			for (size_t i = 0; i < str.size(); i++)
				if (str[i] == '\\' || str[i] == '#' || str[i] == '=' || str[i] == '<' || str[i] == '>')
					str[i] = '_';
			cstr_buf.push_back(str);
			*f << stringf("forward_declare {Q%s::((string#string) list) -> ((string#(string list)) list) -> pexlif};\n",
				      cstr_buf.back().c_str());
		}

		for (auto module : mod_list)
			PexlifDumper::dump(*f, module, design, config);

		PexlifDumper::dump(*f, my_top_module, design, config);
	}
} PexlifBackend;

PRIVATE_NAMESPACE_END
