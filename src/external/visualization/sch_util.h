//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************
*       Original author: Peter Seger 2017       *
************************************************/

#include "place.h"
#include "buf.h"

sch_draw_list_ptr	Add_to_front_network(sch_draw_ptr tree,
					     sch_draw_list_ptr network);
sch_draw_list_ptr	Add_to_back_network(sch_draw_ptr tree,
					    sch_draw_list_ptr network);
sch_draw_ptr		New_sch(sch_type type, int w, int h, int x, int y,
				char* name, string pfn,
				sch_draw_list_ptr fanins);
pfn_type		Pfn2pfn_type(string pfn);
void			Init_sch();
void			Print_network(string name, sch_draw_ptr tree);
void			Pretty_printer(sch_draw_ptr tree, char* filename);

/* Printing Functions */
void		Print_fanins(hash_record_ptr hash);
void		Print_pfn(hash_record_ptr hash);
void		Print_drivers(hash_record_ptr drivers);
void		Print_dists(hash_record_ptr dists);

