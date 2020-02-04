//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************
*       Original author: Peter Seger 2017       *
************************************************/

#ifndef PLACE_H
#define PLACE_H

#include "types.h"
#include "rec.h"
#include "buf.h"
#include "hash.h"
#include "list_util.h"
#include <string.h>


typedef struct sch_draw_list_rec *sch_draw_list_ptr;
typedef struct sch_draw_rec *sch_draw_ptr;
typedef struct tree_rec *tree_ptr;
typedef struct bbox_rec	*bbox_ptr;
typedef struct dag_rec	*dag_ptr;
typedef struct tuple_rec *tuple_ptr;
typedef struct vert_line_rec *vert_line_ptr;
typedef struct pair_list_rec *pair_list_ptr;

typedef struct sch_draw_list_rec {
	sch_draw_ptr	    tree;
	sch_draw_list_ptr   next;
} sch_draw_list_rec;


typedef struct bbox_rec	{
	list_ptr    upper;
	list_ptr    lower;
} bbox_rec;

typedef enum {NODE, LEAF} sch_type;

typedef enum {
		PFN_OUTPUT,
		PFN_LOOP_SRC,
		PFN_REPEAT_ND,
		PFN_WIRE,
		PFN_SPLIT,
		PFN_VERTICAL_WIRE,
		PFN_OTHER
	     } pfn_type;

typedef struct sch_draw_rec {
    sch_type		type;
    int			bin_num;
    int			width;
    int			height;
    int			x;
    int			y;
    char*		name;
    string		pfn;
    pfn_type		pfn_type;
    sch_draw_list_ptr	fanins;
    sch_draw_ptr	loop_src;
} sch_draw_rec;

#define SPT_BRANCH   0
#define SPT_LEAF    1
typedef struct space_tree_rec	*space_tree_ptr;
typedef struct space_tree_rec {
    int		    type:2;
    int		    x:30;
    space_tree_ptr  left;
    space_tree_ptr  right;
    
} space_tree_rec;


#define IS_OUTPUT(tr)		((tr)->pfn_type == PFN_OUTPUT)
#define IS_LOOP_SRC(tr)		((tr)->pfn_type == PFN_LOOP_SRC)
#define IS_REPEAT_ND(tr)	((tr)->pfn_type == PFN_REPEAT_ND)
#define IS_WIRE(tr)		((tr)->pfn_type == PFN_WIRE)
#define IS_SPLIT(tr)		((tr)->pfn_type == PFN_SPLIT)
#define IS_VERTICAL_WIRE(tr)	((tr)->pfn_type == PFN_VERTICAL_WIRE)

typedef struct dag_rec {
	char	*name;
	dag_ptr	left;
	dag_ptr	right;
} dag_rec;


typedef struct tuple_rec {
	int	x;
	int 	y;
} tuple_rec;

typedef struct vert_line_rec {
	sch_draw_ptr	obj;
	vert_line_ptr	same_line_next;
	vert_line_ptr	next;
} vert_line_rec;

typedef struct pair_list_rec {
	int		x;
	int		y;
	pair_list_ptr	next;
} pair_list_rec;


void		Init_place();
void		Setup_draw_network();
void		Adjust_widths(sch_draw_ptr tree);
void		Rank_order_tree(sch_draw_ptr tree, int pos);
sch_draw_ptr	Place(sch_draw_ptr tree);
void		Place_drivers(sch_draw_ptr tree, hash_record_ptr drivers);
int		Find_sep_distance(list_ptr upper, list_ptr lower);
list_ptr	Move_bb(int x, int y, list_ptr bbox);
list_ptr	Merge_min_bb(list_ptr upper, list_ptr lower);
list_ptr	Merge_max_bb(list_ptr upper, list_ptr lower);
void		bin_network(sch_draw_ptr tree, buffer_ptr buf, int pos);
void		Find_repeats(sch_draw_ptr tree, hash_record_ptr repeats);
void		Find_drivers(sch_draw_ptr tree);
int		Move_driver_leftmost(sch_draw_ptr tree);
void		Find_rightmost_repeats(sch_draw_ptr tree);
void		Replace_rmost_repeat_with_wires();
void		Reset_rmost_repeats();
void		Add_vertical_to_wires();
void		Connect_verticals(sch_draw_ptr tree);
void		Remove_drive_repeat_overlaps(sch_draw_ptr tree);

void		Calc_sep_dists(hash_record_ptr repeats, sch_draw_ptr tree, 
				hash_record_ptr drivers, 
				hash_record_ptr dists);
void		Replace_with_wire(hash_record_ptr repeats, sch_draw_ptr tree, 
					hash_record_ptr dists);
void		Equalize_width(sch_draw_ptr tree);
void		Modify_leaf_repeats(sch_draw_ptr tree, 
					hash_record_ptr left_most_repeats,
					hash_record_ptr drivers,
					hash_record_ptr lengths);
void		Expand_for_vertical_wires(sch_draw_ptr tree);
void		Break_loops(sch_draw_ptr tree);
/*
void		Shift_vertical_lines(sch_draw_ptr tree);
void		Find_left_most_repeats(sch_draw_ptr tree, 
					hash_record_ptr repeats);
void		Equalize_height(sch_draw_ptr tree);
void		Find_wires(sch_draw_ptr tree, hash_record_ptr wires);
*/


#endif
