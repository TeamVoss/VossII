//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************
*       Original author: Peter Seger 2017       *
************************************************/
    
#include <stdio.h>
#include <string.h>
#include "list_util.h"
#include "sch_util.h"
#include "place.h"
#include "buf.h"
#include "hash.h"
#include "strmgr.h"

/* -- Global Variables Referred to -- */
extern ustr_mgr         strings;

/* --Local Functions-- */
static bbox_ptr		find_bbox(sch_draw_ptr tree);
static list_ptr		merge_max_bb_rec(int cur_u_y, int cur_l_y,
					 list_ptr upper, list_ptr lower);
static list_ptr		merge_min_bb_rec(int cur_u_y, int cur_l_y,
					 list_ptr upper, list_ptr lower);
void			shift_drivers_fn(sch_draw_ptr tree, 
					 hash_record_ptr drivers, 
					 hash_record_ptr repeats);
void			shift_driver_over(sch_draw_ptr driver, int dist);
sch_draw_ptr		find_driver(sch_draw_ptr repeat,
				    hash_record_ptr drivers);
static void		bin_verticals(sch_draw_ptr tree);
static void		adjust_verticals(pointer key, pointer data);
static space_tree_ptr	build_space_tree(int m, int M, int cur_sep);
static void		make_room_for_verticals(space_tree_ptr spt,
						sch_draw_ptr tree);
static void		rename_loop_repeat_nodes(sch_draw_ptr tree,
						 hash_record_ptr done);
static void		break_loops_rec(sch_draw_ptr tree,
					hash_record_ptr seen,
					hash_record_ptr	new_nodes,
					hash_record_ptr done);
static void		find_drivers_rec(sch_draw_ptr tree);

/* --------------------- Local variables ------------------------ */
static int	    min_sep = 8;
static buffer       vloc_buf;
static buffer       vspace_buf;
static hash_record  verticals;
static hash_record  rmost_repeats;
static hash_record  drivers;


/* --Record Managers--  */
static rec_mgr	bbox_rec_mgr;
static rec_mgr	list_rec_mgr;
static rec_mgr	sch_draw_rec_mgr;
static rec_mgr	sch_draw_list_rec_mgr;
static rec_mgr	bucket_rec_mgr;
static rec_mgr	dag_rec_mgr;
static rec_mgr	tuple_rec_mgr;
static rec_mgr	vert_line_rec_mgr;
static rec_mgr	space_tree_rec_mgr;

/* --Macros-- */
#define PTR2HASH(p)	(((unint) ((((long unsigned int) (p)))>>32)) \
				+ ((unint) (((long unsigned int) (p)))))
#define PTR2INT(p)	((int) ((long) (p)))
#define INT2PTR(i)	((void*) ((long) (i)))

#define NUM_BINS	1000
#define MAX_LOCATION	9999999


void
Dbg_Stop()
{
    fprintf(stderr, "==================Dbg_Stop======================\n");
}



void
Init_place()
{
    new_mgr(&sch_draw_list_rec_mgr, sizeof(sch_draw_list_rec));
    new_mgr(&sch_draw_rec_mgr, sizeof(sch_draw_rec));
    new_mgr(&vert_line_rec_mgr, sizeof(vert_line_rec));
}

void
Setup_draw_network()
{
    create_hash(&rmost_repeats, 100, str_hash, str_equ);
    create_hash(&drivers, 100, str_hash, str_equ);
}

void
Reset_rmost_repeats()
{
    dispose_hash(&rmost_repeats, NULLFCN);
    create_hash(&rmost_repeats, 100, str_hash, str_equ);
}


void
dbg_bbox(string msg, bbox_ptr bp)
{
    fprintf(stderr, "\n%s\n  Upper: ", msg);
    for( list_ptr cur = bp->upper; cur != NULL; cur = cur->next) {
	fprintf(stderr, " (%d,%d)", cur->x, cur->y);
    }
    fprintf(stderr, "\n");
    fprintf(stderr, "  Lower: ");
    for( list_ptr cur = bp->lower; cur != NULL; cur = cur->next) {
	fprintf(stderr, " (%d,%d)", cur->x, cur->y);
    }
    fprintf(stderr, "\n");
}


/* Rank_order_tree takes a network tree head and goes through the whole network 
	adding each element to a corresponding bin based on the 'depth' of the
	element. */
void
Rank_order_tree(sch_draw_ptr tree, int pos)
{
    tree->bin_num = pos; // Assign each element its bin number
    if( pos > 1000 ) {
	// Should not happen.....
	fprintf(stderr, "WHAT????\n");
	return;
    }
    for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	Rank_order_tree(lp->tree, pos+1);
    }
}


/* Move_driver_leftmost ensures the driver is the left-most of all nodes with 
	the same name */
int
Move_driver_leftmost(sch_draw_ptr tree)
{
    if( IS_REPEAT_ND(tree) )  {
	sch_draw_ptr driver = (sch_draw_ptr) find_hash(&drivers, tree->name);
	if( driver->bin_num < tree->bin_num ) {
	    /* Need to swap them */
	    int driver_bin = driver->bin_num;
	    int tree_bin = tree->bin_num;
	    sch_draw_rec tmp = *driver;
	    *driver = *tree;
	    *tree = tmp;
	    driver->bin_num = tree_bin;
	    tree->bin_num = driver_bin;
	    Rank_order_tree(tree, tree_bin);
	    delete_hash(&drivers, tree->name);
	    insert_hash(&drivers, tree->name, (pointer) tree);
	    return 1;
	} else {
	    return 0;
	}
    } else {
	for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next){
	    if( Move_driver_leftmost(lp->tree) != 0 ) { return 1; }
	}
	return 0;
    }
}

static void
increase_bin_num(sch_draw_ptr tree)
{
    tree->bin_num++;
    for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next)
    {
	increase_bin_num(lp->tree);
    } 
}

void
Remove_drive_repeat_overlaps(sch_draw_ptr tree)
{
    if( IS_REPEAT_ND(tree) )  {
	sch_draw_ptr driver =
	    (sch_draw_ptr) find_hash(&drivers, (pointer) tree->name);
	if( driver->bin_num == tree->bin_num ) {
	    /* Need to shift driver left */
	    int cur_driver_bin_num = driver->bin_num;
	    increase_bin_num(driver);
	    char *pfn = uStrsave(&strings, "draw_dummy_wire 8");
	    sch_draw_list_ptr fanin;
	    fanin = (sch_draw_list_ptr) new_rec(&sch_draw_list_rec_mgr);
	    fanin->next = NULL;
	    sch_draw_ptr wire = New_sch(NODE, min_sep, 4,
					driver->x, driver->y, tree->name,
					pfn, fanin);
	    fanin->tree = wire;
	    sch_draw_rec tmp = *driver;
	    *driver = *wire;
	    *wire = tmp;
	    wire->bin_num = cur_driver_bin_num+1;
	    driver->bin_num = cur_driver_bin_num;
	    delete_hash(&drivers, tree->name);
	    insert_hash(&drivers, tree->name, (pointer) wire);
	}
    } else {
	for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	    Remove_drive_repeat_overlaps(lp->tree);
	} 
    }
}



/* Adjust_widths takes the whole network and adjusts the width of each object
	according to the number of fanins it has leaving enough room to route
	each wire that might come out of it.  */
void
Adjust_widths(sch_draw_ptr tree)
{
    int count = 0;
    sch_draw_list_ptr fanin = tree->fanins;
    while( fanin != NULL ) {
	count++;
	fanin = fanin->next;
    }
    if( count != 0 ) {
	tree->width = tree->width + ((count-1)/2)*min_sep; 
    }

    for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	Adjust_widths(lp->tree);
    }
}


/* ---------- Place ---------- */

/* find_bbox traverses down a tree and figures out the bounding boxes for
	every item in it and places them in this bbox.*/
static bbox_ptr
find_bbox(sch_draw_ptr tree)
{
    if( tree->type == LEAF || tree->fanins == NULL ) {
	bbox_ptr bb = (bbox_ptr) new_rec(&bbox_rec_mgr);
	bb->upper = Add_to_front(0,-tree->height/2,
			Add_to_front(-tree->width, -tree->height/2, NULL));
	bb->lower = Add_to_front(0, tree->height/2, 
			Add_to_front(-tree->width, tree->height/2, NULL));
	tree->x = 0;
	tree->y = 0;
	return bb;
    }
    /* A node with children */

    // Get bboxes for children
    buffer bbox_buffer;
    new_buf(&bbox_buffer, 10, sizeof(bbox_ptr));
    for(sch_draw_list_ptr ch = tree->fanins; ch != NULL; ch = ch->next) {
	bbox_ptr ch_bb = find_bbox(ch->tree);
	push_buf(&bbox_buffer, &ch_bb);
    }

    // Compute needed separation
    buffer sep_buffer;
    new_buf(&sep_buffer, 10, sizeof(int));
    bbox_ptr current_bbox = *((bbox_ptr *) locate_buf(&bbox_buffer,0));
    int prev_y_loc = 0;
    int total = 0;
    for(int i = 0; i < COUNT_BUF(&bbox_buffer); i++) {
	bbox_ptr lower_bb =*((bbox_ptr *) locate_buf(&bbox_buffer,i+1));
	if(lower_bb == NULL) {
	    int sep = 0;
	    push_buf(&sep_buffer, &sep);
	    break;
	} 
	if(current_bbox->upper == NULL || current_bbox->lower == NULL) {
	    int sep = 0;
	    push_buf(&sep_buffer, &sep);
	    break;
	} else {
	    int y_loc = Find_sep_distance(lower_bb->upper, current_bbox->lower);
	    lower_bb->upper = Move_bb(0, y_loc, lower_bb->upper);
	    lower_bb->lower = Move_bb(0, y_loc, lower_bb->lower);
	    current_bbox->upper =
		    Merge_min_bb(current_bbox->upper,lower_bb->upper);
	    current_bbox->lower =
		    Merge_max_bb(current_bbox->lower,lower_bb->lower);
	    int sep = y_loc - prev_y_loc;
	    total += sep;
	    prev_y_loc = y_loc;
	    push_buf(&sep_buffer, &sep);
	}
    }
    int center_height = total/2;
    current_bbox->upper = Move_bb(-tree->width,-center_height,
				  current_bbox->upper);
    current_bbox->lower = Move_bb(-tree->width,-center_height,
				  current_bbox->lower);
    // Add parent
    current_bbox->upper = Add_to_front(0,-tree->height/2, current_bbox->upper);
    current_bbox->lower = Add_to_front(0,tree->height/2, current_bbox->lower);

    // Place the children relative to parent
    int idx = 0;
    int new_x = -tree->width;
    int cur_y = -center_height;
    for(sch_draw_list_ptr ch = tree->fanins; ch != NULL; ch = ch->next) {
	ch->tree->x = new_x;
	ch->tree->y = cur_y;
	if( idx < COUNT_BUF(&sep_buffer) ) {
	    cur_y += *((int*) locate_buf(&sep_buffer, idx));
	}
	idx++;
    }
    return current_bbox;
}

/* absolute_placements places a network according to a start position */
static sch_draw_ptr
absolute_placement(int cur_x, int cur_y, sch_draw_ptr tree)
{
    tree->x += cur_x;
    tree->y += cur_y;
    for(sch_draw_list_ptr ch = tree->fanins; ch != NULL; ch = ch->next) {
	absolute_placement(tree->x, tree->y, ch->tree);
    }
    return tree;
}

/* Place takes a schematic drawing parent and places each associated node and
	leaf using correct bounding boxes.*/
sch_draw_ptr
Place(sch_draw_ptr parent)
{
    new_mgr(&bbox_rec_mgr, sizeof(bbox_rec));
    new_mgr(&list_rec_mgr, sizeof(list_rec));
    find_bbox(parent);
    sch_draw_ptr result = absolute_placement(0, 0, parent);
    free_mgr(&bbox_rec_mgr);
    free_mgr(&list_rec_mgr);
    return result;
}

/* Find_sep_distance finds the point of minimum distance between two lines. */
int
Find_sep_distance(list_ptr upper, list_ptr lower)
{
    if(upper == NULL || lower == NULL) {
	return 0;
    }
    int cur_upper_y = upper->y;
    int cur_lower_y = lower->y;
    int min_dist = 0;
    // Line sweeping 
    while(upper != NULL && lower != NULL) {
	if ( upper->x > lower->x ) {
	    int new_dist = cur_lower_y - upper->y;
	    if( new_dist > min_dist ) {
		min_dist = new_dist;
	    }
	    upper = upper->next;
	    if( upper != NULL ) {
		cur_upper_y = upper->y;
	    }
	} else if ( upper->x < lower->x ) {
	    int new_dist = lower->y - cur_upper_y;
	    if( new_dist > min_dist ) {
		min_dist = new_dist;
	    }
	    lower = lower->next;
	    if( lower != NULL ) {
		cur_lower_y = lower->y;
	    }
	} else {
	    int new_dist = lower->y - upper->y;
	    if( new_dist > min_dist ) {
		min_dist = new_dist;
	    }
	    lower = lower->next;
	    upper = upper->next;
	    if( upper != NULL ) {
		cur_upper_y = upper->y;
	    }
	    if( lower != NULL ) {
		cur_lower_y = lower->y;
	    }
	}
    }
    return min_dist;
}

/* Move_bb takes delta x and y values and moves each bbox that amount. */
list_ptr
Move_bb(int x, int y, list_ptr bbox)
{
    list_ptr start = bbox;
    if(bbox == NULL) {
	fprintf(stderr, "Empty bbox given\n");
	return NULL;
    }
    while(bbox != NULL) {
	bbox->x += x;
	if( bbox->y != MAX_LOCATION && bbox->y != -MAX_LOCATION ) {
	    bbox->y += y;
	}
	bbox = bbox->next;
    }
    return start;
}

/* Merge_min_bb takes two bboxes and determines the upper boundary limit. */
list_ptr
Merge_min_bb(list_ptr upper, list_ptr lower)
{
    list_ptr res;
    if(upper == NULL ) {
	if( lower == NULL) {
	    res = NULL;
	} else {
	    res = lower;
	}
    } else {
	if( lower == NULL ) {
	    res = upper;
	} else {
	    res = merge_min_bb_rec(upper->y, lower->y, upper, lower);
	}
    }
    if( res == NULL ) return NULL;
    list_ptr cur = res;
    while( cur->next != NULL ) {
	if( cur->next->x == cur->x ) {
	    if( cur->next->y < cur->y ) {
		cur->y = cur->next->y;
	    }
	    cur->next = cur->next->next;
	} else {
	    cur = cur->next;
	}
    }
    return res;
}

static list_ptr
merge_min_bb_rec(int cur_u_y, int cur_l_y, list_ptr upper, list_ptr lower)
{
    if(upper == NULL ) {
        if( lower == NULL) {
            return NULL;
        } else {
            return lower;
	}
    } else {
        if( lower == NULL ) {
            return upper;
        } else {
	    if( upper->x > lower->x ) {
		list_ptr tmp = (list_ptr) new_rec(&list_rec_mgr);
		tmp->x = upper->x;
		if( upper->y < cur_l_y ) {
		    tmp->y = upper->y;
		} else {
		    tmp->y = cur_l_y;
		}
		tmp->next = merge_min_bb_rec(upper->y, cur_l_y,
					     upper->next, lower);
		return tmp;
	    } else if( upper->x < lower->x ) {
		list_ptr tmp = (list_ptr) new_rec(&list_rec_mgr);
		tmp->x = lower->x;
		if( lower->y < cur_u_y ) {
		    tmp->y = lower->y;
		} else {
		    tmp->y = cur_u_y;
		}
		tmp->next = merge_min_bb_rec(cur_u_y, lower->y,
					     upper, lower->next);
		return tmp;
	    } else {
		list_ptr tmp = (list_ptr) new_rec(&list_rec_mgr);
		tmp->x = lower->x;
		if( lower->y < upper->y ) {
		    tmp->y = lower->y;
		} else {
		    tmp->y = upper->y;
		}
		tmp->next = merge_min_bb_rec(upper->y, lower->y,
					     upper->next, lower->next);
		return tmp;
	    }
	}
    }
}

/* Merge_max_bb takes two bboxes and determines the lower boundary limit. */
list_ptr
Merge_max_bb(list_ptr upper, list_ptr lower)
{
    list_ptr res;
    if(upper == NULL ) {
	if( lower == NULL) {
	    res = NULL;
	} else {
	    res = lower;
	}
    } else {
	if( lower == NULL ) {
	    res = upper;
	} else {
	    res = merge_max_bb_rec(upper->y, lower->y, upper, lower);
	}
    }
    if( res == NULL ) return NULL;
    list_ptr cur = res;
    while( cur->next != NULL ) {
	if( cur->next->x == cur->x ) {
	    if( cur->next->y > cur->y ) {
		cur->y = cur->next->y;
	    }
	    cur->next = cur->next->next;
	} else {
	    cur = cur->next;
	}
    }
    return res;
}

static list_ptr
merge_max_bb_rec(int cur_u_y, int cur_l_y, list_ptr upper, list_ptr lower)
{
    if(upper == NULL ) {
        if( lower == NULL) {
            return NULL;
        } else {
            return lower;
	}
    } else {
        if( lower == NULL ) {
            return upper;
        } else {
	    if( upper->x > lower->x ) {
		list_ptr tmp = (list_ptr) new_rec(&list_rec_mgr);
		tmp->x = upper->x;
		if( upper->y > cur_l_y ) {
		    tmp->y = upper->y;
		} else {
		    tmp->y = cur_l_y;
		}
		tmp->next = merge_max_bb_rec(upper->y, cur_l_y,
					     upper->next, lower);
		return tmp;
	    } else if( upper->x < lower->x ) {
		list_ptr tmp = (list_ptr) new_rec(&list_rec_mgr);
		tmp->x = lower->x;
		if( lower->y > cur_u_y ) {
		    tmp->y = lower->y;
		} else {
		    tmp->y = cur_u_y;
		}
		tmp->next = merge_max_bb_rec(cur_u_y, lower->y,
					     upper, lower->next);
		return tmp;
	    } else {
		list_ptr tmp = (list_ptr) new_rec(&list_rec_mgr);
		tmp->x = lower->x;
		if( lower->y > upper->y ) {
		    tmp->y = lower->y;
		} else {
		    tmp->y = upper->y;
		}
		tmp->next = merge_max_bb_rec(upper->y, lower->y,
					     upper->next, lower->next);
		return tmp;
	    }
	}
    }
}


/* ---------- Draw Long Wires ---------- */

/* Find_rightmost_repeats finds the furthest right repeat node of each
        name and returns a hash with all of them.*/
void
Find_rightmost_repeats(sch_draw_ptr tree)
{
    if( IS_REPEAT_ND(tree) )  {
	string name = tree->name;
	sch_draw_ptr cur = (sch_draw_ptr) find_hash(&rmost_repeats, name);
	if(cur == NULL) {
	    insert_hash(&rmost_repeats, (pointer) name, (pointer) tree);
	    return;
	}
	if( tree->x <= cur->x ) {
	    return;
	}
	delete_hash(&rmost_repeats, name);
	insert_hash(&rmost_repeats, name, (pointer) tree);
	return;
    }
    for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	Find_rightmost_repeats(lp->tree);
    }
}

/* Find_drivers_ptr goes through all the repeats and finds the origin or driver
	of that repeat node and stores a pointer to the driver. */
void
Find_drivers(sch_draw_ptr tree)
{
    dispose_hash(&drivers, NULLFCN);
    create_hash(&drivers, 100, str_hash, str_equ);
    find_drivers_rec(tree);
}

/* Calc_sep_dists goes through the whole network, and finds repeat-driver pairs
	and calculates the distance b/w them and adds this dist to a hash. */
void
Calc_sep_dists(hash_record_ptr repeats, sch_draw_ptr tree,
	       hash_record_ptr drivers, hash_record_ptr dists)
{
    sch_draw_ptr repeated = (sch_draw_ptr) find_hash(repeats, tree->name);
    sch_draw_ptr driver = (sch_draw_ptr) find_hash(drivers, tree->name);
    if(repeated != NULL && driver != NULL) {
	if( tree->x == driver->x && tree->y == driver->y ) {
	    int rx = repeated->x;
	    int dx = driver->x;
	    long sep = abs(rx - dx);
	    insert_hash(dists, (pointer) tree->name, (pointer) sep);
	}
    }
    for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	Calc_sep_dists(repeats, lp->tree, drivers, dists);
    }
}


static void
replace_rightmost_repeat(pointer key, pointer data)
{
    string	 name = (string) key;
    sch_draw_ptr repeat_node = (sch_draw_ptr) data;
    sch_draw_ptr driver = (sch_draw_ptr) find_hash(&drivers, name);
    int width = repeat_node->x - driver->x;
    char new_pfn[1000];
    sprintf(new_pfn, "draw_wire %d", width);
    repeat_node->pfn = uStrsave(&strings, new_pfn);
    repeat_node->pfn_type = Pfn2pfn_type(repeat_node->pfn);
    repeat_node->width = width;
}

/* Replace_with_wire replaces the right-most repeat node with a long wire
	whose length is determined from the driver location */
void
Replace_rmost_repeat_with_wires()
{
    scan_hash(&rmost_repeats, replace_rightmost_repeat);
}

static void
add_vertical_to_wire(pointer key, pointer data)
{
    string	 name   = (string) key;
    sch_draw_ptr wire   = (sch_draw_ptr) data;
    sch_draw_ptr driver = (sch_draw_ptr) find_hash(&drivers, name);
    int width = wire->x - driver->x;
    int height;
    if( wire->y > driver->y ) {
	height = -1*abs(wire->y - driver->y);
    } else {
	height = abs(wire->y - driver->y);
    }
    int x = wire->x - width;
    int y = wire->y;
    char new_pfn[1000];
    sprintf(new_pfn, "draw_vertical_wire %d", height);
    char* pfn = uStrsave(&strings, new_pfn);
    sch_draw_ptr vw = New_sch( LEAF, min_sep, height, x, y, name, pfn, NULL);
    if( IS_LOOP_SRC(driver) ) { vw->loop_src = driver; }
    sch_draw_list_ptr fis = (sch_draw_list_ptr) new_rec(&sch_draw_list_rec_mgr);
    fis->tree = vw;
    fis->next = NULL;
    wire->type = NODE;
    wire->fanins = fis;
}

void
Add_vertical_to_wires()
{
    scan_hash(&rmost_repeats, add_vertical_to_wire);
}


/* ---------- Equalize Widths ---------- */

/* bin_network takes a network tree head and goes through the whole network 
	adding each element to a corresponding bin based on the 'depth' of the
	element. */
void
bin_network(sch_draw_ptr tree, buffer_ptr buf, int pos)
{
    sch_draw_list_ptr nlp = (sch_draw_list_ptr) new_rec(&sch_draw_list_rec_mgr);
    nlp->tree = tree;
    nlp->next = *((sch_draw_list_ptr*) locate_buf(buf, pos));
    store_buf(buf, pos, &nlp);
    for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	bin_network(lp->tree, buf, pos+1);
    }
}

/* Equalize_width takes a network tree head, and goes through every level of
	the network putting each element in that level into a bin, finding the
	widest element in each bin, and setting all other elements in that bin
	to the widest width. */
void
Equalize_width(sch_draw_ptr tree)
{
    int pos = 0;
    buffer buf;
    new_buf(&buf, NUM_BINS, sizeof(sch_draw_ptr));
    bin_network(tree, &buf, pos);

    for(int i = 0; i < COUNT_BUF(&buf); i++) {
	sch_draw_list_ptr head = *((sch_draw_list_ptr*) locate_buf(&buf, i));
	if(head != NULL && head->tree != NULL) {
	    int largest_width = head->tree->width;
	    for(sch_draw_list_ptr lp = head; lp != NULL; lp = lp->next) {
		int cur_wid = lp->tree->width;
		if(cur_wid > largest_width) {
		    largest_width = cur_wid;
		}
	    }
	    for(sch_draw_list_ptr lp = head; lp != NULL; lp = lp->next) {
		lp->tree->width = largest_width;
	    }
	} 
    }
    free_buf(&buf);
}


/* ---------- Drawing Vertical Wires ---------- */

/* Connect_verticals goes through all the repeat nodes and replaces their pfn
	with a vertical wire connecting it to the horizontal line & driver. */
void
Connect_verticals(sch_draw_ptr tree)
{
    if( IS_REPEAT_ND(tree) ) {
	sch_draw_ptr wire = (sch_draw_ptr) find_hash(&rmost_repeats,tree->name);
	int dist = 0;
	if(tree->y > wire->y) {
	    dist = -1*abs(tree->y - wire->y);
	} else if(tree->y < wire->y) {
	    dist = abs(tree->y - wire->y);
	}
	char new_pfn[1000];
	sprintf(new_pfn, "draw_vertical_wire %d", dist);
	char* pfn = uStrsave(&strings, new_pfn);
	tree->pfn = pfn;
	tree->pfn_type = Pfn2pfn_type(tree->pfn);
	return;
    }
    for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	Connect_verticals(lp->tree);
    }
}

/* ---------- Correcting Vertical Overlaps ---------- */

static int
int_cmp(const void *pi, const void *pj)
{
    int *i = (int *) pi;
    int *j = (int *) pj;
    return( *j - *i );
}

void
Expand_for_vertical_wires(sch_draw_ptr tree)
{
    int sz;
    /* Correct for wire overlaps */
    create_hash(&verticals, 100, int_hash, int_equ);
    new_buf(&vloc_buf, 100, sizeof(int));
    bin_verticals(tree);
    sz = COUNT_BUF(&vloc_buf);
    // Sort x-locations for vertical wires
    if( sz != 0 ) {
	new_mgr(&space_tree_rec_mgr, sizeof(space_tree_rec));
	qsort(START_BUF(&vloc_buf), sz, sizeof(int), int_cmp);
	new_buf(&vspace_buf, 100, sizeof(int));
	int *ip;
	int offset = 0;
	FOR_BUF(&vloc_buf, int, ip) {
	    int vwires = 0;
	    for(sch_draw_list_ptr lp = find_hash(&verticals, INT2PTR(*ip));
		lp != NULL;
		lp = lp->next)
	    {
		vwires++;
	    }
	    offset += (vwires * min_sep);
	    push_buf(&vspace_buf, &offset);
	}
	// Build add-space tree
	space_tree_ptr spt = build_space_tree(0, sz-1, 0);
	make_room_for_verticals(spt, tree);
	// Separate out the vertical wires
	scan_hash(&verticals, adjust_verticals);
    }
}

static int
translate_x_location(space_tree_ptr spt, int x)
{
    if( spt->type == SPT_LEAF ) {
	return( x - spt->x );
    }
    if( x <= spt->x )
	return( translate_x_location(spt->left, x) );
    else
	return( translate_x_location(spt->right, x) );
}

static void
make_room_for_verticals(space_tree_ptr spt, sch_draw_ptr tree)
{
    if( IS_WIRE(tree) ) {
	tree->x = translate_x_location(spt, tree->x+1);
    } else {
	tree->x = translate_x_location(spt, tree->x);
    }
    for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	make_room_for_verticals(spt, lp->tree);
    }
}


static space_tree_ptr
build_space_tree(int m, int M, int cur_sep)
{
    if( m > M ) {
	space_tree_ptr res = (space_tree_ptr) new_rec(&space_tree_rec_mgr);
	res->type = SPT_LEAF;
	res->x = cur_sep;
	res->left = NULL;
	res->right = NULL;
	return res;
    }
    int idx = (m+M)/2;
    int x = *((int *) locate_buf(&vloc_buf, idx));
    int new_cur_sep = *((int *) locate_buf(&vspace_buf, idx));
    space_tree_ptr res = (space_tree_ptr) new_rec(&space_tree_rec_mgr);
    res->type = SPT_BRANCH;
    res->x = x;
    res->left = build_space_tree(idx+1, M, new_cur_sep);
    res->right = build_space_tree(m, idx-1, cur_sep);
    return res;
}

/* bin_verticals bins all the vertical wires according to their x-location */
static void
bin_verticals(sch_draw_ptr tree)
{
    if( IS_VERTICAL_WIRE(tree) ) {
	sch_draw_list_ptr head = 
	    (sch_draw_list_ptr) find_hash(&verticals, INT2PTR(tree->x));
	if(head == NULL) {
	    sch_draw_list_ptr addition = new_rec(&sch_draw_list_rec_mgr);
	    addition->tree = tree;
	    addition->next = NULL;
	    insert_hash(&verticals, INT2PTR(tree->x), (pointer) addition);
	    push_buf(&vloc_buf, &(tree->x));
	} else {
	    sch_draw_list_ptr addition = new_rec(&sch_draw_list_rec_mgr);
	    addition->tree = tree;
	    addition->next = head->next;
	    head->next = addition;
	}
    }
    for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	bin_verticals(lp->tree);
    }
}

static void
adjust_verticals(pointer key, pointer data)
{
    hash_record track_tbl;
    create_hash(&track_tbl, 10, str_hash, str_equ);
    sch_draw_list_ptr head = (sch_draw_list_ptr) data;
    int sep = min_sep;
    while( head != NULL ) {
	int csep = PTR2INT(find_hash(&track_tbl, head->tree->name));
	if( csep == 0 ) {
	    csep = sep+1;
	    sep += min_sep;
	    insert_hash(&track_tbl, (pointer) head->tree->name, INT2PTR(csep));
	}
	head->tree->x += (csep-1);
	if( head->tree->loop_src != NULL ) {
	    head->tree->loop_src->x = head->tree->x;
	}
	head = head->next;
    }
    dispose_hash(&track_tbl, NULLFCN);
}

/* ---------- Correct Loops ---------  */

sch_draw_ptr
find_origin(char* name, sch_draw_ptr tree)
{
    //if name matches, and not repeat_nd, return it
    if( name == tree->name ) {
	return tree;
    }
    for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next)
    {
	find_origin(name, lp->tree);
    }
}

void
Break_loops(sch_draw_ptr tree)
{
    hash_record seen_tbl;
    create_hash(&seen_tbl, 100, str_hash, str_equ);
    hash_record done_tbl;
    create_hash(&done_tbl, 100, str_hash, str_equ);
    hash_record new_nodes;
    create_hash(&new_nodes, 100, str_hash, str_equ);
    break_loops_rec(tree, &seen_tbl, &new_nodes, &done_tbl);
    rename_loop_repeat_nodes(tree, &new_nodes);
    dispose_hash(&seen_tbl, NULLFCN);
    dispose_hash(&new_nodes, NULLFCN);
    dispose_hash(&done_tbl, NULLFCN);
}

static void
rename_loop_repeat_nodes(sch_draw_ptr tree, hash_record_ptr done)
{
    if( IS_REPEAT_ND(tree) ) {
	char* fb_name = (string) find_hash(done, (pointer) tree->name);
	if( fb_name != NULL ) {
	    tree->name = fb_name;
	}
    } else {
	for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	    rename_loop_repeat_nodes(lp->tree, done);
	} 
    }
}

static void
break_loops_rec(sch_draw_ptr tree,
		hash_record_ptr seen,
		hash_record_ptr new_nodes,
		hash_record_ptr done)
{
    if( tree == NULL ) return;
    if( IS_REPEAT_ND(tree) ) {
	sch_draw_ptr driver = find_hash(seen, (pointer) tree->name);
	if(driver != NULL) {
	    char* fb_name = (string) find_hash(new_nodes, (pointer) tree->name);
	    if( fb_name != NULL ) {
		tree->name = fb_name;
	    } else {
		fb_name = tree->name;
		char new_name[100];
		sprintf(new_name, "FeEdBaCk_%s", tree->name);
		tree->name = uStrsave(&strings, new_name);
		insert_hash(new_nodes, (pointer) fb_name, (pointer) tree->name);
		tree->pfn = uStrsave(&strings, "draw_loop_source");
		tree->pfn_type = Pfn2pfn_type(tree->pfn);
		sch_draw_ptr new_driver =
				    (sch_draw_ptr) new_rec(&sch_draw_rec_mgr);
		*new_driver = *driver;
		new_driver->loop_src = NULL;
		sprintf(new_name, "AcTuAlOuT_%s", fb_name);
		new_driver->name = uStrsave(&strings, new_name);
		
		driver->pfn = uStrsave(&strings, "draw_loop_merge");
		driver->pfn_type = Pfn2pfn_type(driver->pfn);

		sch_draw_ptr repeat_to_loop_src =
				(sch_draw_ptr) new_rec(&sch_draw_rec_mgr);
		repeat_to_loop_src->loop_src = NULL;
		repeat_to_loop_src->type = LEAF;
		repeat_to_loop_src->name = tree->name;
		repeat_to_loop_src->pfn = uStrsave(&strings, "draw_repeat_nd");
		repeat_to_loop_src->pfn_type =
					Pfn2pfn_type(repeat_to_loop_src->pfn);
		repeat_to_loop_src->width = tree->width;
		repeat_to_loop_src->height = tree->height;
		repeat_to_loop_src->x = 0;
		repeat_to_loop_src->y = 0;
		repeat_to_loop_src->fanins = NULL;
		sch_draw_list_ptr fanin1 = 
		    (sch_draw_list_ptr) new_rec(&sch_draw_list_rec_mgr);
		sch_draw_list_ptr fanin2 =
		    (sch_draw_list_ptr) new_rec(&sch_draw_list_rec_mgr);
		driver->fanins = fanin1;
		fanin1->next = fanin2;
		fanin2->next = NULL;
		fanin1->tree = new_driver;
		fanin2->tree = repeat_to_loop_src;
	    }
	} else {
	    driver = (sch_draw_ptr) find_hash(&drivers, tree->name);
	    break_loops_rec(driver, seen, new_nodes, done);
	}
    } else {
	if( find_hash(done, (pointer) tree->name) ) { return; }
	insert_hash(done, (pointer) tree->name, (pointer) tree->name);
	insert_hash(seen, (pointer) tree->name, (pointer) tree);
	for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	    break_loops_rec(lp->tree, seen, new_nodes, done);
	} 
	delete_hash(seen, (pointer) tree->name);
    }
}


static void
find_drivers_rec(sch_draw_ptr tree)
{   
    if( !IS_REPEAT_ND(tree) && !IS_WIRE(tree) ) {
	insert_hash(&drivers, (pointer) tree->name, (pointer) tree);
    }
    for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	find_drivers_rec(lp->tree);
    }
}
