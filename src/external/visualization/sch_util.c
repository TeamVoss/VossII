//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************
*       Original author: Peter Seger 2017       *
************************************************/

#include "sch_util.h"
#include "list_util.h"
#include "strmgr.h"

/* --Record Managers-- */
static rec_mgr	bbox_rec_mgr;
static rec_mgr	sch_draw_rec_mgr;
static rec_mgr	sch_draw_list_rec_mgr;
ustr_mgr	strings;


/* --Private Methods-- */
char*		type_to_str(sch_type type);
void		find_fanins(buffer_ptr buf, sch_draw_ptr tree);
void		pprint_fn(sch_draw_ptr tree, FILE *fp, buffer_ptr buf);


void
Init_sch()
{
    new_mgr(&sch_draw_rec_mgr, sizeof(sch_draw_rec));
    new_mgr(&sch_draw_list_rec_mgr, sizeof(sch_draw_list_rec));
    new_mgr(&bbox_rec_mgr, sizeof(bbox_rec));
    new_ustrmgr(&strings);
}


pfn_type
Pfn2pfn_type(string pfn)
{
    pfn_type res;
    if( strstr(pfn, "draw_output") != NULL ) {
	res = PFN_OUTPUT;
    } else if( strstr(pfn, "draw_loop_source") != NULL ) {
	res = PFN_LOOP_SRC;
    } else if( strstr(pfn, "draw_repeat_nd") != NULL ) {
	res = PFN_REPEAT_ND;
    } else if( strstr(pfn, "draw_wire") != NULL ) {
	res = PFN_WIRE;
    } else if( strstr(pfn, "draw_split") != NULL ) {
	res = PFN_SPLIT;
    } else if( strstr(pfn, "draw_vertical_wire") != NULL ) {
	res = PFN_VERTICAL_WIRE;
    } else {
	res = PFN_OTHER;
    }
    return res;
}

// New_sch creates a new sch_draw item with the given attributes.
sch_draw_ptr
New_sch(sch_type type, int w, int h, int x, int y, char* name, string pfn, 
		sch_draw_list_ptr fanins)
{
    sch_draw_ptr ans = (sch_draw_ptr) new_rec(&sch_draw_rec_mgr);
    ans->type = type;
    ans->bin_num = 0;
    ans->name = uStrsave(&strings, name);
    ans->width = w;
    ans->height = h;
    ans->x = x;
    ans->y = y;
    ans->pfn = uStrsave(&strings, pfn);
    ans->pfn_type = Pfn2pfn_type(ans->pfn);
    ans->fanins = fanins;
    ans->loop_src = NULL;
    return ans;
}

// Add_to_front_network adds a new sch_draw item to the front of a network.
sch_draw_list_ptr
Add_to_front_network(sch_draw_ptr tree, sch_draw_list_ptr network)
{
    sch_draw_list_ptr res = (sch_draw_list_ptr) new_rec(&sch_draw_list_rec_mgr);
    res->tree = tree;
    res->next = network;
    return res;
}


// Add_to_back_network adds a new sch_draw item to the back of a network.
sch_draw_list_ptr
Add_to_back_network(sch_draw_ptr tree, sch_draw_list_ptr network)
{
    sch_draw_list_ptr res;
    if(network == NULL) {
	res = (sch_draw_list_ptr) new_rec(&sch_draw_list_rec_mgr);
	res->tree = tree;
	res->next = NULL;
	return res;
    } else {
	sch_draw_list_ptr temp;
	temp = network;
	while(temp->next != NULL) {
	    temp = temp->next;
	}
	res = (sch_draw_list_ptr) new_rec(&sch_draw_list_rec_mgr);
	res->tree = tree;
	res->next = NULL;
	temp->next = res;
    }
    return network;
}


static int count = 1;
static char pfn_buf[1024];


/* Print_network goes through a whole network and prints the information
	contained in each node and leaf.*/
void
print_network_rec(int indent, sch_draw_ptr tree)
{
    if(tree == NULL) {
	return;
    }
    char* name = tree->name;
    char* type = type_to_str(tree->type);
    int x = tree->x;
    int y = tree->y;
    int w = tree->width;
    int h = tree->height;
    char* pfn = tree->pfn;
    while( *pfn && *pfn == '\n' ) { pfn++; }
    char *p = pfn_buf;
    int len = 0;
    while( *pfn ) {
	if( len < 40 ) {
	    if( *pfn == '\n' ) {
		*p = ' ';
	    } else {
		*p = *pfn;
	    }
	    p++;
	    len++;
	}
	pfn++;
    }
    if( len == 40 ) {
	*p = '.'; p++;
	*p = '.'; p++;
	*p = '.'; p++;
    }
    *p = '\0';
    fprintf(stderr,
	    "%*s%02d %s Type: %s : (%d,%d), w:%d h:%d PFN: %s\n",
	    indent, "", count, name, type, x, y, w, h, pfn_buf);
    count++;
    for(sch_draw_list_ptr ls = tree->fanins; ls != NULL; ls = ls->next) {
	print_network_rec(indent+2, ls->tree);
    }
}

void
Print_network(string name, sch_draw_ptr tree)
{
#ifdef DBG_INFO
    count = 1;
    fprintf(stderr, "\n====== %s ======\n", name);
#ifdef VERBOSE_DBG
    print_network_rec(1,tree);
#endif
#endif
}

// type_to_str takes a schematic type and returns the string value.
char*
type_to_str(sch_type type)
{
    switch(type) {
	case NODE : return "NODE";
	case LEAF : return "LEAF";
	default : fprintf(stderr, "Not a valid type given");
    }
}


// type_to_str2 takes a schematic type and returns the full string value.
char*
type_to_str2(sch_type type)
{
    switch(type) {
	case NODE : return "SCH_INT";
	case LEAF : return "SCH_LEAF";
	default : fprintf(stderr, "Not a valid type given");
    }
}

// Pretty_printer arranges the network into a printable format
void
Pretty_printer(sch_draw_ptr tree, char* filename)
{
    FILE *fp;
    fp = fopen(filename, "w");

    if(tree != NULL) {
	buffer buf;
	new_buf(&buf, 100, sizeof(char*));
	resize_buf(&buf, 100);
	fprintf(fp, "display_drawing ");
	pprint_fn(tree, fp, &buf);
	fprintf(fp, ";\n");
    } else {
	fprintf(stderr, "Empty tree given");
	fclose(fp);
	return;
    }
    fclose(fp);
}

// pprint_fn acts as the helper function for Pretty_printer
void
pprint_fn(sch_draw_ptr tree, FILE *fp, buffer_ptr buf)
{
    char* type = type_to_str2(tree->type);
    int x = tree->x;
    int y = tree->y;
    char* name = tree->name;
    char* pfn = tree->pfn;
    if( tree->type == LEAF ) {
	fprintf(fp, "(%s (0%+d,0%+d) \"%s\" \"%s\")\n", type, x, y, name, pfn);
    } else {
	char sep = ' ';
	fprintf(fp, "(%s (0%+d,0%+d) \"%s\" \"%s\" [\n", type, x, y, name, pfn);
	for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	    fprintf(fp, "%c", sep);
	    sep = ',';
	    pprint_fn(lp->tree, fp, buf);
	}
	fprintf(fp, "])\n"); 
    }
}

// find_fanins goes through all fanins for a node and adds them to a buffer
void
find_fanins(buffer_ptr buf, sch_draw_ptr tree)
{
    char* name = tree->name;
    pop_buf(buf, &name);

    for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	find_fanins(buf, lp->tree);
    }
}

/* ADDITIONS -------- */


/* print function for scan_hash call */
void
print_fn(pointer key, pointer data)
{
    char* k = (char*) key;
    sch_draw_ptr t = (sch_draw_ptr) data;
    if( t->fanins != NULL ) {
	fprintf(stderr, "Name: %s  --->  ", t->name);
	sch_draw_list_ptr next = t->fanins;
	for(sch_draw_list_ptr i = next; i != NULL; i = i->next)
	{
	    sch_draw_ptr temp = i->tree;
            fprintf(stderr, "Name: %s \n", temp->name);
	}
	fprintf(stderr, "\n");
    } else {
	fprintf(stderr, "Name: %30s  |  Fanins: %6p\n", t->name, t->fanins);
	return;
    }
}

/* print_fn2 prints the pfn of every element in a hash table. */
void
print_fn2(pointer key, pointer data)
{
    char* k = (char*) key;
    sch_draw_ptr t = (sch_draw_ptr) data;
    fprintf(stderr, "Name: %30s  |  PFN: %30s  X:%5d Y:%5d\n", k, t->pfn, t->x, t->y);
}

/* Print_hash prints all the buckets in the given hash table. */
void
Print_fanins(hash_record_ptr hash)
{
    scan_hash(hash, print_fn); 
}

/* Print_pfn prints all the pfn in the given hash table. */
void
Print_pfn(hash_record_ptr hash)
{
    scan_hash(hash, print_fn2);
}

/* print_drivers_fn provides a function for printing drivers in a hash table */
void
print_drivers_fn(pointer key, pointer data)
{
    char* name = (char*) key;
    sch_draw_ptr tree = (sch_draw_ptr) data;
    int x = tree->x;
    int y = tree->y;
    fprintf(stderr, "Name: %30s   X:%7d    Y:%7d\n", name, x, y);
}

/* Print_drivers prints all driver's name, x, and y coordinates in a 
	hash table */
void
Print_drivers(hash_record_ptr drivers)
{
    scan_hash(drivers, print_drivers_fn);
}

/* print_dists_fn provides a function for printing distances in a hash table */
void
print_dists_fn(pointer key, pointer data)
{
    char* name = (char*) key;
    long dist = (long) data;
    fprintf(stderr, "Name: %30s  |  Dist: %4ld\n", name, dist);
}

/* Print_dists prints the name and distance for every repeat-driver pair */
void
Print_dists(hash_record_ptr dists)
{
    scan_hash(dists, print_dists_fn);
}


/* Copy a network */
sch_draw_ptr
copy_draw_tree(sch_draw_ptr tree)
{
    if(tree == NULL) {
	return NULL;
    }
    sch_draw_ptr ans = (sch_draw_ptr) new_rec(&sch_draw_rec_mgr);
    ans->type = tree->type;
    ans->bin_num = tree->bin_num;
    ans->name = uStrsave(&strings, tree->name);
    ans->width = tree->width;
    ans->height = tree->height;
    ans->x = tree->x;
    ans->y = tree->y;
    ans->pfn = uStrsave(&strings, tree->pfn);
    ans->pfn_type = Pfn2pfn_type(ans->pfn);
    ans->fanins = NULL;
    sch_draw_list_ptr *destp = &(ans->fanins);
    sch_draw_list_ptr cur = tree->fanins;
    while( cur != NULL ) {
	sch_draw_list_ptr new;
	new = (sch_draw_list_ptr) new_rec(&sch_draw_list_rec_mgr);
	new->tree = copy_draw_tree(cur->tree);
	new->next = NULL;
	destp = &(new->next);
	cur = cur->next;
    }
    ans->loop_src = copy_draw_tree(tree->loop_src);
    return ans;
}
