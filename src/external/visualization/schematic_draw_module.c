//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************
*       Original author: Peter Seger 2017       *
************************************************/
    
#include <sys/time.h>

#include <tcl.h>
#include <tk.h>

#include "list_util.h"
#include "place_bounds.h"
#include "place.h"
#include "sch_util.h"
#include "strmgr.h"

#define PTR2INT(p)      ((int) ((long) (p)))
#define INT2PTR(i)      ((void*) ((long) (i)))

extern ustr_mgr         strings;

static buffer	    sch_ptr_buf;
static char	    cmd_buf[8*4096]; // Temporary bug workaround for large pfns
static rec_mgr	    pair_list_rec_mgr;

static hash_record  pfn2width;
static hash_record  pfn2height;

int
create_sch(ClientData clientData, Tcl_Interp *interp,
	   int objc, Tcl_Obj *const objv[])
{
    (void) clientData;
    if( objc != 1 ) {
        Tcl_WrongNumArgs(interp,objc,objv, "create_sch");
	fprintf(stderr, "Wrong number of arguments given. Breaking.");
        return TCL_ERROR;
    }
    Init_sch();
    Init_list();
    Init_place();
    new_buf(&sch_ptr_buf, 100, sizeof(sch_draw_ptr));
    new_mgr(&pair_list_rec_mgr, sizeof(pair_list_rec));
    return TCL_OK;
}

static int
max(int x, int y)
{
    return( ( x > y )? x : y );
}

int
add_sch_object(ClientData clientData, Tcl_Interp *interp,
	       int objc, Tcl_Obj *const objv[])
{
    (void) clientData;
    // add_sch_object type name pfn fanins
    if( objc != 5 ) {
        Tcl_WrongNumArgs(interp,objc,objv, "add_sch_object type name pfn fins");
	fprintf(stderr, "Wrong number of arguments given. Cannot build obj.");
        return TCL_ERROR;
    }

    char *type     = Tcl_GetString(objv[1]);
    char *name     = Tcl_GetString(objv[2]);
    char *pfn      = Tcl_GetString(objv[3]);
    Tcl_Obj *fanins = objv[4];

    
    int width, height;
    if( (height = PTR2INT(find_hash(&pfn2height, pfn))) == 0 ) {
	sprintf(cmd_buf, "get_width_height {%s}", pfn);
	if( Tcl_Eval(interp, cmd_buf) != TCL_OK ) {
	    Tcl_AppendResult(interp, "Failed to evaluate '", pfn, "'", NULL);
	    return TCL_ERROR;
	}
	const char *res = Tcl_GetStringResult(interp);
	int lx, ly, ux, uy;
	if( sscanf(res, "%d %d %d %d", &lx, &ly, &ux, &uy) != 4 ) {
	    Tcl_AppendResult(interp, "Failed to extract width/height from: ",
				     res, NULL);
	    return TCL_ERROR;
	}
	if( strstr(pfn, "draw_output") != NULL ) {
	    width = 20;
	} else if( strstr(pfn, "draw_loop_source") != NULL ) {
	    width = 0;
	} else {
	    width = ux-lx+20;
	}
	height = max((uy-ly+2),10);
	if( strstr(pfn, "draw_repeat_nd") != NULL ) {
	    height = max(height,20);
	}
	insert_hash(&pfn2width, pfn, INT2PTR(width));
	insert_hash(&pfn2height, pfn, INT2PTR(height));
    } else {
	width = PTR2INT(find_hash(&pfn2width, pfn));
    }

    int nbr_fanins;
    if( Tcl_ListObjLength(interp, fanins, &nbr_fanins) != TCL_OK ) {
	Tcl_AppendResult(interp,
			 "Invalid fanin list: Cannot determine length", NULL);
	fprintf(stderr, "Invalid fanin list: Cannot determine length.");
	return TCL_ERROR;
    }
    sch_draw_list_ptr fins = NULL;
    for(int i = 0; i < nbr_fanins; i++) {
	Tcl_Obj *opp;
	if( Tcl_ListObjIndex(interp, fanins, i, &opp) != TCL_OK ) {
	    char idx[10];
	    sprintf(idx, "%d", i);
	    Tcl_AppendResult(interp,
			     "Cannot access element ", idx,  "in fanins", NULL);
	    fprintf(stderr, "Cannot acces element %s", idx);
	    return TCL_ERROR;
	}
	int fidx = atoi(Tcl_GetString(opp));
	sch_draw_ptr fanin = *((sch_draw_ptr *) locate_buf(&sch_ptr_buf,fidx));
	fins = Add_to_back_network(fanin, fins);
    }
    sch_type ntype;
    if( strcmp(type, "LEAF") == 0 ) {
	ntype = LEAF;
    } else if( strcmp(type, "NODE") == 0 ) {
	ntype = NODE;
    } else {
	Tcl_AppendResult(interp, "Unknown sch type: ", type, NULL);
	fprintf(stderr, "Unknown sch type given");
	return TCL_ERROR;
    }
    sch_draw_ptr res = New_sch(ntype, width, height, 0, 0, name, pfn, fins);
    int res_idx = COUNT_BUF(&sch_ptr_buf);
    push_buf(&sch_ptr_buf, &res);
    char sres_idx[10];
    sprintf(sres_idx, "%d", res_idx);
    Tcl_SetResult(interp, sres_idx, TCL_VOLATILE);
    return( TCL_OK );
}


static char *
strip_dummy_prefixes(char *vec)
{
  restart:
    if( strncmp(vec, "AcTuAlOuT_", strlen("AcTuAlOuT_")) == 0 ) {
	vec = vec + strlen("AcTuAlOuT_");
	goto restart;
    }
    if( strncmp(vec, "FeEdBaCk_", strlen("FeEdBaCk_")) == 0 ) {
	vec = vec + strlen("FeEdBaCk_");
	goto restart;
    }
    return vec;
}

static bool
get_coords(Tcl_Interp *interp, Tcl_Obj *locs, pair_list_ptr *resp, int *cntp)
{
    int nbr_inputs;
    if( Tcl_ListObjLength(interp, locs, &nbr_inputs) != TCL_OK ) {
	*resp = NULL;
	return FALSE;
    }
    *cntp = nbr_inputs/2;
    pair_list_ptr res = NULL;
    for(int i = nbr_inputs/2-1; i >= 0; i--) {
	Tcl_Obj *xval;
	if( Tcl_ListObjIndex(interp, locs, 2*i, &xval) != TCL_OK ) {
	    *resp = NULL;
	    return FALSE;
	}
	int x;
	if( Tcl_GetIntFromObj(interp, xval, &x) != TCL_OK ) {
	    *resp = NULL;
	    return FALSE;
	}
	Tcl_Obj *yval;
	if( Tcl_ListObjIndex(interp, locs, 2*i+1, &yval) != TCL_OK ) {
	    *resp = NULL;
	    return FALSE;
	}
	int y;
	if( Tcl_GetIntFromObj(interp, yval, &y) != TCL_OK ) {
	    *resp = NULL;
	    return FALSE;
	}
	pair_list_ptr p = (pair_list_ptr) new_rec(&pair_list_rec_mgr);
	p->x = x;
	p->y = y;
	p->next = res;
	res = p;
    }
    *resp = res;
    return TRUE;
}


static bool 
draw_pfn(Tcl_Interp *interp, char *cmd,
			     pair_list_ptr *iresp, int *nbr_inputsp,
			     pair_list_ptr *oresp, int *nbr_outputsp)
{
    if( Tcl_Eval(interp, cmd) != TCL_OK ) {
	*iresp = NULL;
	*oresp = NULL;
	return FALSE;
    }
    Tcl_Obj *tcl_res = Tcl_GetObjResult(interp);
    Tcl_Obj *ilocs;
    if( Tcl_ListObjIndex(interp, tcl_res, 0, &ilocs) != TCL_OK ) {
	*iresp = NULL;
	*oresp = NULL;
	return FALSE;
    }
    if( !get_coords(interp, ilocs, iresp, nbr_inputsp) ) {
	*iresp = NULL;
	*oresp = NULL;
	return FALSE;
    }
    Tcl_Obj *olocs;
    if( Tcl_ListObjIndex(interp, tcl_res, 1, &olocs) != TCL_OK ) {
	*iresp = NULL;
	*oresp = NULL;
	return FALSE;
    }
    if( !get_coords(interp, olocs, oresp, nbr_outputsp) ) {
	*iresp = NULL;
	*oresp = NULL;
	return FALSE;
    }
    return TRUE;
}

static bool
draw_schematics(Tcl_Interp *interp, char *canvas, sch_draw_ptr tree)
{
    char *tag = strip_dummy_prefixes(tree->name);
    // Adjust length of long wires
    if( IS_WIRE(tree) ) {
	if( tree->fanins != NULL ) {
	    int vertical_x = tree->fanins->tree->x;
	    int new_len = tree->x - vertical_x;
	    if( new_len <= 0 ) {
		tree->pfn = uStrsave(&strings, "draw_wire 0");
	    } else {
		char new_pfn[1000];
		sprintf(new_pfn, "draw_wire %d", new_len);
		tree->pfn = uStrsave(&strings, new_pfn);
	    }
	}
    }
    sprintf(cmd_buf, "%s %s %s %d %d", tree->pfn, canvas, tag, tree->x,tree->y);
    pair_list_ptr my_inps, my_outs;
    int nbr_inputs;
    int nbr_outputs;
    if( draw_pfn(interp, cmd_buf, &my_inps, &nbr_inputs,
				  &my_outs, &nbr_outputs) != TRUE ) {
	return FALSE;
    }
    if( tree->type == LEAF ) {
	return TRUE;
    }
    if( IS_OUTPUT(tree) ) {
	// An output
	int i = 0;
	for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	    if( draw_schematics(interp, canvas, lp->tree) != TRUE ) {
		return FALSE;
	    }
	    char *driver_name = lp->tree->name;
	    char *driver_tag = strip_dummy_prefixes(driver_name);
	    int driver_x = lp->tree->x;
	    int driver_y = lp->tree->y;
	    int driven_x = my_inps->x;
	    int driven_y = my_inps->y;
	    my_inps = my_inps->next;
	    sprintf(cmd_buf, "draw_output1 [fl_tag2vec %s %s] %s %s %d %d",
		    canvas, strip_dummy_prefixes(driver_name), canvas,
		    driver_tag, tree->x, driver_y);
	    if( Tcl_Eval(interp, cmd_buf) != TCL_OK ) { return FALSE; }
	    sprintf(cmd_buf, "out_connect %s %s %d %d %d %d %d %d %s",
		    canvas, driver_tag,
		    driver_x, driver_y, tree->x, driver_y, 
		    i, nbr_inputs, tag);
	    if( Tcl_Eval(interp, cmd_buf) != TCL_OK ) {
		return FALSE;
	    }
	    i++;
	}
    } else {
	// An regular internal node
	int i = 1;
	for(sch_draw_list_ptr lp = tree->fanins; lp != NULL; lp = lp->next) {
	    if( draw_schematics(interp, canvas, lp->tree) != TRUE ) {
		return FALSE;
	    }
	    char *driver_tag = strip_dummy_prefixes(lp->tree->name);
	    int driver_x;
	    int driver_y;
	    if(IS_WIRE(lp->tree) &&
		       strcmp(lp->tree->pfn,"draw_wire 0") == 0 &&
		       lp->tree->fanins != NULL
	      )
	    {
		driver_x = lp->tree->fanins->tree->x;
		driver_y = lp->tree->fanins->tree->y;
	    } else {
		driver_x = lp->tree->x;
		driver_y = lp->tree->y;
	    }
	    if( !my_inps ) {
		int cnt = 0;
		for(sch_draw_list_ptr lp2 = tree->fanins;
		    lp2 != NULL; lp2 = lp2->next)
		{
		    cnt++;
		}
		char tmp[30];
		sprintf(tmp, ". Got %d\n\n", cnt);
		Tcl_AppendResult(interp, "\n\nWrong number of inputs to ",
					  tree->pfn, tmp, NULL);
		return FALSE;
	    }
	    int driven_x = my_inps->x;
	    int driven_y = my_inps->y;
	    my_inps = my_inps->next;
	    if( driven_x > driver_x ) {
		sprintf(cmd_buf, "connect %s %s %d %d %d %d %d %d %s",
			canvas, driver_tag,
			driver_x, driver_y, driven_x, driven_y, 
			i, nbr_inputs, tag);
		if( Tcl_Eval(interp, cmd_buf) != TCL_OK ) {
		    return FALSE;
		}
	    }
	    i++;
	}
    }
    return TRUE;
}

int
draw_network(ClientData clientData, Tcl_Interp *interp,
	     int objc, Tcl_Obj *const objv[])
{
    (void) clientData;
    // draw_network canvas tree
    if( objc != 3 ) {
        Tcl_WrongNumArgs(interp,objc,objv,
			 "draw_network canvas network");
	fprintf(stderr, "Wrong number of arguments draw_network. Breaking.");
        return TCL_ERROR;
    }

    Setup_draw_network();

    char *canvas = Tcl_GetString(objv[1]);
    int ntwk_idx = atoi(Tcl_GetString(objv[2]));
    sch_draw_ptr tree = *((sch_draw_ptr *) locate_buf(&sch_ptr_buf, ntwk_idx));

    //Print_network("Initial tree", tree);

    Break_loops(tree);
    //Print_network("After loop breaking", tree);

    // Add space for inputs
    Adjust_widths(tree);
    //Print_network("After Adjust_widths", tree);

    Rank_order_tree(tree, 0); // Initial pos 0
    //Print_network("After Rank_order_tree", tree);

    // Find every (proper) driver
    Find_drivers(tree);
    //Print_network("After Find_drivers", tree);

    // Move drivers to left-most (according to bin number)
    int max_cnt = 100;
    while( (max_cnt > 0) && (Move_driver_leftmost(tree) > 0) ) {
	Break_loops(tree);
	Find_drivers(tree);
	max_cnt--;
    }
    if( max_cnt == 0 ) {
	fprintf(stderr, "WARNING: Iteration in Move_driver_leftmost\n");
    }
    //Print_network("After Move_driver_leftmost", tree);

    Rank_order_tree(tree, 0); // Initial pos 0
//    Remove_drive_repeat_overlaps(tree);

    Equalize_width(tree);
    //Print_network("After Equalize width tree", tree);

    /* Initial placement */
    tree = Place(tree);
    //Print_network("After initial placement", tree);

    // Find right-most repeat nodes
    Find_rightmost_repeats(tree);

    // Replace right-most repeat nodes with long wires to driver locations.
    Replace_rmost_repeat_with_wires();

    /* Second placement (with long wires) */
    tree = Place(tree);
    //Print_network("After second placement", tree);

    Add_vertical_to_wires();
    //Print_network("After Add_vertical_to_wires", tree);

    Connect_verticals(tree);

    Expand_for_vertical_wires(tree);
    //Print_network("Final", tree);

    sprintf(cmd_buf, "%s delete all", canvas);
    if( Tcl_Eval(interp, cmd_buf) != TCL_OK ) {
	return TCL_ERROR;
    }
    if( draw_schematics(interp, canvas, tree) != TRUE ) {
	return TCL_ERROR;
    }
    if( Tcl_Eval(interp, "update") != TCL_OK ) { return TCL_ERROR; }
    sprintf(cmd_buf, "set_scrollregion %s", canvas);
    if( Tcl_Eval(interp, cmd_buf) != TCL_OK ) { return TCL_ERROR; }
    Tcl_SetResult(interp, NULL, NULL);
    //
    free_buf(&sch_ptr_buf);
    free_mgr(&pair_list_rec_mgr);
    //
    return TCL_OK;
}

int
Schematic_draw_module_Init(Tcl_Interp *interp)
{
    Tcl_CreateObjCommand(interp, "create_sch", create_sch,
			 (ClientData) NULL, NULL);
    Tcl_CreateObjCommand(interp, "add_sch_object", add_sch_object,
			 (ClientData) NULL, NULL);
    Tcl_CreateObjCommand(interp, "draw_network", draw_network,
			 (ClientData) NULL, NULL);
    create_hash(&pfn2width, 100, str_hash, str_equ);
    create_hash(&pfn2height, 100, str_hash, str_equ);
    return TCL_OK;
}
