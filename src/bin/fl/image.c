//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*		Original author: Carl-Johan Seger, 2019			*/
/*									*/
/************************************************************************/
#include "image.h"
#include "graph.h"
#include <math.h>

/* ------------- Global variables ------------- */

/********* Global variables referenced ***********/
extern symbol_tbl_ptr	symb_tbl;
extern str_mgr		*stringsp;
extern jmp_buf		*start_envp;
extern char		FailBuf[4096];
extern g_ptr		void_nd;
extern bool		Do_gc_asap;
extern bool		RCverbose_image_print;
extern FILE		*odests_fp;

/***** PRIVATE VARIABLES *****/
static int		image_oidx;
static typeExp_ptr	image_handle_tp;
static image_ptr	image_free_list = NULL;
static char		im_read_buf[READ_BUF_SIZE];

static hash_record	X11_colors;
static rec_mgr	    	color_rec_mgr;
static rec_mgr	    	image_rec_mgr;

/* ----- Forward definitions local functions ----- */
static void	 export_to_ppm(FILE *fp, image_ptr ip);
static string	 get_non_comment_line(FILE *fp);
static void      mark_image_fn(pointer p);
static void      sweep_image_fn(void);
static void      save_image_fn(FILE *fp, pointer p);
static pointer   load_image_fn(FILE *fp);
static string    image2str_fn(pointer p);
static formula   image_eq_fn(pointer p1, pointer p2, bool identical);
static unint	 image_hash_fn(pointer p, unint n);
static image_ptr create_image(string name, int rows, int cols);
static color_ptr mk_rgb(int r, int g, int b);
static void      create_X11_color_map();
static void	 rgb2hsv(int r, int g, int b, int *hp, int *sp, int *vp);
static int       image_sha256_fn(int *g_cntp, hash_record *g_tblp,
				 SHA256_ptr sha, pointer a);

/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

void
Image_Init()
{
    new_mgr(&image_rec_mgr, sizeof(image_rec));
    create_X11_color_map();
    //
    image_oidx = Add_ExtAPI_Object("image",
                                   mark_image_fn,
                                   sweep_image_fn,
                                   save_image_fn,
                                   load_image_fn,
                                   image2str_fn,
                                   image_eq_fn,
                                   image_hash_fn,
                                   NULL,
                                   NULL,
				   image_sha256_fn);
    image_handle_tp = Get_Type("image", NULL, TP_INSERT_FULL_TYPE);
}

/********************************************************/
/*	    EXPORTED EXTAPI FUNCTIONS    		*/
/********************************************************/

static void
color2rgb(g_ptr redex)
{
    g_ptr gcolor;
    EXTRACT_1_ARG(redex, gcolor);
    string color = GET_STRING(gcolor);
    color_ptr cp;
    if( (cp = find_hash(&X11_colors, color)) == NULL ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot find color '%s'\n", color));
	return;
    }
    MAKE_REDEX_PAIR(redex, Make_INT_leaf(cp->r),
			   Make_PAIR_ND(
				Make_INT_leaf(cp->g),
				Make_INT_leaf(cp->b)));
    return;
}

static void
do_rgb2hsv(g_ptr redex)
{
    g_ptr rgb;
    EXTRACT_1_ARG(redex, rgb);
    int R = GET_INT(GET_FST(rgb));
    int G = GET_INT(GET_FST(GET_SND(rgb)));
    int B = GET_INT(GET_SND(GET_SND(rgb)));
    int h, s, v;
    rgb2hsv(R, G, B, &h, &s, &v);
    MAKE_REDEX_PAIR(redex, Make_INT_leaf(h),
			   Make_PAIR_ND(
				Make_INT_leaf(s),
				Make_INT_leaf(v)));
    return;
}

static void
do_hsv2rgb(g_ptr redex)
{
    g_ptr hsv;
    EXTRACT_1_ARG(redex, hsv);
    int Hi = GET_INT(GET_FST(hsv));
    if( Hi < 0 ) {
	Hi += 360;
    }
    int Si = GET_INT(GET_FST(GET_SND(hsv)));
    int Vi = GET_INT(GET_SND(GET_SND(hsv)));
    double  H, S, V, h, s, v, f, p, q, t, r, g, b;
    int R, G, B, i;
    H = (double) Hi;
    S = (double) Si;
    V = (double) Vi;
    h = H/360.0;
    s = S/100.0;
    v = V/100.0;
    i = (int) floor(h * 6.0);
    f = h*6.0-i;
    p = v * (1.0 - s);
    q = v * (1.0 - f * s);
    t = v * (1 - (1 - f) * s);
    switch( i % 6 ) {
	case 0: { r = v; g = t; b = p; break; }
	case 1: { r = q; g = v; b = p; break; }
	case 2: { r = p; g = v; b = t; break; }
	case 3: { r = p; g = q; b = v; break; }
	case 4: { r = t; g = p; b = v; break; }
	case 5: { r = v; g = p; b = q; break; }
    }
    R = (int) (round(r*255));
    G = (int) (round(g*255));
    B = (int) (round(b*255));
    MAKE_REDEX_PAIR(redex, Make_INT_leaf(R),
			   Make_PAIR_ND(
				Make_INT_leaf(G),
				Make_INT_leaf(B)));
    return;
}

static void
import_xpm_image(g_ptr redex)
{
    g_ptr g_file;
    EXTRACT_1_ARG(redex, g_file);
    string file = GET_STRING(g_file);
    FILE *fp = fopen(file, "r");
    if( fp == NULL ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Cannot open file %s for reading", file));
	return;
    }
    string line = fgets(im_read_buf, READ_BUF_SIZE, fp);
    if( strcmp(line, "/* XPM */\n") != 0 ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Incorrect format\nGot %s\nExpected: %s\n",
				   line, "/* XPM */\n"));
	fclose(fp);
	return;
    }
    line = get_non_comment_line(fp);
    string start = index(line, '*');
    string end = index(line, '[');
    if( start == NULL || end == NULL ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Incorrect format\nGot %s\nExpected: %s\n",
				   line, "static char * <name>[] = {\n"));
	fclose(fp);
	return;
    }
    *end = '\0';
    string name = wastrsave(stringsp, start+2);
    int cols, rows, colors, chars;
    line = get_non_comment_line(fp);
    if( sscanf(line,"\"%d %d %d %d\",\n",&cols,&rows,&colors,&chars) != 4 ) {
	string msg = Fail_pr("Incorrect format\nGot %s\nExpected: %s\n", line,
			     "\"<cols> <rows> <colors> <chars>\",\n");
	MAKE_REDEX_FAILURE(redex, msg);
	fclose(fp);
	return;
    }
    image_ptr ip = create_image(name, rows, cols);
    hash_record color_map;
    create_hash(&color_map, colors, str_hash, str_equ);
    ustr_mgr     lstrings;
    new_ustrmgr(&lstrings);
    for(int i = 0; i < colors; i++) {
	line = get_non_comment_line(fp);
	line++;
	char old = *(line+chars);
	*(line+chars) = 0;
	string col_symb = uStrsave(&lstrings,line);
	*(line+chars) = old;
	string end = rindex(line, '"');
	if( end == NULL ) {
	    string msg = Fail_pr("Incorrect format\nGot %s\nExpected: %s\n",
				 line-1,
				 "\"<code>\tc <color>\",\n");
	    MAKE_REDEX_FAILURE(redex, msg);
	    fclose(fp);
	    return;
	}
	*end = 0;
	string start = line + chars + 3;
	color_ptr cp;
	if( *start == '#' ) {
	    int r, g, b;
	    if( sscanf(start, "#%02X%02X%02X", &r, &g, &b) != 3 ) {
		string msg = Fail_pr("Incorrect color format\nGot %s\n", start);
		MAKE_REDEX_FAILURE(redex, msg);
		fclose(fp);
		return;
	    }
	    cp = mk_rgb(r,g,b);
	} else {
	    if( (cp = find_hash(&X11_colors, start)) == NULL ) {
		string msg = Fail_pr("Cannot find color |%s|\n", start);
		MAKE_REDEX_FAILURE(redex, msg);
		fclose(fp);
		return;
	    }
	}
	insert_hash(&color_map, col_symb, cp);
    }
    for(int r = 0; r < rows; r++) {
	line = get_non_comment_line(fp);
	line++;
	for(int c = 0; c < cols; c++) {
	    char old = *(line+chars);
	    *(line+chars) = 0;
	    color_ptr cp = find_hash(&color_map, line);
	    *(line+chars) = old;
	    line += chars;
	    if( cp == NULL ) {
		string msg = Fail_pr("Cannot map color |%s| in row %d col %d\n",
				     line, r+1, c+1);
		MAKE_REDEX_FAILURE(redex, msg);
		fclose(fp);
		return;
	    }
	    SET_PIXEL(ip, c, r, cp);
	}
    }
    fclose(fp);
    dispose_hash(&color_map, NULLFCN);
    free_ustrmgr(&lstrings);
    MAKE_REDEX_EXT_OBJ(redex, image_oidx, ip);
}



static void
import_ppm_image(g_ptr redex)
{
    g_ptr g_file;
    EXTRACT_1_ARG(redex, g_file);
    string file = GET_STRING(g_file);
    FILE *fp = fopen(file, "rb");
    if( fp == NULL ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Cannot open file %s for reading", file));
	return;
    }
    string line = fgets(im_read_buf, READ_BUF_SIZE, fp);

    int cols, rows, color_sz;
    if( sscanf(line, "P6 %d %d %d", &cols, &rows, &color_sz) != 3 ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Incorrect format\nGot %s\nExpected: %s\n",
				   line, "P6 <cols> <rows> 256\n"));
	fclose(fp);
	return;
    }
    image_ptr ip = create_image(file, rows, cols);
    for(int r = 0; r < rows; r++) {
	for(int c = 0; c < cols; c++) {
	    unsigned char buf[3];
	    if( fread(buf, 3, 1, fp) != 1 ) {
		MAKE_REDEX_FAILURE(redex, "Incorrect format in image data");
		fclose(fp);
		return;
	    }
	    color_rec cr;
	    cr.valid = TRUE;
	    cr.r = buf[0];
	    cr.g = buf[1];
	    cr.b = buf[2];
	    SET_PIXEL(ip, c, r, &cr);
	}
    }
    fclose(fp);
    MAKE_REDEX_EXT_OBJ(redex, image_oidx, ip);
}

static void
image_size(g_ptr redex)
{
    g_ptr g_image;
    EXTRACT_1_ARG(redex, g_image);
    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    MAKE_REDEX_CONS_ND(redex, Make_INT_leaf(ip->rows), Make_INT_leaf(ip->cols));
    return;
}

static void
image_name(g_ptr redex)
{
    g_ptr g_image;
    EXTRACT_1_ARG(redex, g_image);
    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    MAKE_REDEX_STRING(redex, ip->name);
    return;
}

static void
image_get_pixel(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_image, g_row, g_col;
    EXTRACT_3_ARGS(redex, g_image, g_row, g_col);
    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    int row = GET_INT(g_row);
    int col = GET_INT(g_col);
    color_ptr cp = GET_PIXEL(ip,col,row);
    g_ptr rgb = Make_CONS_ND(Make_INT_leaf(cp->r),
			     Make_CONS_ND(Make_INT_leaf(cp->g),
					  Make_INT_leaf(cp->b)));
    g_ptr vld = Make_BOOL_leaf(cp->valid? B_One() : B_Zero());
    MAKE_REDEX_PAIR(redex, vld, rgb);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
do_export_to_ppm(g_ptr redex)
{
    g_ptr g_image, g_file;
    EXTRACT_2_ARGS(redex, g_image, g_file);
    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    string file = GET_STRING(g_file);
    FILE *fp = fopen(file, "w");
    if( fp == NULL ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Cannot open file %s for writing", file));
	return;
    }
    export_to_ppm(fp, ip);
    fclose(fp);
    MAKE_REDEX_VOID(redex);
}

static void
hsv_filter(g_ptr redex)
{
    g_ptr g_image, h_range, s_range, v_range, res_color;
    EXTRACT_5_ARGS(redex, g_image, h_range, s_range, v_range, res_color);
    //
    // ----- Get and check H range
    //
    int min_h = GET_INT(GET_CONS_HD(h_range));
    int max_h = GET_INT(GET_CONS_TL(h_range));
    if( max_h < min_h ) {
	min_h -= 360;
    }
    if( max_h < 0 ) {
	string msg = Fail_pr("hsv_filter: H_max(%d) should be >= 0\n", max_h);
	MAKE_REDEX_FAILURE(redex, msg);
	return;
    }
    if( min_h < -360 || max_h > 360 ) {
	string msg =
	 Fail_pr("hsv_filter: Range of H is [%d,%d] but should be [-360,360]\n",
		 min_h, max_h);
	MAKE_REDEX_FAILURE(redex, msg);
	return;
    }
    if( min_h < 0 && max_h < 0 ) {
	min_h += 360;
	max_h += 360;
    }
    bool split_h = min_h < 0;
    //
    // ----- Get and check S range
    //
    int min_s = GET_INT(GET_CONS_HD(s_range));
    int max_s = GET_INT(GET_CONS_TL(s_range));
    if( max_s < min_s ) {
	string msg = Fail_pr("S_max(%d) < S_min(%d) in hsv_filter",max_s,min_s);
	MAKE_REDEX_FAILURE(redex,msg);
	return;
    }
    if( min_s < 0 || max_s > 100 ) {
	string msg =
	    Fail_pr("hsv_filter: Range of S is [%d,%d] but should be [0,100]\n",
		    min_s, max_s);
	MAKE_REDEX_FAILURE(redex, msg);
	return;
    }
    //
    // ----- Get and check V range
    //
    int min_v = GET_INT(GET_CONS_HD(v_range));
    int max_v = GET_INT(GET_CONS_TL(v_range));
    if( max_v < min_v ) {
	string msg = Fail_pr("V_max(%d) < V_min(%d) in hsv_filter",max_v,min_v);
	MAKE_REDEX_FAILURE(redex,msg);
	return;
    }
    if( min_v < 0 || max_v > 100 ) {
	string msg =
	    Fail_pr("hsv_filter: Range of V is [%d,%d] but should be [0,100]\n",
		    min_v, max_v);
	MAKE_REDEX_FAILURE(redex, msg);
	return;
    }
    //
    // Get result color
    //
    int res_r = GET_INT(GET_CONS_HD(res_color));
    int res_g = GET_INT(GET_CONS_HD(GET_CONS_TL(res_color)));
    int res_b = GET_INT(GET_CONS_TL(GET_CONS_TL(res_color)));
    //
    // And perform the filter function
    //
    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    image_ptr nip = create_image(ip->name, ip->rows, ip->cols);
    for(int r = 0; r < ip->rows; r++) {
	for(int c = 0; c < ip->cols; c++) {
	    color_ptr cp = GET_PIXEL(ip, c, r);
	    color_ptr ncp = GET_PIXEL(nip, c, r);
	    ncp->valid = FALSE;
	    ncp->r = 0;
	    ncp->g = 0;
	    ncp->b = 0;
	    if( cp->valid ) {
		int h, s, v;
		rgb2hsv(cp->r, cp->g, cp->b, &h, &s, &v);
		if( v < min_v ) continue;
		if( max_v < v ) continue;
		if( s < min_s ) continue;
		if( max_s < s ) continue;
		if( split_h ) {
		    if( max_h < h && h < (360+min_h) ) continue;
		} else {
		    if( h < min_h ) continue;
		    if( max_h < h ) continue;
		}
		ncp->valid = TRUE;
		ncp->r = res_r;
		ncp->g = res_g;
		ncp->b = res_b;
	    }
	}
    }
    MAKE_REDEX_EXT_OBJ(redex, image_oidx, nip);
}

static bool
check_range(g_ptr redex, string name, int min_v, int max_v,
				      int min_range, int max_range)
{
    if( min_v > max_v ) {
	string msg = Fail_pr("max_%s(%d) < min_%s(%d)",
			     name, max_v, name, min_v);
	MAKE_REDEX_FAILURE(redex,msg);
	return FALSE;
    }
    if( min_v < min_range ) {
	string msg = Fail_pr("%s_min(%d) should be >= %d\n",
			     name, min_v, min_range);
	MAKE_REDEX_FAILURE(redex, msg);
	return FALSE;
    }
    if( max_v > max_range ) {
	string msg = Fail_pr("%s_max(%d) should be <= %d\n",
			     name, max_v, max_range);
	MAKE_REDEX_FAILURE(redex, msg);
	return FALSE;
    }
    return TRUE;
}

static void
rgb_filter(g_ptr redex)
{
    g_ptr g_image, r_range, g_range, b_range, res_color;
    EXTRACT_5_ARGS(redex, g_image, r_range, g_range, b_range, res_color);
    //
    // ----- Get and check ranges
    //
    int min_r = GET_INT(GET_CONS_HD(r_range));
    int max_r = GET_INT(GET_CONS_TL(r_range));
    if( !check_range(redex, "R",  min_r, max_r, 0, 255) ) { return; }
    int min_g = GET_INT(GET_CONS_HD(g_range));
    int max_g = GET_INT(GET_CONS_TL(g_range));
    if( !check_range(redex, "G",  min_g, max_g, 0, 255) ) { return; }
    int min_b = GET_INT(GET_CONS_HD(b_range));
    int max_b = GET_INT(GET_CONS_TL(b_range));
    if( !check_range(redex, "B",  min_b, max_b, 0, 255) ) { return; }
    //
    // Get result color
    //
    int res_r = GET_INT(GET_CONS_HD(res_color));
    int res_g = GET_INT(GET_CONS_HD(GET_CONS_TL(res_color)));
    int res_b = GET_INT(GET_CONS_TL(GET_CONS_TL(res_color)));
    //
    // And perform the filter function
    //
    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    image_ptr nip = create_image(ip->name, ip->rows, ip->cols);
    for(int r = 0; r < ip->rows; r++) {
	for(int c = 0; c < ip->cols; c++) {
	    color_ptr cp = GET_PIXEL(ip, c, r);
	    color_ptr ncp = GET_PIXEL(nip, c, r);
	    ncp->valid = FALSE;
	    ncp->r = 0;
	    ncp->g = 0;
	    ncp->b = 0;
	    if( cp->valid ) {
		int r, g, b;
		r = cp->r;
		g = cp->g;
		b = cp->b;
		if( r < min_r ) continue;
		if( r > max_r ) continue;
		if( g < min_g ) continue;
		if( g > max_g ) continue;
		if( b < min_b ) continue;
		if( b > max_b ) continue;
		ncp->valid = TRUE;
		ncp->r = res_r;
		ncp->g = res_g;
		ncp->b = res_b;
	    }
	}
    }
    MAKE_REDEX_EXT_OBJ(redex, image_oidx, nip);
}

static int
euclidian_distance(int r1, int g1, int b1, int r2, int g2, int b2) {
    int dr = r1-r2;
    int dg = g1-g2;
    int db = b1-b2;
    return( dr*dr + dg*dg + db*db );
}

static void
color_distance_filter(g_ptr redex)
{
    g_ptr g_image, g_rgb, g_distance, res_color;
    EXTRACT_4_ARGS(redex, g_image, g_rgb, g_distance, res_color);
    //
    // ----- Get and check ranges
    //
    int c_r = GET_INT(GET_FST(g_rgb));
    int c_g = GET_INT(GET_FST(GET_SND(g_rgb)));
    int c_b = GET_INT(GET_SND(GET_SND(g_rgb)));
    int dist = GET_INT(g_distance);
    if( !check_range(redex, "R", c_r, c_r, 0, 255) ) { return; }
    if( !check_range(redex, "G", c_g, c_g, 0, 255) ) { return; }
    if( !check_range(redex, "B", c_b, c_b, 0, 255) ) { return; }
    //
    // Get result color
    //
    int res_r = GET_INT(GET_CONS_HD(res_color));
    int res_g = GET_INT(GET_CONS_HD(GET_CONS_TL(res_color)));
    int res_b = GET_INT(GET_CONS_TL(GET_CONS_TL(res_color)));
    //
    // And perform the filter function
    //
    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    image_ptr nip = create_image(ip->name, ip->rows, ip->cols);
    for(int r = 0; r < ip->rows; r++) {
	for(int c = 0; c < ip->cols; c++) {
	    color_ptr cp = GET_PIXEL(ip, c, r);
	    color_ptr ncp = GET_PIXEL(nip, c, r);
	    ncp->valid = FALSE;
	    ncp->r = 0;
	    ncp->g = 0;
	    ncp->b = 0;
	    if( cp->valid ) {
		int r, g, b;
		r = cp->r;
		g = cp->g;
		b = cp->b;
		if( euclidian_distance(r,g,b, c_r, c_g, c_b) <= dist ) {
		    ncp->valid = TRUE;
		    ncp->r = res_r;
		    ncp->g = res_g;
		    ncp->b = res_b;
		}
	    }
	}
    }
    MAKE_REDEX_EXT_OBJ(redex, image_oidx, nip);
}

static int
limit_color_range(double f)
{
    int i = (int) round(f);
    if( i < 0 ) { return 0; }
    else if (i > 255) { return 255; }
    return i;
}

static void
image_change_contrast_and_brightness(g_ptr redex)
{
    g_ptr g_image, g_contrast, g_brightness;
    EXTRACT_3_ARGS(redex, g_image, g_contrast, g_brightness);
    int contrast   = GET_INT(g_contrast);
    int brightness = GET_INT(g_brightness);
    if( (contrast == 100) && (brightness == 0) ) {
	OVERWRITE(redex, g_image);
	return;
    }
    //
    // And perform the filter function
    //
    double alpha = ((double) contrast)/100.0;
    double beta  = ((double) brightness);
    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    image_ptr nip = create_image(ip->name, ip->rows, ip->cols);
    for(int r = 0; r < ip->rows; r++) {
	for(int c = 0; c < ip->cols; c++) {
	    color_ptr cp = GET_PIXEL(ip, c, r);
	    color_ptr ncp = GET_PIXEL(nip, c, r);
	    ncp->valid = cp->valid;
	    ncp->r = limit_color_range(((double) (cp->r))*alpha+beta);
	    ncp->g = limit_color_range(((double) (cp->g))*alpha+beta);
	    ncp->b = limit_color_range(((double) (cp->b))*alpha+beta);
	}
    }
    MAKE_REDEX_EXT_OBJ(redex, image_oidx, nip);
}

#define IS_BLACK(c) (!(c)->valid || (((c)->r==0)&&((c)->g==0)&&((c)->b==0)))
#define IS_WHITE(c) ((c)->valid && ((c)->r==255)&&((c)->g==255)&&((c)->b==255))

static void
image_denoise(g_ptr redex)
{
    g_ptr g_image, g_width, g_height, g_threshold;
    EXTRACT_4_ARGS(redex, g_image, g_width, g_height, g_threshold);
    int width     = GET_INT(g_width);
    int height    = GET_INT(g_height);
    int threshold = GET_INT(g_threshold);
    if( width == 0 && height == 0 ) {
	OVERWRITE(redex, g_image);
	return;
    }
    //
    // And perform the filter function
    //
    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    image_ptr nip = create_image(ip->name, ip->rows, ip->cols);
    for(int r = 0; r < ip->rows; r++) {
	for(int c = 0; c < ip->cols; c++) {
	    // Simplistic algorithm.
	    // A sweep-line approach would be a lot faster...
	    color_ptr cp = GET_PIXEL(ip, c, r);
	    color_ptr ncp = GET_PIXEL(nip, c, r);
	    ncp->valid = cp->valid;
	    ncp->r = 0;
	    ncp->g = 0;
	    ncp->b = 0;
	    if( !cp->valid ) { continue; }
	    if( !IS_BLACK(cp) && !IS_WHITE(cp) ) {
		MAKE_REDEX_FAILURE(redex,
		   Fail_pr(
		   "image not black/white at row %d and col %d (#%02x%02x%02x)",
		   r, c, cp->r, cp->g, cp->b));
		return;
	    }
	    int cnt = 0;
	    for(int deltax = -1*width; deltax <= width; deltax++) {
		for(int deltay = -1*height; deltay <= height; deltay++) {
		    int rx = c+deltax;
		    int ry = r+deltay;
		    // Pixels outside the image are treated as black
		    if( rx < 0 || rx >= ip->cols || ry < 0 || ry >= ip->rows ) {
			continue;
		    }
		    color_ptr lcp = GET_PIXEL(ip, rx, ry);
		    if( IS_WHITE(lcp) ) { cnt++; }
		}
	    }
	    if( cnt >= threshold ) {
		ncp->valid = 255;
		ncp->r = 255;
		ncp->g = 255;
		ncp->b = 255;
	    }
	}
    }
    MAKE_REDEX_EXT_OBJ(redex, image_oidx, nip);
}


static void
image_crop(g_ptr redex)
{
    g_ptr g_image, g_min_r, g_max_r, g_min_c, g_max_c;;
    EXTRACT_5_ARGS(redex, g_image, g_min_r, g_max_r, g_min_c, g_max_c);
    int min_r = GET_INT(g_min_r);
    int max_r = GET_INT(g_max_r);
    int min_c = GET_INT(g_min_c);
    int max_c = GET_INT(g_max_c);
    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    if( min_r < 0 || max_r > ip->rows ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Row range out of bounds!"));
	return;
    }
    if( min_c < 0 || max_c > ip->cols ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Column range out of bounds!"));
	return;
    }
    image_ptr nip = create_image(ip->name, max_r-min_r+1, max_c-min_c+1);
    for(int r = min_r; r < max_r; r++) {
	for(int c = min_c; c < max_c; c++) {
	    color_ptr cp = GET_PIXEL(ip, c, r);
	    color_ptr ncp = GET_PIXEL(nip, c-min_c, r-min_r);
	    *ncp = *cp;
	}
    }
    MAKE_REDEX_EXT_OBJ(redex, image_oidx, nip);
}

static void
update_image(g_ptr redex)
{
    g_ptr g_image, change_list;
    EXTRACT_2_ARGS(redex, g_image, change_list);

    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    image_ptr nip = create_image(ip->name, ip->rows, ip->cols);
    // First, copy current image
    for(int r = 0; r < ip->rows; r++) {
	for(int c = 0; c < ip->cols; c++) {
	    color_ptr cp = GET_PIXEL(ip, c, r);
	    color_ptr ncp = GET_PIXEL(nip, c, r);
	    ncp->valid = cp->valid;
	    ncp->r = cp->r;
	    ncp->g = cp->g;
	    ncp->b = cp->b;
	}
    }
    // Now process the update list
    while( !IS_NIL(change_list) ) {
	g_ptr p = GET_CONS_HD(change_list);
	int x, y, R, G, B;
	x = GET_INT(GET_FST((GET_FST(p))));
	y = GET_INT(GET_SND((GET_FST(p))));
	if( x < 0 || x > ip->cols ) {
	    MAKE_REDEX_FAILURE(redex,
			       Fail_pr("x-coordinate (%d) outside image", x));
	    return;
	}
	if( y < 0 || y > ip->rows ) {
	    MAKE_REDEX_FAILURE(redex,
			       Fail_pr("y-coordinate (%d) outside image", y));
	    return;
	}
	p = GET_SND(p);
	R = GET_INT(GET_FST(p));
	p = GET_SND(p);
	G = GET_INT(GET_FST(p));
	B = GET_INT(GET_SND(p));
	color_ptr ncp = GET_PIXEL(nip, x, y);
	ncp->valid = TRUE;
	ncp->r = R;
	ncp->g = G;
	ncp->b = B;
	change_list = GET_CONS_TL(change_list);
    }
    MAKE_REDEX_EXT_OBJ(redex, image_oidx, nip);
}

static formula
bl_is_i(g_ptr vs, int pvs, int i)
{
    formula res = B_One();
    pvs = pvs/2;
    while( !IS_NIL(vs) ) {
	if( (i/pvs)%2 == 1 ) {
	    res = B_And(res, GET_BOOL(GET_CONS_HD(vs)));
	} else {
	    res = B_And(res, B_Not(GET_BOOL(GET_CONS_HD(vs))));
	}
	pvs /= 2;
	vs = GET_CONS_TL(vs);
    }
    return res;
}


static void
image2bv(g_ptr redex)
{
    g_ptr g_image, g_bvrow, g_bvcol;
    EXTRACT_3_ARGS(redex, g_image, g_bvrow, g_bvcol);
    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    g_ptr blrow = Bv_get_list((bv_ptr) GET_EXT_OBJ(g_bvrow));
    g_ptr blcol  = Bv_get_list((bv_ptr) GET_EXT_OBJ(g_bvcol));

    int prow = 1;
    for(g_ptr cur = blrow; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	prow *= 2;
    }
    if( prow < ip->rows ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("ridx cannot select all of the rows\n"));
	return;
    }
    int pcol = 1;
    for(g_ptr cur = blcol; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	pcol *= 2;
    }
    if( pcol < ip->cols ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("cidx cannot select all of the columns\n"));
	return;
    }

    g_ptr res = Make_CONS_ND(Make_BOOL_leaf(B_Zero()), Make_NIL());
    PUSH_GLOBAL_GC(res);
    for(int r = 0; r < ip->rows; r++) {
	for(int c = 0; c < ip->cols; c++) {
	    color_ptr cp = GET_PIXEL(ip, c, r);
	    if( !cp->valid ) continue;
	    if( cp->r != 0 || cp->g != 0 || cp->b != 0 ) {
		formula r_cond = bl_is_i(blrow, prow, r);
		formula c_cond = bl_is_i(blcol, pcol, c);
		formula cond = B_And(r_cond, c_cond);
		arbi_T ai = Arbi_FromInt(65536*cp->r+256*cp->g+cp->b);
		res = Ite_bv_list(cond, Aint2bv(ai), res);
	    }
	}
    }
    POP_GLOBAL_GC(1);
    MAKE_REDEX_BV(redex, res);
}

static void
image2symbolic_bitmap(g_ptr redex)
{
    g_ptr g_image, g_bvrow, g_bvcol;
    EXTRACT_3_ARGS(redex, g_image, g_bvrow, g_bvcol);
    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    g_ptr blrow = Bv_get_list((bv_ptr) GET_EXT_OBJ(g_bvrow));
    g_ptr blcol  = Bv_get_list((bv_ptr) GET_EXT_OBJ(g_bvcol));

    int prow = 1;
    for(g_ptr cur = blrow; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	prow *= 2;
    }
    if( prow < ip->rows ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("ridx cannot select all of the rows\n"));
	return;
    }
    int pcol = 1;
    for(g_ptr cur = blcol; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	pcol *= 2;
    }
    if( pcol < ip->cols ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("cidx cannot select all of the columns\n"));
	return;
    }

    formula res = B_Zero();
    PUSH_BDD_GC(res);
    for(int r = 0; r < ip->rows; r++) {
	for(int c = 0; c < ip->cols; c++) {
	    color_ptr cp = GET_PIXEL(ip, c, r);
	    if( !cp->valid ) continue;
	    if( cp->r == 0xff && cp->g == 0xff && cp->b == 0xff ) {
		formula r_cond = bl_is_i(blrow, prow, r);
		formula c_cond = bl_is_i(blcol, pcol, c);
		formula cond = B_And(r_cond, c_cond);
		res = B_Or(res, cond);
		continue;
	    }
	    if( cp->r != 0 || cp->g != 0 || cp->b != 0 ) {
		POP_BDD_GC(1);
		string msg =
		    Fail_pr("Neither black nor white in location (%d,%d)",r,c);
		MAKE_REDEX_FAILURE(redex, msg);
		return;
	    }
	}
    }
    POP_BDD_GC(1);
    MAKE_REDEX_BOOL(redex, res);
}


static g_ptr                color_scan_tail;

static void
color_scan_fn(pointer pkey, pointer pdata)
{
    string name = (string) pkey;
    (void) pdata;
    APPEND1(color_scan_tail, Make_STRING_leaf(wastrsave(stringsp, name)));
}

static void
colors(g_ptr redex)
{
    MAKE_REDEX_NIL(redex);
    color_scan_tail = redex;
    scan_hash(&X11_colors, color_scan_fn);
    return;
}

void
Image_Install_Functions()
{
    typeExp_ptr bv_type = Get_Type("bv", NULL, TP_DONT_INSERT);

    
    Add_ExtAPI_Function("colors", "", FALSE,
			GLmake_list(GLmake_string()),
			colors);

    Add_ExtAPI_Function("image_change_contrast_and_brightness", "111", FALSE,
			GLmake_arrow(
			  image_handle_tp,
			  GLmake_arrow(
			    GLmake_int(),
			    GLmake_arrow(GLmake_int(), image_handle_tp))),
			image_change_contrast_and_brightness);

    Add_ExtAPI_Function("image_denoise", "1111", FALSE,
			GLmake_arrow(
			  image_handle_tp,
			  GLmake_arrow(
			    GLmake_int(),
			    GLmake_arrow(
			      GLmake_int(),
			      GLmake_arrow(
				GLmake_int(),
			        image_handle_tp)))),
			image_denoise);

    Add_ExtAPI_Function("image2symbolic_bitmap", "111", FALSE,
			GLmake_arrow(
			  image_handle_tp,
			  GLmake_arrow(
			    bv_type,
			    GLmake_arrow(bv_type, GLmake_bool()))),
			image2symbolic_bitmap);

    Add_ExtAPI_Function("image2bv", "111", FALSE,
			GLmake_arrow(
			  image_handle_tp,
			  GLmake_arrow(
			    bv_type,
			    GLmake_arrow(bv_type,bv_type))),
			image2bv);

    Add_ExtAPI_Function("color2rgb", "1", FALSE,
                        GLmake_arrow(GLmake_string(), 
			    GLmake_tuple(
				GLmake_int(),
				GLmake_tuple(GLmake_int(), GLmake_int()))),
                        color2rgb);

    Add_ExtAPI_Function("rgb2hsv", "1", FALSE,
                        GLmake_arrow(
			    GLmake_tuple(
				GLmake_int(),
				GLmake_tuple(GLmake_int(), GLmake_int())),
			    GLmake_tuple(
				GLmake_int(),
				GLmake_tuple(GLmake_int(), GLmake_int()))),
                        do_rgb2hsv);

    Add_ExtAPI_Function("hsv2rgb", "1", FALSE,
                        GLmake_arrow(
			    GLmake_tuple(
				GLmake_int(),
				GLmake_tuple(GLmake_int(), GLmake_int())),
			    GLmake_tuple(
				GLmake_int(),
				GLmake_tuple(GLmake_int(), GLmake_int()))),
                        do_hsv2rgb);

    Add_ExtAPI_Function("import_xpm_image", "1", FALSE,
                        GLmake_arrow(GLmake_string(), image_handle_tp),
                        import_xpm_image);

    Add_ExtAPI_Function("import_ppm_image", "1", FALSE,
                        GLmake_arrow(GLmake_string(), image_handle_tp),
                        import_ppm_image);

    Add_ExtAPI_Function("image_crop", "11111", FALSE,
                        GLmake_arrow(
			  image_handle_tp,
			  GLmake_arrow(
			    GLmake_int(),
			    GLmake_arrow(
			      GLmake_int(),
			      GLmake_arrow(
				GLmake_int(),
				GLmake_arrow(
				  GLmake_int(),
				  image_handle_tp))))),
                        image_crop);

    Add_ExtAPI_Function("hsv_filter", "11111", FALSE,
                        GLmake_arrow(
			  image_handle_tp,
			  GLmake_arrow(
			    GLmake_tuple(GLmake_int(), GLmake_int()),
			    GLmake_arrow(
			      GLmake_tuple(GLmake_int(), GLmake_int()),
			      GLmake_arrow(
				GLmake_tuple(GLmake_int(), GLmake_int()),
				GLmake_arrow(
				  GLmake_tuple(
				    GLmake_int(),
				    GLmake_tuple(GLmake_int(), GLmake_int())),
				  image_handle_tp))))),
                        hsv_filter);

    Add_ExtAPI_Function("rgb_filter", "11111", FALSE,
                        GLmake_arrow(
			  image_handle_tp,
			  GLmake_arrow(
			    GLmake_tuple(GLmake_int(), GLmake_int()),
			    GLmake_arrow(
			      GLmake_tuple(GLmake_int(), GLmake_int()),
			      GLmake_arrow(
				GLmake_tuple(GLmake_int(), GLmake_int()),
				GLmake_arrow(
				  GLmake_tuple(
				    GLmake_int(),
				    GLmake_tuple(GLmake_int(), GLmake_int())),
				  image_handle_tp))))),
                        rgb_filter);

    Add_ExtAPI_Function("color_distance_filter", "1111", FALSE,
                        GLmake_arrow(
			  image_handle_tp,
			  GLmake_arrow(
			    GLmake_tuple(
				GLmake_int(),
				GLmake_tuple(GLmake_int(), GLmake_int())
			    ),
			    GLmake_arrow(
			      GLmake_int(),
			      GLmake_arrow(
				  GLmake_tuple(
				    GLmake_int(),
				    GLmake_tuple(GLmake_int(), GLmake_int())),
				  image_handle_tp)))),
                        color_distance_filter);

    Add_ExtAPI_Function("update_image", "11", FALSE,
                        GLmake_arrow(
			  image_handle_tp,
			    GLmake_arrow(
			      GLmake_list(
			        GLmake_tuple(
				  GLmake_tuple(GLmake_int(), GLmake_int()),
				  GLmake_tuple(
			  	    GLmake_int(),
				    GLmake_tuple(
				      GLmake_int(),
				      GLmake_int())))),
			      image_handle_tp)),
			update_image);
		    
    Add_ExtAPI_Function("export_to_ppm", "11", FALSE,
                        GLmake_arrow(
			    image_handle_tp,
			    GLmake_arrow(GLmake_string(), GLmake_void())),
                        do_export_to_ppm);

    Add_ExtAPI_Function("image_size", "1", FALSE,
                        GLmake_arrow(
			    image_handle_tp,
			    GLmake_tuple(GLmake_int(), GLmake_int())),
                        image_size);

    Add_ExtAPI_Function("image_name", "1", FALSE,
                        GLmake_arrow(image_handle_tp, GLmake_string()),
                        image_name);

    Add_ExtAPI_Function("image_get_pixel", "111", FALSE,
                        GLmake_arrow(
			  image_handle_tp,
			  GLmake_arrow(
			    GLmake_int(),
			    GLmake_arrow(
			      GLmake_int(),
			      GLmake_tuple(
				GLmake_bool(),
				GLmake_tuple(
				  GLmake_int(),
				  GLmake_tuple(
				    GLmake_int(),
				    GLmake_int())))))),
                        image_get_pixel);

}


/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

static void
export_to_ppm(FILE *fp, image_ptr ip)
{
    fprintf(fp, "P6\n%d %d\n255\n", ip->cols, ip->rows);
    for(int r = 0; r < ip->rows; r++) {
	for(int c = 0; c < ip->cols; c++) {
	    color_ptr cp = GET_PIXEL(ip, c, r);
	    ppm_color pmc;
	    if( cp->valid ) {
		pmc.r = cp->r;
		pmc.g = cp->g;
		pmc.b = cp->b;
	    } else {
		pmc.r = 0;
		pmc.g = 0;
		pmc.b = 0;
	    }
	    fwrite(&pmc, sizeof(ppm_color), 1, fp);
	}
    }
}


static string
get_non_comment_line(FILE *fp)
{
    string line;
    do {
	line = fgets(im_read_buf, READ_BUF_SIZE, fp);
    } while ( strncmp(line, "/*", 2) == 0 );
    return line;
}

static void
mark_image_fn(pointer p)
{
    image_ptr image = (image_ptr) p;
    image->mark = 2;
}

static void
sweep_image_fn(void)
{
    image_ptr image;
    FOR_REC(&image_rec_mgr, image_ptr, image) {
        switch( image->mark ) {
            case 0:
                break;
            case 1:
                free_buf(&(image->cbuf));
                image->next = image_free_list;
                image_free_list = image;
                image->mark = 0;
                break;
            case 2:
                image->mark = 1;
                break;
            default:
                DIE("Should not happen");
        }
    }
}

static void
save_image_fn(FILE *fp, pointer p)
{
    image_ptr ip = (image_ptr) p;
    write_string(fp, ip->name);
    write_int(fp, ip->rows);
    write_int(fp, ip->cols);
    color_ptr cp;
    FOR_BUF(&(ip->cbuf), color_rec, cp) {
	write_uchar(fp, cp->valid);
	write_uchar(fp, cp->r);
	write_uchar(fp, cp->g);
	write_uchar(fp, cp->b);
    }
}

static pointer
load_image_fn(FILE *fp)
{
    string name;
    read_string(fp, &name);
    int rows;
    read_int(fp, &rows);
    int cols;
    read_int(fp, &cols);
    image_ptr ip = create_image(name, rows, cols);
    buffer  *cbp = &(ip->cbuf);
    for(int i = 0; i < rows*cols; i++) {
	color_rec cr;
	read_uchar(fp, &(cr.valid));
	read_uchar(fp, &(cr.r));
	read_uchar(fp, &(cr.g));
	read_uchar(fp, &(cr.b));
	push_buf(cbp, &cr);
    }
    return((pointer) ip);
}

static string
image2str_fn(pointer p)
{
    image_ptr ip = (image_ptr) p;
    sprintf(im_read_buf, "Image with %d rows and %d columns\n",
			ip->rows, ip->cols);
    string msg = wastrsave(stringsp, im_read_buf);
    if( RCverbose_image_print ) {
        FP(stdout_fp, "%s", msg);
	color_ptr cp;
	for(int r = 0; r < ip->rows; r++) {
	    char sep = '\n';
	    for(int c = 0; c < ip->cols; c++) {
		cp = GET_PIXEL(ip, c, r);
		if( cp->valid ) {
		    FP(stdout_fp, "%c%02x%02x%02x", sep, cp->r, cp->g, cp->b);
		} else {
		    FP(stdout_fp, "%c%------", sep);
		}
		sep = ' ';
	    }
	}
    }
    return( msg );
}

static unint
image_hash_fn(pointer p, unint n)
{
    image_ptr ip = (image_ptr) p;
    return( (((unint) ip->rows)+((unint) ip->cols)) % n );
}

static formula
image_eq_fn(pointer p1, pointer p2, bool identical)
{
    (void) identical;
    image_ptr ip1 = (image_ptr) p1;
    image_ptr ip2 = (image_ptr) p2;
    if( ip1->rows != ip2->rows ) { return( B_Zero() ); }
    if( ip1->cols != ip2->cols ) { return( B_Zero() ); }
    for(int r = 0; r < ip1->rows; r++) {
	for(int c = 0; c < ip1->cols; c++) {
	    color_ptr	cp1, cp2;
	    cp1 = GET_PIXEL(ip1, c, r);
	    cp2 = GET_PIXEL(ip2, c, r);
	    if( cp1->valid != cp2->valid ) { return( B_Zero() ); }
	    if( cp1->r != cp2->r ) { return( B_Zero() ); }
	    if( cp1->g != cp2->g ) { return( B_Zero() ); }
	    if( cp1->b != cp2->b ) { return( B_Zero() ); }
	}
    }
    return( B_One() );
}

static image_ptr
create_image(string name, int rows, int cols)
{
    image_ptr ip;
    if( image_free_list != NULL ) {
        ip = image_free_list;
        image_free_list = ip->next;
        ip->next = NULL;
    } else {
        ip = (image_ptr) new_rec(&image_rec_mgr);
    }
    ip->next = NULL;
    ip->mark = 1;
    ip->name = wastrsave(stringsp, name);
    ip->rows = rows;
    ip->cols = cols;
    buffer *cbp = &(ip->cbuf);
    new_buf(cbp, rows*cols, sizeof(color_rec));
    color_rec cr;
    cr.valid = FALSE;
    cr.r = 0;
    cr.g = 0;
    cr.b = 0;
    for(int r = 0; r < rows; r++) {
	for(int c = 0; c < cols; c++) {
	    push_buf(cbp, &cr);
	}
    }
    return ip;
}


static color_ptr
mk_rgb(int r, int g, int b)
{
    color_ptr cp = (color_ptr) new_rec(&color_rec_mgr);
    cp->valid = TRUE;
    cp->r = r;
    cp->g = g;
    cp->b = b;
    return cp;
}

static void
create_X11_color_map()
{
    create_hash(&X11_colors, 1000, str_hash, str_equ);
    new_mgr(&color_rec_mgr, sizeof(color_rec));
    // Insert None (alpha=0)
    color_ptr	cp = mk_rgb(0,0,0);
    cp->valid = FALSE;
    insert_hash(&X11_colors, wastrsave(stringsp, "None"), cp);
    //
    //
    // Generated from /etc/X11/rgb.txt
    //
    insert_hash(&X11_colors, wastrsave(stringsp, "snow"),
                             mk_rgb(255,250,250));
    insert_hash(&X11_colors, wastrsave(stringsp, "ghost white"),
                             mk_rgb(248,248,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "GhostWhite"),
                             mk_rgb(248,248,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "white smoke"),
                             mk_rgb(245,245,245));
    insert_hash(&X11_colors, wastrsave(stringsp, "WhiteSmoke"),
                             mk_rgb(245,245,245));
    insert_hash(&X11_colors, wastrsave(stringsp, "gainsboro"),
                             mk_rgb(220,220,220));
    insert_hash(&X11_colors, wastrsave(stringsp, "floral white"),
                             mk_rgb(255,250,240));
    insert_hash(&X11_colors, wastrsave(stringsp, "FloralWhite"),
                             mk_rgb(255,250,240));
    insert_hash(&X11_colors, wastrsave(stringsp, "old lace"),
                             mk_rgb(253,245,230));
    insert_hash(&X11_colors, wastrsave(stringsp, "OldLace"),
                             mk_rgb(253,245,230));
    insert_hash(&X11_colors, wastrsave(stringsp, "linen"),
                             mk_rgb(250,240,230));
    insert_hash(&X11_colors, wastrsave(stringsp, "antique white"),
                             mk_rgb(250,235,215));
    insert_hash(&X11_colors, wastrsave(stringsp, "AntiqueWhite"),
                             mk_rgb(250,235,215));
    insert_hash(&X11_colors, wastrsave(stringsp, "papaya whip"),
                             mk_rgb(255,239,213));
    insert_hash(&X11_colors, wastrsave(stringsp, "PapayaWhip"),
                             mk_rgb(255,239,213));
    insert_hash(&X11_colors, wastrsave(stringsp, "blanched almond"),
                             mk_rgb(255,235,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "BlanchedAlmond"),
                             mk_rgb(255,235,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "bisque"),
                             mk_rgb(255,228,196));
    insert_hash(&X11_colors, wastrsave(stringsp, "peach puff"),
                             mk_rgb(255,218,185));
    insert_hash(&X11_colors, wastrsave(stringsp, "PeachPuff"),
                             mk_rgb(255,218,185));
    insert_hash(&X11_colors, wastrsave(stringsp, "navajo white"),
                             mk_rgb(255,222,173));
    insert_hash(&X11_colors, wastrsave(stringsp, "NavajoWhite"),
                             mk_rgb(255,222,173));
    insert_hash(&X11_colors, wastrsave(stringsp, "moccasin"),
                             mk_rgb(255,228,181));
    insert_hash(&X11_colors, wastrsave(stringsp, "cornsilk"),
                             mk_rgb(255,248,220));
    insert_hash(&X11_colors, wastrsave(stringsp, "ivory"),
                             mk_rgb(255,255,240));
    insert_hash(&X11_colors, wastrsave(stringsp, "lemon chiffon"),
                             mk_rgb(255,250,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "LemonChiffon"),
                             mk_rgb(255,250,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "seashell"),
                             mk_rgb(255,245,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "honeydew"),
                             mk_rgb(240,255,240));
    insert_hash(&X11_colors, wastrsave(stringsp, "mint cream"),
                             mk_rgb(245,255,250));
    insert_hash(&X11_colors, wastrsave(stringsp, "MintCream"),
                             mk_rgb(245,255,250));
    insert_hash(&X11_colors, wastrsave(stringsp, "azure"),
                             mk_rgb(240,255,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "alice blue"),
                             mk_rgb(240,248,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "AliceBlue"),
                             mk_rgb(240,248,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "lavender"),
                             mk_rgb(230,230,250));
    insert_hash(&X11_colors, wastrsave(stringsp, "lavender blush"),
                             mk_rgb(255,240,245));
    insert_hash(&X11_colors, wastrsave(stringsp, "LavenderBlush"),
                             mk_rgb(255,240,245));
    insert_hash(&X11_colors, wastrsave(stringsp, "misty rose"),
                             mk_rgb(255,228,225));
    insert_hash(&X11_colors, wastrsave(stringsp, "MistyRose"),
                             mk_rgb(255,228,225));
    insert_hash(&X11_colors, wastrsave(stringsp, "white"),
                             mk_rgb(255,255,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "black"),
                             mk_rgb(0,0,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark slate gray"),
                             mk_rgb(47,79,79));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkSlateGray"),
                             mk_rgb(47,79,79));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark slate grey"),
                             mk_rgb(47,79,79));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkSlateGrey"),
                             mk_rgb(47,79,79));
    insert_hash(&X11_colors, wastrsave(stringsp, "dim gray"),
                             mk_rgb(105,105,105));
    insert_hash(&X11_colors, wastrsave(stringsp, "DimGray"),
                             mk_rgb(105,105,105));
    insert_hash(&X11_colors, wastrsave(stringsp, "dim grey"),
                             mk_rgb(105,105,105));
    insert_hash(&X11_colors, wastrsave(stringsp, "DimGrey"),
                             mk_rgb(105,105,105));
    insert_hash(&X11_colors, wastrsave(stringsp, "slate gray"),
                             mk_rgb(112,128,144));
    insert_hash(&X11_colors, wastrsave(stringsp, "SlateGray"),
                             mk_rgb(112,128,144));
    insert_hash(&X11_colors, wastrsave(stringsp, "slate grey"),
                             mk_rgb(112,128,144));
    insert_hash(&X11_colors, wastrsave(stringsp, "SlateGrey"),
                             mk_rgb(112,128,144));
    insert_hash(&X11_colors, wastrsave(stringsp, "light slate gray"),
                             mk_rgb(119,136,153));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSlateGray"),
                             mk_rgb(119,136,153));
    insert_hash(&X11_colors, wastrsave(stringsp, "light slate grey"),
                             mk_rgb(119,136,153));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSlateGrey"),
                             mk_rgb(119,136,153));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray"),
                             mk_rgb(190,190,190));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey"),
                             mk_rgb(190,190,190));
    insert_hash(&X11_colors, wastrsave(stringsp, "light grey"),
                             mk_rgb(211,211,211));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightGrey"),
                             mk_rgb(211,211,211));
    insert_hash(&X11_colors, wastrsave(stringsp, "light gray"),
                             mk_rgb(211,211,211));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightGray"),
                             mk_rgb(211,211,211));
    insert_hash(&X11_colors, wastrsave(stringsp, "midnight blue"),
                             mk_rgb(25,25,112));
    insert_hash(&X11_colors, wastrsave(stringsp, "MidnightBlue"),
                             mk_rgb(25,25,112));
    insert_hash(&X11_colors, wastrsave(stringsp, "navy"),
                             mk_rgb(0,0,128));
    insert_hash(&X11_colors, wastrsave(stringsp, "navy blue"),
                             mk_rgb(0,0,128));
    insert_hash(&X11_colors, wastrsave(stringsp, "NavyBlue"),
                             mk_rgb(0,0,128));
    insert_hash(&X11_colors, wastrsave(stringsp, "cornflower blue"),
                             mk_rgb(100,149,237));
    insert_hash(&X11_colors, wastrsave(stringsp, "CornflowerBlue"),
                             mk_rgb(100,149,237));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark slate blue"),
                             mk_rgb(72,61,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkSlateBlue"),
                             mk_rgb(72,61,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "slate blue"),
                             mk_rgb(106,90,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "SlateBlue"),
                             mk_rgb(106,90,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "medium slate blue"),
                             mk_rgb(123,104,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumSlateBlue"),
                             mk_rgb(123,104,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "light slate blue"),
                             mk_rgb(132,112,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSlateBlue"),
                             mk_rgb(132,112,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "medium blue"),
                             mk_rgb(0,0,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumBlue"),
                             mk_rgb(0,0,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "royal blue"),
                             mk_rgb(65,105,225));
    insert_hash(&X11_colors, wastrsave(stringsp, "RoyalBlue"),
                             mk_rgb(65,105,225));
    insert_hash(&X11_colors, wastrsave(stringsp, "blue"),
                             mk_rgb(0,0,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "dodger blue"),
                             mk_rgb(30,144,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "DodgerBlue"),
                             mk_rgb(30,144,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "deep sky blue"),
                             mk_rgb(0,191,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "DeepSkyBlue"),
                             mk_rgb(0,191,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "sky blue"),
                             mk_rgb(135,206,235));
    insert_hash(&X11_colors, wastrsave(stringsp, "SkyBlue"),
                             mk_rgb(135,206,235));
    insert_hash(&X11_colors, wastrsave(stringsp, "light sky blue"),
                             mk_rgb(135,206,250));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSkyBlue"),
                             mk_rgb(135,206,250));
    insert_hash(&X11_colors, wastrsave(stringsp, "steel blue"),
                             mk_rgb(70,130,180));
    insert_hash(&X11_colors, wastrsave(stringsp, "SteelBlue"),
                             mk_rgb(70,130,180));
    insert_hash(&X11_colors, wastrsave(stringsp, "light steel blue"),
                             mk_rgb(176,196,222));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSteelBlue"),
                             mk_rgb(176,196,222));
    insert_hash(&X11_colors, wastrsave(stringsp, "light blue"),
                             mk_rgb(173,216,230));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightBlue"),
                             mk_rgb(173,216,230));
    insert_hash(&X11_colors, wastrsave(stringsp, "powder blue"),
                             mk_rgb(176,224,230));
    insert_hash(&X11_colors, wastrsave(stringsp, "PowderBlue"),
                             mk_rgb(176,224,230));
    insert_hash(&X11_colors, wastrsave(stringsp, "pale turquoise"),
                             mk_rgb(175,238,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleTurquoise"),
                             mk_rgb(175,238,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark turquoise"),
                             mk_rgb(0,206,209));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkTurquoise"),
                             mk_rgb(0,206,209));
    insert_hash(&X11_colors, wastrsave(stringsp, "medium turquoise"),
                             mk_rgb(72,209,204));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumTurquoise"),
                             mk_rgb(72,209,204));
    insert_hash(&X11_colors, wastrsave(stringsp, "turquoise"),
                             mk_rgb(64,224,208));
    insert_hash(&X11_colors, wastrsave(stringsp, "cyan"),
                             mk_rgb(0,255,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "light cyan"),
                             mk_rgb(224,255,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightCyan"),
                             mk_rgb(224,255,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "cadet blue"),
                             mk_rgb(95,158,160));
    insert_hash(&X11_colors, wastrsave(stringsp, "CadetBlue"),
                             mk_rgb(95,158,160));
    insert_hash(&X11_colors, wastrsave(stringsp, "medium aquamarine"),
                             mk_rgb(102,205,170));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumAquamarine"),
                             mk_rgb(102,205,170));
    insert_hash(&X11_colors, wastrsave(stringsp, "aquamarine"),
                             mk_rgb(127,255,212));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark green"),
                             mk_rgb(0,100,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkGreen"),
                             mk_rgb(0,100,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark olive green"),
                             mk_rgb(85,107,47));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkOliveGreen"),
                             mk_rgb(85,107,47));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark sea green"),
                             mk_rgb(143,188,143));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkSeaGreen"),
                             mk_rgb(143,188,143));
    insert_hash(&X11_colors, wastrsave(stringsp, "sea green"),
                             mk_rgb(46,139,87));
    insert_hash(&X11_colors, wastrsave(stringsp, "SeaGreen"),
                             mk_rgb(46,139,87));
    insert_hash(&X11_colors, wastrsave(stringsp, "medium sea green"),
                             mk_rgb(60,179,113));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumSeaGreen"),
                             mk_rgb(60,179,113));
    insert_hash(&X11_colors, wastrsave(stringsp, "light sea green"),
                             mk_rgb(32,178,170));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSeaGreen"),
                             mk_rgb(32,178,170));
    insert_hash(&X11_colors, wastrsave(stringsp, "pale green"),
                             mk_rgb(152,251,152));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleGreen"),
                             mk_rgb(152,251,152));
    insert_hash(&X11_colors, wastrsave(stringsp, "spring green"),
                             mk_rgb(0,255,127));
    insert_hash(&X11_colors, wastrsave(stringsp, "SpringGreen"),
                             mk_rgb(0,255,127));
    insert_hash(&X11_colors, wastrsave(stringsp, "lawn green"),
                             mk_rgb(124,252,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "LawnGreen"),
                             mk_rgb(124,252,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "green"),
                             mk_rgb(0,255,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "chartreuse"),
                             mk_rgb(127,255,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "medium spring green"),
                             mk_rgb(0,250,154));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumSpringGreen"),
                             mk_rgb(0,250,154));
    insert_hash(&X11_colors, wastrsave(stringsp, "green yellow"),
                             mk_rgb(173,255,47));
    insert_hash(&X11_colors, wastrsave(stringsp, "GreenYellow"),
                             mk_rgb(173,255,47));
    insert_hash(&X11_colors, wastrsave(stringsp, "lime green"),
                             mk_rgb(50,205,50));
    insert_hash(&X11_colors, wastrsave(stringsp, "LimeGreen"),
                             mk_rgb(50,205,50));
    insert_hash(&X11_colors, wastrsave(stringsp, "yellow green"),
                             mk_rgb(154,205,50));
    insert_hash(&X11_colors, wastrsave(stringsp, "YellowGreen"),
                             mk_rgb(154,205,50));
    insert_hash(&X11_colors, wastrsave(stringsp, "forest green"),
                             mk_rgb(34,139,34));
    insert_hash(&X11_colors, wastrsave(stringsp, "ForestGreen"),
                             mk_rgb(34,139,34));
    insert_hash(&X11_colors, wastrsave(stringsp, "olive drab"),
                             mk_rgb(107,142,35));
    insert_hash(&X11_colors, wastrsave(stringsp, "OliveDrab"),
                             mk_rgb(107,142,35));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark khaki"),
                             mk_rgb(189,183,107));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkKhaki"),
                             mk_rgb(189,183,107));
    insert_hash(&X11_colors, wastrsave(stringsp, "khaki"),
                             mk_rgb(240,230,140));
    insert_hash(&X11_colors, wastrsave(stringsp, "pale goldenrod"),
                             mk_rgb(238,232,170));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleGoldenrod"),
                             mk_rgb(238,232,170));
    insert_hash(&X11_colors, wastrsave(stringsp, "light goldenrod yellow"),
                             mk_rgb(250,250,210));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightGoldenrodYellow"),
                             mk_rgb(250,250,210));
    insert_hash(&X11_colors, wastrsave(stringsp, "light yellow"),
                             mk_rgb(255,255,224));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightYellow"),
                             mk_rgb(255,255,224));
    insert_hash(&X11_colors, wastrsave(stringsp, "yellow"),
                             mk_rgb(255,255,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "gold"),
                             mk_rgb(255,215,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "light goldenrod"),
                             mk_rgb(238,221,130));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightGoldenrod"),
                             mk_rgb(238,221,130));
    insert_hash(&X11_colors, wastrsave(stringsp, "goldenrod"),
                             mk_rgb(218,165,32));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark goldenrod"),
                             mk_rgb(184,134,11));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkGoldenrod"),
                             mk_rgb(184,134,11));
    insert_hash(&X11_colors, wastrsave(stringsp, "rosy brown"),
                             mk_rgb(188,143,143));
    insert_hash(&X11_colors, wastrsave(stringsp, "RosyBrown"),
                             mk_rgb(188,143,143));
    insert_hash(&X11_colors, wastrsave(stringsp, "indian red"),
                             mk_rgb(205,92,92));
    insert_hash(&X11_colors, wastrsave(stringsp, "IndianRed"),
                             mk_rgb(205,92,92));
    insert_hash(&X11_colors, wastrsave(stringsp, "saddle brown"),
                             mk_rgb(139,69,19));
    insert_hash(&X11_colors, wastrsave(stringsp, "SaddleBrown"),
                             mk_rgb(139,69,19));
    insert_hash(&X11_colors, wastrsave(stringsp, "sienna"),
                             mk_rgb(160,82,45));
    insert_hash(&X11_colors, wastrsave(stringsp, "peru"),
                             mk_rgb(205,133,63));
    insert_hash(&X11_colors, wastrsave(stringsp, "burlywood"),
                             mk_rgb(222,184,135));
    insert_hash(&X11_colors, wastrsave(stringsp, "beige"),
                             mk_rgb(245,245,220));
    insert_hash(&X11_colors, wastrsave(stringsp, "wheat"),
                             mk_rgb(245,222,179));
    insert_hash(&X11_colors, wastrsave(stringsp, "sandy brown"),
                             mk_rgb(244,164,96));
    insert_hash(&X11_colors, wastrsave(stringsp, "SandyBrown"),
                             mk_rgb(244,164,96));
    insert_hash(&X11_colors, wastrsave(stringsp, "tan"),
                             mk_rgb(210,180,140));
    insert_hash(&X11_colors, wastrsave(stringsp, "chocolate"),
                             mk_rgb(210,105,30));
    insert_hash(&X11_colors, wastrsave(stringsp, "firebrick"),
                             mk_rgb(178,34,34));
    insert_hash(&X11_colors, wastrsave(stringsp, "brown"),
                             mk_rgb(165,42,42));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark salmon"),
                             mk_rgb(233,150,122));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkSalmon"),
                             mk_rgb(233,150,122));
    insert_hash(&X11_colors, wastrsave(stringsp, "salmon"),
                             mk_rgb(250,128,114));
    insert_hash(&X11_colors, wastrsave(stringsp, "light salmon"),
                             mk_rgb(255,160,122));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSalmon"),
                             mk_rgb(255,160,122));
    insert_hash(&X11_colors, wastrsave(stringsp, "orange"),
                             mk_rgb(255,165,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark orange"),
                             mk_rgb(255,140,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkOrange"),
                             mk_rgb(255,140,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "coral"),
                             mk_rgb(255,127,80));
    insert_hash(&X11_colors, wastrsave(stringsp, "light coral"),
                             mk_rgb(240,128,128));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightCoral"),
                             mk_rgb(240,128,128));
    insert_hash(&X11_colors, wastrsave(stringsp, "tomato"),
                             mk_rgb(255,99,71));
    insert_hash(&X11_colors, wastrsave(stringsp, "orange red"),
                             mk_rgb(255,69,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "OrangeRed"),
                             mk_rgb(255,69,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "red"),
                             mk_rgb(255,0,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "hot pink"),
                             mk_rgb(255,105,180));
    insert_hash(&X11_colors, wastrsave(stringsp, "HotPink"),
                             mk_rgb(255,105,180));
    insert_hash(&X11_colors, wastrsave(stringsp, "deep pink"),
                             mk_rgb(255,20,147));
    insert_hash(&X11_colors, wastrsave(stringsp, "DeepPink"),
                             mk_rgb(255,20,147));
    insert_hash(&X11_colors, wastrsave(stringsp, "pink"),
                             mk_rgb(255,192,203));
    insert_hash(&X11_colors, wastrsave(stringsp, "light pink"),
                             mk_rgb(255,182,193));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightPink"),
                             mk_rgb(255,182,193));
    insert_hash(&X11_colors, wastrsave(stringsp, "pale violet red"),
                             mk_rgb(219,112,147));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleVioletRed"),
                             mk_rgb(219,112,147));
    insert_hash(&X11_colors, wastrsave(stringsp, "maroon"),
                             mk_rgb(176,48,96));
    insert_hash(&X11_colors, wastrsave(stringsp, "medium violet red"),
                             mk_rgb(199,21,133));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumVioletRed"),
                             mk_rgb(199,21,133));
    insert_hash(&X11_colors, wastrsave(stringsp, "violet red"),
                             mk_rgb(208,32,144));
    insert_hash(&X11_colors, wastrsave(stringsp, "VioletRed"),
                             mk_rgb(208,32,144));
    insert_hash(&X11_colors, wastrsave(stringsp, "magenta"),
                             mk_rgb(255,0,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "violet"),
                             mk_rgb(238,130,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "plum"),
                             mk_rgb(221,160,221));
    insert_hash(&X11_colors, wastrsave(stringsp, "orchid"),
                             mk_rgb(218,112,214));
    insert_hash(&X11_colors, wastrsave(stringsp, "medium orchid"),
                             mk_rgb(186,85,211));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumOrchid"),
                             mk_rgb(186,85,211));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark orchid"),
                             mk_rgb(153,50,204));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkOrchid"),
                             mk_rgb(153,50,204));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark violet"),
                             mk_rgb(148,0,211));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkViolet"),
                             mk_rgb(148,0,211));
    insert_hash(&X11_colors, wastrsave(stringsp, "blue violet"),
                             mk_rgb(138,43,226));
    insert_hash(&X11_colors, wastrsave(stringsp, "BlueViolet"),
                             mk_rgb(138,43,226));
    insert_hash(&X11_colors, wastrsave(stringsp, "purple"),
                             mk_rgb(160,32,240));
    insert_hash(&X11_colors, wastrsave(stringsp, "medium purple"),
                             mk_rgb(147,112,219));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumPurple"),
                             mk_rgb(147,112,219));
    insert_hash(&X11_colors, wastrsave(stringsp, "thistle"),
                             mk_rgb(216,191,216));
    insert_hash(&X11_colors, wastrsave(stringsp, "snow1"),
                             mk_rgb(255,250,250));
    insert_hash(&X11_colors, wastrsave(stringsp, "snow2"),
                             mk_rgb(238,233,233));
    insert_hash(&X11_colors, wastrsave(stringsp, "snow3"),
                             mk_rgb(205,201,201));
    insert_hash(&X11_colors, wastrsave(stringsp, "snow4"),
                             mk_rgb(139,137,137));
    insert_hash(&X11_colors, wastrsave(stringsp, "seashell1"),
                             mk_rgb(255,245,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "seashell2"),
                             mk_rgb(238,229,222));
    insert_hash(&X11_colors, wastrsave(stringsp, "seashell3"),
                             mk_rgb(205,197,191));
    insert_hash(&X11_colors, wastrsave(stringsp, "seashell4"),
                             mk_rgb(139,134,130));
    insert_hash(&X11_colors, wastrsave(stringsp, "AntiqueWhite1"),
                             mk_rgb(255,239,219));
    insert_hash(&X11_colors, wastrsave(stringsp, "AntiqueWhite2"),
                             mk_rgb(238,223,204));
    insert_hash(&X11_colors, wastrsave(stringsp, "AntiqueWhite3"),
                             mk_rgb(205,192,176));
    insert_hash(&X11_colors, wastrsave(stringsp, "AntiqueWhite4"),
                             mk_rgb(139,131,120));
    insert_hash(&X11_colors, wastrsave(stringsp, "bisque1"),
                             mk_rgb(255,228,196));
    insert_hash(&X11_colors, wastrsave(stringsp, "bisque2"),
                             mk_rgb(238,213,183));
    insert_hash(&X11_colors, wastrsave(stringsp, "bisque3"),
                             mk_rgb(205,183,158));
    insert_hash(&X11_colors, wastrsave(stringsp, "bisque4"),
                             mk_rgb(139,125,107));
    insert_hash(&X11_colors, wastrsave(stringsp, "PeachPuff1"),
                             mk_rgb(255,218,185));
    insert_hash(&X11_colors, wastrsave(stringsp, "PeachPuff2"),
                             mk_rgb(238,203,173));
    insert_hash(&X11_colors, wastrsave(stringsp, "PeachPuff3"),
                             mk_rgb(205,175,149));
    insert_hash(&X11_colors, wastrsave(stringsp, "PeachPuff4"),
                             mk_rgb(139,119,101));
    insert_hash(&X11_colors, wastrsave(stringsp, "NavajoWhite1"),
                             mk_rgb(255,222,173));
    insert_hash(&X11_colors, wastrsave(stringsp, "NavajoWhite2"),
                             mk_rgb(238,207,161));
    insert_hash(&X11_colors, wastrsave(stringsp, "NavajoWhite3"),
                             mk_rgb(205,179,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "NavajoWhite4"),
                             mk_rgb(139,121,94));
    insert_hash(&X11_colors, wastrsave(stringsp, "LemonChiffon1"),
                             mk_rgb(255,250,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "LemonChiffon2"),
                             mk_rgb(238,233,191));
    insert_hash(&X11_colors, wastrsave(stringsp, "LemonChiffon3"),
                             mk_rgb(205,201,165));
    insert_hash(&X11_colors, wastrsave(stringsp, "LemonChiffon4"),
                             mk_rgb(139,137,112));
    insert_hash(&X11_colors, wastrsave(stringsp, "cornsilk1"),
                             mk_rgb(255,248,220));
    insert_hash(&X11_colors, wastrsave(stringsp, "cornsilk2"),
                             mk_rgb(238,232,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "cornsilk3"),
                             mk_rgb(205,200,177));
    insert_hash(&X11_colors, wastrsave(stringsp, "cornsilk4"),
                             mk_rgb(139,136,120));
    insert_hash(&X11_colors, wastrsave(stringsp, "ivory1"),
                             mk_rgb(255,255,240));
    insert_hash(&X11_colors, wastrsave(stringsp, "ivory2"),
                             mk_rgb(238,238,224));
    insert_hash(&X11_colors, wastrsave(stringsp, "ivory3"),
                             mk_rgb(205,205,193));
    insert_hash(&X11_colors, wastrsave(stringsp, "ivory4"),
                             mk_rgb(139,139,131));
    insert_hash(&X11_colors, wastrsave(stringsp, "honeydew1"),
                             mk_rgb(240,255,240));
    insert_hash(&X11_colors, wastrsave(stringsp, "honeydew2"),
                             mk_rgb(224,238,224));
    insert_hash(&X11_colors, wastrsave(stringsp, "honeydew3"),
                             mk_rgb(193,205,193));
    insert_hash(&X11_colors, wastrsave(stringsp, "honeydew4"),
                             mk_rgb(131,139,131));
    insert_hash(&X11_colors, wastrsave(stringsp, "LavenderBlush1"),
                             mk_rgb(255,240,245));
    insert_hash(&X11_colors, wastrsave(stringsp, "LavenderBlush2"),
                             mk_rgb(238,224,229));
    insert_hash(&X11_colors, wastrsave(stringsp, "LavenderBlush3"),
                             mk_rgb(205,193,197));
    insert_hash(&X11_colors, wastrsave(stringsp, "LavenderBlush4"),
                             mk_rgb(139,131,134));
    insert_hash(&X11_colors, wastrsave(stringsp, "MistyRose1"),
                             mk_rgb(255,228,225));
    insert_hash(&X11_colors, wastrsave(stringsp, "MistyRose2"),
                             mk_rgb(238,213,210));
    insert_hash(&X11_colors, wastrsave(stringsp, "MistyRose3"),
                             mk_rgb(205,183,181));
    insert_hash(&X11_colors, wastrsave(stringsp, "MistyRose4"),
                             mk_rgb(139,125,123));
    insert_hash(&X11_colors, wastrsave(stringsp, "azure1"),
                             mk_rgb(240,255,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "azure2"),
                             mk_rgb(224,238,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "azure3"),
                             mk_rgb(193,205,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "azure4"),
                             mk_rgb(131,139,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "SlateBlue1"),
                             mk_rgb(131,111,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "SlateBlue2"),
                             mk_rgb(122,103,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "SlateBlue3"),
                             mk_rgb(105,89,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "SlateBlue4"),
                             mk_rgb(71,60,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "RoyalBlue1"),
                             mk_rgb(72,118,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "RoyalBlue2"),
                             mk_rgb(67,110,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "RoyalBlue3"),
                             mk_rgb(58,95,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "RoyalBlue4"),
                             mk_rgb(39,64,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "blue1"),
                             mk_rgb(0,0,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "blue2"),
                             mk_rgb(0,0,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "blue3"),
                             mk_rgb(0,0,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "blue4"),
                             mk_rgb(0,0,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "DodgerBlue1"),
                             mk_rgb(30,144,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "DodgerBlue2"),
                             mk_rgb(28,134,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "DodgerBlue3"),
                             mk_rgb(24,116,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "DodgerBlue4"),
                             mk_rgb(16,78,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "SteelBlue1"),
                             mk_rgb(99,184,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "SteelBlue2"),
                             mk_rgb(92,172,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "SteelBlue3"),
                             mk_rgb(79,148,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "SteelBlue4"),
                             mk_rgb(54,100,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "DeepSkyBlue1"),
                             mk_rgb(0,191,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "DeepSkyBlue2"),
                             mk_rgb(0,178,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "DeepSkyBlue3"),
                             mk_rgb(0,154,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "DeepSkyBlue4"),
                             mk_rgb(0,104,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "SkyBlue1"),
                             mk_rgb(135,206,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "SkyBlue2"),
                             mk_rgb(126,192,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "SkyBlue3"),
                             mk_rgb(108,166,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "SkyBlue4"),
                             mk_rgb(74,112,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSkyBlue1"),
                             mk_rgb(176,226,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSkyBlue2"),
                             mk_rgb(164,211,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSkyBlue3"),
                             mk_rgb(141,182,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSkyBlue4"),
                             mk_rgb(96,123,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "SlateGray1"),
                             mk_rgb(198,226,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "SlateGray2"),
                             mk_rgb(185,211,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "SlateGray3"),
                             mk_rgb(159,182,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "SlateGray4"),
                             mk_rgb(108,123,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSteelBlue1"),
                             mk_rgb(202,225,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSteelBlue2"),
                             mk_rgb(188,210,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSteelBlue3"),
                             mk_rgb(162,181,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSteelBlue4"),
                             mk_rgb(110,123,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightBlue1"),
                             mk_rgb(191,239,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightBlue2"),
                             mk_rgb(178,223,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightBlue3"),
                             mk_rgb(154,192,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightBlue4"),
                             mk_rgb(104,131,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightCyan1"),
                             mk_rgb(224,255,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightCyan2"),
                             mk_rgb(209,238,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightCyan3"),
                             mk_rgb(180,205,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightCyan4"),
                             mk_rgb(122,139,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleTurquoise1"),
                             mk_rgb(187,255,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleTurquoise2"),
                             mk_rgb(174,238,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleTurquoise3"),
                             mk_rgb(150,205,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleTurquoise4"),
                             mk_rgb(102,139,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "CadetBlue1"),
                             mk_rgb(152,245,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "CadetBlue2"),
                             mk_rgb(142,229,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "CadetBlue3"),
                             mk_rgb(122,197,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "CadetBlue4"),
                             mk_rgb(83,134,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "turquoise1"),
                             mk_rgb(0,245,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "turquoise2"),
                             mk_rgb(0,229,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "turquoise3"),
                             mk_rgb(0,197,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "turquoise4"),
                             mk_rgb(0,134,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "cyan1"),
                             mk_rgb(0,255,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "cyan2"),
                             mk_rgb(0,238,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "cyan3"),
                             mk_rgb(0,205,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "cyan4"),
                             mk_rgb(0,139,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkSlateGray1"),
                             mk_rgb(151,255,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkSlateGray2"),
                             mk_rgb(141,238,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkSlateGray3"),
                             mk_rgb(121,205,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkSlateGray4"),
                             mk_rgb(82,139,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "aquamarine1"),
                             mk_rgb(127,255,212));
    insert_hash(&X11_colors, wastrsave(stringsp, "aquamarine2"),
                             mk_rgb(118,238,198));
    insert_hash(&X11_colors, wastrsave(stringsp, "aquamarine3"),
                             mk_rgb(102,205,170));
    insert_hash(&X11_colors, wastrsave(stringsp, "aquamarine4"),
                             mk_rgb(69,139,116));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkSeaGreen1"),
                             mk_rgb(193,255,193));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkSeaGreen2"),
                             mk_rgb(180,238,180));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkSeaGreen3"),
                             mk_rgb(155,205,155));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkSeaGreen4"),
                             mk_rgb(105,139,105));
    insert_hash(&X11_colors, wastrsave(stringsp, "SeaGreen1"),
                             mk_rgb(84,255,159));
    insert_hash(&X11_colors, wastrsave(stringsp, "SeaGreen2"),
                             mk_rgb(78,238,148));
    insert_hash(&X11_colors, wastrsave(stringsp, "SeaGreen3"),
                             mk_rgb(67,205,128));
    insert_hash(&X11_colors, wastrsave(stringsp, "SeaGreen4"),
                             mk_rgb(46,139,87));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleGreen1"),
                             mk_rgb(154,255,154));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleGreen2"),
                             mk_rgb(144,238,144));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleGreen3"),
                             mk_rgb(124,205,124));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleGreen4"),
                             mk_rgb(84,139,84));
    insert_hash(&X11_colors, wastrsave(stringsp, "SpringGreen1"),
                             mk_rgb(0,255,127));
    insert_hash(&X11_colors, wastrsave(stringsp, "SpringGreen2"),
                             mk_rgb(0,238,118));
    insert_hash(&X11_colors, wastrsave(stringsp, "SpringGreen3"),
                             mk_rgb(0,205,102));
    insert_hash(&X11_colors, wastrsave(stringsp, "SpringGreen4"),
                             mk_rgb(0,139,69));
    insert_hash(&X11_colors, wastrsave(stringsp, "green1"),
                             mk_rgb(0,255,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "green2"),
                             mk_rgb(0,238,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "green3"),
                             mk_rgb(0,205,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "green4"),
                             mk_rgb(0,139,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "chartreuse1"),
                             mk_rgb(127,255,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "chartreuse2"),
                             mk_rgb(118,238,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "chartreuse3"),
                             mk_rgb(102,205,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "chartreuse4"),
                             mk_rgb(69,139,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "OliveDrab1"),
                             mk_rgb(192,255,62));
    insert_hash(&X11_colors, wastrsave(stringsp, "OliveDrab2"),
                             mk_rgb(179,238,58));
    insert_hash(&X11_colors, wastrsave(stringsp, "OliveDrab3"),
                             mk_rgb(154,205,50));
    insert_hash(&X11_colors, wastrsave(stringsp, "OliveDrab4"),
                             mk_rgb(105,139,34));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkOliveGreen1"),
                             mk_rgb(202,255,112));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkOliveGreen2"),
                             mk_rgb(188,238,104));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkOliveGreen3"),
                             mk_rgb(162,205,90));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkOliveGreen4"),
                             mk_rgb(110,139,61));
    insert_hash(&X11_colors, wastrsave(stringsp, "khaki1"),
                             mk_rgb(255,246,143));
    insert_hash(&X11_colors, wastrsave(stringsp, "khaki2"),
                             mk_rgb(238,230,133));
    insert_hash(&X11_colors, wastrsave(stringsp, "khaki3"),
                             mk_rgb(205,198,115));
    insert_hash(&X11_colors, wastrsave(stringsp, "khaki4"),
                             mk_rgb(139,134,78));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightGoldenrod1"),
                             mk_rgb(255,236,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightGoldenrod2"),
                             mk_rgb(238,220,130));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightGoldenrod3"),
                             mk_rgb(205,190,112));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightGoldenrod4"),
                             mk_rgb(139,129,76));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightYellow1"),
                             mk_rgb(255,255,224));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightYellow2"),
                             mk_rgb(238,238,209));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightYellow3"),
                             mk_rgb(205,205,180));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightYellow4"),
                             mk_rgb(139,139,122));
    insert_hash(&X11_colors, wastrsave(stringsp, "yellow1"),
                             mk_rgb(255,255,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "yellow2"),
                             mk_rgb(238,238,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "yellow3"),
                             mk_rgb(205,205,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "yellow4"),
                             mk_rgb(139,139,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "gold1"),
                             mk_rgb(255,215,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "gold2"),
                             mk_rgb(238,201,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "gold3"),
                             mk_rgb(205,173,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "gold4"),
                             mk_rgb(139,117,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "goldenrod1"),
                             mk_rgb(255,193,37));
    insert_hash(&X11_colors, wastrsave(stringsp, "goldenrod2"),
                             mk_rgb(238,180,34));
    insert_hash(&X11_colors, wastrsave(stringsp, "goldenrod3"),
                             mk_rgb(205,155,29));
    insert_hash(&X11_colors, wastrsave(stringsp, "goldenrod4"),
                             mk_rgb(139,105,20));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkGoldenrod1"),
                             mk_rgb(255,185,15));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkGoldenrod2"),
                             mk_rgb(238,173,14));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkGoldenrod3"),
                             mk_rgb(205,149,12));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkGoldenrod4"),
                             mk_rgb(139,101,8));
    insert_hash(&X11_colors, wastrsave(stringsp, "RosyBrown1"),
                             mk_rgb(255,193,193));
    insert_hash(&X11_colors, wastrsave(stringsp, "RosyBrown2"),
                             mk_rgb(238,180,180));
    insert_hash(&X11_colors, wastrsave(stringsp, "RosyBrown3"),
                             mk_rgb(205,155,155));
    insert_hash(&X11_colors, wastrsave(stringsp, "RosyBrown4"),
                             mk_rgb(139,105,105));
    insert_hash(&X11_colors, wastrsave(stringsp, "IndianRed1"),
                             mk_rgb(255,106,106));
    insert_hash(&X11_colors, wastrsave(stringsp, "IndianRed2"),
                             mk_rgb(238,99,99));
    insert_hash(&X11_colors, wastrsave(stringsp, "IndianRed3"),
                             mk_rgb(205,85,85));
    insert_hash(&X11_colors, wastrsave(stringsp, "IndianRed4"),
                             mk_rgb(139,58,58));
    insert_hash(&X11_colors, wastrsave(stringsp, "sienna1"),
                             mk_rgb(255,130,71));
    insert_hash(&X11_colors, wastrsave(stringsp, "sienna2"),
                             mk_rgb(238,121,66));
    insert_hash(&X11_colors, wastrsave(stringsp, "sienna3"),
                             mk_rgb(205,104,57));
    insert_hash(&X11_colors, wastrsave(stringsp, "sienna4"),
                             mk_rgb(139,71,38));
    insert_hash(&X11_colors, wastrsave(stringsp, "burlywood1"),
                             mk_rgb(255,211,155));
    insert_hash(&X11_colors, wastrsave(stringsp, "burlywood2"),
                             mk_rgb(238,197,145));
    insert_hash(&X11_colors, wastrsave(stringsp, "burlywood3"),
                             mk_rgb(205,170,125));
    insert_hash(&X11_colors, wastrsave(stringsp, "burlywood4"),
                             mk_rgb(139,115,85));
    insert_hash(&X11_colors, wastrsave(stringsp, "wheat1"),
                             mk_rgb(255,231,186));
    insert_hash(&X11_colors, wastrsave(stringsp, "wheat2"),
                             mk_rgb(238,216,174));
    insert_hash(&X11_colors, wastrsave(stringsp, "wheat3"),
                             mk_rgb(205,186,150));
    insert_hash(&X11_colors, wastrsave(stringsp, "wheat4"),
                             mk_rgb(139,126,102));
    insert_hash(&X11_colors, wastrsave(stringsp, "tan1"),
                             mk_rgb(255,165,79));
    insert_hash(&X11_colors, wastrsave(stringsp, "tan2"),
                             mk_rgb(238,154,73));
    insert_hash(&X11_colors, wastrsave(stringsp, "tan3"),
                             mk_rgb(205,133,63));
    insert_hash(&X11_colors, wastrsave(stringsp, "tan4"),
                             mk_rgb(139,90,43));
    insert_hash(&X11_colors, wastrsave(stringsp, "chocolate1"),
                             mk_rgb(255,127,36));
    insert_hash(&X11_colors, wastrsave(stringsp, "chocolate2"),
                             mk_rgb(238,118,33));
    insert_hash(&X11_colors, wastrsave(stringsp, "chocolate3"),
                             mk_rgb(205,102,29));
    insert_hash(&X11_colors, wastrsave(stringsp, "chocolate4"),
                             mk_rgb(139,69,19));
    insert_hash(&X11_colors, wastrsave(stringsp, "firebrick1"),
                             mk_rgb(255,48,48));
    insert_hash(&X11_colors, wastrsave(stringsp, "firebrick2"),
                             mk_rgb(238,44,44));
    insert_hash(&X11_colors, wastrsave(stringsp, "firebrick3"),
                             mk_rgb(205,38,38));
    insert_hash(&X11_colors, wastrsave(stringsp, "firebrick4"),
                             mk_rgb(139,26,26));
    insert_hash(&X11_colors, wastrsave(stringsp, "brown1"),
                             mk_rgb(255,64,64));
    insert_hash(&X11_colors, wastrsave(stringsp, "brown2"),
                             mk_rgb(238,59,59));
    insert_hash(&X11_colors, wastrsave(stringsp, "brown3"),
                             mk_rgb(205,51,51));
    insert_hash(&X11_colors, wastrsave(stringsp, "brown4"),
                             mk_rgb(139,35,35));
    insert_hash(&X11_colors, wastrsave(stringsp, "salmon1"),
                             mk_rgb(255,140,105));
    insert_hash(&X11_colors, wastrsave(stringsp, "salmon2"),
                             mk_rgb(238,130,98));
    insert_hash(&X11_colors, wastrsave(stringsp, "salmon3"),
                             mk_rgb(205,112,84));
    insert_hash(&X11_colors, wastrsave(stringsp, "salmon4"),
                             mk_rgb(139,76,57));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSalmon1"),
                             mk_rgb(255,160,122));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSalmon2"),
                             mk_rgb(238,149,114));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSalmon3"),
                             mk_rgb(205,129,98));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightSalmon4"),
                             mk_rgb(139,87,66));
    insert_hash(&X11_colors, wastrsave(stringsp, "orange1"),
                             mk_rgb(255,165,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "orange2"),
                             mk_rgb(238,154,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "orange3"),
                             mk_rgb(205,133,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "orange4"),
                             mk_rgb(139,90,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkOrange1"),
                             mk_rgb(255,127,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkOrange2"),
                             mk_rgb(238,118,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkOrange3"),
                             mk_rgb(205,102,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkOrange4"),
                             mk_rgb(139,69,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "coral1"),
                             mk_rgb(255,114,86));
    insert_hash(&X11_colors, wastrsave(stringsp, "coral2"),
                             mk_rgb(238,106,80));
    insert_hash(&X11_colors, wastrsave(stringsp, "coral3"),
                             mk_rgb(205,91,69));
    insert_hash(&X11_colors, wastrsave(stringsp, "coral4"),
                             mk_rgb(139,62,47));
    insert_hash(&X11_colors, wastrsave(stringsp, "tomato1"),
                             mk_rgb(255,99,71));
    insert_hash(&X11_colors, wastrsave(stringsp, "tomato2"),
                             mk_rgb(238,92,66));
    insert_hash(&X11_colors, wastrsave(stringsp, "tomato3"),
                             mk_rgb(205,79,57));
    insert_hash(&X11_colors, wastrsave(stringsp, "tomato4"),
                             mk_rgb(139,54,38));
    insert_hash(&X11_colors, wastrsave(stringsp, "OrangeRed1"),
                             mk_rgb(255,69,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "OrangeRed2"),
                             mk_rgb(238,64,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "OrangeRed3"),
                             mk_rgb(205,55,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "OrangeRed4"),
                             mk_rgb(139,37,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "red1"),
                             mk_rgb(255,0,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "red2"),
                             mk_rgb(238,0,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "red3"),
                             mk_rgb(205,0,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "red4"),
                             mk_rgb(139,0,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "DebianRed"),
                             mk_rgb(215,7,81));
    insert_hash(&X11_colors, wastrsave(stringsp, "DeepPink1"),
                             mk_rgb(255,20,147));
    insert_hash(&X11_colors, wastrsave(stringsp, "DeepPink2"),
                             mk_rgb(238,18,137));
    insert_hash(&X11_colors, wastrsave(stringsp, "DeepPink3"),
                             mk_rgb(205,16,118));
    insert_hash(&X11_colors, wastrsave(stringsp, "DeepPink4"),
                             mk_rgb(139,10,80));
    insert_hash(&X11_colors, wastrsave(stringsp, "HotPink1"),
                             mk_rgb(255,110,180));
    insert_hash(&X11_colors, wastrsave(stringsp, "HotPink2"),
                             mk_rgb(238,106,167));
    insert_hash(&X11_colors, wastrsave(stringsp, "HotPink3"),
                             mk_rgb(205,96,144));
    insert_hash(&X11_colors, wastrsave(stringsp, "HotPink4"),
                             mk_rgb(139,58,98));
    insert_hash(&X11_colors, wastrsave(stringsp, "pink1"),
                             mk_rgb(255,181,197));
    insert_hash(&X11_colors, wastrsave(stringsp, "pink2"),
                             mk_rgb(238,169,184));
    insert_hash(&X11_colors, wastrsave(stringsp, "pink3"),
                             mk_rgb(205,145,158));
    insert_hash(&X11_colors, wastrsave(stringsp, "pink4"),
                             mk_rgb(139,99,108));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightPink1"),
                             mk_rgb(255,174,185));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightPink2"),
                             mk_rgb(238,162,173));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightPink3"),
                             mk_rgb(205,140,149));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightPink4"),
                             mk_rgb(139,95,101));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleVioletRed1"),
                             mk_rgb(255,130,171));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleVioletRed2"),
                             mk_rgb(238,121,159));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleVioletRed3"),
                             mk_rgb(205,104,137));
    insert_hash(&X11_colors, wastrsave(stringsp, "PaleVioletRed4"),
                             mk_rgb(139,71,93));
    insert_hash(&X11_colors, wastrsave(stringsp, "maroon1"),
                             mk_rgb(255,52,179));
    insert_hash(&X11_colors, wastrsave(stringsp, "maroon2"),
                             mk_rgb(238,48,167));
    insert_hash(&X11_colors, wastrsave(stringsp, "maroon3"),
                             mk_rgb(205,41,144));
    insert_hash(&X11_colors, wastrsave(stringsp, "maroon4"),
                             mk_rgb(139,28,98));
    insert_hash(&X11_colors, wastrsave(stringsp, "VioletRed1"),
                             mk_rgb(255,62,150));
    insert_hash(&X11_colors, wastrsave(stringsp, "VioletRed2"),
                             mk_rgb(238,58,140));
    insert_hash(&X11_colors, wastrsave(stringsp, "VioletRed3"),
                             mk_rgb(205,50,120));
    insert_hash(&X11_colors, wastrsave(stringsp, "VioletRed4"),
                             mk_rgb(139,34,82));
    insert_hash(&X11_colors, wastrsave(stringsp, "magenta1"),
                             mk_rgb(255,0,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "magenta2"),
                             mk_rgb(238,0,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "magenta3"),
                             mk_rgb(205,0,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "magenta4"),
                             mk_rgb(139,0,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "orchid1"),
                             mk_rgb(255,131,250));
    insert_hash(&X11_colors, wastrsave(stringsp, "orchid2"),
                             mk_rgb(238,122,233));
    insert_hash(&X11_colors, wastrsave(stringsp, "orchid3"),
                             mk_rgb(205,105,201));
    insert_hash(&X11_colors, wastrsave(stringsp, "orchid4"),
                             mk_rgb(139,71,137));
    insert_hash(&X11_colors, wastrsave(stringsp, "plum1"),
                             mk_rgb(255,187,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "plum2"),
                             mk_rgb(238,174,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "plum3"),
                             mk_rgb(205,150,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "plum4"),
                             mk_rgb(139,102,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumOrchid1"),
                             mk_rgb(224,102,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumOrchid2"),
                             mk_rgb(209,95,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumOrchid3"),
                             mk_rgb(180,82,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumOrchid4"),
                             mk_rgb(122,55,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkOrchid1"),
                             mk_rgb(191,62,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkOrchid2"),
                             mk_rgb(178,58,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkOrchid3"),
                             mk_rgb(154,50,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkOrchid4"),
                             mk_rgb(104,34,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "purple1"),
                             mk_rgb(155,48,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "purple2"),
                             mk_rgb(145,44,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "purple3"),
                             mk_rgb(125,38,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "purple4"),
                             mk_rgb(85,26,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumPurple1"),
                             mk_rgb(171,130,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumPurple2"),
                             mk_rgb(159,121,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumPurple3"),
                             mk_rgb(137,104,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "MediumPurple4"),
                             mk_rgb(93,71,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "thistle1"),
                             mk_rgb(255,225,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "thistle2"),
                             mk_rgb(238,210,238));
    insert_hash(&X11_colors, wastrsave(stringsp, "thistle3"),
                             mk_rgb(205,181,205));
    insert_hash(&X11_colors, wastrsave(stringsp, "thistle4"),
                             mk_rgb(139,123,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray0"),
                             mk_rgb(0,0,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey0"),
                             mk_rgb(0,0,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray1"),
                             mk_rgb(3,3,3));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey1"),
                             mk_rgb(3,3,3));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray2"),
                             mk_rgb(5,5,5));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey2"),
                             mk_rgb(5,5,5));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray3"),
                             mk_rgb(8,8,8));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey3"),
                             mk_rgb(8,8,8));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray4"),
                             mk_rgb(10,10,10));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey4"),
                             mk_rgb(10,10,10));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray5"),
                             mk_rgb(13,13,13));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey5"),
                             mk_rgb(13,13,13));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray6"),
                             mk_rgb(15,15,15));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey6"),
                             mk_rgb(15,15,15));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray7"),
                             mk_rgb(18,18,18));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey7"),
                             mk_rgb(18,18,18));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray8"),
                             mk_rgb(20,20,20));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey8"),
                             mk_rgb(20,20,20));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray9"),
                             mk_rgb(23,23,23));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey9"),
                             mk_rgb(23,23,23));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray10"),
                             mk_rgb(26,26,26));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey10"),
                             mk_rgb(26,26,26));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray11"),
                             mk_rgb(28,28,28));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey11"),
                             mk_rgb(28,28,28));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray12"),
                             mk_rgb(31,31,31));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey12"),
                             mk_rgb(31,31,31));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray13"),
                             mk_rgb(33,33,33));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey13"),
                             mk_rgb(33,33,33));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray14"),
                             mk_rgb(36,36,36));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey14"),
                             mk_rgb(36,36,36));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray15"),
                             mk_rgb(38,38,38));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey15"),
                             mk_rgb(38,38,38));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray16"),
                             mk_rgb(41,41,41));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey16"),
                             mk_rgb(41,41,41));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray17"),
                             mk_rgb(43,43,43));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey17"),
                             mk_rgb(43,43,43));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray18"),
                             mk_rgb(46,46,46));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey18"),
                             mk_rgb(46,46,46));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray19"),
                             mk_rgb(48,48,48));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey19"),
                             mk_rgb(48,48,48));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray20"),
                             mk_rgb(51,51,51));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey20"),
                             mk_rgb(51,51,51));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray21"),
                             mk_rgb(54,54,54));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey21"),
                             mk_rgb(54,54,54));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray22"),
                             mk_rgb(56,56,56));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey22"),
                             mk_rgb(56,56,56));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray23"),
                             mk_rgb(59,59,59));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey23"),
                             mk_rgb(59,59,59));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray24"),
                             mk_rgb(61,61,61));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey24"),
                             mk_rgb(61,61,61));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray25"),
                             mk_rgb(64,64,64));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey25"),
                             mk_rgb(64,64,64));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray26"),
                             mk_rgb(66,66,66));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey26"),
                             mk_rgb(66,66,66));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray27"),
                             mk_rgb(69,69,69));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey27"),
                             mk_rgb(69,69,69));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray28"),
                             mk_rgb(71,71,71));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey28"),
                             mk_rgb(71,71,71));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray29"),
                             mk_rgb(74,74,74));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey29"),
                             mk_rgb(74,74,74));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray30"),
                             mk_rgb(77,77,77));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey30"),
                             mk_rgb(77,77,77));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray31"),
                             mk_rgb(79,79,79));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey31"),
                             mk_rgb(79,79,79));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray32"),
                             mk_rgb(82,82,82));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey32"),
                             mk_rgb(82,82,82));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray33"),
                             mk_rgb(84,84,84));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey33"),
                             mk_rgb(84,84,84));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray34"),
                             mk_rgb(87,87,87));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey34"),
                             mk_rgb(87,87,87));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray35"),
                             mk_rgb(89,89,89));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey35"),
                             mk_rgb(89,89,89));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray36"),
                             mk_rgb(92,92,92));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey36"),
                             mk_rgb(92,92,92));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray37"),
                             mk_rgb(94,94,94));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey37"),
                             mk_rgb(94,94,94));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray38"),
                             mk_rgb(97,97,97));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey38"),
                             mk_rgb(97,97,97));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray39"),
                             mk_rgb(99,99,99));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey39"),
                             mk_rgb(99,99,99));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray40"),
                             mk_rgb(102,102,102));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey40"),
                             mk_rgb(102,102,102));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray41"),
                             mk_rgb(105,105,105));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey41"),
                             mk_rgb(105,105,105));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray42"),
                             mk_rgb(107,107,107));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey42"),
                             mk_rgb(107,107,107));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray43"),
                             mk_rgb(110,110,110));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey43"),
                             mk_rgb(110,110,110));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray44"),
                             mk_rgb(112,112,112));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey44"),
                             mk_rgb(112,112,112));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray45"),
                             mk_rgb(115,115,115));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey45"),
                             mk_rgb(115,115,115));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray46"),
                             mk_rgb(117,117,117));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey46"),
                             mk_rgb(117,117,117));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray47"),
                             mk_rgb(120,120,120));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey47"),
                             mk_rgb(120,120,120));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray48"),
                             mk_rgb(122,122,122));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey48"),
                             mk_rgb(122,122,122));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray49"),
                             mk_rgb(125,125,125));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey49"),
                             mk_rgb(125,125,125));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray50"),
                             mk_rgb(127,127,127));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey50"),
                             mk_rgb(127,127,127));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray51"),
                             mk_rgb(130,130,130));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey51"),
                             mk_rgb(130,130,130));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray52"),
                             mk_rgb(133,133,133));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey52"),
                             mk_rgb(133,133,133));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray53"),
                             mk_rgb(135,135,135));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey53"),
                             mk_rgb(135,135,135));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray54"),
                             mk_rgb(138,138,138));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey54"),
                             mk_rgb(138,138,138));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray55"),
                             mk_rgb(140,140,140));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey55"),
                             mk_rgb(140,140,140));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray56"),
                             mk_rgb(143,143,143));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey56"),
                             mk_rgb(143,143,143));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray57"),
                             mk_rgb(145,145,145));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey57"),
                             mk_rgb(145,145,145));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray58"),
                             mk_rgb(148,148,148));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey58"),
                             mk_rgb(148,148,148));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray59"),
                             mk_rgb(150,150,150));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey59"),
                             mk_rgb(150,150,150));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray60"),
                             mk_rgb(153,153,153));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey60"),
                             mk_rgb(153,153,153));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray61"),
                             mk_rgb(156,156,156));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey61"),
                             mk_rgb(156,156,156));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray62"),
                             mk_rgb(158,158,158));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey62"),
                             mk_rgb(158,158,158));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray63"),
                             mk_rgb(161,161,161));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey63"),
                             mk_rgb(161,161,161));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray64"),
                             mk_rgb(163,163,163));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey64"),
                             mk_rgb(163,163,163));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray65"),
                             mk_rgb(166,166,166));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey65"),
                             mk_rgb(166,166,166));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray66"),
                             mk_rgb(168,168,168));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey66"),
                             mk_rgb(168,168,168));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray67"),
                             mk_rgb(171,171,171));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey67"),
                             mk_rgb(171,171,171));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray68"),
                             mk_rgb(173,173,173));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey68"),
                             mk_rgb(173,173,173));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray69"),
                             mk_rgb(176,176,176));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey69"),
                             mk_rgb(176,176,176));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray70"),
                             mk_rgb(179,179,179));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey70"),
                             mk_rgb(179,179,179));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray71"),
                             mk_rgb(181,181,181));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey71"),
                             mk_rgb(181,181,181));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray72"),
                             mk_rgb(184,184,184));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey72"),
                             mk_rgb(184,184,184));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray73"),
                             mk_rgb(186,186,186));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey73"),
                             mk_rgb(186,186,186));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray74"),
                             mk_rgb(189,189,189));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey74"),
                             mk_rgb(189,189,189));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray75"),
                             mk_rgb(191,191,191));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey75"),
                             mk_rgb(191,191,191));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray76"),
                             mk_rgb(194,194,194));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey76"),
                             mk_rgb(194,194,194));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray77"),
                             mk_rgb(196,196,196));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey77"),
                             mk_rgb(196,196,196));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray78"),
                             mk_rgb(199,199,199));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey78"),
                             mk_rgb(199,199,199));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray79"),
                             mk_rgb(201,201,201));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey79"),
                             mk_rgb(201,201,201));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray80"),
                             mk_rgb(204,204,204));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey80"),
                             mk_rgb(204,204,204));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray81"),
                             mk_rgb(207,207,207));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey81"),
                             mk_rgb(207,207,207));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray82"),
                             mk_rgb(209,209,209));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey82"),
                             mk_rgb(209,209,209));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray83"),
                             mk_rgb(212,212,212));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey83"),
                             mk_rgb(212,212,212));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray84"),
                             mk_rgb(214,214,214));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey84"),
                             mk_rgb(214,214,214));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray85"),
                             mk_rgb(217,217,217));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey85"),
                             mk_rgb(217,217,217));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray86"),
                             mk_rgb(219,219,219));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey86"),
                             mk_rgb(219,219,219));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray87"),
                             mk_rgb(222,222,222));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey87"),
                             mk_rgb(222,222,222));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray88"),
                             mk_rgb(224,224,224));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey88"),
                             mk_rgb(224,224,224));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray89"),
                             mk_rgb(227,227,227));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey89"),
                             mk_rgb(227,227,227));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray90"),
                             mk_rgb(229,229,229));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey90"),
                             mk_rgb(229,229,229));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray91"),
                             mk_rgb(232,232,232));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey91"),
                             mk_rgb(232,232,232));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray92"),
                             mk_rgb(235,235,235));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey92"),
                             mk_rgb(235,235,235));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray93"),
                             mk_rgb(237,237,237));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey93"),
                             mk_rgb(237,237,237));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray94"),
                             mk_rgb(240,240,240));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey94"),
                             mk_rgb(240,240,240));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray95"),
                             mk_rgb(242,242,242));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey95"),
                             mk_rgb(242,242,242));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray96"),
                             mk_rgb(245,245,245));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey96"),
                             mk_rgb(245,245,245));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray97"),
                             mk_rgb(247,247,247));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey97"),
                             mk_rgb(247,247,247));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray98"),
                             mk_rgb(250,250,250));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey98"),
                             mk_rgb(250,250,250));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray99"),
                             mk_rgb(252,252,252));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey99"),
                             mk_rgb(252,252,252));
    insert_hash(&X11_colors, wastrsave(stringsp, "gray100"),
                             mk_rgb(255,255,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "grey100"),
                             mk_rgb(255,255,255));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark grey"),
                             mk_rgb(169,169,169));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkGrey"),
                             mk_rgb(169,169,169));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark gray"),
                             mk_rgb(169,169,169));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkGray"),
                             mk_rgb(169,169,169));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark blue"),
                             mk_rgb(0,0,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkBlue"),
                             mk_rgb(0,0,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark cyan"),
                             mk_rgb(0,139,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkCyan"),
                             mk_rgb(0,139,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark magenta"),
                             mk_rgb(139,0,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkMagenta"),
                             mk_rgb(139,0,139));
    insert_hash(&X11_colors, wastrsave(stringsp, "dark red"),
                             mk_rgb(139,0,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "DarkRed"),
                             mk_rgb(139,0,0));
    insert_hash(&X11_colors, wastrsave(stringsp, "light green"),
                             mk_rgb(144,238,144));
    insert_hash(&X11_colors, wastrsave(stringsp, "LightGreen"),
                             mk_rgb(144,238,144));
}

static void
rgb2hsv(int r, int g, int b, int *hp, int *sp, int *vp)
{
    double R, G, B, Cmin, Cmax, delta;
    R = ((double) r)/255.0;
    G = ((double) g)/255.0;
    B = ((double) b)/255.0;
    Cmin = fmin(R, fmin(G,B));
    Cmax = fmax(R, fmax(G,B));
    delta = Cmax-Cmin;
    *vp = (int) round(100.0 * Cmax);
    if( Cmax == 0.0 ) {
	*sp = 0;
    } else {
	*sp = (int) round((100.0 * delta) / Cmax);
    }
    if( Cmax == Cmin ) {
	*hp = 0;
    } else if( Cmax == R) {
	*hp = ((int) round(60.0*(G-B)/delta + 360.0)) % 360;
    } else if( Cmax == G) {
	*hp = ((int) round(60.0*((B-R)/delta) + 120.0)) % 360;
    } else {
	*hp = ((int) round(60.0*((R-G)/delta) + 240.0)) % 360;
    }
    return;
}

static int
image_sha256_fn(int *g_cntp, hash_record *g_tblp, SHA256_ptr sha, pointer a)
{
    (void) g_tblp;
    image_ptr ip = (image_ptr) a;
    int res = *g_cntp;
    *g_cntp = res+1;
    SHA256_printf(sha,"%d=image %s %d %d\n", res, ip->name, ip->rows, ip->cols);
    for(int r = 0; r < ip->rows; r++) {
	for(int c = 0; c < ip->cols; c++) {
	    color_ptr	cp;
	    cp = GET_PIXEL(ip, c, r);
	    SHA256_printf(sha,"%c %c %c %c\n", cp->valid, cp->r, cp->g, cp->b);
	}
    }
    return res;
}

