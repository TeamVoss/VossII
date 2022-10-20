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
extern str_mgr		strings;
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
static void      mark_image_fn(pointer p);
static void      sweep_image_fn(void);
static void      save_image_fn(FILE *fp, pointer p);
static pointer   load_image_fn(FILE *fp);
static string    image2str_fn(pointer p);
static formula   image_eq_fn(pointer p1, pointer p2, bool identical);
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
                                   NULL,
                                   NULL,
				   image_sha256_fn);
    image_handle_tp = Get_Type("image", NULL, TP_INSERT_FULL_TYPE);
}

/********************************************************/
/*	    EXPORTED EXTAPI FUNCTIONS    		*/
/********************************************************/

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
    string name = wastrsave(&strings, start+2);
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
image_size(g_ptr redex)
{
    g_ptr g_image;
    EXTRACT_1_ARG(redex, g_image);
    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    MAKE_REDEX_CONS_ND(redex, Make_INT_leaf(ip->rows), Make_INT_leaf(ip->cols));
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
    MAKE_REDEX_CONS_ND(redex, vld, rgb);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

typedef struct ppm_color {
    uchar	r;
    uchar	g;
    uchar	b;
} ppm_color;

static void
export_to_ppm(g_ptr redex)
{
    g_ptr g_image, g_file;
    EXTRACT_2_ARGS(redex, g_image, g_file);
    string file = GET_STRING(g_file);
    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    FILE *fp = fopen(file, "w");
    if( fp == NULL ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Cannot open file %s for writing", file));
	return;
    }
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
    fclose(fp);
    MAKE_REDEX_VOID(redex);
}

static void
hsv_filter(g_ptr redex)
{
    g_ptr g_image, h_range, s_range, v_range, res_color;
    EXTRACT_5_ARGS(redex, g_image, h_range, s_range, v_range, res_color);
    int min_h = GET_INT(GET_CONS_HD(h_range));
    int max_h = GET_INT(GET_CONS_TL(h_range));
    if( max_h < min_h ) {
	string msg = Fail_pr("H_max(%d) < H_min(%d) in bsv_filter",max_h,min_h);
	MAKE_REDEX_FAILURE(redex,msg);
	return;
    }
    int min_s = GET_INT(GET_CONS_HD(s_range));
    int max_s = GET_INT(GET_CONS_TL(s_range));
    if( max_s < min_s ) {
	string msg = Fail_pr("S_max(%d) < S_min(%d) in bsv_filter",max_s,min_s);
	MAKE_REDEX_FAILURE(redex,msg);
	return;
    }
    int min_v = GET_INT(GET_CONS_HD(v_range));
    int max_v = GET_INT(GET_CONS_TL(v_range));
    if( max_v < min_v ) {
	string msg = Fail_pr("H_max(%d) < H_min(%d) in bsv_filter",max_v,min_v);
	MAKE_REDEX_FAILURE(redex,msg);
	return;
    }
    if( max_v < 0 ) {
	min_v = min_v % 360;
	max_v = max_v % 360;
    }
    if( max_v < min_v ) {
	string msg = Fail_pr("H_max(%d) < H_min(%d) in bsv_filter",max_v,min_v);
	MAKE_REDEX_FAILURE(redex,msg);
	return;
    }
    int res_r = GET_INT(GET_CONS_HD(res_color));
    int res_g = GET_INT(GET_CONS_HD(GET_CONS_TL(res_color)));
    int res_b = GET_INT(GET_CONS_TL(GET_CONS_TL(res_color)));
    image_ptr ip = (image_ptr) GET_EXT_OBJ(g_image);
    image_ptr nip = create_image(ip->name, ip->rows, ip->cols);
    for(int r = 0; r < ip->rows; r++) {
	for(int c = 0; c < ip->cols; c++) {
	    color_ptr cp = GET_PIXEL(ip, c, r);
	    color_ptr ncp = GET_PIXEL(nip, c, r);
	    ncp->valid = FALSE;
	    if( cp->valid ) {
		int h, s, v;
		rgb2hsv(cp->r, cp->g, cp->b, &h, &s, &v);
		if( min_v > v ) continue;
		if( max_v < v ) continue;
		if( min_s > s ) continue;
		if( max_s < s ) continue;
		if( min_h < 0 ) {
		    if( max_h < h && h < (min_h+360) ) continue;
		} else {
		    if( min_h > h ) continue;
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


void
Image_Install_Functions()
{
   Add_ExtAPI_Function("import_xpm_image", "1", FALSE,
                        GLmake_arrow(GLmake_string(), image_handle_tp),
                        import_xpm_image);

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

   Add_ExtAPI_Function("export_to_ppm", "11", FALSE,
                        GLmake_arrow(
			    image_handle_tp,
			    GLmake_arrow(GLmake_string(), GLmake_void())),
                        export_to_ppm);

   Add_ExtAPI_Function("image_size", "1", FALSE,
                        GLmake_arrow(
			    image_handle_tp,
			    GLmake_tuple(GLmake_int(), GLmake_int())),
                        image_size);

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
    sprintf(im_read_buf, "Image with %d rows and %d columns\n",ip->rows,ip->cols);
    string msg = wastrsave(&strings, im_read_buf);
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
    ip->name = wastrsave(&strings, name);
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
    insert_hash(&X11_colors, wastrsave(&strings, "None"), cp);
    //
    //
    // Generated from /etc/X11/rgb.txt
    //
    insert_hash(&X11_colors, wastrsave(&strings, "snow"),
                             mk_rgb(255,250,250));
    insert_hash(&X11_colors, wastrsave(&strings, "ghost white"),
                             mk_rgb(248,248,255));
    insert_hash(&X11_colors, wastrsave(&strings, "GhostWhite"),
                             mk_rgb(248,248,255));
    insert_hash(&X11_colors, wastrsave(&strings, "white smoke"),
                             mk_rgb(245,245,245));
    insert_hash(&X11_colors, wastrsave(&strings, "WhiteSmoke"),
                             mk_rgb(245,245,245));
    insert_hash(&X11_colors, wastrsave(&strings, "gainsboro"),
                             mk_rgb(220,220,220));
    insert_hash(&X11_colors, wastrsave(&strings, "floral white"),
                             mk_rgb(255,250,240));
    insert_hash(&X11_colors, wastrsave(&strings, "FloralWhite"),
                             mk_rgb(255,250,240));
    insert_hash(&X11_colors, wastrsave(&strings, "old lace"),
                             mk_rgb(253,245,230));
    insert_hash(&X11_colors, wastrsave(&strings, "OldLace"),
                             mk_rgb(253,245,230));
    insert_hash(&X11_colors, wastrsave(&strings, "linen"),
                             mk_rgb(250,240,230));
    insert_hash(&X11_colors, wastrsave(&strings, "antique white"),
                             mk_rgb(250,235,215));
    insert_hash(&X11_colors, wastrsave(&strings, "AntiqueWhite"),
                             mk_rgb(250,235,215));
    insert_hash(&X11_colors, wastrsave(&strings, "papaya whip"),
                             mk_rgb(255,239,213));
    insert_hash(&X11_colors, wastrsave(&strings, "PapayaWhip"),
                             mk_rgb(255,239,213));
    insert_hash(&X11_colors, wastrsave(&strings, "blanched almond"),
                             mk_rgb(255,235,205));
    insert_hash(&X11_colors, wastrsave(&strings, "BlanchedAlmond"),
                             mk_rgb(255,235,205));
    insert_hash(&X11_colors, wastrsave(&strings, "bisque"),
                             mk_rgb(255,228,196));
    insert_hash(&X11_colors, wastrsave(&strings, "peach puff"),
                             mk_rgb(255,218,185));
    insert_hash(&X11_colors, wastrsave(&strings, "PeachPuff"),
                             mk_rgb(255,218,185));
    insert_hash(&X11_colors, wastrsave(&strings, "navajo white"),
                             mk_rgb(255,222,173));
    insert_hash(&X11_colors, wastrsave(&strings, "NavajoWhite"),
                             mk_rgb(255,222,173));
    insert_hash(&X11_colors, wastrsave(&strings, "moccasin"),
                             mk_rgb(255,228,181));
    insert_hash(&X11_colors, wastrsave(&strings, "cornsilk"),
                             mk_rgb(255,248,220));
    insert_hash(&X11_colors, wastrsave(&strings, "ivory"),
                             mk_rgb(255,255,240));
    insert_hash(&X11_colors, wastrsave(&strings, "lemon chiffon"),
                             mk_rgb(255,250,205));
    insert_hash(&X11_colors, wastrsave(&strings, "LemonChiffon"),
                             mk_rgb(255,250,205));
    insert_hash(&X11_colors, wastrsave(&strings, "seashell"),
                             mk_rgb(255,245,238));
    insert_hash(&X11_colors, wastrsave(&strings, "honeydew"),
                             mk_rgb(240,255,240));
    insert_hash(&X11_colors, wastrsave(&strings, "mint cream"),
                             mk_rgb(245,255,250));
    insert_hash(&X11_colors, wastrsave(&strings, "MintCream"),
                             mk_rgb(245,255,250));
    insert_hash(&X11_colors, wastrsave(&strings, "azure"),
                             mk_rgb(240,255,255));
    insert_hash(&X11_colors, wastrsave(&strings, "alice blue"),
                             mk_rgb(240,248,255));
    insert_hash(&X11_colors, wastrsave(&strings, "AliceBlue"),
                             mk_rgb(240,248,255));
    insert_hash(&X11_colors, wastrsave(&strings, "lavender"),
                             mk_rgb(230,230,250));
    insert_hash(&X11_colors, wastrsave(&strings, "lavender blush"),
                             mk_rgb(255,240,245));
    insert_hash(&X11_colors, wastrsave(&strings, "LavenderBlush"),
                             mk_rgb(255,240,245));
    insert_hash(&X11_colors, wastrsave(&strings, "misty rose"),
                             mk_rgb(255,228,225));
    insert_hash(&X11_colors, wastrsave(&strings, "MistyRose"),
                             mk_rgb(255,228,225));
    insert_hash(&X11_colors, wastrsave(&strings, "white"),
                             mk_rgb(255,255,255));
    insert_hash(&X11_colors, wastrsave(&strings, "black"),
                             mk_rgb(0,0,0));
    insert_hash(&X11_colors, wastrsave(&strings, "dark slate gray"),
                             mk_rgb(47,79,79));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkSlateGray"),
                             mk_rgb(47,79,79));
    insert_hash(&X11_colors, wastrsave(&strings, "dark slate grey"),
                             mk_rgb(47,79,79));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkSlateGrey"),
                             mk_rgb(47,79,79));
    insert_hash(&X11_colors, wastrsave(&strings, "dim gray"),
                             mk_rgb(105,105,105));
    insert_hash(&X11_colors, wastrsave(&strings, "DimGray"),
                             mk_rgb(105,105,105));
    insert_hash(&X11_colors, wastrsave(&strings, "dim grey"),
                             mk_rgb(105,105,105));
    insert_hash(&X11_colors, wastrsave(&strings, "DimGrey"),
                             mk_rgb(105,105,105));
    insert_hash(&X11_colors, wastrsave(&strings, "slate gray"),
                             mk_rgb(112,128,144));
    insert_hash(&X11_colors, wastrsave(&strings, "SlateGray"),
                             mk_rgb(112,128,144));
    insert_hash(&X11_colors, wastrsave(&strings, "slate grey"),
                             mk_rgb(112,128,144));
    insert_hash(&X11_colors, wastrsave(&strings, "SlateGrey"),
                             mk_rgb(112,128,144));
    insert_hash(&X11_colors, wastrsave(&strings, "light slate gray"),
                             mk_rgb(119,136,153));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSlateGray"),
                             mk_rgb(119,136,153));
    insert_hash(&X11_colors, wastrsave(&strings, "light slate grey"),
                             mk_rgb(119,136,153));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSlateGrey"),
                             mk_rgb(119,136,153));
    insert_hash(&X11_colors, wastrsave(&strings, "gray"),
                             mk_rgb(190,190,190));
    insert_hash(&X11_colors, wastrsave(&strings, "grey"),
                             mk_rgb(190,190,190));
    insert_hash(&X11_colors, wastrsave(&strings, "light grey"),
                             mk_rgb(211,211,211));
    insert_hash(&X11_colors, wastrsave(&strings, "LightGrey"),
                             mk_rgb(211,211,211));
    insert_hash(&X11_colors, wastrsave(&strings, "light gray"),
                             mk_rgb(211,211,211));
    insert_hash(&X11_colors, wastrsave(&strings, "LightGray"),
                             mk_rgb(211,211,211));
    insert_hash(&X11_colors, wastrsave(&strings, "midnight blue"),
                             mk_rgb(25,25,112));
    insert_hash(&X11_colors, wastrsave(&strings, "MidnightBlue"),
                             mk_rgb(25,25,112));
    insert_hash(&X11_colors, wastrsave(&strings, "navy"),
                             mk_rgb(0,0,128));
    insert_hash(&X11_colors, wastrsave(&strings, "navy blue"),
                             mk_rgb(0,0,128));
    insert_hash(&X11_colors, wastrsave(&strings, "NavyBlue"),
                             mk_rgb(0,0,128));
    insert_hash(&X11_colors, wastrsave(&strings, "cornflower blue"),
                             mk_rgb(100,149,237));
    insert_hash(&X11_colors, wastrsave(&strings, "CornflowerBlue"),
                             mk_rgb(100,149,237));
    insert_hash(&X11_colors, wastrsave(&strings, "dark slate blue"),
                             mk_rgb(72,61,139));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkSlateBlue"),
                             mk_rgb(72,61,139));
    insert_hash(&X11_colors, wastrsave(&strings, "slate blue"),
                             mk_rgb(106,90,205));
    insert_hash(&X11_colors, wastrsave(&strings, "SlateBlue"),
                             mk_rgb(106,90,205));
    insert_hash(&X11_colors, wastrsave(&strings, "medium slate blue"),
                             mk_rgb(123,104,238));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumSlateBlue"),
                             mk_rgb(123,104,238));
    insert_hash(&X11_colors, wastrsave(&strings, "light slate blue"),
                             mk_rgb(132,112,255));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSlateBlue"),
                             mk_rgb(132,112,255));
    insert_hash(&X11_colors, wastrsave(&strings, "medium blue"),
                             mk_rgb(0,0,205));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumBlue"),
                             mk_rgb(0,0,205));
    insert_hash(&X11_colors, wastrsave(&strings, "royal blue"),
                             mk_rgb(65,105,225));
    insert_hash(&X11_colors, wastrsave(&strings, "RoyalBlue"),
                             mk_rgb(65,105,225));
    insert_hash(&X11_colors, wastrsave(&strings, "blue"),
                             mk_rgb(0,0,255));
    insert_hash(&X11_colors, wastrsave(&strings, "dodger blue"),
                             mk_rgb(30,144,255));
    insert_hash(&X11_colors, wastrsave(&strings, "DodgerBlue"),
                             mk_rgb(30,144,255));
    insert_hash(&X11_colors, wastrsave(&strings, "deep sky blue"),
                             mk_rgb(0,191,255));
    insert_hash(&X11_colors, wastrsave(&strings, "DeepSkyBlue"),
                             mk_rgb(0,191,255));
    insert_hash(&X11_colors, wastrsave(&strings, "sky blue"),
                             mk_rgb(135,206,235));
    insert_hash(&X11_colors, wastrsave(&strings, "SkyBlue"),
                             mk_rgb(135,206,235));
    insert_hash(&X11_colors, wastrsave(&strings, "light sky blue"),
                             mk_rgb(135,206,250));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSkyBlue"),
                             mk_rgb(135,206,250));
    insert_hash(&X11_colors, wastrsave(&strings, "steel blue"),
                             mk_rgb(70,130,180));
    insert_hash(&X11_colors, wastrsave(&strings, "SteelBlue"),
                             mk_rgb(70,130,180));
    insert_hash(&X11_colors, wastrsave(&strings, "light steel blue"),
                             mk_rgb(176,196,222));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSteelBlue"),
                             mk_rgb(176,196,222));
    insert_hash(&X11_colors, wastrsave(&strings, "light blue"),
                             mk_rgb(173,216,230));
    insert_hash(&X11_colors, wastrsave(&strings, "LightBlue"),
                             mk_rgb(173,216,230));
    insert_hash(&X11_colors, wastrsave(&strings, "powder blue"),
                             mk_rgb(176,224,230));
    insert_hash(&X11_colors, wastrsave(&strings, "PowderBlue"),
                             mk_rgb(176,224,230));
    insert_hash(&X11_colors, wastrsave(&strings, "pale turquoise"),
                             mk_rgb(175,238,238));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleTurquoise"),
                             mk_rgb(175,238,238));
    insert_hash(&X11_colors, wastrsave(&strings, "dark turquoise"),
                             mk_rgb(0,206,209));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkTurquoise"),
                             mk_rgb(0,206,209));
    insert_hash(&X11_colors, wastrsave(&strings, "medium turquoise"),
                             mk_rgb(72,209,204));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumTurquoise"),
                             mk_rgb(72,209,204));
    insert_hash(&X11_colors, wastrsave(&strings, "turquoise"),
                             mk_rgb(64,224,208));
    insert_hash(&X11_colors, wastrsave(&strings, "cyan"),
                             mk_rgb(0,255,255));
    insert_hash(&X11_colors, wastrsave(&strings, "light cyan"),
                             mk_rgb(224,255,255));
    insert_hash(&X11_colors, wastrsave(&strings, "LightCyan"),
                             mk_rgb(224,255,255));
    insert_hash(&X11_colors, wastrsave(&strings, "cadet blue"),
                             mk_rgb(95,158,160));
    insert_hash(&X11_colors, wastrsave(&strings, "CadetBlue"),
                             mk_rgb(95,158,160));
    insert_hash(&X11_colors, wastrsave(&strings, "medium aquamarine"),
                             mk_rgb(102,205,170));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumAquamarine"),
                             mk_rgb(102,205,170));
    insert_hash(&X11_colors, wastrsave(&strings, "aquamarine"),
                             mk_rgb(127,255,212));
    insert_hash(&X11_colors, wastrsave(&strings, "dark green"),
                             mk_rgb(0,100,0));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkGreen"),
                             mk_rgb(0,100,0));
    insert_hash(&X11_colors, wastrsave(&strings, "dark olive green"),
                             mk_rgb(85,107,47));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkOliveGreen"),
                             mk_rgb(85,107,47));
    insert_hash(&X11_colors, wastrsave(&strings, "dark sea green"),
                             mk_rgb(143,188,143));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkSeaGreen"),
                             mk_rgb(143,188,143));
    insert_hash(&X11_colors, wastrsave(&strings, "sea green"),
                             mk_rgb(46,139,87));
    insert_hash(&X11_colors, wastrsave(&strings, "SeaGreen"),
                             mk_rgb(46,139,87));
    insert_hash(&X11_colors, wastrsave(&strings, "medium sea green"),
                             mk_rgb(60,179,113));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumSeaGreen"),
                             mk_rgb(60,179,113));
    insert_hash(&X11_colors, wastrsave(&strings, "light sea green"),
                             mk_rgb(32,178,170));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSeaGreen"),
                             mk_rgb(32,178,170));
    insert_hash(&X11_colors, wastrsave(&strings, "pale green"),
                             mk_rgb(152,251,152));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleGreen"),
                             mk_rgb(152,251,152));
    insert_hash(&X11_colors, wastrsave(&strings, "spring green"),
                             mk_rgb(0,255,127));
    insert_hash(&X11_colors, wastrsave(&strings, "SpringGreen"),
                             mk_rgb(0,255,127));
    insert_hash(&X11_colors, wastrsave(&strings, "lawn green"),
                             mk_rgb(124,252,0));
    insert_hash(&X11_colors, wastrsave(&strings, "LawnGreen"),
                             mk_rgb(124,252,0));
    insert_hash(&X11_colors, wastrsave(&strings, "green"),
                             mk_rgb(0,255,0));
    insert_hash(&X11_colors, wastrsave(&strings, "chartreuse"),
                             mk_rgb(127,255,0));
    insert_hash(&X11_colors, wastrsave(&strings, "medium spring green"),
                             mk_rgb(0,250,154));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumSpringGreen"),
                             mk_rgb(0,250,154));
    insert_hash(&X11_colors, wastrsave(&strings, "green yellow"),
                             mk_rgb(173,255,47));
    insert_hash(&X11_colors, wastrsave(&strings, "GreenYellow"),
                             mk_rgb(173,255,47));
    insert_hash(&X11_colors, wastrsave(&strings, "lime green"),
                             mk_rgb(50,205,50));
    insert_hash(&X11_colors, wastrsave(&strings, "LimeGreen"),
                             mk_rgb(50,205,50));
    insert_hash(&X11_colors, wastrsave(&strings, "yellow green"),
                             mk_rgb(154,205,50));
    insert_hash(&X11_colors, wastrsave(&strings, "YellowGreen"),
                             mk_rgb(154,205,50));
    insert_hash(&X11_colors, wastrsave(&strings, "forest green"),
                             mk_rgb(34,139,34));
    insert_hash(&X11_colors, wastrsave(&strings, "ForestGreen"),
                             mk_rgb(34,139,34));
    insert_hash(&X11_colors, wastrsave(&strings, "olive drab"),
                             mk_rgb(107,142,35));
    insert_hash(&X11_colors, wastrsave(&strings, "OliveDrab"),
                             mk_rgb(107,142,35));
    insert_hash(&X11_colors, wastrsave(&strings, "dark khaki"),
                             mk_rgb(189,183,107));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkKhaki"),
                             mk_rgb(189,183,107));
    insert_hash(&X11_colors, wastrsave(&strings, "khaki"),
                             mk_rgb(240,230,140));
    insert_hash(&X11_colors, wastrsave(&strings, "pale goldenrod"),
                             mk_rgb(238,232,170));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleGoldenrod"),
                             mk_rgb(238,232,170));
    insert_hash(&X11_colors, wastrsave(&strings, "light goldenrod yellow"),
                             mk_rgb(250,250,210));
    insert_hash(&X11_colors, wastrsave(&strings, "LightGoldenrodYellow"),
                             mk_rgb(250,250,210));
    insert_hash(&X11_colors, wastrsave(&strings, "light yellow"),
                             mk_rgb(255,255,224));
    insert_hash(&X11_colors, wastrsave(&strings, "LightYellow"),
                             mk_rgb(255,255,224));
    insert_hash(&X11_colors, wastrsave(&strings, "yellow"),
                             mk_rgb(255,255,0));
    insert_hash(&X11_colors, wastrsave(&strings, "gold"),
                             mk_rgb(255,215,0));
    insert_hash(&X11_colors, wastrsave(&strings, "light goldenrod"),
                             mk_rgb(238,221,130));
    insert_hash(&X11_colors, wastrsave(&strings, "LightGoldenrod"),
                             mk_rgb(238,221,130));
    insert_hash(&X11_colors, wastrsave(&strings, "goldenrod"),
                             mk_rgb(218,165,32));
    insert_hash(&X11_colors, wastrsave(&strings, "dark goldenrod"),
                             mk_rgb(184,134,11));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkGoldenrod"),
                             mk_rgb(184,134,11));
    insert_hash(&X11_colors, wastrsave(&strings, "rosy brown"),
                             mk_rgb(188,143,143));
    insert_hash(&X11_colors, wastrsave(&strings, "RosyBrown"),
                             mk_rgb(188,143,143));
    insert_hash(&X11_colors, wastrsave(&strings, "indian red"),
                             mk_rgb(205,92,92));
    insert_hash(&X11_colors, wastrsave(&strings, "IndianRed"),
                             mk_rgb(205,92,92));
    insert_hash(&X11_colors, wastrsave(&strings, "saddle brown"),
                             mk_rgb(139,69,19));
    insert_hash(&X11_colors, wastrsave(&strings, "SaddleBrown"),
                             mk_rgb(139,69,19));
    insert_hash(&X11_colors, wastrsave(&strings, "sienna"),
                             mk_rgb(160,82,45));
    insert_hash(&X11_colors, wastrsave(&strings, "peru"),
                             mk_rgb(205,133,63));
    insert_hash(&X11_colors, wastrsave(&strings, "burlywood"),
                             mk_rgb(222,184,135));
    insert_hash(&X11_colors, wastrsave(&strings, "beige"),
                             mk_rgb(245,245,220));
    insert_hash(&X11_colors, wastrsave(&strings, "wheat"),
                             mk_rgb(245,222,179));
    insert_hash(&X11_colors, wastrsave(&strings, "sandy brown"),
                             mk_rgb(244,164,96));
    insert_hash(&X11_colors, wastrsave(&strings, "SandyBrown"),
                             mk_rgb(244,164,96));
    insert_hash(&X11_colors, wastrsave(&strings, "tan"),
                             mk_rgb(210,180,140));
    insert_hash(&X11_colors, wastrsave(&strings, "chocolate"),
                             mk_rgb(210,105,30));
    insert_hash(&X11_colors, wastrsave(&strings, "firebrick"),
                             mk_rgb(178,34,34));
    insert_hash(&X11_colors, wastrsave(&strings, "brown"),
                             mk_rgb(165,42,42));
    insert_hash(&X11_colors, wastrsave(&strings, "dark salmon"),
                             mk_rgb(233,150,122));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkSalmon"),
                             mk_rgb(233,150,122));
    insert_hash(&X11_colors, wastrsave(&strings, "salmon"),
                             mk_rgb(250,128,114));
    insert_hash(&X11_colors, wastrsave(&strings, "light salmon"),
                             mk_rgb(255,160,122));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSalmon"),
                             mk_rgb(255,160,122));
    insert_hash(&X11_colors, wastrsave(&strings, "orange"),
                             mk_rgb(255,165,0));
    insert_hash(&X11_colors, wastrsave(&strings, "dark orange"),
                             mk_rgb(255,140,0));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkOrange"),
                             mk_rgb(255,140,0));
    insert_hash(&X11_colors, wastrsave(&strings, "coral"),
                             mk_rgb(255,127,80));
    insert_hash(&X11_colors, wastrsave(&strings, "light coral"),
                             mk_rgb(240,128,128));
    insert_hash(&X11_colors, wastrsave(&strings, "LightCoral"),
                             mk_rgb(240,128,128));
    insert_hash(&X11_colors, wastrsave(&strings, "tomato"),
                             mk_rgb(255,99,71));
    insert_hash(&X11_colors, wastrsave(&strings, "orange red"),
                             mk_rgb(255,69,0));
    insert_hash(&X11_colors, wastrsave(&strings, "OrangeRed"),
                             mk_rgb(255,69,0));
    insert_hash(&X11_colors, wastrsave(&strings, "red"),
                             mk_rgb(255,0,0));
    insert_hash(&X11_colors, wastrsave(&strings, "hot pink"),
                             mk_rgb(255,105,180));
    insert_hash(&X11_colors, wastrsave(&strings, "HotPink"),
                             mk_rgb(255,105,180));
    insert_hash(&X11_colors, wastrsave(&strings, "deep pink"),
                             mk_rgb(255,20,147));
    insert_hash(&X11_colors, wastrsave(&strings, "DeepPink"),
                             mk_rgb(255,20,147));
    insert_hash(&X11_colors, wastrsave(&strings, "pink"),
                             mk_rgb(255,192,203));
    insert_hash(&X11_colors, wastrsave(&strings, "light pink"),
                             mk_rgb(255,182,193));
    insert_hash(&X11_colors, wastrsave(&strings, "LightPink"),
                             mk_rgb(255,182,193));
    insert_hash(&X11_colors, wastrsave(&strings, "pale violet red"),
                             mk_rgb(219,112,147));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleVioletRed"),
                             mk_rgb(219,112,147));
    insert_hash(&X11_colors, wastrsave(&strings, "maroon"),
                             mk_rgb(176,48,96));
    insert_hash(&X11_colors, wastrsave(&strings, "medium violet red"),
                             mk_rgb(199,21,133));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumVioletRed"),
                             mk_rgb(199,21,133));
    insert_hash(&X11_colors, wastrsave(&strings, "violet red"),
                             mk_rgb(208,32,144));
    insert_hash(&X11_colors, wastrsave(&strings, "VioletRed"),
                             mk_rgb(208,32,144));
    insert_hash(&X11_colors, wastrsave(&strings, "magenta"),
                             mk_rgb(255,0,255));
    insert_hash(&X11_colors, wastrsave(&strings, "violet"),
                             mk_rgb(238,130,238));
    insert_hash(&X11_colors, wastrsave(&strings, "plum"),
                             mk_rgb(221,160,221));
    insert_hash(&X11_colors, wastrsave(&strings, "orchid"),
                             mk_rgb(218,112,214));
    insert_hash(&X11_colors, wastrsave(&strings, "medium orchid"),
                             mk_rgb(186,85,211));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumOrchid"),
                             mk_rgb(186,85,211));
    insert_hash(&X11_colors, wastrsave(&strings, "dark orchid"),
                             mk_rgb(153,50,204));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkOrchid"),
                             mk_rgb(153,50,204));
    insert_hash(&X11_colors, wastrsave(&strings, "dark violet"),
                             mk_rgb(148,0,211));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkViolet"),
                             mk_rgb(148,0,211));
    insert_hash(&X11_colors, wastrsave(&strings, "blue violet"),
                             mk_rgb(138,43,226));
    insert_hash(&X11_colors, wastrsave(&strings, "BlueViolet"),
                             mk_rgb(138,43,226));
    insert_hash(&X11_colors, wastrsave(&strings, "purple"),
                             mk_rgb(160,32,240));
    insert_hash(&X11_colors, wastrsave(&strings, "medium purple"),
                             mk_rgb(147,112,219));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumPurple"),
                             mk_rgb(147,112,219));
    insert_hash(&X11_colors, wastrsave(&strings, "thistle"),
                             mk_rgb(216,191,216));
    insert_hash(&X11_colors, wastrsave(&strings, "snow1"),
                             mk_rgb(255,250,250));
    insert_hash(&X11_colors, wastrsave(&strings, "snow2"),
                             mk_rgb(238,233,233));
    insert_hash(&X11_colors, wastrsave(&strings, "snow3"),
                             mk_rgb(205,201,201));
    insert_hash(&X11_colors, wastrsave(&strings, "snow4"),
                             mk_rgb(139,137,137));
    insert_hash(&X11_colors, wastrsave(&strings, "seashell1"),
                             mk_rgb(255,245,238));
    insert_hash(&X11_colors, wastrsave(&strings, "seashell2"),
                             mk_rgb(238,229,222));
    insert_hash(&X11_colors, wastrsave(&strings, "seashell3"),
                             mk_rgb(205,197,191));
    insert_hash(&X11_colors, wastrsave(&strings, "seashell4"),
                             mk_rgb(139,134,130));
    insert_hash(&X11_colors, wastrsave(&strings, "AntiqueWhite1"),
                             mk_rgb(255,239,219));
    insert_hash(&X11_colors, wastrsave(&strings, "AntiqueWhite2"),
                             mk_rgb(238,223,204));
    insert_hash(&X11_colors, wastrsave(&strings, "AntiqueWhite3"),
                             mk_rgb(205,192,176));
    insert_hash(&X11_colors, wastrsave(&strings, "AntiqueWhite4"),
                             mk_rgb(139,131,120));
    insert_hash(&X11_colors, wastrsave(&strings, "bisque1"),
                             mk_rgb(255,228,196));
    insert_hash(&X11_colors, wastrsave(&strings, "bisque2"),
                             mk_rgb(238,213,183));
    insert_hash(&X11_colors, wastrsave(&strings, "bisque3"),
                             mk_rgb(205,183,158));
    insert_hash(&X11_colors, wastrsave(&strings, "bisque4"),
                             mk_rgb(139,125,107));
    insert_hash(&X11_colors, wastrsave(&strings, "PeachPuff1"),
                             mk_rgb(255,218,185));
    insert_hash(&X11_colors, wastrsave(&strings, "PeachPuff2"),
                             mk_rgb(238,203,173));
    insert_hash(&X11_colors, wastrsave(&strings, "PeachPuff3"),
                             mk_rgb(205,175,149));
    insert_hash(&X11_colors, wastrsave(&strings, "PeachPuff4"),
                             mk_rgb(139,119,101));
    insert_hash(&X11_colors, wastrsave(&strings, "NavajoWhite1"),
                             mk_rgb(255,222,173));
    insert_hash(&X11_colors, wastrsave(&strings, "NavajoWhite2"),
                             mk_rgb(238,207,161));
    insert_hash(&X11_colors, wastrsave(&strings, "NavajoWhite3"),
                             mk_rgb(205,179,139));
    insert_hash(&X11_colors, wastrsave(&strings, "NavajoWhite4"),
                             mk_rgb(139,121,94));
    insert_hash(&X11_colors, wastrsave(&strings, "LemonChiffon1"),
                             mk_rgb(255,250,205));
    insert_hash(&X11_colors, wastrsave(&strings, "LemonChiffon2"),
                             mk_rgb(238,233,191));
    insert_hash(&X11_colors, wastrsave(&strings, "LemonChiffon3"),
                             mk_rgb(205,201,165));
    insert_hash(&X11_colors, wastrsave(&strings, "LemonChiffon4"),
                             mk_rgb(139,137,112));
    insert_hash(&X11_colors, wastrsave(&strings, "cornsilk1"),
                             mk_rgb(255,248,220));
    insert_hash(&X11_colors, wastrsave(&strings, "cornsilk2"),
                             mk_rgb(238,232,205));
    insert_hash(&X11_colors, wastrsave(&strings, "cornsilk3"),
                             mk_rgb(205,200,177));
    insert_hash(&X11_colors, wastrsave(&strings, "cornsilk4"),
                             mk_rgb(139,136,120));
    insert_hash(&X11_colors, wastrsave(&strings, "ivory1"),
                             mk_rgb(255,255,240));
    insert_hash(&X11_colors, wastrsave(&strings, "ivory2"),
                             mk_rgb(238,238,224));
    insert_hash(&X11_colors, wastrsave(&strings, "ivory3"),
                             mk_rgb(205,205,193));
    insert_hash(&X11_colors, wastrsave(&strings, "ivory4"),
                             mk_rgb(139,139,131));
    insert_hash(&X11_colors, wastrsave(&strings, "honeydew1"),
                             mk_rgb(240,255,240));
    insert_hash(&X11_colors, wastrsave(&strings, "honeydew2"),
                             mk_rgb(224,238,224));
    insert_hash(&X11_colors, wastrsave(&strings, "honeydew3"),
                             mk_rgb(193,205,193));
    insert_hash(&X11_colors, wastrsave(&strings, "honeydew4"),
                             mk_rgb(131,139,131));
    insert_hash(&X11_colors, wastrsave(&strings, "LavenderBlush1"),
                             mk_rgb(255,240,245));
    insert_hash(&X11_colors, wastrsave(&strings, "LavenderBlush2"),
                             mk_rgb(238,224,229));
    insert_hash(&X11_colors, wastrsave(&strings, "LavenderBlush3"),
                             mk_rgb(205,193,197));
    insert_hash(&X11_colors, wastrsave(&strings, "LavenderBlush4"),
                             mk_rgb(139,131,134));
    insert_hash(&X11_colors, wastrsave(&strings, "MistyRose1"),
                             mk_rgb(255,228,225));
    insert_hash(&X11_colors, wastrsave(&strings, "MistyRose2"),
                             mk_rgb(238,213,210));
    insert_hash(&X11_colors, wastrsave(&strings, "MistyRose3"),
                             mk_rgb(205,183,181));
    insert_hash(&X11_colors, wastrsave(&strings, "MistyRose4"),
                             mk_rgb(139,125,123));
    insert_hash(&X11_colors, wastrsave(&strings, "azure1"),
                             mk_rgb(240,255,255));
    insert_hash(&X11_colors, wastrsave(&strings, "azure2"),
                             mk_rgb(224,238,238));
    insert_hash(&X11_colors, wastrsave(&strings, "azure3"),
                             mk_rgb(193,205,205));
    insert_hash(&X11_colors, wastrsave(&strings, "azure4"),
                             mk_rgb(131,139,139));
    insert_hash(&X11_colors, wastrsave(&strings, "SlateBlue1"),
                             mk_rgb(131,111,255));
    insert_hash(&X11_colors, wastrsave(&strings, "SlateBlue2"),
                             mk_rgb(122,103,238));
    insert_hash(&X11_colors, wastrsave(&strings, "SlateBlue3"),
                             mk_rgb(105,89,205));
    insert_hash(&X11_colors, wastrsave(&strings, "SlateBlue4"),
                             mk_rgb(71,60,139));
    insert_hash(&X11_colors, wastrsave(&strings, "RoyalBlue1"),
                             mk_rgb(72,118,255));
    insert_hash(&X11_colors, wastrsave(&strings, "RoyalBlue2"),
                             mk_rgb(67,110,238));
    insert_hash(&X11_colors, wastrsave(&strings, "RoyalBlue3"),
                             mk_rgb(58,95,205));
    insert_hash(&X11_colors, wastrsave(&strings, "RoyalBlue4"),
                             mk_rgb(39,64,139));
    insert_hash(&X11_colors, wastrsave(&strings, "blue1"),
                             mk_rgb(0,0,255));
    insert_hash(&X11_colors, wastrsave(&strings, "blue2"),
                             mk_rgb(0,0,238));
    insert_hash(&X11_colors, wastrsave(&strings, "blue3"),
                             mk_rgb(0,0,205));
    insert_hash(&X11_colors, wastrsave(&strings, "blue4"),
                             mk_rgb(0,0,139));
    insert_hash(&X11_colors, wastrsave(&strings, "DodgerBlue1"),
                             mk_rgb(30,144,255));
    insert_hash(&X11_colors, wastrsave(&strings, "DodgerBlue2"),
                             mk_rgb(28,134,238));
    insert_hash(&X11_colors, wastrsave(&strings, "DodgerBlue3"),
                             mk_rgb(24,116,205));
    insert_hash(&X11_colors, wastrsave(&strings, "DodgerBlue4"),
                             mk_rgb(16,78,139));
    insert_hash(&X11_colors, wastrsave(&strings, "SteelBlue1"),
                             mk_rgb(99,184,255));
    insert_hash(&X11_colors, wastrsave(&strings, "SteelBlue2"),
                             mk_rgb(92,172,238));
    insert_hash(&X11_colors, wastrsave(&strings, "SteelBlue3"),
                             mk_rgb(79,148,205));
    insert_hash(&X11_colors, wastrsave(&strings, "SteelBlue4"),
                             mk_rgb(54,100,139));
    insert_hash(&X11_colors, wastrsave(&strings, "DeepSkyBlue1"),
                             mk_rgb(0,191,255));
    insert_hash(&X11_colors, wastrsave(&strings, "DeepSkyBlue2"),
                             mk_rgb(0,178,238));
    insert_hash(&X11_colors, wastrsave(&strings, "DeepSkyBlue3"),
                             mk_rgb(0,154,205));
    insert_hash(&X11_colors, wastrsave(&strings, "DeepSkyBlue4"),
                             mk_rgb(0,104,139));
    insert_hash(&X11_colors, wastrsave(&strings, "SkyBlue1"),
                             mk_rgb(135,206,255));
    insert_hash(&X11_colors, wastrsave(&strings, "SkyBlue2"),
                             mk_rgb(126,192,238));
    insert_hash(&X11_colors, wastrsave(&strings, "SkyBlue3"),
                             mk_rgb(108,166,205));
    insert_hash(&X11_colors, wastrsave(&strings, "SkyBlue4"),
                             mk_rgb(74,112,139));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSkyBlue1"),
                             mk_rgb(176,226,255));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSkyBlue2"),
                             mk_rgb(164,211,238));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSkyBlue3"),
                             mk_rgb(141,182,205));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSkyBlue4"),
                             mk_rgb(96,123,139));
    insert_hash(&X11_colors, wastrsave(&strings, "SlateGray1"),
                             mk_rgb(198,226,255));
    insert_hash(&X11_colors, wastrsave(&strings, "SlateGray2"),
                             mk_rgb(185,211,238));
    insert_hash(&X11_colors, wastrsave(&strings, "SlateGray3"),
                             mk_rgb(159,182,205));
    insert_hash(&X11_colors, wastrsave(&strings, "SlateGray4"),
                             mk_rgb(108,123,139));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSteelBlue1"),
                             mk_rgb(202,225,255));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSteelBlue2"),
                             mk_rgb(188,210,238));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSteelBlue3"),
                             mk_rgb(162,181,205));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSteelBlue4"),
                             mk_rgb(110,123,139));
    insert_hash(&X11_colors, wastrsave(&strings, "LightBlue1"),
                             mk_rgb(191,239,255));
    insert_hash(&X11_colors, wastrsave(&strings, "LightBlue2"),
                             mk_rgb(178,223,238));
    insert_hash(&X11_colors, wastrsave(&strings, "LightBlue3"),
                             mk_rgb(154,192,205));
    insert_hash(&X11_colors, wastrsave(&strings, "LightBlue4"),
                             mk_rgb(104,131,139));
    insert_hash(&X11_colors, wastrsave(&strings, "LightCyan1"),
                             mk_rgb(224,255,255));
    insert_hash(&X11_colors, wastrsave(&strings, "LightCyan2"),
                             mk_rgb(209,238,238));
    insert_hash(&X11_colors, wastrsave(&strings, "LightCyan3"),
                             mk_rgb(180,205,205));
    insert_hash(&X11_colors, wastrsave(&strings, "LightCyan4"),
                             mk_rgb(122,139,139));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleTurquoise1"),
                             mk_rgb(187,255,255));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleTurquoise2"),
                             mk_rgb(174,238,238));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleTurquoise3"),
                             mk_rgb(150,205,205));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleTurquoise4"),
                             mk_rgb(102,139,139));
    insert_hash(&X11_colors, wastrsave(&strings, "CadetBlue1"),
                             mk_rgb(152,245,255));
    insert_hash(&X11_colors, wastrsave(&strings, "CadetBlue2"),
                             mk_rgb(142,229,238));
    insert_hash(&X11_colors, wastrsave(&strings, "CadetBlue3"),
                             mk_rgb(122,197,205));
    insert_hash(&X11_colors, wastrsave(&strings, "CadetBlue4"),
                             mk_rgb(83,134,139));
    insert_hash(&X11_colors, wastrsave(&strings, "turquoise1"),
                             mk_rgb(0,245,255));
    insert_hash(&X11_colors, wastrsave(&strings, "turquoise2"),
                             mk_rgb(0,229,238));
    insert_hash(&X11_colors, wastrsave(&strings, "turquoise3"),
                             mk_rgb(0,197,205));
    insert_hash(&X11_colors, wastrsave(&strings, "turquoise4"),
                             mk_rgb(0,134,139));
    insert_hash(&X11_colors, wastrsave(&strings, "cyan1"),
                             mk_rgb(0,255,255));
    insert_hash(&X11_colors, wastrsave(&strings, "cyan2"),
                             mk_rgb(0,238,238));
    insert_hash(&X11_colors, wastrsave(&strings, "cyan3"),
                             mk_rgb(0,205,205));
    insert_hash(&X11_colors, wastrsave(&strings, "cyan4"),
                             mk_rgb(0,139,139));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkSlateGray1"),
                             mk_rgb(151,255,255));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkSlateGray2"),
                             mk_rgb(141,238,238));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkSlateGray3"),
                             mk_rgb(121,205,205));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkSlateGray4"),
                             mk_rgb(82,139,139));
    insert_hash(&X11_colors, wastrsave(&strings, "aquamarine1"),
                             mk_rgb(127,255,212));
    insert_hash(&X11_colors, wastrsave(&strings, "aquamarine2"),
                             mk_rgb(118,238,198));
    insert_hash(&X11_colors, wastrsave(&strings, "aquamarine3"),
                             mk_rgb(102,205,170));
    insert_hash(&X11_colors, wastrsave(&strings, "aquamarine4"),
                             mk_rgb(69,139,116));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkSeaGreen1"),
                             mk_rgb(193,255,193));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkSeaGreen2"),
                             mk_rgb(180,238,180));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkSeaGreen3"),
                             mk_rgb(155,205,155));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkSeaGreen4"),
                             mk_rgb(105,139,105));
    insert_hash(&X11_colors, wastrsave(&strings, "SeaGreen1"),
                             mk_rgb(84,255,159));
    insert_hash(&X11_colors, wastrsave(&strings, "SeaGreen2"),
                             mk_rgb(78,238,148));
    insert_hash(&X11_colors, wastrsave(&strings, "SeaGreen3"),
                             mk_rgb(67,205,128));
    insert_hash(&X11_colors, wastrsave(&strings, "SeaGreen4"),
                             mk_rgb(46,139,87));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleGreen1"),
                             mk_rgb(154,255,154));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleGreen2"),
                             mk_rgb(144,238,144));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleGreen3"),
                             mk_rgb(124,205,124));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleGreen4"),
                             mk_rgb(84,139,84));
    insert_hash(&X11_colors, wastrsave(&strings, "SpringGreen1"),
                             mk_rgb(0,255,127));
    insert_hash(&X11_colors, wastrsave(&strings, "SpringGreen2"),
                             mk_rgb(0,238,118));
    insert_hash(&X11_colors, wastrsave(&strings, "SpringGreen3"),
                             mk_rgb(0,205,102));
    insert_hash(&X11_colors, wastrsave(&strings, "SpringGreen4"),
                             mk_rgb(0,139,69));
    insert_hash(&X11_colors, wastrsave(&strings, "green1"),
                             mk_rgb(0,255,0));
    insert_hash(&X11_colors, wastrsave(&strings, "green2"),
                             mk_rgb(0,238,0));
    insert_hash(&X11_colors, wastrsave(&strings, "green3"),
                             mk_rgb(0,205,0));
    insert_hash(&X11_colors, wastrsave(&strings, "green4"),
                             mk_rgb(0,139,0));
    insert_hash(&X11_colors, wastrsave(&strings, "chartreuse1"),
                             mk_rgb(127,255,0));
    insert_hash(&X11_colors, wastrsave(&strings, "chartreuse2"),
                             mk_rgb(118,238,0));
    insert_hash(&X11_colors, wastrsave(&strings, "chartreuse3"),
                             mk_rgb(102,205,0));
    insert_hash(&X11_colors, wastrsave(&strings, "chartreuse4"),
                             mk_rgb(69,139,0));
    insert_hash(&X11_colors, wastrsave(&strings, "OliveDrab1"),
                             mk_rgb(192,255,62));
    insert_hash(&X11_colors, wastrsave(&strings, "OliveDrab2"),
                             mk_rgb(179,238,58));
    insert_hash(&X11_colors, wastrsave(&strings, "OliveDrab3"),
                             mk_rgb(154,205,50));
    insert_hash(&X11_colors, wastrsave(&strings, "OliveDrab4"),
                             mk_rgb(105,139,34));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkOliveGreen1"),
                             mk_rgb(202,255,112));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkOliveGreen2"),
                             mk_rgb(188,238,104));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkOliveGreen3"),
                             mk_rgb(162,205,90));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkOliveGreen4"),
                             mk_rgb(110,139,61));
    insert_hash(&X11_colors, wastrsave(&strings, "khaki1"),
                             mk_rgb(255,246,143));
    insert_hash(&X11_colors, wastrsave(&strings, "khaki2"),
                             mk_rgb(238,230,133));
    insert_hash(&X11_colors, wastrsave(&strings, "khaki3"),
                             mk_rgb(205,198,115));
    insert_hash(&X11_colors, wastrsave(&strings, "khaki4"),
                             mk_rgb(139,134,78));
    insert_hash(&X11_colors, wastrsave(&strings, "LightGoldenrod1"),
                             mk_rgb(255,236,139));
    insert_hash(&X11_colors, wastrsave(&strings, "LightGoldenrod2"),
                             mk_rgb(238,220,130));
    insert_hash(&X11_colors, wastrsave(&strings, "LightGoldenrod3"),
                             mk_rgb(205,190,112));
    insert_hash(&X11_colors, wastrsave(&strings, "LightGoldenrod4"),
                             mk_rgb(139,129,76));
    insert_hash(&X11_colors, wastrsave(&strings, "LightYellow1"),
                             mk_rgb(255,255,224));
    insert_hash(&X11_colors, wastrsave(&strings, "LightYellow2"),
                             mk_rgb(238,238,209));
    insert_hash(&X11_colors, wastrsave(&strings, "LightYellow3"),
                             mk_rgb(205,205,180));
    insert_hash(&X11_colors, wastrsave(&strings, "LightYellow4"),
                             mk_rgb(139,139,122));
    insert_hash(&X11_colors, wastrsave(&strings, "yellow1"),
                             mk_rgb(255,255,0));
    insert_hash(&X11_colors, wastrsave(&strings, "yellow2"),
                             mk_rgb(238,238,0));
    insert_hash(&X11_colors, wastrsave(&strings, "yellow3"),
                             mk_rgb(205,205,0));
    insert_hash(&X11_colors, wastrsave(&strings, "yellow4"),
                             mk_rgb(139,139,0));
    insert_hash(&X11_colors, wastrsave(&strings, "gold1"),
                             mk_rgb(255,215,0));
    insert_hash(&X11_colors, wastrsave(&strings, "gold2"),
                             mk_rgb(238,201,0));
    insert_hash(&X11_colors, wastrsave(&strings, "gold3"),
                             mk_rgb(205,173,0));
    insert_hash(&X11_colors, wastrsave(&strings, "gold4"),
                             mk_rgb(139,117,0));
    insert_hash(&X11_colors, wastrsave(&strings, "goldenrod1"),
                             mk_rgb(255,193,37));
    insert_hash(&X11_colors, wastrsave(&strings, "goldenrod2"),
                             mk_rgb(238,180,34));
    insert_hash(&X11_colors, wastrsave(&strings, "goldenrod3"),
                             mk_rgb(205,155,29));
    insert_hash(&X11_colors, wastrsave(&strings, "goldenrod4"),
                             mk_rgb(139,105,20));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkGoldenrod1"),
                             mk_rgb(255,185,15));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkGoldenrod2"),
                             mk_rgb(238,173,14));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkGoldenrod3"),
                             mk_rgb(205,149,12));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkGoldenrod4"),
                             mk_rgb(139,101,8));
    insert_hash(&X11_colors, wastrsave(&strings, "RosyBrown1"),
                             mk_rgb(255,193,193));
    insert_hash(&X11_colors, wastrsave(&strings, "RosyBrown2"),
                             mk_rgb(238,180,180));
    insert_hash(&X11_colors, wastrsave(&strings, "RosyBrown3"),
                             mk_rgb(205,155,155));
    insert_hash(&X11_colors, wastrsave(&strings, "RosyBrown4"),
                             mk_rgb(139,105,105));
    insert_hash(&X11_colors, wastrsave(&strings, "IndianRed1"),
                             mk_rgb(255,106,106));
    insert_hash(&X11_colors, wastrsave(&strings, "IndianRed2"),
                             mk_rgb(238,99,99));
    insert_hash(&X11_colors, wastrsave(&strings, "IndianRed3"),
                             mk_rgb(205,85,85));
    insert_hash(&X11_colors, wastrsave(&strings, "IndianRed4"),
                             mk_rgb(139,58,58));
    insert_hash(&X11_colors, wastrsave(&strings, "sienna1"),
                             mk_rgb(255,130,71));
    insert_hash(&X11_colors, wastrsave(&strings, "sienna2"),
                             mk_rgb(238,121,66));
    insert_hash(&X11_colors, wastrsave(&strings, "sienna3"),
                             mk_rgb(205,104,57));
    insert_hash(&X11_colors, wastrsave(&strings, "sienna4"),
                             mk_rgb(139,71,38));
    insert_hash(&X11_colors, wastrsave(&strings, "burlywood1"),
                             mk_rgb(255,211,155));
    insert_hash(&X11_colors, wastrsave(&strings, "burlywood2"),
                             mk_rgb(238,197,145));
    insert_hash(&X11_colors, wastrsave(&strings, "burlywood3"),
                             mk_rgb(205,170,125));
    insert_hash(&X11_colors, wastrsave(&strings, "burlywood4"),
                             mk_rgb(139,115,85));
    insert_hash(&X11_colors, wastrsave(&strings, "wheat1"),
                             mk_rgb(255,231,186));
    insert_hash(&X11_colors, wastrsave(&strings, "wheat2"),
                             mk_rgb(238,216,174));
    insert_hash(&X11_colors, wastrsave(&strings, "wheat3"),
                             mk_rgb(205,186,150));
    insert_hash(&X11_colors, wastrsave(&strings, "wheat4"),
                             mk_rgb(139,126,102));
    insert_hash(&X11_colors, wastrsave(&strings, "tan1"),
                             mk_rgb(255,165,79));
    insert_hash(&X11_colors, wastrsave(&strings, "tan2"),
                             mk_rgb(238,154,73));
    insert_hash(&X11_colors, wastrsave(&strings, "tan3"),
                             mk_rgb(205,133,63));
    insert_hash(&X11_colors, wastrsave(&strings, "tan4"),
                             mk_rgb(139,90,43));
    insert_hash(&X11_colors, wastrsave(&strings, "chocolate1"),
                             mk_rgb(255,127,36));
    insert_hash(&X11_colors, wastrsave(&strings, "chocolate2"),
                             mk_rgb(238,118,33));
    insert_hash(&X11_colors, wastrsave(&strings, "chocolate3"),
                             mk_rgb(205,102,29));
    insert_hash(&X11_colors, wastrsave(&strings, "chocolate4"),
                             mk_rgb(139,69,19));
    insert_hash(&X11_colors, wastrsave(&strings, "firebrick1"),
                             mk_rgb(255,48,48));
    insert_hash(&X11_colors, wastrsave(&strings, "firebrick2"),
                             mk_rgb(238,44,44));
    insert_hash(&X11_colors, wastrsave(&strings, "firebrick3"),
                             mk_rgb(205,38,38));
    insert_hash(&X11_colors, wastrsave(&strings, "firebrick4"),
                             mk_rgb(139,26,26));
    insert_hash(&X11_colors, wastrsave(&strings, "brown1"),
                             mk_rgb(255,64,64));
    insert_hash(&X11_colors, wastrsave(&strings, "brown2"),
                             mk_rgb(238,59,59));
    insert_hash(&X11_colors, wastrsave(&strings, "brown3"),
                             mk_rgb(205,51,51));
    insert_hash(&X11_colors, wastrsave(&strings, "brown4"),
                             mk_rgb(139,35,35));
    insert_hash(&X11_colors, wastrsave(&strings, "salmon1"),
                             mk_rgb(255,140,105));
    insert_hash(&X11_colors, wastrsave(&strings, "salmon2"),
                             mk_rgb(238,130,98));
    insert_hash(&X11_colors, wastrsave(&strings, "salmon3"),
                             mk_rgb(205,112,84));
    insert_hash(&X11_colors, wastrsave(&strings, "salmon4"),
                             mk_rgb(139,76,57));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSalmon1"),
                             mk_rgb(255,160,122));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSalmon2"),
                             mk_rgb(238,149,114));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSalmon3"),
                             mk_rgb(205,129,98));
    insert_hash(&X11_colors, wastrsave(&strings, "LightSalmon4"),
                             mk_rgb(139,87,66));
    insert_hash(&X11_colors, wastrsave(&strings, "orange1"),
                             mk_rgb(255,165,0));
    insert_hash(&X11_colors, wastrsave(&strings, "orange2"),
                             mk_rgb(238,154,0));
    insert_hash(&X11_colors, wastrsave(&strings, "orange3"),
                             mk_rgb(205,133,0));
    insert_hash(&X11_colors, wastrsave(&strings, "orange4"),
                             mk_rgb(139,90,0));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkOrange1"),
                             mk_rgb(255,127,0));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkOrange2"),
                             mk_rgb(238,118,0));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkOrange3"),
                             mk_rgb(205,102,0));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkOrange4"),
                             mk_rgb(139,69,0));
    insert_hash(&X11_colors, wastrsave(&strings, "coral1"),
                             mk_rgb(255,114,86));
    insert_hash(&X11_colors, wastrsave(&strings, "coral2"),
                             mk_rgb(238,106,80));
    insert_hash(&X11_colors, wastrsave(&strings, "coral3"),
                             mk_rgb(205,91,69));
    insert_hash(&X11_colors, wastrsave(&strings, "coral4"),
                             mk_rgb(139,62,47));
    insert_hash(&X11_colors, wastrsave(&strings, "tomato1"),
                             mk_rgb(255,99,71));
    insert_hash(&X11_colors, wastrsave(&strings, "tomato2"),
                             mk_rgb(238,92,66));
    insert_hash(&X11_colors, wastrsave(&strings, "tomato3"),
                             mk_rgb(205,79,57));
    insert_hash(&X11_colors, wastrsave(&strings, "tomato4"),
                             mk_rgb(139,54,38));
    insert_hash(&X11_colors, wastrsave(&strings, "OrangeRed1"),
                             mk_rgb(255,69,0));
    insert_hash(&X11_colors, wastrsave(&strings, "OrangeRed2"),
                             mk_rgb(238,64,0));
    insert_hash(&X11_colors, wastrsave(&strings, "OrangeRed3"),
                             mk_rgb(205,55,0));
    insert_hash(&X11_colors, wastrsave(&strings, "OrangeRed4"),
                             mk_rgb(139,37,0));
    insert_hash(&X11_colors, wastrsave(&strings, "red1"),
                             mk_rgb(255,0,0));
    insert_hash(&X11_colors, wastrsave(&strings, "red2"),
                             mk_rgb(238,0,0));
    insert_hash(&X11_colors, wastrsave(&strings, "red3"),
                             mk_rgb(205,0,0));
    insert_hash(&X11_colors, wastrsave(&strings, "red4"),
                             mk_rgb(139,0,0));
    insert_hash(&X11_colors, wastrsave(&strings, "DebianRed"),
                             mk_rgb(215,7,81));
    insert_hash(&X11_colors, wastrsave(&strings, "DeepPink1"),
                             mk_rgb(255,20,147));
    insert_hash(&X11_colors, wastrsave(&strings, "DeepPink2"),
                             mk_rgb(238,18,137));
    insert_hash(&X11_colors, wastrsave(&strings, "DeepPink3"),
                             mk_rgb(205,16,118));
    insert_hash(&X11_colors, wastrsave(&strings, "DeepPink4"),
                             mk_rgb(139,10,80));
    insert_hash(&X11_colors, wastrsave(&strings, "HotPink1"),
                             mk_rgb(255,110,180));
    insert_hash(&X11_colors, wastrsave(&strings, "HotPink2"),
                             mk_rgb(238,106,167));
    insert_hash(&X11_colors, wastrsave(&strings, "HotPink3"),
                             mk_rgb(205,96,144));
    insert_hash(&X11_colors, wastrsave(&strings, "HotPink4"),
                             mk_rgb(139,58,98));
    insert_hash(&X11_colors, wastrsave(&strings, "pink1"),
                             mk_rgb(255,181,197));
    insert_hash(&X11_colors, wastrsave(&strings, "pink2"),
                             mk_rgb(238,169,184));
    insert_hash(&X11_colors, wastrsave(&strings, "pink3"),
                             mk_rgb(205,145,158));
    insert_hash(&X11_colors, wastrsave(&strings, "pink4"),
                             mk_rgb(139,99,108));
    insert_hash(&X11_colors, wastrsave(&strings, "LightPink1"),
                             mk_rgb(255,174,185));
    insert_hash(&X11_colors, wastrsave(&strings, "LightPink2"),
                             mk_rgb(238,162,173));
    insert_hash(&X11_colors, wastrsave(&strings, "LightPink3"),
                             mk_rgb(205,140,149));
    insert_hash(&X11_colors, wastrsave(&strings, "LightPink4"),
                             mk_rgb(139,95,101));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleVioletRed1"),
                             mk_rgb(255,130,171));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleVioletRed2"),
                             mk_rgb(238,121,159));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleVioletRed3"),
                             mk_rgb(205,104,137));
    insert_hash(&X11_colors, wastrsave(&strings, "PaleVioletRed4"),
                             mk_rgb(139,71,93));
    insert_hash(&X11_colors, wastrsave(&strings, "maroon1"),
                             mk_rgb(255,52,179));
    insert_hash(&X11_colors, wastrsave(&strings, "maroon2"),
                             mk_rgb(238,48,167));
    insert_hash(&X11_colors, wastrsave(&strings, "maroon3"),
                             mk_rgb(205,41,144));
    insert_hash(&X11_colors, wastrsave(&strings, "maroon4"),
                             mk_rgb(139,28,98));
    insert_hash(&X11_colors, wastrsave(&strings, "VioletRed1"),
                             mk_rgb(255,62,150));
    insert_hash(&X11_colors, wastrsave(&strings, "VioletRed2"),
                             mk_rgb(238,58,140));
    insert_hash(&X11_colors, wastrsave(&strings, "VioletRed3"),
                             mk_rgb(205,50,120));
    insert_hash(&X11_colors, wastrsave(&strings, "VioletRed4"),
                             mk_rgb(139,34,82));
    insert_hash(&X11_colors, wastrsave(&strings, "magenta1"),
                             mk_rgb(255,0,255));
    insert_hash(&X11_colors, wastrsave(&strings, "magenta2"),
                             mk_rgb(238,0,238));
    insert_hash(&X11_colors, wastrsave(&strings, "magenta3"),
                             mk_rgb(205,0,205));
    insert_hash(&X11_colors, wastrsave(&strings, "magenta4"),
                             mk_rgb(139,0,139));
    insert_hash(&X11_colors, wastrsave(&strings, "orchid1"),
                             mk_rgb(255,131,250));
    insert_hash(&X11_colors, wastrsave(&strings, "orchid2"),
                             mk_rgb(238,122,233));
    insert_hash(&X11_colors, wastrsave(&strings, "orchid3"),
                             mk_rgb(205,105,201));
    insert_hash(&X11_colors, wastrsave(&strings, "orchid4"),
                             mk_rgb(139,71,137));
    insert_hash(&X11_colors, wastrsave(&strings, "plum1"),
                             mk_rgb(255,187,255));
    insert_hash(&X11_colors, wastrsave(&strings, "plum2"),
                             mk_rgb(238,174,238));
    insert_hash(&X11_colors, wastrsave(&strings, "plum3"),
                             mk_rgb(205,150,205));
    insert_hash(&X11_colors, wastrsave(&strings, "plum4"),
                             mk_rgb(139,102,139));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumOrchid1"),
                             mk_rgb(224,102,255));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumOrchid2"),
                             mk_rgb(209,95,238));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumOrchid3"),
                             mk_rgb(180,82,205));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumOrchid4"),
                             mk_rgb(122,55,139));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkOrchid1"),
                             mk_rgb(191,62,255));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkOrchid2"),
                             mk_rgb(178,58,238));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkOrchid3"),
                             mk_rgb(154,50,205));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkOrchid4"),
                             mk_rgb(104,34,139));
    insert_hash(&X11_colors, wastrsave(&strings, "purple1"),
                             mk_rgb(155,48,255));
    insert_hash(&X11_colors, wastrsave(&strings, "purple2"),
                             mk_rgb(145,44,238));
    insert_hash(&X11_colors, wastrsave(&strings, "purple3"),
                             mk_rgb(125,38,205));
    insert_hash(&X11_colors, wastrsave(&strings, "purple4"),
                             mk_rgb(85,26,139));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumPurple1"),
                             mk_rgb(171,130,255));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumPurple2"),
                             mk_rgb(159,121,238));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumPurple3"),
                             mk_rgb(137,104,205));
    insert_hash(&X11_colors, wastrsave(&strings, "MediumPurple4"),
                             mk_rgb(93,71,139));
    insert_hash(&X11_colors, wastrsave(&strings, "thistle1"),
                             mk_rgb(255,225,255));
    insert_hash(&X11_colors, wastrsave(&strings, "thistle2"),
                             mk_rgb(238,210,238));
    insert_hash(&X11_colors, wastrsave(&strings, "thistle3"),
                             mk_rgb(205,181,205));
    insert_hash(&X11_colors, wastrsave(&strings, "thistle4"),
                             mk_rgb(139,123,139));
    insert_hash(&X11_colors, wastrsave(&strings, "gray0"),
                             mk_rgb(0,0,0));
    insert_hash(&X11_colors, wastrsave(&strings, "grey0"),
                             mk_rgb(0,0,0));
    insert_hash(&X11_colors, wastrsave(&strings, "gray1"),
                             mk_rgb(3,3,3));
    insert_hash(&X11_colors, wastrsave(&strings, "grey1"),
                             mk_rgb(3,3,3));
    insert_hash(&X11_colors, wastrsave(&strings, "gray2"),
                             mk_rgb(5,5,5));
    insert_hash(&X11_colors, wastrsave(&strings, "grey2"),
                             mk_rgb(5,5,5));
    insert_hash(&X11_colors, wastrsave(&strings, "gray3"),
                             mk_rgb(8,8,8));
    insert_hash(&X11_colors, wastrsave(&strings, "grey3"),
                             mk_rgb(8,8,8));
    insert_hash(&X11_colors, wastrsave(&strings, "gray4"),
                             mk_rgb(10,10,10));
    insert_hash(&X11_colors, wastrsave(&strings, "grey4"),
                             mk_rgb(10,10,10));
    insert_hash(&X11_colors, wastrsave(&strings, "gray5"),
                             mk_rgb(13,13,13));
    insert_hash(&X11_colors, wastrsave(&strings, "grey5"),
                             mk_rgb(13,13,13));
    insert_hash(&X11_colors, wastrsave(&strings, "gray6"),
                             mk_rgb(15,15,15));
    insert_hash(&X11_colors, wastrsave(&strings, "grey6"),
                             mk_rgb(15,15,15));
    insert_hash(&X11_colors, wastrsave(&strings, "gray7"),
                             mk_rgb(18,18,18));
    insert_hash(&X11_colors, wastrsave(&strings, "grey7"),
                             mk_rgb(18,18,18));
    insert_hash(&X11_colors, wastrsave(&strings, "gray8"),
                             mk_rgb(20,20,20));
    insert_hash(&X11_colors, wastrsave(&strings, "grey8"),
                             mk_rgb(20,20,20));
    insert_hash(&X11_colors, wastrsave(&strings, "gray9"),
                             mk_rgb(23,23,23));
    insert_hash(&X11_colors, wastrsave(&strings, "grey9"),
                             mk_rgb(23,23,23));
    insert_hash(&X11_colors, wastrsave(&strings, "gray10"),
                             mk_rgb(26,26,26));
    insert_hash(&X11_colors, wastrsave(&strings, "grey10"),
                             mk_rgb(26,26,26));
    insert_hash(&X11_colors, wastrsave(&strings, "gray11"),
                             mk_rgb(28,28,28));
    insert_hash(&X11_colors, wastrsave(&strings, "grey11"),
                             mk_rgb(28,28,28));
    insert_hash(&X11_colors, wastrsave(&strings, "gray12"),
                             mk_rgb(31,31,31));
    insert_hash(&X11_colors, wastrsave(&strings, "grey12"),
                             mk_rgb(31,31,31));
    insert_hash(&X11_colors, wastrsave(&strings, "gray13"),
                             mk_rgb(33,33,33));
    insert_hash(&X11_colors, wastrsave(&strings, "grey13"),
                             mk_rgb(33,33,33));
    insert_hash(&X11_colors, wastrsave(&strings, "gray14"),
                             mk_rgb(36,36,36));
    insert_hash(&X11_colors, wastrsave(&strings, "grey14"),
                             mk_rgb(36,36,36));
    insert_hash(&X11_colors, wastrsave(&strings, "gray15"),
                             mk_rgb(38,38,38));
    insert_hash(&X11_colors, wastrsave(&strings, "grey15"),
                             mk_rgb(38,38,38));
    insert_hash(&X11_colors, wastrsave(&strings, "gray16"),
                             mk_rgb(41,41,41));
    insert_hash(&X11_colors, wastrsave(&strings, "grey16"),
                             mk_rgb(41,41,41));
    insert_hash(&X11_colors, wastrsave(&strings, "gray17"),
                             mk_rgb(43,43,43));
    insert_hash(&X11_colors, wastrsave(&strings, "grey17"),
                             mk_rgb(43,43,43));
    insert_hash(&X11_colors, wastrsave(&strings, "gray18"),
                             mk_rgb(46,46,46));
    insert_hash(&X11_colors, wastrsave(&strings, "grey18"),
                             mk_rgb(46,46,46));
    insert_hash(&X11_colors, wastrsave(&strings, "gray19"),
                             mk_rgb(48,48,48));
    insert_hash(&X11_colors, wastrsave(&strings, "grey19"),
                             mk_rgb(48,48,48));
    insert_hash(&X11_colors, wastrsave(&strings, "gray20"),
                             mk_rgb(51,51,51));
    insert_hash(&X11_colors, wastrsave(&strings, "grey20"),
                             mk_rgb(51,51,51));
    insert_hash(&X11_colors, wastrsave(&strings, "gray21"),
                             mk_rgb(54,54,54));
    insert_hash(&X11_colors, wastrsave(&strings, "grey21"),
                             mk_rgb(54,54,54));
    insert_hash(&X11_colors, wastrsave(&strings, "gray22"),
                             mk_rgb(56,56,56));
    insert_hash(&X11_colors, wastrsave(&strings, "grey22"),
                             mk_rgb(56,56,56));
    insert_hash(&X11_colors, wastrsave(&strings, "gray23"),
                             mk_rgb(59,59,59));
    insert_hash(&X11_colors, wastrsave(&strings, "grey23"),
                             mk_rgb(59,59,59));
    insert_hash(&X11_colors, wastrsave(&strings, "gray24"),
                             mk_rgb(61,61,61));
    insert_hash(&X11_colors, wastrsave(&strings, "grey24"),
                             mk_rgb(61,61,61));
    insert_hash(&X11_colors, wastrsave(&strings, "gray25"),
                             mk_rgb(64,64,64));
    insert_hash(&X11_colors, wastrsave(&strings, "grey25"),
                             mk_rgb(64,64,64));
    insert_hash(&X11_colors, wastrsave(&strings, "gray26"),
                             mk_rgb(66,66,66));
    insert_hash(&X11_colors, wastrsave(&strings, "grey26"),
                             mk_rgb(66,66,66));
    insert_hash(&X11_colors, wastrsave(&strings, "gray27"),
                             mk_rgb(69,69,69));
    insert_hash(&X11_colors, wastrsave(&strings, "grey27"),
                             mk_rgb(69,69,69));
    insert_hash(&X11_colors, wastrsave(&strings, "gray28"),
                             mk_rgb(71,71,71));
    insert_hash(&X11_colors, wastrsave(&strings, "grey28"),
                             mk_rgb(71,71,71));
    insert_hash(&X11_colors, wastrsave(&strings, "gray29"),
                             mk_rgb(74,74,74));
    insert_hash(&X11_colors, wastrsave(&strings, "grey29"),
                             mk_rgb(74,74,74));
    insert_hash(&X11_colors, wastrsave(&strings, "gray30"),
                             mk_rgb(77,77,77));
    insert_hash(&X11_colors, wastrsave(&strings, "grey30"),
                             mk_rgb(77,77,77));
    insert_hash(&X11_colors, wastrsave(&strings, "gray31"),
                             mk_rgb(79,79,79));
    insert_hash(&X11_colors, wastrsave(&strings, "grey31"),
                             mk_rgb(79,79,79));
    insert_hash(&X11_colors, wastrsave(&strings, "gray32"),
                             mk_rgb(82,82,82));
    insert_hash(&X11_colors, wastrsave(&strings, "grey32"),
                             mk_rgb(82,82,82));
    insert_hash(&X11_colors, wastrsave(&strings, "gray33"),
                             mk_rgb(84,84,84));
    insert_hash(&X11_colors, wastrsave(&strings, "grey33"),
                             mk_rgb(84,84,84));
    insert_hash(&X11_colors, wastrsave(&strings, "gray34"),
                             mk_rgb(87,87,87));
    insert_hash(&X11_colors, wastrsave(&strings, "grey34"),
                             mk_rgb(87,87,87));
    insert_hash(&X11_colors, wastrsave(&strings, "gray35"),
                             mk_rgb(89,89,89));
    insert_hash(&X11_colors, wastrsave(&strings, "grey35"),
                             mk_rgb(89,89,89));
    insert_hash(&X11_colors, wastrsave(&strings, "gray36"),
                             mk_rgb(92,92,92));
    insert_hash(&X11_colors, wastrsave(&strings, "grey36"),
                             mk_rgb(92,92,92));
    insert_hash(&X11_colors, wastrsave(&strings, "gray37"),
                             mk_rgb(94,94,94));
    insert_hash(&X11_colors, wastrsave(&strings, "grey37"),
                             mk_rgb(94,94,94));
    insert_hash(&X11_colors, wastrsave(&strings, "gray38"),
                             mk_rgb(97,97,97));
    insert_hash(&X11_colors, wastrsave(&strings, "grey38"),
                             mk_rgb(97,97,97));
    insert_hash(&X11_colors, wastrsave(&strings, "gray39"),
                             mk_rgb(99,99,99));
    insert_hash(&X11_colors, wastrsave(&strings, "grey39"),
                             mk_rgb(99,99,99));
    insert_hash(&X11_colors, wastrsave(&strings, "gray40"),
                             mk_rgb(102,102,102));
    insert_hash(&X11_colors, wastrsave(&strings, "grey40"),
                             mk_rgb(102,102,102));
    insert_hash(&X11_colors, wastrsave(&strings, "gray41"),
                             mk_rgb(105,105,105));
    insert_hash(&X11_colors, wastrsave(&strings, "grey41"),
                             mk_rgb(105,105,105));
    insert_hash(&X11_colors, wastrsave(&strings, "gray42"),
                             mk_rgb(107,107,107));
    insert_hash(&X11_colors, wastrsave(&strings, "grey42"),
                             mk_rgb(107,107,107));
    insert_hash(&X11_colors, wastrsave(&strings, "gray43"),
                             mk_rgb(110,110,110));
    insert_hash(&X11_colors, wastrsave(&strings, "grey43"),
                             mk_rgb(110,110,110));
    insert_hash(&X11_colors, wastrsave(&strings, "gray44"),
                             mk_rgb(112,112,112));
    insert_hash(&X11_colors, wastrsave(&strings, "grey44"),
                             mk_rgb(112,112,112));
    insert_hash(&X11_colors, wastrsave(&strings, "gray45"),
                             mk_rgb(115,115,115));
    insert_hash(&X11_colors, wastrsave(&strings, "grey45"),
                             mk_rgb(115,115,115));
    insert_hash(&X11_colors, wastrsave(&strings, "gray46"),
                             mk_rgb(117,117,117));
    insert_hash(&X11_colors, wastrsave(&strings, "grey46"),
                             mk_rgb(117,117,117));
    insert_hash(&X11_colors, wastrsave(&strings, "gray47"),
                             mk_rgb(120,120,120));
    insert_hash(&X11_colors, wastrsave(&strings, "grey47"),
                             mk_rgb(120,120,120));
    insert_hash(&X11_colors, wastrsave(&strings, "gray48"),
                             mk_rgb(122,122,122));
    insert_hash(&X11_colors, wastrsave(&strings, "grey48"),
                             mk_rgb(122,122,122));
    insert_hash(&X11_colors, wastrsave(&strings, "gray49"),
                             mk_rgb(125,125,125));
    insert_hash(&X11_colors, wastrsave(&strings, "grey49"),
                             mk_rgb(125,125,125));
    insert_hash(&X11_colors, wastrsave(&strings, "gray50"),
                             mk_rgb(127,127,127));
    insert_hash(&X11_colors, wastrsave(&strings, "grey50"),
                             mk_rgb(127,127,127));
    insert_hash(&X11_colors, wastrsave(&strings, "gray51"),
                             mk_rgb(130,130,130));
    insert_hash(&X11_colors, wastrsave(&strings, "grey51"),
                             mk_rgb(130,130,130));
    insert_hash(&X11_colors, wastrsave(&strings, "gray52"),
                             mk_rgb(133,133,133));
    insert_hash(&X11_colors, wastrsave(&strings, "grey52"),
                             mk_rgb(133,133,133));
    insert_hash(&X11_colors, wastrsave(&strings, "gray53"),
                             mk_rgb(135,135,135));
    insert_hash(&X11_colors, wastrsave(&strings, "grey53"),
                             mk_rgb(135,135,135));
    insert_hash(&X11_colors, wastrsave(&strings, "gray54"),
                             mk_rgb(138,138,138));
    insert_hash(&X11_colors, wastrsave(&strings, "grey54"),
                             mk_rgb(138,138,138));
    insert_hash(&X11_colors, wastrsave(&strings, "gray55"),
                             mk_rgb(140,140,140));
    insert_hash(&X11_colors, wastrsave(&strings, "grey55"),
                             mk_rgb(140,140,140));
    insert_hash(&X11_colors, wastrsave(&strings, "gray56"),
                             mk_rgb(143,143,143));
    insert_hash(&X11_colors, wastrsave(&strings, "grey56"),
                             mk_rgb(143,143,143));
    insert_hash(&X11_colors, wastrsave(&strings, "gray57"),
                             mk_rgb(145,145,145));
    insert_hash(&X11_colors, wastrsave(&strings, "grey57"),
                             mk_rgb(145,145,145));
    insert_hash(&X11_colors, wastrsave(&strings, "gray58"),
                             mk_rgb(148,148,148));
    insert_hash(&X11_colors, wastrsave(&strings, "grey58"),
                             mk_rgb(148,148,148));
    insert_hash(&X11_colors, wastrsave(&strings, "gray59"),
                             mk_rgb(150,150,150));
    insert_hash(&X11_colors, wastrsave(&strings, "grey59"),
                             mk_rgb(150,150,150));
    insert_hash(&X11_colors, wastrsave(&strings, "gray60"),
                             mk_rgb(153,153,153));
    insert_hash(&X11_colors, wastrsave(&strings, "grey60"),
                             mk_rgb(153,153,153));
    insert_hash(&X11_colors, wastrsave(&strings, "gray61"),
                             mk_rgb(156,156,156));
    insert_hash(&X11_colors, wastrsave(&strings, "grey61"),
                             mk_rgb(156,156,156));
    insert_hash(&X11_colors, wastrsave(&strings, "gray62"),
                             mk_rgb(158,158,158));
    insert_hash(&X11_colors, wastrsave(&strings, "grey62"),
                             mk_rgb(158,158,158));
    insert_hash(&X11_colors, wastrsave(&strings, "gray63"),
                             mk_rgb(161,161,161));
    insert_hash(&X11_colors, wastrsave(&strings, "grey63"),
                             mk_rgb(161,161,161));
    insert_hash(&X11_colors, wastrsave(&strings, "gray64"),
                             mk_rgb(163,163,163));
    insert_hash(&X11_colors, wastrsave(&strings, "grey64"),
                             mk_rgb(163,163,163));
    insert_hash(&X11_colors, wastrsave(&strings, "gray65"),
                             mk_rgb(166,166,166));
    insert_hash(&X11_colors, wastrsave(&strings, "grey65"),
                             mk_rgb(166,166,166));
    insert_hash(&X11_colors, wastrsave(&strings, "gray66"),
                             mk_rgb(168,168,168));
    insert_hash(&X11_colors, wastrsave(&strings, "grey66"),
                             mk_rgb(168,168,168));
    insert_hash(&X11_colors, wastrsave(&strings, "gray67"),
                             mk_rgb(171,171,171));
    insert_hash(&X11_colors, wastrsave(&strings, "grey67"),
                             mk_rgb(171,171,171));
    insert_hash(&X11_colors, wastrsave(&strings, "gray68"),
                             mk_rgb(173,173,173));
    insert_hash(&X11_colors, wastrsave(&strings, "grey68"),
                             mk_rgb(173,173,173));
    insert_hash(&X11_colors, wastrsave(&strings, "gray69"),
                             mk_rgb(176,176,176));
    insert_hash(&X11_colors, wastrsave(&strings, "grey69"),
                             mk_rgb(176,176,176));
    insert_hash(&X11_colors, wastrsave(&strings, "gray70"),
                             mk_rgb(179,179,179));
    insert_hash(&X11_colors, wastrsave(&strings, "grey70"),
                             mk_rgb(179,179,179));
    insert_hash(&X11_colors, wastrsave(&strings, "gray71"),
                             mk_rgb(181,181,181));
    insert_hash(&X11_colors, wastrsave(&strings, "grey71"),
                             mk_rgb(181,181,181));
    insert_hash(&X11_colors, wastrsave(&strings, "gray72"),
                             mk_rgb(184,184,184));
    insert_hash(&X11_colors, wastrsave(&strings, "grey72"),
                             mk_rgb(184,184,184));
    insert_hash(&X11_colors, wastrsave(&strings, "gray73"),
                             mk_rgb(186,186,186));
    insert_hash(&X11_colors, wastrsave(&strings, "grey73"),
                             mk_rgb(186,186,186));
    insert_hash(&X11_colors, wastrsave(&strings, "gray74"),
                             mk_rgb(189,189,189));
    insert_hash(&X11_colors, wastrsave(&strings, "grey74"),
                             mk_rgb(189,189,189));
    insert_hash(&X11_colors, wastrsave(&strings, "gray75"),
                             mk_rgb(191,191,191));
    insert_hash(&X11_colors, wastrsave(&strings, "grey75"),
                             mk_rgb(191,191,191));
    insert_hash(&X11_colors, wastrsave(&strings, "gray76"),
                             mk_rgb(194,194,194));
    insert_hash(&X11_colors, wastrsave(&strings, "grey76"),
                             mk_rgb(194,194,194));
    insert_hash(&X11_colors, wastrsave(&strings, "gray77"),
                             mk_rgb(196,196,196));
    insert_hash(&X11_colors, wastrsave(&strings, "grey77"),
                             mk_rgb(196,196,196));
    insert_hash(&X11_colors, wastrsave(&strings, "gray78"),
                             mk_rgb(199,199,199));
    insert_hash(&X11_colors, wastrsave(&strings, "grey78"),
                             mk_rgb(199,199,199));
    insert_hash(&X11_colors, wastrsave(&strings, "gray79"),
                             mk_rgb(201,201,201));
    insert_hash(&X11_colors, wastrsave(&strings, "grey79"),
                             mk_rgb(201,201,201));
    insert_hash(&X11_colors, wastrsave(&strings, "gray80"),
                             mk_rgb(204,204,204));
    insert_hash(&X11_colors, wastrsave(&strings, "grey80"),
                             mk_rgb(204,204,204));
    insert_hash(&X11_colors, wastrsave(&strings, "gray81"),
                             mk_rgb(207,207,207));
    insert_hash(&X11_colors, wastrsave(&strings, "grey81"),
                             mk_rgb(207,207,207));
    insert_hash(&X11_colors, wastrsave(&strings, "gray82"),
                             mk_rgb(209,209,209));
    insert_hash(&X11_colors, wastrsave(&strings, "grey82"),
                             mk_rgb(209,209,209));
    insert_hash(&X11_colors, wastrsave(&strings, "gray83"),
                             mk_rgb(212,212,212));
    insert_hash(&X11_colors, wastrsave(&strings, "grey83"),
                             mk_rgb(212,212,212));
    insert_hash(&X11_colors, wastrsave(&strings, "gray84"),
                             mk_rgb(214,214,214));
    insert_hash(&X11_colors, wastrsave(&strings, "grey84"),
                             mk_rgb(214,214,214));
    insert_hash(&X11_colors, wastrsave(&strings, "gray85"),
                             mk_rgb(217,217,217));
    insert_hash(&X11_colors, wastrsave(&strings, "grey85"),
                             mk_rgb(217,217,217));
    insert_hash(&X11_colors, wastrsave(&strings, "gray86"),
                             mk_rgb(219,219,219));
    insert_hash(&X11_colors, wastrsave(&strings, "grey86"),
                             mk_rgb(219,219,219));
    insert_hash(&X11_colors, wastrsave(&strings, "gray87"),
                             mk_rgb(222,222,222));
    insert_hash(&X11_colors, wastrsave(&strings, "grey87"),
                             mk_rgb(222,222,222));
    insert_hash(&X11_colors, wastrsave(&strings, "gray88"),
                             mk_rgb(224,224,224));
    insert_hash(&X11_colors, wastrsave(&strings, "grey88"),
                             mk_rgb(224,224,224));
    insert_hash(&X11_colors, wastrsave(&strings, "gray89"),
                             mk_rgb(227,227,227));
    insert_hash(&X11_colors, wastrsave(&strings, "grey89"),
                             mk_rgb(227,227,227));
    insert_hash(&X11_colors, wastrsave(&strings, "gray90"),
                             mk_rgb(229,229,229));
    insert_hash(&X11_colors, wastrsave(&strings, "grey90"),
                             mk_rgb(229,229,229));
    insert_hash(&X11_colors, wastrsave(&strings, "gray91"),
                             mk_rgb(232,232,232));
    insert_hash(&X11_colors, wastrsave(&strings, "grey91"),
                             mk_rgb(232,232,232));
    insert_hash(&X11_colors, wastrsave(&strings, "gray92"),
                             mk_rgb(235,235,235));
    insert_hash(&X11_colors, wastrsave(&strings, "grey92"),
                             mk_rgb(235,235,235));
    insert_hash(&X11_colors, wastrsave(&strings, "gray93"),
                             mk_rgb(237,237,237));
    insert_hash(&X11_colors, wastrsave(&strings, "grey93"),
                             mk_rgb(237,237,237));
    insert_hash(&X11_colors, wastrsave(&strings, "gray94"),
                             mk_rgb(240,240,240));
    insert_hash(&X11_colors, wastrsave(&strings, "grey94"),
                             mk_rgb(240,240,240));
    insert_hash(&X11_colors, wastrsave(&strings, "gray95"),
                             mk_rgb(242,242,242));
    insert_hash(&X11_colors, wastrsave(&strings, "grey95"),
                             mk_rgb(242,242,242));
    insert_hash(&X11_colors, wastrsave(&strings, "gray96"),
                             mk_rgb(245,245,245));
    insert_hash(&X11_colors, wastrsave(&strings, "grey96"),
                             mk_rgb(245,245,245));
    insert_hash(&X11_colors, wastrsave(&strings, "gray97"),
                             mk_rgb(247,247,247));
    insert_hash(&X11_colors, wastrsave(&strings, "grey97"),
                             mk_rgb(247,247,247));
    insert_hash(&X11_colors, wastrsave(&strings, "gray98"),
                             mk_rgb(250,250,250));
    insert_hash(&X11_colors, wastrsave(&strings, "grey98"),
                             mk_rgb(250,250,250));
    insert_hash(&X11_colors, wastrsave(&strings, "gray99"),
                             mk_rgb(252,252,252));
    insert_hash(&X11_colors, wastrsave(&strings, "grey99"),
                             mk_rgb(252,252,252));
    insert_hash(&X11_colors, wastrsave(&strings, "gray100"),
                             mk_rgb(255,255,255));
    insert_hash(&X11_colors, wastrsave(&strings, "grey100"),
                             mk_rgb(255,255,255));
    insert_hash(&X11_colors, wastrsave(&strings, "dark grey"),
                             mk_rgb(169,169,169));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkGrey"),
                             mk_rgb(169,169,169));
    insert_hash(&X11_colors, wastrsave(&strings, "dark gray"),
                             mk_rgb(169,169,169));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkGray"),
                             mk_rgb(169,169,169));
    insert_hash(&X11_colors, wastrsave(&strings, "dark blue"),
                             mk_rgb(0,0,139));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkBlue"),
                             mk_rgb(0,0,139));
    insert_hash(&X11_colors, wastrsave(&strings, "dark cyan"),
                             mk_rgb(0,139,139));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkCyan"),
                             mk_rgb(0,139,139));
    insert_hash(&X11_colors, wastrsave(&strings, "dark magenta"),
                             mk_rgb(139,0,139));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkMagenta"),
                             mk_rgb(139,0,139));
    insert_hash(&X11_colors, wastrsave(&strings, "dark red"),
                             mk_rgb(139,0,0));
    insert_hash(&X11_colors, wastrsave(&strings, "DarkRed"),
                             mk_rgb(139,0,0));
    insert_hash(&X11_colors, wastrsave(&strings, "light green"),
                             mk_rgb(144,238,144));
    insert_hash(&X11_colors, wastrsave(&strings, "LightGreen"),
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
    double tmp =  (Cmax == Cmin)? 0.0 :
		  (Cmax == R)? (G-B)/delta :
		  (Cmax == G)? 2.0+(B-R)/delta :
		  4.0 + (R-G)/delta;
    *hp = ((int) (round(60.0*tmp))) % 360;
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

