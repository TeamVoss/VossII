//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2019                        *
*                                                                   *
*********************************************************************/
/* image.h -- header for image.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* ----- Function prototypes for public functions ----- */
void	    Image_Init();
void	    Image_Install_Functions();

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef IMAGE_H
#define IMAGE_H
#include "fl.h"	/* Global data types and include files 		     */

#define READ_BUF_SIZE	4096

typedef struct color_rec    *color_ptr;
typedef struct color_rec {
    uchar	valid;
    uchar	r;
    uchar	g;
    uchar	b;
} color_rec;

typedef struct image_rec    *image_ptr;
typedef struct image_rec {
    int		mark;		// For g.c.
    image_ptr	next;		// Only used for free list
    // Real content
    string	name;
    int		rows;
    int		cols;
    buffer	cbuf;
} image_rec;

#define GET_PIXEL(im,x,y)						    \
		    (color_ptr) M_LOCATE_BUF(&(im->cbuf),(y*im->cols)+x)
#define SET_PIXEL(im,x,y,cp)						    \
		    store_buf(&(im->cbuf),(y*im->cols)+x, cp)

#endif /* IMAGE_H */
#endif /* EXPORT_FORWARD_DECL */
