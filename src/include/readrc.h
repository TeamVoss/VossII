/********************************************************************
*                                                                   *
*     Copyright (C) 2016 Carl-Johan Seger                           *
*                                                                   *
*********************************************************************/
/* readrc.h -- header for readrc.c */

#ifndef _READRC_H
#define _READRC_H
#include "hash.h"
#include "strmgr.h"
#include "morestr.h"

#define READRC_MAGIC_NBR	88274658


typedef enum {RC_HEADER, RC_INT, RC_BOOL, RC_STRING, RC_ENUM}  vossrc_type;

typedef struct vossrc_rec *vossrc_ptr;

typedef struct vossrc_rec {
    char        *name;
    void        *valp;
    char        *description;
    vossrc_type type;
    char        *default_values;
} vossrc_rec;


void		initRC(vossrc_ptr vp, string file, string path);
string          getRCvalue(string option);
string          updateRCvalue(string option, string new_value);

#endif /* _READRC_H */
