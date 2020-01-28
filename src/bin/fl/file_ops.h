/********************************************************************
*                                                                   *
*     Copyright (C) 1991 Carl-Johan Seger			    *
*                                                                   *
*********************************************************************/
/* file_ops.h -- header for file_ops.c -- Encoded file operations  */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* ----- Function prototypes for public functions ----- */
void		Init_file_ops(void);
void 		Quit_file_ops(void);
int 		LL_Open(char *, char);
void 		LL_Close(void);
void 		LL_Write(pointer p, unsigned int);
pointer         LL_Read(int);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef FILE_OPS_H
#define FILE_OPS_H
#include "fl.h"
#include <sys/file.h>
#include <sys/types.h>
#include <fcntl.h>
#ifndef LSEEK_RTN_TYPE
#define LSEEK_RTN_TYPE	long
#endif
#ifndef OPEN_RTN_TYPE
#define OPEN_RTN_TYPE	int
#endif


#define BUF_SIZE	1024

typedef struct file_rec {
	string		file_name;	/* File name    	              */
	OPEN_RTN_TYPE	file_desc;	/* File descriptor of file            */
	LSEEK_RTN_TYPE	file_size;	/* Total size of file		      */
	char		op;		/* Operation r/w/a		      */
	unsigned char	buf[BUF_SIZE];	/* Buffer to speed up I/O behavior    */
	int		buf_size;	/* Real size of buf (for reads only)  */
	int		pos;		/* Current position in buffer         */
} file_rec, *file_ptr;

#endif /* FILE_OPS_H */
#endif /* EXPORT_FORWARD_DECL */
