//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************
 *									*
 *	Original author: Carl-Johan Seger 1990 				*
 *									*
 ************************************************************************/
/* file_ops.c -- Write and read encoded files */

#include "file_ops.h"

/**** PRIVATE VARIABLES ****/
static file_rec		LL_file;
static unsigned char 	Tmp_buf[BUF_SIZE];


/**** PRIVATE FUNCTIONS ****/
static bool		file_open();
static VOID		file_close();
static VOID		file_write();
static bool		file_read();

/****************************************************************************/
/*                           Main functions                                 */
/****************************************************************************/

/* Init_file_ops -- Initialize the file operation manager */
VOID
Init_file_ops()
{
    LL_file.file_desc = -1;
}

/* Quit_file_ops -- Release the file operation manager */
VOID
Quit_file_ops()
{
    LL_file.file_desc = -1;
}

/* LL_Open -- (low-level) Open file filename for operation op     	   */
/*             Returns TRUE iff operation was successful          	   */
/*             Note that only one LL file can be open at any time 	   */
/*  Supported operations: r   -- read operation			  	   */
/*  			  w   -- write operation (append to existing file) */
/*  			  t   -- write operation (truncates existing file) */
bool
LL_Open(string filename, char op)
{
    string new_name;
    bool res;
    extern string RCDefault_dir;

    if( (res = file_open(filename, op, &LL_file)) )
	return(res);
    new_name = strtemp(RCDefault_dir);
    new_name = strappend("/");
    new_name = strappend(filename);
    return( file_open(new_name, op, &LL_file) );
}

/* LL_Close -- (low-level) Close the tempfile. Write out buffered writes etc. */
VOID
LL_Close()
{
    file_close(&LL_file);
}

/* LL_Write -- (low-level) Write n bytes at p to the LL file */
void
LL_Write(pointer p, unsigned int size)
{
    file_write(p, size, &LL_file);
}

/* LL_Read -- (low-level) read a line from fp and return a pointer to 	*/
/*	      this entry. Return NULL upon EOF		         	*/
pointer
LL_Read(int size)
{
    if( file_read(Tmp_buf, size, &LL_file) == TRUE )
	return Tmp_buf;
    else
	return NULL;
}

/****************************************************************************/
/*                          Local functions                                 */
/****************************************************************************/

/* file_open -- Open file filename for operation op     	   	   */
/*              Returns TRUE iff the file can be opened as described	   */
/*  Supported operations: r   -- read operation			  	   */
/*  			  w   -- write operation (append to existing file) */
/*  			  t   -- write operation (truncates existing file) */
static bool
file_open(filename, op, fp)
string   filename;
char     op;
file_ptr fp;
{
    ASSERT( fp->file_desc == -1 );
    ASSERT(op == 'w' || op == 'r' || op == 't');
    switch( op ) {
	case 'r':
	    fp->file_desc = open(filename, O_RDONLY, 0666);
	    break;
	case 't':
	    fp->file_desc = open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0666);
	    break;
	case 'w':
	    fp->file_desc = open(filename, O_WRONLY | O_CREAT | O_APPEND, 0666);
	    break;
    }
    if( fp->file_desc >= 0 ) {
	fp->file_name = filename;
	fp->op 	      = op;
	fp->buf_size  = 0;
	fp->pos	      = 0;
	if( op == 't' )
	    fp->file_size = 0;
	else
	    fp->file_size = lseek(fp->file_desc, 0L, L_XTND);
	if( op == 'r' )
	    lseek(fp->file_desc, 0L, L_SET);
	return TRUE;
    } else
	return FALSE;
}

/* file_close -- Close the file fp. Write out buffered writes etc. */
static VOID
file_close(fp)
file_ptr fp;
{
    ASSERT( fp->file_desc >= 0 );
    if( fp->op != 'r' ) {
	/* Write out the buffer */
	if( write(fp->file_desc, fp->buf, fp->pos) != fp->pos ){
	    Wprintf("Failed to write file %s.\n", fp->file_name);
	}
    }
    close( fp->file_desc );
    fp->file_desc = -1;
}

static VOID
file_write(pointer pp, unsigned int n, file_ptr fp)
{
    register unsigned char  *bp, *p;
    register int  	    used = fp->pos;

    p = (unsigned char *) pp;

    ASSERT(fp->file_desc >= 0 && fp->pos <= BUF_SIZE );
    ASSERT(fp->op == 'w' || fp->op == 't' );
    bp = fp->buf + used;
    while( n > 0 ) {
	/* Fill the buffer as many times as needed */
	while( n > 0 && used < BUF_SIZE ) {
	    *bp++ = *p++;
	    n--;
	    used++;
	}
	if( n > 0 ) {
	    /* Does not fit in buffer. Write out buffer */
	    if( write(fp->file_desc, fp->buf, BUF_SIZE) != BUF_SIZE ) {
		Wprintf("Cannot write to file %s\n", fp->file_name);
	    }
	    bp = fp->buf;
	    used = 0;
	}
    }
    fp->pos = used;
}

/* file_read -- Read n bytes from file fp into the area pointed to by p	   */
/*		Returns TRUE is successful and FALSE if EOF is encountered */
static bool
file_read(p, n, fp)
unsigned char 		*p;
register unsigned int	n;
file_ptr		fp;
{
    register unsigned char  *bp;
    register int  	    used = fp->pos;
    register int  	    buf_size = fp->buf_size;
    unint			    start_n = n;

    ASSERT(fp->file_desc >= 0 && fp->pos <= BUF_SIZE && fp->pos <=fp->buf_size);
    ASSERT(fp->op == 'r' );

    bp = fp->buf + used;
    while( n > 0 ) {
	/* Fill the buffer as many times as needed */
	while( n > 0 && used < buf_size ) {
	    *p++ = *bp++;
	    n--;
	    used++;
	}
	if( n > 0 ) {
	    /* Must read in more data */
	    buf_size = read(fp->file_desc, fp->buf, BUF_SIZE);
	    if( buf_size == 0 ) {
		/* EOF encountered */
		if( n != start_n ) {
		    Eprintf("Unexpected EOF in %s\n", fp->file_name);
		}
		return FALSE;
	    }
	    ASSERT( fp->buf_size == 0 || fp->buf_size == BUF_SIZE );
	    bp = fp->buf;
	    used = 0;
	}
    }
    fp->pos 	 = used;
    fp->buf_size = buf_size;
    return TRUE;
}
