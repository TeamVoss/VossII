/************************************************
*	@Author: Peter Seger			*
*		2017				*
************************************************/

#ifndef LIST_UTIL_H
#define LIST_UTIL_H

#include <stdio.h>
#include <stdlib.h>


typedef struct list_rec *list_ptr;


typedef struct list_rec {
    int		x;
    int		y;
    list_ptr	next;
} list_rec;


void	    Print_list(list_ptr list);
list_ptr    Add_to_front(int x, int y, list_ptr old_list);
list_ptr    Add_to_end(int x, int y, list_ptr old_list);
void	    Free_list(list_ptr list);
list_ptr    Copy_list(list_ptr list);
list_ptr    Read_pairs(char *filenname);
void	    Init_list();

#endif
