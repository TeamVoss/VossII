//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************
*	Original author: Peter Seger 2017	*
************************************************/

#include "list_util.h"
#include "rec.h"

/* --Record Manager-- */
static rec_mgr list_rec_mgr;

void
Init_list()
{
    new_mgr(&list_rec_mgr, sizeof(list_rec));
}

// Add_to_front takes a list_ptr and x,y values to create a new ptr and adds it
// to the front of the old one, setting it as the head of the tree.
list_ptr
Add_to_front(int x, int y, list_ptr old_list)
{
    list_ptr res;
    res = (list_ptr) new_rec(&list_rec_mgr);
    res->x = x;
    res->y = y;
    res->next = old_list;
    return res;
}

// Add_to_end takes a list_ptr and x,y values to create a new ptr and adds it
// to the end of the old one, returning the head of the original tree.
list_ptr
Add_to_end(int x, int y, list_ptr old_list)
{
    list_ptr res;
    if( old_list == NULL ){
	res = (list_ptr) new_rec(&list_rec_mgr);
	res->x = x;
	res->y = y;
	res->next = NULL;
	return res;
    } else {
	list_ptr temp;
	temp = old_list;
	while ( temp->next != NULL ){
	    temp = temp->next;
	}
	res = (list_ptr) new_rec(&list_rec_mgr);
	res->x = x;
	res->y = y;
	res->next = NULL;
	temp->next = res;
    }
    return old_list;
}

list_ptr
Copy_list(list_ptr list)
{
    if( list == NULL ) {
	return NULL;
    }
    list_ptr new_list;
    new_list = (list_ptr) new_rec(&list_rec_mgr);
    new_list->x = list->x;
    new_list->y = list->y;
    new_list->next = Copy_list(list->next);
    return new_list;
}




/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
list_ptr
Read_pairs(char *filename)
{
    FILE *fp;
    char buf[1000];
    if( (fp = fopen(filename, "r")) == NULL ) {
	fprintf(stderr, "Cannot open file %s", filename);
	return NULL;
    }
    int line = 0;
    list_ptr result = NULL;
    while( fgets(buf, 999, fp) != NULL ) {
	int x, y;
	line++;
	if( sscanf(buf, "(%d,%d)", &x, &y) != 2 ) {
	    fprintf(stderr, "Syntax error in %s around line %d\n",
			    filename, line);
	    return NULL;
	}
	result = Add_to_front(x, y, result);
    }
    fclose(fp);
    return result;
}

void
Print_list(list_ptr list)
{
    if( list == NULL ) {
	fprintf(stderr, "Empty");
    } else {
	fprintf(stderr, "(%d,%d)", list->x, list->y);
	Print_list(list->next);
    }
}

void
Free_list(list_ptr list)
{
    while( list != NULL ) {
	list_ptr tmp = list;
	list = list->next;
	free(tmp);
    }
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
