/************************************************
*	@Author: Peter Seger			*
*		2017				*
************************************************/

/* This document provides tools to place line modules and manipulate their posiitons.
   It includes functions to:
	-Find separation distance between lines
	-Determine which line is on top and which is on the bottom
	-Shift the top & bottom lines up and down
	-Shift the lines right and left
*/

#include "place_bounds.h"

// Get_top takes two lines and returns which line is on top of the other.
list_ptr
Get_top( list_ptr line1, list_ptr line2 )
{
    if(line1 == NULL || line2 == NULL) {
	fprintf(stderr, "Empty line given");
	return NULL;
    } else {
	if(line1->y < line2->y){
	    return line1;
	} else {
	    return line2;
	}
    }
}

// Get_bottom takes two lines and returns which line is on the bottom.
list_ptr
Get_bottom( list_ptr line1, list_ptr line2 )
{
    if(line1 == NULL || line2 == NULL) {
	fprintf(stderr, "Empty line given");
	return NULL;
    } else {
	if(line1->y > line2->y){
	    return line1;
	} else {
	    return line2;
	}
    }
}

// Shift_top_line takes a distance beteween two lines and shifts the top line by dist/2.
list_ptr
Shift_top_line( int dist, list_ptr line )
{
    if( line == NULL ) {
	fprintf(stderr, "Empty line given");
	return NULL;
    }
    list_ptr start_pnt = line;
    
    list_ptr temp = line;
    while( temp != NULL ) {
	if( temp->y < 0 ) {
	    temp->y = temp->y + (dist/2);
	} else {
	    temp->y = temp->y - (dist/2);
	}
	temp = temp->next;
    }
    return start_pnt;
}


// Shift_bottom_line takes a distance beteween two lines and shifts the bottom line by dist/2.
list_ptr
Shift_bottom_line( int dist, list_ptr line )
{
    if( line == NULL) {
	fprintf(stderr, "Empty line given");
	return NULL;
    }
    list_ptr start_pnt = line;
    
    list_ptr temp = line;
    while( temp != NULL ) {
	temp->y = temp->y - (dist/2);
	temp = temp->next;
    }
    return start_pnt;
}

// Shift_width takes a shift distance and a line and shifts every x value over by dist.
list_ptr
Shift_width( int dist, list_ptr line )
{
    if( line == NULL ) {
	fprintf(stderr, "Empty line given");
	return NULL;
    }
    list_ptr start_pnt = line;

    list_ptr temp = line;
    while( temp != NULL ) {
	if( dist < 0 ) {
	    temp->x = temp->x - dist;
	} else {
	    temp->x = temp->x + dist;
	}
	temp = temp->next;
    }
    return start_pnt;
}
