/************************************************
*	@Author: Peter Seger			*
*		2017				*
************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "list_util.h"


int		 Find_sep_distance( list_ptr one, list_ptr two );
list_ptr	 Shift_top_line( int dist, list_ptr line );
list_ptr	 Shift_bottom_line( int dist, list_ptr line);
list_ptr	 Get_top( list_ptr line1, list_ptr line2 );
list_ptr	 Get_bottom( list_ptr line1, list_ptr line2 );
list_ptr	 Shift_width( int dist, list_ptr line );
