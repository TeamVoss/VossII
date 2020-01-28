#include "expand_cursor.h"
#include <stdlib.h>
/************************************************************************/
/*                      Global Variables                                */
/************************************************************************/

/************************************************************************/
/*                      Local Variables                                 */
/************************************************************************/
static int		current_column;
static line_ptr		cur_lineptr;

/************************************************************************/
/*                      Local Functions                                 */
/************************************************************************/
static line_ptr		new_line();
static void		move_up();
static void		move_down();
static void		move_left();
static void		move_right();
static void		print_char(char c);
static int		get_columns();

/************************************************************************/
/*                      Public Functions                                */
/************************************************************************/
void
Print_line(string s, odests fp)
{
    int max_columns;

    current_column = 0;
    max_columns = get_columns();
    cur_lineptr = new_line();
    cur_lineptr->up = NULL;
    cur_lineptr->down = NULL;

    while( *s ) {
      restart:
	if( *s == '\n' ) {
	    s++;
	    move_down();
	    current_column = 0;
	} else {
	    if( current_column > 0 && (*s == 1) ) {
	    /* Potential line break for pretty printing */
		int rem;
		char *t;

		s++;
		t = s;
		rem = max_columns - current_column - 1;
		while( rem > 0 && *t && *t != 1 && *t != '\n' ) {
		    rem--; t++;
		}
		if( rem <= 0 ) {
		    move_down();
		    current_column = 0;
		}
	    } else {
		if( *s == 27 ) {
		    if( !*(s+1) ) {
			print_char(*s);
			s++;
			break;
		    }
		    if( *(s+1) == 91 ) {
			if( !*(s+2) ) {
			    print_char(*s);
			    s++;
			    print_char(*s);
			    s++;
			    break;
			} else {
			    switch(*(s+2)) {
				case 65:
				    move_up(); s = s+3; break;
				case 66:
				    move_down(); s = s+3; break;
				case 67:
				    move_right(); s = s+3; break;
				case 68:
				    move_left(); s = s+3; break;
				default:
				    print_char(*s);
				    s++;
				    print_char(*s);
				    s++;
				    goto restart;
			    }
			}
		    } else {
			print_char(*s);
			s++;
			goto restart;
		    }
		} else {
		    print_char(*s);
		    s++;
		}
	    }
	}
    }
    while( cur_lineptr->up != NULL )
	cur_lineptr = cur_lineptr->up;
    while( cur_lineptr ) {
	line_ptr tmp;
	cur_lineptr->line[cur_lineptr->used] = '\0';
	FP(fp, "%s%s", cur_lineptr->line,
			(cur_lineptr->down != NULL)?"\n":"");
	tmp = cur_lineptr;
	cur_lineptr = cur_lineptr->down;
	free(tmp->line);
	free(tmp);
    }
    Flush(fp);
}

/************************************************************************/
/*                      Local Functions                                 */
/************************************************************************/

static line_ptr
new_line()
{
    line_ptr	ret;

    if( (ret = (line_ptr) Malloc(sizeof(line_rec))) == NULL ) {
	Eprintf("Out of memory in malloc\n");
    }
    if( (ret->line = (string) Malloc(LINE_SIZE)) == NULL ) {
	Eprintf("Out of memory in malloc\n");
    }
    ret->allocated = LINE_SIZE;
    ret->used = 0;
    ret->up = NULL;
    ret->down = NULL;
    return(ret);
}

static void
move_up()
{
    if( cur_lineptr->up == NULL ) {
	cur_lineptr->up = new_line();
	cur_lineptr->up->down = cur_lineptr;
    }
    cur_lineptr = cur_lineptr->up;
}

static void
move_down()
{
    if( cur_lineptr->down == NULL ) {
	cur_lineptr->down = new_line();
	cur_lineptr->down->up = cur_lineptr;
    }
    cur_lineptr = cur_lineptr->down;
}

static void
move_left()
{
    current_column--;
    if( current_column < 0 ) {
	FP(err_fp, "Moved off the screen to the left.\n");
	current_column = 0;
    }
}

static void
move_right()
{
    current_column++;
}

static void
print_char(char c)
{
    int 	new_size, i;

    if( c == 1 )
	return;
    if( current_column+1 >= cur_lineptr->allocated ) {
	/* Need to grow the line */
	new_size = cur_lineptr->allocated+LINE_SIZE;
	while( new_size <= current_column+1 )
	    new_size = new_size + LINE_SIZE;
	cur_lineptr->allocated = new_size;
	cur_lineptr->line = (char *) realloc(cur_lineptr->line, new_size);
    }

    for(i = cur_lineptr->used; i < current_column; i++)
	cur_lineptr->line[i] = ' ';
    cur_lineptr->line[current_column] = c;
    current_column++;
    if( current_column > cur_lineptr->used ) {
	cur_lineptr->used = current_column;
    }
}


static int
get_columns()
{
    char *r;
    int	 columns;
    r = getenv("COLUMNS");
    if( (r == NULL) || (sscanf(r, "%d", &columns) != 1) ) {
	return( 80 );
    }
    return(columns);
}

