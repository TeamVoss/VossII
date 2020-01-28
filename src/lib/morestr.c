#include "morestr.h"

/************************************************************************/
/*                      Public Functions                                */
/************************************************************************/

bool
str2int(string s, int *ip)
{
    bool	negative, ok;
    int		radix, cur = 0;

    ok = FALSE;
    if( *s == '-' ) {
	negative = TRUE;
	s++;
    } else
	negative = FALSE;
    if( *s == '0' ) {
	s++;
	/* Hex or octal (or plain 0) */
	if( *s == 'x' || *s == 'X' ) {
	    radix = 16;			/* Hex */
	    s++;
	} else
	    radix = 8;			/* Octal */
    } else
	radix = 10;			/* Decimal */
    cur = 0;
    if( radix <= 10 ) {
	while( *s >= '0' && *s <= '9' ) {
	    ok = TRUE;
	    cur = cur*radix+(*s-'0');
	    s++;
	}
    } else {
	/* Hex */
	while( (*s >= '0' && *s <= '9') || (*s >= 'a' && *s <= 'h') ||
		(*s >= 'A' && *s <= 'H') ) {
	    ok = TRUE;
	    if( (*s >= '0' && *s <= '9') )
		cur = cur*radix+(*s-'0');
	    else if( (*s >= 'a' && *s <= 'h') )
		cur = cur*radix+(*s-'a');
	    else
		cur = cur*radix+(*s-'A');
	    s++;
	}
    }
    if( !ok || (*s != NULLSTR) )
	return( FALSE );
    if( negative )
	*ip = -cur;
    else
	*ip = cur;
    return( TRUE );
}

