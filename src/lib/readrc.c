//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include "readrc.h"

/************************************************************************/
/*                      Local variables                                 */
/************************************************************************/
static int		initialized = 0;
static str_mgr		rc_strings;
static char		buf[1000];
static char		keyword[80];
static char		association[1024];
static vossrc_ptr       vossrc_values = NULL;


/************************************************************************/
/*                Declaration of local functions                        */
/************************************************************************/
static bool	v_readline(FILE *fp, string key, string val);

#define STRSAVE(s)	WAstrsave(&rc_strings,(s))

extern void Rprintf(string fmt, ...);
#define ABORT(...)      { Rprintf(__VA_ARGS__); }

/************************************************************************/
/*                      Public Functions                                */
/************************************************************************/


string
getRCvalue(string name)
{
    vossrc_ptr vp = vossrc_values;
    while( vp->name != 0 ) {
        if( strcmp(vp->name, name) == 0 ) {
            switch( vp->type ) {
                case RC_HEADER:  { vp++; break; }
                case RC_INT:  {
                    int i = *((int *) vp->valp);
                    sprintf(buf, "%d", i);
                    return( STRSAVE(buf) );
                }
                case RC_BOOL: {
                    int i = *((int *) vp->valp);
                    sprintf(buf, "%s", (i == 0)? "NO" : "YES");
                    return( STRSAVE(buf) );
                }
                case RC_ENUM:
                case RC_STRING: {
                    string s = *((string *) vp->valp);
                    return( STRSAVE(s) );
                }
                default:
                    fprintf(stderr, "Should never happen\n");
                    exit(-1);
            }
        } else {
            vp++;
        }
    }
    buf[0] = 0;
    return(buf);
}

string
updateRCvalue(string name, string value)
{
    vossrc_ptr vp = vossrc_values;
    while( vp->name != 0 ) {
        if( strcmp(vp->name, name) == 0 ) {
            switch( vp->type ) {
                case RC_HEADER:  { vp++; break; }
                case RC_INT:  {
                    int old = *((int *) vp->valp);
                    sprintf(buf, "%d", old);
                    *((int *) vp->valp) = atoi(value);
                    return( STRSAVE(buf) );
                }
                case RC_BOOL: {
                    int old = *((int *) vp->valp);
                    sprintf(buf, "%s", (old == 0)? "NO" : "YES");
                    if( strcmp(value, "YES") == 0 ||
                        strcmp(value, "Yes") == 0 ||
                        strcmp(value, "yes") == 0 ||
                        strcmp(value, "TRUE") == 0 ||
                        strcmp(value, "true") == 0 ||
                        strcmp(value, "T") == 0  ) {
                        *((int *) vp->valp) = 1;
                    } else {
                        *((int *) vp->valp) = 0;
                    }
                    return( STRSAVE(buf) );
                }
                case RC_STRING: {
                    string old = *((string *) vp->valp);
                    *((string *) vp->valp) = STRSAVE(value);
                    return( old );
                }
                case RC_ENUM: {
                    string old = *((string *) vp->valp);
                    string new = STRSAVE(value);
                    strcpy(buf, vp->default_values);
                    string alt = strtok(buf, " ");
                    bool ok = FALSE;
                    while( alt != NULL ) {
                        if( strcmp(alt, new) == 0 ) {
                            ok = TRUE;
                        }
                        alt = strtok(NULL, " ");
                    }
                    if( ok ) {
                        *((string *) vp->valp) = new;
                        return( old );
                    } else {
                        fprintf(stderr, "Illegal enum value (%s) for %s\n",
                                        new, name);
                        return( old );
                    }
                }
                default:
                    fprintf(stderr, "Should never happen\n");
                    exit(-1);
            }
        } else {
            vp++;
        }
    }
    buf[0] = 0;
    return(buf);
}

void
initRC(vossrc_ptr vp, string file, string path)
{
    if( initialized != READRC_MAGIC_NBR ) {
	/* Initialize the tables etc. */
	new_strmgr(&rc_strings);
        vossrc_values = vp;
        while( vp->name != 0 ) {
            if( vp->type != RC_HEADER ) {
                if( vp->type != RC_ENUM ) {
                    if(!strcmp("VOSS-LIBRARY-DIRECTORY", vp->name)) {
                        updateRCvalue(vp->name,
				      getenv("VOSS-LIBRARY-DIRECTORY"));
                    } else if(!strcmp("VOSS-BINARY-DIRECTORY", vp->name)) {
                        updateRCvalue(vp->name,getenv("VOSS-BINARY-DIRECTORY"));
                    } else {
                        updateRCvalue(vp->name, vp->default_values);
                    }
                } else {
                    strcpy(buf, vp->default_values);
                    string alt = strtok(buf, " ");
                    updateRCvalue(vp->name, alt);
                }
	    }
            vp++;
        }
	initialized = READRC_MAGIC_NBR;
    }

    string	cur, new_name;
    FILE	*fp;

    /* Find and load options */
    if( path == NULL || file == NULL )
	return;
    if( *path == NULLSTR ) {
	if( (fp = fopen(file, "r+")) != NULL ) {
	    /* There was such a file. Read it in */
	    while( v_readline(fp, keyword, association) ) {
                updateRCvalue(keyword, association);
	    }
	    fclose(fp);
	}
	return;
    }

    cur = path;
    new_name = strtemp("");
    while( *cur != NULLSTR ) {
	if( *cur == ':' ) {
	    new_name = charappend('/');
	    new_name = strappend(file);
	    if( (fp = fopen(new_name, "r+")) != NULL ) {
		/* There was such a file. Read it in */
		while( v_readline(fp, keyword, association) ) {
                    updateRCvalue(keyword, association);
		}
		fclose(fp);
	    }
	    cur++;
	    new_name = strtemp("");
	} else if ( *cur == '$' ) {
	    string env_var;
	    ASSERT( strcmp(new_name,"") == 0 );
	    env_var = strtemp("");
	    cur++;
	    while( *cur != NULLSTR && *cur != ':' && *cur != '/' ) {
		env_var = charappend(*cur);
		cur++;
	    }
	    if( (new_name = (string) getenv(env_var)) != NULL ) {
		new_name = strtemp(new_name);
	    } else {
		new_name = strtemp("");
	    }
	} else {
	    new_name = charappend(*cur);
	    cur++;
	}
    }
    if( strcmp(new_name, "") != 0 ) {
	new_name = charappend('/');
	new_name = strappend(file);
	if( (fp = fopen(new_name, "r+")) != NULL ) {
	    /* There was such a file. Read it in */
	    while( v_readline(fp, keyword, association) ) {
                updateRCvalue(keyword, association);
	    }
	    fclose(fp);
	}
    }
}


/************************************************************************/
/*                      Local functions                                 */
/************************************************************************/

#define GET_CHAR     { c = getc(fp); if( c == EOF ) return FALSE; }

static bool
v_readline(FILE *fp, string key, string val)
{
    int         c;
    string      s;

  start:
    s = key;
    GET_CHAR;
    if( c == '#' ) {
        do { GET_CHAR } while( c != '\n' );
        goto start;
    }
    while( c == ' ' || c == '\t' ) GET_CHAR;
    if( c == '\n' ) goto start;
    while( c != ' ' && c != '\t' && c != '=' ) {
        *s++ = c;
        GET_CHAR;
    } 
    *s = 0;
    while( c != '=' ) GET_CHAR;
    GET_CHAR;
    while( c == ' ' || c == '\t' ) GET_CHAR;
    bool curly = FALSE;
    if( c == '{' ) {
        curly = TRUE;
        GET_CHAR;
    }
    s = val;
    do {
        *s++ = c;
        GET_CHAR;
    } while( c != '\n' && ( !curly || c != '}' ) );
    *s = 0;
    if( c != EOF && curly ) {
        GET_CHAR;
    }
    return(TRUE); 
}



#undef STRSAVE
