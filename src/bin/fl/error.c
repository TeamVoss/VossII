//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*		Original author: Carl-Johan Seger, 1990			*/
/*									*/
/************************************************************************/
#include "error.h"

/***** REFERENCED GLOBAL VARIABLES *****/
extern bool		RCadd_debug_info;
extern jmp_buf		*start_envp;
extern bool		file_load;
extern FILE		*yyin;
extern bool             gui_mode;
extern bool             use_stdout;
extern old_yyin_ptr	cur_file;
extern string		cur_file_name;
extern int		line_nbr;
extern char		FailBuf[4096];
extern FILE             *odests_fp;
extern FILE             *to_tcl_fp;
extern int		RCmax_stack_trace_entries;

/***** PRIVATE VARIABLES *****/
#define FBUF_MAX_SZ 16384
static char fbuf[FBUF_MAX_SZ+1];
static char safe_buf[2*FBUF_MAX_SZ];

/************************************************************************/
/*			GLOBAL FUNCTIONS				*/
/************************************************************************/

// A shorthand for fprintf() with additional logging possible
void
FP_va(odests fp, const string format, va_list arg)
{
    if( fp == FILE_fp ) {
        vfprintf(odests_fp, format, arg);
    } else {
	vsnprintf(fbuf, FBUF_MAX_SZ, format, arg);
        if( !gui_mode || use_stdout ) {
            switch( fp ) {
                case fl_gc_fp:
                case bdd_gc_fp:
                case stdout_fp:
                    fprintf(stdout, "%s", fbuf); break;
                case sim_fp:
                case err_fp:
                case warning_fp:
                    fprintf(stderr, "%s", fbuf); break;
                default:
                    fprintf(stderr, "%s", fbuf); break;
            }
        }
        if( gui_mode ) {
	    string safe_buf = make_tcl_safe(fbuf);
            string tmp = protect(safe_buf);
            switch( fp ) {
                case stdout_fp:
                    fprintf(to_tcl_fp, "WriteStdOut {%s}\n", tmp); break;
                case sim_fp:
                    fprintf(to_tcl_fp, "WriteSim {%s}\n", tmp); break;
                case err_fp:
                    fprintf(to_tcl_fp, "WriteStdErr {%s}\n", tmp); break;
                case warning_fp:
                    fprintf(to_tcl_fp, "WriteWarning {%s}\n", tmp); break;
                case fl_gc_fp:
                    fprintf(to_tcl_fp, "WriteFlGC {%s}\n", tmp); break;
                case bdd_gc_fp:
                    fprintf(to_tcl_fp, "WriteBddGC {%s}\n", tmp); break;
                default:
                    fprintf(to_tcl_fp, "WriteStdOut {%s}\n", tmp); break;
            }
            fflush(to_tcl_fp);
        }

    }
}

// A shorthand for fprintf() with additional logging possible
void
FP(odests fp, const string format, ...)
{
    va_list arg;
    va_start(arg, format);
    FP_va(fp, format, arg);
    va_end(arg);
}

void
Flush(odests fp)
{
    switch (fp) {
        case FILE_fp:
            fflush(odests_fp); break;
        default:
            fflush(stdout); break;
    }
}

/* Error_printf -- My own version of C Library fprintf(stderr...) + exit(-1) */
/* NOTE: Eprintf never returns (ends with an exit(-1)). */
VOID
Eprintf (const string format, ...)
{
    va_list arg;
    va_start(arg, format);
    // Make sure error message is visible on stderr since GUI will be disappear
    use_stdout = TRUE;
    FP_va(err_fp, format, arg);
    va_end(arg);
    exit(-1);
}

VOID
ErrMsg(const string format, ...)
{
    if( file_load ) {
        FP(err_fp,
	  "\n#### Error while reading file %s around line %d\n",
          cur_file_name, line_nbr);
    }
    va_list arg;
    va_start(arg, format);
    FP_va(err_fp, format, arg);
    va_end(arg);
}


/* Rprintf -- My own version of C Library fprintf(stderr...) + long_jmp */
/* Runtime error functions. Ends with a longjmp */
VOID
Rprintf (const string format, ...)
{
    if( file_load ) {
        FP(err_fp,
	  "\n#### Run-time error while reading file %s. Began around line %d\n",
          cur_file_name, line_nbr);
    } else {
        FP(err_fp, "#### Run-time error\n");
    }
    FP(err_fp, "---- ");
    va_list arg;
    va_start(arg, format);
    FP_va(err_fp, format, arg);
    va_end(arg);
    FP(err_fp, "\n");
    longjmp(*start_envp, 1);
}

/* Info_printf -- A printf function for information messages */
VOID
Info_printf (const string format, ...)
{
    FP(warning_fp, "---- ");
    va_list arg;
    va_start(arg, format);
    FP_va(warning_fp, format, arg);
    va_end(arg);
    FP(warning_fp, "\n");
}

/* Fail_pr -- My own version of C Library Sprintf(FailBuf,...) */
/* Failure message functions. */
char *
Fail_pr (const string format, ...)
{
    va_list arg;
    va_start(arg, format);
    vsnprintf(fbuf, FBUF_MAX_SZ, format, arg);
    va_end(arg);
    char        *res;
    res = strtemp("---  ");
    res = strappend(fbuf);
    res = charappend('\n');
    if( RCadd_debug_info && strstr(res, "Stack trace:\n") == 0 ) {
	strappend(Get_stack_trace(RCmax_stack_trace_entries));
    }
    strncpy(FailBuf, res, 4096-1);
    FailBuf[4096-1] = 0;
    return FailBuf;
}

/* Fail_append -- Like Fail_pr but append at the end of the FailBuf */
VOID
Fail_append(const string format, ...)
{
    va_list arg;
    va_start(arg, format);
    vsnprintf(fbuf, FBUF_MAX_SZ, format, arg);
    va_end(arg);
    string stack_tracep = strstr(FailBuf, "Stack trace:");
    string res;
    if( stack_tracep != NULL ) {
        *stack_tracep = '\0';
        res = strtemp(FailBuf);
        res = strappend(fbuf);
        *stack_tracep = 'S';
        res = strappend(stack_tracep);
    } else {
        res = strtemp(FailBuf);
        res = strappend(fbuf);
    }
    strncpy(FailBuf, res, 4096-1);
    FailBuf[4096-1] = 0;
}

/* A printf function into strtemp() */
string
tprintf (const string format, ...)
{
    string ret;
    va_list arg;
    va_start(arg, format);
    vsnprintf(fbuf, FBUF_MAX_SZ, format, arg);
    va_end(arg);
    ret = strtemp(fbuf);
    return( ret );
}

/* tmp_buffer append -- Like tprintf but append at the end of the tmpbuffer */
string
tappend(const string format, ...)
{
    string ret;
    va_list arg;
    va_start(arg, format);
    vsnprintf(fbuf, FBUF_MAX_SZ, format, arg);
    va_end(arg);
    ret = strappend(fbuf);
    return( ret );
}

/* A printf function into strtemp() */
string
gen_tprintf (tstr_ptr tp, const string format, ...)
{
    string ret;
    va_list arg;
    va_start(arg, format);
    vsnprintf(fbuf, FBUF_MAX_SZ, format, arg);
    va_end(arg);
    ret = gen_strtemp(tp, fbuf);
    return( ret );
}

/* tmp_buffer appern -- Like tprintf but append at the end of the tmpbuffer */
string
gen_tappend(tstr_ptr tp, const string format, ...)
{
    string ret;
    va_list arg;
    va_start(arg, format);
    vsnprintf(fbuf, FBUF_MAX_SZ, format, arg);
    va_end(arg);
    ret = gen_strappend(tp, fbuf);
    return( ret );
}

/* Wprintf -- My own version of C Library fprintf(stderr, "Warning: ...) */
VOID
Wprintf (const string format, ...)
{
    if( file_load ) {
        FP(warning_fp, "\nWARNING while reading file %s on line %d:\n",
	        cur_file_name, line_nbr);
    } else {
        FP(warning_fp, "\nWARNING:\n");
    }
    va_list arg;
    va_start(arg, format);
    FP_va(warning_fp, format, arg);
    va_end(arg);
}

/* Debug printf */
void
DBG_printf(const string format, ...)
{
    va_list arg;
    va_start(arg, format);
    FP_va(err_fp, format, arg);
    va_end(arg);
}

string
make_tcl_safe(string s)
{
    string r = safe_buf;
    int active_bsl = 0;
    while( *s ) {
	switch( *s ) {
	    case '{':
	    case '}':
		if( active_bsl == 1 ) {
		    *r = '\\';
		    r++;
		}
		*r = '\\';
		r++;
		*r = *s;
		s++; r++;
		active_bsl = 0;
		break;
	    case '\\':
		active_bsl = 1-active_bsl;
		*r = *s;
		s++; r++;
		break;
	    default:
		*r = *s;
		s++; r++;
		active_bsl = 0;
		break;
	}
    }
    *r = 0;
    return safe_buf;
}
