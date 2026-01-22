//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include "fl.h"
#include "language.tab.h"
#include <ctype.h>
#include <limits.h>
#include "prefs_ext.h"
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#include <poll.h>
#include <X11/Xresource.h>


#define FL_VERSION "4.0"

/* ======================== Global variables ======================== */
string      Voss_tmp_dir = NULL;
char        *prompt = ": ";
bool        compile_to_C_flag = FALSE;
FILE        *v_order_fp = NULL;
FILE        *odests_fp = NULL;
bool	    implication_only = FALSE;
jmp_buf	    toplevel_eval_env;
bool	    gui_mode = TRUE;
bool	    hide_window = FALSE;
bool	    use_stdin = FALSE;
bool	    use_stdout = FALSE;
bool        Interrupt_asap = FALSE;
bool	    cephalopode_mode = FALSE;
bool	    profiling_active = FALSE;
bool	    profiling_builtins = FALSE;
bool	    quiet_run = FALSE;
#if DBG_TRACE_AND_SAVE
int	    debug_id = -1;
int	    debug_start_comparing = 0;
bool	    dbg_dummy_graph_comparison = FALSE;
int	    start_save_graph_idx = 0;
int	    end_save_graph_idx = 0;
#endif
#ifdef CHECK_REF_CNTS
int	    start_save_graph_after_ref_cnt_checks = -1;
#endif
string	    *fl_args;

FILE        *to_tcl_fp;
FILE        *sync_to_tcl_fp;
FILE        *cmd_from_tcl_fp;
FILE        *callback_from_tcl_fp;
FILE        *callback_from_tcl_fp_res;

/* ===================== Global variables referenced ===================== */
extern int		LG_TBL_SIZE;
extern bool		file_load;
extern string		binary_location;
extern bool		RCadd_debug_info;
extern str_mgr		*stringsp;

bool	do_parse(bool flush);
bool	do_parse_stdin(bool flush);
bool	perform_fl_command(string txt, bool restore_line_nbr);

/* ===================== Local variables defined ===================== */
#define TCL_CMD_BUF_SZ	    16384
#define TCL_PRINTF_BUF_SZ   16384
//
static char             buf[TCL_CMD_BUF_SZ];
static char             path_buf[PATH_MAX+1];
static char             parse_buf[6*TCL_CMD_BUF_SZ];
static char             stdin_buf[2*TCL_CMD_BUF_SZ];
static char		tcl_buf[TCL_PRINTF_BUF_SZ+1];
static string 		use_window		= NULL;
static string 		start_file		= NULL;
static string 		input_file_for_cmds     = NULL;
static int 		input_file_for_cmds_pid = -1;
static string 		output_file_for_results = NULL;
static string 		v_order_file		= NULL;
static string	        new_default_dir		= NULL;
static string	        new_temp_dir		= NULL;
static buffer		tcl_eval_done_buf;
static buffer		tcl_eval_result_buf;
static string		s_empty_string;
static string		scaling_factor = "";
static string		default_font =
				"-*-courier-bold-r-normal-*-14-*-*-*-*-*-*-*";
static string		display = NULL;

/* ===================== Local functions defined ===================== */
static void	busy(bool busy);
static void	break_handler(int sig);
static void     setup_sockets();
static void	print_help();
static bool	process_commands(string bufp, bool verbose);
static int	call_fl_inside_setjmp(string cur_start, bool *okp);
static string	get_xresource(string resource, string default_res);

/* ===================== Global functions defined ===================== */
void
Emit_prompt(const char *pre)
{
    if( !file_load ) {
	FP(stdout_fp, "%s%s", pre, prompt);
	Flush(stdout_fp);
    }
}

string
Get_DIR(string file_fullname)
{
    char *tmp = realpath(file_fullname, path_buf);
    char *end = strrchr(tmp, '/');
    if (end != NULL) {
	*(end+1) = 0;
    } else {
	DIE("Should never happen");
    }
    return( wastrsave(stringsp, tmp) );
}

FILE *
Tryopen(string name, string mode, string *fullnamep)
{
    FILE *ret;
    string new_name;

    if( (ret = fopen(name, mode)) != NULL ) {
        if( fullnamep != NULL) {
	    char *tmp = realpath(name, path_buf);
            *fullnamep = wastrsave(stringsp, tmp);
        }
	return ret;
    }
    new_name = strtemp(RCDefault_dir);
    new_name = strappend("/");
    new_name = strappend(name);
    if( (ret = fopen(new_name, mode)) != NULL ) {
        if( fullnamep != NULL) {
	    char *tmp = realpath(new_name, path_buf);
            *fullnamep = wastrsave(stringsp, tmp);
        }
    }
    return( ret );
}


FILE *
file_mktemp (const char *base)
{
    Sprintf(buf, "%s/%s_XXXXXX", Voss_tmp_dir, base);
    int fd = mkstemp(buf);
    if (fd < 0) { return NULL; }
    return( fdopen (fd, "w+") );
}

bool
Mk_output_file_in_tmp_dir(string prefix, FILE **fpp, string *filename)
{
    FILE *fp;
    if( *prefix == '/' ) {
	Sprintf(buf, "%s_XXXXXX", prefix);
    } else {
	Sprintf(buf, "%s/%s_XXXXXX", Voss_tmp_dir, prefix);
    }
    int fd = mkstemp(buf);
    fp = fdopen(fd, "w+");
    if( fp == NULL ) {
        fprintf(stderr, "Could not create tempfile for '%s'\n", buf);
	return( FALSE );
    }
    string name = wastrsave(stringsp, buf);
    setbuf(fp, NULL);
    *fpp = fp;
    *filename = name;
    return(TRUE);
}

static string
read_line(FILE *fp, bool once)
{
    while(1) {
	string s = fgets(buf,TCL_CMD_BUF_SZ-1, fp);
	if( s != NULL || once ) { return s; }
	if( errno != EWOULDBLOCK ) {
	    buf[0] = 0;
	    return buf;
	}
    }
}

int
fl_main(int argc, char *argv[])
{
    string	size_str;
    string	rand_str;
    string	expr_eval_file = NULL;
    int		rand_init;
    FILE *	input_file_for_cmds_fp = NULL;
    bool	exit_on_failure = FALSE;
    bool	unbuf_stdout = FALSE;
    bool	ok;

#ifndef __APPLE__
    // Unlimit stacksize
    struct rlimit rlim = {RLIM_INFINITY, RLIM_INFINITY};
    if( setrlimit(RLIMIT_STACK, &rlim) == -1 ) {
	fprintf(stderr, "Failed to unset the stacksize limit. Guru problem!\n");
	exit(-1);
    }
#endif
    size_str = NULL;
    rand_str = NULL;
    stdin_buf[0] = 0;
    while( argc > 1 && argv[1][0] == '-' ) {
        if( strcmp(argv[1], "--") == 0 ) {
            argc -= 1; argv += 1;
	    break;
	}
#if DBG_TRACE_AND_SAVE
        if( strcmp(argv[1], "--dummy_graph_compare") == 0 ) {
	    dbg_dummy_graph_comparison = TRUE;
            argc -= 1; argv += 1;
        } else
        if( strcmp(argv[1], "--id") == 0 ) {
            debug_id = atoi(argv[2]);
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "--start_comparing") == 0 ) {
            debug_start_comparing = atoi(argv[2]);
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "-z") == 0 ) {
            start_save_graph_idx = atoi(argv[2]);
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "-Z") == 0 ) {
            end_save_graph_idx = atoi(argv[2]);
            argc -= 2; argv += 2;
        } else
#endif
#ifdef CHECK_REF_CNTS
        if( strcmp(argv[1], "--start_save_graph_after_ref_cnt_checks") == 0 ) {
            start_save_graph_after_ref_cnt_checks = atoi(argv[2]);
            argc -= 2; argv += 2;
        } else
#endif
        if( (strcmp(argv[1], "-h") == 0) || (strcmp(argv[1], "--help") == 0) ) {
	    print_help();
	    exit(0);
        } else
        if( strcmp(argv[1], "--scaling") == 0 ) {
	    scaling_factor = argv[2];
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "-display") == 0 ) {
	    display = argv[2];
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "--max_cpu_time") == 0 ) {
	    int limit;
	    str2int(argv[2], &limit);
	    struct rlimit rl;
	    getrlimit(RLIMIT_CPU, &rl);
	    rl.rlim_cur = (rlim_t) limit;
	    rl.rlim_max = (rlim_t) limit;
	    setrlimit(RLIMIT_CPU, &rl);
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "--max_memory") == 0 ) {
	    int limit;
	    str2int(argv[2], &limit);
	    struct rlimit rl;
	    getrlimit(RLIMIT_DATA, &rl);
	    rl.rlim_cur = (rlim_t) limit*1000000;
	    setrlimit(RLIMIT_DATA, &rl);
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "--eval_expr") == 0 ) {
	    expr_eval_file = argv[2];
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "--read_input_from_file") == 0 ) {
            input_file_for_cmds = argv[2];
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "--write_output_to_file") == 0 ) {
            output_file_for_results = argv[2];
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "-d") == 0 ) {
	    // Turn off function tracing information
            RCadd_debug_info = FALSE;
            argc--, argv++;
        } else
        if( strcmp(argv[1], "-q") == 0 ) {
	    // Turn off printing of the VossII banner.
            quiet_run = TRUE;
            argc--, argv++;
        } else
        if( (strcmp(argv[1], "-use_stdin") == 0) ||
	    (strcmp(argv[1], "--use_stdin") == 0) )
	{
            use_stdin = TRUE;
            argc--, argv++;
        } else
        if( (strcmp(argv[1], "-P") == 0) ||
	    (strcmp(argv[1], "--Profiling") == 0) )
	{
            profiling_active = TRUE;
	    profiling_builtins = TRUE;
            argc--, argv++;
        } else
        if( (strcmp(argv[1], "-p") == 0) ||
	    (strcmp(argv[1], "--profiling") == 0) )
	{
            profiling_active = TRUE;
            argc--, argv++;
        } else
        if( (strcmp(argv[1], "-C") == 0) ||
	    (strcmp(argv[1], "--cephalopode") == 0) )
	{
            cephalopode_mode = TRUE;
	    RCadd_debug_info = FALSE;
            argc--, argv++;
        } else
        if( (strcmp(argv[1], "-unbuf_stdout") == 0) ||
	    (strcmp(argv[1], "--unbuf_stdout") == 0) )
	{
            unbuf_stdout = TRUE;
            argc--, argv++;
        } else
        if( (strcmp(argv[1], "-use_stdout") == 0) ||
	    (strcmp(argv[1], "--use_stdout") == 0) )
	{
            use_stdout = TRUE;
            argc--, argv++;
        } else
        if( (strcmp(argv[1], "-noX") == 0) || (strcmp(argv[1], "--noX") == 0)) {
            gui_mode = FALSE;
            argc--, argv++;
        } else
        if( (strcmp(argv[1], "-hide-window") == 0) || (strcmp(argv[1], "--hide-window") == 0)) {
            hide_window = TRUE;
            argc--, argv++;
        } else
	if( strcmp(argv[1], "-T") == 0 ) {
            new_temp_dir = argv[2];
            argc -= 2; argv += 2;
	} else
        if( strcmp(argv[1], "-I") == 0 ) {
            new_default_dir = argv[2];
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "-use") == 0 ||
	    strcmp(argv[1], "--use") == 0 ) {
            use_window = argv[2];
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "-r") == 0 ) {
            rand_str = argv[2];
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "-s") == 0 ) {
            size_str = argv[2];
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "-v") == 0 ) {
            v_order_file = argv[2];
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "-f") == 0 ) {
            start_file = argv[2];
            argc -= 2; argv += 2;
        } else
        if( strcmp(argv[1], "-F") == 0 ) {
	    exit_on_failure = TRUE;
            start_file = argv[2];
            argc -= 2; argv += 2;
        } else {
            fprintf(stderr, "Unknown option %s\n", argv[1]);
            exit(-1);
        }
    }

    if( unbuf_stdout ) { setbuf(stdout, NULL); }

    LG_TBL_SIZE = 20;
    if( size_str != NULL && !str2int(size_str, &LG_TBL_SIZE) )
    {
	Eprintf("Size flag takes a number between 12 and 24.\n");
    }
    if( LG_TBL_SIZE < 12 || LG_TBL_SIZE > 24 ) {
	Eprintf("Size flag takes a number between 12 and 24.\n");
    }
    rand_init = 1;
    if( rand_str != NULL && !str2int(rand_str, &rand_init) )
	Eprintf(
	     "-r takes an integer to initialize the random number generator\n");
    srandom(rand_init);
    Init();
    new_buf(&tcl_eval_done_buf, 10, sizeof(int));
    new_buf(&tcl_eval_result_buf, 10, sizeof(string));
    s_empty_string = wastrsave(stringsp, "");

    fl_args = argv+1;

    if( new_default_dir != NULL )
	RCDefault_dir = wastrsave(stringsp, new_default_dir);
    if( v_order_file != NULL ) {
	if( (v_order_fp = fopen(v_order_file, "w+")) == NULL )
	    Eprintf("Cannot open file %s for writing\n", v_order_file);
    } else {
	v_order_fp = fopen("/dev/null", "w");
    }
    if( input_file_for_cmds != NULL ) {
	input_file_for_cmds = wastrsave(stringsp, input_file_for_cmds);
    }
    Set_default_break_handler();

    char * user = getenv("USER");
    if (user == NULL) {
        // By default, sprintf "%s" NULL prints "(none)"
        // Which leads to invalid file paths
        user = "_nouser_";
    }
    if( new_temp_dir == NULL ) {
	Sprintf(buf, "/tmp/voss2_%s_XXXXXX", user);
	char *res = mkdtemp(buf);
	if( res == NULL ) {
	    Eprintf("Cannot create temporary directory from %s\n", buf);
	}
	Voss_tmp_dir = (char *) Malloc(strlen(res)+1);
	strcpy(Voss_tmp_dir, res);
    } else {
	Voss_tmp_dir = (char *) Malloc(strlen(new_temp_dir)+1);
	strcpy(Voss_tmp_dir, new_temp_dir);
    }

    // Initialize built-in functions
    Install_BuiltIns();

#if DBG_TRACE_AND_SAVE
    Start_Compare_Fun(debug_id);
#endif

    if( expr_eval_file != NULL ) {
	string inp_file = tprintf("%s.inp.tar.gz", expr_eval_file);
	// Only evaluate expression in file and write out result
	g_ptr redex = Get_node();
	PUSH_GLOBAL_GC(redex);
	if( !Load_graph("_dummy_sig_", inp_file, redex) ) {
	    fprintf(stderr, "Load_graph in expr_eval_file failed (%s)\n",
			    inp_file);
	    exit(-21);
	}
	// Remove input file
	string cmd = tprintf("rm -f %s.inp.tar.gz", expr_eval_file);
	system(cmd);
	// Now force evaluation of redex
	redex = force(redex, FALSE);
	// And save the result in the output file
	string out_file = tprintf("%s.out.tar.gz", expr_eval_file);
	out_file = wastrsave(stringsp, out_file);
	if( !Save_graph("_dummy_sig_", out_file, redex) ) {
	    fprintf(stderr, "Save_graph in expr_eval_file failed (%s)\n",
			    out_file);
	    exit(-22);
	}
	// And quit
	// Make sure file is written (and visible) before quitting.
	for(int i = 0; i < 5; i++) {
	    if( access(out_file, F_OK) >= 0 ) {
		Exit(0);
	    }
	    sleep(1);
	}
	Exit(-33);
    }

    if( gui_mode ) {
        /* Until GUI is up and running */
        gui_mode = 0;

	if( output_file_for_results != NULL ) {
	    use_stdout = TRUE;
	    dup2(fileno(stdout), fileno(stderr));
	    FILE *fp = fopen(output_file_for_results, "w");
	    if( fp == NULL ) {
		Eprintf("Cannot open file %s for writing\n",
			output_file_for_results);
	    }
	    dup2(fileno(stdout), fileno(fp));
	    setvbuf(stdout, NULL, _IONBF, 1000);
	}

	if( input_file_for_cmds != NULL ) {
	    // Gymnastics needed to ensure we can read input from
	    // a file that we gradually append to....
	    // Needed for vi-mode and emacs-mode to avoid busy-wait
	    int i= system(tprintf("rm -f %s", input_file_for_cmds));
	    i = system(tprintf("touch %s", input_file_for_cmds));
	    (void) i;
	    int fd[2];
	    if( pipe(fd) != 0 ) {
		Eprintf("pipe failed.... GURU!");
	    }
	    int rd_fd  = fd[0];
	    int wr_fd = fd[1];
	    int parent_pid = getpid();
	    input_file_for_cmds_pid = fork();
	    if( input_file_for_cmds_pid == -1 ) { Eprintf("fork failed...."); }
	    if( input_file_for_cmds_pid == 0 ) {
		// Child (do the tail)
		close(rd_fd);
		dup2(wr_fd, 1);
		close(wr_fd);
		char pbuf[16];
		Sprintf(pbuf, "%d", parent_pid);
		execlp("tail", "tail", "-f", input_file_for_cmds,
		      "--pid", pbuf, NULL);
	    }
	    close(wr_fd);
	    input_file_for_cmds_fp = fdopen(rd_fd, "r");
	    setvbuf(input_file_for_cmds_fp, NULL, _IOLBF, 1000);
	    int cur_flags = fcntl(rd_fd, F_GETFL, 0);
	    fcntl(rd_fd, F_SETFL, cur_flags | O_NONBLOCK);
	    use_stdin = FALSE;
	}

        setup_sockets();

	int flags = fcntl(0, F_GETFL, 0);
	fcntl(0, F_SETFL, flags | O_NONBLOCK);

        gui_mode = 1;

        if(hide_window) { Send_to_tcl("hide_fl_window", NULL); }

	if( !quiet_run ) {
	    FP(stdout_fp, "     /\\           \n");
	    FP(stdout_fp, "    /  \\ /\\       \n");
	    FP(stdout_fp, "   /    VossII %s (%s)\n", FL_VERSION, VERSION_DATE);
	    FP(stdout_fp, "VOSS-LIBRARY-DIRECTORY = %s\n", RCDefault_dir);
	    FP(stdout_fp, "Temporary files directory = %s\n", Voss_tmp_dir);
	}

        {
            string cmd = tprintf("(_load \"%s/preamble.fl\" F) fseq ();",
				 binary_location);
            if( perform_fl_command(cmd, FALSE) ) {
                /* Successful load */
            } else {
                /* Failed load */
                Eprintf("Failed to load  %s\n", cmd+5);
            };
        }
	string font_cmd = tprintf("set_font \"%s\";\n", 
				  get_xresource("VossII.font", default_font));
	busy(TRUE);
	call_fl_inside_setjmp(font_cmd, &ok);
	busy(FALSE);

        if( start_file != NULL ) {
            string cmd = tprintf("((_load \"%s\" F) fseq ())", start_file);
	    if( exit_on_failure ) {
		FP(err_fp,
		    " gen_catch (\\m. fprintf stderr \"%%s\" m) fseq (exit 1)"
		);
	    }
            charappend(';');
	    busy(TRUE);
	    bool ok;
	    if( call_fl_inside_setjmp(cmd, &ok) != 0 ) {
		if( exit_on_failure ) {
		    exit(1);
		}
	    }
	    busy(FALSE);
        }

        while (1) {
            struct pollfd fds[3];
            int source_cnt = 2;
	    FILE *fp;
            fds[0].fd = fileno(cmd_from_tcl_fp);
            fds[0].events = POLLIN;
            fds[0].revents = 0;
            fds[1].fd = fileno(callback_from_tcl_fp);
            fds[1].events = POLLIN;
            fds[1].revents = 0;
	    if( input_file_for_cmds_fp != NULL ) {
		fds[2].fd = fileno(input_file_for_cmds_fp);
		fds[2].events = POLLIN;
		fds[2].revents = 0;
		source_cnt = 3;
		fp = input_file_for_cmds_fp;
	    }
	    if( use_stdin ) {
		fds[2].fd = fileno(stdin);
		fds[2].events = POLLIN;
		fds[2].revents = 0;
		source_cnt = 3;
		fp = stdin;
	    }
            int poll_res;

          restart_inp:
	    while( (poll_res = poll(fds, source_cnt, -1)) < 1 ) {
		CHECK_FOR_INTERRUPT;
	    }
	  process_more:
            poll_res = 99;
            for(int i = 0; i < source_cnt; i++) {
                if( poll_res == 99 && fds[i].revents == POLLIN ) {
                    poll_res = i;
		    break;
		}
            }
	    if( poll_res == 99 ) { goto restart_inp; }
	    CHECK_FOR_INTERRUPT;
            switch( poll_res ) {
                case 0: {
                    // Command(s) to be executed (coming from cmd_from_tcl_fp)
		    string cur_start = strtemp("");
		    // Get number of lines
		    string s = read_line(cmd_from_tcl_fp, FALSE);
		    int lines = atoi(s);
		    tstr_ptr tstrings = new_temp_str_mgr();
		    int sz = 0;
		    string res = gen_strtemp(tstrings, "");
		    for(int i = 0; i < lines; i++) {
			string s = read_line(cmd_from_tcl_fp, FALSE);
			sz += strlen(s);
			if( sz >= TCL_CMD_BUF_SZ ) {
			    Wprintf("Too long command. Ignored.");
			    while(fgets(buf, TCL_CMD_BUF_SZ-1, cmd_from_tcl_fp))
				;
			    fprintf(cmd_from_tcl_fp, "{1 0 0}\n");
			    fflush(cmd_from_tcl_fp);
			    goto process_more;
			}
			gen_strappend(tstrings, buf);
		    }
		    cur_start = unprotect(res);
		    free_temp_str_mgr(tstrings);
		    strcpy(parse_buf, cur_start);

		    clearerr(cmd_from_tcl_fp);
		    if( !process_commands(parse_buf, FALSE) ) {
			fprintf(cmd_from_tcl_fp, "{0 0 0}\n");
		    } else {
			fprintf(cmd_from_tcl_fp, "{1 0 %d}\n",
						 (int) strlen(stdin_buf));
		    }
                    fflush(cmd_from_tcl_fp);
                }
                break;
                case 1: {
                    // Callback to execute (coming from callback_from_tcl_fp)
                    string s = read_line(callback_from_tcl_fp, FALSE);
		    if( s == NULL ) {
			break;
		    }
                    buf[strlen(s)-1] = 0;
		    string start;
		    int rid = (int) strtol(buf, &start, 10);
		    Tcl_callback_eval(start, rid, callback_from_tcl_fp_res);
                    fflush(callback_from_tcl_fp_res);
                }
                break;
                case 2: {
                    // Input from stdin or file
		    fprintf(to_tcl_fp, "prepare_for_stdin\n");
		    fflush(to_tcl_fp);
		    while( read_line(fp, TRUE) ) {
			size_t used = strlen(stdin_buf);
			size_t req  = strlen(buf);
			// Remove comments if too many...
			while( used+req+1 >= 2*TCL_CMD_BUF_SZ) {
			    // Try to remove comments
			    if( stdin_buf[0] == '/' && stdin_buf[1] == '/' ) {
				string nl = index(stdin_buf, '\n');
				if( nl == NULL ) {
				    // Just remove it all
				    stdin_buf[0] = 0;
				    used = 0;
				} else {
				    string p = stdin_buf;
				    while( *nl ) {
					*p = *(nl+1);
					p++; nl++;
				    }
				    used = strlen(stdin_buf);
				}
			    } else {
				// Just remove it all
				stdin_buf[0] = 0;
				used = 0;
			    }
			}
			string res = strcat(stdin_buf, buf);
			if( !process_commands(res, TRUE) ) {
			    stdin_buf[0] = 0;
			    goto process_more;
			}
		    }
                }
                break;
                default:
                    // Something went wrong
                    break;
            }
        }
    } else {
        /* noX mode */
	if( !quiet_run ) {
	    FP(stdout_fp, "     /\\           \n");
	    FP(stdout_fp, "    /  \\ /\\       \n");
	    FP(stdout_fp, "   /    VossII %s (%s)\n", FL_VERSION, VERSION_DATE);
	    FP(stdout_fp, "VOSS-LIBRARY-DIRECTORY = %s\n", RCDefault_dir);
	    FP(stdout_fp, "Temporary files directory = %s\n", Voss_tmp_dir);
	}
        if( start_file != NULL ) {
	    // Create a dummy file that loads preamble.fl and start_file
	    FILE *fp;
	    string filename;
	    if( !Mk_output_file_in_tmp_dir("load0", &fp, &filename) ) {
		exit(-2);
	    }
	    fprintf(fp, "(_load \"%s/preamble.fl\" F) fseq ();\n",
			binary_location);
	    fprintf(fp, "((_load \"%s\" F) fseq ())", start_file);
	    if( exit_on_failure ) {
		fprintf(fp,
		    " gen_catch (\\m. fprintf stderr \"%%s\" m) fseq (exit 1)"
		);
	    }
	    fprintf(fp, ";\n");
	    fclose(fp);
	    switch( setjmp(toplevel_eval_env) ) {
		case 0:
		    Read_from_file(filename, FALSE, FALSE);
		    break;
		case 2:
		    // Interrupted and "return to top" selected
		    break;
		default:
		    DIE("Should never happen");
	    }
	} else {
	    Sprintf(buf, "%s/preamble.fl", binary_location);
	    Read_from_file(buf, FALSE, FALSE);
	}
        Set_default_break_handler();
        while(1) {
            switch( setjmp(toplevel_eval_env) ) {
                case 0:
                    /* All ok */
                    if (gui_mode) {
                        Emit_prompt("");
                        do_parse(TRUE);
                    }
                    else
                        do_parse_stdin(TRUE);
                    break;
                case 2:
                    /* User interrupt with "return to top" */
                    break;
                default:
                    DIE("Should never happen");
                    break;
            }
        }
    }
    printf("\n");
    return 0;
}

void
Set_default_break_handler()
{
    signal(SIGINT, break_handler);
}

void
Exit(int status)
{
    if( output_file_for_results != NULL ) {
	unlink(output_file_for_results);
    }
    if( input_file_for_cmds_pid != -1 ) {
	kill(input_file_for_cmds_pid, SIGKILL);
	unlink(input_file_for_cmds);
    }
    // Clean up and remove temp files
    if( (new_temp_dir == NULL) && (Voss_tmp_dir != NULL) ) {
        Sprintf(buf, "rm -rf %s", Voss_tmp_dir);
        int i = system(buf);
	(void) i;
    }
    fprintf(stderr, "\n");
    if( profiling_active ) { Emit_profile_data(); }
    exit(status);
}

unsigned int
Ustr_hash(pointer np, unsigned int n)
{
    return( ((unint) ((lunint) np)) % n );
}

bool
Ustr_equ(pointer p1, pointer p2)
{
    return( p1 == p2 );
}

void
Tcl_printf (FILE *tcl_fp, const string format, ...)
{
    va_list arg;
    va_start(arg, format);
    vsnprintf(tcl_buf, TCL_PRINTF_BUF_SZ, format, arg);
    va_end(arg);
    string p = tcl_buf;
    while( *p ) {
        switch( *p ) {
            case '\r':
		// Ignore CR
                p++;
                break;
            case '"':
            case '[':
            case ']':
            case '{':
            case '}':
            case ';':
            case '#':
            case '$':
            case ' ':
            case '\t':
            case '\n':
                fprintf(tcl_fp, "\\u%04x", ((unsigned int) *p));
                p++;
                break;
            case '\\':
                p++;
                if( *p && *p == 'u' ) {
                    unsigned int ch;
                    if( sscanf(p, "u%04x", &ch) == 1 ) {
                        p--;
                        char tmp = *(p+6);
                        *(p+6) = 0;
			fprintf(tcl_fp, "%s", p);
                        *(p+6) = tmp;
                        p += 6;
                        break;
                    }
                }
                p--;
                fprintf(tcl_fp, "\\u%04x", ((unsigned int) *p));
                p++;
                break;
            default:
		fputc(*p, tcl_fp);
                p++;
                break;
        }
    }
}

/* ==================================================================== */
/*                                                                      */
/*                         Local functions                              */
/*                                                                      */
/* ==================================================================== */

static void
break_handler(int signo)
{
    (void) signo;
    signal(SIGINT, break_handler);
    Interrupt_asap = TRUE;
}

void
Serve_Interrupt()
{
    int c;

    Interrupt_asap = FALSE;
    if( gui_mode ) {
        if( use_stdout ) FP(err_fp, "\n\n---- Interrupt raised ----\n");
        while(1) {
	    string res = NULL;
            Sprintf(buf, "voss2_interrupt_action");
            Send_to_tcl(buf, &res);
            switch( *res ) {
                    case 'c': {
			    free(res);
                            return;
		    }
                    case 'x': {
			    free(res);
                            Exit(2);
			    break;
		    }
                    case 'r': {
			    free(res);
			    res = NULL;
			    Reset_eval_context();
                            Emit_prompt("\n");
                            longjmp(toplevel_eval_env, 2);
			    break;
		    }
		    case 's': {
			    free(res);
			    res = NULL;
			    FP(err_fp, "%s\n",
			       Get_stack_trace(RCmax_stack_trace_entries));
			    break;
		    }
                    default: {
			    free(res);
			    res = NULL;
                            break;
		    }
            }
        }
    } else {
        FP(err_fp, "\n\n---- Interrupt raised ----\n");
        if( !isatty(fileno(stdin)) ) { Exit(3); }
        while( 1 ) {
            FP(err_fp, "\n\tContinue, return to top level, or exit [C,R,X]?");
	    clearerr(stdin);
            c = fgetc(stdin);
            switch( c ) {
                    case 'c':
                    case 'C':
                            return;
                    case 'x':
                    case 'X':
                            Exit(2);
			    break;
                    case 'r':
                    case 'R':
			    Reset_eval_context();
                            Emit_prompt("\n");
                            longjmp(toplevel_eval_env, 2);
			    break;
		    case 's':
		    case 'S':
			    FP(err_fp, "%s\n",
			       Get_stack_trace(RCmax_stack_trace_entries));
			    break;
                    default:
			    sleep(1);
                            break;
            }
        }
    }
}

static void
setup_sockets()
{
    struct sockaddr_in serv_addr;

    socklen_t len = sizeof(serv_addr);
    memset(&serv_addr, 0, len);

#ifdef __APPLE__
    serv_addr.sin_len = len;
#endif
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = INADDR_ANY;
    serv_addr.sin_port = htons(0);

    int listenfd = socket(AF_INET, SOCK_STREAM, 0);
    if (listenfd < 0) {
        Eprintf("Creating socket failed\n");
    }


    if( bind(listenfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr)) != 0 ) {
        Eprintf("Failed to bind\n");
	fprintf(stderr, "Error: %s\n", strerror(errno));
    }

    if (getsockname(listenfd, (struct sockaddr *)&serv_addr, &len) != 0) {
        Eprintf("Failed to getsockname\n");
	fprintf(stderr, "Error: %s\n", strerror(errno));
    }

    uint32_t addr = ntohs(serv_addr.sin_addr.s_addr);
    uint16_t port = ntohs(serv_addr.sin_port);

    pid_t fl_pid = getpid();

    if( use_window != NULL ) {
	if( display != NULL  ) {
	    Sprintf(buf,
		   "wish %s/front_end.tcl %d %d %d %s %s -use %s -display %s &",
		    binary_location, fl_pid, addr, port, Voss_tmp_dir,
		    scaling_factor, use_window, display);
	} else {
	    Sprintf(buf, "wish %s/front_end.tcl %d %d %d %s %s -use %s &",
			 binary_location, fl_pid, addr, port, Voss_tmp_dir,
			 scaling_factor, use_window);
	}
    } else {
	if( display != NULL ) {
	    Sprintf(buf, "wish %s/front_end.tcl %d %d %d %s %s -display %s &",
			 binary_location, fl_pid, addr, port,
			 Voss_tmp_dir, scaling_factor, display);
	} else {
	    Sprintf(buf, "wish %s/front_end.tcl %d %d %d %s %s &",
			 binary_location, fl_pid, addr, port, Voss_tmp_dir,
			 scaling_factor);
	}
    }
    if( system(buf) != 0 ) {
        Eprintf("Failed to execute %s/front_end.tcl\n", binary_location);
    }

    if (listen(listenfd, 5) != 0) {
	Eprintf("failed to listen\n");
	fprintf(stderr, "Error: %s\n", strerror(errno));
    }


    int connfd0 = accept(listenfd, (struct sockaddr*)NULL, NULL);
    if (connfd0 == -1) {
        Eprintf("failed to accept\n");
        fprintf(stderr, "Error: %s\n", strerror(errno));
    }
    fcntl(connfd0, F_SETFL, O_NONBLOCK);
    sync_to_tcl_fp = fdopen(connfd0, "r+");
    setvbuf(sync_to_tcl_fp, NULL, _IOLBF, 1000);

    int connfd1 = accept(listenfd, (struct sockaddr*)NULL, NULL);
    fcntl(connfd1, F_SETFL, O_NONBLOCK);
    to_tcl_fp = fdopen(connfd1, "r+");
    setvbuf(to_tcl_fp, NULL, _IOLBF, 1000);

    int connfd2 = accept(listenfd, (struct sockaddr*)NULL, NULL);
    fcntl(connfd2, F_SETFL, O_NONBLOCK);
    cmd_from_tcl_fp = fdopen(connfd2, "r+");
    setvbuf(cmd_from_tcl_fp, NULL, _IOLBF, 1000);

    int connfd3 = accept(listenfd, (struct sockaddr*)NULL, NULL);
    fcntl(connfd3, F_SETFL, O_NONBLOCK);
    callback_from_tcl_fp = fdopen(connfd3, "r+");
    setvbuf(callback_from_tcl_fp, NULL, _IOLBF, 1000);

    int connfd4 = accept(listenfd, (struct sockaddr*)NULL, NULL);
    fcntl(connfd4, F_SETFL, O_NONBLOCK);
    callback_from_tcl_fp_res = fdopen(connfd4, "w");
    setvbuf(callback_from_tcl_fp_res, NULL, _IOLBF, 1000);

}



string
protect(string txt, bool *changedp)
{
    bool changed = FALSE;
    tstr_ptr tp = new_temp_str_mgr();
    char tbuf[10];
    string p = txt;
    string res = gen_strtemp(tp, "");
    while( *p ) {
        switch( *p ) {
            case '\r':
		// Ignore CR
                p++;
                break;
            case '"':
            case '[':
            case ']':
            case '{':
            case '}':
            case ';':
            case '#':
            case '$':
            case ' ':
            case '\t':
            case '\n':
                Sprintf(tbuf, "\\u%04x", ((unsigned int) *p));
                res = gen_strappend(tp, tbuf);
                p++;
		changed = TRUE;
                break;
            case '\\':
                p++;
                if( *p && *p == 'u' ) {
                    unsigned int ch;
                    if( sscanf(p, "u%04x", &ch) == 1 ) {
                        p--;
                        char tmp = *(p+6);
                        *(p+6) = 0;
                        res = gen_strappend(tp, p);
                        *(p+6) = tmp;
                        p += 6;
                        break;
                    }
                }
                p--;
                Sprintf(tbuf, "\\u%04x", ((unsigned int) *p));
                res = gen_strappend(tp, tbuf);
                p++;
		changed = TRUE;
                break;
            default:
                res = gen_charappend(tp, *p);
                p++;
                break;
        }
    }
    res = strtemp(res);
    free_temp_str_mgr(tp);
    if( changedp != NULL ) { *changedp = changed; }
    return res;
}

string
unprotect(string txt)
{
    string p = txt;
    string res = strtemp("");
    unsigned int ch;
    while( *p ) {
        switch( *p ) {
            case '\\':
                sscanf(p, "\\u%04x", &ch);
		if( ch != 0x0d ) charappend((char) ch);
                p += 6;
                break;
            default:
                charappend(*p);
                p++;
                break;
        }
    }
    return res;
}

void
Info_to_tcl(string cmd)
{
    fprintf(to_tcl_fp, "%s\n", protect(cmd, NULL));
    fflush(to_tcl_fp);
}

bool
Send_to_tcl(string cmd, string *resp)
{
    int done = -1;
    int tcl_eval_depth = COUNT_BUF(&tcl_eval_done_buf);
    push_buf(&tcl_eval_done_buf, &done);
    push_buf(&tcl_eval_result_buf, &s_empty_string);
    fprintf(sync_to_tcl_fp, "%d %s\n", tcl_eval_depth, protect(cmd, NULL));
    fflush(sync_to_tcl_fp);
    // Now enter an event loop processing (potential) fl callbacks while
    // waiting for the return result
    while (1) {
	// Have we (already) gotten the result
	int ok = *( (int *) M_LOCATE_BUF(&tcl_eval_done_buf, tcl_eval_depth));
	if( ok != -1 ) {
            if( resp != NULL ) {
                *resp = *((string *)
			    M_LOCATE_BUF(&tcl_eval_result_buf, tcl_eval_depth));
            }
	    pop_buf(&tcl_eval_done_buf, NULL);
	    pop_buf(&tcl_eval_result_buf, NULL);
            return ok;
        }
	// Check for results (might be an earlier tcl_eval's result!)
	string s;
	if( (s = fgets(buf, TCL_CMD_BUF_SZ-1, sync_to_tcl_fp)) != NULL ) {
            bool ok = (*s == '0')? TRUE : FALSE;
	    string p = buf+1;
	    int rid = 0;
	    while( *p && isdigit(*p) ) {
		rid = 10*rid + *p - '0';
		p++;
	    }
	    p++;
	    ASSERT(rid <= tcl_eval_depth);
	    store_buf(&tcl_eval_done_buf, rid, &ok);
	    if( *(p+strlen(p)-1) != '\n' ) {
		// Too long return value to fit in the buffer
		int c;
		while( (c = fgetc(sync_to_tcl_fp)) != EOF && c != '\n' ) ;
		FP(err_fp,
		   "WARNING: tcl_eval return value too long. Truncated.\n");
	    }
	    string res = unprotect(p);
	    int len = strlen(res);
	    string fres = Malloc(len+1);
	    strncpy(fres, res, len+1);
	    store_buf(&tcl_eval_result_buf, rid, &fres);
	}
	if( (s = fgets(buf, TCL_CMD_BUF_SZ-1, callback_from_tcl_fp)) != NULL ) {
            buf[strlen(s)-1] = 0;
	    string start;
	    int rid = (int) strtol(buf, &start, 10);
	    Tcl_callback_eval(start, rid, callback_from_tcl_fp_res);
            fflush(callback_from_tcl_fp_res);
        }
	CHECK_FOR_INTERRUPT;
    }
}

static void
busy(bool busy)
{
    (void) busy;
//
// Crashes wish in some circumstances.
//    if( busy )
//	fprintf(to_tcl_fp, "i_am_busy\n");
//    else
//	fprintf(to_tcl_fp, "i_am_free\n");
//    fflush(to_tcl_fp);
}

static int
call_fl_inside_setjmp(string cur_start, bool *okp)
{
    switch( setjmp(toplevel_eval_env) ) {
	case 0:
	    *okp = perform_fl_command(cur_start, FALSE);
	    return 0;
	case 2:
	    return 2;
	default:
	    DIE("Should not happen");
    }
}

static bool
process_commands(string bufp, bool verbose)
{
    string cur_start = bufp;
    string p  = cur_start;
    bool in_string = FALSE;
    while( *p ) {
	switch( *p ) {
	    case '/':
		p++;
		if( !in_string ) {
		   if( *p && *p == '/' ) {
			/* Begin comment */
			p++;
			while( *p && *p != '\n' ) p++;
		    }
		}
		break;
	    case '<':
		p++;
		if( !in_string ) {
		   if( *p && *p == '{' ) {
			/* Begin external parser region */
			p++;
			while( *p && *(p+1) &&
			       (*p != '}' || *(p+1) != '>') )
			{
			    p++;
			}
		    }
		}
		break;
	    case '\\':
		p++;
		if( in_string ) {
		    if( *p ) p++;
		}
		break;
	    case '"':
		if( in_string ) {
		    in_string = FALSE;
		} else {
		    in_string = TRUE;
		}
		p++;
		break;
	    case ';':
		if( in_string ) {
		    p++;
		    break;
		} else {
		    // End of command
		    p++;
		    string end;
		    end = p;
		    char tmp = *end;
		    *end = 0;
		    bool ok;
		    if( verbose ) {
			string nb = cur_start;
			while( *nb && isspace(*nb) ) nb++;
			if( strncmp(nb, "set_file_name \"", 15) == 0 ) {
			    string pcmd = protect(nb, NULL);
			    fprintf(to_tcl_fp, "WriteInfo {%s}\n", pcmd);
			} else
			if( strncmp(nb, "set_line_number ", 16) == 0 ) {
			    string pcmd = protect(nb, NULL);
			    fprintf(to_tcl_fp, "WriteInfo {%s}\n", pcmd);
			} else {
			    string pcmd = protect(make_tcl_safe(nb), NULL);
			    fprintf(to_tcl_fp, "WriteStdIn {%s}\n", pcmd);
			    fprintf(to_tcl_fp, "WriteNewLine {}\n");
			}
			fflush(to_tcl_fp);
		    }
		    busy(TRUE);
		    if( call_fl_inside_setjmp(cur_start, &ok) != 0 ) {
			// User interrupt with
			// "return to top" answer
			busy(FALSE);
			return FALSE;
		    }
		    busy(FALSE);
		    *end = tmp;
		    if( !ok ) {
			return FALSE;
		    }
		    cur_start = p;
		    break;
		}
	    default:
		p++;
		break;
	}
    }
    if( cur_start != bufp ) {
	p = bufp;
	while( *cur_start ) {
	    *p = *cur_start;
	    p++;
	    cur_start++;
	}
	*p = 0;
    }
    return TRUE;
}

#define P(s) fprintf(stdout, s)
static void
print_help()
{
 P("Usage: fl [flags] [-- {args}+]\n");
 P(" Flags:\n");
 P("  -noX                          do not use X windows (text only)\n");
 P("  -f file                       Start by reading file\n");
 P("  -F file                       Start by reading file. Quit on failure\n");
 P("  -I dir                        Search for fl libraries in dir\n");
 P("  -T dir                        Use dir for all temporary files.\n");
 P("  -hide-window / --hide-window  Hide the main window while still preserving X functionality\n");
 P("  -use_stdin / --use_stdin      Read inputs also from stdin\n");
 P("  -use_stdout / --use_stdout    Write outputs also to stdout\n");
 P("  -unbuf_stdout/--unbuf_stdout  Do not buffer stdout.\n");
 P("  --hide-window                 Hide the main window while still preserving X functionality\n");
 P("  --read_input_from_file file   Read inputs from file\n");
 P("  --write_output_to_file file   Write outputs to file\n");
 P("  -r n                          Initialize random number generator to n\n");
 P("  -v file                       Save dynamic variable order in file\n");
 P("  -h / --help                   Print out this message and quit\n");
 P("  -d                            Turn off function tracing information\n");
 P("  -q                            Turn off printing of the VossII banner.\n");
 P("  -p / --profiling              Profile all functions in the fl run.\n");
 P("  -P / --Profiling              Profile only user defined functions.\n");
 P("  -C / --cephalopode            Cephalopode mode\n");
 P("  --use window                  Place top-level GUI inside window\n");
 P("  --scaling num                 Set scaling factor for GUI\n");
 P("  --display window              Use X window display\n");
 P("  --max_cpu_time                Max CPU time in seconds allowed\n");
 P("  --max_memory                  Max memory in Mbytes allowed\n");
 P("  --eval_expr basename          Read <basename>.inp.tar.gz,\n");
 P("                                evaluate the expression, and\n");
 P("                                write result to <basename>.out.tar.gz\n");
 P("    --                          Remaining arguments placed in ARGS list\n");
}

static string
get_xresource(string resource, string default_res)
{
    XrmInitialize();

    Display *display = XOpenDisplay(NULL);
    if( display == NULL ) Eprintf("Can't open display\n");

    char *resource_manager = XResourceManagerString(display);
    if( resource_manager == NULL ) {
	XCloseDisplay(display);
	return( wastrsave(stringsp, default_res) );
    }

    XrmDatabase db = XrmGetStringDatabase(resource_manager);
    if (db == NULL) Eprintf("Can't open resource database\n");

    XrmValue value;
    string type;
    if (XrmGetResource(db, resource, resource, &type, &value)) {
	buf[TCL_CMD_BUF_SZ-1] = 0;
	strncpy(buf, value.addr, TCL_CMD_BUF_SZ-2);
	ui len = strlen(buf);
	if(buf[0] == '"' && buf[len-1] == '"' ) {
	    buf[len-1] = 0;
	    XCloseDisplay(display);
	    return( wastrsave(stringsp, &(buf[1])) );
	} else {
	    XCloseDisplay(display);
	    return( wastrsave(stringsp, buf) );
	}
    } else {
	// Resource not found so use default
	XCloseDisplay(display);
	return( wastrsave(stringsp, default_res) );
    }
}

void
voss_abort()
{
    abort();
}
