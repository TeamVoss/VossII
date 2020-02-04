//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include "fl.h"
#include "y.tab.h"
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

#define FL_VERSION "1.0"

/* ======================== Global variables ======================== */
string      Voss_tmp_dir = NULL;
bool        debug_on = TRUE;
char        *prompt = ": ";
bool        compile_to_C_flag = FALSE;
FILE        *v_order_fp = NULL;
FILE        *odests_fp = NULL;
str_mgr	    strings;
bool	    implication_only = FALSE;
jmp_buf	    toplevel_eval_env;
bool	    gui_mode = TRUE;
bool	    hide_window = FALSE;
bool	    use_stdin = FALSE;
bool	    use_stdout = FALSE;
bool        Interrupt_asap = FALSE;
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
extern old_yyin_ptr	cur_file;
extern bool		file_load;
extern string		binary_location;

bool	do_parse(bool flush);
bool	perform_fl_command(string txt);
void	Start_stdin_parsing();

#define TCL_CMD_BUF_SZ  16384
static char             buf[TCL_CMD_BUF_SZ];
static char             path_buf[PATH_MAX+1];
static char             parse_buf[6*TCL_CMD_BUF_SZ];
static char             stdin_buf[2*TCL_CMD_BUF_SZ];
static string 		start_file   = NULL;
static string 		input_file_for_cmds   = NULL;
static int 		input_file_for_cmds_pid = -1;
static string 		output_file_for_results = NULL;
static string 		v_order_file = NULL;
static string	        new_default_dir = NULL;

/* ===================== Local functions defined ===================== */
static void	busy(bool busy);
static void	break_handler(int sig);
static void     setup_sockets();
static void	print_help();
static bool	process_commands(string bufp, bool verbose);
static int	call_setjmp(string cur_start, bool *okp);

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
    return( wastrsave(&strings, tmp) );
}

FILE *
Tryopen(string name, string mode, string *fullnamep)
{
    FILE *ret;
    string new_name;

    if( (ret = fopen(name, mode)) != NULL ) {
        if( fullnamep != NULL) {
	    char *tmp = realpath(name, path_buf);
            *fullnamep = wastrsave(&strings, tmp);
        }
	return ret;
    }
    new_name = strtemp(RCDefault_dir);
    new_name = strappend("/");
    new_name = strappend(name);
    if( (ret = fopen(new_name, mode)) != NULL ) {
        if( fullnamep != NULL) {
	    char *tmp = realpath(new_name, path_buf);
            *fullnamep = wastrsave(&strings, tmp);
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
    string name = wastrsave(&strings, buf);
    setbuf(fp, NULL);
    *fpp = fp;
    *filename = name;
    return(TRUE);    
}

int
fl_main(int argc, char *argv[])
{
    string	size_str;
    string	rand_str;
    int		rand_init;
    FILE *	input_file_for_cmds_fp = NULL;

    // Unlimit stacksize
    struct rlimit rlim = {RLIM_INFINITY, RLIM_INFINITY};
    if( setrlimit(RLIMIT_STACK, &rlim) == -1 ) {
	fprintf(stderr, "Failed to unset the stacksize limit. Guru problem!\n");
	exit(-1);
    }
    size_str = NULL;
    rand_str = NULL;
    stdin_buf[0] = 0;
    while( argc > 1 && argv[1][0] == '-' ) {
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
            debug_on = FALSE;
            argc--, argv++;
        } else
        if( (strcmp(argv[1], "-use_stdin") == 0) ||
	    (strcmp(argv[1], "--use_stdin") == 0) )
	{
            use_stdin = TRUE;
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
        if( strcmp(argv[1], "-I") == 0 ) {
            new_default_dir = argv[2];
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
#if 0
        if( strcmp(argv[1], "-c") == 0 ) {
            compile_to_C_flag = TRUE;
            argc--, argv++;
        } else
#endif
        if( strcmp(argv[1], "-f") == 0 ) {
            start_file = argv[2];
            argc -= 2; argv += 2;
        } else {
            fprintf(stderr, "Unknown option %s\n", argv[1]);
            exit(-1);
        }
    }

    LG_TBL_SIZE = 18;
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

    fl_args = argv+1;

    if( new_default_dir != NULL )
	RCDefault_dir = wastrsave(&strings, new_default_dir);
    if( v_order_file != NULL ) {
	if( (v_order_fp = fopen(v_order_file, "w+")) == NULL )
	    Eprintf("Cannot open file %s for writing\n", v_order_file);
    } else {
	v_order_fp = fopen("/dev/null", "w");
    }
    if( input_file_for_cmds != NULL ) {
	input_file_for_cmds = wastrsave(&strings, input_file_for_cmds);
    }
    Set_default_break_handler();

    Sprintf(buf, "/tmp/voss2_%s_XXXXXX", getenv("USER"));
    char *res = mkdtemp(buf);
    if( res == NULL ) {
	Eprintf("Cannot create temporary directory from %s\n", buf);
    }
    Voss_tmp_dir = (char *) Malloc(strlen(res)+1);
    strcpy(Voss_tmp_dir, res);

    // Initialize built-in functions
    Install_BuiltIns();

#if DBG_TRACE_AND_SAVE
    Start_Compare_Fun(debug_id);
#endif

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
	    int i = system(tprintf("/bin/rm -f %s", input_file_for_cmds));
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
		execl("/usr/bin/tail", "tail", "-f", input_file_for_cmds,
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

	// Set the font to the default according to vossrc entry
	string font_cmd = tprintf("change_fonts %s", RCtext_font);
	Send_to_tcl(font_cmd, NULL);

        if(hide_window) { Send_to_tcl("hide_fl_window", NULL); }

        FP(stdout_fp, "     /\\           \n");
        FP(stdout_fp, "    /  \\ /\\       \n");
        FP(stdout_fp, "   /    VossII %s (%s)\n", FL_VERSION, VERSION_DATE);
        FP(stdout_fp, "VOSS-LIBRARY-DIRECTORY = %s\n", RCDefault_dir);
        FP(stdout_fp, "Temporary files directory = %s\n", Voss_tmp_dir);

        {
            string cmd = tprintf("(_load \"%s/preamble.fl\" F) fseq ();",
				 binary_location);
            if( perform_fl_command(cmd) ) {
                /* Successful load */
            } else {
                /* Failed load */
                Eprintf("Failed to load  %s\n", cmd+5);
            };
        }

        if( start_file != NULL ) {
            string cmd = strtemp("(_load ");
            charappend('"');
            strappend(start_file);
            charappend('"');
	    strappend(" F) fseq ();");
	    busy(TRUE);
	    bool ok;
	    if( call_setjmp(cmd, &ok) != 0 ) { }
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
            int res;

          restart_inp:
	    while( (res = poll(fds, source_cnt, -1)) < 1 ) {
		CHECK_FOR_INTERRUPT;
	    }
            res = 99;
            for(int i = 0; i < 3; i++) {
                if( res == 99 && fds[i].revents == POLLIN )
                    res = i;
            }
	    CHECK_FOR_INTERRUPT;
            switch( res ) {
                case 0: {
                    // Command(s) to be executed (coming from cmd_from_tcl_fp)
		    string cur_start = strtemp("");
		    // Get number of lines
		    string s = fgets(buf,TCL_CMD_BUF_SZ-1, cmd_from_tcl_fp);
		    int lines = atoi(s);
		    tstr_ptr tstrings = new_temp_str_mgr();
		    int sz = 0;
		    string res = gen_strtemp(tstrings, "");
		    for(int i = 0; i < lines; i++) {
			string s = fgets(buf,TCL_CMD_BUF_SZ-1, cmd_from_tcl_fp);
			sz += strlen(s);
			if( sz >= TCL_CMD_BUF_SZ ) {
			    Wprintf("Too long command. Ignored.");
			    while(fgets(buf, TCL_CMD_BUF_SZ-1, cmd_from_tcl_fp))
				;
			    fprintf(cmd_from_tcl_fp, "{1 0 0}\n");
			    fflush(cmd_from_tcl_fp);
			    goto restart_inp;
			}
			gen_strappend(tstrings, buf);
		    }
		    cur_start = unprotect(res);
		    free_temp_str_mgr(tstrings);
		    strcpy(parse_buf, cur_start);

		    if( !process_commands(parse_buf, FALSE) ) {
			clearerr(cmd_from_tcl_fp);
			fprintf(cmd_from_tcl_fp, "{0 0 0}\n");
			fflush(cmd_from_tcl_fp);
			parse_buf[0] = 0;
			goto restart_inp;
		    }
                    clearerr(cmd_from_tcl_fp);
                    fprintf(cmd_from_tcl_fp, "{1 0 %d}\n",
					     (int) strlen(stdin_buf));
                    fflush(cmd_from_tcl_fp);
                }
                break;
                case 1: {
                    // Callback to execute (coming from callback_from_tcl_fp)
                    string s = fgets(buf,TCL_CMD_BUF_SZ-1,callback_from_tcl_fp);
// fprintf(stderr, "<<<<< CGot |%s|\n", buf);
                    buf[strlen(s)-1] = 0;
                    string res;
		    string start;
		    int rid = (int) strtol(buf, &start, 10);
// fprintf(stderr, "2:rid before %d\n", rid);
                    bool ok = Tcl_callback_eval(start, &res);
// fprintf(stderr, "2:rid after %d\n", rid);
                    if( ok ) {
// fprintf(stderr, "\n$$$$$$$$$$ SEND $$$ 1%s\n", protect(res));
                        fprintf(callback_from_tcl_fp_res, "%d 1%s\n", rid, protect(res));
                    } else {
// fprintf(stderr, "\n$$$$$$$ SEND $$$$$$ 0%s\n", protect(res));
                        fprintf(callback_from_tcl_fp_res, "%d 0%s\n", rid, protect(res));
                    }
                    fflush(callback_from_tcl_fp_res);
                }
                break;
                case 2: {
                    // Input from stdin or file
		    fprintf(to_tcl_fp, "prepare_for_stdin\n");
		    fflush(to_tcl_fp);
		    while( fgets(buf, TCL_CMD_BUF_SZ-1, fp) ) {
			string res = strcat(stdin_buf, buf);
			if( !process_commands(res, TRUE) ) {
			    stdin_buf[0] = 0;
			    goto restart_inp;
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
        FP(stdout_fp, "     /\\           \n");
        FP(stdout_fp, "    /  \\ /\\       \n");
        FP(stdout_fp, "   /    VossII %s (%s)\n", FL_VERSION, VERSION_DATE);
        FP(stdout_fp, "VOSS-LIBRARY-DIRECTORY = %s\n", RCDefault_dir);
        FP(stdout_fp, "Temporary files directory = %s\n", Voss_tmp_dir);
        Start_stdin_parsing();
        if( start_file != NULL ) {
	    // Create a dummy file that loads preamble.fl and start_file
	    FILE *fp;
	    string filename;
	    if( !Mk_output_file_in_tmp_dir("load0", &fp, &filename) ) {
		exit(-2);
	    }
	    fprintf(fp, "(_load \"%s/preamble.fl\" F) fseq ();\n",
			binary_location);
	    fprintf(fp, "(_load \"%s\" F) fseq ();\n", start_file);
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
        Emit_prompt("");
        Set_default_break_handler();
        while(1) {
            switch( setjmp(toplevel_eval_env) ) {
                case 0:
                    /* All ok */
                    do_parse(TRUE);
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
    if( Voss_tmp_dir != NULL ) {
        Sprintf(buf, "/bin/rm -rf %s", Voss_tmp_dir);
        int i = system(buf);
	(void) i;
    }
    fprintf(stderr, "\n");
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
            string res = strtemp("");
            Sprintf(buf, "voss2_interrupt_action");
            Send_to_tcl(buf, &res);
            switch( *res ) {
                    case 'c':
                            return;
                    case 'x':
                            Exit(2);
			    break;
                    case 'r':
			    Reset_eval_context();
                            Emit_prompt("\n");
                            longjmp(toplevel_eval_env, 2);
			    break;
		    case 's':
			    FP(err_fp, "%s\n",
			       Get_stack_trace(RCmax_stack_trace_entries));
			    break;
                    default:
                            break;
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

    int listenfd = socket(AF_INET, SOCK_STREAM, 0);

    socklen_t len = sizeof(serv_addr);
    memset(&serv_addr, '0', len);

    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    serv_addr.sin_port = 0;

    if( bind(listenfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr)) < 0 ) {
        Eprintf("Failed to bind\n");
    }

    if (getsockname(listenfd, (struct sockaddr *)&serv_addr, &len) == -1) {
        Eprintf("Failed to getsockname\n");
    }

    uint32_t addr = ntohs(serv_addr.sin_addr.s_addr);
    uint16_t port = ntohs(serv_addr.sin_port);

    Sprintf(buf, "wish %s/front_end.tcl %d %d %d %s &",
	         binary_location, getpid(), addr, port, Voss_tmp_dir);
    if( system(buf) != 0 ) {
        Eprintf("Failed to execute %s/front_end.tcl\n", binary_location);
    }

    listen(listenfd, 5);

    int connfd0 = accept(listenfd, (struct sockaddr*)NULL, NULL);
    sync_to_tcl_fp = fdopen(connfd0, "r+");
    setvbuf(sync_to_tcl_fp, NULL, _IOLBF, 1000);

    int connfd1 = accept(listenfd, (struct sockaddr*)NULL, NULL);
    to_tcl_fp = fdopen(connfd1, "r+");
    setvbuf(to_tcl_fp, NULL, _IOLBF, 1000);

    int connfd2 = accept(listenfd, (struct sockaddr*)NULL, NULL);
    cmd_from_tcl_fp = fdopen(connfd2, "r+");
    setvbuf(cmd_from_tcl_fp, NULL, _IOLBF, 1000);

    int connfd3 = accept(listenfd, (struct sockaddr*)NULL, NULL);
    callback_from_tcl_fp = fdopen(connfd3, "r+");
    setvbuf(callback_from_tcl_fp, NULL, _IOLBF, 1000);

    int connfd4 = accept(listenfd, (struct sockaddr*)NULL, NULL);
    callback_from_tcl_fp_res = fdopen(connfd4, "w");
    setvbuf(callback_from_tcl_fp_res, NULL, _IOLBF, 1000);

}

string
protect(string txt)
{
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
                break;
            default:
                res = gen_charappend(tp, *p);
                p++;
                break;
        }
    }
    res = strtemp(res);
    free_temp_str_mgr(tp);
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
    fprintf(to_tcl_fp, "%s\n", protect(cmd));
    fflush(to_tcl_fp);
}

bool
Send_to_tcl(string cmd, string *resp)
{
// fprintf(stderr, "TRSEND |%s|\n", cmd);
    fprintf(sync_to_tcl_fp, "%s\n", protect(cmd));
    fflush(sync_to_tcl_fp);
    // Now enter an event loop processing (potential) fl callbacks while
    // waiting for the return result
    while (1) {
        struct pollfd fds[2];
        fds[0].fd = fileno(sync_to_tcl_fp);
        fds[0].events = POLLIN;
        fds[0].revents = 0;
        fds[1].fd = fileno(callback_from_tcl_fp);
        fds[1].events = POLLIN;
        fds[1].revents = 0;
        while( poll(fds, 2, -1) < 1 ) {
	    CHECK_FOR_INTERRUPT;
	}
        if( fds[0].revents == POLLIN ) {
            /* Result available */
            string s = fgets(buf, TCL_CMD_BUF_SZ-1, sync_to_tcl_fp);
            bool ok = (*s == '0')? TRUE : FALSE;
            if( resp != NULL ) {
                *resp = unprotect(buf+1);
// fprintf(stderr, "Result1 --> %s\n", *resp);
            } else {
// fprintf(stderr, "Result0 --> %s\n", buf);
	    }
            return ok;
        } else 
        if( fds[1].revents == POLLIN ) {
            // Callback to execute (coming from callback_from_tcl_fp)
            string s = fgets(buf, TCL_CMD_BUF_SZ-1, callback_from_tcl_fp);
            buf[strlen(s)-1] = 0;
            string res;
	    string start;
	    int rid = (int) strtol(buf, &start, 10);
// fprintf(stderr, "++++ Callback rid:%d cmd:|%s|\n", rid, start);
	    bool ok = Tcl_callback_eval(start, &res);
// fprintf(stderr, "  --> ok=%s\n", (ok?"yes":"no"));
            if( ok ) {
// fprintf(stderr, "\n!!!! Answer %d 1 |%s|\n", rid, res);
                fprintf(callback_from_tcl_fp_res,"%d 1%s\n", rid, protect(res));
            } else {
// fprintf(stderr, "\n!!!! Answer %d 0 |%s|\n", rid, res);
                fprintf(callback_from_tcl_fp_res,"%d 0%s\n", rid, protect(res));
            }
            fflush(callback_from_tcl_fp_res);
        }
	CHECK_FOR_INTERRUPT;
    }
}

static void
busy(bool busy)
{
    if( busy )
	fprintf(to_tcl_fp, "i_am_busy\n");
    else 
	fprintf(to_tcl_fp, "i_am_free\n");
    fflush(to_tcl_fp);
}

static int
call_setjmp(string cur_start, bool *okp)
{
    switch( setjmp(toplevel_eval_env) ) {
	case 0:
	    *okp = perform_fl_command(cur_start);
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
			    string pcmd = protect(nb);
			    fprintf(to_tcl_fp, "WriteInfo {%s}\n", pcmd);
			} else
			if( strncmp(nb, "set_line_number ", 16) == 0 ) {
			    string pcmd = protect(nb);
			    fprintf(to_tcl_fp, "WriteInfo {%s}\n", pcmd);
			} else {
			    string pcmd = protect(make_tcl_safe(nb));
			    fprintf(to_tcl_fp, "WriteStdIn {%s}\n", pcmd);
			    fprintf(to_tcl_fp, "WriteNewLine {}\n");
			}
			fflush(to_tcl_fp);
		    }
		    busy(TRUE);
		    if( call_setjmp(cur_start, &ok) != 0 ) {
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
 P("Usage: fl [flags]\n");
 P("  Flags:\n");
 P("    -f file			    start by reading file\n");
 P("    -I dir			    Search for fl libraries in dir\n");
 P("    -hide-window		    hide the main window while still preserving X functionality\n");
 P("    -noX			    do not use X windows (text only)\n");
 P("    -use_stdin		    read inputs also from stdin\n");
 P("    -use_stdout		    write outputs also to stdout\n");
 P("    --hide-window		    hide the main window while still preserving X functionality\n");
 P("    --noX			    do not use X windows (text only)\n");
 P("    --use_stdin		    read inputs also from stdin\n");
 P("    --use_stdout		    write outputs also to stdout\n");
 P("    --read_input_from_file file read inputs from file\n");
 P("    --write_output_to_file file write outputs to file\n");
 P("    -r n			    initialize random number generator to n\n");
 P("    -v file			    save dynamic variable order in file\n");
 P("    -h			    print out this message and quit\n");
 P("    --help			    print out this message and quit\n");

}
