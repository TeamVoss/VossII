/************************************************************************/
/*									*/
/*		Copyright: Carl-Johan Seger, 1990			*/
/*									*/
/************************************************************************/
#include "initialize.h"

/***** GLOBAL VARIABLES DEFINED *****/
#include "prefs.h"

/***** GLOBAL VARIABLES ACCESSED *****/
void        Init_lexer();
extern str_mgr	strings;
extern string	default_vosslib_dir;


/***** PRIVATE VARIABLES *****/

/************************************************************************/
/*			GLOBAL FUNCTIONS				*/
/************************************************************************/

VOID
Init()
{
    Init_strmgr();
    new_strmgr(&strings);

    Init_symb_tbl();
    Init_Paths(NULL, NULL);
    initRC(default_vossrc, ".vossrc", getenv("VOSS-LIBRARY-DIRECTORY"));
    initRC(default_vossrc, ".vossrc", "$VOSS");
    initRC(default_vossrc, ".vossrc", "$HOME");
    initRC(default_vossrc, ".vossrc", ".");
    Init_Paths(getRCvalue("VOSS-BINARY-DIRECTORY"),
	       getRCvalue("VOSS-LIBRARY-DIRECTORY"));

    B_Init();
    Init_file_ops();
    TC_Init();
    Init_lexer();
    Init_G_Caches();
    Init_io();
    Init_tcl();
    Init_symbol();
    G_Init();
    Parse_Init();
    Sat_Init();
    BE_Init();
    Strings_Init();
    System_Init();
    Int_ops_Init();
    List_ops_Init();
    Signature_Init();
    Float_Init();
    Bv_Init();
    Bev_Init();
    LP_Init();
    Fsm_Init();
    Init_SHA256();
    Table_Init();
    Image_Init();

    // Now install builtin functions
    Symbols_Install_Functions();
    Draw_Graph_Install_Functions();
    BDD_Install_Functions();
    Bexpr_Install_Functions();
    Io_Install_Functions();
    Strings_Install_Functions();
    System_Install_Functions();
    Int_ops_Install_Functions();
    List_ops_Install_Functions();
    Signature_Install_Functions();
    Cache_ops_Install_Functions();
    Float_Install_Functions();
    Bv_Install_Functions();
    Bev_Install_Functions();
    LP_Install_Functions();
    Fsm_Install_Functions();
    Table_Install_Functions();
    Image_Install_Functions();
}

