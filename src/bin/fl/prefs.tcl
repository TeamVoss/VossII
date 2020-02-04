;#########################################################################
;# Copyright 2020 Carl-Johan Seger
;# SPDX-License-Identifier: Apache-2.0
;#########################################################################

set DIR [file dirname [file normalize [info script]]]

set voss2_preferences {
    {"Display" "" "" "header" ""}
    {"TEXT-FONT" "RCtext_font"
	"Font used for text widgets" "enum"
	"-*-courier-bold-r-normal-*-14-*-*-*-*-*-*-* -*-courier-bold-r-normal-*-10-*-*-*-*-*-*-* -*-courier-bold-r-normal-*-12-*-*-*-*-*-*-* -*-courier-bold-r-normal-*-16-*-*-*-*-*-*-* -*-courier-bold-r-normal-*-18-*-*-*-*-*-*-* -*-courier-bold-r-normal-*-20-*-*-*-*-*-*-*"}

    {"Runtime" "" "" "header" ""}
    {"RECURSION-CALL-LIMIT" "RCcall_limit"
	"Maximum recursion limit" "int" "10000"}

    {"BDD" "" "" "header" ""}
    {"DYNAMIC-ORDERING" "RCdo_dynamic_var_order"
        "Perform dynamic ordering" "bool" "TRUE"}
    {"DYNAMIC-ORDERING-THRESHOLD" "RCdynamic_ordering_threshold"
	"Minimum size of BDD to consider reordering" "int" "20000"}
    {"OPTIMAL-DYNAMIC-ORDERING" "RCoptimal_dynamic_var_order"
        "Try to find optimal ordering" "bool" "TRUE"}
    {"MINIMUM-SIZE-FOR-DYNAMIC-ORDERING" "RCminsize_for_dyn_ordering"
	"Minimum size to continue to re-order" "int" "0"}
    {"DYNAMIC-ORDERING-REPETITIONS" "RCdyn_var_repetitions"
        "Times to repeat a full sweep" "int" "1"}
    {"ELASTICITY-IN-DYNAMIC-ORDERING" "RCelasticity"
        "Ratio to start reordering" "int" "2"}
    {"MIN-RECOVERY-IN-DYNAMIC-ORDERING" "RCminimum_reduction"
        "Minimum recovery to continue reordering" "int"  "100"}

    {"Bexprs" "" "" "header" ""}
    {"BEXPR-ORDER-AND-ARGS" "RC_reorder_bexpr_ands"
        "Reorder the argumanet to ANDs to make bexrs more similar"
	"bool" "FALSE"}
    {"BEXPR-ADD-REDUNDANT-TERMS" "RC_add_redundant_terms"
        "Add redundatn terms to make the ternary extensions less pessimistic"
	"bool" "FALSE"}
    {"BEXPR-MAX-PRINT-DEPTH" "RCmax_bexpr_print_depth"
	"Maximum size of bexpr to print" "int" "5"}
    {"BEXPR-SAT-TIME-LIMIT" "RC_BEXPR_SAT_LIMIT"
	"Maximum time used to make bexpr canonical" "int" "5"}

    {"STE" "" "" "header" ""}
    {"DELAY-MODEL" "RCdel_model"
        "Delay model used for simulation" "enum"
        "UNIT-DELAY MINIMUM-DELAY MAXIMUM-DELAY AVERAGE-DELAY BOUNDED-DELAY"}
    {"NOTIFY-OK_A-FAILURES" "RCnotify_OK_A_failures"
        "Notify internal failures in antecedent" "bool" "TRUE"}
    {"NOTIFY-OK_C-FAILURES" "RCnotify_OK_C_failures"
        "Notify internal failures in consequent" "bool" "TRUE"}
    {"NOTIFY-TRAJECTORY-FAILURES" "RCnotify_traj_failures"
        "Notify final failures in antecedent" "bool" "TRUE"}
    {"NOTIFY-CHECK-FAILURES" "RCnotify_check_failures"
        "Notify final failures in consequent" "bool" "TRUE"}
    {"PRINT-FAILURE-FORMULA" "RCprint_failures"
        "Print high-low expression for failing nodes" "bool" "TRUE"}
    {"PRINT-TIME" "RCprint_time"
        "Print time points during simulation" "bool" "TRUE"}
    {"STEP-LIMIT" "RCStep_limit"
        "Maximum number of simulation steps" "int" "100"}

    {"Verbosity" "" "" "header"  ""}
    {"VERBOSE-GARBAGE-COLLECTION" "RCverbose_GC"
        "Print out information about g.c." "bool"  "FALSE"}
    {"VERBOSE-DYNAMIC-ORDERING" "RCverbose_dynamic_ordering"
        "Print out information about dynamic BDD reordering" "bool"  "TRUE"}
    {"MAXIMUM-NUMBER-ERRORS-REPORTED" "RCmax_nbr_errors"
        "Limit number of error messages for a single command" "int" "5"}
    {"VERBOSE-EVAL-COMMAND" "RCverbose_eval_command"
	"Treat 'eval' as top-level input w.r.t. output" "bool" "TRUE"}
    {"PRINT_ALIASES" "RCprint_aliases_flag"
        "Print aliases in addition to base node name" "bool" "TRUE"}
    {"PRINT-FORMAT" "RCpr_str"
        "Format to print BDDs in" "enum"  "SOP INFIX TREE"}
    {"MAX-PRODUCTS-IN-SOP-TO-PRINT" "RCmax_prods_to_print"
        "Maximum number of products to print" "int"  "5"}
    {"MAX-ENTRIES-IN-STACK-TRACE-TO-PRINT" "RCmax_stack_trace_entries" 
	"Maximum number of stack trace entries printed" "int" "40"}
    {"VERBOSE-FSM-PRINT" "RCverbose_fsm_print"
        "Print out detailed structure of fsm" "bool"  "FALSE"}
    {"VERBOSE-STE-RUN" "RCverbose_ste_run"
        "Print out every time a traced node change during STE" "bool"  "FALSE"}
    {"VERBOSE-IMAGE-PRINT" "RCverbose_image_print"
        "Print out detailed image" "bool"  "FALSE"}

    {"System" "" "" "header"  ""}
    {"TMP_FILE_DIR" "RC_TMP_FILE_DIR"
        "Directory in which temporary directory will be created"
        "string" "/tmp"}
};

lappend voss2_preferences [list "VOSS-LIBRARY-DIRECTORY" "RCDefault_dir" \
        "Search path for .fl files" "string" ""]
lappend voss2_preferences [list "VOSS-BINARY-DIRECTORY" "RCBinary_dir" \
                               "Search path for Voss binaries" "string" ""]

proc generate_C_datatype {} {
    set fp [open "prefs.h" "w"]
    foreach line $::voss2_preferences {
        lassign $line name cname description type vals
        switch $type {
            bool    { puts $fp "bool   $cname;" }
            int     { puts $fp "int    $cname;" }
            string  { puts $fp "string $cname;" }
            enum    { puts $fp "string $cname;" }
            default {}
        }
    }
    puts -nonewline $fp "\n\nstatic vossrc_rec default_vossrc\[\] = {\n\t"
    set sep ""
    foreach line $::voss2_preferences {
        lassign $line name cname description type vals
        if { $type == "header" } {
            set cn "NULL"
        } else {
            set cn "&$cname"
        }
        switch $type {
            header  { set ctype RC_HEADER }
            bool    { set ctype RC_BOOL }
            int     { set ctype RC_INT }
            string  { set ctype RC_STRING }
            enum    { set ctype RC_ENUM }
        }
        puts -nonewline $fp [format {%s%s"%s", %s, "%s", %s, "%s"%s} \
                            $sep "{" $name $cn $description $ctype $vals "}"]
        set sep ",\n\t"
    }
    puts $fp ",\n\t{ }\n};\n\n"
    close $fp

    set fp [open "prefs_ext.h" "w"]
    foreach line $::voss2_preferences {
        lassign $line name cname description type vals
        switch $type {
            bool    { puts $fp "extern bool   $cname;" }
            int     { puts $fp "extern int    $cname;" }
            string  { puts $fp "extern string $cname;" }
            enum    { puts $fp "extern string $cname;" }
            default {}
        }
    }
    close $fp
}

