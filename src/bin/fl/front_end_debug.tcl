set auto_path [linsert $auto_path 0 $::env(FL_PRODEBUG_AUTOPATH)]
package require remotedebug


debugger_init localhost 2576
debugger_eval {
    source [string map {front_end_debug front_end} $argv0]
}

