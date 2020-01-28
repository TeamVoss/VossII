proc process_is_alive {process_id} {
    if [catch {set rl [split [exec ps -o s $process_id] "\n"]}] {
        return 0
    }
    if { [llength $rl] != 2 } { return 0 }
    set status [lindex $rl 1]
    if { $status == "R" } { return 1 }
    if { $status == "S" } { return 1 }
    if { $status == "D" } { return 1 }
    if { $status == "T" } { return 1 }
    return 0
}

proc watch_process {pid {action "destroy ."} } {
    if [process_is_alive $pid] {
        after 200 [list watch_process $pid]
    } else {
        eval $action
    }
}



set f_stdin /tmp/cjhseger_stdin
set f_stdout /tmp/cjhseger_stdout

exec /usr/bin/touch $f_stdout
exec /bin/rm -f $f_stdout
exec /usr/bin/touch $f_stdout

set fp_stdin [open "|./fl >>& $f_stdout" w]
set fl_pid [pid $fp_stdin]
watch_process $fl_pid

set fp_stdout [open $f_stdout r]
fconfigure $fp_stdout -blocking 0
fconfigure stdin -blocking 0

proc read_fl_out {} {
    set out [read $::fp_stdout]
    if {[llength $out] > 0 } {
        puts -nonewline $out
        flush stdout
    }
    after 100
}
fileevent $fp_stdout readable read_fl_out

proc read_stdin {} {
    set line [read stdin]
    puts -nonewline $::fp_stdin $line
    flush $::fp_stdin
}

fileevent stdin readable read_stdin

