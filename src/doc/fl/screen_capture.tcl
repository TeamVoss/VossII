;#########################################################################
;# Copyright 2020 Carl-Johan Seger
;# SPDX-License-Identifier: Apache-2.0
;#########################################################################

proc screen_capture:do_cmd {cmd} {
    WriteStdIn $cmd
    WriteNewLine {}
    catch {send_top_voss2_cmd $cmd}
    update
}

proc screen_capture:do_cmds {cmds} {
    clean_voss2_window all
    WriteStdOut ": "
    set idx 0
    set start $idx
    set inquote 0
    set inslash 0
    set len [string length $cmds]
    while { $idx < $len } {
	set c [string range $cmds $idx $idx]
	switch  $c {
	    {;}	{
		    if { $inslash == 0 } {
			if {$inquote == 0} {
			    set cmd [string range $cmds $start $idx]
			    screen_capture:do_cmd [string trimleft $cmd]
			    set start [expr $idx+1]
			}
		    }
		    set inslash 0
		}
	    "\\" {
		    set inslash 1
		}
	    {"}	{
		    if { $inslash == 0 } {
			set inquote [expr 1-$inquote]
		    }
		    set inslash 0
		}
	    default {
		    set inslash 0
		}
	}
	incr idx
    }
    set cmd [string range $cmds $start $idx]
    if { [string length $cmd] != 0 } {
	screen_capture:do_cmd [string trimleft $cmd]
    }
}

proc screen_capture:do_full_window_capture {ofile} {
    set wid [winfo id .]
    exec import -window $wid $ofile
}

proc screen_capture:do_capture {ofile} {
    set wid [winfo id .]
    set y_sz [expr [.voss2.t count -update -ypixels 1.0 end] + 35] 
    exec import -window $wid png:- | convert -crop x$y_sz+0+0 png:- $ofile
}


.voss2.t configure -height 59
