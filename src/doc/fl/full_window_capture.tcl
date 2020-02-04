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
    set len [string length $cmds]
    while { $idx < $len } {
	set c [string range $cmds $idx $idx]
	switch  $c {
	    {;}	{
		    if {$inquote == 0} {
			set cmd [string range $cmds $start $idx]
			screen_capture:do_cmd [string trimleft $cmd]
			set start [expr $idx+1]
		    }
		}
	    {"}	{
		    set inquote [expr 1-$inquote]
		}
	    default { }
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

proc screen_capture:show_help_example {pat idx} {
    ::voss2_help::help_start
    set w $::voss2_top_level.voss2_help
    set w_search_name_pat       $w.nb.f.p.w.name
    set w_search_alts           $w.nb.f.p.w.alts
    set w_help_tab_notebook     $w.nb.f.p.e
    $w_search_name_pat.entry delete 0 end
    $w_search_name_pat.entry insert 0 $pat
    ::voss2_help::help_do_match $w_search_alts.list
    $w_search_alts.list selection set $idx
    ::voss2_help::help_show_info $w_search_alts.list $w_help_tab_notebook
}
