;#########################################################################
;# Copyright 2020 Carl-Johan Seger
;# SPDX-License-Identifier: Apache-2.0
;#########################################################################

#
# Get the arguments that establish the process link to fl
#
set fl_pid          [lindex $argv 0]
set socket_addr     [lindex $argv 1]
set socket_port     [lindex $argv 2]
set tmp_dir         [lindex $argv 3]

# Get the directory this file is in
set DIR  [file dirname [file normalize [info script]]]

# Load the vossrc preferences
source "$DIR/prefs.tcl"

# Load misc. utilities
source "$DIR/utils.tcl"

# Top-level stuff
wm title . "VossII"
wm iconname . "VossII"
wm minsize . 1 1

set ::voss2_bcolor	#fff0b0

# Load user VossII Xdefaults if it exists
if {! [catch {glob "~/.VossII.Xdefaults"} x_resources]} {
    option readfile $x_resources 
}

# Make sure we use a 72dpi independent of the display
tk scaling 1.0

# Counter for return results for fl callbacks
set ::result_id 0

set my_pid [pid]
watch_process $fl_pid [list exec kill -9 $my_pid]

set x_selection         ""

proc provide_selection {offset maxbytes} {
    return [string range $::x_selection $offset [expr $offset+$maxbytes-1]]
}

proc set_selection {w name} {
    global x_selection
    set x_selection $name
    selection clear
    selection own .
}

# Protect all characters in msg from tcl interpretation by
# translating them to their unicode versions
proc protect {msg} {
    set bad_tcl_chars_RE {[][{};#\"\\\$ \r\t\u0080-\uffff]}
    set bad_tcl_chars_substitution {[format \\\\u%04x [scan "\\&" %c]]}
    set msg2 [subst [string map {\n {\\u000a}} \
          [regsub -all $bad_tcl_chars_RE $msg $bad_tcl_chars_substitution]]]
    return $msg2
}

set ::tr(u0022) {"}
set ::tr(u005b) {[}
set ::tr(u005c) "\\"
set ::tr(u005d) {]}
set ::tr(u007b) "{"
set ::tr(u007d) "}"
set ::tr(u003b) {;}
set ::tr(u0023) {#}
set ::tr(u0024) {$}
set ::tr(u0020) { }
set ::tr(u0009) "\t"
set ::tr(u000a) "\n"


# Unprotect all characters in msg by translating them from
# their unicode versions to normal characters
proc unprotect {txt} {
    set res ""
    set cnt 0
    foreach c [split $txt ""] {
        if { $cnt == 0 } {
            if { $c == "\\" } {
                set cnt 1
                set uc ""
            } else {
                append res $c
            }
        } else {
            if { $cnt <= 4 } {
                append uc $c
                incr cnt
            } else {
                append uc $c
                append res [set ::tr($uc)]
                set cnt 0
            }
        }
    }
    return $res
}

# Remove backslahes, unless they are a pair of backslahes
proc clean_backslashes {txt} {
    set res ""
    set seen 0
    foreach c [split $txt ""] {
	if { $seen == 1 } {
	    if { $c == "{" || $c == "}" } {
		append res $c
	    } else {
		append res "\\"
		append res $c
	    }
	    set seen 0
	} else {
	    if { $c == "\\" } {
		set seen 1
	    } else {
		append res $c
		set seen 0
	    }
	}
    }
    return $res
}


proc send_top_voss2_cmd {tinp} {
    if { [string length $tinp] > 1000 } {
	set fp [file tempfile tmp_file_name [format {%s/inp_XXXXXX} $::tmp_dir]]
	puts $fp $tinp
	close $fp
	puts $::cmd_from_tcl_fp 1
	flush $::cmd_from_tcl_fp
	puts $::cmd_from_tcl_fp [format {load "%s";} $tmp_file_name]
	flush $::cmd_from_tcl_fp
	tkwait variable ::return_result_from_cmd
	exec /bin/rm -f $tmp_file_name
	return $::return_result_from_cmd
    } else {
	set inps [split $tinp "\n"]
	set l [llength $inps]
	puts $::cmd_from_tcl_fp $l
	flush $::cmd_from_tcl_fp
	foreach inp $inps {
	    puts $::cmd_from_tcl_fp [protect $inp]
	    flush $::cmd_from_tcl_fp
	}
	tkwait variable ::return_result_from_cmd
	return $::return_result_from_cmd
    }
}

proc voss2_interrupt {} {
    exec kill -s INT $::fl_pid
}

# Called from fl
proc voss2_interrupt_action {} {
    set w .voss2_interrupt
    catch {destroy $w}
    toplevel $w

    wm title $w "VossII interrupt"
    wm iconname $w "VossII interrupt"
    regexp {[0-9]*x[0-9]*\+([0-9]*)\+([0-9]*)} [winfo geometry .] --> dx dy
    wm geometry $w +$dx+$dy

    label $w.l -text Interrupt
    pack $w.l -side top -pady 4 -fill x -expand yes

    button $w.c -text "Continue" -command {set ::interrupt_choice c}
    pack $w.c -side top -pady 2 -fill x -expand yes

    button $w.s -text "Stack trace" -command {set ::interrupt_choice s}
    pack $w.s -side top -pady 2 -fill x -expand yes

    button $w.x -text "Exit VossII" -command {set ::interrupt_choice x}
    pack $w.x -side top -pady 2 -fill x -expand yes

    button $w.r -text "Return to top" -command {set ::interrupt_choice r}
    pack $w.r -side top -pady 2 -fill x -expand yes

    $::voss2_info(intr) configure -state disable
    $::voss2_info(txtwin) configure -state disable

    update idletasks
    tkwait variable ::interrupt_choice
    destroy $w

    $::voss2_info(intr) configure -state normal
    $::voss2_info(txtwin) configure -state normal
    
    return $::interrupt_choice
}


proc reorder_start {nbr_vars start_size} {
    set w $::voss2_top_level.reorder
    catch {destroy $w} 
    if [info exists ::voss2_status(hidden_fl_window)] {
	toplevel $w
    } else {
	frame $w 
    }
        label $w.l -text "BDD re-ordering"
        ttk::progressbar $w.p -orient horizontal -length 150 -mode determinate \
            -maximum $nbr_vars.0
        label $w.sl -text [format {Start size: %d} $start_size]
        label $w.cvl -text "Current size: "
        set ::reorder_current_size $start_size
        label $w.cv -textvariable ::reorder_current_size
        button $w.quit -text Abort -command reorder_abort
	make_window_always_alive $w.quit
        pack $w.l -side left
        pack $w.p -side left -padx 2
        pack $w.sl -side left -padx 2
        pack $w.cvl -side left -padx 2
        pack $w.cv -side left -padx 2
        pack $w.quit -side left -padx 2 -fill x -expand y
    if [info exists ::voss2_status(hidden_fl_window)] {
	wm geometry $w 600x40+10+10
    } else {
	pack $w -side bottom -before $::voss2_info(txtwin) -fill x
    }
    update
    $::voss2_info(txtwin) see end
}

proc reorder_abort {} {
    voss2_interrupt
}

proc reorder_end {} {
    set w $::voss2_top_level.reorder
    catch {destroy $w}
    update
    $::voss2_info(txtwin) see end
}

proc reorder_update {cur_size} {
    set w $::voss2_top_level.reorder
    $w.p step
    set ::reorder_current_size $cur_size
    update
}

# Make sure every way of killing voss2 (except kill -9) uses a call to quit.
# That way it suffices to re-define quit to allow clean-up operations
rename destroy voss2_destroy
proc destroy {args} {
    foreach w $args {
        if { $w != "." } {
            voss2_destroy $w
        } else {
            send_top_voss2_cmd "quit;"   
        }
    }
}
wm protocol . WM_DELETE_WINDOW {destroy .}

# -------------------------------------------------------------------------
# Menu items
# -------------------------------------------------------------------------

# top-level frame variables
set voss2_top_level		""
set voss2_info(menu)	""
set voss2_info(txtwin)	""
set voss2_info(txtscroll)	""
set voss2_info(intr)	""


proc clean_range {tags} {
    set t $::voss2_info(txtwin)
    foreach tag $tags {
        set ranges [$t tag ranges $tag]
        if { $ranges == {} } { return }
        eval $t delete $ranges
    }
}


proc clean_voss2_window {{type all}} {
    set t $::voss2_info(txtwin)
    switch $type {
        stdin    { clean_range stdin_color }
        stdout   { clean_range stdout_color }
        stderr   { clean_range "stderr_color inp_err_color" }
        gc       { clean_range "flgc_color bddgc_color" }
        trace    { clean_range "trace_color" }
        warnings { clean_range "warning_color" }
        all      { $t delete 1.0 end }
        default  {}
    }
}

proc voss2_load_file {} {
    set types {
        {{VossII Files}      {.fl}        }
        {{All Files}        *             }
    }
    set filename [tk_getOpenFile -filetypes $types -title "File to load"]
    if {$filename ne ""} {
        send_top_voss2_cmd "load \"$filename\";"
    }
}

proc save_file {file} {
    if [catch {open $file w} fp] {
	report_error "Cannot save script in file $file."
	return
    }
    set stdin_tag_list [$::voss2_info(txtwin) tag ranges stdin_color]
    set l [expr [llength $stdin_tag_list] / 2 ]
    for {set i 0} {$i < $l} {incr i} {
	set idx1 [lindex $stdin_tag_list [expr 2*$i]]
	set idx2 [lindex $stdin_tag_list [expr 2*$i+1]]
	puts $fp  [$::voss2_info(txtwin) get $idx1 $idx2]
    }
    close $fp
}

proc voss2_save_file {} {
    set types {
        {{VossII Files}      {.fl}        }
        {{All Files}        *             }
    }
    set filename [tk_getSaveFile -filetypes $types -defaultextension ".fl" \
                                 -title "File to save to"]
    if {$filename ne ""} {
        save_file $filename
    }
}

#
# create top level menu:
#   $tm - frame
#   $tm.file - file button
#   $tm.help - help button
#   $tm.intr - interrupt button
#
proc create_voss2_top_level_menu {{tm ".voss2.menu"}} {
    # create menu frame, if it doesn't exist yet ...
    if { ! [winfo exists $tm] } {
	frame $tm
	pack $tm -side top -fill x
    }

    # create menu entries
    menubutton $tm.file -text File -borderwidth 1 -menu $tm.file.m
    menu $tm.file.m -tearoff 0
    $tm.file.m add command -label "About VossII" \
	    -command {about_voss2} -underline 0
    $tm.file.m add command -label "Preferences" \
	    -command "set_preferences" -underline 0

    # Add a menu for changing the font size for all windows
    set fm $tm.file.m.fm
    $tm.file.m add cascade -label "Font Size" -menu $fm
    menu $fm -tearoff 0
    $fm add command -label "Tiny" -command "change_fonts $::voss2_txtfont0"
    $fm add command -label "Small" -command "change_fonts $::voss2_txtfont1"
    $fm add command -label "Normal" -command "change_fonts $::voss2_txtfont2"
    $fm add command -label "Large" -command "change_fonts $::voss2_txtfont3"
    $fm add command -label "Larger" -command "change_fonts $::voss2_txtfont4"
    $fm add command -label "Largest" -command "change_fonts $::voss2_txtfont5"

    # Add a menu for cleaning the VossII text window
    set cm $tm.file.m.cm
    $tm.file.m add cascade -label "Clean VossII window" -menu $cm
    menu $cm -tearoff 0
    $cm add command -label "Input text" -command "clean_voss2_window stdin"
    $cm add command -label "Output text" -command "clean_voss2_window stdout"
    $cm add command -label "Error text" -command "clean_voss2_window stderr"
    $cm add command -label "Warning text" -command "clean_voss2_window warnings"
    $cm add command -label "G.C. messages" -command "clean_voss2_window gc"
    $cm add command -label "STE trace messages" \
	    -command "clean_voss2_window trace"
    $cm add command -label "All" -command "clean_voss2_window all"

    $tm.file.m add command -label "Load" \
	    -command {voss2_load_file} -underline 0
    $tm.file.m add command -label "Save" \
	    -command {voss2_save_file} -underline 0
    $tm.file.m add command -label "Quit VossII" \
	    -command {send_top_voss2_cmd "quit;"} -underline 0
    pack $tm.file -side left

    # create help button
    button $tm.help -text "Help" -command {::voss2_help::help_start}
    pack $tm.help -padx 2 -side right

    # create interrupt button
    button $tm.intr -text Interrupt -command voss2_interrupt
    pack $tm.intr -padx 2 -side left -fill both -expand 1
    make_window_always_alive $tm.intr

    set ::voss2_info(menu) $tm
    set ::voss2_info(intr) $tm.intr
}

set ::voss2_current_cursor ""


set shift_release_time 0

proc detect_shift {current_time} {
    set delta [expr {$current_time - $::shift_release_time}]
    return [expr {($delta < 2) && ($delta >= 0)}]
}

proc create_voss2_top_level_txtwin {{t ".voss2.t"} {s ".voss2.s"}} {

    # create scroll bar
    if { ! [winfo exists $s] } {
	scrollbar $s -command "$t yview"
	pack $s -side right -fill both
	make_window_always_alive $s
    }

    # create text window
    if { ! [winfo exists $t] } {
	text $t -relief sunken -bd 2 -yscrollcommand "$s set" \
		-background $::voss2_bcolor -foreground green \
		-font $::voss2_txtfont
	pack $t -side left -fill both -expand 1

	$t tag configure trace_color   -foreground grey
	$t tag configure stdin_color   -foreground blue
	$t tag configure info_color    -elide yes
	$t tag configure stdout_color  -foreground black
	$t tag configure stderr_color  -foreground red
	$t tag configure warning_color -foreground orange
	$t tag configure inp_err_color -foreground pink
	$t tag configure flgc_color    -foreground #007c00
	$t tag configure bddgc_color   -foreground #00ac00
    }

    set ::voss2_info(txtwin) $t
    set ::voss2_info(txtscroll) $s

    # Create a binding to forward commands to voss2
    # Make sure only the write-
    # plus modify many of the built-in bindings so that only information
    # in the current command can be deleted

    bindtags $t [list $t . all Text];

    bind $t <Enter> {
	catch {focus %W}
    }

    bind $t <KeyPress> {
	if { $::voss2_current_cursor == "watch" } {
	    # disable keyboard input when busy ...
	    break;
	}

	::voss2_history::reset_history
	catch {%W tag lower readonly}
	if { [string match [lindex [%W tag names insert] 0] readonly] != 0 } {
	    %W mark set insert {end - 1c}
	}
	%W see insert
    }

    bind $t <Prior> {
	::voss2_history::get_history_cmd -1
	break
    }

    bind $t <Next> {
	::voss2_history::get_history_cmd 1
	break
    }

    bind $t <ButtonRelease-2> {
	%W tag lower readonly
	if { [string match [lindex [%W tag names insert] 0] readonly] != 0 } {
	    %W mark set insert {end - 1c}
	}
	catch {%W insert insert [selection get]}
	%W see insert
	break;
    }

    set ::voss2_selection ""

    # cut selection
    bind $t <Control-x> {
	catch {set ::voss2_selection [selection get]}
	catch {%W tag remove sel sel.first promptEnd}
	if {[%W tag nextrange sel 1.0 end] == ""} {
	    if [%W compare insert <= promptEnd] {
		break
	    }
	}
    }

    # copy selection
    bind $t <Control-c> {
	catch {set ::voss2_selection [selection get]}
    }

    # paste selection
    bind $t <Control-v> {
	catch {%W insert insert $::voss2_selection}
	%W see insert
	break
    }

    # insert newline at cursor
    bind $t <Shift-Return> {
	if { $::voss2_current_cursor == "watch" } {
	    # disable keyboard input when busy ...
	    break;
	}
	::voss2_history::reset_history
	%W insert insert \n
	$::voss2_info(txtwin) yview -pickplace insert
	break
    }

    # Run command
    bind $t <Return> {
	if { $::voss2_current_cursor == "watch" } {
	    # disable keyboard input when busy ...
	    break;
	}
	::voss2_history::reset_history
	$::voss2_info(txtwin) mark set insert end
	catch invoke
	break
    }

    bind $t <Delete> {
	catch {%W tag remove sel sel.first promptEnd}
	if {[%W tag nextrange sel 1.0 end] == ""} {
	    if [%W compare insert < promptEnd] {
		break
	    }
	}
    }

    bind $t <BackSpace> {
	catch {%W tag remove sel sel.first promptEnd}
	if {[%W tag nextrange sel 1.0 end] == ""} {
	    if [%W compare insert <= promptEnd] {
		break
	    }
	}
    }

    # Reset input 
    bind $t <Control-u> {
	set cmd_start [%W index promptEnd]
	set cmd_end [%W index end]
	%W delete $cmd_start $cmd_end
    }

    # Delete from cursor to end of line
    bind $t <Control-k> {
	if [%W compare insert < promptEnd] {
	    %W mark set insert promptEnd
	}
    }

    # Swap chars
    bind $t <Control-t> {
	if [%W compare insert < promptEnd] {
	    break
	}
    }

    bind $t <Meta-d> {
	if [%W compare insert < promptEnd] {
	    break
	}
    }
    bind $t <Meta-BackSpace> {
	if [%W compare insert <= promptEnd] {
	    break
	}
    }

    # Select all
    bind $t <Control-a> {
        tk::TextSetCursor %W promptEnd
        tk::TextKeySelect %W {end -1c}
       break
    }

    # Select current word
    bind $t <Control-w> {
	tk::TextSetCursor %W {insert +1c}
	tk::TextSetCursor %W [tk::TextPrevPos %W insert tcl_startOfPreviousWord]
	tk::TextKeySelect %W [tk::TextNextWord %W insert]
	break
    }

    bind $t <Home> {
	if {[%W compare {insert linestart} < promptEnd]} {
	    tk::TextSetCursor %W promptEnd
	    break;
	}
    }

    bind $t <Control-Home> {
	tk::TextSetCursor %W promptEnd
	break
    }

    bind $t <Shift-Home> {
	if {[%W compare {insert linestart} < promptEnd]} {
	    tk::TextKeySelect %W promptEnd
	    break
	}
    }

    bind $t <Control-Shift-Home> {
	tk::TextKeySelect %W promptEnd
	break
    }

    bind $t <Shift-Up> {
	set loc [tk::TextUpDownLine %W -1]
	if [%W compare $loc <= promptEnd] {
	    set loc promptEnd
	}
	tk::TextKeySelect %W $loc
	break

    }

    bind $t <Up> {
	set loc [tk::TextUpDownLine %W -1]
	if [%W compare $loc <= promptEnd] {
	    set loc promptEnd
	}

	tk::TextSetCursor %W $loc
	break
    }

    bind $t <Shift-Left> {
	if [%W compare insert <= promptEnd] {
	    break
	}
    }

    bind $t <Left> {
	if [%W compare insert <= promptEnd] {
	    %W tag remove sel 1.0 end
	    break;
	}
    }

    bind $t <Shift-Right> {
	if [%W compare insert >= [%W index "end -1c lineend"]] {
	    break;
	}
    }

    # bind keypad events to regular events
    bind $t <KeyRelease-Shift_L> {
	set shift_release_time %t
    }

    bind $t <KeyRelease-Shift_R> {
	set shift_release_time %t
    }

    bind $t <Control-KP_Left> {
	if {[detect_shift %t]} {
	    event generate %W <Control-Shift-Left>
	} else {
	    event generate %W <Control-Left>
	}
    }

    bind $t <KP_Left> {
	if {[detect_shift %t]} {
	    event generate %W <Shift-Left>
	} else {
	    event generate %W <Left>
	}
    }

    bind $t <Control-KP_Right> {
	if {[detect_shift %t]} {
	    event generate %W <Control-Shift-Right>
	} else {
	    event generate %W <Control-Right>
	}
    }

    bind $t <KP_Right> {
	if {[detect_shift %t]} {
	    event generate %W <Shift-Right>
	} else {
	    event generate %W <Right>
	}
    }

    bind $t <KP_Up> {
	if {[detect_shift %t]} {
	    event generate %W <Shift-Up>
	} else {
	    event generate %W <Up>
	}
    }

    bind $t <KP_Down> {
	if {[detect_shift %t]} {
	    event generate %W <Shift-Down>
	} else {
	    event generate %W <Down>
	}
    }

    bind $t <Control-KP_Home> {
	if {[detect_shift %t]} {
	    event generate %W <Control-Shift-Home>
	} else {
	    event generate %W <Control-Home>
	}
    }
 
    bind $t <KP_Home> {
	if {[detect_shift %t]} {
	    event generate %W <Shift-Home>
	} else {
	    event generate %W <Home>
	}
    }

    bind $t <Control-KP_End> {
	if {[detect_shift %t]} {
	    event generate %W <Control-Shift-End>
	} else {
	    event generate %W <Control-End>
	}
    }

    bind $t <KP_End> {
	if {[detect_shift %t]} {
	    event generate %W <Shift-End>
	} else {
	    event generate %W <End>
	}
    }

    bind $t <KP_Enter> [bind $t <Return>]
    bind $t <Shift-KP_Enter> [bind $t <Shift-Return>]

    bind $t <KP_Prior> [bind $t <Prior>]
    bind $t <KP_Next>  [bind $t <Next>]
    bind $t <KP_Delete> {
	event generate %W <Delete>
    }

    bind $t <Control-KP_Up> [bind $t <Control-Up>]
    bind $t <Control-KP_Down> [bind $t <Control-Down>]

    ## Keypad bindings end

    update
}





#
# really create UI
#
set voss2_top_level ".voss2"
frame $voss2_top_level
pack $voss2_top_level -fill both -expand 1
create_voss2_top_level_menu "$voss2_top_level.menu"
frame ".voss2.toolbar"
pack ".voss2.toolbar" -fill both  
create_voss2_top_level_txtwin "$voss2_top_level.t" "$voss2_top_level.s"

#-----------------------------------------------------------------------

proc bgerror {msg} {
    # Release grabs
    if {[grab current .] != ""} {
	grab release [grab current .]
    }
    if { $msg == "FL interrupt" } {
	# Ignore FL interrupts
	return
    }

    set info $::errorInfo

    # Reset the cursor and send error to stderr
    puts stderr [concat Error: $msg]
    puts stderr [concat Stack trace: $info]

    switch [tk_dialog .error "Error in Fl/tcl/tk script" \
		    "Error: $msg" error 0 Ok Details ] {
	0	{ voss2_set_cursor {} ;  return }
    }

    set w .errorMsg
    catch {destroy $w}
    toplevel $w
    wm minsize $w 1 1
    wm title $w "Error Stack Trace"
    wm iconname $w "Stack Trace"
    button $w.ok -text OK -command "destroy $w"
    text $w.text -relief sunken -bd 2 -yscrollcommand "$w.scroll set" \
		-setgrid 1 -width 80 -height 20
    scrollbar $w.scroll -relief sunken -command "$w.text yview"

    $w.text insert 0.0 "Error: $msg\nStack trace:\n$info"
    $w.text mark set insert 0.0
    pack $w.ok -side bottom -padx 3m -pady 2m
    pack $w.scroll -side right -fill y
    pack $w.text -side left -expand yes -fill both

    # Center the window

    wm withdraw $w
    update idletasks
    set x [expr [winfo screenwidth $w]/2 - [winfo vrootx [winfo parent $w]] -\
		[winfo reqwidth $w]/2]
    set y [expr [winfo screenheight $w]/2 - [winfo vrooty [winfo parent $w]] -\
		[winfo reqheight $w]/2]
    wm geom $w +$x+$y
    wm deiconify $w

    tkwait window $w

    return
}

proc report_error {msg} {
    tk_dialog .err_msg ERROR99 $msg error 0 Ok
}

proc about_voss2 {} {
    set w $::voss2_top_level.about
    toplevel $w
    wm title $w "voss2: about"
    wm iconname $w "voss2: about"
    wm geometry $w +50+20
    message $w.msg -width 15c -justify left -relief raised \
	-text \
"VossII is based on Voss and uses the fl language.\
fl is a general purpose lazy functional language\
with BDDs, SAT solver, symbolic trajectory evaluation, etc. built in.
The language and this verification system has been in development since 1990 \
by Carl Seger. VossII is a branch from the Voss system similar to the Forte \
branch that has been in active use inside Intel for more than 20 years."
    button $w.ok -text Ok -command "destroy $w"
    pack $w.msg $w.ok -side top
}

variable last_output_color "stdout_color";
variable help_tag_idx	   0;

proc WriteOutput {color txt} {
    variable last_output_color
    variable help_tag_idx

    set t $::voss2_info(txtwin)

    set txt [clean_backslashes $txt]

    if { [string compare -length 4 $color "help_"] == 0 } {
	# $color is not really a color but the name of a command ... which
	# we'll also use as the name of the bind tag.
	set cmd $color
	set tag "help_tag_$help_tag_idx"
	incr help_tag_idx

	$t tag bind $tag <Button-1> "$cmd"
	$t tag configure $tag -underline 1
	$t insert end $txt [list [list $tag $last_output_color readonly]]
    } else {
	if { $color == "" } {
	    set color stdout_color
	}
	set last_output_color $color
	$t insert end "$txt" [list $color readonly]
    }
    $t mark set insert {end-1c}
    $t mark set promptEnd {insert}
    $t mark gravity promptEnd left
    $t see end
}

proc WriteSim {txt} {
    WriteOutput "trace_color" $txt
}

proc WriteFlGC {txt} {
    WriteOutput "flgc_color" $txt
}

proc WriteBddGC {txt} {
    WriteOutput "bddgc_color" $txt
}

proc WriteStdOut {txt} {
    WriteOutput "stdout_color" $txt
}

proc WriteStdIn {txt} {
    WriteOutput "stdin_color" $txt
}

proc WriteNewLine {txt} {
    WriteOutput "stdout_color" "\n"
}

proc WriteInfo {line} {
    WriteOutput "info_color" $line
}

proc WriteStdErr {txt} {
    WriteOutput "stderr_color" $txt
}

proc WriteWarning {txt} {
    WriteOutput "warning_color" $txt
}

proc skip_info_idx {t old_start} {
    set l1 [$t search -elide "set_line_number " $old_start {end}]
    if { $l1 == "" } { return $old_start }
    set l2 [$t index "$l1 lineend + 1 c"]
    if { $l2 == "" } { return $old_start }
    set l3 [$t search -elide -regexp {\S} $l2 {end}]
    return $l3
}

proc invoke {} {
    set t $::voss2_info(txtwin)

    set cmd	    [$t get promptEnd {end}]
    set cmd_start   [$t index promptEnd]
    set cmd_end	    [$t index {end - 1c}]

    set cmd_start [skip_info_idx $t $cmd_start]
    $t mark set promptEnd $cmd_start
    $t mark gravity promptEnd left


    if [string match "*;*" $cmd] {
	$t insert insert \n
	$t yview -pickplace insert
    } else {
	# nothing is going to be evaluated - just insert a space in the middle
	# (like Shift-Return)
	$t insert insert \n
	$t yview -pickplace insert
	return
    }

    if { [regexp {set_file_name} $cmd] == 0 } { 
	set cmd [format {set_file_name "stdin";set_line_number 1;%s} $cmd]
    }
    set in_color stdin_color

    #
    # puts stdout [string trimright $cmd "\n"]
    # [ return status, error index, remaining elements in cmd_buffer ]
    update idletasks
    lassign [uplevel #0 [list send_top_voss2_cmd $cmd]] res error_pos rem_inps

    update idletasks

    # gross hack to make returns (i.e., just typing return and nothing
    # else) not show up in the history list.
    if { $cmd != "\n" } {
	$t tag add $in_color $cmd_start "$cmd_end"
    }

    $t yview -pickplace insert
}

set preference_anon_cnt 0
set choice_width 30

proc pref:add_int {w name text choices} {
    pref:add_string $w $name $text $choices
}

proc pref:add_string {w name text choices} {
    set i $::preference_anon_cnt
    incr ::preference_anon_cnt
    if [expr ($i%2) == 0 ] {
        set bg darkgrey
    } else {
        set bg lightgrey
    }
    frame $w.f$i -relief groove -background $bg
    pack $w.f$i -side top -fill x -pady 3
    label $w.f$i.n -text $name -width 60 -anchor w -background $bg \
		-compound left -image ::bitmap::copy_to_clipboard
    bind $w.f$i.n <Button-1> "set_selection $w $name"
    label $w.f$i.l -text "  $text" -width 60 -anchor w -background $bg
    entry $w.f$i.om -textvariable ::voss2_preference_value($name) \
                    -width $::choice_width \
            -relief groove
    set ::voss2_preference_value($name) $choices
    pack $w.f$i.n -side top -fill x -expand yes -anchor w
    pack $w.f$i.om -side right 
    pack $w.f$i.l -side left -fill x -expand yes
}


proc pref:add_enum {w name text choices} {
    set i $::preference_anon_cnt
    incr ::preference_anon_cnt
    if [expr ($i%2) == 0 ] {
        set bg darkgrey
    } else {
        set bg lightgrey
    }
    frame $w.f$i -relief groove -background $bg
    pack $w.f$i -side top -fill x -pady 3
    label $w.f$i.n -text $name -width 60 -anchor w -background $bg \
		-compound left -image ::bitmap::copy_to_clipboard
    bind $w.f$i.n <Button-1> "set_selection $w $name"
    label $w.f$i.l -text "  $text" -width 60 -anchor w -background $bg
    menubutton $w.f$i.om -menu $w.f$i.om.m  \
                -background lightgrey \
                -textvariable ::voss2_preference_value($name) \
                -anchor w \
                -direction flush \
                -width $::choice_width \
                -relief groove
    menu  $w.f$i.om.m -tearoff 0
    set first 1
    foreach ch $choices {
        $w.f$i.om.m add command -label $ch \
            -command [list set ::voss2_preference_value($name) $ch]
        if { $first == 1 } {
            set ::voss2_preference_value($name) $ch
            set first 0
        }
    }
    pack $w.f$i.n -side top -fill x -expand yes -anchor w
    pack $w.f$i.om -side right 
    pack $w.f$i.l -side left -fill x -expand yes
}

proc pref:add_bool {w name text choices} {
    if { $choices == "Yes" || $choices == "YES" || $choices == "yes" || \
         $choices == "T" || $choices == "True" || $choices == "TRUE" } {
        set complete_choices "YES NO"
    } else {
        set complete_choices "NO YES"
    }
    pref:add_enum $w $name $text $complete_choices
}


proc cget_vossrc {txt} {
    return [::voss2_help::help_clean [get_vossrc $txt]]
}

proc do_vossrc_update {w} {
    foreach line $::voss2_preferences {
        lassign $line name cname text type choices
        if { $type != "header" } {
            if { [cget_vossrc $name] != $::voss2_preference_value($name) } {
                update_vossrc $name $::voss2_preference_value($name)
            }
        }
    }
    destroy $w
}

proc do_vossrc_save {} {
    set filename [tk_getSaveFile -initialfile .vossrc]
    if { $filename != "" } {
        set fp [open $filename w]
        foreach line $::voss2_preferences {
            lassign $line name cname text type choices
            if { $type != "header" } {
                puts $fp "# $text"
                puts $fp [format {%s=%s} $name $::voss2_preference_value($name)]
                puts $fp ""
            }
        }
        close $fp
    }
}

proc set_preferences {} {
    set w $::voss2_top_level.preferences
    toplevel $w
    wm title $w "voss2: preferences"
    wm iconname $w "voss2: preferences"

    regexp {[0-9]*x[0-9]*\+([0-9]*)\+([0-9]*)} [winfo geometry .] --> dx dy
    set dx [expr $dx-100]
    wm geometry $w +$dx+$dy

    selection handle . provide_selection STRING
    selection handle $w provide_selection STRING

    label $w.title -text "Preferences"
    pack $w.title -side top -pady 4

    ttk::notebook $w.nb

    set i 0
    set tabcnt 0
    foreach line $::voss2_preferences {
        lassign $line name cname text type choices
	switch $type {
	    header  {
                        set ww [frame $w.nb.c_$tabcnt]
			$w.nb add $ww -text $name
			incr tabcnt
		   }
	    enum   {
                        pref:add_enum $ww $name $text $choices
		        set ::voss2_preference_value($name) [cget_vossrc $name]
		   }
	    bool   {
                        pref:add_bool $ww $name $text $choices
		        set ::voss2_preference_value($name) [cget_vossrc $name]
                   }
	    int    {
                        pref:add_int $ww $name $text $choices
		        set ::voss2_preference_value($name) [cget_vossrc $name]
		   }
	    string {
                        pref:add_string $ww $name $text $choices
		        set ::voss2_preference_value($name) [cget_vossrc $name]
		   }
	}
    }
    # do this here so that it comes up faster (don't have to display while
    # filling in the notebook tabs.
    pack $w.nb -expand yes -fill both -padx 5 -pady 5 -side top

    frame $w.buttons
    pack $w.buttons -side bottom -pady 4


    button $w.buttons.ok -text Ok -command "do_vossrc_update $w"
    button $w.buttons.cancel -text Cancel \
	-command "destroy $w"
    button $w.buttons.save -text "Save preferences to file" \
	-command "do_vossrc_save"
    pack $w.buttons.ok -side left -padx 10
    pack $w.buttons.save -side left -padx 10
    pack $w.buttons.cancel -side right -padx 10

    set old [focus]
    focus $w

    tkwait window $w

    focus $old
}


namespace eval voss2_history {

    variable history_idx 0

    proc reset_history {} {
	variable history_idx
	set history_idx -1
    }

    proc get_history_cmd {dir} {
	variable history_idx
	set t $::voss2_info(txtwin)
	set stdin_tag_list [$t tag ranges stdin_color]
	set commands [expr [llength $stdin_tag_list]/2]
	if {$commands > 0} {
	    # Compute new idx
	    incr history_idx $dir
	    if { $history_idx < 0 } { set history_idx [expr $commands-1] }
	    if { $history_idx >= $commands } { set history_idx 0 }

	    # Delete whatever we have been typing so far...
	    set cmd_start [$t index promptEnd]
	    set cmd_end   [$t index end]
	    $t delete $cmd_start $cmd_end
	    $t mark set insert {end}

	    # Find the old command
	    set idx1 [lindex $stdin_tag_list [expr 2 * $history_idx]]
	    set idx2 [lindex $stdin_tag_list [expr 2 * $history_idx + 1]]
	    set txt [$t get $idx1 $idx2]

	    # And insert it
	    set txt [string trim $txt]
	    catch {$t insert insert $txt}
	    $t see end
	}
    }

}

namespace eval voss2_help {

    proc help_clean {txt} {
	regsub -all {\\} $txt {} res
	regsub {^\{(.*)\}$} $res {\1} nres
	return $nres
    }

    proc name2wname {n} {
	set res "w"
	foreach c [split $n {}] {
	    set res [format {%s_%d} $res [scan $c %c]]
	}
	return $res
    }

    proc help_open_file {tb fun file s_line e_line} {
	set fname [name2wname $fun]"-def"
	if { ![winfo exists $tb.$fname] } {
	    set f $tb.$fname
	    frame $f -relief flat
	    $tb add $f -text "$fun-def"
	    $tb select $f
	    button $f.b -text Close -command [list destroy $f]
	    pack $f.b -side bottom
	    ttk::scrollbar $f.yscroll -command [list $f.t yview]
	    ttk::scrollbar $f.xscroll -orient horizontal \
				      -command [list $f.t xview]
	    text $f.t -background white -font $::voss2_help_font -wrap none \
		-yscrollcommand [list $f.yscroll set] -setgrid 1 -width 50
	    set fp [open $file]
	    set data [read $fp]
	    $f.t insert 1.0 $data
	    close $fp
	    $f.t tag add selected_fun "$s_line.0 linestart" "$e_line.0 lineend"
	    $f.t tag configure selected_fun -foreground red
	    $f.t tag configure selected_fun -background yellow
	    $f.t see "$s_line.0"
	    $f.t configure -state disabled
	    pack $f.yscroll -side right -fill y
	    pack $f.xscroll -side bottom -fill x
	    pack $f.t -side right -expand yes -fill both
	}
    }

    proc help_show_info {lb tb} {
	foreach index [$lb curselection] {
	    set fun [$lb get $index]
	    set wname [name2wname $fun]
	    if { ![winfo exists $tb.$wname] } {
		set f $tb.$wname
		frame $f -relief flat
		$tb add $f -text $fun
		$tb select $f
		button $f.b -text Close -command [list destroy $f]
		pack $f.b -side bottom
		ttk::scrollbar $f.yscroll -command [list $f.t yview]
		ttk::scrollbar $f.xscroll -orient horizontal \
					  -command [list $f.t xview]
		text $f.t -background white -font $::voss2_help_font \
		    -wrap none \
		    -xscrollcommand [list $f.xscroll set] \
		    -yscrollcommand [list $f.yscroll set] \
		    -setgrid 1 -width 50
		$f.t insert 1.0 [help_clean [help $fun]]
		$f.t tag configure source_loc   -foreground blue
		set file_loc [$f.t search {File:  } 1.0]
		if { $file_loc != "" } {
		    $f.t tag add source_loc "$file_loc+7chars" \
					    "$file_loc lineend"
		    set start_loc [$f.t search {Start: } $file_loc]
		    set end_loc   [$f.t search {End:   } $start_loc]
		    set file [$f.t get "$file_loc+7chars" "$file_loc lineend"]
		    set s_line [$f.t get "$start_loc+7chars" \
					 "$start_loc lineend"]
		    set e_line [$f.t get "$end_loc+7chars" "$end_loc lineend"]
		    $f.t tag bind source_loc <Button-1> \
				"::voss2_help::help_open_file $tb $fun \
					$file $s_line $e_line"
		}
		$f.t configure -state disabled
		pack $f.yscroll -side right -fill y
		pack $f.xscroll -side bottom -fill x
		pack $f.t -side right -expand yes -fill both
	    } else {
		set f $tb.$wname
                $tb select $f
	    }
	}
	$lb selection clear 0 end
    }


    proc help_do_match {lb} {
	$lb delete 0 end 
	set alts [get_matching_functions $::help_search_name_pat \
					 $::help_search_file_pat \
					 $::help_search_arg_pat \
					 $::help_search_res_pat]
	foreach fun [lsort $alts] {
	    $lb insert end $fun
	}
    }

    proc help_detach {w nb} {
	set idx [$nb index current]
	if { $idx == 0 } {
	    set tw $::voss2_top_level.voss2_help
	    wm manage $tw
	    $nb configure -width 900 -height 500
	    $nb select 1
	}
    }

    proc help_start {} {
	set w $::voss2_top_level.voss2_help
	if [winfo exists $w] { return }
	if [info exists ::voss2_status(hidden_fl_window)] {
	    toplevel $w
	} else {
	    frame $w 
	}

	set w_top_notebook		$w.nb
	set w_detach_tab_frame	$w.nb.dt
	set w_function_tab_frame	$w.nb.f
	set w_panedwindow		$w.nb.f.p
	set w_search_frame		$w.nb.f.p.w
	set w_help_tab_notebook	$w.nb.f.p.e

	set w_special_tab_frame	$w.nb.s
	set w_special_panedwindow		$w.nb.s.p
	set w_special_search_frame		$w.nb.s.p.w
	set w_special_help_tab_notebook		$w.nb.s.p.e
	set w_special_search_alts	    	$w.nb.s.p.w.alts
	set w_special_action_buttons    	$w.nb.s.p.w.actions

	set w_search_name_pat	$w.nb.f.p.w.name
	set w_search_file_pat	$w.nb.f.p.w.file
	set w_search_arg_pat	$w.nb.f.p.w.arg
	set w_search_res_pat	$w.nb.f.p.w.res
	set w_search_button	    	$w.nb.f.p.w.search
	set w_search_alts	    	$w.nb.f.p.w.alts
	set w_action_buttons    	$w.nb.f.p.w.actions
	
	set ::help_search_name_pat "*"
	set ::help_search_file_pat "*"
	set ::help_search_arg_pat "*"
	set ::help_search_res_pat "*"

	ttk::notebook $w_top_notebook
	pack $w_top_notebook -side top -fill both -expand yes
	
	frame $w_detach_tab_frame -relief flat
	$w.nb add $w_detach_tab_frame -image ::bitmap::detach

	bind $w.nb <<NotebookTabChanged>> \
		[list ::voss2_help::help_detach $w $w.nb]

	frame $w_function_tab_frame -relief flat
	$w.nb add $w_function_tab_frame -text "Functions"

	frame $w_special_tab_frame -relief flat
	$w.nb add $w_special_tab_frame -text "Special"

	$w.nb select 1

############

	ttk::panedwindow $w_special_panedwindow -orient horizontal
	pack $w_special_panedwindow -side top -fill both -expand yes

	frame $w_special_search_frame -relief raised
	$w_special_panedwindow add $w_special_search_frame

	ttk::notebook $w_special_help_tab_notebook
	$w_special_panedwindow add $w_special_help_tab_notebook


	frame $w_special_search_alts -relief flat
	pack $w_special_search_alts -side top -fill both -expand yes

	ttk::scrollbar $w_special_search_alts.scroll \
			-command "$w_special_search_alts.list yview"
	listbox $w_special_search_alts.list \
		    -yscroll "$w_special_search_alts.scroll set" \
		    -font $::voss2_txtfont -setgrid 1 -selectmode extended \
		    -width 30
	pack $w_special_search_alts.scroll -side right -fill y
	pack $w_special_search_alts.list -side left -fill both -expand yes
	
	frame $w_special_action_buttons -relief raised
	pack $w_special_action_buttons -side top -fill x

	    button $w_special_action_buttons.quit -text "Quit" \
		-command ::voss2_help::help_end
	    pack $w_special_action_buttons.quit -side left -expand yes

	    button $w_special_action_buttons.info -text "Man page" \
	      -command "::voss2_help::help_show_info \
				$w_special_search_alts.list \
				$w_special_help_tab_notebook"
	    pack $w_special_action_buttons.info -side left -expand yes

	bind $w_special_search_alts.list <Double-Button-1> \
			"$w_special_action_buttons.info invoke"
	if [info exists ::voss2_status(hidden_fl_window)] {
	    wm minsize $::voss2_top_level.voss2_help 100 20
	} else {
	    pack $w -side bottom -before $::voss2_info(txtwin) -fill x
	}

	set lb $w_special_search_alts.list
	$lb delete 0 end
	$lb insert end "DIR"
	$lb insert end "eprintf"
	$lb insert end "fprintf"
	$lb insert end "printf"
	$lb insert end "sprintf"
	$lb insert end "sscanf"

	ttk::panedwindow $w_panedwindow -orient horizontal
	pack $w_panedwindow -side top -fill both -expand yes

	frame $w_search_frame -relief raised
	$w_panedwindow add $w_search_frame

	ttk::notebook $w_help_tab_notebook
	$w_panedwindow add $w_help_tab_notebook

	ttk::labelframe $w_search_name_pat -text "Function name matching"
	pack $w_search_name_pat -side top -fill x -pady 4
	entry $w_search_name_pat.entry \
	    -textvariable ::help_search_name_pat -width 30 \
		-font $::voss2_txtfont
	pack $w_search_name_pat.entry -side top -fill both -expand yes
	bind $w_search_name_pat.entry <Return> \
		[list ::voss2_help::help_do_match $w_search_alts.list]

	ttk::labelframe $w_search_file_pat -text "File name matching"
	pack $w_search_file_pat -side top -fill x -pady 4
	entry $w_search_file_pat.entry -textvariable ::help_search_file_pat \
		-font $::voss2_txtfont
	pack $w_search_file_pat.entry -side top -fill both -expand yes
	bind $w_search_file_pat.entry <Return> \
		[list ::voss2_help::help_do_match $w_search_alts.list]

	ttk::labelframe $w_search_arg_pat -text "Argument type matching"
	pack $w_search_arg_pat -side top -fill x -pady 4
	entry $w_search_arg_pat.entry -textvariable ::help_search_arg_pat \
		-font $::voss2_txtfont
	pack $w_search_arg_pat.entry -side top -fill both -expand yes
	bind $w_search_arg_pat.entry <Return> \
		[list ::voss2_help::help_do_match $w_search_alts.list]

	ttk::labelframe $w_search_res_pat -text "Result type matching"
	pack $w_search_res_pat -side top -fill x -pady 4
	entry $w_search_res_pat.entry -textvariable ::help_search_res_pat \
		-font $::voss2_txtfont
	pack $w_search_res_pat.entry -side top -fill both -expand yes
	bind $w_search_res_pat.entry <Return> \
		[list ::voss2_help::help_do_match $w_search_alts.list]

	button $w_search_button -text Search \
	    -command "::voss2_help::help_do_match $w_search_alts.list"
	pack $w_search_button -side top -fill x 

	frame $w_search_alts -relief flat
	pack $w_search_alts -side top -fill both -expand yes

	ttk::scrollbar $w_search_alts.scroll \
			-command "$w_search_alts.list yview"
	listbox $w_search_alts.list -yscroll "$w_search_alts.scroll set" \
		    -font $::voss2_txtfont -setgrid 1 -selectmode extended
	pack $w_search_alts.scroll -side right -fill y
	pack $w_search_alts.list -side left -fill both -expand yes
	
	frame $w_action_buttons -relief raised
	pack $w_action_buttons -side top -fill x

	    button $w_action_buttons.quit -text "Quit" \
		-command ::voss2_help::help_end
	    pack $w_action_buttons.quit -side left -expand yes

	    button $w_action_buttons.info -text "Man page" \
	      -command "::voss2_help::help_show_info $w_search_alts.list \
						     $w_help_tab_notebook"
	    pack $w_action_buttons.info -side left -expand yes

	bind $w_search_alts.list <Double-Button-1> \
			"$w_action_buttons.info invoke"
	if [info exists ::voss2_status(hidden_fl_window)] {
	    wm minsize $::voss2_top_level.voss2_help 100 20
	} else {
	    pack $w -side bottom -before $::voss2_info(txtwin) -fill x
	}
    }

    proc help_end {} {
	catch {destroy $::voss2_top_level.voss2_help}
	update
	$::voss2_info(txtwin) see end
    }

}

#
# Open the socket links
#
# There are four pairs of (uni-directional) links:
#   1) sync_to_tcl_fp evaluating tcl code sent from fl and expecting results
#   2) to_tcl_fp: Used to send tcl evaluation requests from fl without results
#   3) cmd_from_tcl_fp: Used to send fl commands from tcl
#   4) callback_from_tcl_fp: Used to send fl commands from tcl
#   5) callback_from_tcl_fp_res: Used to send results of fl commands to tcl
#
set ::sync_to_tcl_fp [socket $socket_addr $socket_port]
fconfigure $::sync_to_tcl_fp -buffering line -translation lf

set ::to_tcl_fp [socket $socket_addr $socket_port]
fconfigure $::to_tcl_fp -buffering line -translation lf

set ::cmd_from_tcl_fp [socket $socket_addr $socket_port]
fconfigure $::cmd_from_tcl_fp -buffering line -translation lf

set ::callback_from_tcl_fp [socket $socket_addr $socket_port]
fconfigure $::callback_from_tcl_fp -buffering line -translation lf

set ::callback_from_tcl_fp_res [socket $socket_addr $socket_port]
fconfigure $::callback_from_tcl_fp_res -buffering line -translation lf


proc do_read_return_result_from_cmd {chan} {
    if ![eof $::cmd_from_tcl_fp] {
        if { [gets $::cmd_from_tcl_fp line] > 0 } {
            eval set ::return_result_from_cmd $line
        }
    }
}
fileevent $::cmd_from_tcl_fp readable \
            [list do_read_return_result_from_cmd $::cmd_from_tcl_fp]

proc do_read_return_result_from_callback {chan} {
    if ![eof $::callback_from_tcl_fp_res] {
        if { [gets $::callback_from_tcl_fp_res line] > 0 } {
	    set line2 [unprotect $line]
	    regexp {([0-9][0-9]*) (.*)$} $line2 -> rid res
            set ::return_result_from_callback($rid) $res
        }
    }
}
fileevent $::callback_from_tcl_fp_res readable \
	[list do_read_return_result_from_callback $::callback_from_tcl_fp_res]


proc do_remote_tcl_request {chan} {
    if ![eof $::sync_to_tcl_fp] {
        if { [gets $::sync_to_tcl_fp line] > 0 } {
	    set line2 [unprotect $line]
            if [catch {uplevel #0 $line2} res] {
                puts -nonewline $::sync_to_tcl_fp "1"
		set msg "$res $::errorInfo"
                puts $::sync_to_tcl_fp [protect $msg]
            } else {
                puts -nonewline $::sync_to_tcl_fp "0"
                puts $::sync_to_tcl_fp [protect $res]
            }
            flush $::sync_to_tcl_fp
        }
    }
}

fconfigure $::sync_to_tcl_fp -buffering line -translation lf
fileevent $::sync_to_tcl_fp readable \
            [list do_remote_tcl_request $::sync_to_tcl_fp]

proc do_tcl_cmd {chan} {
    if ![eof $::to_tcl_fp] {
        if { [gets $::to_tcl_fp line] > 0 } {
	    set line3 [unprotect $line]
            uplevel #0 $line3
        }
    }
}

fconfigure $::to_tcl_fp -buffering line -translation lf
fileevent $::to_tcl_fp readable \
            [list do_tcl_cmd $::to_tcl_fp]

 
proc prepare_for_stdin {} {
    set w $::voss2_info(txtwin)
    ::voss2_history::reset_history
    catch {$w tag lower readonly}
    if { [string match [lindex [$w tag names insert] 0] readonly] != 0 } {
	$w mark set insert {end - 1c}
    }
    $w see insert
}

proc provide_stdin {line} {
    set t $::voss2_info(txtwin)
    $t insert insert $line readonly
    $t mark set insert {end}
}

proc provide_info {line} {
    set t $::voss2_info(txtwin)
    $t insert end $line [list info_color readonly]
    $t mark set insert {end}
}

proc process_stdin {} {
    set t $::voss2_info(txtwin)
    $t mark set insert {end}
    update idletasks
    invoke
}

set ::busy_level 0

proc mk_busy {w mode} {
    foreach cw [winfo children $w] {
	set active_child 0
	set len [string length $cw]
	foreach aw $::always_alive {
	    if [string equal -length $len $cw $aw] {
		set active_child 1
	    }
	}
	if { $active_child == 0 } {
	    if { $mode == 1 } {
		catch {tk busy hold $cw -cursor watch}
	    } else {
		catch {tk busy forget $cw}
	    }
	} else {
	    mk_busy $cw $mode
	}
    }
}

proc i_am_busy {} {
    incr ::busy_level
    if { $::busy_level == 1 } {
	mk_busy . 1
	update
    }
}

proc i_am_free {} {
    set ::busy_level [expr $::busy_level-1]
    if [expr $::busy_level < 0] { 
	set ::busy_level 0
    }
    if { $::busy_level == 0 } {
	mk_busy . 0
	update
    }
}

set ::dbg_levels 0

proc __basic_fl_callback {fun_name args} {
    set carg ""
    set sep ""
    foreach arg $args {
        append carg $sep [protect $arg]
        set sep " "
    }
    incr ::result_id
    set cur_id $::result_id
    puts $::callback_from_tcl_fp "$cur_id $fun_name $carg"
    flush $::callback_from_tcl_fp

    tkwait variable ::return_result_from_callback($cur_id)
    set rline $::return_result_from_callback($cur_id)
    set status [string range $rline 0 0]
    set result [string range $rline 1 end]
    if { $status == 0 } {
	WriteStdErr "Callback failure: $result\n"
        error $result
    } else {
        return [clean_name $result]
    }
}


proc hide_fl_window {} {
    set ::voss2_status(hidden_fl_window) 1
    wm withdraw .
}
