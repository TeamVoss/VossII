;#########################################################################
;# Copyright 2020 Carl-Johan Seger
;# SPDX-License-Identifier: Apache-2.0
;#########################################################################

set ::util_window_cnt 0

set base_ttfont [font actual {{bitstream vera sans mono} 6}]
set base_tfont  [font actual {{bitstream vera sans mono} 8}]
set base_sfont  [font actual {{bitstream vera sans mono} 10}]
set base_mfont  [font actual {{bitstream vera sans mono} 11}]
set base_bfont  [font actual {{bitstream vera sans mono} 13}]

set ::voss2_txtfont0    "-*-courier-bold-r-normal-*-10-*-*-*-*-*-*-*"
set ::voss2_txtfont1    "-*-courier-bold-r-normal-*-12-*-*-*-*-*-*-*"
set ::voss2_txtfont2    "-*-courier-bold-r-normal-*-14-*-*-*-*-*-*-*"
set ::voss2_txtfont3    "-*-courier-bold-r-normal-*-16-*-*-*-*-*-*-*"
set ::voss2_txtfont4    "-*-courier-bold-r-normal-*-18-*-*-*-*-*-*-*"
set ::voss2_txtfont5    "-*-courier-bold-r-normal-*-20-*-*-*-*-*-*-*"
set ::voss2_txtfont6    "-*-courier-bold-r-normal-*-24-*-*-*-*-*-*-*"
set ::voss2_txtfont     $::voss2_txtfont2
set ::voss2_help_font   $::voss2_txtfont1


set base_sc     0.85            ;# Scale factor

image create bitmap ::bitmap::detach -data "#define detach_width 16
#define detach_height 16
static unsigned char detach_bits[] = {
   0x00, 0x00, 0xce, 0x7f, 0x06, 0x40, 0x0a, 0x40, 0x10, 0x40, 0x20, 0x40,
   0x42, 0x40, 0x02, 0x40, 0x02, 0x40, 0x02, 0x40, 0x02, 0x40, 0x02, 0x40,
   0x02, 0x40, 0x02, 0x40, 0xfe, 0x7f, 0x00, 0x00};"

image create bitmap ::bitmap::copy_to_clipboard -data \
"#define copy_to_clipboard_width 16
#define copy_to_clipboard_height 16
static unsigned char copy_to_clipboard_bits[] = {
   0x60, 0x00, 0xf8, 0x01, 0xff, 0x0f, 0x01, 0x08, 0x7d, 0x08, 0x01, 0x08,
   0x1d, 0x08, 0x01, 0x08, 0x7d, 0x0a, 0x01, 0x03, 0x9d, 0x1f, 0x01, 0x03,
   0x1d, 0x0a, 0x01, 0x08, 0x01, 0x08, 0xff, 0x0f};"


proc change_font_rec {w new_font} {
    catch { $w configure -font $new_font }
    foreach ch [winfo children $w] {
        change_font_rec $ch $new_font
    }
}

proc change_fonts {new_font} {
    change_font_rec . $new_font
    set ::voss2_txtfont $new_font
    set ::voss2_help_font $new_font
    ttk::style configure TNotebook.Tab -font $new_font
    ttk::style configure TLabelframe.Label -font $new_font
    ttk::style configure TCombobox.Entry -font $new_font
    ttk::style configure TButton -font $new_font
    ttk::style configure TLabel -font $new_font
    ttk::style configure TEntry -font $new_font
    ttk::style configure TCheckbutton -font $new_font
    option add *TCombobox*Listbox.font $new_font
    option add *TCombobox*Entry.font $new_font
    option add *TCombobox*TEntry.font $new_font
    update
}

# Useful procedure that allows syntaxx like: val {v1 v2 v3} $list
proc val {vars list} {
    if { [llength $vars] != [llength $list] } {
        error "-E- val: length mismatch |$vars| != |$list|"
    }
    foreach v $vars e $list {
        if { [llength $v] > 1 } {
            uplevel 1 "val [list $v] [list $e]"
        } else {
            upvar 1 $v var
            set var $e
        }
    }
}

proc util:set_result {w ret} {
    set ::results($w) $ret
    destroy $w
}

proc util:report_result {msg_header msg_details return_alts {pw ""}} {
    if { $pw == "" } {
	set w [format {.result_report_%d} $::util_window_cnt]
	incr ::util_window_cnt
	catch {destroy $w}
	toplevel $w
    } else {
	set w $pw
    }
    frame $w.tf -relief flat
	ttk::scrollbar $w.tf.hsb -orient horizontal \
		-command [list $w.tf.t xview]
	ttk::scrollbar $w.tf.vsb -orient vertical \
		-command [list $w.tf.t yview]
	text $w.tf.t -xscrollcommand [list $w.tf.hsb set] \
		-yscrollcommand [list $w.tf.vsb set] -width 80
	pack $w.tf.hsb -side bottom -fill x
	pack $w.tf.vsb -side right -fill y
	pack $w.tf.t -side top -fill both -expand yes

    frame $w.bf -relief flat
    set i 0
    foreach alt $return_alts {
	set txt [lindex $alt 0]
	set ret [lindex $alt 1]
	button $w.bf.b$i -text $txt -command [list util:set_result $w $ret]
	pack $w.bf.b$i -side left -expand yes
	incr i
    }
    pack $w.tf -side top -fill both -expand yes
    pack $w.bf -side bottom -fill x

    $w.tf.t tag configure message -foreground red -background yellow

    $w.tf.t insert end $msg_header message
    $w.tf.t insert end \n
    $w.tf.t insert end $msg_details

    $w.tf.t see end

    return $w
}

proc util:wait_for_button {w} {
    tkwait window $w
    return $::results($w)
}

proc util:report_result_in_file {msg file return_alts} {
    set fp [open $file "r"]
    set txt [read $fp]
    close $fp
    append header $msg "\n" "--------------------------------------------------------------------------------" "\n"
    util:report_result $header $txt $return_alts
}

# NOTE: This is OS dependent!!!!
proc util:process_is_alive {process_id} {
    if [catch {set rl [split [exec ps -o s $process_id] "\n"]}] {
        return 0
    }
    if { [llength $rl] != 2 } { return 0 }
    set status [lindex $rl 1]
    switch $status {
	R	-
	S	-
	D	-
	T	{ return 1; }
	default { return 0; }
    }
}

proc util:watch_process {pid {delay 50} {action "destroy ."} } {
    if [util:process_is_alive $pid] {
        after $delay [list util:watch_process $pid $delay $action]
    } else {
        eval uplevel #0 $action
        update idletasks
    }
}

proc util:process_bg_data {fp w} {
    set status [catch { gets $fp new_output } res]
    if { $status != 0 } {
        set ::bg(error_msg) $res
        set ::process_bg_status fail
    } elseif { $res >= 0 } {
	append ::bg(output) $new_output "\n"
        if { $w != "-" } {
            $w.t insert end $new_output
            $w.t insert end "\n"
            $w.t see end 
            update idletasks
        }
    } elseif { [chan eof $fp] } {
	chan event $fp readable {}
        set ::process_bg_status ok
    } elseif { ![chan blocked $fp] } {
        WriteStdErr "Should never happen!\n"
        set ::process_bg_status fatal_error
    } 
} 

proc util:bg_exec {cmd display_output after_cmd {parent_window .}} {
    set w "-"
    if { $display_output } {
        set w .bg_display
        catch {destroy $w}
        set px [winfo x $parent_window]
        set py [winfo y $parent_window]
        toplevel $w
        wm geometry $w +$px+$py
        scrollbar $w.yscroll -command "$w.t yview"
        scrollbar $w.xscroll -orient horizontal -command "$w.t xview"
        text $w.t -bg AntiqueWhite2 -fg black -width 80 -height 40 \
            -yscrollcommand "$w.yscroll set" \
            -xscrollcommand "$w.xscroll set"
        pack $w.yscroll -side right -fill y
        pack $w.xscroll -side bottom -fill x
        pack $w.t -side top -fill both -expand yes
    }
    set ::bg(error_msg) "_"
    set ::bg(output) " "
    set ::process_bg_status ""
    set fp [open "| $cmd |& cat -" r]
    chan configure $fp -blocking no
    chan event $fp readable [list util:process_bg_data $fp $w]
    tkwait variable ::process_bg_status
    if { $after_cmd != "" } {
        {*}$after_cmd $w $::process_bg_status "$::bg(error_msg)" "$::bg(output)"
    }
    return $w
}

proc util:after_try_program {title choices w status err_msg output} {
    if { $status == "ok" && [regexp {===SuCcEsS===} $output]} {
        set ::util_bg(result) ok
    } else {
        frame $w.after -relief flat -height 30
        pack $w.after -before $w.xscroll -side bottom -fill x
        label $w.after.l -text $title
        pack $w.after.l -side left -padx 20
        foreach choice $choices {
            val {txt retval} $choice
            button $w.after.b$retval -text $txt \
                -command "set ::util_bg(result) $retval"
            pack $w.after.b$retval -side left -padx 10
        }
#        update
	tkwait variable ::util_bg(result)
    }
}


# Run the shell program (with arguments) in args
# If it fails, put up a choice window with title 'title' and
# alternatives 'choices' ({button_text return_value} list)
#
proc util:try_program {parent_window title choices args} {
    set ::util_bg(result) ""
    set w [util:bg_exec $args 1 \
		[list util:after_try_program "$title" $choices] \
		$parent_window]
    destroy $w
    return $::util_bg(result)
}


set ::always_alive {}

proc make_window_always_alive {w} {
    lappend ::always_alive $w
}

proc remove_window_always_alive {w} {
    set old $::always_alive
    set ::always_alive {}
    foreach ww $old {
	if { $ww != $w } {
	    lappend ::always_alive $ww
	}
    }
}

proc mk_busy {w mode} {
    foreach cw [winfo children $w] {
	set has_active_child 0
	set len [string length $cw]
	foreach aw $::always_alive {
	    if [string equal -length $len $cw $aw] {
		set has_active_child 1
	    }
	}
	if { $has_active_child == 0 } {
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
    foreach w [wm stackorder .] {
	incr ::busy_level($w)
	if { $::busy_level($w) == 1 } {
	    mk_busy $w 1
	}
    }
    update
}

proc i_am_free {} {
    update
    foreach w [wm stackorder .] {
	if [info exists ::busy_level($w)] {
	    incr ::busy_level($w) -1
	    if { [expr $::busy_level($w) <= 0] } {
		mk_busy $w 0
		set ::busy_level($w) 0
	    }
	} else {
	    mk_busy $w 0
	    set ::busy_level($w) 0
	}
    }
    update
}

proc make_all_active {} {
    foreach w [wm stackorder .] {
	mk_busy $w 0
	set ::busy_level($w) 0
    }
}


proc draw_bdd_profile {vws} {
    set w [format {.bddw_%d} $::util_window_cnt]
    incr ::util_window_cnt
    catch {destroy $w}
    toplevel $w
    scrollbar $w.yscroll -command "$w.c yview"
    scrollbar $w.xscroll -orient horizontal -command "$w.c xview"
    set cnt [llength $vws]
    set ht [expr 10*$cnt+50]
    if [expr $ht > 500] { set ht 500; }
    canvas $w.c -background white -width 500 -height $ht \
            -yscrollcommand "$w.yscroll set" \
            -xscrollcommand "$w.xscroll set"
    pack $w.yscroll -side right -fill y
    pack $w.xscroll -side bottom -fill x
    pack $w.c -side top -fill both -expand yes

    bind $w.c <KeyPress-q> "destroy $w"
    focus $w.c

    set mx 0
    foreach vw $vws {
        set var [lindex $vw 0]
        set wid [lindex $vw 1]
        if [expr $wid > $mx] {
            set mx $wid
        }
    }
    set y 10
    set x 200
    foreach vw $vws {
        set var [lindex $vw 0]
        set wid [lindex $vw 1]
        set X [expr $x + round(200*$wid/$mx)]
        $w.c create text [expr $x-10] $y -anchor e -justify right -text $var \
                -font $::voss2_txtfont0
        $w.c create rectangle $x [expr $y+3] $X [expr $y-3] -fill blue
        $w.c create text [expr $X+5] $y -anchor w -justify left -text $wid \
                -font $::voss2_txtfont0
        set y [expr $y + 10]
    }
    update idletasks
    $w.c configure -scrollregion [$w.c bbox all]
}

proc display_dot {dot_pgm {w ""} {close_fun ""}} {
    if { $w == "" } {
	incr ::dot_displays
	set w .dot$::dot_displays
	catch {destroy $w}
	toplevel $w
	set c $w.c
	scrollbar $w.yscroll -command "$c yview"
	scrollbar $w.xscroll -orient horizontal -command "$c xview"
	canvas $c -background white \
		-yscrollcommand "$w.yscroll set" \
		-xscrollcommand "$w.xscroll set"
	pack $w.yscroll -side right -fill y
	pack $w.xscroll -side bottom -fill x
	pack $c -side top -fill both -expand yes

	bind $c <2> "%W scan mark %x %y"
	bind $c <B2-Motion> "%W scan dragto %x %y"

	# Zoom bindings
	bind $c <ButtonPress-3> "zoom_lock %W %x %y"
	bind $c <B3-Motion> "zoom_move %W %x %y"
	bind $c <ButtonRelease-3> "zoom_execute %W %x %y %X %Y {}"

	# Mouse-wheel bindings for zooming in/out
	bind $c <Button-4> "zoom_out $c 1.1 %x %y"
	bind $c <Button-5> "zoom_out $c [expr {1.0/1.1}] %x %y"
    } else {
	set c $w.c
	$c delete all
    }

    set ::cur_zoom_factor($w) 100.0
    set ::cur_zoom_factor($c) 100.0
    set ::sc($c) $::base_sc
    set ::mfont($c) $::base_mfont
    set ::sfont($c) $::base_sfont

    set fp [open "|dot -Ttk $dot_pgm" "r"]
    while {[gets $fp line] >= 0} {
	set nline [regsub {{"Times" 14}} $line {$::voss2_txtfont1}]
        eval $nline
    }
    close $fp
    update idletasks

    ;# Create maps from node/edge name to tag of drawn symbol
    catch {unset ::dot_node2node_fig_tag}
    catch {unset ::dot_node2text_tag}
    catch {unset ::dot_edge2line_tag}
    catch {unset ::dot_edge2text_tag}
    foreach t [$c find all] {
        if ![catch {$c itemcget $t -text} txt] {
            set tag [$c gettags $t]
            $c itemconfigure $tag -font $::base_mfont
            $c addtag "_IsTeXt_" withtag $tag
            if [regexp "0node.*" $tag] {
                set rtag "1[string range $tag 1 end]"
                set ::dot_node2node_fig_tag($c,$txt) $rtag
                set ::dot_node2text_tag($c,$txt) $tag
            } elseif [regexp "0edge.*" $tag] {
                set rtag "1[string range $tag 1 end]"
                lappend ::dot_edge2line_tag($c,$txt) $rtag
                lappend ::dot_edge2text_tag($c,$txt) $tag
            }
        }
    }

    val {lx ly ux uy} [$w.c bbox all]
    set wid [expr $ux-$lx]
    if [expr $wid > 800] { set wid 800 }
    set ht [expr $uy-$ly]
    if [expr $ht > 800] { set ht 800 }
    $c configure -width $wid -height $ht
    update
    $w.c configure -scrollregion [$w.c bbox all]
    bind $w.c <KeyPress-q> "destroy $w"
    if { $close_fun != "" } {
	bind $c <Destroy> [list $close_fun $w]
    }
    focus $w.c
    return $w
}


set ::gui_io(wcnt) 0

proc gui_io:restore_defaults {w texts} {
    set tcnt 0
    foreach txt $texts {
	val {label default} $txt
	set tf "$w.tf$tcnt"
	incr tcnt
	set te $tf.e
	$te delete 0 end
	$te insert 0 $default
    }
}

proc gui_io:get_data {title texts} {
    incr ::gui_io(wcnt)
    set w ".gui_io$::gui_io(wcnt)"
    toplevel $w
    wm title $w $title
    label $w.l -text $title
    pack $w.l -side top -fill x
    set sz 0
    foreach txt $texts {
	val {label default} $txt
	set ll [string length $label]
	if [expr $ll > $sz] {
	    set sz $ll
	}
    }
    set tcnt 0
    foreach txt $texts {
	val {label default} $txt
	set tf "$w.tf$tcnt"
	incr tcnt
	frame $tf
	set tl $tf.l
	label $tl -text $label -width $sz -anchor w
	set te $tf.e
	entry $te 
	bind $te <KeyPress-Return> [list :gui:return 1 $w $tcnt]
	$te insert 0 $default
	pack $tl -side left
	pack $te -side left -fill x
	pack $tf -side top -fill x
    }
    frame $w.bf
    pack $w.bf -side bottom -fill x
    label $w.bf.sp1
    button $w.bf.cancel -text Cancel -command ":gui:return 0 $w $tcnt"

    label $w.bf.sp2
    button $w.bf.restore -text Defaults \
	    -command "gui_io:restore_defaults $w [list $texts]"

    label $w.bf.sp3
    button $w.bf.ok -text Ok -command ":gui:return 1 $w $tcnt"

    label $w.bf.sp4
    pack $w.bf.sp1 -side left -fill x -expand 1
    pack $w.bf.cancel -side left -padx 10
    pack $w.bf.sp2 -side left -fill x -expand 1
    pack $w.bf.restore -side left -padx 10
    pack $w.bf.sp3 -side left -fill x -expand 1
    pack $w.bf.ok -side left -padx 10
    pack $w.bf.sp4 -side left -fill x -expand 1
    i_am_free;
    foreach ww [winfo children .] {
	catch {tk busy forget $ww}
    }
#    update
    tkwait window $w
}

proc :gui:return {status w tcnt} {
    if { $status == 0 } {
	destroy $w
	return
    }
    for {set i 0} {$i < $tcnt} {incr i} {
	fl_set_gui_return ["$w.tf$i.e" get]
    }
    destroy $w
}

proc clean_name {name {repeat 0}} {
    if { $repeat == 1 } {
	while { [regexp "^{\(.*\)}$" $name --> base] } {
	    set name $base
	}
	return $name
    } else {
	if { [regexp "^{\(.*\)}$" $name --> base] } {
	    set name $base
	}
	return $name
    }
}

set popup_window_cnt 0

proc vis_toplevel {w {host .} {width {}} {height {}} {title ""}} {
    toplevel $w
    set lx [winfo rootx $host]
    set ly [winfo rooty $host]
    if { $width != {} } {
        if { $height != {} } {
            wm geometry $w \
                [expr $width]x[expr $height]+[expr $lx+2]+[expr $ly+2]
        } else {
            wm geometry $w \
                [expr $width]x[expr $width]+[expr $lx+2]+[expr $ly+2]
        }
    } else {
        wm geometry $w +[expr $lx+2]+[expr $ly+2]
    }
    if { $title != "" } {
        wm title $w $title
    }
}

proc post_big_popup_window {host_window lines {title popup_data}} {
    set w [format {.popup_data_%d} $::popup_window_cnt]
    incr ::popup_window_cnt
    catch {destroy $w}
    if { $lines == {} } return
    vis_toplevel $w $host_window {} {} $title
    frame $w.txt -relief flat
	text $w.txt.t -relief sunken -bd 2 \
		    -yscrollcommand "$w.txt.yscroll set" \
		    -xscrollcommand "$w.txt.xscroll set" \
		    -setgrid 1  -height 30 -width 80 \
		    -font $::hdl_font
	scrollbar $w.txt.yscroll -command "$w.txt.t yview"
	scrollbar $w.txt.xscroll -orient horizontal -command "$w.txt.t xview"
	pack $w.txt.xscroll -side bottom -fill x
	pack $w.txt.yscroll -side right -fill y
	pack $w.txt.t -side top -fill both -expand yes
    set txt ""
    set sep ""
    foreach line $lines {
      append txt $sep [clean_name $line]
      set sep "\n"
    }
    $w.txt.t insert end $txt
    button $w.box -text Ok -width 6 -command "destroy $w"
    pack $w.txt -side top -pady 10 -expand yes -fill both 
    pack $w.box -side bottom -anchor s
}

proc post_popup_window {host_window text_item {title popup_data}} {
    set w [format {.popup_data_%d} $::popup_window_cnt]
    incr ::popup_window_cnt
    catch {destroy $w}
    if { $text_item == "" } return
    vis_toplevel $w $host_window {} {} $title
    frame $w.txt -relief flat
	text $w.txt.t -relief sunken -bd 2 \
		    -yscrollcommand "$w.txt.yscroll set" \
		    -xscrollcommand "$w.txt.xscroll set" \
		    -setgrid 1  -height 30 -width 80 \
		    -font $::hdl_font
	scrollbar $w.txt.yscroll -command "$w.txt.t yview"
	scrollbar $w.txt.xscroll -orient horizontal -command "$w.txt.t xview"
	pack $w.txt.xscroll -side bottom -fill x
	pack $w.txt.yscroll -side right -fill y
	pack $w.txt.t -side top -fill both -expand yes
    set text_item [clean_name $text_item]
    set txt ""
    set sep ""
    foreach line [split $text_item "\n"] {
      append txt $sep [clean_name $line]
      set sep "\n"
    }
    $w.txt.t insert end $txt
    button $w.box -text Ok -width 6 -command "destroy $w"
    pack $w.txt -side top -pady 10 -expand yes -fill both 
    pack $w.box -side bottom -anchor s
}

proc post_popup {c text_item x y {keep_formatting 0}} {
    global ninfo_color base_mfont
    if { $text_item == "" } return
    set cx [$c canvasx $x]
    set cy [$c canvasy $y]
    set text_item [clean_name $text_item]
    set txt ""
    if { $keep_formatting == 1 } {
        set txt $text_item
    } else {
        set sep ""
        foreach line [split $text_item] {
          append txt $sep [clean_name $line]
          set sep "\n"
        }
    }
    val {ax1 ay1 ax2 ay2} [$c bbox all]
    val {lfx ufx} [$c xview]
    val {lfy ufy} [$c yview]
    set minx [expr $ax1+$lfx*($ax2-$ax1)+5]
    set maxx [expr $ax1+$ufx*($ax2-$ax1)-5]
    set miny [expr $ay1+$lfy*($ay2-$ay1)+5]
    set maxy [expr $ay1+$ufy*($ay2-$ay1)-5]
    set dx [expr $maxx-$minx]
    set dy [expr $maxy-$miny]
    set frac 5.0
    if [expr $cx < ($minx+$dx/$frac)] {
	if [expr $cy < ($miny+$dy/$frac)] {
	    # x: leftmost 20%
	    # y: top 20%
	    set anch nw
	    set cy [expr $cy+20]
	} elseif [expr $cy > ($maxy-$dy/$frac)] {
	    # x: leftmost 20%
	    # y: bottom 20%
	    set anch sw
	    set cy [expr $cy-20]
	} else {
	    # x: leftmost 20%
	    # y: center
	    set anch sw
	    set cy [expr $cy-20]
	}
    } elseif [expr $cx > ($maxx-$dx/$frac)] {
	if [expr $cy < ($miny+$dy/$frac)] {
	    # x: rightmost 20%
	    # y: top 20%
	    set anch ne
	    set cy [expr $cy+20]
	} elseif [expr $cy > ($maxy-$dy/$frac)] {
	    # x: rightmost 20%
	    # y: bottom 20%
	    set anch se
	    set cy [expr $cy-20]
	} else {
	    # x: rightmost 20%
	    # y: center
	    set anch se
	    set cy [expr $cy-20]
	}
    } else {
	if [expr $cy < ($miny+$dy/$frac)] {
	    # x: center
	    # y: top 20%
	    set anch n
	    set cy [expr $cy+20]
	} elseif [expr $cy > ($maxy-$dy/$frac)] {
	    # x: center
	    # y: bottom 20%
	    set anch s
	    set cy [expr $cy-20]
	} else {
	    # x: center
	    # y: center
	    set anch s
	    set cy [expr $cy-20]
	}
    }
    set r1 [$c create text $cx $cy -text $txt -anchor $anch \
		-font $base_mfont -fill red -justify left \
		-tag pOpUp_TxT]
    val {x1 y1 x2 y2} [$c bbox $r1]
    set r2 [$c create rectangle \
                                [expr $x1-3] [expr $y2+2] \
                                [expr $x2+3] [expr $y1-2] \
                                -fill yellow -outline yellow \
				-tag pOpUp_TxT]
    $c lower $r2 $r1
#    update
}


proc unpost_popup {c} {
    catch {$c delete pOpUp_TxT}
#    update
}


proc scale_font {font zoom_factor} {
    set idx [lsearch -exact $font -size]
    incr idx
    set old_size [lindex $font $idx]
    set new_size [expr round($old_size*$zoom_factor/100.0)]
    set new_font [lreplace $font $idx $idx $new_size]
    return [list $new_size $new_font]
}

proc scale_text_objects {c label new_size new_font zoom_factor} {
    if { $new_size < 4 } {
	foreach tag [$c find withtag $label] {
            if ![info exists ::store_zoom_text($c,$tag)] {
                set ::store_zoom_text($c,$tag) [$c itemcget $tag -text]
            }
	    $c itemconfigure $tag -text {}
	}
    } else {
	foreach tag [$c find withtag $label] {
	    set txt [$c itemcget $tag -text]
	    if { $txt == "" && [info exists ::store_zoom_text($c,$tag)] } {
		# Restore text if removed due to being too small to show
                set txt $::store_zoom_text($c,$tag)
                unset ::store_zoom_text($c,$tag)
	    }
	    $c itemconfigure $tag -font $new_font -text $txt
	}
    }
}

proc set_text_font_according_to_zoom {c zoom_factor} {
    # Update new font sizes and scale factor
    global base_mfont mfont base_sfont sfont base_sc sc
    val {new_msize new_mfont}  [scale_font $base_mfont $zoom_factor]
    val {new_ssize new_sfont}  [scale_font $base_sfont $zoom_factor]
    set mfont($c) $new_mfont
    set sfont($c) $new_sfont
    scale_text_objects $c "_IsTeXt_"  $new_msize $mfont($c) $zoom_factor
    scale_text_objects $c "_IsVaLuE_" $new_ssize $sfont($c) $zoom_factor
}

proc set_zoom_factor {c zoom_factor {xc 0} {yc 0} {reset_scroll 1}} {
    global cur_zoom_factor
    global base_mfont mfont base_sfont sfont base_sc sc
    # Clean up display by removing pop-up windows etc.
    unpost_popup $c
    catch {destroy .popup_data}
    # Compute scale factor
    set scale [expr $zoom_factor*1.0/($cur_zoom_factor($c)) ]
    set cur_zoom_factor($c) $zoom_factor
    # Update new font sizes and scale factor
    val {new_msize new_mfont}  [scale_font $base_mfont $zoom_factor]
    val {new_ssize new_sfont}  [scale_font $base_sfont $zoom_factor]
    set mfont($c) $new_mfont
    set sfont($c) $new_sfont
    set sc($c) [expr $base_sc*$zoom_factor/100.0]
    # Now scale current drawn objects and the text objects
    # If text size gets too small, remove the text (but restore later if needed)
    scale_text_objects $c "_IsTeXt_"  $new_msize $mfont($c) $zoom_factor
    scale_text_objects $c "_IsVaLuE_" $new_ssize $sfont($c) $zoom_factor
    $c scale all $xc $yc $scale $scale
    if { $reset_scroll == 1 } {
	$c config -scrollregion [$c bbox all]
    }
}

proc zoom_to_fit {c} {
    global cur_zoom_factor
    set ww [winfo width $c].0
    set wh [winfo height $c].0
    val {lx ly ux uy} [$c bbox all]
    set old_frac [expr $cur_zoom_factor($c)/100.0]
    set w [expr [expr abs($ux-$lx)].0/$old_frac]
    set h [expr [expr abs($uy-$ly)].0/$old_frac]
    set zfx [expr $ww/$w]
    set zfy [expr $wh/$h]
    if [expr $zfx < $zfy] { set fzf [expr 100.0*$zfx]
    } else { set fzf [expr 100.0*$zfy] }
    set_zoom_factor $c $fzf
}

proc zoom_out {c factor wx wy} {
    global cur_zoom_factor
    set xc [$c canvasx $wx]
    set yc [$c canvasy $wy]
    set_zoom_factor $c [expr $cur_zoom_factor($c)*$factor] $xc $yc
}

#
# Centers the given rectangle in the view for a canvas.
# Coordinates for the rectangle should be in canvas coordinates, not screen (mouse)
# coordinates.
#
# c - Canvas
# x1 - top left x
# y1 - top left y
# x2 - bottom right x
# y2 - bottom right y
#
proc center_rectangle {c x1 y1 x2 y2} {

    # Make sure all zooming is done
    update idletasks

    # Get the canvas dimensions
    val {cl ct cr cb} [$c cget -scrollregion]
    set cw [expr ($cr - $cl)]
    set ch [expr ($cb - $ct)]

    # puts [format "x1: %f, y1: %f, x2: %f, y2: %f" $x1 $y1 $x2 $y2]

    # Limit the provided rectangle to the canvas
    if [expr $x1 < $cl] { set x1 $cl }
    if [expr $x1 > $cr] { set x1 $cr }
    if [expr $x2 < $cl] { set x2 $cl }
    if [expr $x2 > $cr] { set x2 $cr }
    if [expr $y1 < $ct] { set y1 $ct }
    if [expr $y1 > $cb] { set y1 $cb }
    if [expr $y2 < $ct] { set y2 $ct }
    if [expr $y2 > $cb] { set y2 $cb }

    # puts [format "x1: %f, y1: %f, x2: %f, y2: %f (adjusted)" $x1 $y1 $x2 $y2]

    # Compute the center of the rectangle
    set crx [expr $x1 + ($x2 - $x1) / 2.0]
    set cry [expr $y1 + ($y2 - $y1) / 2.0]

    # Compute what percent the center of the rectangle is into the canvas.
    set crx_p [expr ($crx - $cl) / $cw];
    set cry_p [expr ($cry - $ct) / $ch];

    # Compute what percent of the canvas is in the center of the current scroll
    # position.
    val {ledge_p redge_p} [$c xview]
    val {tedge_p bedge_p} [$c yview]
    set cur_centerx_p [expr $ledge_p + ($redge_p - $ledge_p) / 2.0]
    set cur_centery_p [expr $tedge_p + ($bedge_p - $tedge_p) / 2.0]

    # Now compute the new left and top edge percentages to move to
    set new_ledge_p 0.0
    set new_tedge_p 0.0
    if [expr $crx_p <= $cur_centerx_p] {
        # Need to move left
        set deltax_p [expr $cur_centerx_p - $crx_p]
        # puts [format "moving left by: %f" $deltax_p]
        set new_ledge_p [expr $ledge_p - $deltax_p]
        if [expr $new_ledge_p < 0.0] {
            set new_ledge_p 0.0
        }
    } else {
        # Need to move right
        set deltax_p [expr $crx_p - $cur_centerx_p]
        # puts [format "moving right by: %f" $deltax_p]
        set new_ledge_p [expr $ledge_p + $deltax_p]
        if [expr $new_ledge_p > 1.0] {
            set new_ledge_p 1.0
        }
    }
    if [expr $cry_p <= $cur_centery_p] {
        # Need to move up
        set deltay_p [expr $cur_centery_p - $cry_p]
        # puts [format "moving up by: %f" $deltay_p]
        set new_tedge_p [expr $tedge_p - $deltay_p]
        if [expr $new_tedge_p < 0.0] {
            set new_tedge_p 0.0
        }
    } else {
        # Need to move down
        set deltay_p [expr $cry_p - $cur_centery_p]
        # puts [format "moving down by: %f" $deltay_p]
        set new_tedge_p [expr $tedge_p + $deltay_p]
        if [expr $new_tedge_p > 1.0] {
            set new_tedge_p 1.0
        }
    }
    # puts [format "new_ledge_p: %f, new_tedge_p: %f" $new_ledge_p $new_tedge_p]

    $c xview moveto $new_ledge_p
    $c yview moveto $new_tedge_p
}

proc base_zoom_to_rectangle {c x1 y1 x2 y2} {
    global cur_zoom_factor
    set old_frac [expr $cur_zoom_factor($c)/100.0]
    set rx1 [expr $x1/$old_frac]
    set ry1 [expr $y1/$old_frac]
    set rx2 [expr $x2/$old_frac]
    set ry2 [expr $y2/$old_frac]
    set w [expr $rx2-$rx1]
    set h [expr $ry2-$ry1]
    set ww [winfo width $c].0
    set wh [winfo height $c].0
    set zfx [expr $ww/$w]
    set zfy [expr $wh/$h]
    if [expr $zfx < $zfy] {
	set fzf [expr 100.0*$zfx]
    } else {
	set fzf [expr 100.0*$zfy]
    }
    set_zoom_factor $c $fzf
    # Now center the selected (and zoomed) rectangle
    set new_zoom_factor [expr $fzf/100.0]
    set nx1 [expr $rx1*$new_zoom_factor]
    set ny1 [expr $ry1*$new_zoom_factor]
    set nx2 [expr $rx2*$new_zoom_factor]
    set ny2 [expr $ry2*$new_zoom_factor]
    center_rectangle $c $nx1 $ny1 $nx2 $ny2
}

proc zoom_to_rectangle {c x1 y1 x2 y2} {
    # puts [format "Zooming to: ul(%d, %d) lr(%d, %d)\n" $x1 $y1 $x2 $y2]
    catch "base_zoom_to_rectangle $c $x1 $y1 $x2 $y2"
}

proc zoom_lock {c wx wy} {
    global zoom_anchor_point
    set x [$c canvasx $wx]
    set y [$c canvasy $wy]
    set zoom_anchor_point($c,orig_x) $x
    set zoom_anchor_point($c,orig_y) $y
    set zoom_anchor_point($c,fig) ""
    set zoom_anchor_point($c,txt) ""
    set zoom_anchor_point($c,zoom_movement) 0
}

proc zoom_move {c wx wy} {
    global zoom_anchor_point
    set x [$c canvasx $wx]
    set y [$c canvasy $wy]
    catch {$c delete $zoom_anchor_point($c,fig)}
    catch {$c delete $zoom_anchor_point($c,txt)}
    set ox $zoom_anchor_point($c,orig_x)
    set oy $zoom_anchor_point($c,orig_y)
    set deltax [expr $x - $ox]
    set deltay [expr $y - $oy]
    #
    if [expr abs($deltax) <= 2.0 && abs($deltay) <= 2.0] { return }
    set zoom_anchor_point($c,zoom_movement) 1
    if [expr $deltax < 0.0] {
	if [expr $deltay < 0.0] {
	    # NW
	    set zoom_anchor_point($c,fig) [$c create line $ox $oy $x $y \
						    -arrow last -fill red]
	    set zoom_anchor_point($c,txt) \
		    [$c create text [expr $x-2] [expr $y-2] \
						-anchor sw \
						-text "View more" \
						-fill red]
	    $c lower $zoom_anchor_point($c,fig) $zoom_anchor_point($c,txt)
	} else {
	    # SW
	    set zoom_anchor_point($c,fig) [$c create line $ox $oy $x $y \
						    -arrow last -fill red]
	    set zoom_anchor_point($c,txt) \
			[$c create text [expr $x-2] [expr $y+2] \
						-anchor ne \
						-text "Zoom to fit" \
						-fill red]
	    $c lower $zoom_anchor_point($c,fig) $zoom_anchor_point($c,txt)
	}
    } else {
	if [expr $deltay < 0.0] {
	    # NE
	    set zoom_anchor_point($c,fig) [$c create line $ox $oy $x $y \
						    -arrow last -fill red]
	    set zoom_anchor_point($c,txt) \
		    [$c create text [expr $x-2] [expr $y-2] \
						-anchor sw \
						-text "Undo last operation" \
						-fill red]
	    $c lower $zoom_anchor_point($c,fig) $zoom_anchor_point($c,txt)
	} else {
	    # SE
	    set zoom_anchor_point($c,fig) \
		[$c create line  $ox $oy $x $oy $x $y $ox $y $ox $oy \
		    -fill red -dash {1 3}]
	    set zoom_anchor_point($c,txt) \
		[$c create text [expr $x+2] [expr $y+2] \
						-anchor nw \
						-text "Zoom to rectangle" \
						-fill red]
	    $c lower $zoom_anchor_point($c,fig) $zoom_anchor_point($c,txt)
	}
    }
}

proc zoom_execute {c wx wy sx sy menu_function} {
    global zoom_anchor_point
    set x [$c canvasx $wx]
    set y [$c canvasy $wy]
    catch {$c delete $zoom_anchor_point($c,fig)}
    catch {$c delete $zoom_anchor_point($c,txt)}
    set ox $zoom_anchor_point($c,orig_x)
    set oy $zoom_anchor_point($c,orig_y)
    set deltax [expr $x - $ox]
    set deltay [expr $y - $oy]
 
    if [expr abs($deltax) <= 2.0 && abs($deltay) <= 2.0] {
	# Was it a cancelled zoom operation?
	if [expr $zoom_anchor_point($c,zoom_movement) == 1] { return }
	if { $menu_function != {} } {
	    eval $menu_function $c $wx $wy $sx $sy
	}
    } else {
	if [expr $deltax < 0.0] {
	    if [expr $deltay < 0.0] {
		# NW
		zoom_out $c 0.75 $wx $wy
	    } else {
		# SW
		zoom_to_fit $c
	    }
	} else {
	    if [expr $deltay < 0.0] {
		# NE
		after idle [list cb:expand_undo $c]
	    } else {
		# SE
		zoom_to_rectangle $c [expr round($ox)] [expr round($oy)] \
				     [expr round($x)] [expr round($y)]
	    }
	}
    }
}

proc clean_string {s} {
    if { [regexp "^{\(.*\)}$" $s -> ss] } {
        set s $ss
    }
    return $s
}

proc get_width_height {pfn} { 
    set c .c9999999 
    if { [winfo exists $c] == 0 } {
        frame $c
        frame $c.cc 
        canvas $c.cc.c
        set cur_zoom_factor($c.cc.c) 100.0
        set ::sc($c.cc.c) $::base_sc
        set ::tfont($c.cc.c) $::base_tfont
        set ::mfont($c.cc.c) $::base_mfont
        set ::sfont($c.cc.c) $::base_sfont
    }
    if [info exists ::pfn_size_info($pfn)] {
        return $::pfn_size_info($pfn)
    }
    catch {$c.cc.c delete all}
    eval $pfn $c.cc.c n999 0 0
    catch {unset ::value_tag(n999)}
    set res [bbox2fl [$c.cc.c bbox all]]
    set ::pfn_size_info($pfn) $res
    return $res
}   

proc bbox2fl {bb} {
    val {lx ly ux uy} $bb
    return [list [expr round($lx)] [expr round($ly)] \
                 [expr round($ux)] [expr round($uy)]]
}

change_fonts $::voss2_txtfont


proc balloon {w help} {
    bind $w <Any-Enter> "after 300 [list balloon:show %W [list $help]]"
    bind $w <Any-Leave> "destroy %W.balloon"
}

proc balloon:remove {w} {
    catch {destroy $w}
}

proc balloon:show {w arg} {
    set top $w.balloon
    catch {destroy $top}
    catch {destroy $::last_balloon_window}
    set ::last_balloon_window $top
    toplevel $top -bd 1 -bg black
    wm overrideredirect $top 1
    if {[string equal [tk windowingsystem] aqua]}  {
        ::tk::unsupported::MacWindowStyle style $top help none
    }   
    pack [message $top.txt -aspect 10000 -bg yellow -fg red \
        -font $::voss2_help_font -text $arg]
    set wmx [winfo rootx $w]
    set wmy [expr [winfo rooty $w]+[winfo height $w]]
    wm geometry $top [winfo reqwidth $top.txt]x[
        winfo reqheight $top.txt]+$wmx+$wmy
    raise $top
    after 2000 "balloon:remove $top"
}


############################################################

proc gui_execute_sub_process {w cmd logfile comment {cmd_file ""}} {

    set fp [open "|$cmd 2>@1"]
    fconfigure $fp -blocking 0

    set pids [pid $fp]

    set sw $w.t

    if ![winfo exists $sw] {
        frame $sw -relief flat
        pack $sw -side top -fill both -expand yes

        label $sw.l -text $comment -relief flat -font {Helvetica -12 bold}
        pack $sw.l -side top -pady 2 -fill x

        button $sw.b -text Interrupt -command "subproc:interrupt $w $pids"
        pack $sw.b -side bottom -pady 10
	
        scrollbar $sw.yscroll -command "$sw.t yview"
        scrollbar $sw.xscroll -orient horizontal -command "$sw.t xview"
        text $sw.t -setgrid 1 \
            -yscroll "$sw.yscroll set" -xscroll "$sw.xscroll set" \
            -font $::voss2_txtfont1 -bg lightgrey
        pack $sw.yscroll -side right -fill y
        pack $sw.xscroll -side bottom -fill x
        pack $sw.t -side top -fill both -expand yes
        update idletasks
    } else {
        button $sw.b -text Interrupt -command "subproc:interrupt $w $pids"
        pack $sw.b -side bottom -pady 10
	$sw.t delete 1.0 end
        update idletasks
    }
    if { $cmd_file != "" } {
        set cfp [open $cmd_file "r"]    
        $sw.t insert end [read $cfp]
        $sw.t insert end "\n"
        close $cfp
    }

    set logfp [open $logfile w]

    while 1 {
	if [eof $fp] {
	    fconfigure $fp -blocking 1
	    catch {close $fp} result options
	    break
	} else {
	    set txt [read $fp]
	    if [winfo exists $sw] {
		$sw.t insert end $txt
		$sw.t yview end
	    }
	    update
	}
	after 100
    }
    close $logfp
    catch {destroy $sw.b}
    if { $result == "" } {
	return "0 {}"
    }
    set errorcode [dict get $options -errorcode]
    set ecode [lrange $errorcode 2 end]
    return "$ecode $result"
}

proc subproc:interrupt {w pids} {
    foreach pid $pids {
        catch "exec kill -9 $pid"
    }
    set tw [winfo toplevel $w]
    catch {destroy $w}
    update
    wm geometry $tw ""
}
