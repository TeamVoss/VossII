;#########################################################################
;# Copyright 2020 Carl-Johan Seger
;# SPDX-License-Identifier: Apache-2.0
;#########################################################################

# ---------------------------------------------------
# Dependencies
# ---------------------------------------------------

# Put all icons in the (global) array ::icons()
foreach gfile [glob $::imagedir/*.gif] {
    set name [string range [lindex [split $gfile /] end] 0 end-4]
    set ::icon($name) [image create photo -file $gfile]
}

image create photo ::img::delete -format GIF -data {
    R0lGODlhEAAQAIABAIQAAP///yH5BAEAAAEALAAAAAAQABAAAAIjjI+pmwAc3HGy
    PUSvqYpuvWQg40FfSVacBa5nN6JYDI3mzRQAOw==
}

# ---------------------------------------------------
# Constants
# ---------------------------------------------------
set max_window_width	[expr round([winfo screenwidth .]*0.8)]
set max_window_height	[expr round([winfo screenheight .]*0.8)]
set min_window_width	300
set min_window_height	300
set name_window_width	150
set x_selection		""

set gcolor	"black"		;# line color
set selectc	"orange"	;# Selected nodes color
set fc		"white"		;# fill color
set value_col	"blue"		;# Value color

set hdl_src_cnt	0		;# Window counter for iHDL windows

set hdl_font	[font actual -*-Courier-Medium-B-Normal--*-120-*]

set base_sc	0.85		;# Scale factor

proc conn_rad	 {c} { return  [expr  1.0*$::sc($c)] }
proc inv_rad	 {c} { return  [expr  4.0*$::sc($c)] }
proc buf_ht	 {c} { return  [expr 30.0*$::sc($c)] }
proc buf_wid	 {c} { return  [expr 30.0*$::sc($c)] }
proc and_ht	 {c} { return  [expr 40.0*$::sc($c)] }
proc and_wid	 {c} { return  [expr 40.0*$::sc($c)] }
proc rect_ht	 {c} { return  [expr 15.0*$::sc($c)] }
proc rect_wid	 {c} { return  [expr 40.0*$::sc($c)] }
proc fsm_wid	 {c} { return  [expr 60.0*$::sc($c)] }
proc fsm_ht	 {c} { return  [expr 60.0*$::sc($c)] }
proc fsm_rad	 {c} { return  [expr 10.0*$::sc($c)] }
proc or_ht	 {c} { return  [expr 40.0*$::sc($c)] }
proc or_wid	 {c} { return  [expr 40.0*$::sc($c)] }
proc xor_sep	 {c} { return  [expr  5.0*$::sc($c)] }
proc wire_len	 {c} { return  [expr 10.0*$::sc($c)] }
proc min_sep	 {c} { return  [expr  8.0*$::sc($c)] }
proc min_txt_sep {c} { return  [expr  5.0*$::sc($c)] }
proc inp_ht	 {c} { return  [expr  8.0*$::sc($c)] }
proc ptr_ht	 {c} { return  [expr  3.0*$::sc($c)] }
proc letter_sz	 {c} { return  [expr  2.0*$::sc($c)] }
proc rtl_rad	 {c} { return  [expr 20.0*$::sc($c)] }
proc stop_sz	 {c} { return  [expr 4.0*$::sc($c)] }
proc out_sep	 {c} { return  [expr 20.0*$::sc($c)] }
proc decode_ht	 {c} { return  [expr 40.0*$::sc($c)] }
proc decode_wid	 {c} { return  [expr 40.0*$::sc($c)] }
proc pmux_ht_sep {c} { return  [expr 16*$::sc($c)] }

proc w2root {w} {
    return ".[lindex [split $w {.}] 1]"
}

proc min {a b} {
    if [expr $a < $b] {
	return $a
    } else {
	return $b
    }
}

proc max {a b} {
    if [expr $a > $b] {
	return $a
    } else {
	return $b
    }
}

proc sch:show_values_on_canvas {c} {
    set show [expr $::vstatus(show_value,$c) || $::vstatus(show_bdd_group,$c)]
    sch:sch_display_values $c $show
}

proc sc:inform_time_change {w args} {
    set root [w2root $w]
    set t $::vstatus(time,$root)
    if { $t == "" } {
	set t 0
    }
    fl_set_global_time $root $t
    foreach c [fl_get_active_sch_tab_windows $root] {
	sch:show_values_on_canvas $c
    }
    foreach c_vec [fl_get_active_fsms $root] {
	val {c vec} $c_vec
	fsm:show_current_state $c $root $vec
    }
}

proc create_ste_debugger {w} {
    catch {destroy $w}
    toplevel $w
    wm geometry $w 70x35-20+100
    set nb $w.nb
    ttk::notebook $nb -width 1200 -height 500
    pack $nb -side top -expand y -fill both
    set ::sch_window_cnt($w) 0
    set ::ste_debugger_notebook $nb
    set root [w2root $w]
    set ::vstatus(time,$root) 0
    trace add variable ::vstatus(time,$root) write \
	    "after idle [list sc:inform_time_change $w]"
}

proc nb:expand_selected_list {nlb y} {
    set w [winfo parent [winfo parent $nlb]]
    set idx [$nlb nearest $y]
    set sel [$nlb get $idx]
    if { $sel != "..." } { return; }
    set ::nodebrowser(max_cnt,$w) [expr $::nodebrowser(max_cnt,$w)+40]
    nb:update_nb_list $w $nlb
}

proc nb:create_node_browser {w} {
    set nb $w.nb
    $nb add [frame $nb.browser] -text "Node Browser"
    $nb add [frame $nb.waveform] -text "Waveforms"
    set wvf [wv:create_waveform_viewer $nb.waveform 10]
    $nb select 0
    set w $nb.browser
    set ww $w
    frame $w.f1 -width 400
    pack $w.f1 -side left -fill y
    set w $w.f1
 
    ttk::labelframe $w.search -text "Search:"
        labelframe $w.search.lbl -relief flat -text "Limit search to: " \
                -labelanchor w
        ttk::combobox $w.search.lbl.c -textvariable ::nodebrowser(source,$w) \
            -state readonly \
            -values {Inputs Outputs {User Given} All}
        set ::nodebrowser(source,$w) Outputs
 
        labelframe $w.search.pat_lbl -relief flat -text "Pattern: " \
                -labelanchor w
        ttk::combobox $w.search.pat_lbl.c \
		-textvariable ::nodebrowser(pattern,$w)
        bind $w.search.pat_lbl.c <KeyPress-Return> \
		[list $w.search.refresh invoke]
        set ::nodebrowser(pattern,$w) {*}
        button $w.search.refresh -text Refresh \
                -command [list nb:update_nb_list $w $w.lf.list]
    pack $w.search -side top -pady 10 -fill x
        pack $w.search.lbl -side top -fill x
            pack $w.search.lbl.c -side top
        pack $w.search.pat_lbl -side top -fill x
            pack $w.search.pat_lbl.c -side top
        pack $w.search.refresh -side top -fill x
 
    set f $w.lf
    frame $f -relief flat
    scrollbar $f.yscroll -command "$f.list yview"
    scrollbar $f.xscroll -orient horizontal -command "$f.list xview"
    listbox $f.list -setgrid 1 \
        -yscroll "$f.yscroll set" -xscroll "$f.xscroll set" -selectmode extended
    bind $f.list <Double-1> { nb:expand_selected_list %W %y }
    pack $f.yscroll -side right -fill y
    pack $f.xscroll -side bottom -fill x
    pack $f.list -side top -fill both -expand yes
 
    pack $f -side top -fill both -expand yes
 
    ttk::labelframe $w.opts -text "Options:"
	labelframe $w.opts.hier -text "Hierarchical drawing:" \
		-labelanchor w -relief flat
	    frame $w.opts.hier.space
	    ttk::checkbutton $w.opts.hier.cb \
		-variable ::nodebrowser(hierarchy,$w)
	    set ::nodebrowser(hierarchy,$w) 1
	labelframe $w.opts.levels -text "Levels of fanin:" \
		-labelanchor w -relief flat
	    ttk::combobox $w.opts.levels.c \
		-textvariable ::nodebrowser(levels,$w)
	    set ::nodebrowser(levels,$w) 20
    pack $w.opts -side top -fill x
        pack $w.opts.hier -side top -fill x
            pack $w.opts.hier.space -side left -fill x -expand yes
            pack $w.opts.hier.cb -side right -fill x
        pack $w.opts.levels -side top -fill x
            pack $w.opts.levels.c -side top -fill x -expand yes
 
    ttk::labelframe $w.operations -text "Operation:"
        button $w.operations.fanin -text Fanin \
            -command [list nb:draw_fanin $w $f.list]
        button $w.operations.waveform -text Waveform \
            -command [list nb:add_waveform $nb.waveform $f.list]
        button $w.operations.stop -text Stop \
            -command [list sl:add_stop_node $ww $f.list]
    pack $w.operations -side top -fill x
        pack $w.operations.fanin -side left -expand yes
        pack $w.operations.waveform -side left -expand yes
        pack $w.operations.stop -side left -expand yes

    set ::nodebrowser(max_cnt,$w) 40
 
    $w.search.refresh invoke
    return $nb.waveform
}


proc nb:update_nb_list {w lb} {
    $lb delete 0 end
    set src $::nodebrowser(source,$w)
    set pat $::nodebrowser(pattern,$w)
    set max_cnt $::nodebrowser(max_cnt,$w)
    set vecs [fl_get_vectors $w $src $pat $max_cnt]
    foreach v $vecs {
        $lb insert end $v
    }
}

proc sl:create_stop_node_browser {w} {
    set nb $w.nb
    set nb_browser_page $nb
    set w $nb.browser
    set ww $w
    frame $w.snf -width 400
    pack $w.snf -side left -fill y
    frame $w.f2
    pack $w.f2 -side left -fill both -expand yes
    set w $w.snf
    label $w.lbl -text "Stop nodes"
    pack $w.lbl -side top -fill x
    set f $w.lf
    frame $f -relief flat
    scrollbar $f.yscroll -command "$f.list yview"
    scrollbar $f.xscroll -orient horizontal -command "$f.list xview"
    listbox $f.list -setgrid 1 \
        -yscroll "$f.yscroll set" -xscroll "$f.xscroll set" -selectmode extended
    pack $f.yscroll -side right -fill y
    pack $f.xscroll -side bottom -fill x
    pack $f.list -side top -fill both -expand yes
    pack $f -side top -fill both -expand yes
 
    set ::stop_list_info($ww,nb) $f.list
    ttk::labelframe $w.operations -text "Operation:"
        button $w.operations.b -text Delete \
            -command [list sl:delete_stop_nd $w $f.list]
    pack $w.operations -side top -fill x
        pack $w.operations.b -side top
    sl:update_stop_node_list $w $w.lf.list
    bind $nb.browser <Map> [list sl:update_stop_node_list $w $w.lf.list]
}

proc sl:add_stop_node {w nlb} {
    foreach idx [$nlb curselection] {
	set nd [$nlb get $idx]
	fl_top_level_add_stop_nd $w $nd
    }
    sl:update_stop_node_list $w $::stop_list_info($w,nb)
}

proc sl:delete_stop_nd {w lb} {
    foreach idx [$lb curselection] {
	set nd [$lb get $idx]
	fl_delete_stop_nd $w $nd
    }
    sl:update_stop_node_list $w $lb
}

proc sl:update_stop_node_list {w lb} {
    $lb delete 0 end
    foreach v [fl_get_stop_nodes $w] {
        $lb insert end $v
    }
}

proc nb:draw_fanin {w lb} {
    foreach idx [$lb curselection] { lappend sel [$lb get $idx] }
    if [info exists sel] {
	if $::nodebrowser(hierarchy,$w) {
	    set draw_level 0
	} else {
	    set draw_level -1
	}
	fl_draw_fanin_by_name $w $draw_level $::nodebrowser(levels,$w) $sel
    }
}

proc nb:add_waveform {w lb} {
    foreach idx [$lb curselection] {
	wv:add_waveform $w [$lb get $idx]
    }
}

proc get_new_sch_canvas {w draw_level} {
    set nb $w.nb
    incr ::sch_window_cnt($w)
    set cc [format {%s.c%d} $nb $::sch_window_cnt($w)]
    $nb add [frame $cc] -text "Ckt $::sch_window_cnt($w)"
    set c [create_circuit_canvas $nb $cc]
    set ::cur_zoom_factor($cc) 100.0
    set ::cur_zoom_factor($c) 100.0
    set ::sc($c) $::base_sc
    set ::mfont($c) $::base_mfont
    set ::sfont($c) $::base_sfont
    $nb select $cc
    set ::sch_info(draw_level,$c) $draw_level
    return [list $cc $c]
}

proc set_scrollregion {c} {
    update idletasks
    $c configure -scrollregion [$c bbox all]
}


proc sch:toggle_show_value_buttons {c f} {
    set tl $f.tp
    set cb $tl.show
    set tt $tl.time
    set et $tt.time
    set pb $tt.plus
    set mb $tt.minus
    set root [w2root $c]
    if [expr $::vstatus(show_value,$c) || $::vstatus(show_bdd_group,$c)] {
        pack $tt -after $cb -side left -fill x -expand 1 \
            -pady 0 -padx 0
        pack $mb $et $pb -in $tt -side left -fill x -expand 1 \
            -pady 0 -padx 0
	sch:sch_display_values $c 1
    } else { 
        pack forget $mb $et $pb
        pack forget $tt
	sch:sch_display_values $c 0
    }
}

proc sch:create_time_point {c} {
    set w [winfo parent $c]
    set root [w2root $c]
    set f $w.menu
    set tl $f.tp
    set cbc $tl.show_bdd
    set cb $tl.show
    set tt $tl.time
    set et $tt.time
    set pb $tt.plus
    set mb $tt.minus
    
    frame $tl -relief flat -height 10
    pack $tl -side right
	set ::vstatus(show_value,$c) 0
        checkbutton $cbc -text "Show dependencies:" \
		    -variable ::vstatus(show_bdd_group,$c)  \
		    -command [list sch:toggle_show_value_buttons $c $f]
        checkbutton $cb -text "Show values:" \
		-variable ::vstatus(show_value,$c) \
                -command [list sch:toggle_show_value_buttons $c $f]
        frame $tt -relief flat -height 10
            entry $et -width 5 \
                -background black -foreground red -justify center \
		-textvariable ::vstatus(time,$root) \
		-validate all \
		-validatecommand {string is integer %P}

            button $pb -text "+" -command "incr ::vstatus(time,[w2root $c]) +1"
            button $mb -text "-" -command "incr ::vstatus(time,[w2root $c]) -1"
        pack $cbc -side left -pady 0 -padx 0
        pack $cb -side left -pady 0 -padx 0
        pack $tt -side left -pady 0 -padx 0 -expand yes
    #
}


proc sch:print {c} {
    set w .print_dialog
    catch "destroy $w"
    toplevel $w
    wm geometry $w +20+20

    bwidget::buttonbox $w.bb -orient horizontal
    pack $w.bb -side bottom -fill x
        $w.bb add print -text Print -command "sch:do_print $w $c"
        $w.bb add refresh -text Refresh -command "$w.pcb refresh"
        $w.bb add cancel -text Cancel -command "$w.pcb stop; destroy $w"

    set ::sch(print,paper_size)     Letter
    set ::sch(print,stretch)        1
    set ::sch(print,posterize)      0
    set ::sch(print,poster_h)       1
    set ::sch(print,poster_v)       1
    set ::sch(print,orientation)    landscape
    set ::sch(print,xref)           0
    set ::sch(print,filename)       schematics.pdf

    bwidget::canvasprintbox $w.pcb -output file \
        -pagesize  $::sch(print,paper_size) \
        -filename  $::sch(print,filename) \
        -posterize $::sch(print,posterize) \
        -hpagecnt  $::sch(print,poster_h) \
        -vpagecnt  $::sch(print,poster_v) \
        -orient    $::sch(print,orientation) \
        -stretch   $::sch(print,stretch)

    pack $w.pcb -side top -fill both -expand yes
    $w.pcb setcanvas $c

    frame $w.pcb.dummy -relief flat
    bwidget::checkbox $w.pcb.xref -labelpos nw -labeltext "Cross-references"
    grid configure $w.pcb.dummy $w.pcb.xref -row 3 -sticky ew
    $w.pcb.xref add xref -text "Create cross-reference file" \
            -command "sch:update_xrefs $w $c" -variable ::sch(print,xref)
}

proc sch:update_xrefs {w c} {
    if $::sch(print,xref) {
        Label_gates $c
    } else {
        Remove_Labels_on_Gates $c
    }
    $w.pcb refresh
}

proc sch:do_print {w c} {
    $w.pcb refresh
    set filename [$w.pcb getoutput]
    set ps_file /tmp/schematics
    set posterize [$w.pcb cget -posterize]
    set poster_h [$w.pcb cget -hpagecnt]
    set poster_v [$w.pcb cget -vpagecnt]
    if [expr $posterize && ($poster_h > 1 || $poster_v > 1) ] {
        for {set h 0} {$h < $poster_h} {incr h} {
            for {set v 0} {$v < $poster_v} {incr v} {
                lappend ps_files [format {%s%d.%d} $ps_file $h $v]
            }
        }
    } else {
        set ps_files [list $ps_file]
    }
    $w.pcb configure -filename $ps_file

    if [$w.pcb print] {
        if $::sch(print,xref) {
            set xref_ps [ckt:do_print_xref $c]
            # Merge PS files
            eval exec -- /usr/intel/bin/gs -dBATCH -dSAFER -DNOPAUSE \
                    -sDEVICE=pdfwrite -q -sOutputFile=$filename \
                    -f $ps_files $xref_ps
            eval exec /bin/rm -f $ps_files $xref_ps
        } else {
            eval exec -- /usr/intel/bin/gs -dBATCH -dSAFER -DNOPAUSE \
                    -sDEVICE=pdfwrite -q -sOutputFile=$filename \
                    -f $ps_files
            eval exec /bin/rm -f $ps_files
        }
        destroy $w
    } else {
        # Postscript generation failed
        eval exec /bin/rm -f $ps_files
        Remove_Labels_on_Gates $c
    }
}

proc sch:detach {nb tw} {
    if $::vstatus(inside_notebook,$tw) {
	wm manage $tw
	set lbl [$nb tab current -text]
	$nb forget current
	set ::vstatus(notebook_label,$tw) $lbl
	set ::vstatus(inside_notebook,$tw) 0
    } else {
	wm forget $tw
	$nb add [winfo parent $tw] -text $::vstatus(notebook_label,$tw)
	set ::vstatus(inside_notebook,$tw) 1
	pack configure $tw -fill both -anchor w -expand yes
    }
}

proc sch:destroy_canvas {nb tw} {
    destroy $tw
    fl_remove_active_sch_tab_window $tw.cc.c
}

proc sc:enter_sch_window  {ww x y X Y} {
    set ::wv_focus [focus]
    $ww focus all
    focus $ww
}   

proc create_circuit_canvas {nb mw} {
    global old_repeat_color old_repeat_loc cur_zoom_factor

    # Create a frame that the circuit canvas is contained in
    frame $mw.cc -relief flat
    pack $mw.cc -fill both -anchor w -expand yes

    set w $mw.cc
    set c $w.c

    scrollbar $w.hscroll -orient horiz -command "$w.c xview"
    pack $w.hscroll -side bottom -fill x
    scrollbar $w.vscroll -command "$w.c yview"
    pack $w.vscroll -side right -fill y

    canvas $c -background white  -xscrollcommand [list $w.hscroll set] \
				 -yscrollcommand [list $w.vscroll set] \
				 -xscrollincrement 1 \
				 -yscrollincrement 1 \
				 -width 1000 -height 800
    bind $c <2> "%W scan mark %x %y"
    bind $c <B2-Motion> "%W scan dragto %x %y"

    bind $c <Enter> { sc:enter_sch_window %W %x %y %X %Y }

    # Menus
    frame $w.menu -relief raised -bd 1
    pack $w.menu -side top -fill x

    ttk::button $w.menu.destroy -image ::img::delete \
		    -command [list sch:destroy_canvas $nb $mw]
    pack $w.menu.destroy -side right

    ttk::button $w.menu.detach -image $::icon(detach) \
	    -command "sch:detach $nb $w"
    pack $w.menu.detach -side left
    set ::vstatus(inside_notebook,$w) 1

    sch:create_time_point $c


    selection handle . provide_selection STRING

    set cur_zoom_factor($c) 100.0
    set ::sc($c)	    $::base_sc
    set ::tfont($c)	    $::base_tfont
    set ::mfont($c)	    $::base_mfont
    set ::sfont($c)	    $::base_sfont

    # Local highlights (this canvas only)
    $c bind all 1 { cb:set_sel_ws_to_col %W DarkOrchid1 }
    $c bind all 2 { cb:set_sel_ws_to_col %W magenta2 }
    $c bind all 3 { cb:set_sel_ws_to_col %W DarkOrange1 }
    $c bind all 4 { cb:set_sel_ws_to_col %W green }
    $c bind all 5 { cb:set_sel_ws_to_col %W gold3 }
    $c bind all 6 { cb:set_sel_ws_to_col %W yellow }
    $c bind all 7 { cb:set_sel_ws_to_col %W cyan }
    $c bind all 8 { cb:set_sel_ws_to_col %W purple }
    $c bind all 9 { cb:set_sel_ws_to_col %W brown }
    $c bind all 0 { cb:set_sel_ws_to_col %W _OrIgInAlCoLoR_ }

    $c bind RePeAt <ButtonRelease-2> {
	set_screen_view %W $old_repeat_loc %x %y
    }

    # Selection bindings
    bind $c <ButtonPress-1> "selection_lock %W %x %y 0"
    bind $c <B1-Motion> "selection_move %W %x %y 0"
    bind $c <ButtonRelease-1>  "selection_execute %W %x %y %X %Y 0"
    bind $c <Shift-ButtonPress-1> "selection_lock %W %x %y 1"
    bind $c <Shift-B1-Motion> "selection_move %W %x %y 1"
    bind $c <Shift-ButtonRelease-1>  "selection_execute %W %x %y %X %Y 1"

    # Zoom bindings
    bind $c <ButtonPress-3> "zoom_lock %W %x %y"
    bind $c <B3-Motion> "zoom_move %W %x %y"
    bind $c <ButtonRelease-3>  "zoom_execute %W %x %y %X %Y cb:sch_canvas_menu"

    # Mouse-wheel bindings for zooming in/out
    bind $c <Button-4> "zoom_out $c 1.1 %x %y"
    bind $c <Button-5> "zoom_out $c [expr {1.0/1.1}] %x %y"

    # Ctrl-F to move focus to search box
    bind $c <Control-f> "sch:start_search $w.menu"

    bind $c <Shift-KeyPress-Right>  { incr ::vstatus(time,[w2root %W]) +2 }
    bind $c <Shift-KeyPress-Left>  { incr ::vstatus(time,[w2root %W]) -2 }
    bind $c <KeyPress-Right>  { incr ::vstatus(time,[w2root %W]) +1 }
    bind $c <KeyPress-Left>  { incr ::vstatus(time,[w2root %W]) -1 }

    $c bind all <Enter> {
	set tags [%W gettags current]
        set ::sch_focus [focus] 
        %W focus all
        focus %W
	set node_tag [get_anon_name $tags]
	if { [is_value_tag $tags] } {
	    cb:post_extended_value %W $node_tag %x %y 0
	} elseif { [is_repeat_nd $tags] } {
	    set taglist [%W find withtag $node_tag]
	    catch {unset old_repeat_color}
	    foreach t $taglist {
		lappend old_repeat_color "$t [%W itemcget $t -fill]"
	    }
	    %W itemconfigure $node_tag -fill violet
	    break
	} else {
	    if { $node_tag != "current" && $node_tag != {} } {
		post_popup %W [list [fl_tag2vec %W $node_tag]] %x %y
		break
	    }
	}
    }
    $c bind all <Leave> {
        catch {focus $::sch_focus}
	set tags [%W gettags current]
	set node [get_anon_name $tags]
	if { [is_repeat_nd $tags] } {
	    foreach tcpair $old_repeat_color {
		lappend old_repeat_color [%W itemcget $t -fill]
		set t [lindex $tcpair 0]
		set cfc [lindex $tcpair 1]
		%W itemconfigure $t -fill $cfc
	    }
	    break
	} else {
	    if { $node != {current} } {
		unpost_popup %W
		break
	    }
	}
    }
    pack $c -fill both -expand yes

    # We're creating the sch viewer, and we want it to have focus by default.
    focus $c

    return $c
}

proc cb:update_time_and_colors {c} {
    set show [expr $::vstatus(show_value,$c) || $::vstatus(show_bdd_group,$c)]
    sch:sch_display_values $c $show
}

proc cb:sch_canvas_menu {c wx wy sx sy} {
    set selected [fl_get_anon_selected $c]
    selection_lock $c $wx $wy 0
    selection_execute $c $wx $wy $sx $sy 0
    set tags [$c gettags current]
    set nodes [get_anon_name $tags]
    set m $c.sel_op
    catch {destroy $m}
    menu $m -tearoff 0
    if { $nodes == "" } {
	return
    }
    set root [w2root $c]
    $m add command -label "New fanin" \
	-command "after idle [list fl_draw_fanin_by_tag $c \
		    $::sch_info(draw_level,$c) 1 $nodes]"
    $m add command -label "Draw inside" \
	-command "after idle \
	    [list fl_draw_inside $c $::sch_info(draw_level,$c) $nodes]"

    $m add command -label "Add waveform" \
	-command "wv:add_waveform [fl_c2w $c] [fl_tag2vec $c $nodes]"
    
    set mm $m.mark
    catch {destroy $mm}
    menu $mm -tearoff 0
    $m add cascade -label "Mark" -menu $mm

    set cols {DarkOrchid1 magenta2 DarkOrange1 green gold3 yellow \
              cyan purple brown}
    set laccs { "1" "2" "3" "4" "5" "6" "7" "8" "9" }

    foreach col $cols lacc $laccs {
        $mm add command -label "" -background $col \
                -command "cb:set_wire_color $c $nodes $col" \
                -accelerator $lacc
    }
    $mm add command -label "Unmark" \
	-command "cb:set_wire_color $c $nodes _OrIgInAlCoLoR_"

    set cb [winfo parent $c].menu.tp.show
    set te [winfo parent $c].menu.tp.time.time

    $m add command -label "Stop here" \
	    -command "unpost_popup $c; \
		      cb:hide_fanin $c $nodes 1; \
		      cb:restore_attributes $c"

    $m add command -label "Hide fanin" \
	    -command "unpost_popup $c; \
		      cb:hide_fanin $c $nodes 0; \
		      cb:restore_attributes $c"

    set mm $m.expand_fanin
    catch {destroy $mm}
    menu $mm -tearoff 0
    $m add cascade -label "Expand fanin cone" -menu $mm
    for {set amt 1} {$amt < 10} {incr amt} {
	$mm add command -label "by $amt" \
	    -command [list cb:expand_fanin $c $nodes $amt]
    }

    tk_popup $m $sx $sy
    return
}

proc get_anon_name {tags} {
    set res {}
    foreach tag $tags {
	if [regexp {^an[0-9][0-9][0-9][0-9][0-9][0-9]____[0-9]*$} $tag] {
;# NEED TO FIX EVENTUALLY!!!
;#            set override $tag
;#
        } elseif [regexp {^an[0-9][0-9][0-9][0-9][0-9][0-9]$} $tag] {
	    lappend res $tag
	}
    }
    if [info exists override] { return $override }
    return $res
}

proc is_value_tag {tags} {
    foreach tag $tags { if { $tag == "_IsVaLuE_" } { return 1 } }
    return 0
}

proc add_font_tags {c tag type_tag} {
    global mfont sfont
    set txt [$c itemcget $tag -text]
    $c addtag _TeXt_$txt withtag $tag
    $c addtag $type_tag withtag $tag
}

proc add_value_field {c aname x y {special_tag ""}} {
    global mfont sfont
    if { $special_tag != "" } {
	set ttag "$aname $special_tag"
    } else {
	set ttag $aname
    }
    set t [$c create text $x [expr $y-[min_sep $c]/2] \
		-tags $ttag -text {} -anchor sw -justify left \
		-font $sfont($c) -fill $::value_col]
    add_font_tags $c $t _IsVaLuE_
    $c bind $t <ButtonPress-3> "cb:add_value_menu $c $aname %X %Y; break"
}

proc draw_three_dots {c lx ly ux uy tag} {
    set dx [expr ($ux-$lx)]
    set dy [expr ($uy-$ly)]
    set x1 [expr $lx+1.0*$dx/4.0]
    set x2 [expr $lx+2.0*$dx/4.0]
    set x3 [expr $lx+3.0*$dx/4.0]
    set y1 [expr $ly+1.0*$dy/4.0]
    set y2 [expr $ly+2.0*$dy/4.0]
    set y3 [expr $ly+3.0*$dy/4.0]
    $c create oval [expr $x1-[conn_rad $c]] [expr $y1+[conn_rad $c]] \
		   [expr $x1+[conn_rad $c]] [expr $y1-[conn_rad $c]] \
		   -fill black -outline black -tags $tag
    $c create oval [expr $x2-[conn_rad $c]] [expr $y2+[conn_rad $c]] \
		   [expr $x2+[conn_rad $c]] [expr $y2-[conn_rad $c]] \
		   -fill black -outline black -tags $tag
    $c create oval [expr $x3-[conn_rad $c]] [expr $y3+[conn_rad $c]] \
		   [expr $x3+[conn_rad $c]] [expr $y3-[conn_rad $c]] \
		   -fill black -outline black -tags $tag
}

proc add_three_horizontal_dots {c left_x right_x y tag} {
    set x1 $left_x
    set xn $right_x
    set x11 [expr $x1+($xn-$x1)/4.0]
    set x1c [expr $x1+2.0*($xn-$x1)/4.0]
    set x1n [expr $x1+3.0*($xn-$x1)/4.0]
    set yc  $y
    $c create oval [expr $x11-[conn_rad $c]] [expr $yc+[conn_rad $c]] \
		   [expr $x11+[conn_rad $c]] [expr $yc-[conn_rad $c]] \
		   -fill black -outline black -tags $tag
    $c create oval [expr $x1c-[conn_rad $c]] [expr $yc+[conn_rad $c]] \
		   [expr $x1c+[conn_rad $c]] [expr $yc-[conn_rad $c]] \
		   -fill black -outline black -tags $tag
    $c create oval [expr $x1n-[conn_rad $c]] [expr $yc+[conn_rad $c]] \
		   [expr $x1n+[conn_rad $c]] [expr $yc-[conn_rad $c]] \
		   -fill black -outline black -tags $tag
}

proc cb:add_value_menu {c aname x y} {
    set m $c.sm
    catch {destroy $m}
    menu $m -tearoff 0
    $m add command -label "Show expanded value" \
        -command "cb:post_extended_value $c $aname $x $y 1"
    tk_popup $m $x $y
}

proc cb:post_extended_value {c aname x y sep_window} {
    if { $sep_window == 1 } {
        set time [fl_get_global_time $c]
	post_big_popup_window [fl_get_complete_value $c $aname] \
                          [format {Value for %s at time %d} \
				  [fl_tag2vec $c $aname] $time]
    } else {
	post_popup $c [fl_get_long_value $c $aname] $x $y 1
    }
}

proc grow_bbox {c} {
    val {x1 y1 x2 y2} [$c bbox all]
    return [list [expr $x1-30] [expr $y1-50] [expr $x2+30] [expr $y2+50]]
}

proc selection_lock {c wx wy shift} {
    global selection_anchor_point
    # Grab the focus when the user clicks on the canvas, so the key bindings
    # for the canvas will be active (like for coloring signals).
    focus $c
    set x [$c canvasx $wx]
    set y [$c canvasy $wy]
    set selection_anchor_point($c,orig_x) $x
    set selection_anchor_point($c,orig_y) $y
    set selection_anchor_point($c,fig) ""
    set selection_anchor_point($c,txt) ""
    set selection_anchor_point($c,selection_movement) 0
}

proc selection_move {c wx wy shift} {
    global selection_anchor_point
    set x [$c canvasx $wx]
    set y [$c canvasy $wy]
    catch {$c delete $selection_anchor_point($c,fig)}
    catch {$c delete $selection_anchor_point($c,txt)}
    if ![info exists selection_anchor_point($c,orig_x)] { return }
    if ![info exists selection_anchor_point($c,orig_y)] { return }
    set ox $selection_anchor_point($c,orig_x)
    set oy $selection_anchor_point($c,orig_y)
    set deltax [expr $x - $ox]
    set deltay [expr $y - $oy]
    #
    if [expr abs($deltax) <= 2.0 && abs($deltay) <= 2.0] { return }
    set selection_anchor_point($c,selection_movement) 1
    set selection_anchor_point($c,fig) \
	[$c create line  $ox $oy $x $oy $x $y $ox $y $ox $oy \
	    -fill red -dash {1 3}]
    catch {$c lower $selection_anchor_point($c,fig) \
		    $selection_anchor_point($c,txt)}
}

proc is_wire {c tag} {
    foreach stag [$c gettags $tag] {
	if { $stag == "_WiRe_" } { return 1 }
    }
    return 0
}

proc selection_execute {c wx wy sx sy shift} {
    global selection_anchor_point
    if ![info exists selection_anchor_point($c,orig_x)] { return }
    if ![info exists selection_anchor_point($c,orig_y)] { return }
    set x [$c canvasx $wx]
    set y [$c canvasy $wy]
    catch {$c delete $selection_anchor_point($c,fig)}
    catch {$c delete $selection_anchor_point($c,txt)}
    set ox $selection_anchor_point($c,orig_x)
    set oy $selection_anchor_point($c,orig_y)
    set deltax [expr $x - $ox]
    set deltay [expr $y - $oy]
 
    if [expr abs($deltax) <= 2.0 && abs($deltay) <= 2.0] {
	# Was it a cancelled area selection operation?
	if {$selection_anchor_point($c,selection_movement) == 1} { return }
	set sel_tags [get_anon_name [$c gettags current]]
    } else {
	set rtags [$c find overlap [min $x $ox] [min $y $oy] \
				      [max $x $ox] [max $y $oy]]
	set tags {}
	set wtags {}
	foreach tag $rtags {
	    if [is_wire $c $tag] {
		eval lappend wtags [get_anon_name [$c gettags $tag]]
	    } else {
		eval lappend tags [get_anon_name [$c gettags $tag]]
	    }
	}
	if {$tags == {}} { set tags $wtags }
	set sel_tags [get_anon_name [lsort -unique $tags]]
    }
    if { $sel_tags == {} } {
	set nodes "_"
    } else {
	foreach t $sel_tags {
	    foreach vec [fl_tag2vec $c $t] {
		lappend nodes [clean_name $vec]
	    }
	}
    }
    if $shift {
	fl_set_selection $c "MODIFY_SELECTION" $nodes
    } else {
	fl_set_selection $c "SET_SELECTION" $nodes
    }
}

proc cb:expand_undo {c} {
    unpost_popup $c
    set old_zoom $::cur_zoom_factor($c)
    set_zoom_factor $c 100.0
    fl_undo_expansion $c
    set_zoom_factor $c $old_zoom
    catch {cb:restore_attributes $c}
}


proc find_driver_loc {c node xdefault ydefault} {
    set objs {}
    foreach tag [$c find withtag $node&&_IsVaLuE_] {
	set ignore 0
	foreach stag [$c gettags $tag] {
	    if { $stag == "_WiRe_" } { set ignore 1; break }
	    if { $stag == "RePeAt" } { set ignore 1; break }
	}
	if $ignore { continue }
	lappend objs $tag
    }
    if { $objs == {} } { return [list $xdefault.0 $ydefault.0] }
    val {lx ly ux uy} [eval $c bbox $objs]
    set xdr [expr ($ux+0.0)]
    set ydr [expr (($uy+$ly)/2.0)]
    return [list $xdr $ydr]
}

set ::expanding_fanin 0

proc cb:expand_fanin {c node amt} {
    if {$::expanding_fanin} {
        # ignore fanin while another one is in progress
        return
    }
    set ::expanding_fanin 1
    set code [catch {
        unpost_popup $c
        # Find the fraction of the visible region for the cursor (before!)
        val {b_x b_y} [find_driver_loc $c $node 0 0]
        val {b_left b_right} [$c xview]
        val {b_top b_bottom} [$c yview]
        val {b_s_left b_s_top b_s_right b_s_bottom} [$c cget -scrollregion]
        set xfrac [expr ((($b_x-$b_s_left)/($b_s_right-$b_s_left)) - \
                        $b_left)/($b_right-$b_left)]
        set yfrac [expr ((($b_y-$b_s_top)/($b_s_bottom-$b_s_top)) - \
                        $b_top)/($b_bottom-$b_top)]
        # Now expand fanin
	set old_zoom $::cur_zoom_factor($c)
	set_zoom_factor $c 100.0
        fl_expand_fanin $c $::sch_info(draw_level,$c) $amt $node
	set_zoom_factor $c $old_zoom
        cb:restore_attributes $c
        adjust_circuit_window [winfo parent $c] $c 0
        # Make sure state is updated
        update idletasks
        # Make sure new expanded logic is placed at the expansion point
	cb:update_time_and_colors $c
        val {new_dr_x new_dr_y} [find_driver_loc $c $node 0 0]
        val {top bottom} [$c yview]
        val {left right} [$c xview]
        val {s_left s_top s_right s_bottom} [$c cget -scrollregion]

        set xpos [expr {($new_dr_x+0.0-$s_left)/($s_right-$s_left) - \
                        ($right-$left)*$xfrac}]
        set ypos [expr {($new_dr_y+0.0-$s_top)/($s_bottom-$s_top) - \
                        ($bottom-$top)*$yfrac}]
        $c xview moveto $xpos
        $c yview moveto $ypos
    } result]
    set ::expanding_fanin 0
    return -code $code $result
}

proc cb:add_stop_node {c node} {
    unpost_popup $c
    fl_add_stop_nd $c $node
    cb:restore_attributes $c
}

proc cb:hide_fanin {c node make_stop_node} {
    unpost_popup $c
    fl_hide_fanin $c $node $make_stop_node
    cb:restore_attributes $c
}

proc get_stored_default_color {c item} {
    global fc gcolor
    set tp [$c type $item]
    if { $tp == "line" || $tp == "text" } {
        set color $gcolor
    } else {
        set color $fc
    }
    regexp {_DeFaUlTcOlOr_(\S*)} [$c gettags $item] -> color
    return $color
}

proc do_not_change_color {c item} {
    return [regexp {_DoNotChangeColor_} [$c gettags $item]]
}

proc is_WiRe {c item} {
    return [regexp {_WiRe_} [$c gettags $item]]
}

proc record_orig_color {c item} {
    if [regexp {_DeFaUlTcOlOr_} [$c gettags $item]] { return }
    if { [catch {set orig_color [$c itemcget $item -fill]}] == 0 } {
	set color_tag [format {_DeFaUlTcOlOr_%s} $orig_color]
	$c addtag $color_tag withtag $item
    }
}

proc cb:prim_set_wire_color {c name color} {
    global gcolor fc
    if ![winfo exists $c] { return }
    set items [$c find withtag "$name&&!_IsTeXt_"]
    set cname [clean_name $name]
    eval lappend items [$c find withtag "$cname&&!_IsTeXt_"]
    foreach item $items {
	if [do_not_change_color $c $item] { continue; }
	if { [$c type $item] == "line" && ![is_WiRe $c $item] } { continue; }
	if { $color == "_OrIgInAlCoLoR_" } {
	    set dcol [get_stored_default_color $c $item]
	    catch {$c itemconfigure $item -fill $dcol}
	} else {
	    record_orig_color $c $item
	    if { [catch {$c itemconfigure $item -fill $color}] == 0 } {
		lappend ::changed_colors($c) $item
	    }
	}
    }
}

proc cb:restore_original_colors {c} {
    if [info exists ::changed_colors($c)] {
        foreach item $::changed_colors($c) {
	    set dcol [get_stored_default_color $c $item]
	    $c itemconfigure $item -fill $dcol
        }
	unset ::changed_colors($c)
    }
}

proc cb:restore_attributes {c} {
    fl_update_colors $c
    # Should restore backannotation values and other attributes
}

proc cb:set_wire_color {c tags color} {
    set tags [get_anon_name $tags]
    if { $tags == {} } { return; }
    fl_set_sch_highlight_color $c $color [get_anon_name $tags]
}

proc cb:set_sel_ws_to_col {c color} {
    set tags [$c gettags current]
    cb:set_wire_color $c $tags $color
}

proc adjust_circuit_window {w c first} {
    global max_window_width max_window_height
    global min_window_width min_window_height
    val {x1 y1 x2 y2} [$c bbox all]
    set xsize [expr $x2-$x1+30]
    set ysize [expr $y2-$y1+100]
    set is_scrolled 0
    if {$xsize > $max_window_width} {
	set xsize $max_window_width
	set is_scrolled 1
    }
    if {$ysize > $max_window_height} {
	set ysize $max_window_height
	set is_scrolled 1
    }
    if { $xsize < $min_window_width } {
	set xsize $min_window_width
    }
    if { $ysize < $min_window_height } {
	set ysize $min_window_height
    }
    if { $first == 1 } {
	catch {wm geometry $w [join [list = $xsize x $ysize] ""]}
	update idletasks
	zoom_to_fit $c
    }
    $c config -scrollregion [grow_bbox $c]
}

proc set_color {c color nodelist} {
    if { [winfo exists $c] } {
	foreach node $nodelist {
	    catch {$c itemconfigure $node -fill $color}
	}
    }
}

proc restore_color {c node} {
    global gcolor fc
    set tags [$c find withtag $node]
    foreach tag $tags {
	switch [$c type $tag] {
	    rectangle	{$c itemconfigure $tag -outline $gcolor -fill $fc}
	    oval	{$c itemconfigure $tag -outline $gcolor -fill $fc}
	    polygon	{$c itemconfigure $tag -outline $gcolor -fill $fc}
	    arc		{$c itemconfigure $tag -outline $gcolor -fill $fc}
	    line	{$c itemconfigure $tag -fill $gcolor}
	    text	{$c itemconfigure $tag -fill $gcolor}
	}
    }
}


proc Label_gates {c} {
    global name2idx idx2name snamelist

    # Label the wires to prepare for name map
    set idx 1
    catch {unset name2idx}
    catch {unset idx2name}
    foreach node [array names ::lbl_tag] {
	set entryl $::lbl_tag($node)
	set done 0
	foreach entry $entryl {
	    set ctmp   [lindex $entry 0]
	    if { $ctmp == $c } {
		if { [winfo exists $c] } {
		    set txt_tag [lindex $entry 1]
		    $c itemconfigure $txt_tag -text $idx
		    if { $done == 0 } {
			set name2idx($node) $idx
			set idx2name($idx) $node
			lappend namelist $node
			incr idx
			set done 1
		    }
		}
	    }
	}
    }
    set snamelist [lsort $namelist]
}

proc ckt:do_print_xref {c} {
    global name2idx idx2name snamelist
    Label_gates $c
    set filename /tmp/xref.txt
    if { [catch {set fp [open $filename w]}] != 0 } {
	report_error [format "WARNING: Cannot open %s for writing" $filename]
	return
    }
    puts $fp "Index->node map:"
    puts $fp "================"
    for {set j 1} {$j <= [llength $snamelist] } {incr j} {
        set nds [fl_tag2vec $c $idx2name($j)]
	puts $fp [format {%3d --> %s} $j [clean_name $nds]]
    }
    puts $fp ""
    puts $fp "Node->index map:"
    puts $fp "================"
    set last ""
    foreach anm $snamelist {
        set nm [fl_tag2vec $c $anm]
	if { $last != $nm } {
	    puts $fp [format {%s --> %s} [clean_name $nm] $name2idx($anm)]
	    set last $nm
	}
    }
    close $fp
    Remove_Labels_on_Gates $c
    exec /usr/bin/enscript -q -p/tmp/xref.ps -2r -fTimes-Roman10 /tmp/xref.txt
    return /tmp/xref.ps
}


proc Remove_Labels_on_Gates {c} {
    global name2idx idx2name

    # Remove labels
    foreach node [array names ::lbl_tag] {
	set entryl $::lbl_tag($node)
	foreach entry $entryl {
	    set ctmp   [lindex $entry 0]
	    if { $ctmp == $c } {
		if { [winfo exists $c] } {
		    set txt_tag [lindex $entry 1]
		    $c itemconfigure $txt_tag -text ""
		}
	    }
	}
    }
}

proc sch:sch_display_values {c show} {
    if ![winfo exists $c] { return; }
    if { $show } {
	if { $::vstatus(show_bdd_group,$c) } {
	    fl_set_color_by_bdd_prefix on $c
	    fl_update_colors $c
	} else {
	    fl_set_color_by_bdd_prefix off $c
	    fl_update_colors $c
	}
	if { $::vstatus(show_value,$c) } {
	    foreach tag [$c find withtag _IsVaLuE_] {
		set aname [get_anon_name [$c gettags $tag]]
		    set sv [fl_get_short_value_from_aname $c $aname 0]
		    $c itemconfigure $tag -text [clean_string $sv]
	    }
	} else {
	    foreach tag [$c find withtag _IsVaLuE_] {
		$c itemconfigure $tag -text ""
	    }
	}
    } else {
        foreach tag [$c find withtag _IsVaLuE_] {
            $c itemconfigure $tag -text ""
        }
	fl_set_color_by_bdd_prefix off $c
	fl_update_colors $c
    }
}

proc clear_value_tags { cur_canvas } {
    global value_tag

    foreach node [array names value_tag] {
	catch {unset new_entryl}
	set entryl $value_tag($node)
	foreach entry $entryl {
	    set c   [lindex $entry 0]
	    if { $c != $cur_canvas } {
		lappend new_entryl $entry
	    }
	}
	if { [info exists new_entryl] } {
	   set value_tag($node) $new_entryl
	} else {
	    unset value_tag($node)
	}
    }

    catch {ccsn_remove_canvas $cur_canvas}
}

proc is_repeat_nd {tags} {
    foreach tag $tags {
	if { [string compare $tag RePeAt] == 0 } {
	    return 1
	}
    }
    return 0
}


proc post_excitation {c node} {
    post_popup_window [fl_get_excitation $c $node 20]
}

proc popup_excitation {c node x y} {
    post_popup $c [fl_get_excitation $c $node 3] $x $y
}

proc post_node_name {c node} {
    post_popup_window [fl_tag2vec $c $node]
}

proc popup_node_name {c node x y} {
    post_popup $c [fl_tag2vec $c $node] $x $y
}

proc vec_draw {vec_size args} {
    global gcolor mfont

    set argcnt [llength $args]
    set cmd [lrange $args 0 [expr $argcnt-5]]
    set c [lindex $args [expr $argcnt-4]]
    set tag [lindex $args [expr $argcnt-3]]
    set x [lindex $args [expr $argcnt-2]]
    set y [lindex $args [expr $argcnt-1]]
    # Limit the amount of extra space added for values
    if { $vec_size > 64 } { set vec_size 16 }
    set xn [expr round($x-$vec_size*[letter_sz $c]/4)]
    $c create line $xn $y $x $y -fill $gcolor -tags "$tag _WiRe_" -width 3
    lappend cmd $c $tag $xn $y
    eval $cmd
}

proc fanout_connect {c tag x1 y1 x2 y2} {
    global gcolor mfont
    set xmid [expr ($x1+2*[min_sep $c])]
    if { [fl_is_vector $c $tag] } {
	$c create line $x1 $y1 $xmid $y1 $xmid $y2 $x2 $y2 \
		-fill $gcolor -tags $tag -width 3
    } else {
	$c create line $x1 $y1 $xmid $y1 $xmid $y2 $x2 $y2 \
		-fill $gcolor -tags $tag
    }
}


proc and_ht_needed {c ands} {
    set inputs 0
    foreach a $ands {
        if { $a != {} } { incr inputs; }
    }
    set sep [expr ([and_ht $c]/$inputs)]
    set min_sep [expr 3*[inv_rad $c]]
    if [expr $sep < $min_sep] {
        set tot_height [expr $inputs*$min_sep]
    } else {
        set tot_height [and_ht $c]
    }
    return [expr $tot_height + [min_sep $c]]
}

proc or_ht_needed {c ors} {
    set inputs 0
    foreach a $ors {
        if { $a != {} } { incr inputs; }
    }
    set sep [expr ([or_ht $c]/$inputs)]
    set min_sep [expr 3*[inv_rad $c]]
    if [expr $sep < $min_sep] {
        set tot_height [expr $inputs*$min_sep]
    } else {
        set tot_height [or_ht $c]
    }
    return [expr $tot_height + [min_sep $c]]
}

proc draw_loop_merge {c tag x y} {
    $c create oval [expr ($x-1)] [expr ($y-1)] [expr ($x+1)] [expr ($y+1)] \
		       -outline $::gcolor -fill $::fc -tags $tag
    lappend inp_locs $x $y
    lappend inp_locs $x $y
    return [list $inp_locs [list $x $y]]
}

proc draw_loop_source {c tag x y} {
    return [list {} [list $x $y]]
}

proc draw_or_and_gate {n orlist out_neg c tag x y} {
    global gcolor mfont
    global fc sfont
    add_value_field $c $tag $x $y
    if {$out_neg != ""} {
	set nx [expr ($x-[inv_rad $c]*2)]
	$c create oval [expr ($x-2*[inv_rad $c])] [expr ($y-[inv_rad $c])] \
		       $x [expr ($y+[inv_rad $c])] \
		       -outline $gcolor -fill $fc -tags $tag
    } else {
	set nx $x
    }
    set nbr_ors [llength $orlist]
    set inpneg1 {}
    foreach a $orlist { lappend inpneg1 "" }
    # The final AND gate
    $c create polygon \
        [expr ($nx-[and_wid $c])] [expr ($y-[and_ht $c]/2)] \
        [expr ($nx-[and_wid $c])] [expr ($y+[and_ht $c]/2)] \
        [expr ($nx-[and_wid $c])] [expr ($y+[and_ht $c]/2)] \
        [expr ($nx-[and_wid $c]/4)] [expr ($y+[and_ht $c]/2)] \
        [expr 1.0*$nx] [expr $y] \
        [expr ($nx-[and_wid $c]/4)] [expr ($y-[and_ht $c]/2)] \
        [expr ($nx-[and_wid $c])] [expr ($y-[and_ht $c]/2)] \
        [expr ($nx-[and_wid $c])] [expr ($y-[and_ht $c]/2)] \
        -smooth yes -outline $gcolor -fill $fc \
        -tags $tag
    set orx [expr $nx-[and_wid $c]]
    set tot_or_ht 0
    foreach a $orlist {
        set tot_or_ht [expr $tot_or_ht + [or_ht_needed $c $a]]
    }
    set ytop [expr $y - ($tot_or_ht/2)]
    set ybot [expr $y + ($tot_or_ht/2)]
    set idx 0
    set ya $ytop 
    foreach a $orlist {
	set ainvs {}
        set cnt 0
        set icnt 0
        set remap {}
	foreach p $a {
            incr cnt
	    if { $p == "+" } { 
                incr icnt
		lappend ainvs {}
                lappend remap $cnt
	    } elseif { $p == "-" } {
                incr icnt
		lappend ainvs {Y}
                lappend remap $cnt
	    }
	}
        set sz [or_ht_needed $c $a]
        set ya [expr $ya+[expr $sz/2]]
	set ains($idx) [draw_or_pat 1 $ainvs "" "" $c $tag $orx $ya $remap]
	incr idx
        set or_y_locs($idx) $ya
        set ya [expr $ya+[expr $sz/2]]
    }
    $c create line [expr $orx-[inv_rad $c]] $or_y_locs(1) \
		   $orx $or_y_locs(1) \
		   $orx [expr $y-([and_ht $c]/2)] \
                   -fill $gcolor \
		   -tags _DoNotChangeColor_
    $c create line $orx [expr $y+([and_ht $c]/2)] \
		   $orx $or_y_locs($nbr_ors) \
		   [expr $orx-[inv_rad $c]]  $or_y_locs($nbr_ors)  \
		   -fill $gcolor \
		   -tags _DoNotChangeColor_
    #
    set ix [expr $orx-[or_wid $c]-[inv_rad $c]-[min_sep $c]]
    set lmx [expr  $ix-$n*[min_sep $c]]
    set inps {}
    set isep [expr ($tot_or_ht/($n+1))]
    set cinpy [expr ($ytop+$isep)]
    for {set i 0} {$i < $n} {incr i} {
	set xl [expr $ix-($n-$i-1)*[min_sep $c]]
        set itag [format {%s____%d} $tag [expr $i+1]]
	$c create line [expr $lmx-[min_sep $c]] $cinpy $xl $cinpy -fill $gcolor\
		   -tags "$itag _WiRe_"
        set min_y($i) $cinpy
        set max_y($i) $cinpy
        set connections($i) [list $cinpy]
	lappend inps [expr round($lmx-[min_sep $c])] [expr round($cinpy)]
	set cinpy [expr ($cinpy+$isep)]
    }
    set idx 0
    foreach a $orlist {
	set conns [lindex $ains($idx) 0]
	incr idx
	set i 0
	set inp 0
	foreach t $a {
	    incr i
	    if { $t == {} } { continue }
	    set conx [lindex $conns [expr 2*$inp]]
	    set cony [lindex $conns [expr 2*$inp+1]]
	    incr inp
	    set wx [expr $ix-($n-$i)*[min_sep $c]]
            set itag [format {%s____%d} $tag [expr $i]]
	    $c create line $wx $cony $conx $cony -fill $gcolor \
		-tags "$itag _WiRe_"
            set im1 [expr $i-1]
            if [expr $cony < $min_y($im1)] { set min_y($im1) $cony }
            if [expr $cony > $max_y($im1)] { set max_y($im1) $cony }
            lappend connections($im1) $cony
	}
    }
    set cinpy [expr ($ytop+$isep)]
    for {set i 0} {$i < $n} {incr i} {
        set xl [expr $ix-($n-$i-1)*[min_sep $c]]
        set lytop $min_y($i)
        set lybot $max_y($i)
        set itag [format {%s____%d} $tag [expr $i+1]]
	$c create line $xl $lytop $xl $lybot -fill $gcolor \
		   -tags "$itag _WiRe_"
        foreach py $connections($i) {
            if [expr $py > $lytop && $py < $lybot] {
                set mrad [conn_rad $c]
                set conn [$c create oval [expr $xl-$mrad] [expr $py-$mrad] \
                               [expr $xl+$mrad] [expr $py+$mrad] \
                               -outline black -fill black -tags "$itag _WiRe_"]
                record_orig_color $c $conn
            }
        }
	set cinpy [expr ($cinpy+$isep)]
    }
    set lmy [expr ($ybot+2)]
    set rmy [expr ($ytop-2)]
    $c create line  $lmx $lmy $lmx $rmy $nx $rmy $nx $lmy $lmx $lmy \
	    -fill $gcolor -tags [list $tag _DoNotChangeColor_] -dash {1 3}
    return [list $inps [list [expr round($x)] [expr round($y)]]]
}

proc draw_aoi12 {c tag x y} {
    global gcolor mfont
    global fc sfont
    add_value_field $c $tag $x $y
    set nx [expr ($x-[inv_rad $c]*2)]
    $c create oval [expr ($x-2*[inv_rad $c])] [expr ($y-[inv_rad $c])] \
		   $x [expr ($y+[inv_rad $c])] \
		   -outline $gcolor -fill $fc -tags $tag
    set nbr_ors 2
    set inpneg1 {}
    set inpneg1 {{} {}}
    # The final OR gate
    $c create polygon   \
        [expr ($nx-[or_wid $c])] [expr ($y+[or_ht $c]/2)] \
        [expr ($nx-[or_wid $c])] [expr ($y+[or_ht $c]/2)] \
        [expr ($nx-5*[or_wid $c]/6)] $y \
        [expr ($nx-[or_wid $c])] [expr ($y-[or_ht $c]/2)] \
        [expr ($nx-[or_wid $c])] [expr ($y-[or_ht $c]/2)] \
        [expr ($nx-3*[or_wid $c]/4)] [expr ($y-[or_ht $c]/2)] \
        [expr ($nx-1*[or_wid $c]/3)] [expr ($y-6*([or_ht $c]/2)/7)] \
        $nx $y \
        $nx $y \
        [expr ($nx-1*[or_wid $c]/3)] [expr ($y+6*([or_ht $c]/2)/7)] \
        [expr ($nx-3*[or_wid $c]/4)] [expr ($y+[or_ht $c]/2)] \
        [expr ($nx-[or_wid $c])] [expr ($y+[or_ht $c]/2)] \
        [expr ($nx-[or_wid $c])] [expr ($y+[or_ht $c]/2)] \
        -smooth yes -outline $gcolor -fill $fc \
        -tags $tag
    set andx [expr $nx-[or_wid $c]]
    set ya [expr ($y+[or_ht $c]/3)]
    val {ainps aout} [draw_and_pat 0 {"" ""} "" $c $tag $andx $ya]
    set or_x_inp [expr $nx-[or_wid $c]+[min_sep $c]/3]
    $c create  line $andx $ya  $or_x_inp [expr $y+[or_ht $c]/3] \
		    -fill $gcolor -tags _DoNotChangeColor_
    set ai1_x [lindex $ainps 0]
    set ai1_y [expr $y-[or_ht $c]/3]
    $c create  line $ai1_x $ai1_y $or_x_inp $ai1_y \
		    -fill $gcolor -tags _DoNotChangeColor_
    eval lappend inps  [expr round($ai1_x)] [expr round($ai1_y)] 
    foreach ai $ainps {
	lappend inps [expr round($ai)]
    }
    return [list $inps [list [expr round($x)] [expr round($y)]]]
}

proc draw_oai12 {c tag x y} {
    global gcolor mfont
    global fc sfont
    add_value_field $c $tag $x $y
    set nx [expr ($x-[inv_rad $c]*2)]
    $c create oval [expr ($x-2*[inv_rad $c])] [expr ($y-[inv_rad $c])] \
		   $x [expr ($y+[inv_rad $c])] \
		   -outline $gcolor -fill $fc -tags $tag
    set nbr_ors 2
    set inpneg1 {}
    set inpneg1 {{} {}}
    # The final AND gate
    $c create polygon \
        [expr ($nx-[and_wid $c])] [expr ($y-[and_ht $c]/2)] \
        [expr ($nx-[and_wid $c])] [expr ($y+[and_ht $c]/2)] \
        [expr ($nx-[and_wid $c])] [expr ($y+[and_ht $c]/2)] \
        [expr ($nx-[and_wid $c]/4)] [expr ($y+[and_ht $c]/2)] \
        [expr 1.0*$nx] [expr $y] \
        [expr ($nx-[and_wid $c]/4)] [expr ($y-[and_ht $c]/2)] \
        [expr ($nx-[and_wid $c])] [expr ($y-[and_ht $c]/2)] \
        [expr ($nx-[and_wid $c])] [expr ($y-[and_ht $c]/2)] \
        -smooth yes -outline $gcolor -fill $fc \
        -tags $tag
    set andx [expr $nx-[or_wid $c]]
    set ya [expr ($y+[or_ht $c]/3)]
    val {ainps aout} [draw_or_pat 0 {"" ""} "" "" $c $tag $andx $ya]
    set or_x_inp [expr $nx-[or_wid $c]+[min_sep $c]/3]
    $c create  line $andx $ya  $or_x_inp [expr $y+[or_ht $c]/3] \
		    -fill $gcolor -tags _DoNotChangeColor_
    set ai1_x [lindex $ainps 0]
    set ai1_y [expr $y-[or_ht $c]/3]
    $c create  line $ai1_x $ai1_y $or_x_inp $ai1_y \
		    -fill $gcolor -tags _DoNotChangeColor_
    eval lappend inps  [expr round($ai1_x)] [expr round($ai1_y)] 
    foreach ai $ainps {
	lappend inps [expr round($ai)]
    }
    return [list $inps [list [expr round($x)] [expr round($y)]]]
}

proc draw_and_or_gate {n andlist out_neg c tag x y} {
    global gcolor mfont
    global fc sfont
    add_value_field $c $tag $x $y
    if {$out_neg != ""} {
	set nx [expr ($x-[inv_rad $c]*2)]
	$c create oval [expr ($x-2*[inv_rad $c])] [expr ($y-[inv_rad $c])] \
		       $x [expr ($y+[inv_rad $c])] \
		       -outline $gcolor -fill $fc -tags $tag
    } else {
	set nx $x
    }
    set nbr_ors [llength $andlist]
    set inpneg1 {}
    foreach a $andlist { lappend inpneg1 "" }
    # The final OR gate
    $c create polygon   \
        [expr ($nx-[or_wid $c])] [expr ($y+[or_ht $c]/2)] \
        [expr ($nx-[or_wid $c])] [expr ($y+[or_ht $c]/2)] \
        [expr ($nx-5*[or_wid $c]/6)] $y \
        [expr ($nx-[or_wid $c])] [expr ($y-[or_ht $c]/2)] \
        [expr ($nx-[or_wid $c])] [expr ($y-[or_ht $c]/2)] \
        [expr ($nx-3*[or_wid $c]/4)] [expr ($y-[or_ht $c]/2)] \
        [expr ($nx-1*[or_wid $c]/3)] [expr ($y-6*([or_ht $c]/2)/7)] \
        $nx $y \
        $nx $y \
        [expr ($nx-1*[or_wid $c]/3)] [expr ($y+6*([or_ht $c]/2)/7)] \
        [expr ($nx-3*[or_wid $c]/4)] [expr ($y+[or_ht $c]/2)] \
        [expr ($nx-[or_wid $c])] [expr ($y+[or_ht $c]/2)] \
        [expr ($nx-[or_wid $c])] [expr ($y+[or_ht $c]/2)] \
        -smooth yes -outline $gcolor -fill $fc \
        -tags $tag
    set andx [expr $nx-[or_wid $c]]
    set tot_and_ht 0
    foreach a $andlist {
        set tot_and_ht [expr $tot_and_ht + [and_ht_needed $c $a]]
    }
    set ytop [expr $y - ($tot_and_ht/2)]
    set ybot [expr $y + ($tot_and_ht/2)]
    set idx 0
    set ya $ytop 
    foreach a $andlist {
	set ainvs {}
        set cnt 0
        set icnt 0
        set remap {}
	foreach p $a {
            incr cnt
	    if { $p == "+" } { 
                incr icnt
		lappend ainvs {}
                lappend remap $cnt
	    } elseif { $p == "-" } {
                incr icnt
		lappend ainvs {Y}
                lappend remap $cnt
	    }
	}
        set sz [and_ht_needed $c $a]
        set ya [expr $ya+[expr $sz/2]]
	set ains($idx) [draw_and_pat 1 $ainvs "" $c $tag $andx $ya $remap]
	incr idx
        set and_y_locs($idx) $ya
        set ya [expr $ya+[expr $sz/2]]
    }
    $c create line [expr $andx-[inv_rad $c]] $and_y_locs(1) \
		   $andx $and_y_locs(1) \
		   $andx [expr $y-([or_ht $c]/2)] \
                   -fill $gcolor \
		   -tags _DoNotChangeColor_
    $c create line $andx [expr $y+([or_ht $c]/2)] \
		   $andx $and_y_locs($nbr_ors) \
		   [expr $andx-[inv_rad $c]]  $and_y_locs($nbr_ors)  \
		   -fill $gcolor \
		   -tags _DoNotChangeColor_
    #
    set ix [expr $andx-[and_wid $c]-[inv_rad $c]-[min_sep $c]]
    set lmx [expr  $ix-$n*[min_sep $c]]
    set inps {}
    set isep [expr ($tot_and_ht/($n+1))]
    set cinpy [expr $ytop+$isep]
    for {set i 0} {$i < $n} {incr i} {
	set xl [expr $ix-($n-$i-1)*[min_sep $c]]
        set itag [format {%s____%d} $tag [expr $i+1]]
	$c create line [expr $lmx-[min_sep $c]] $cinpy $xl $cinpy -fill $gcolor\
		   -tags "$itag _WiRe_"
        set min_y($i) $cinpy
        set max_y($i) $cinpy
        set connections($i) [list $cinpy]
	lappend inps [expr round($lmx-[min_sep $c])] [expr round($cinpy)]
	set cinpy [expr ($cinpy+$isep)]
    }
    set idx 0
    foreach a $andlist {
	set conns [lindex $ains($idx) 0]
	incr idx
	set i 0
	set inp 0
	foreach t $a {
	    incr i
	    if { $t == {} } { continue }
	    set conx [lindex $conns [expr 2*$inp]]
	    set cony [lindex $conns [expr 2*$inp+1]]
	    incr inp
	    set wx [expr $ix-($n-$i)*[min_sep $c]]
            set itag [format {%s____%d} $tag [expr $i]]
	    $c create line $wx $cony $conx $cony -fill $gcolor \
		-tags "$itag _WiRe_"
            set im1 [expr $i-1]
            if [expr $cony < $min_y($im1)] { set min_y($im1) $cony }
            if [expr $cony > $max_y($im1)] { set max_y($im1) $cony }
            lappend connections($im1) $cony
	}
    }
    set cinpy [expr ($ytop+$isep)]
    for {set i 0} {$i < $n} {incr i} {
        set xl [expr $ix-($n-$i-1)*[min_sep $c]]
        set lytop $min_y($i)
        set lybot $max_y($i)
        set itag [format {%s____%d} $tag [expr $i+1]]
	$c create line $xl $lytop $xl $lybot -fill $gcolor \
		   -tags "$itag _WiRe_"
        foreach py $connections($i) {
            if [expr $py > $lytop && $py < $lybot] {
                set mrad [conn_rad $c]
                set conn [$c create oval [expr $xl-$mrad] [expr $py-$mrad] \
                               [expr $xl+$mrad] [expr $py+$mrad] \
                               -outline black -fill black -tags "$itag _WiRe_"]
                record_orig_color $c $conn
            }
        }
	set cinpy [expr ($cinpy+$isep)]
    }
    set lmy [expr ($ybot+2)]
    set rmy [expr ($ytop-2)]
    $c create line  $lmx $lmy $lmx $rmy $nx $rmy $nx $lmy $lmx $lmy \
	    -fill $gcolor -tags [list $tag _DoNotChangeColor_] -dash {1 3}
    return [list $inps [list [expr round($x)] [expr round($y)]]]
}

proc draw_concat {n c tag x y} {
    global gcolor mfont
    global fc
    add_value_field $c $tag $x $y
    set ht [expr (($n+1)*[min_sep $c])]
    set y1 [expr $y-$ht/2]
    set y2 [expr $y+$ht/2]
    set x1 [expr round($x-[min_sep $c]/3)]
    $c create rectangle  $x1 $y1 $x $y2 \
	    -outline $gcolor -fill $fc -tags $tag
    set cy $y1
    set inp_locs {}
    for {set i 1} {$i <= $n} {incr i} {
	set cy [expr round($cy + [min_sep $c])]
	lappend inp_locs [expr ($x1)] [expr ($cy)]
    }
    return [list $inp_locs [list [expr round($x)] [expr round($y1)]]]
}

proc draw_split {c tag x y} {
    global gcolor mfont
    global fc
    add_value_field $c $tag $x $y
    set ht [expr (2*[min_sep $c])]
    set y1 [expr $y-$ht/2]
    set y2 [expr $y+$ht/2]
    set x1 [expr $x-[min_sep $c]/3]
    $c create rectangle  $x1 $y1 $x $y2 -outline $gcolor -fill $fc -tags $tag
    set inp_locs {}
    set cy [expr $y1 + [min_sep $c]]
    lappend inp_locs [expr round($x1)] [expr round($cy)]
    return [list $inp_locs [list $x [expr round($y1)]]]
}

proc draw_field {name c tag x y} {
    draw_hfl_code 1 ">$name" $c $tag $x $y
}

proc draw_predicate {name inps c tag x y} {
    draw_hfl_code $inps $name $c $tag $x $y
}

proc draw_binary_arithm {symbol c tag x y} {
    global gcolor mfont fc
    add_value_field $c $tag $x $y
    set xl [expr $x-2*[rtl_rad $c]]
    set yl [expr $y+[rtl_rad $c]]
    set yt [expr $y-[rtl_rad $c]]
    set t [$c create oval  $xl $yl $x $yt -outline $gcolor -fill $fc \
                -tags [list _Is_Hierarchical_ $tag]]
    set tt [$c create text [expr $x-[rtl_rad $c]] $y -anchor center \
			    -font $mfont($c) -text $symbol \
		-tags [list _Is_Hierarchical_ $tag]]
    add_font_tags $c $tt _IsTeXt_
    set xi [expr round($x-[rtl_rad $c]-0.866*[rtl_rad $c])]
    set y1 [expr round($y-[rtl_rad $c]/2)]
    set y2 [expr round($y+[rtl_rad $c]/2)]
    lappend inp_locs $xi $y1 $xi $y2
    return [list $inp_locs [list $x $y]]
}

proc draw_rtl {symbol c tag x y} {
    global gcolor mfont fc
    add_value_field $c $tag $x $y
    set xl [expr $x-2*[rtl_rad $c]]
    set yl [expr $y+[rtl_rad $c]]
    set yt [expr $y-[rtl_rad $c]]
    set t [$c create oval  $xl $yl $x $yt -outline $gcolor -fill $fc \
                -tags [list _Is_Hierarchical_ $tag]]
    set tt [$c create text [expr $x-[rtl_rad $c]] $y -anchor center \
			    -font $mfont($c) -text $symbol \
		-tags [list _Is_Hierarchical_ $tag]]
    add_font_tags $c $tt _IsTeXt_
    set xi [expr round($x-[rtl_rad $c]-0.866*[rtl_rad $c])]
    set y1 [expr round($y-[rtl_rad $c]/2)]
    set y2 [expr round($y+[rtl_rad $c]/2)]
    lappend inp_locs $xi $y1 $xi $y2
    return [list $inp_locs [list $x $y]]
}

proc draw_unary_arithm {symbol c tag x y} {
    global gcolor mfont fc 
    add_value_field $c $tag $x $y
    set xl [expr $x-2*[rtl_rad $c]]
    set yl [expr $y+[rtl_rad $c]]
    set yt [expr $y-[rtl_rad $c]]
    $c create oval  $xl $yl $x $yt -outline $gcolor -fill $fc -tags $tag
    set t [$c create text [expr $x-[rtl_rad $c]] $y -anchor center \
			    -font $mfont($c) -text $symbol -tags $tag]
    add_font_tags $c $t _IsTeXt_
    set xi [expr round($x-2.0*[rtl_rad $c])]
    lappend inp_locs $xi $y
    return [list $inp_locs [list $x $y]]
}

proc draw_mem {n c tag x y} {
    global gcolor mfont fc
    #
    set d [expr (1.3*[min_sep $c])]
    # 
    add_value_field $c $tag $x $y
    #
    set x1 [expr round($x-10*$d)]
    set x2 [expr round($x-8*$d)]
    set x3 [expr round($x-4*$d)]
    set x3b [expr round($x-4*$d)]
    set x4 [expr round($x-2*$d)]
    set y1 [expr round($y-9*$d)]
    set y2 [expr round($y-7*$d)]
    set y3 [expr round($y-5*$d)]
    set y4 [expr round($y+5*$d)]
    set y5 [expr round($y+7*$d)]
    set y6 [expr round($y+9*$d)]
    set y7 [expr round($y+11*$d)]
    #
    $c create rectangle $x2 $y4 $x $y2 -outline $gcolor -fill $fc -tags $tag
    $c create rectangle $x1 $y5 $x4 $y3 -outline $gcolor -fill $fc -tags $tag
    $c create line $x1 $y6 $x2 $y6 $x2 $y5 -fill $gcolor
    $c create line $x1 $y7 $x3 $y7 $x3 $y5 -fill $gcolor -width 3
    $c create line $x1 $y1 $x3b $y1 $x3b $y2 -fill $gcolor -width 3
    add_font_tags $c [$c create text $x1 $y -anchor w \
			-font $mfont($c) -text di -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x $y -anchor e \
			-font $mfont($c) -text do -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x2 $y5 -anchor s \
			-font $mfont($c) -text en -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x3 $y5 -anchor s \
			-font $mfont($c) -text wa -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x3b $y2 -anchor n \
			-font $mfont($c) -text ra -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x1 $y3 -anchor nw \
			-font $mfont($c) -text 0 -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x2 $y2 -anchor nw \
			-font $mfont($c) -text [expr $n-1] -tags $tag] _IsTeXt_
    lappend inp_locs $x1 $y1 $x1 $y $x1 $y6 $x1 $y7
    return [list $inp_locs [list $x $y]]
}

proc draw_mem_with_reset {n c tag x y} {
    global gcolor mfont fc
    #
    set d [expr (1.3*[min_sep $c])]
    # 
    add_value_field $c $tag $x $y
    #
    set x1 [expr round($x-10*$d)]
    set x2 [expr round($x-8*$d)]
    set x25 [expr round($x-6*$d)]
    set x3 [expr round($x-4*$d)]
    set x3b [expr round($x-4*$d)]
    set x4 [expr round($x-2*$d)]
    set y1 [expr round($y-9*$d)]
    set y2 [expr round($y-7*$d)]
    set y3 [expr round($y-5*$d)]
    set y4 [expr round($y+5*$d)]
    set y5 [expr round($y+7*$d)]
    set y6 [expr round($y+9*$d)]
    set y7 [expr round($y+11*$d)]
    set y8 [expr round($y+13*$d)]
    #
    $c create rectangle $x2 $y4 $x $y2 -outline $gcolor -fill $fc -tags $tag
    $c create rectangle $x1 $y5 $x4 $y3 -outline $gcolor -fill $fc -tags $tag
    $c create line $x1 $y6 $x2 $y6 $x2 $y5 -fill $gcolor
    $c create line $x1 $y7 $x25 $y7 $x25 $y5 -fill $gcolor
    $c create line $x1 $y8 $x3 $y8 $x3 $y5 -fill $gcolor -width 3
    $c create line $x1 $y1 $x3b $y1 $x3b $y2 -fill $gcolor -width 3
    add_font_tags $c [$c create text $x1 $y -anchor w \
			-font $mfont($c) -text din -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x $y -anchor s \
		    -font $mfont($c) -text dout -tags $tag -angle 90.0] _IsTeXt_
    add_font_tags $c [$c create text $x2 $y5 -anchor w \
		-font $mfont($c) -text reset -tags $tag -angle 90.0] _IsTeXt_
    add_font_tags $c [$c create text $x25 $y5 -anchor w \
		-font $mfont($c) -text enable -tags $tag -angle 90.0] _IsTeXt_
    add_font_tags $c [$c create text $x3 $y5 -anchor w \
		-font $mfont($c) -text wadr -tags $tag -angle 90.0] _IsTeXt_
    add_font_tags $c [$c create text $x3b $y2 -anchor n \
			-font $mfont($c) -text radr -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x1 $y3 -anchor nw \
			-font $mfont($c) -text 0 -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x2 $y2 -anchor nw \
			-font $mfont($c) -text [expr $n-1] -tags $tag] _IsTeXt_
    lappend inp_locs $x1 $y1 $x1 $y $x1 $y6 $x1 $y7 $x1 $y8
    return [list $inp_locs [list $x $y]]
}

proc draw_mem_init {wid content c tag x y} {
    draw_hfl_code 0 "ROM" $c $tag $x $y
}

proc draw_mem_read {n c tag x y} {
    global gcolor mfont fc
    #
    set d [expr (1.3*[min_sep $c])]
    # 
    add_value_field $c $tag $x $y
    #
    set x1 [expr round($x-10*$d)]
    set x2 [expr round($x-8*$d)]
    set x25 [expr round($x-6*$d)]
    set x3 [expr round($x-4*$d)]
    set x3b [expr round($x-4*$d)]
    set x4 [expr round($x-2*$d)]
    set y1 [expr round($y-9*$d)]
    set y2 [expr round($y-7*$d)]
    set y3 [expr round($y-5*$d)]
    set y4 [expr round($y+5*$d)]
    set y5 [expr round($y+7*$d)]
    #
    $c create rectangle $x2 $y4 $x $y2 -outline $gcolor -fill $fc -tags $tag
    $c create rectangle $x1 $y5 $x4 $y3 -outline $gcolor -fill $fc -tags $tag

    set itag [format {%s____%d} $tag 1]
    $c create line $x1 $y1 $x3b $y1 $x3b $y2 -fill $gcolor -width 3 \
	    -tags "$itag _WiRe_"
    add_font_tags $c [$c create text $x1 $y -anchor w \
			-font $mfont($c) -text mem -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x $y -anchor s \
		    -font $mfont($c) -text dout -tags $tag -angle 90.0] _IsTeXt_
    add_font_tags $c [$c create text $x3b $y2 -anchor n \
			-font $mfont($c) -text radr -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x1 $y3 -anchor nw \
			-font $mfont($c) -text 0 -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x2 $y2 -anchor nw \
			-font $mfont($c) -text [expr $n-1] -tags $tag] _IsTeXt_
    lappend inp_locs $x1 $y1 $x1 $y
    return [list $inp_locs [list $x $y]]
}

proc draw_rom {name n c tag x y} {
    global gcolor mfont fc
    #
    set d [expr (1.3*[min_sep $c])]
    # 
    add_value_field $c $tag $x $y
    #
    set x1 [expr round($x-10*$d)]
    set x2 [expr round($x-8*$d)]
    set x25 [expr round($x-6*$d)]
    set x3 [expr round($x-4*$d)]
    set x3b [expr round($x-4*$d)]
    set x4 [expr round($x-2*$d)]
    set y1 [expr round($y-9*$d)]
    set y2 [expr round($y-7*$d)]
    set y3 [expr round($y-5*$d)]
    set y4 [expr round($y+5*$d)]
    set y5 [expr round($y+7*$d)]
    #
    $c create rectangle $x2 $y4 $x $y2 -outline $gcolor -fill $fc -tags $tag
    $c create rectangle $x1 $y5 $x4 $y3 -outline $gcolor -fill $fc -tags $tag
    $c create line $x1 $y1 $x3b $y1 $x3b $y2 -fill $gcolor -width 3
    add_font_tags $c [$c create text $x $y -anchor s \
		    -font $mfont($c) -text dout -tags $tag -angle 90.0] _IsTeXt_
    add_font_tags $c [$c create text $x3b $y2 -anchor n \
			-font $mfont($c) -text radr -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x1 $y3 -anchor nw \
			-font $mfont($c) -text 0 -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x2 $y2 -anchor nw \
			-font $mfont($c) -text [expr $n-1] -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x25 $y -anchor c -justify center \
			-font $mfont($c) -text $name -tags $tag] _IsTeXt_
    lappend inp_locs $x1 $y1
    draw_three_dots $c $x1 $y3 $x2 $y2 $tag
    draw_three_dots $c $x4 $y5 $x $y4 $tag
    draw_three_dots $c $x4 $y3 $x $y2 $tag
    return [list $inp_locs [list $x $y]]
}

proc draw_ram {name n c tag x y} {
    global gcolor mfont fc
    #
    set d [expr (1.3*[min_sep $c])]
    # 
    add_value_field $c $tag $x $y
    #
    set x1 [expr round($x-10*$d)]
    set x2 [expr round($x-8*$d)]
    set x25 [expr round($x-6*$d)]
    set x3 [expr round($x-4*$d)]
    set x3b [expr round($x-4*$d)]
    set x4 [expr round($x-2*$d)]
    set y1 [expr round($y-9*$d)]
    set y2 [expr round($y-7*$d)]
    set y3 [expr round($y-5*$d)]
    set y4 [expr round($y+5*$d)]
    set y5 [expr round($y+7*$d)]
    set y6 [expr round($y+9*$d)]
    set y7 [expr round($y+11*$d)]
    #
    $c create rectangle $x2 $y4 $x $y2 -outline $gcolor -fill $fc -tags $tag
    $c create rectangle $x1 $y5 $x4 $y3 -outline $gcolor -fill $fc -tags $tag
    $c create line $x1 $y6 $x2 $y6 $x2 $y5 -fill $gcolor
    $c create line $x1 $y7 $x3 $y7 $x3 $y5 -fill $gcolor -width 3
    $c create line $x1 $y1 $x3b $y1 $x3b $y2 -fill $gcolor -width 3
    add_font_tags $c [$c create text $x1 $y -anchor w \
			-font $mfont($c) -text di -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x $y -anchor e \
			-font $mfont($c) -text do -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x2 $y5 -anchor s \
			-font $mfont($c) -text en -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x3 $y5 -anchor s \
			-font $mfont($c) -text wa -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x3b $y2 -anchor n \
			-font $mfont($c) -text ra -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x1 $y3 -anchor nw \
			-font $mfont($c) -text 0 -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x2 $y2 -anchor nw \
			-font $mfont($c) -text [expr $n-1] -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x25 [expr $y-2*$d] \
			-anchor c -justify center \
			-font $mfont($c) -text $name -tags $tag] _IsTeXt_
    lappend inp_locs $x1 $y1 $x1 $y $x1 $y6 $x1 $y7
    draw_three_dots $c $x1 $y3 $x2 $y2 $tag
    draw_three_dots $c $x4 $y5 $x $y4 $tag
    draw_three_dots $c $x4 $y3 $x $y2 $tag
    return [list $inp_locs [list $x $y]]
}

proc draw_mem_write {n c tag x y} {
    global gcolor mfont fc
    #
    set d [expr (1.3*[min_sep $c])]
    # 
    add_value_field $c $tag $x $y
    #
    set x1 [expr round($x-10*$d)]
    set x2 [expr round($x-8*$d)]
    set x25 [expr round($x-6*$d)]
    set x3 [expr round($x-4*$d)]
    set x3b [expr round($x-4*$d)]
    set x4 [expr round($x-2*$d)]
    set y2 [expr round($y-7*$d)]
    set y3 [expr round($y-5*$d)]
    set y4 [expr round($y+5*$d)]
    set y5 [expr round($y+7*$d)]
    set y7 [expr round($y+11*$d)]
    set y8 [expr round($y+13*$d)]
    #
    $c create rectangle $x2 $y4 $x $y2 -outline $gcolor -fill $fc -tags $tag
    $c create rectangle $x1 $y5 $x4 $y3 -outline $gcolor -fill $fc -tags $tag
    $c create line $x1 $y7 $x25 $y7 $x25 $y5 -fill $gcolor -width 3
    $c create line $x1 $y8 $x3 $y8 $x3 $y5 -fill $gcolor -width 3
    add_font_tags $c [$c create text $x1 $y -anchor w \
			-font $mfont($c) -text din -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x $y -anchor s \
		    -font $mfont($c) -text mem -tags $tag -angle 90.0] _IsTeXt_
    add_font_tags $c [$c create text $x25 $y5 -anchor w \
		-font $mfont($c) -text enable -tags $tag -angle 90.0] _IsTeXt_
    add_font_tags $c [$c create text $x3 $y5 -anchor w \
		-font $mfont($c) -text wadr -tags $tag -angle 90.0] _IsTeXt_
    add_font_tags $c [$c create text $x1 $y3 -anchor nw \
			-font $mfont($c) -text 0 -tags $tag] _IsTeXt_
    add_font_tags $c [$c create text $x2 $y2 -anchor nw \
			-font $mfont($c) -text [expr $n-1] -tags $tag] _IsTeXt_
    lappend inp_locs $x1 $y $x1 $y7 $x1 $y8
    return [list $inp_locs [list $x $y]]
}

proc draw_DECO {iw aw ow c tag x y} {
    global gcolor mfont fc 
    #
    add_value_field $c $tag $x $y
    set d [expr (1.3*[min_sep $c])]
    set sels [expr $iw/$ow]
    #
    set mxr [expr ($x-$d)]
    set mxl [expr ($x-6*$d)]
    set myt [expr ($y-3.5*$d)]
    set mytt [expr ($y-5.5*$d)]
    set mb [expr ($y+3.5*$d)]
    set mbb [expr ($y+5.5*$d)]
    set dt [expr ($y+6.0*$d)]
    set db [expr ($y+8.0*$d)]
    set dcx [expr ($x-3.5*$d)]
    set dcy [expr ($y+7.0*$d)]
    set top [expr ($y-6.5*$d)]
    set etop [expr ($y-6.5*$d)]
    set bot [expr ($y+8.5*$d)]
    set bb [expr ($y+7.5*$d)]
    set bm [expr ($x-3.5*$d)]
    set rxl [expr ($x-7*$d)]
    # Frame
    $c create rectangle $rxl $bot $x $etop -outline $gcolor \
                            -tags [list _Is_Hierarchical_ $tag]
    # MUX
    $c create polygon $mxl $mytt $mxr $myt $mxr $mb $mxl $mbb \
	    -outline $gcolor -fill $fc -tags [list _Is_Hierarchical_ $tag]
    # Decoder
    $c create rectangle $mxl $db $mxr $dt -outline $gcolor -fill $fc \
            -tags [list _Is_Hierarchical_ $tag]
    #
    set txt [format {%d->%d} $aw $sels]
    add_font_tags $c [$c create text $dcx $dcy -anchor center \
			    -font $mfont($c) -text $txt \
                            -tags [list _Is_Hierarchical_ $tag]] _IsTeXt_
    if { [fl_is_vector $c $tag] } {
        $c create line $x $y $mxr $y \
                -fill $gcolor -tags "$tag _WiRe_ _Is_Hierarchical_" -width 3
    } else {
        $c create line $x $y $mxr $y \
                -fill $gcolor -tags "$tag _WiRe_ _Is_Hierarchical_"
    }
    if { $sels > 2 } {
        $c create line $mxl $dcy $rxl $dcy -fill $gcolor -width 3
    } else {
        $c create line $mxl $dcy $rxl $dcy -fill $gcolor
    }
    lappend inp_locs $rxl $dcy $rxl $y
    set x1 [expr ($mxl+$d)]
    $c create line $x1 $dt $x1 [expr $mbb-2.0*$d/5.0]
    set xn [expr ($mxr-$d)]
    $c create line $xn $dt $xn [expr $mbb-4.0*2.0*$d/5.0]
    set x11 [expr $x1+($xn-$x1)/4.0]
    set x1c [expr $x1+2.0*($xn-$x1)/4.0]
    set x1n [expr $x1+3.0*($xn-$x1)/4.0]
    set yc  [expr $y+5.3*$d]
    $c create oval [expr $x11-[conn_rad $c]] [expr $yc+[conn_rad $c]] \
		   [expr $x11+[conn_rad $c]] [expr $yc-[conn_rad $c]] \
		   -fill $gcolor -outline $gcolor -tags "$tag _Is_Hierarchical_"
    $c create oval [expr $x1c-[conn_rad $c]] [expr $yc+[conn_rad $c]] \
		   [expr $x1c+[conn_rad $c]] [expr $yc-[conn_rad $c]] \
		   -fill $gcolor -outline $gcolor -tags "$tag _Is_Hierarchical_"
    $c create oval [expr $x1n-[conn_rad $c]] [expr $yc+[conn_rad $c]] \
		   [expr $x1n+[conn_rad $c]] [expr $yc-[conn_rad $c]] \
		   -fill $gcolor -outline $gcolor -tags "$tag _Is_Hierarchical_"
    return [list $inp_locs [list $x $y]]
}

proc draw_U_CONST_REL {const rel c tag x y} {
    global gcolor mfont
    global fc 
    add_value_field $c $tag $x $y
    set xl [expr $x-2*[rtl_rad $c]]
    set yl [expr $y+[rtl_rad $c]]
    set yt [expr $y-[rtl_rad $c]]
    $c create oval  $xl $yl $x $yt -outline $gcolor -fill $fc -tags $tag
    add_font_tags $c [$c create text [expr $x-[rtl_rad $c]] $y -anchor center \
			    -font $mfont($c) -text $rel -tags $tag] _IsTeXt_
    set xi [expr ($x-[rtl_rad $c]-0.866*[rtl_rad $c])]
    set y1 [expr ($y-[rtl_rad $c]/2)]
    set x2 [expr $xi-[min_sep $c]]
    set y2 [expr ($y+[rtl_rad $c]/2)]
    add_font_tags $c [$c create text $x2 $y2 -anchor e -font $mfont($c) \
				    -text $const -tags $tag] _IsTeXt_
    $c create line $x2 $y2 $xi $y2 -fill $gcolor -tags $tag -width 3
    lappend inp_locs $xi $y1
    return [list $inp_locs [list $x $y1]]
}

proc draw_decoder {from_sz to_sz default_val c tag x y} {
    global gcolor fc sfont
    $c create polygon \
        [expr $x-[decode_wid $c]] [expr $y+(2*[decode_ht $c]/3.0)] \
        [expr $x-[decode_wid $c]] [expr $y-(2*[decode_ht $c]/3.0)] \
        [expr $x] [expr $y-[decode_ht $c]] \
        [expr $x] [expr $y+[decode_ht $c]] \
        -outline $gcolor -fill $fc -tags [list _Is_Hierarchical_ $tag]
    set txt "$from_sz:$to_sz\n/$default_val"
    add_font_tags $c [$c create text \
            [expr $x-([decode_wid $c]/2.0)] $y -anchor center \
            -justify center \
            -font $::sfont($c) -text $txt -tags $tag] _IsTeXt_
    add_value_field $c $tag $x $y
    set nx [expr $x-[decode_wid $c]]
    return [list [list $nx $y] [list $x $y]]
}

proc draw_vcase {inputs default c tag x y} {
    global gcolor fc mfont sfont
    add_value_field $c $tag $x $y
    set n [expr [llength $inputs] + 1]
    set ht [expr ($n*2+2)*[min_sep $c]]
    set top [expr round($y-(($ht+1)/2))]
    set bot [expr round($y+(($ht+1)/2))]
    set wid [expr [get_width $c $x $y 0 [lindex $inputs 0]] + 6*[min_sep $c]]
    set xl [expr round($x-$wid)]
    $c create polygon \
	    $xl $top \
	    $x  [expr $top+[min_sep $c]] \
	    $x  [expr $bot-[min_sep $c]] \
	    $xl $bot \
	    $xl $top \
	    -outline $gcolor -fill $fc -tags $tag
    $c create line [expr $xl+2*[min_sep $c]] [expr $top+2*[min_sep $c]] \
		   [expr $xl+2*[min_sep $c]] [expr $bot-2*[min_sep $c]] \
		   -arrow last -fill $gcolor -tags $tag
    set xtxt [expr $xl+4*[min_sep $c]]
    set inps {}
    set cy [expr round($top+2*[min_sep $c])]
    set cx $xl
    set xt [expr $xl+[min_sep $c]]
    for {set i 0} {$i < [llength $inputs]} {incr i} {
	set f [$c create text $xtxt $cy -anchor w -justify left \
		    -font $mfont($c) -text [lindex $inputs $i] \
		    -tags $tag]
	add_font_tags $c $f _IsTeXt_
	lappend inp_locs $xl $cy
	set cy [expr round($cy+2*[min_sep $c])]
    }
    set f [$c create text $xtxt $cy -anchor w -justify left \
		-font $mfont($c) -text $default -tags $tag]
    add_font_tags $c $f _IsTeXt_
    return [list $inp_locs [list $x $y]]
}

proc draw_CASE {n c tag x y} {
    global gcolor fc
    add_value_field $c $tag $x $y
    set ht [expr ($n*3+2)*[min_sep $c]]
    set wid [expr round(max((0.8*[rect_wid $c]),$n*[min_sep $c]/3))]
    set xl [expr round($x-$wid)]
    set top [expr round($y-(($ht+1)/2))]
    set bot [expr round($y+(($ht+1)/2))]
    $c create polygon \
	    $xl $top \
	    $x  [expr $top+[min_sep $c]] \
	    $x  [expr $bot-[min_sep $c]] \
	    $xl $bot \
	    $xl $top \
	    -outline $gcolor -fill $fc -tags $tag
    $c create line [expr $xl+2*[min_sep $c]] [expr $top+2*[min_sep $c]] \
		   [expr $xl+2*[min_sep $c]] [expr $bot-2*[min_sep $c]] \
		   -arrow last -fill $gcolor -tags $tag
    set inps {}
    set cy [expr round($top+[min_sep $c])]
    set cx $xl
    set xt [expr $xl+[min_sep $c]]
    for {set i 1} {$i <= $n} {incr i} {
	lappend inp_locs $xl $cy
	set cy [expr round($cy+[min_sep $c])]
	lappend inp_locs $xl $cy
	set cy [expr round($cy+[min_sep $c])]
    }
    lappend inp_locs $xl $cy
    return [list $inp_locs [list $x $y]]
}

proc draw_SELECT {c tag x y} {
    global gcolor fc
    add_value_field $c $tag $x $y
    set ht [expr 6*[min_sep $c]]
    set wid [expr round(0.8*[rect_wid $c])]
    set xl [expr round($x-$wid)]
    set top [expr round($y-(($ht+1)/2))]
    set bot [expr round($y+(($ht+1)/2))]
    set yaddr [expr round($bot+[min_sep $c])]
    set xmid [expr round($xl+$wid/2.0)]
    $c create polygon \
	    $xl $top \
	    $x  [expr $top+[min_sep $c]] \
	    $x  [expr $bot-[min_sep $c]] \
	    $xl $bot \
	    $xl $top \
	    -outline $gcolor -fill $fc -tags $tag
    lappend inp_locs $xl $y
    $c create line $xl $yaddr $xmid $yaddr $xmid [expr $bot-[min_sep $c]/2.0] \
	    -fill $gcolor -width 3
    lappend inp_locs $xl $yaddr
    return [list $inp_locs [list $x $y]]
}

proc draw_DECODER {with_enable inp_sz out_sz c tag x y} {
    global gcolor fc
    add_value_field $c $tag $x $y
    set ht [expr 6*[min_sep $c]]
    set wid [expr round(1.0*[rect_wid $c])]
    set xl [expr round($x-$wid)]
    set top [expr round($y-(($ht+1)/2))]
    set bot [expr round($y+(($ht+1)/2))]
    set yaddr [expr round($top-[min_sep $c])]
    set xmid [expr round($xl+$wid/2.0)]
    $c create polygon \
	    $xl [expr $top+[min_sep $c]] \
	    $x  $top  \
	    $x  $bot \
	    $xl [expr $bot-[min_sep $c]] \
	    $xl [expr $top+[min_sep $c]] \
	    -outline $gcolor -fill $fc -tags $tag
    set txt [format {%d->%d} $inp_sz $out_sz]
    set t [$c create text [expr ($xl+$x)/2.0] $y -anchor c -justify c \
					-font $::tfont($c) -text $txt]
    add_font_tags $c $t _IsTeXt_
    if [expr $with_enable == 1] {
	$c create line $xl $yaddr $xmid $yaddr \
				  $xmid [expr $top+[min_sep $c]/2.0] \
				  -fill $gcolor
	lappend inp_locs $xl $yaddr
    }
    lappend inp_locs $xl $y
    return [list $inp_locs [list $x $y]]
}

proc draw_MUXn {priority n c tag x y} {
    global gcolor fc
    set x [expr ($x)]
    set y [expr ($y)]
    add_value_field $c $tag $x $y
    set ht [expr ($n+1)*2*[min_sep $c]]
    set wid [expr ($n+1)*[min_sep $c]]
    set xl [expr round($x-$wid)]
    set top [expr $y-(($ht+1)/2)]
    set bot [expr $top+($n+1)*2*[min_sep $c]]
    $c create polygon \
	    $xl $top \
	    $x  [expr $top+[min_sep $c]] \
	    $x  [expr $bot-[min_sep $c]] \
	    $xl $bot \
	    $xl $top \
	    -outline $gcolor -fill $fc -tags $tag
    if { $priority == 1 } {
	$c create line [expr $xl+[min_sep $c]] [expr $top+2*[min_sep $c]] \
		       [expr $xl+[min_sep $c]] [expr $bot-2*[min_sep $c]] \
		       -arrow last -fill $gcolor -tags $tag
    }
    set inps {}
    set cy $top
    set cx $xl
    for {set i 1} {$i <= $n} {incr i} {
	set cy [expr $cy + 2*[min_sep $c]]
	lappend inp_locs [expr round($xl)] [expr round($cy)]
        set itag [format {%s____%d} $tag $i]
	set cx [expr ($cx + [min_sep $c])]
	set mcy [expr ($bot-($i*[min_sep $c]/($n+1))) - [min_sep $c]/2.0]
	$c create oval [expr $cx-[inv_rad $c]/2] [expr $mcy-[inv_rad $c]/2] \
		       [expr $cx+[inv_rad $c]/2] [expr $mcy+[inv_rad $c]/2] \
		       -outline white -fill $fc -tags $itag
    }
    set mcy $top
    set cy $bot
    set cx $xl
    set cinp_locs {}
    for {set i 1} {$i <= $n} {incr i} {
	set mcy [expr $mcy + 2*[min_sep $c]]
	set cy [expr round($cy + [min_sep $c])]
	set cx [expr ($cx + [min_sep $c])]
	set iy [expr ($bot-($i*[min_sep $c]/($n+1)))]
        set itag [format {%s____%d} $tag [expr $n+$i]]
	$c create line $xl $cy $cx $cy $cx $iy \
		-fill $gcolor -tags "$itag _WiRe_"
	$c create oval [expr $xl+[inv_rad $c]] [expr $mcy-[inv_rad $c]/2] \
		       [expr $xl+2*[inv_rad $c]] [expr $mcy+[inv_rad $c]/2] \
		       -outline white -fill $fc -tags $itag
	set cinp_locs [linsert $cinp_locs 0 $xl]
	set cinp_locs [linsert $cinp_locs 1 $cy]
    }
    eval lappend inp_locs $cinp_locs
    return [list $inp_locs [list $x $y]]
}

proc draw_pmux {out_sz sel_sz c tag x y} {
    global gcolor fc
    set n $sel_sz
    add_value_field $c $tag $x $y
    set ht [expr ($n+2)*2*[pmux_ht_sep $c]]
    set wid [expr ($n+1)*1.5*[min_sep $c]]
    set xl [expr round($x-$wid)]
    set xt [expr round($xl+[min_sep $c]/2)]
    set xli [expr round($xl - [min_sep $c])]
    set xlinp [expr round($xl - 2*[min_sep $c])]
    set top [expr $y-(($ht+1)/2)]
    set bot [expr $top+($n+1)*2*[pmux_ht_sep $c]]
    $c create polygon \
	    $xl $top \
	    $x  [expr $top+[pmux_ht_sep $c]] \
	    $x  [expr $bot-[pmux_ht_sep $c]] \
	    $xl $bot \
	    $xl $top \
	    -outline $gcolor -fill $fc -tags $tag
    $c create line [expr $xl+[min_sep $c]] [expr $top+2*[pmux_ht_sep $c]] \
		   [expr $xl+[min_sep $c]] [expr $bot-2*[pmux_ht_sep $c]] \
		   -arrow last -fill $gcolor -tags $tag
    set inps {}
    set cy $top
    set cx $xl
    set start_range [expr $out_sz*$sel_sz-1]
    for {set i 0} {$i < $n} {incr i} {
	set cy [expr $cy + 2*[pmux_ht_sep $c]]
	if { $out_sz > 1 } {
	    $c create line $xli $cy $xl $cy -width 3
	} else {
	    $c create line $xli $cy $xl $cy
	}
    }
    set cy [expr $cy + [pmux_ht_sep $c]]
    set t [$c create text $xt $cy -anchor w -justify left \
					-font $::tfont($c) -text "0"]
    add_font_tags $c $t _IsTeXt_
    if { $out_sz > 1 } {
	$c create line $xlinp $cy $xl $cy -width 3
    } else {
	$c create line $xlinp $cy $xl $cy
    }
    set ydata [expr round(($top+$bot)/2)]
    set yother [expr round($cy)]
    $c create line $xli [expr $top+2*[pmux_ht_sep $c]] \
		   $xli [expr $top+$n*2*[pmux_ht_sep $c]] -width 3
    $c create line $xlinp $ydata $xli $ydata -width 3
    set mcy $top
    set cy $bot
    set cx $xl
    for {set i 1} {$i <= $n} {incr i} {
	set mcy [expr $mcy + 2*[pmux_ht_sep $c]]
	set cy [expr ($cy + [pmux_ht_sep $c])]
	set cx [expr ($cx + [min_sep $c])]
	set iy [expr ($bot-($i*[pmux_ht_sep $c]/($n+1)))]
	$c create line $xl $cy $cx $cy $cx $iy -fill $gcolor
    }
    $c create line $xl [expr $bot+[pmux_ht_sep $c]] \
		   $xl [expr $bot+$n*[pmux_ht_sep $c]] -width 3
    set ycmid [expr round($bot + $n*[pmux_ht_sep $c]/2)]
    $c create line $xlinp $ycmid $xl $ycmid -width 3
    lappend inp_locs $xlinp $ydata
    lappend inp_locs $xlinp $yother
    lappend inp_locs $xlinp $ycmid
    return [list $inp_locs [list $x $y]]
}

proc draw_gen_mux2 {out_neg c tag x y} {
    global gcolor fc
    set x [expr round($x)]
    set y [expr round($y)]
    if {$out_neg != ""} {
	set nx [expr ($x-[inv_rad $c]*2+2)]
	$c create oval [expr ($x-2*[inv_rad $c])] \
		       [expr ($y-[inv_rad $c])] \
		       $x [expr ($y+[inv_rad $c])] \
		       -outline $gcolor -fill $fc -tags $tag
    } else {
	set nx $x
    }
    add_value_field $c $tag $x $y
    set ht  [expr round(4*[rect_ht $c])]
    set wid [expr round(0.66*[rect_wid $c])]
    set xl [expr round($nx-$wid)]
    set top [expr $y-$ht/2.0]
    set bot [expr $y+$ht/2.0]
    $c create polygon \
	    $xl $top \
	    $nx  [expr $top+[min_sep $c]] \
	    $nx  [expr $bot-[min_sep $c]] \
	    $xl $bot \
	    $xl $top \
	    -outline $gcolor -fill $fc -tags $tag
    set inps {}
    set sep [expr ($bot-$top)/4.0]
    set cy [expr round($top+$sep)]
    for {set i 1} {$i <= 2} {incr i} {
	lappend inp_locs $xl $cy
	set t [$c create text $xl $cy \
		    -anchor w -justify left \
		    -font $::mfont($c) -text [expr $i-1] \
		    -tags $tag -fill $gcolor]
	add_font_tags $c $t _IsTeXt_
        set itag [format {%s____%d} $tag $i]
	set cy [expr round($cy + 2*$sep)]
    }
    set mcy $top
    set cx [expr round(($nx+$xl)/2.0)]
    set c_conn_y [expr $bot - [min_sep $c]/2]
    set c_in_y [expr round($bot + [min_sep $c])]
    lappend inp_locs $xl $c_in_y
    set itag [format {%s____%d} $tag 3]
    $c create line $xl $c_in_y $cx $c_in_y $cx $c_conn_y \
	    -fill $gcolor -tags "$itag _WiRe_"
    return [list $inp_locs [list $x $y]]
}

proc draw_mux2 {c tag x y} {
    return [draw_gen_mux2 "" $c $tag $x $y]
}

proc draw_imux2 {c tag x y} {
    return [draw_gen_mux2 "1" $c $tag $x $y]
}

proc draw_ITE {c tag x y} {
    global gcolor fc
    set x [expr ($x)]
    set y [expr ($y)]
    add_value_field $c $tag $x $y
    set wid [expr 4*[min_sep $c]]
    set xl [expr ($x-$wid)]
    set top [expr $y-3*[min_sep $c]]
    set bot [expr $y+3*[min_sep $c]]
    $c create polygon \
	    $xl $top \
	    $x  [expr $top+[min_sep $c]] \
	    $x  [expr $bot-[min_sep $c]] \
	    $xl $bot \
	    $xl $top \
	    -outline $gcolor -fill $fc -tags $tag
    set inps {}
    set cy $top
    for {set i 1} {$i <= 2} {incr i} {
	set cy [expr ($cy + 2*[min_sep $c])]
	lappend inp_locs [expr round($xl)] [expr round($cy)]
	set t [$c create text $xl $cy \
		    -anchor w -justify left \
		    -font $::mfont($c) -text [expr 2-$i] \
		    -tags $tag -fill $gcolor]
	add_font_tags $c $t _IsTeXt_
    }
    set cy [expr ($top - [min_sep $c])]
    set cx [expr ($xl + 2*[min_sep $c])]
    set iy [expr ($top+([min_sep $c]/2))]
    set ch_tag [format {%s____%d} $tag 1]
    $c create line $xl $cy $cx $cy $cx $iy \
	    -fill $gcolor -tags "$ch_tag _WiRe_"
    lappend inp_locs [expr round($xl)] [expr round($cy)]
    set dummy_cy [expr ($bot + [min_sep $c])]
    set dummy_iy [expr ($bot - ([min_sep $c]/2))]
    $c create line $cx $dummy_cy $cx $dummy_iy -fill white \
            -tags _DoNotChangeColor_
    return [list $inp_locs [list $x $y]]
}

proc draw_vconnection {c tag x y} {
    set mrad [conn_rad $c]
    if { [fl_is_vector $c $tag] } { set mrad [expr [conn_rad $c]*4] }
    $c create oval [expr $x-$mrad] [expr $y-$mrad] \
		   [expr $x+$mrad] [expr $y+$mrad] \
		   -outline black -fill black \
		   -tags "$tag _WiRe_"
}

proc draw_vertical_wire {oht c tag x y} {
    global mfont
    global gcolor
    set ht [expr ($::cur_zoom_factor($c)*$oht/100.0)]
    set y1 $y
    set y2 [expr $y+$ht]
    if { [fl_is_vector $c $tag] } {
        $c create line $x $y1 $x $y2 \
                -fill $gcolor -tags "$tag _WiRe_" -width 3
    } else {
        $c create line $x $y1 $x $y2 \
                -fill $gcolor -tags "$tag _WiRe_"
    }
    return [list [list $x [expr round($y2)]] [list $x [expr round($y)]]]
}

proc out_connect {c tag x1 y1 x2 y2 inp_nbr tot_inps mtag} {
    global gcolor mfont
    if { $y1 < $y2 } {
	# Connection going upwards
	set xmid [expr ($x2-($inp_nbr)*[min_sep $c])]
    } else {
	# Connection going downwards
	set xmid [expr ($x2-($tot_inps-$inp_nbr+1)*[min_sep $c])]
    }
    set wtag [format {%s____%d} $mtag $inp_nbr]
    if { [fl_is_vector $c $tag] } {
	$c create line $x1 $y1 $xmid $y1 $xmid $y2 $x2 $y2 \
		-fill $gcolor -tags "$tag _WiRe_" -width 3
        # Widen internal wires as well
        foreach iw [$c find withtag $wtag&&_WiRe_] {
            $c itemconfigure $iw -width 3
        }
    } else {
	$c create line $x1 $y1 $xmid $y1 $xmid $y2 $x2 $y2 \
		-fill $gcolor -tags "$tag _WiRe_"
    }
    # Add the wire tag to internal wires of the driven instance as well
    set lbl_txt [$c create text [expr $x1+2*[inv_rad $c]] \
				[expr $y1+1+2.3*[inv_rad $c]] \
				-fill blue -anchor center -justify center \
				-font $mfont($c) -text ""]
    add_font_tags $c $lbl_txt _IsTeXt_
    lappend ::lbl_tag($tag) [list $c $lbl_txt]
}

proc connect {c tag x1 y1 x2 y2 inp_nbr tot_inps mtag} {
    global mfont
    global gcolor
    if { $y1 < $y2 } {
	# Connection going upwards
	set xmid [expr ($x2-($inp_nbr)*[min_sep $c])]
    } else {
	# Connection going downwards
	set xmid [expr ($x2-($tot_inps-$inp_nbr+1)*[min_sep $c])]
    }
    set wtag [format {%s____%d} $mtag $inp_nbr]
    if { [fl_is_vector $c $tag] } {
	$c create line $x1 $y1 $xmid $y1 $xmid $y2 $x2 $y2 \
		-fill $gcolor -tags "$tag _WiRe_" -width 3
        # Widen internal wires as well
        foreach iw [$c find withtag $wtag&&_WiRe_] {
            $c itemconfigure $iw -width 3
        }
    } else {
	$c create line $x1 $y1 $xmid $y1 $xmid $y2 $x2 $y2 \
		-fill $gcolor -tags "$tag _WiRe_"
    }
    # Add the wire tag to internal wires of the driven instance as well
    $c addtag $tag withtag $wtag
    set lbl_txt [$c create text [expr $x1+2*[inv_rad $c]] \
				[expr $y1+1+2.3*[inv_rad $c]] \
				-fill blue -anchor center -justify center \
				-font $mfont($c) -text ""]
    add_font_tags $c $lbl_txt _IsTeXt_
    lappend ::lbl_tag($tag) [list $c $lbl_txt]
}

proc draw_output1 {txt c tag x y} {
    global gcolor mfont 
    set txt [clean_name $txt] 
    set txt [clean_name $txt] 
    if { [string length $txt] > 20 } {
        set short_name [format {%s......%s} [string range $txt 0 9] \
                                         [string range $txt end-9 end]]
    } else {
        set short_name $txt
    }
    set xt [expr round($x+[min_sep $c])] 
    set t [$c create text $xt $y -anchor w -justify left \
            -font $mfont($c) -text $short_name \
            -tags $tag -fill $gcolor]
    add_font_tags $c $t _IsTeXt_
    add_font_tags $c $t {$txt}
    lappend inp_locs $x $y
    return [list $inp_locs [list $x $y]]
}

proc draw_output {text_items c tag x y} {
    global gcolor mfont
    set n [llength $text_items]
    set ht [expr (($n+1)*2*[out_sep $c])]
    set x1 [expr $x-[out_sep $c]]
        set cur_y $y
    if { $n == 1 } {
    } else {
        set y1 [expr $y-$ht/2]
        set cur_y [expr $y1+[out_sep $c]]
    }
    foreach text_item $text_items {
        set text_item [clean_name $text_item]
        set txt ""
        set sep ""
        foreach line [split $text_item] {
            append txt $sep [clean_name $line]
            set sep " "
        }
        set txt [clean_name $txt]
        if { [string length $txt] > 20 } {
            set short_name [format {%s......%s} [string range $txt 0 9] \
                                             [string range $txt end-9 end]]
        } else {
            set short_name $txt
        }
        set t [$c create text $x $cur_y -anchor w -justify left \
                -font $mfont($c) -text $short_name \
                -tags $tag -fill $gcolor]
        if { [llength [split $txt "\n"]] > 1 } {
            $c bind $t <Button-1> "post_node_name %W $tag; break"
        }
        add_font_tags $c $t _IsTeXt_
        add_font_tags $c $t {$txt}
        lappend inp_locs [expr round($x1)] [expr round($cur_y)]
        set cur_y [expr $cur_y + 2*[out_sep $c]]
    }
    return [list $inp_locs [list $x $y]]
}


proc draw_ifc_input {txt c tag x y} {
    global gcolor fc mfont sfont
    set xl [expr ($x-[wire_len $c])]
    if { [fl_is_vector $c $tag] } {
	$c create line $x $y $xl $y -fill $gcolor \
	    -tags "$tag _WiRe_" -width 3
	$c create line [expr ($xl-[inp_ht $c])] [expr ($y-[inp_ht $c]/2)] \
		$xl $y -fill $gcolor -tags "$tag _WiRe_" -width 3
	$c create line $xl $y \
		[expr ($xl-[inp_ht $c])] [expr ($y+[inp_ht $c]/2)] \
		$xl $y -fill $gcolor -tags "$tag _WiRe_" -width 3
    } else {
	$c create line $x $y $xl $y -fill $gcolor \
	    -tags "$tag _WiRe_"
	$c create line [expr ($xl-[inp_ht $c])] [expr ($y-[inp_ht $c]/2)] \
		$xl $y -fill $gcolor -tags "$tag _WiRe_"
	$c create line $xl $y \
		[expr ($xl-[inp_ht $c])] [expr ($y+[inp_ht $c]/2)] \
		$xl $y -fill $gcolor -tags "$tag _WiRe_"
    }
    add_value_field $c $tag [expr $x-[wire_len $c]] $y
    set sep ""
    set res ""
    foreach t $txt {
        append res $sep [clean_name $t]
        set sep "\n"
    }
    set t [$c create text [expr $xl-[inp_ht $c]-2] $y \
			    -anchor e -font $mfont($c) -text $res \
                            -justify right \
			    -tags $tag -fill $gcolor]
    add_font_tags $c $t _IsTeXt_
    return [list {} [list $x $y]]
}

proc draw_input {txt c tag x y} {
    global gcolor fc mfont sfont
    set xl [expr ($x-[wire_len $c])]
    if { [fl_is_vector $c $tag] } {
	$c create line $x $y $xl $y -fill $gcolor \
	    -tags "$tag _WiRe_" -width 3
    } else {
	$c create line $x $y $xl $y -fill $gcolor -tags "$tag _WiRe_"
    }
    $c create polygon \
	    [expr ($xl-[inp_ht $c])] [expr ($y+[inp_ht $c]/2)] \
	    [expr ($xl-[inp_ht $c])] [expr ($y-[inp_ht $c]/2)] \
	    $xl $y \
	    [expr ($xl-[inp_ht $c])] [expr ($y+[inp_ht $c]/2)] \
	    -outline $gcolor -fill $fc -tags $tag
    add_value_field $c $tag [expr $x-[wire_len $c]] $y
    set sep ""
    set res ""
    foreach t $txt {
        append res $sep [clean_name $t]
        set sep "\n"
    }
    set t [$c create text [expr $xl-[inp_ht $c]-2] $y \
			    -anchor e -font $mfont($c) -text $res \
                            -justify right \
			    -tags $tag -fill $gcolor]
    add_font_tags $c $t _IsTeXt_
    return [list {} [list $x $y]]
}

proc draw_constant {txt c tag x y} {
    return [draw_input $txt $c $tag $x $y]
}

proc draw_dangling_input {txt c tag x y} {
    global gcolor fc mfont sfont
    set xl [expr ($x-[wire_len $c])]
    if { [fl_is_vector $c $tag] } {
	$c create line $x $y $xl $y -fill $gcolor \
	    -tags $tag -width 3
    } else {
	$c create line $x $y $xl $y -fill $gcolor -tags $tag
    }
    $c create rectangle \
	    [expr ($xl-[inp_ht $c])] [expr ($y+[inp_ht $c]/2)] \
	    $xl [expr ($y-[inp_ht $c]/2)] \
	    -outline $gcolor -fill $fc -tags $tag
    add_value_field $c $tag [expr $x-[wire_len $c]] $y
    set sep ""
    set res ""
    foreach t $txt {
        append res $sep [clean_name $t]
        set sep "\n"
    }
    set t [$c create text [expr $xl-[inp_ht $c]-2] $y \
			    -anchor e -font $mfont($c) -text $res \
                            -justify right \
			    -tags $tag -fill $gcolor]
    add_font_tags $c $t _IsTeXt_
    return [list {} [list $x $y]]
}

proc draw_internal {txt c tag x y} {
    global gcolor fc mfont sfont
    set xl [expr ($x-[wire_len $c])]
    set yl [expr ($y+[wire_len $c])]
    if { [fl_is_vector $c $tag] } {
	$c create line $x $y $xl $y $xl $yl -fill $gcolor \
	    -tags $tag -width 3
    } else {
	$c create line $x $y $xl $y $xl $yl -fill $gcolor -tags $tag
    }
    $c create polygon \
	    [expr ($xl-[inp_ht $c])] [expr ($yl+[inp_ht $c]/2)] \
	    [expr ($xl+[inp_ht $c])] [expr ($yl+[inp_ht $c]/2)] \
	    $xl $yl \
	    [expr ($xl-[inp_ht $c])] [expr ($yl+[inp_ht $c]/2)] \
	    -outline $gcolor -fill $fc -tags $tag
    add_value_field $c $tag [expr $x-[wire_len $c]] $y
    set sep ""
    set res ""
    foreach t $txt {
        append res $sep [clean_name $t]
        set sep "\n"
    }
    set t [$c create text [expr $xl-[inp_ht $c]-2] $yl \
			    -anchor n -font $mfont($c) -text $res \
                            -justify right \
			    -tags $tag -fill $gcolor]
    add_font_tags $c $t _IsTeXt_
    return [list {} [list $x $y]]
}

proc draw_repeat_nd {c tag x y} {
    global gcolor
    set xl [expr round($x-[wire_len $c]/2)]
    if { [fl_is_vector $c $tag] } {
	$c create line $x $y $xl $y -fill $gcolor \
	    -tags "$tag _WiRe_" -width 3
    } else {
	$c create line $x $y $xl $y -fill $gcolor \
	    -tags "$tag _WiRe_"
    }
    set rect [$c create rectangle [expr ($xl-2*[ptr_ht $c])] \
				[expr ($y+[ptr_ht $c])] \
				$xl [expr ($y-[ptr_ht $c])] \
				-fill black -outline $gcolor \
				-tags $tag]
    $c addtag RePeAt withtag $rect
    return [list [list $xl $y] [list $x $y]]
}

proc draw_feedback_node {c tag x y} {
    global gcolor
    set xl [expr ($x-[wire_len $c]/2)]
    if { [fl_is_vector $c $tag] } {
        $c create line $x $y $xl $y -fill $gcolor \
            -tags "$tag _WiRe_" -width 3
    } else {
        $c create line $x $y $xl $y -fill $gcolor \
            -tags "$tag _WiRe_"
    }

    set rect [$c create rectangle \
            [expr ($xl-[buf_wid $c])] [expr ($y+[buf_ht $c]/2)] \
            $xl [expr ($y-[buf_ht $c]/2)] \
            -fill white -outline $gcolor \
            -tags $tag]
    $c addtag RePeAt withtag $rect

    set iw [expr [buf_wid $c]/4]
    set ih [expr [buf_wid $c]/2]
    set ilx [expr $xl-[buf_wid $c]/2-$iw/2]
    set ily [expr $y+$ih/2]
    set irx [expr $xl-[buf_wid $c]/2+$iw/2]
    set iry [expr $y-$ih/2]
    set irect [$c create rectangle $ilx $ily $irx $iry \
            -fill white -outline $gcolor -tags $tag]
    $c addtag RePeAt withtag $irect

    set xinp [expr (($xl-[buf_wid $c])+$ilx)/2] 
    set yinp [expr $y+($ily-$iry)/3]
    set ybelow [expr (($y+[buf_ht $c]/2)+$ily)/2]
    set iline [$c create line \
        $irx $y \
        [expr ($irx+$xl)/2] $y \
        [expr ($irx+$xl)/2] $ybelow \
        $xinp $ybelow \
        $xinp $yinp \
        $ilx $yinp \
        -fill $gcolor -tags $tag]
    $c addtag RePeAt withtag $iline

    return [list [list $xl $y] [list $x $y]]
}


proc draw_repeat_nd_dbg {rank c tag x y} {
    global gcolor mfont
    set xl [expr ($x-[wire_len $c]/2)]
    if { [fl_is_vector $c $tag] } {
	$c create line $x $y $xl $y -fill $gcolor \
	    -tags "$tag _WiRe_" -width 3
    } else {
	$c create line $x $y $xl $y -fill $gcolor \
	    -tags "$tag _WiRe_"
    }
    set rect [$c create rectangle [expr ($xl-2*[ptr_ht $c])] \
				[expr ($y+[ptr_ht $c])] \
				$xl [expr ($y-[ptr_ht $c])] \
				-fill black -outline $gcolor \
				-tags $tag]
    set f2 [$c create text $x $y -anchor sw -font $mfont($c) \
		-text $rank -tags $tag]
    add_font_tags $c $f2 _IsTeXt_
    $c addtag RePeAt withtag $rect
    return [list [list $xl $y] [list $x $y]]
}

proc draw_repeat_nd_out {c tag x y} {
    global gcolor
    set xl [expr ($x-2*[wire_len $c])]
    $c create line $x $y $xl $y -fill $gcolor -tags $tag
    $c create rectangle [expr ($x-2*[ptr_ht $c])] [expr ($y+[ptr_ht $c])] \
				$x [expr ($y-[ptr_ht $c])] \
				-fill black -outline $gcolor \
				-tags [list $tag RePeAt]
    return [list [list $xl $y] [list $x $y]]
}


proc get_suffix_basename {name} {
    set bn [file tail $name]
    if { $bn == $name } {
	return $name
    }
    return ".../$bn"
}

proc draw_incomplete {name c tag x y} {
    global gcolor mfont
    set xl [expr ($x-[wire_len $c]/2)]
    set f1 [$c create line $x $y $xl $y -fill $gcolor \
		-tags $tag]
    if [catch {set sname [get_suffix_basename $name]}] {
	set sname ""
    }
    set left_x  [expr $xl - [expr 3.5*[min_sep $c]]]
    set right_x [expr $xl - [expr 0.5*[min_sep $c]]]
    add_three_horizontal_dots $c $left_x $right_x $y $tag
    set f2 [$c create text $left_x $y -anchor e -font $mfont($c) \
		-text $sname -tags $tag]
    add_font_tags $c $f2 _IsTeXt_
    add_value_field $c $tag $x $y
    if { [llength [split $tag "\n"]] > 1 } {
        $c bind $f2 <Button-1> "post_node_name %W $tag; break"
    }
    $c bind $f2 <Button-2> {
	set tags [%W gettags current]
	set nodes [get_anon_name $tags]
	after idle [list cb:expand_fanin %W $nodes 1]
    }
    return [list {} [list $x $y]]
}


proc draw_incomplete_grey {name c tag x y} {
    global gcolor mfont
    set xl [expr ($x-[wire_len $c]/2)]
    set f1 [$c create line $x $y $xl $y -fill $gcolor \
		-tags $tag]
    if [catch {set sname [get_suffix_basename $name]}] {
	set sname ""
    }
    set f2 [$c create text $xl $y -anchor se -font $mfont($c) \
		-text [format "%s ... " $sname] \
		-tags $tag]
    add_font_tags $c $f2 _IsTeXt_
    add_value_field $c $tag $x $y
    if { [llength [split $tag "\n"]] > 1 } {
        $c bind $f2 <Button-1> "post_node_name %W $tag; break"
    }
    $c bind $f2 <Button-2> {
	set tags [%W gettags current]
	set node [get_anon_name $tags]
	cb:expand_fanin %W $node 1
    }
    return [list {} [list $x $y]]
}

proc draw_stop_symbol {txt c tag x y} {
    global gcolor sfont mfont
    set color_tag [format {_DeFaUlTcOlOr_%s} red]
    $c create polygon \
		    $x $y \
		    [expr $x-3*[stop_sz $c]] [expr $y-6*[stop_sz $c]] \
		    [expr $x-11*[stop_sz $c]] [expr $y-6*[stop_sz $c]] \
		    [expr $x-14*[stop_sz $c]] $y \
		    [expr $x-11*[stop_sz $c]] [expr $y+6*[stop_sz $c]] \
		    [expr $x-3*[stop_sz $c]] [expr $y+6*[stop_sz $c]] \
            -outline white -fill red -tags [list $tag $color_tag]
    set f [$c create text [expr $x-7*[stop_sz $c]] $y \
		-tags [list $tag _DoNotChangeColor_] -fill white \
		-font $sfont($c) -text "STOP"]
    add_font_tags $c $f _IsTeXt_
    add_value_field $c $tag $x $y
    set t [$c create text [expr $x-15*[stop_sz $c]] $y \
			    -anchor e -font $mfont($c) -text $txt \
                            -justify right \
			    -tags $tag -fill $gcolor]
    add_font_tags $c $t _IsTeXt_
    return [list {} [list $x $y]]
}

proc draw_const_stop_symbol {txt name c tag x y} {
    global gcolor sfont mfont
    set color_tag [format {_DeFaUlTcOlOr_%s} orange]
    $c create polygon \
		    $x $y \
		    [expr $x-3*[stop_sz $c]] [expr $y-6*[stop_sz $c]] \
		    [expr $x-11*[stop_sz $c]] [expr $y-6*[stop_sz $c]] \
		    [expr $x-14*[stop_sz $c]] $y \
		    [expr $x-11*[stop_sz $c]] [expr $y+6*[stop_sz $c]] \
		    [expr $x-3*[stop_sz $c]] [expr $y+6*[stop_sz $c]] \
            -outline white -fill orange -tags [list $tag $color_tag]
    set f [$c create text [expr $x-7*[stop_sz $c]] $y \
		-tags [list $tag _DoNotChangeColor_] -fill white \
		-font $sfont($c) -text CONST]
    add_font_tags $c $f _IsTeXt_
    add_value_field $c $tag $x $y
    if [catch {set sname [get_suffix_basename $name]}] {
        set sname ""
    }
    set t [$c create text [expr $x-15*[stop_sz $c]] $y \
			    -anchor e -font $mfont($c) -text $sname \
                            -justify right \
			    -tags $tag -fill $gcolor]
    add_font_tags $c $t _IsTeXt_

    set tt [$c create text [expr $x-1*[stop_sz $c]] [expr $y-6*[stop_sz $c]] \
			    -anchor se -font $mfont($c) -text $txt \
                            -justify right \
			    -tags $tag -fill $gcolor]
    add_font_tags $c $tt _IsTeXt_
    return [list {} [list $x $y]]
}


proc draw_box_pat {inputs pic_proc pic_proc_list c tag x y} {
    global gcolor fc sfont
    set rht [expr (($inputs+1)*[rect_ht $c])]
    $c create rectangle [expr ($x-[rect_wid $c])] [expr ($y+$rht/2)] \
	    $x [expr ($y-$rht/2)] -outline $gcolor -fill $fc -tags $tag
    add_value_field $c $tag [expr $x+[letter_sz $c]] $y
    eval $pic_proc $c [expr ($x-[rect_wid $c]/2)] $y $tag
    set nx [expr ($x-[rect_wid $c])]
    set lowy [expr ($y-$rht/2+$rht/($inputs*2))]
    set sep [expr ($rht/$inputs)]
    set inp_patterns [llength $pic_proc_list]
    set xl [expr ($nx)]
    for {set i 0} {$i < $inputs} {incr i} {
	set yl [expr ($lowy+$i*$sep)]
	if {$inp_patterns != 0} {
	    [lindex $pic_proc_list $i] $c $nx $yl $tag
	}
	$c create line $xl $yl $nx $yl -fill $gcolor
	lappend inp_locs [expr round($xl)] [expr round($yl)]
    }
    return [list $inp_locs [list $x $y]]
}

proc get_width {c x y cur_max txt} {
    set f [$c create text $x $y -anchor w -justify left \
	    -font $::sfont($c) -text $txt]
    val {x1 y1 x2 y2} [$c bbox $f]
    $c delete $f
    set new_wid [expr abs($x2-$x1)]
    if { $new_wid > $cur_max } {
	set cur_max $new_wid
    }
    return $cur_max
}

proc draw_fub {module inst inames onames c tag x y} {
    global gcolor fc sfont
    #
    set inps [llength $inames]
    set outs [llength $onames]
    #
    # Compute the needed width
    #
    set twid [rect_wid $c]
    set twid [get_width $c $x $y $twid $module]
    set twid [get_width $c $x $y $twid $inst]
    set iwid [letter_sz $c]
    foreach name $inames {
	val {fname anames} $name
	set iwid [get_width $c $x $y $iwid $fname]
    }
    set owid [letter_sz $c]
    foreach name $onames {
	val {fname anames} $name
	set owid [get_width $c $x $y $owid $fname]
    }
    set rwid [max $twid [expr $iwid + $owid + 5*[letter_sz $c]]]
    set xr [expr $x-5*[min_sep $c]]
    set rht [expr round(([max $inps $outs]+1)*[rect_ht $c])] 
    set erht [expr $rht+4*[min_txt_sep $c]] 
    #
    # Draw box with module name inside and instance name above
    #
    $c create rectangle [expr ($xr-$rwid)] [expr ($y+$erht/2)] \
	    $xr [expr ($y-$erht/2)] -outline $gcolor -fill $fc -tags $tag
    set mid_x [expr ($xr-$rwid/2)]
    set top_y [expr $y-$erht/2]
    set f [$c create text $mid_x $top_y -anchor n -justify center \
	    -font $::mfont($c) -text $module]
    add_font_tags $c $f _IsTeXt_
    set f [$c create text $mid_x $top_y -anchor s -justify center \
	    -font $::mfont($c) -text $inst]
    add_font_tags $c $f _IsTeXt_
    # Inputs
    set nx [expr ($xr-$rwid)]
    set lowy [expr ($y-$rht/2+$rht/($inps*2))]
    set sep [expr ($rht/$inps)]
    set xl [expr ($nx)]
    for {set i 0} {$i < $inps} {incr i} {
	set yl [expr ($lowy+$i*$sep)]
	val {fname anames} [lindex $inames $i]
	set f [$c create text $nx $yl -anchor w -justify left \
		-font $::mfont($c) -text $fname]
	add_font_tags $c $f _IsTeXt_
	$c create line $xl $yl $nx $yl -fill $gcolor
	lappend inp_locs [expr round($xl)] [expr round($yl)]
    }


    # Outputs
    set nx $xr
    set lowy [expr ($y-$rht/2+$rht/($outs*2))]
    set sep [expr ($rht/$outs)]
    set m_lo_y [expr round($lowy-[min_sep $c]/2)]
    set m_hi_y [expr round($lowy+($outs-1)*$sep+[min_sep $c]/2)]
    set m_x [expr $x-[min_sep $c]/3]
    $c create rectangle $m_x $m_lo_y $x $m_hi_y \
	    -outline $gcolor -fill $fc -tags $tag
    set xl [expr $xr]
    for {set i 0} {$i < $outs} {incr i} {
	set yl [expr round($lowy+$i*$sep)]
	val {fname anames} [lindex $onames $i]
	set f [$c create text $xl $yl -anchor e -justify right \
		-font $::sfont($c) -text $fname]
	add_font_tags $c $f _IsTeXt_
	set wtag [fl_vecs2tags $c $anames]
	if [fl_is_vector_name $fname] {
	    $c create line $xl $yl $m_x $yl -fill $gcolor -width 3 \
		-tags "$wtag _WiRe_"
	} else {
	    $c create line $xl $yl $m_x $yl -fill $gcolor \
		-tags "$wtag _WiRe_"
	}
	add_value_field $c $wtag $xl $yl
	lappend out_locs [expr round($x)] [expr round($yl)]
    }
    return [list $inp_locs $out_locs]
}

proc draw_function {nbr_inps nbr_outs name c tag x y} {
    global gcolor fc sfont mfont
    #
    # Determine the width needed for text
    #
    set txtwid [expr [string length $name].0*12.0*$::sc($c)]
    if { [expr $txtwid > [rect_wid $c]] } {
	set wid $txtwid
    } else {
	set wid [rect_wid $c]
    }
    set rht [expr (($nbr_inps+1)*[rect_ht $c])]
    set lx [expr $x-$wid]
    set t [$c create rectangle $lx [expr ($y+$rht/2)] \
                               $x  [expr ($y-$rht/2)] \
                               -outline $gcolor -fill $fc \
                               -tags [list _Is_Hierarchical_ $tag]]
    #
    set xc [expr $x-$wid/2.0]
    set f [$c create text $xc $y -anchor c -justify center \
		-font $mfont($c) -text $name \
                -tags [list _Is_Hierarchical_ $tag]]
    add_font_tags $c $f _IsTeXt_
    #
    add_value_field $c $tag [expr $x+[letter_sz $c]] $y
    if { $nbr_inps > 0 } {
        set lowy [expr ($y-$rht/2+$rht/($nbr_inps*2))]
        set sep [expr ($rht/$nbr_inps)]
        for {set i 0} {$i < $nbr_inps} {incr i} {
            set yl [expr ($lowy+$i.0*$sep)]
            lappend inp_locs [expr round($lx)] [expr round($yl)]
        }
        return [list $inp_locs [list $x $y]]
    } else {
        return [list {} [list $x $y]]
    }
}

proc draw_hier {gid nbr_inps nbr_outs name c tag x y} {
    global gcolor fc sfont mfont
    #
    # Determine the width needed for text
    #
    set txtwid [expr [string length $name].0*12.0*$::sc($c)]
    if { [expr $txtwid > [rect_wid $c]] } {
	set wid $txtwid
    } else {
	set wid [rect_wid $c]
    }
    set rht [expr (($nbr_inps+1)*[rect_ht $c])]
    set lx [expr $x-$wid]
    set t [$c create rectangle $lx [expr ($y+$rht/2)] \
                               $x  [expr ($y-$rht/2)] \
                               -outline $gcolor -fill $fc \
                               -tags [list _Is_Hierarchical_ $gid $tag]]
    #
    set xc [expr $x-$wid/2.0]
    set f [$c create text $xc $y -anchor c -justify center \
		-font $mfont($c) -text $name \
                -tags [list _Is_Hierarchical_ $gid $tag]]
    add_font_tags $c $f _IsTeXt_
    #
    add_value_field $c $tag [expr $x+[letter_sz $c]] $y
    if { $nbr_inps > 0 } {
        set lowy [expr ($y-$rht/2+$rht/($nbr_inps*2))]
        set sep [expr ($rht/$nbr_inps)]
        for {set i 0} {$i < $nbr_inps} {incr i} {
            set yl [expr ($lowy+$i.0*$sep)]
            lappend inp_locs [expr round($lx)] [expr round($yl)]
        }
        return [list $inp_locs [list $x $y]]
    } else {
        return [list {} [list $x $y]]
    }
}

proc draw_instance {gid nbr_inps nbr_outs module_name inst_name c tag x y} {
    global gcolor fc sfont mfont
    #
    # Determine the width needed for text
    #
    set mtxtwid [expr [string length $module_name].0*12.0*$::sc($c)]
    set itxtwid [expr [string length $inst_name].0*12.0*$::sc($c)]
    set txtwid [expr ($mtxtwid > $itxtwid)? $mtxtwid : $itxtwid]
    if { [expr $txtwid > [rect_wid $c]] } {
	set wid $txtwid
    } else {
	set wid [rect_wid $c]
    }
    set rht [expr (($nbr_inps+1)*[rect_ht $c])]
    set lx [expr $x-$wid]
    set t [$c create rectangle $lx [expr ($y+$rht/2)] \
                               $x  [expr ($y-$rht/2)] \
                               -outline $gcolor -fill $fc \
                               -tags [list _Is_Hierarchical_ $gid $tag]]
    #
    set xc [expr $x-$wid/2.0]
    set f [$c create text $xc $y -anchor c -justify center \
		-font $mfont($c) -text $module_name \
                -tags [list _Is_Hierarchical_ $gid $tag]]
    add_font_tags $c $f _IsTeXt_
    #
    set ff [$c create text $xc [expr ($y-$rht/2)] -anchor s -justify center \
		-font $mfont($c) -text $inst_name \
                -tags [list _Is_Hierarchical_ $gid $tag]]
    add_font_tags $c $ff _IsTeXt_
    #
    add_value_field $c $tag [expr $x+[letter_sz $c]] $y
    if { $nbr_inps > 0 } {
        set lowy [expr ($y-$rht/2+$rht/($nbr_inps*2))]
        set sep [expr ($rht/$nbr_inps)]
        for {set i 0} {$i < $nbr_inps} {incr i} {
            set yl [expr ($lowy+$i.0*$sep)]
            lappend inp_locs [expr round($lx)] [expr round($yl)]
        }
        return [list $inp_locs [list $x $y]]
    } else {
        return [list {} [list $x $y]]
    }
}

proc draw_buf_pat {inp_neg out_neg c tag x y} {
    global gcolor fc sfont
    if {$out_neg != ""} {
	set nx [expr $x-[inv_rad $c]*2]
	$c create oval [expr $x-2*[inv_rad $c]] [expr $y-[inv_rad $c]] \
		       $x [expr $y+[inv_rad $c]] \
		       -outline $gcolor -fill $fc -tags $tag
    } else {
	set nx $x
    }
    $c create polygon [expr $nx-[buf_wid $c]] [expr $y+[buf_ht $c]/2] \
			    [expr $nx-[buf_wid $c]] [expr $y-[buf_ht $c]/2] \
			    $nx $y \
			    [expr $nx-[buf_wid $c]] [expr $y+[buf_ht $c]/2] \
			    -outline $gcolor -fill $fc -tags $tag
    add_value_field $c $tag $x $y
    set nx [expr $nx-[buf_wid $c]]
    if {$inp_neg != ""} {
	$c create oval [expr $nx-2*[inv_rad $c]] [expr $y-[inv_rad $c]] \
		       $nx [expr $y+[inv_rad $c]] \
		       -outline $gcolor -fill $fc -tags $tag
	set nx [expr $nx-[inv_rad $c]*2]
    }
    return [list [list [expr round($nx)] $y] [list $x $y]]
}


proc draw_nothing {c tag x y} { return [list [list $x $y] [list $x $y]] }

proc draw_wire {olen c tag x y} {
    global fc sfont
    global gcolor
    set len [expr ($::cur_zoom_factor($c)*$olen/100.0)]
    set xl [expr $x-$len]
    if { [fl_is_vector $c $tag] } {
        $c create line $xl $y $x $y \
                -fill $gcolor -tags "$tag _WiRe_" -width 3
    } else {
        $c create line $xl $y $x $y \
                -fill $gcolor -tags "$tag _WiRe_"
    }
    return [list [list [expr round($xl)] $y] [list $x $y]]
}

proc draw_dummy_wire {olen c tag x y} {
    global fc sfont
    global gcolor
    set len [expr ($::cur_zoom_factor($c)*$olen/100.0)]
    set xl [expr $x-$len]
    if { [fl_is_vector $c $tag] } {
        $c create line $xl $y $x $y \
                -fill $gcolor -tags "$tag _WiRe_" -width 3
    } else {
        $c create line $xl $y $x $y \
                -fill $gcolor -tags "$tag _WiRe_"
    }
    return [list [list [expr round($xl)] $y] [list $x $y]]
}

proc draw_and_pat {embedded inpnegl out_neg c tag x y {remap {}}} {
    global gcolor fc sfont
    if {$out_neg != ""} {
	set nx [expr ($x-[inv_rad $c]*2+2)]
	$c create oval [expr ($x-2*[inv_rad $c])] \
		       [expr ($y-[inv_rad $c])] \
		       $x [expr ($y+[inv_rad $c])] \
		       -outline $gcolor -fill $fc -tags $tag
    } else {
	set nx $x
    }
    $c create polygon \
	[expr ($nx-[and_wid $c])] [expr ($y-[and_ht $c]/2)] \
	[expr ($nx-[and_wid $c])] [expr ($y+[and_ht $c]/2)] \
	[expr ($nx-[and_wid $c])] [expr ($y+[and_ht $c]/2)] \
	[expr ($nx-[and_wid $c]/4)] [expr ($y+[and_ht $c]/2)] \
	[expr 1.0*$nx+3] [expr $y] \
	[expr ($nx-[and_wid $c]/4)] [expr ($y-[and_ht $c]/2)] \
	[expr ($nx-[and_wid $c])] [expr ($y-[and_ht $c]/2)] \
	[expr ($nx-[and_wid $c])] [expr ($y-[and_ht $c]/2)] \
	-smooth yes -outline $gcolor -fill $fc \
	-tags $tag
    if { $embedded == 0 } { add_value_field $c $tag $x $y }
    set nx [expr round($nx-[and_wid $c])]
    set inputs [llength $inpnegl]
    set sep [expr ([and_ht $c]/$inputs)]
    set min_sep [expr 3*[inv_rad $c]]
    if [expr $sep < $min_sep] {
        set sep $min_sep
        set tot_height [expr $inputs*$sep]
        $c create line $nx [expr $y-$tot_height/2] $nx [expr $y+$tot_height/2] \
            -tags $tag
        set lowy [expr $y-$tot_height/2+[inv_rad $c]]
    } else {
        set lowy [expr $y-([and_ht $c]/2.0-$sep/2.0)]
    }
    set xl [expr round($nx-2*[inv_rad $c])]
    set i 0
    set yl [expr round($lowy)]
    foreach inp_neg $inpnegl {
        if { $remap != {} } {
            set itag [format {%s____%d} $tag [lindex $remap $i]]
        } else {
            set itag [format {%s____%d} $tag [expr $i+1]]
        }
	if {$inp_neg != "" } {
	    $c create oval [expr round($nx-2*[inv_rad $c])] \
			   [expr round($yl-[inv_rad $c])] \
			   $nx [expr round($yl+[inv_rad $c])] \
			   -outline $gcolor -fill $fc -tags "$itag _WiRe_"
	    $c create line $xl $yl [expr ($nx-2*[inv_rad $c])] $yl \
		-fill $gcolor -tags "$itag _WiRe_"
	} else {
	    $c create line $xl $yl $nx $yl -fill $gcolor -tags "$itag _WiRe_"
	}
	lappend inp_locs [expr ($xl)] [expr round($yl)]
	set yl [expr round($yl+$sep)]
	incr i
    }
    return [list $inp_locs [list $x $y]]
}

proc draw_explicit_or_n {n c tag x y} {
    for {set i 0} {$i < $n} {incr i} { lappend inpnegs "" }
    return [draw_or_pat 0 $inpnegs "" "" $c $tag $x $y]
}

proc draw_explicit_nor_n {n c tag x y} {
    for {set i 0} {$i < $n} {incr i} { lappend inpnegs "" }
    return [draw_or_pat 0 $inpnegs "1" "" $c $tag $x $y]
}

proc draw_explicit_and_n {n c tag x y} {
    for {set i 0} {$i < $n} {incr i} { lappend inpnegs "" }
    return [draw_and_pat 0 $inpnegs "" $c $tag $x $y]
}

proc draw_explicit_nand_n {n c tag x y} {
    for {set i 0} {$i < $n} {incr i} { lappend inpnegs "" }
    return [draw_and_pat 0 $inpnegs "1" $c $tag $x $y]
}

proc draw_or_n {c tag x y} { return [draw_or_pat 0 {""} "" "" $c $tag $x $y] }
proc draw_nor_n {c tag x y} { return [draw_or_pat 0 {""} "Y" "" $c $tag $x $y] }
proc draw_and_n {c tag x y} { return [draw_and_pat 0 {""} "" $c $tag $x $y] }
proc draw_nand_n {c tag x y} { return [draw_and_pat 0 {""} "Y" $c $tag $x $y] }
proc draw_xor_n {c tag x y} { return [draw_or_pat 0 {""} "" "Y" $c $tag $x $y] }
proc draw_xnor_n {c tag x y} { return [draw_or_pat 0 {""} "Y" "Y" $c $tag $x $y] }

proc draw_or_pat {embedded inpnegl out_neg is_xor c tag x y {remap {}}} {
    global gcolor fc sfont
    if {$out_neg != ""} {
	set nx [expr ($x-[inv_rad $c]*2)]
	$c create oval [expr ($x-2*[inv_rad $c])] [expr ($y-[inv_rad $c])] \
		       $x [expr ($y+[inv_rad $c])] \
		       -outline $gcolor -fill $fc -tags $tag
    } else {
	set nx $x
    }
    $c create polygon   \
	[expr ($nx-[or_wid $c])] [expr ($y+[or_ht $c]/2)] \
	[expr ($nx-[or_wid $c])] [expr ($y+[or_ht $c]/2)] \
	[expr ($nx-5*[or_wid $c]/6)] $y \
	[expr ($nx-[or_wid $c])] [expr ($y-[or_ht $c]/2)] \
	[expr ($nx-[or_wid $c])] [expr ($y-[or_ht $c]/2)] \
	[expr ($nx-3*[or_wid $c]/4)] [expr ($y-[or_ht $c]/2)] \
	[expr ($nx-1*[or_wid $c]/3)] [expr ($y-6*([or_ht $c]/2)/7)] \
	$nx $y \
	$nx $y \
	[expr ($nx-1*[or_wid $c]/3)] [expr ($y+6*([or_ht $c]/2)/7)] \
	[expr ($nx-3*[or_wid $c]/4)] [expr ($y+[or_ht $c]/2)] \
	[expr ($nx-[or_wid $c])] [expr ($y+[or_ht $c]/2)] \
	[expr ($nx-[or_wid $c])] [expr ($y+[or_ht $c]/2)] \
	-smooth yes -outline $gcolor -fill $fc \
	-tags $tag
    if {$is_xor != ""} {
	set nx [expr ($nx-[xor_sep $c])]
	$c create line  \
	    [expr ($nx-[or_wid $c])] [expr ($y-[or_ht $c]/2)] \
	    [expr ($nx-[or_wid $c])] [expr ($y-[or_ht $c]/2)] \
	    [expr ($nx-5*[or_wid $c]/6)] $y \
	    [expr ($nx-[or_wid $c])] [expr ($y+[or_ht $c]/2)] \
	    [expr ($nx-[or_wid $c])] [expr ($y+[or_ht $c]/2)] \
	    -fill $gcolor -smooth yes
    }
    if { $embedded == 0 } { add_value_field $c $tag $x $y }
    set nx [expr ($nx-[or_wid $c])]
    set inputs [llength $inpnegl]
    set sep [expr ([or_ht $c]/$inputs.0)]
    set min_sep [expr 3*[inv_rad $c]]
    if [expr $sep < $min_sep] {
        set sep $min_sep
        set tot_height [expr $inputs*$sep]
        $c create line $nx [expr $y-$tot_height/2] $nx [expr $y-[or_ht $c]/2] \
            -tags $tag
        $c create line $nx [expr $y+$tot_height/2] $nx [expr $y+[or_ht $c]/2] \
            -tags $tag
        set lowy [expr $y-$tot_height/2+[inv_rad $c]]
    } else {
        set lowy [expr $y-([or_ht $c]/2.0-$sep/2.0)]
    }
    set xl [expr round($nx-2*[inv_rad $c])]
    set i 0
    set yl [expr round($lowy)]
    foreach inp_neg $inpnegl {
        if { $remap != {} } {
            set itag [format {%s____%d} $tag [lindex $remap $i]]
        } else { 
            set itag [format {%s____%d} $tag [expr $i+1]]
        }
	if [expr (2*[expr $i+1]) <= ($inputs+1)] {
	    set xx [expr ($nx+([or_wid $c]*[expr $i+1]/($inputs+1))/6)]
	} else {
	    set xx [expr \
                      ($nx+([or_wid $c]*($inputs-[expr $i+1]+1)/($inputs+1))/6)]
	}
	if {$inp_neg != "" } {
	    $c create oval [expr ($xx-2*[inv_rad $c])] \
			   [expr round($yl-[inv_rad $c])] \
			   $xx [expr round($yl+[inv_rad $c])] \
			   -outline $gcolor -fill $fc -tags "$itag _WiRe_"
	    $c create line $xl $yl [expr ($xx-2*[inv_rad $c])] $yl \
			-fill $gcolor -tags "$itag _WiRe_"
	} else {
	    $c create line $xl $yl $xx $yl -fill $gcolor -tags "$itag _WiRe_"
	}
	lappend inp_locs [expr ($xl)] [expr ($yl)]
	set yl [expr round($yl+$sep)]
	incr i
    }
    return [list $inp_locs [list $x $y]]
}

proc draw_array_read {c tag x y} {
    proc ar_read {c x y tag} {
	set f [$c create text $x $y -anchor center -justify center \
		-font $::sfont($c) -text {i2[i1]}]
	add_font_tags $c $f _IsTeXt_
    }
    return [draw_box_pat 2 {ar_read} "" $c $tag $x $y]
}

proc draw_unknown_gate {inputs c tag x y} {
    proc unk {c x y tag} {
	global gcolor mfont
	set f [$c create text $x $y -anchor center -justify center \
		-font $mfont($c) -text "?"]
	add_font_tags $c $f _IsTeXt_
	$c bind $f <Enter> "popup_excitation %W $tag %x %y"
	$c bind $f <Button-1> "post_excitation %W $tag; break"
	$c bind $f <Leave> "unpost_popup %W"
    }
    return [draw_box_pat $inputs {unk} "" $c $tag $x $y]
}

proc draw_complex_gate {type inputs c tag x y} {
    proc unk {type c x y tag} {
	global gcolor mfont
	set f [$c create text $x $y -anchor c -justify center \
		    -font $mfont($c) -text $type]
	add_font_tags $c $f _IsTeXt_
	$c bind $f <Enter> "popup_excitation %W $tag %x %y"
	$c bind $f <Button-1> "post_excitation %W $tag; break"
	$c bind $f <Leave> "unpost_popup %W"
    }
    return [draw_box_pat $inputs [list unk $type] "" $c $tag $x $y]
}

proc draw_ml_symbols {symbol c tag x y} {
    global gcolor fc sfont 
    set f [$c create image $x $y -anchor c -image $::icon($symbol)]
    val {x1 y1 x2 y2} [$c bbox $f]
    $c delete $f
    set w_name [expr round($x2-$x1)]
    set iwid [expr round([max $w_name [rect_wid $c]]+2*[min_sep $c])]
    set rad [expr round($iwid/2)]
    global gcolor mfont fc 
    add_value_field $c $tag $x $y
    set xl [expr $x-2*$rad]
    set xmid [expr round(($xl+$x)/2)]
    set yl [expr $y+$rad]
    set yt [expr $y-$rad]
    $c create oval  $xl $yl $x $yt -outline $gcolor -fill $fc -tags $tag
    $c create image [expr $x-$rad] $y -anchor c -image $::icon($symbol) \
	-tag $tag
    set xi [expr round($x-2.0*$rad)]
    set yw [expr round($yt-[min_sep $c])]
#    $c create line $xmid $yt $xmid $yw $xl $yw -fill $gcolor -width 3
#    lappend inp_locs $xi $yw
#    set yb [expr round($yl+[min_sep $c])]
#    $c create line $xmid $yl $xmid $yb $xl $yb -fill $gcolor -width 3
#    lappend inp_locs $xi $yb
    lappend inp_locs $xi $y
    return [list $inp_locs [list $x $y]]
}

proc draw_perceptron {c tag x y} {
    return [draw_ml_symbols perceptron $c $tag $x $y]
}

proc draw_checker {name nbr_inps c tag x y} {
    global gcolor fc sfont mfont
    #
    # Determine the width needed for text
    #
    set txtwid [expr [string length $name].0*12.0*$::sc($c)]
    if { [expr $txtwid > [rect_wid $c]] } {
	set wid $txtwid
    } else {
	set wid [rect_wid $c]
    }
    set rht [expr (($nbr_inps+1)*[rect_ht $c])]
    set lx [expr $x-$wid]
    set y_up [expr $y-4*$rht/10]
    set y_dn [expr $y+4*$rht/10]
    $c create rectangle $lx [expr ($y+$rht/2)] \
               $x  [expr ($y-$rht/2)] -outline $gcolor -fill $fc \
               -tags [list _Is_Hierarchical_ $tag]
   
    $c create rectangle $lx $y_up $x [expr ($y-$rht/2)] \
            -tags [list _Is_Hierarchical_ $tag] -fill orange
    $c create rectangle $lx $y_dn $x [expr ($y+$rht/2)] \
            -tags [list _Is_Hierarchical_ $tag] -fill orange
    #
    set xc [expr $x-$wid/2.0]
    set f [$c create text $xc $y -anchor c -justify center \
		-font $mfont($c) -text $name \
                -tags [list _Is_Hierarchical_ $tag]]
    add_font_tags $c $f _IsTeXt_
    #
    add_value_field $c $tag [expr $x+[letter_sz $c]] $y
    if { $nbr_inps > 0 } {
        set lowy [expr ($y-$rht/2+$rht/($nbr_inps*2))]
        set sep [expr ($rht/$nbr_inps)]
        for {set i 0} {$i < $nbr_inps} {incr i} {
            set yl [expr ($lowy+$i.0*$sep)]
            lappend inp_locs $lx $yl
        }
        return [list $inp_locs [list $x $y]]
    } else {
        return [list {} [list $x $y]]
    }
}

proc draw_same_clock {c tag x y} {
    return [draw_checker {i1==#i2} 2 $c $tag $x $y]
}

proc draw_phase_delay_on_output {pfn c tag x y} {
    set orig_x $x
    set orig_y $y
    set dres [draw_observation_delays 0 0 $c $tag $x $y]
    set x [lindex [lindex [lindex $dres 0] 0] 0]
    set res [eval $pfn $c $tag $x $y]
    set inp_locs [lindex $res 0]
    return [list $inp_locs [list $orig_x $orig_y]]
}

proc draw_hfl_code {inputs type c tag x y} {
    global gcolor fc mfont sfont
 
    set orig_x $x
    set orig_y $y
    set xt [expr $x-[min_txt_sep $c]]
    set f [$c create text $xt $y -anchor e -justify right \
		-font $mfont($c) -text $type -tags _Is_Hierarchical_ ]
    add_font_tags $c $f _IsTeXt_
    set bb [$c bbox $f]
    set x1 [expr round([lindex $bb 0])]
    set txtwid [expr $x-$x1+[min_txt_sep $c]]
    if { [expr $txtwid > [rect_wid $c]] } {
	set wid $txtwid
    } else {
	set wid [rect_wid $c]
	$c delete $f
	set xt [expr round($x-[rect_wid $c]/2)]
	set f [$c create text $xt $y -anchor c -justify center \
		    -font $mfont($c) -text $type]
	add_font_tags $c $f _IsTeXt_
    }
    set rht [expr ($inputs+1)*[rect_ht $c]]
    set txt_ht [expr round(abs([lindex $bb 3]-[lindex $bb 1]))]
    if [expr $txt_ht > $rht] {
	set rht $txt_ht
    }
    set g [$c create rectangle [expr $x-$wid] [expr round($y+$rht/2)] \
		$x [expr round($y-$rht/2)] -outline $gcolor -fill $fc \
		-tags [list _Is_Hierarchical_ $tag]]
    add_value_field $c $tag $x $y
    $c lower $g $f
    set nx [expr $x-$wid]
    if { $inputs ne 0 } {
	set lowy [expr round($y-$rht/2+$rht/($inputs*2))]
	set sep [expr round($rht/$inputs)]
	set xl [expr round($nx)]
	for {set i 0} {$i < $inputs} {incr i} {
	    set yl [expr round($lowy+$i*$sep)]
	    $c create line $xl $yl $nx $yl -fill $gcolor
	    lappend inp_locs $xl $yl
	}
    } else {
	set inp_locs {}
    }
    return [list $inp_locs [list $orig_x $orig_y]]
}

proc draw_observation_delays {min_cnt max_cnt c tag x y} {
    global gcolor mfont sfont
    global fc
    add_value_field $c $tag $x $y
    set ht [expr (2*[min_sep $c])]
    set y1 [expr $y-$ht]
    set y2 [expr $y+$ht]
    set x1 [expr $x-[rect_wid $c]]
    $c create rectangle  $x1 $y1 $x $y2 -outline $gcolor -fill $fc -tags $tag
    $c create polygon \
                [expr $x-7*[rect_wid $c]/8] [expr $y2-$ht/6] \
                [expr $x-1*[rect_wid $c]/2] [expr $y1+$ht/6] \
                [expr $x-1*[rect_wid $c]/8] [expr $y2-$ht/6] \
                -tags $tag \
                -outline $gcolor -fill yellow
    if { $max_cnt == $min_cnt } {
	if { $min_cnt == 0 } {
	    set txt ""
	} else {
	    set txt $min_cnt
	}
    } else {
        set txt [format {%d-%d} $min_cnt $max_cnt]
    }
    set t [$c create text [expr $x-1*[rect_wid $c]/2] [expr $y2] \
            -tags $tag -text $txt -anchor s -justify center -font $sfont($c)]
    add_font_tags $c $t _IsTeXt_
    set inp_locs {}
    lappend inp_locs $x1 $y
    return [list $inp_locs [list $x $y1]]
}

proc Delta {c x y tag} {
    global gcolor mfont fc
    set s [expr [rect_wid $c]/20]
    set f1 [$c create polygon \
		0 0 \
		[expr 4*$s] [expr -8*$s] \
		[expr 8*$s] 0 \
		-outline $gcolor -fill $fc ]
    $c move $f1 [expr $x-4*$s] [expr $y+4*$s]
}

proc draw_delay {c tag x y} { return [draw_box_pat 1 {Delta} {} $c $tag $x $y] }

proc draw_buffer {c tag x y} { return [draw_buf_pat "" "" $c $tag $x $y] }
proc draw_inverter {c tag x y} { return [draw_buf_pat "" "Y" $c $tag $x $y] }
proc draw_not {c tag x y} { return [draw_buf_pat "" "Y" $c $tag $x $y] }
proc draw_and2 {c tag x y} {return [draw_and_pat 0 {"" ""} "" $c $tag $x $y]}
proc draw_or2 {c tag x y} {return [draw_or_pat 0 {"" ""} "" "" $c $tag $x $y]}
proc draw_xor2 {c tag x y} {return [draw_or_pat 0 {"" ""} "" "Y" $c $tag $x $y]}
proc draw_xnor2 {c tag x y} {return [draw_or_pat 0 {"" ""} "Y" "Y" $c $tag $x $y]}
proc draw_nand2 {c tag x y} {return [draw_and_pat 0 {"" ""} "1" $c $tag $x $y]}
proc draw_nor2 {c tag x y} {return [draw_or_pat 0 {"" ""} "1" "" $c $tag $x $y]}


proc wr_lbl {txt c x y tag} {
    global mfont
    add_font_tags $c [$c create text [expr $x+1] $y -anchor w -text $txt \
			-font $mfont($c) -tags [list $tag _DoNotChangeColor_]] \
		    _IsTeXt_
}

proc Dlabel {c x y tag}	    { wr_lbl D $c $x $y $tag }
proc Elabel {c x y tag}	    { wr_lbl E $c $x $y $tag }
proc Rlabel {c x y tag}	    { wr_lbl R $c $x $y $tag }
proc Slabel {c x y tag}	    { wr_lbl S $c $x $y $tag }
proc ZeroLabel {c x y tag}  { wr_lbl 0 $c $x $y $tag }
proc OneLabel {c x y tag}   { wr_lbl 1 $c $x $y $tag }

proc MUXLabel {c x y tag} {
    global mfont
    add_font_tags $c [$c create text $x $y -anchor center -justify center \
			-text MUX -font $mfont($c) \
			-tags [list $tag _DoNotChangeColor_]] _IsTeXt_
}
proc ITELabel {c x y tag} {
    global mfont
    add_font_tags $c [$c create text $x $y -anchor center -justify center \
			    -text ITE -font $mfont($c) \
			    -tags [list $tag _DoNotChangeColor_]] _IsTeXt_
}
proc UnLabeled {x y tag} {
}

proc EdgeLabel {c x y tag} {
    global gcolor mfont
    set s [expr [rect_wid $c]/20]
    $c create line $x [expr $y-3*$s] [expr $x+4*$s] $y $x [expr $y+3*$s] \
	-fill $gcolor -tags [list $tag _DoNotChangeColor_]
}
proc ActHighWaveForm {c x y tag} {
    global gcolor
    set s [expr [rect_wid $c]/20]
    set f1 [$c create line 0 0 [expr 4*$s] 0 [expr 4*$s] \
		[expr -8*$s] [expr 8*$s] [expr -8*$s] [expr 8*$s] \
		0 [expr 12*$s] 0 -fill $gcolor \
		-tags [list $tag _DoNotChangeColor_]]
    $c move $f1 [expr $x-4*$s] [expr $y+4*$s]
}
proc ActLowWaveForm {c x y tag} {
    global gcolor
    set s [expr [rect_wid $c]/20]
    set f1 [$c create line 0 [expr -4*$s] [expr 4*$s] \
		[expr -4*$s] [expr 4*$s] [expr 4*$s] \
		[expr 8*$s] [expr 4*$s] [expr 8*$s] \
		[expr -4*$s] [expr 12*$s] [expr -4*$s] -fill $gcolor \
		-tags [list $tag _DoNotChangeColor_]]
    $c move $f1 [expr $x-4*$s] [expr $y+4*$s]
}
proc RisingEdge {c x y tag} {
    global gcolor mfont
    set s [expr [rect_wid $c]/20]
    set f1 [$c create line 0 0 [expr 4*$s] 0 [expr 4*$s] \
		[expr -8*$s] [expr 8*$s] [expr -8*$s] -fill $gcolor \
		-tags [list $tag _DoNotChangeColor_]]
    $c move $f1 [expr $x-4*$s] [expr $y+4*$s]
}
proc FallingEdge {c x y tag} {
    global gcolor
    set s [expr [rect_wid $c]/20]
    set f1 [$c create line 0 [expr -4*$s] [expr 4*$s] \
		[expr -4*$s] [expr 4*$s] [expr 4*$s] [expr 8*$s] \
		[expr 4*$s] -fill $gcolor \
		-tags [list $tag _DoNotChangeColor_]]
    $c move $f1 [expr $x-4*$s] [expr $y-1*$s]
}

proc re_order_inps {res new_order} {
    set inps [lindex $res 0]
    set out  [lindex $res 1]
    set new_inps {}
    foreach idx $new_order {
	lappend new_inps [lindex $inps [expr 2*($idx-1)]]
	lappend new_inps [lindex $inps [expr 2*($idx-1)+1]]
    }
    return [list $new_inps $out]
}
 
proc draw_ite {c tag x y} {
    return [re_order_inps [draw_ITE $c $tag $x $y] {3 1 2}]
}


proc draw_mux3 {c tag x y} {
    return [draw_box_pat 6 {MUXLabel} {} $c $tag $x $y]
}

proc draw_mux4 {c tag x y} {
    return [draw_box_pat 8 {MUXLabel} {} $c $tag $x $y]
}

proc draw_mux5 {c tag x y} {
    return [draw_box_pat 10 {MUXLabel} {} $c $tag $x $y]
}

proc draw_pos_d_latchen {c tag x y} {
    return [draw_box_pat 2 {ActHighWaveForm} {Dlabel EdgeLabel} $c $tag $x $y]
}

proc draw_pos_d_latch {c tag x y} {
    return [draw_box_pat 2 {ActHighWaveForm} {Dlabel EdgeLabel} $c $tag $x $y]
}

proc draw_pos_d_latch_reset {c tag x y} {
    return [draw_box_pat 3 {ActHighWaveForm} {Dlabel Rlabel EdgeLabel} $c $tag $x $y]
}

proc draw_pos_d_latchbig {c tag x y} {
    return [draw_box_pat 2 {ActHighWaveForm} {Dlabel EdgeLabel} $c $tag $x $y]
}

proc draw_neg_d_latch {c tag x y} {
    return [draw_box_pat 2 {ActLowWaveForm} {Dlabel EdgeLabel} $c $tag $x $y]
}

proc draw_master_slave {c tag x y} {
    return [draw_box_pat 2 {RisingEdge} {Dlabel EdgeLabel} $c $tag $x $y]
}

proc draw_ff_re_with_en {c tag x y} {
    return [draw_box_pat 3 {RisingEdge} {Dlabel Elabel EdgeLabel} $c $tag $x $y]
}

proc draw_ff_fe_with_en {c tag x y} {
    return [draw_box_pat 3 {FallingEdge} {Dlabel Elabel EdgeLabel} $c $tag $x $y]
}

proc draw_ff_fe {c tag x y} {
    return [draw_box_pat 2 {FallingEdge} {Dlabel EdgeLabel} $c $tag $x $y]
}

proc draw_ff_re {c tag x y} {
    return [draw_box_pat 2 {RisingEdge} {Dlabel EdgeLabel} $c $tag $x $y]
}

proc draw_dff {c tag x y} {
    return [draw_box_pat 2 {RisingEdge} {Dlabel EdgeLabel} $c $tag $x $y]
}

proc draw_adff {c tag x y} {
    return [draw_box_pat 3 {RisingEdge} {Dlabel EdgeLabel Rlabel} $c $tag $x $y]
}

proc draw_ff_re_with_en_reset {c tag x y} {
    return [draw_box_pat 4 {RisingEdge} {Dlabel Elabel Rlabel EdgeLabel} $c $tag $x $y]
}

proc draw_ff_fe_with_en_reset {c tag x y} {
    return [draw_box_pat 4 {FallingEdge} {Dlabel Elabel Rlabel EdgeLabel} $c $tag $x $y]
}

proc draw_ff_re_with_en_set {c tag x y} {
    return [draw_box_pat 4 {RisingEdge} {Dlabel Elabel Slabel EdgeLabel} $c $tag $x $y]
}

proc draw_ff_fe_with_en_set {c tag x y} {
    return [draw_box_pat 4 {FallingEdge} {Dlabel Elabel Slabel EdgeLabel} $c $tag $x $y]
}

proc draw_ff_re_with_en_reset_set {c tag x y} {
    return [draw_box_pat 5 {RisingEdge} {Dlabel Elabel Rlabel Slabel EdgeLabel} $c $tag $x $y]
}

proc draw_ff_fe_with_en_reset_set {c tag x y} {
    return [draw_box_pat 5 {FallingEdge} {Dlabel Elabel Rlabel Slabel EdgeLabel} $c $tag $x $y]
}

proc draw_ff_re_reset {c tag x y} {
    return [draw_box_pat 3 {RisingEdge} {Dlabel Rlabel EdgeLabel} $c $tag $x $y]
}

proc draw_ff_re_reset2 {c tag x y} {
    return [draw_box_pat 3 {RisingEdge} {Dlabel Rlabel EdgeLabel} $c $tag $x $y]
}

proc draw_ff_fe_reset {c tag x y} {
    return [draw_box_pat 3 {FallingEdge} {Dlabel Rlabel EdgeLabel} $c $tag $x $y]
}

proc draw_ff_re_set {c tag x y} {
    return [draw_box_pat 3 {RisingEdge} {Dlabel Slabel EdgeLabel} $c $tag $x $y]
}
proc draw_ff_fe_set {c tag x y} {
    return [draw_box_pat 3 {FallingEdge} {Dlabel Slabel EdgeLabel} $c $tag $x $y]
}

proc draw_ff_re_reset_set {c tag x y} {
    return [draw_box_pat 4 {RisingEdge} {Dlabel Slabel Rlabel EdgeLabel} $c $tag $x $y]
}
proc draw_ff_fe_reset_set {c tag x y} {
    return [draw_box_pat 4 {FallingEdge} {Dlabel Slabel Rlabel EdgeLabel} $c $tag $x $y]
}

###########################################################################
## Utilities
###########################################################################

proc vis_toplevel {w {width {}} {height {}} {title ""}} {
    toplevel $w
    set lx [winfo rootx $::ste_debugger_notebook]
    set ly [winfo rooty $::ste_debugger_notebook]
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

proc fsm:make_state_hierarchical {c state} {
    foreach st $::fsm_machine_info($c,states) {
	val {i_name r_name} $st
	if { $r_name == $state } {
	    $c itemconfigure $::fsm_node2circle_tag($c,$i_name) -dash {2 6}
	}
    }
}

proc fsm:show_current_state {c root_window vec} {
    set svs [split [fl_get_short_value $root_window [list $vec] 1] "/"]
    # Reset colors
    foreach st $::fsm_machine_info($c,states) {
	val {i_name r_name} $st
	$c itemconfigure $::fsm_node2circle_tag($c,$i_name) -fill white
    }
    if { [llength $svs] > 1 } {
	set col LightGreen
    } else {
	set col green
    }
    foreach sv $svs {
	foreach st $::fsm_machine_info($c,states) {
	    val {i_name r_name} $st
	    if { $r_name == $sv } {
		$c itemconfigure $::fsm_node2circle_tag($c,$i_name) -fill $col
	    }
	}
    }
}

proc fsm:display_fsm {sch_c aname fsm_name dot_pgm states edges inps} {
    incr ::dot_displays
    set w .dot$::dot_displays
    catch {destroy $w}
    toplevel $w
    wm title $w $fsm_name
    set c $w.c

    # Record relevant information
    set ::fsm_machine_info($c,states) $states
    set ::fsm_machine_info($c,edges) $edges

    set ::sc($c) $::base_sc
    set ::mfont($c) $::base_bfont
    scrollbar $w.yscroll -command "$w.c yview"
    scrollbar $w.xscroll -orient horizontal -command "$w.c xview"
    canvas $w.c -background white \
            -yscrollcommand "$w.yscroll set" \
            -xscrollcommand "$w.xscroll set"
    pack $w.yscroll -side right -fill y
    pack $w.xscroll -side bottom -fill x
    pack $w.c -side top -fill both -expand yes

    # Now call dot and evaluate the resulting tcl code
    set fp [open "|dot -Ttk $dot_pgm" "r"]
    while {[gets $fp line] >= 0} {
        eval $line
    }
    close $fp
    update idletasks

    bind $c <2> "%W scan mark %x %y"
    bind $c <B2-Motion> "%W scan dragto %x %y"

    # Zoom bindings
    bind $c <ButtonPress-3> "zoom_lock %W %x %y"
    bind $c <B3-Motion> "zoom_move %W %x %y"
    bind $c <ButtonRelease-3> "zoom_execute %W %x %y %X %Y {}"

    # Mouse-wheel bindings for zooming in/out
    bind $c <Button-4> "zoom_out $c 1.1 %x %y"
    bind $c <Button-5> "zoom_out $c [expr {1.0/1.1}] %x %y"

    set ::cur_zoom_factor($w) 100.0
    set ::cur_zoom_factor($c) 100.0
    set ::sc($c) $::base_sc
    set ::mfont($c) $::base_mfont
    set ::sfont($c) $::base_sfont

    ;# Create maps from node/edge name to tag of drawn symbol
    foreach t [$c find all] {
	if ![catch {$c itemcget $t -text} txt] {
	    set tag [$c gettags $t]
	    if [regexp "0node.*" $tag] {
		set rtag "1[string range $tag 1 end]"
		set ::fsm_node2circle_tag($c,$txt) $rtag
		set ::fsm_node2text_tag($c,$txt) $tag
	    } elseif [regexp "0edge.*" $tag] {
		set rtag "1[string range $tag 1 end]"
		lappend ::edge2line_tag($c,$txt) $rtag
		lappend ::edge2text_tag($c,$txt) $tag
	    }
	}
    }

    val {lx ly ux uy} [$w.c bbox all]
    set cnt 1
    set cy [expr $uy + [min_sep $c]]
    set rx $lx
    foreach st $states {
	val {i_name r_name} $st
	set t [$c create text $lx $cy -anchor w -justify left \
		-font $::mfont($c) -text "$i_name = $r_name"]
	val {tlx tly tux tuy} [$w.c bbox $t]
	if { $tux > $rx } { set rx $tux }
	set cy [expr $cy + 2*[min_sep $c]]
	$c bind $::fsm_node2text_tag($c,$i_name) <Enter> \
	    [list post_popup %W $r_name %x %y]
	$c bind $::fsm_node2text_tag($c,$i_name) <Leave> { unpost_popup %W }
	$c bind $::fsm_node2circle_tag($c,$i_name) <Enter> \
	    [list post_popup %W $r_name %x %y]
	$c bind $::fsm_node2circle_tag($c,$i_name) <Leave> { unpost_popup %W }
    }
    foreach edge $edges {
	val {i_name f_name} $edge
	    foreach ltag $::edge2line_tag($c,$i_name) {
		$c bind $ltag <Enter> [list post_popup %W $f_name %x %y]
		$c bind $ltag <Leave> { unpost_popup %W }
	    }
	    foreach ttag $::edge2text_tag($c,$i_name) {
		$c bind $ttag <Enter> [list post_popup %W $f_name %x %y]
		$c bind $ttag <Leave> { unpost_popup %W }
	    }
    }
    set cy [expr $uy + [min_sep $c]]
    set lx [expr $rx + 4*[min_sep $c]]
    set cnt 1
    foreach inp $inps {
	set txt [format {i%d = %s} $cnt $inp]
	$c create text $lx $cy -anchor w -justify left \
		-font $::mfont($c) -text $txt
	incr cnt
	set cy [expr $cy + 2*[min_sep $c]]
    }
    val {lx ly ux uy} [$w.c bbox all]
    set wid [expr $ux-$lx]
    if [expr $wid > 800] { set wid 800 }
    set ht [expr $uy-$ly]
    if [expr $ht > 800] { set ht 800 }
    $c configure -width $wid -height $ht
    $w.c configure -scrollregion [$w.c bbox all]
    bind $w.c <KeyPress-q> "destroy $w"

    set root [w2root $sch_c]
    fl_add_fsm $root $c [fl_tag2vec $sch_c $aname]
    bind $c <Destroy> [list fl_remove_fsm $root $c]

    focus $w.c
}


proc mk_fsm_circle {c x y tag dotfile rad} {
    global gcolor fc
    set f [$c create oval [expr $x-$rad] [expr $y+$rad] \
		          [expr $x+$rad] [expr $y-$rad] \
			  -tags [list _Is_Hierarchical_ $tag $dotfile] \
			  -outline $gcolor -fill $fc]
}

proc draw_fsm {name dotfile states edges inps c tag x y} {
    global gcolor fc mfont
    $c bind $dotfile <ButtonPress-1> \
		  "fsm:display_fsm $c $tag $name $dotfile [list $states] \
			       [list $edges] [list $inps]"
    set inputs [llength $inps]
    set orig_x $x
    set orig_y $y
    set rht [expr round(2*($inputs+1)*[min_sep $c])]
    if { $rht < [fsm_ht $c] } {
	set rht [fsm_ht $c]
    }
    set xt [expr $x-[min_txt_sep $c]]
    set yt [expr round($y-$rht/2)+2*[min_sep $c]]
    set f [$c create text $xt $yt -anchor e -justify right \
		-font $mfont($c) -text $name -tags "$dotfile _Is_Hierarchical_"]
    add_font_tags $c $f _IsTeXt_
    set x1 [expr round([lindex [$c bbox $f] 0])]
    set txtwid [expr $x-$x1+[min_txt_sep $c]]
    if { [expr $txtwid > [fsm_wid $c]] } {
	set wid $txtwid
    } else {
	set wid [fsm_wid $c]
	$c delete $f
	set xt [expr round($x-[fsm_wid $c]/2)]
	set f [$c create text $xt $yt -anchor c -justify center \
		    -font $mfont($c) -text $name -tags $dotfile]
	add_font_tags $c $f _IsTeXt_
    }
    set dim [min $wid $rht]
    set x0 [expr round($x-$wid/2)]
    set y0 [expr $y+$dim/10]
    set sep [expr round($dim/5)]
    set rad [expr round($dim/15)]
    set x1 [expr round($x0-$sep)]
    set y1 $y0
    set x2 $x0
    set y2 [expr round($y0-$sep)]
    set x3 [expr round($x0+$sep)]
    set y3 $y0
    set x4 $x0
    set y4 [expr round($y0+$sep)]
    mk_fsm_circle $c $x1 $y1 $tag $dotfile $rad
    mk_fsm_circle $c $x2 $y2 $tag $dotfile $rad
    mk_fsm_circle $c $x3 $y3 $tag $dotfile $rad
    mk_fsm_circle $c $x4 $y4 $tag $dotfile $rad
    # Reset
    $c create line [expr $x1-$sep] $y1 \
		   [expr $x1-$rad] $y1 \
		   -arrow last \
		   -tags [list _Is_Hierarchical_ $tag $dotfile]] \
		   -fill $gcolor
    # 1->2
    $c create line [expr round($x1+$rad/1.4142)] \
		   [expr round($y1-$rad/1.4142)] \
		   [expr round($x2-$rad/1.4142)] \
		   [expr round($y2+$rad/1.4142)] \
		   -arrow last \
		   -tags [list _Is_Hierarchical_ $tag $dotfile]] \
		   -fill $gcolor
    # 4->1
    $c create line [expr round($x4-$rad/1.4142)] \
		   [expr round($y4-$rad/1.4142)] \
		   [expr round($x1+$rad/1.4142)] \
		   [expr round($y1+$rad/1.4142)] \
		   -arrow last \
		   -tags [list _Is_Hierarchical_ $tag $dotfile]] \
		   -fill $gcolor
    # 2->3
    $c create line [expr round($x2+$rad/1.4142)] \
		   [expr round($y2+$rad/1.4142)] \
		   [expr round($x3-$rad/1.4142)] \
		   [expr round($y3-$rad/1.4142)] \
		   -arrow last \
		   -tags [list _Is_Hierarchical_ $tag $dotfile]] \
		   -fill $gcolor
    # 2->4
    $c create line $x2 [expr $y2+$rad] \
		   $x4 [expr $y4-$rad] \
		   -arrow last \
		   -tags [list _Is_Hierarchical_ $tag $dotfile]] \
		   -fill $gcolor
    #
    set g [$c create rectangle [expr $x-$wid] [expr round($y+$rht/2)] \
		$x [expr round($y-$rht/2)] -outline $gcolor -fill $fc \
		-tags [list _Is_Hierarchical_ $tag $dotfile]]
    add_value_field $c $tag $x $y "_FsM_OuTpUt_"
    foreach st $states {
	val {i_name r_name} $st
	lappend ::fsm_output_info($c,$tag) $r_name
    }
    $c lower $g $f
    set nx [expr $x-$wid]
    if { $inputs ne 0 } {
	set lowy [expr round($y-$rht/2+$rht/($inputs*2))]
	set sep [expr round($rht/$inputs)]
	set xl [expr round($nx)]
	for {set i 0} {$i < $inputs} {incr i} {
	    set yl [expr round($lowy+$i*$sep)]
	    $c create line $xl $yl $nx $yl -fill $gcolor
	    lappend inp_locs $xl $yl
	}
    } else {
	set inp_locs {}
    }
    return [list $inp_locs [list $orig_x $orig_y]]
}
