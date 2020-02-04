;#########################################################################
;# Copyright 2020 Carl-Johan Seger
;# SPDX-License-Identifier: Apache-2.0
;#########################################################################

set ::wv_phase_width	    15
set ::wv_height		    20
set ::wv_sep		    5

image create bitmap ::bitmap::zoom_out -data "#define zoom_out_width 16
#define zoom_out_height 16
static unsigned char _expand_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x08, 0x08, 0x10,
   0x04, 0x20, 0x02, 0x40, 0xff, 0xff, 0x02, 0x40, 0x04, 0x20, 0x08, 0x10,
   0x10, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};"

image create bitmap ::bitmap::zoom_in -data "#define zoom_in_width 16
#define zoom_in_height 16
static unsigned char zoom_in_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x20, 0x08, 0x10,
   0x10, 0x08, 0x20, 0x04, 0x7f, 0xfe, 0x20, 0x04, 0x10, 0x08, 0x08, 0x10,
   0x04, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};"

proc wv:view {wins args} { foreach w $wins { eval $w $args } }

proc wv:create_waveform_viewer {w maxtime} {
    set f $w.f
    catch {destroy $f}

    set ::wv_xscale($f)	$::wv_phase_width
    if [expr $maxtime < 1] {
	set maxtime 1
    }
    set ::wv_info($f,maxtime) $maxtime
    set ::wv_info($f,vectors) {}

    set pw $f.panes
    set nf $pw.name_frame
    set nl $nf.title
    set nn $nf.names
    set nx $nf.xscroll
    set sb $nf.yscroll
    set wf $pw.waveform_frame
    set wt $wf.time_scale
    set ww $wf.waveforms
    set wx $wf.xscroll

    frame $f

    set ::vstatus(inside_notebook,$f) 1

    # Menus
    frame $f.menu -relief raised -bd 1
    pack $f.menu -side top -fill x
    set nb [winfo parent $w]
	ttk::button $f.menu.detach -image $::icon(detach) \
			-command "wv:detach $nb $f"
	pack $f.menu.detach -side left

	set menu $f.menu
	set cbc $menu.show_bdd
	set tf $menu.time
	set zi $tf.zoom_in
	set zt $tf.zoom
	set zo $tf.zoom_out

	set cbc $tf.show_dep
	set mb $tf.minus
	set et $tf.time
	set pb $tf.plus

	set root [w2root $w]
	frame $tf -relief flat -height 10
	pack $tf -side right
	    button $zi -image ::bitmap::zoom_in -command "wv:zoom_in $w"
	    pack $zi -side left
	    label $zt -text Zoom
	    pack $zt -side left
	    button $zo -image ::bitmap::zoom_out -command "wv:zoom_out $w"
	    pack $zo -side left

	    checkbutton $cbc -text "  Show dependencies:" \
			-variable ::wv_info($f,show_depend)  \
			-command [list wv:toggle_show_depend $w]
	    pack $cbc -side left

	    button $mb -text "-" -command "incr ::vstatus(time,[w2root $w]) -1"
	    pack $mb -side left
	    entry $et -width 5 \
		-background black -foreground red -justify center \
		-textvariable ::vstatus(time,$root) \
		-validate all \
		-validatecommand {string is integer %P}
	    pack $et -side left
	    button $pb -text "+" -command "incr ::vstatus(time,[w2root $w]) +1"
	    pack $pb -side left

    # Actual waveform viewer windows
	ttk::panedwindow $pw -orient horizontal 
	frame $nf -relief flat
	    canvas $nl -height 30 -background white
	    $nl create text 20 15 -text "Vector:" -anchor w -justify left
	    pack $nl -side top -fill x
	    scrollbar $sb -orient vertical \
		    -command "wv:view [list [list $nn $ww]] yview"
	    pack $sb -side left -fill y
	    scrollbar $nx -orient horizontal -command "$nn xview"
	    pack $nx -side bottom -fill x
	    canvas $nn -background white -yscrollcommand [list $sb set] \
					     -xscrollcommand [list $nx set]
	    pack $nn -side top -fill both -expand yes

	    bind $nn <Enter> { wv:enter_name_window %W %x %y %X %Y }
	    bind $nn <Leave> { wv:leave_name_window %W %x %y %X %Y }
	    bind $nn <Motion>  { wv:move_name_window %W %x %y %X %Y }

	    $nn bind all <ButtonPress-3> {wv:mk_name_list_menu %W %x %y %X %Y}

	    bind $nn <ButtonPress-1> "selection_lock %W %x %y 0"
	    bind $nn <B1-Motion> "selection_move %W %x %y 0"
	    bind $nn <ButtonRelease-1>  "wv:selection_execute %W %x %y %X %Y 0"
	    bind $nn <Shift-ButtonPress-1> "selection_lock %W %x %y 1"
	    bind $nn <Shift-B1-Motion> "selection_move %W %x %y 1"
	    bind $nn <Shift-ButtonRelease-1> \
			"wv:selection_execute %W %x %y %X %Y 1"
	    bind $nn 1 "wv:set_sel_to_col %W DarkOrchid1"
	    bind $nn 2 "wv:set_sel_to_col %W magenta2"
	    bind $nn 3 "wv:set_sel_to_col %W DarkOrange1"
	    bind $nn 4 "wv:set_sel_to_col %W green"
	    bind $nn 5 "wv:set_sel_to_col %W gold3"
	    bind $nn 6 "wv:set_sel_to_col %W yellow"
	    bind $nn 7 "wv:set_sel_to_col %W cyan"
	    bind $nn 8 "wv:set_sel_to_col %W purple"
	    bind $nn 9 "wv:set_sel_to_col %W brown"
	    bind $nn 0 "wv:set_sel_to_col %W _OrIgInAlCoLoR_"
	    bind $nn <Delete> { wv:keyboard_delete_waveform %W %x %y %X %Y }
	    bind $nn <KeyPress-Up>   \
		{ wv:keyboard_move_waveform %W %x %y %X %Y -1 }
	    bind $nn <KeyPress-Down> \
		{ wv:keyboard_move_waveform %W %x %y %X %Y 1 }

	frame $wf -relief flat
	    canvas $wt -background white -height 30 \
		    -xscrollcommand [list $wx set]
	    wv:draw_time_line $f $::wv_info($f,maxtime)
	    bind $wt <ButtonPress-1> "wv:set_time_explicitly %W %x %y"

	    pack $wt -side top -fill x
	    scrollbar $wx -orient horizontal \
		-command "wv:view [list [list $wt $ww]] xview"
	    pack $wx -side bottom -fill x
	    canvas $ww -background white -yscrollcommand [list $sb set] \
					 -xscrollcommand [list $wx set] \
					-cursor center_ptr
	    pack $ww -side top -fill both -expand yes
	    bind $ww <Enter> { wv:enter_wv_window %W %x %y %X %Y }
	    bind $ww <Leave> { wv:leave_wv_window %W %x %y %X %Y }
	    bind $ww <Shift-KeyPress-Right>  \
		    { incr ::vstatus(time,[w2root %W]) +2 }
	    bind $ww <Shift-KeyPress-Left>   \
		    { incr ::vstatus(time,[w2root %W]) -2 }
	    bind $ww <KeyPress-Right>  { incr ::vstatus(time,[w2root %W]) +1 }
	    bind $ww <KeyPress-Left>   { incr ::vstatus(time,[w2root %W]) -1 }


	$pw add $nf
	$pw add $wf
	pack $pw -side right -fill both -expand yes
    pack $f -side left -fill both -expand yes



    $ww config -scrollregion [$wt bbox all]
    $wt config -scrollregion [$wt bbox all]
    $pw sashpos 0 50

    bind $nn <Button-4>	"$nn yview scroll -1 units; $ww yview scroll -1 units"
    bind $nn <Button-5>	"$nn yview scroll 1 units; $ww yview scroll 1 units"
    bind $ww <Button-4>	"$nn yview scroll -1 units; $ww yview scroll -1 units"
    bind $ww <Button-5>	"$nn yview scroll 1 units; $ww yview scroll 1 units"

    wv:set_time_pointer $w {}

    trace add variable ::vstatus(time,$root) write \
	    "after idle [list wv:set_time_pointer $w]"

    return $f
}

proc wv:set_time_explicitly {w x y} {
    set root [w2root $w]
    set f $root.nb.waveform.f
    set time [expr int([$w canvasx $x]/$::wv_xscale($f))]
    set ::vstatus(time,[w2root $w]) $time
}

proc wv:selection_execute {w wx wy sx sy shift} {
    global selection_anchor_point
    if ![info exists selection_anchor_point($w,orig_x)] { return }
    if ![info exists selection_anchor_point($w,orig_y)] { return }
    set x [$w canvasx $wx]
    set y [$w canvasy $wy]
    catch {$w delete $selection_anchor_point($w,fig)}
    catch {$w delete $selection_anchor_point($w,txt)}
    set ox $selection_anchor_point($w,orig_x)
    set oy $selection_anchor_point($w,orig_y)
    set deltax [expr $x - $ox]
    set deltay [expr $y - $oy]
    if [expr abs($deltax) <= 2.0 && abs($deltay) <= 2.0] { 
        # Was it a cancelled area selection operation?
        if {$selection_anchor_point($w,selection_movement) == 1} { return }
	set vecs [wv:tags2vecs [$w gettags current]]
    } else {
	set vecs [wv:tags2vecs [$w find overlap [min $x $ox] [min $y $oy] \
					        [max $x $ox] [max $y $oy]]]
    } 
    if $shift { 
        fl_set_selection $w "MODIFY_SELECTION" [list $vecs]
    } else {
        fl_set_selection $w "SET_SELECTION" [list $vecs]
    }
}

proc wv:detach {nb f} {
    if $::vstatus(inside_notebook,$f) {
        wm manage $f
        $nb forget 1
        set ::vstatus(inside_notebook,$f) 0
    } else {
        wm forget $f
	if [expr [llength [$nb tabs]] <= 1] {
	    $nb add [winfo parent $f] -text "Waveforms"
	} else {
	    $nb insert 1 [winfo parent $f] -text "Waveforms"
	}
        set ::vstatus(inside_notebook,$f) 1
        pack configure $f -fill both -anchor w -expand yes
    }   
}


proc wv:vtag2name {tags} {
    set res {}
    foreach tag $tags {
        if [regexp {^VtAg_[0-9][0-9][0-9][0-9][0-9][0-9]$} $tag] {
	    lappend res $tag
	}
    }
    if [info exists ::wv_info(vtag2name,$res)] {
	return $::wv_info(vtag2name,$res)
    } else {
	return ""
    }
}

proc wv:ww2f {ww} {
    return [winfo parent [winfo parent [winfo parent $ww]]]
}

proc wv:post_extended_value {w vec t x y} {
    post_big_popup_window [fl_wv_get_complete_value $w $vec $t] \
		      [format {Value for %s at time %d} $vec $t]
}

proc wv:add_value_menu {w vec t x y} {
    set m $w.sm
    catch {destroy $m}
    menu $m -tearoff 0
    $m add command -label "Show expanded value" \
        -command "wv:post_extended_value $w $vec $t $x $y"
    tk_popup $m $x $y
}

proc wv:add_value_popup {ww obj} {
    $ww bind $obj <ButtonPress-3> {
	set f [wv:ww2f %W]
	set w [winfo parent [winfo parent [winfo parent $f]]]
	set time [expr int([%W canvasx %x]/$::wv_xscale($f))]
	set tags [%W gettags current]
	set vec [wv:vtag2name $tags]
	if { $vec != "" } { wv:add_value_menu $w [list $vec] $time %X %Y }
	break
    }
    $ww bind $obj <Enter> {
	set f [wv:ww2f %W]
	set w [winfo parent [winfo parent [winfo parent $f]]]
	set time [expr int([%W canvasx %x]/$::wv_xscale($f))]
	set tags [%W gettags current]
	set vec [wv:vtag2name $tags]
	if { $vec != "" } {
	    set sv [fl_wv_get_short $w $vec $time]
	    post_popup %W $sv %x %y
	}
    }
    $ww bind $obj <Leave> {
	unpost_popup %W
    }
    $ww bind $obj <Motion> {
	unpost_popup %W
	set f [wv:ww2f %W]
	set w [winfo parent [winfo parent [winfo parent $f]]]
	set time [expr int([%W canvasx %x]/$::wv_xscale($f))]
	set tags [%W gettags current]
	set vec [wv:vtag2name $tags]
	if { $vec != "" } {
	    set sv [fl_wv_get_short $w $vec $time]
	    post_popup %W $sv %x %y
	}
    }
}

proc wv:set_time_pointer {w args} {
    set f $w.f
    set pw $f.panes
    set wf $pw.waveform_frame
    set wt $wf.time_scale
    set ww $wf.waveforms

    set t $::vstatus(time,[w2root $w])
    if { $t == "" } { set t 0 }
    set x1 [expr $t * $::wv_xscale($f)]
    set x2 [expr $x1 + $::wv_xscale($f)]
    set ytop 0
    set ybot 30
    $wt delete withtag _TiMwPoInT_
    $wt create rectangle $x1 $ybot $x2 $ytop -tag _TiMwPoInT_ \
		-fill "light green"
    $wt lower _TiMwPoInT_

    $ww delete withtag _TiMwPoInT_
    set bb [$ww bbox all]
    if { $bb == "" } { return; }
    val {lx ly ux uy} [$ww bbox all]
    $ww create rectangle $x1 $ly $x2 $uy -tag _TiMwPoInT_ -fill "light green"
    $ww lower _TiMwPoInT_
}

proc wv:draw_time_line {f maxtime} {
    set pw $f.panes
    set wf $pw.waveform_frame
    set wt $wf.time_scale
    $wt create line 0 20 [expr $::wv_xscale($f)*$maxtime] 20 -arrow last
    for {set t 0} {$t <= $maxtime} {incr t} {
	set x [expr $t*$::wv_xscale($f)]
	if [expr ($t % 10) == 0] {
	    $wt create line $x 10 $x 30
	    $wt create text $x 17 -text $t -anchor sw -justify left
	} elseif [expr ($t % 5) == 0] {
	    $wt create line $x 15 $x 25
	} else {
	    $wt create line $x 18 $x 22
	}
    }

}

proc wv:add_waveform {w vecs} {
    foreach vec $vecs {
	wv:prim_add_waveform $w $vec
    }
    wv:set_time_pointer $w {}
}

proc wv:restore_original_colors {w} {
    set f $w.f
    set pw $f.panes
    set nf $pw.name_frame
    set nn $nf.names

    if [info exists ::changed_colors($nn)] {
        foreach item $::changed_colors($nn) {
            set dcol [get_stored_default_color $nn $item]
            $nn itemconfigure $item -fill $dcol
        }
        unset ::changed_colors($nn)
    }
}


proc wv:prim_set_name_color {w vec color} {
    set f $w.f
    set pw $f.panes
    set nf $pw.name_frame
    set nn $nf.names

    foreach item [$nn find withtag BaCkGrOuNd$vec] {
	if { $color == "_OrIgInAlCoLoR_" } {
	    set dcol [get_stored_default_color $nn $item]
	    $nn itemconfigure $item -fill $dcol
	} else {
	    record_orig_color $nn $item
	    $nn itemconfigure $item -fill $color
	    lappend ::changed_colors($nn) $item
	}
    }
}

proc wv:is_real_tag {tag} {
    if [regexp {BaCkGrOuNd.*} $tag] { return 0; }
    if { $tag == "current" } { return 0; }
    if { $tag == "_ArRoW_" } { return 0; }
    if { $tag == "TxTxT" } { return 0; }
    return 1
}

proc wv:tags2vecs {tags} {
    set vecs {}
    foreach tag $tags {
	if [wv:is_real_tag $tag] {
	    lappend vecs $tag
	}
    }
    return $vecs
}

proc wv:delete_waveform {nn idx} {
    set f [winfo parent [winfo parent [winfo parent $nn]]]
    set pw $f.panes
    set wf $pw.waveform_frame
    set ww $wf.waveforms
    set nf $pw.name_frame
    set nn $nf.names

    if ![info exists ::wv_info($f,vectors)] { return 0 }
    set max [expr [llength $::wv_info($f,vectors)]-1]
    if [expr $idx > $max] { return }

    # Remove the name
    set vecs $::wv_info($f,vectors)
    unset ::wv_info($f,vectors)
    for {set i 0} { $i < $idx } {incr i} {
	lappend ::wv_info($f,vectors) [lindex $vecs $i]
    }
    for {set i [expr $idx+1]} {$i <= $max} {incr i} {
	lappend ::wv_info($f,vectors) [lindex $vecs $i]
    }
    # 
    val {lx ly ux uy} [$nn bbox all]
    set ytop [expr $idx*($::wv_height + $::wv_sep)+$::wv_sep]
    set ybot [expr $ytop + $::wv_height]
    set items [$nn find overlapping $lx $ytop $ux $ybot]
    set arrow_idx [$nn find withtag _ArRoW_]
    foreach item $items {
	if { $item != $arrow_idx } {
	    $nn delete $item
	}
    }
    set deltay [expr -1*($::wv_height + $::wv_sep)]
    foreach item [$nn find overlapping $lx $ybot $ux $uy] {
	$nn move $item 0 $deltay
    }

    # Delete the waveform
    val {lx ly ux uy} [$ww bbox all]
    set ytop [expr $idx*($::wv_height + $::wv_sep)+$::wv_sep]
    set ybot [expr $ytop + $::wv_height]
    set items [$ww find overlapping $lx $ytop $ux $ybot]
    foreach item $items { $ww delete $item }
    foreach item [$ww find overlapping $lx $ybot $ux $uy] {
	$ww move $item 0 $deltay
    }
    wv:set_time_pointer [winfo parent $f] {}
}

proc wv:move_waveform {nn idx dir} {
    set f [winfo parent [winfo parent [winfo parent $nn]]]
    set pw $f.panes
    set wf $pw.waveform_frame
    set ww $wf.waveforms
    set nf $pw.name_frame
    set nn $nf.names

    if ![info exists ::wv_info($f,vectors)] { return 0 }
    if [expr $idx == 0 && $dir == -1] { return 0 }
    set max [expr [llength $::wv_info($f,vectors)]-1]
    if [expr $idx == $max && $dir == 1] { return 0 }
    set swap_idx [expr $idx+$dir]

    # Swap the names
    set vec [lindex $::wv_info($f,vectors) $idx]
    set swap_vec [lindex $::wv_info($f,vectors) $swap_idx]
    lset ::wv_info($f,vectors) $idx $swap_vec
    lset ::wv_info($f,vectors) $swap_idx $vec
    val {blx bly bux buy} [$nn bbox all]
    set ytop [expr $idx*($::wv_height + $::wv_sep)+$::wv_sep]
    set ybot [expr $ytop + $::wv_height]
    set items [$nn find overlapping $blx $ytop $bux $ybot]
    set swap_ytop [expr $swap_idx*($::wv_height + $::wv_sep)+$::wv_sep]
    set swap_ybot [expr $swap_ytop + $::wv_height]
    set swap_items [$nn find overlapping $blx $swap_ytop $bux $swap_ybot]
    foreach item $items {
	$nn move $item 0 [expr $swap_ytop - $ytop]
    }
    foreach swap_item $swap_items {
	$nn move $swap_item 0 [expr $ytop - $swap_ytop]
    }

    # Swap the drawings
    val {blx bly bux buy} [$ww bbox all]
    set ytop [expr $idx*($::wv_height + $::wv_sep)+$::wv_sep]
    set ybot [expr $ytop + $::wv_height]
    set items [$ww find overlapping $blx $ytop $bux $ybot]
    set swap_ytop [expr $swap_idx*($::wv_height + $::wv_sep)+$::wv_sep]
    set swap_ybot [expr $swap_ytop + $::wv_height]
    set swap_items [$ww find overlapping $blx $swap_ytop $bux $swap_ybot]
    foreach item $items {
	$ww move $item 0 [expr $swap_ytop - $ytop]
    }
    foreach swap_item $swap_items {
	$ww move $swap_item 0 [expr $ytop - $swap_ytop]
    }
    return 1
}

proc wv:enter_wv_window {ww x y X Y} {
    set ::wv_focus [focus]
    $ww focus all
    focus $ww
}

proc wv:leave_wv_window {ww x y X Y} {
    catch {focus $::wv_focus}
    unpost_popup $ww
}

proc wv:enter_name_window {nn x y X Y} {
    set ::wv_focus [focus]
    $nn focus all
    focus $nn
    
    set f [winfo parent [winfo parent [winfo parent $nn]]]
    $nn delete _ArRoW_
    set idx [expr int([$nn canvasy $y]/($::wv_height + $::wv_sep))]
    if [info exists ::wv_info($f,vectors)] {
	set max [expr [llength $::wv_info($f,vectors)]-1]
    } else {
	set max 0
    }
    if [expr $idx > $max] { set idx $max }
    set ytop [expr $idx*($::wv_height + $::wv_sep)+$::wv_sep]
    set ybot [expr $ytop + $::wv_height]
    set ymid [expr int(($ytop+$ybot)/2)]
    $nn create line 0 $ymid 12 $ymid -arrow last -fill LightGreen -width 3 \
	-tag _ArRoW_
}

proc wv:leave_name_window {nn x y X Y} {
    catch {focus $::wv_focus}
    $nn delete _ArRoW_
}

proc wv:move_name_window {nn x y X Y} {
    set f [winfo parent [winfo parent [winfo parent $nn]]]
    $nn delete _ArRoW_
    set idx [expr int([$nn canvasy $y]/($::wv_height + $::wv_sep))]
    if [info exists ::wv_info($f,vectors)] {
	set max [expr [llength $::wv_info($f,vectors)]-1]
    } else {
	set max 0
    }
    if [expr $idx > $max] { set idx $max }
    set ytop [expr $idx*($::wv_height + $::wv_sep)+$::wv_sep]
    set ybot [expr $ytop + $::wv_height]
    set ymid [expr int(($ytop+$ybot)/2)]
    $nn create line 0 $ymid 12 $ymid -arrow last -fill LightGreen -width 3 \
	-tag _ArRoW_
}


proc wv:keyboard_move_waveform {nn x y X Y dir} {
    set arrow [$nn find withtag _ArRoW_]
    val {lx ly ux uy} [$nn bbox $arrow]
    set idx [expr int($ly/($::wv_height + $::wv_sep))]
    wv:move_waveform $nn $idx $dir
    update
}

proc wv:keyboard_delete_waveform {nn x y X Y} {
    set arrow [$nn find withtag _ArRoW_]
    val {lx ly ux uy} [$nn bbox $arrow]
    set idx [expr int($ly/($::wv_height + $::wv_sep))]
    wv:delete_waveform $nn $idx
    update
}

proc wv:mk_name_list_menu {w x y X Y} {
    foreach tag [$w gettags current] {
	if [wv:is_real_tag $tag] {
	    lappend vec $tag
	}
    }
    if ![info exist vec] { return }

    set idx [expr int([$w canvasy $y]/($::wv_height + $::wv_sep))]

    set m $w.sel_op
    catch {destroy $m}
    menu $m -tearoff 0

    $m add command -label "Delete" -command "wv:delete_waveform $w $idx"
    $m add command -label "Move up" -command "wv:move_waveform $w $idx -1"
    $m add command -label "Move down" -command "wv:move_waveform $w $idx 1"

    set mm $m.mark
    catch {destroy $mm}
    menu $mm -tearoff 0
    $m add cascade -label "Mark" -menu $mm

    set cols {DarkOrchid1 magenta2 DarkOrange1 green gold3 yellow \
              cyan purple brown}
    set laccs { "1" "2" "3" "4" "5" "6" "7" "8" "9" }

    foreach col $cols lacc $laccs {
        $mm add command -label "" -background $col \
                -command "fl_set_wv_highlight_color $w $col [list $vec]" \
                -accelerator $lacc
    }
    $mm add command -label "Unmark" \
	-command "fl_set_wv_highlight_color $w _OrIgInAlCoLoR_ [list $vec]"

    tk_popup $m $X $Y
}

proc wv:set_sel_to_col {w color} {
    foreach tag [$w gettags current] {
	if [wv:is_real_tag $tag] {
	    fl_set_wv_highlight_color $w $color "$tag"
	}
    }
}

proc wv:prim_add_waveform {w vec} {
    set f $w.f
    set pw $f.panes
    set nf $pw.name_frame
    set nl $nf.title
    set nn $nf.names
    set nx $nf.xscroll
    set sb $nf.yscroll
    set wf $pw.waveform_frame
    set wt $wf.time_scale
    set ww $wf.waveforms
    set wx $wf.xscroll

    unpost_popup $ww
    set new_maxtime [fl_get_ste_maxtime $w]
    if { $new_maxtime != $::wv_info($f,maxtime) } {
	wv:set_new_max_time $w 0
    }
    set t $::wv_info($f,maxtime)
    set vec [clean_name $vec]
    if [info exists ::wv_info($f,vectors)] {
	set cnt [llength $::wv_info($f,vectors)]
    } else {
	set cnt 0
    }
    lappend ::wv_info($f,vectors) $vec
    set ytop [expr $cnt*($::wv_height + $::wv_sep)+$::wv_sep]
    set ybot [expr $ytop + $::wv_height]

    # Add vector name
    set w1 [$nn create rectangle 0 $ybot 400 $ytop -fill white -outline "" \
	    -tags "BaCkGrOuNd$vec $vec"]
    set w2 [$nn create line 0 $ybot 400 $ybot -fill lightblue -dash . ]
    set w3 [$nn create text 15 $ybot -text $vec -anchor sw -justify left \
	    -tags "TxTxT $vec"]

    incr ::wv_info(anon_cnt)
    set vtag [format {VtAg_%06d} $::wv_info(anon_cnt)]
    set ::wv_info(vtag2name,$vtag) $vec

    # Add waveform background
    set maxt [expr $t*$::wv_xscale($f)]
    set bb [$ww create rectangle 0 $ybot $maxt $ytop -fill white \
		-outline "" -tags $vtag]
    wv:add_value_popup $ww $bb
    set bl [$ww create line 0 $ybot $maxt $ybot -fill lightblue -dash . \
		-tags $vtag]
    wv:add_value_popup $ww $bl

    # Add actual waveform
    set cnt [fl_get_waveform_cnt $w $vec $::wv_info($f,show_depend)]
    for {set i 0} {$i < $cnt} {incr i} {
	foreach ch [fl_get_waveform_res $i] {
	    val {nt {v dep_col}} $ch
	    if { $nt >= $t } { continue; }
	    set lx [expr $nt*$::wv_xscale($f)]
	    set rx [expr $t*$::wv_xscale($f)]
	    
	    set t $nt
	    switch $v {
		+	    {
			    wv:add_value_popup $ww \
			    [$ww create rectangle $lx $ybot $rx $ytop \
				    -fill grey \
				    -tags $vtag -outline ""]
			}
		0	    {
			    wv:add_value_popup $ww \
			    [$ww create line $lx $ybot $rx $ybot \
				    -tags $vtag -width 2]
			}
		1	    {
			    wv:add_value_popup $ww \
			    [$ww create line $lx $ytop $rx $ytop \
				    -tags $vtag -width 2]
			}
		*	    {
			    wv:add_value_popup $ww \
			    [$ww create rectangle $lx $ybot $rx $ytop \
				    -fill black \
				    -tags $vtag -outline ""]
			}
		S	    {
			    wv:add_value_popup $ww \
			    [$ww create rectangle $lx $ybot $rx $ytop \
				    -tags $vtag -fill $dep_col]
			    set xmid [expr round(($lx+$rx)/2.0)]
			    set ymid [expr round(($ytop+$ybot)/2.0)]
			    set txt [$ww create text $xmid $ymid -text S \
					-anchor c -tags $vtag]
			    wv:add_value_popup $ww $txt
			}
		S0X	    {
			    if { $dep_col == "white" } { set dep_col "black" }
			    wv:add_value_popup $ww \
			    [$ww create rectangle $lx $ybot $rx $ytop \
				    -tags $vtag -outline "" \
				    -fill $dep_col \
				    -stipple @[file join $::imagedir X_pat.xbm]]
			    wv:add_value_popup $ww \
			    [$ww create line $lx $ybot $rx $ybot \
				    -tags $vtag -width 2]
			}
		SX1	    {
			    if { $dep_col == "white" } { set dep_col "black" }
			    wv:add_value_popup $ww \
			    [$ww create rectangle $lx $ybot $rx $ytop \
				    -tags $vtag -outline "" \
				    -fill $dep_col \
				    -stipple @[file join $::imagedir X_pat.xbm]]
			    wv:add_value_popup $ww \
			    [$ww create line $lx $ytop $rx $ytop \
				    -tags $vtag -width 2]
			}
		S0X1    {
			    if { $dep_col == "white" } { set dep_col "black" }
			    wv:add_value_popup $ww \
			    [$ww create rectangle $lx $ybot $rx $ytop \
				    -tags $vtag -outline "" \
				    -fill $dep_col \
				    -stipple @[file join $::imagedir X_pat.xbm]]
			    wv:add_value_popup $ww \
			    [$ww create line $lx $ybot $rx $ybot \
				    -tags $vtag -width 2]
			    wv:add_value_popup $ww \
			    [$ww create line $lx $ytop $rx $ytop \
				    -tags $vtag -width 2]
			}
		default {
			    if [regexp {\\+} $v] {
				if { $dep_col == "white" } {
				    set dep_col "black"
				}
				wv:add_value_popup $ww \
				[$ww create rectangle $lx $ybot $rx $ytop \
					-tags $vtag \
					-fill $dep_col \
				    -stipple @[file join $::imagedir X_pat.xbm]]
			    } else {
				wv:add_value_popup $ww \
				[$ww create rectangle $lx $ybot $rx $ytop \
					-tags $vtag -fill $dep_col]
			    }
			    set xmid [expr round(($lx+$rx)/2.0)]
			    set ymid [expr round(($ytop+$ybot)/2.0)]
			    set txt [$ww create text $xmid $ymid -text $v \
					-anchor c -tags $vtag]
			    wv:add_value_popup $ww $txt
			    val {blx bly bux buy} [$ww bbox $txt]
			    if [expr ($blx < $lx) || ($rx < $bux) ] {
				# Text does not fit
				$ww itemconfigure $txt -text "?"
			    }
			    $ww raise $txt
			}
	    }
	    wv:add_value_popup $ww \
		[$ww create line $lx $ybot $lx $ytop -tags $vtag -width 2]
	}
    }
    $ww config -scrollregion [$ww bbox all]
    $wt config -scrollregion [$ww bbox all]
    val {alx aly aux auy} [$nn bbox TxTxT]
    val {blx bly bux buy} [$nn bbox all]
    $nn config -scrollregion [list 0 $bly $aux $buy]
}

proc wv:zoom_in {w} {
    set f $w.f
    set cur $::wv_xscale($f)
    set new [expr round($cur*0.75)]
    if { $new > 3 } {
	set ::wv_xscale($f) $new
    }
    wv:set_new_max_time $w 1
}

proc wv:zoom_out {w} {
    set f $w.f
    set cur $::wv_xscale($f)
    set new [expr round($cur*1.25)]
    set ::wv_xscale($f) $new
    wv:set_new_max_time $w 1
}

proc wv:set_new_max_time {w force} {
    set f $w.f
    set pw $f.panes
    set nf $pw.name_frame
    set nl $nf.title
    set nn $nf.names
    set nx $nf.xscroll
    set sb $nf.yscroll
    set wf $pw.waveform_frame
    set wt $wf.time_scale
    set ww $wf.waveforms
    set wx $wf.xscroll
    
    set new_maxtime [fl_get_ste_maxtime $w]
    if { !$force && $new_maxtime == $::wv_info($f,maxtime) } { return }

    set ::wv_info($f,maxtime) $new_maxtime
    if [info exists ::wv_info($f,vectors)] {
	set vectors $::wv_info($f,vectors)
    } else {
	set vectors {}
    }
    set ::wv_info($f,vectors) {}

    $wt delete all
    $ww delete all
    $nn delete all

    wv:draw_time_line $f $::wv_info($f,maxtime)

    foreach vec $vectors {
	wv:add_waveform $w $vec
    }
}


proc wv:toggle_show_depend {w} {
    set f $w.f
    set pw $f.panes
    set nf $pw.name_frame
    set nn $nf.names
    set wf $pw.waveform_frame
    set wt $wf.time_scale
    set ww $wf.waveforms
    
    set new_maxtime [fl_get_ste_maxtime $w]

    set ::wv_info($f,maxtime) $new_maxtime
    if [info exists ::wv_info($f,vectors)] {
	set vectors $::wv_info($f,vectors)
    } else {
	set vectors {}
    }
    set ::wv_info($f,vectors) {}

    $wt delete all
    $ww delete all
    $nn delete all

    wv:draw_time_line $f $::wv_info($f,maxtime)

    foreach vec $vectors {
	wv:add_waveform $w $vec
    }
}
