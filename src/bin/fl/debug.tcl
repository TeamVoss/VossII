# ---------------------------------------------------
set max_window_width	900
set max_window_height	900
set sc		.65		;# Scaling factor
set gcolor	"black"		;# line color
set fc		"white"		;# fill color
set vcolor	"black"		;# Value color
set ncolor	"black"		;# Node name color
set c		".c"
set inv_rad	[expr 4*$sc]
set buf_ht	[expr 30*$sc]
set buf_wid	[expr 30*$sc]
set and_ht	[expr 40*$sc]
set and_wid	[expr 40*$sc]
set rect_ht	[expr 15*$sc]
set rect_wid	[expr 40*$sc]
set or_ht	[expr 40*$sc]
set or_wid	[expr 40*$sc]
set xor_sep	[expr 5*$sc]
set wire_len	[expr 10*$sc]
set deltax	[expr 10*$sc]
set min_sep	[expr 5*$sc]
set inp_ht	[expr 8*$sc]
set ptr_ht	[expr 3*$sc]
set sfont -*-Courier-Medium-R-Normal--*-100-*
set mfont -*-Courier-Medium-R-Normal--*-120-*

proc safe_name {name} {
    regsub -all {\\} "$name" "" tmp
    regsub -all {\-} "$tmp" "\\-" tmp2
    regsub -all {\[} $tmp2 "\\\[" tmp3
    regsub -all {\]} "$tmp3" "\\\]" res
    return $res
}

proc print_proc {} {
    global c

    proc do_print {w} {
	global c
	set name [$w.e1 get]
	set size [grow_bbox [$c bbox all]]
	set x1 [lindex $size 0]
	set y1 [lindex $size 1]
	set x2 [lindex $size 2]
	set y2 [lindex $size 3]
	set xsize [expr $x2-$x1+30]
	set ysize [expr $y2-$y1+10]
	if {$xsize < $ysize } {
	    # Use portrait mode
	    set wscale [expr $xsize/7.5]
	    set hscale [expr $ysize/10.0]
	    if {$wscale > $hscale } {
		# Scale width to fit on page
		$c postscript -file $name -x $x1 -y $y1 -width $xsize \
			-height $ysize -pagewidth 7.5i
	    } else {
		# Scale height to fit on page
		$c postscript -file $name -x $x1 -y $y1 -width $xsize \
			-height $ysize -pageheight 10.0i
	    }
	} else {
	    # Use landscape mode
	    set wscale [expr $xsize/10.0]
	    set hscale [expr $ysize/7.5]
	    if {$wscale > $hscale } {
		# Scale width to fit on page
		$c postscript -file $name -rotate "y" \
		    -x $x1 -y $y1 -width $xsize -height $ysize -pagewidth 10.0i
	    } else {
		# Scale height to fit on page
		$c postscript -file $name -rotate "y" \
		    -x $x1 -y $y1 -width $xsize -height $ysize -pageheight 7.5i
	    }
	}
	destroy $w
	focus .
	return
    }

    set w .entry
    catch {destroy $w}
    toplevel $w
    wm title $w "Save to Postscript file"
    wm geometry $w +200+100

    label $w.msg -justify left -text "File to send PostScript to: "
    entry $w.e1 -relief sunken
    $w.e1 insert 0 "schematics.ps"
    button $w.cancel -text Cancel -command "destroy $w; focus .; return"
    button $w.ok -text Ok -command "do_print $w"
    pack $w.msg $w.e1 -side top -padx 10 -fill x
    pack $w.cancel -side left -padx 10
    pack $w.ok
    focus $w.ok
}

proc set_plot_options {} {
    global from_time to_time c

    proc set_plot_pref {w} {
	global from_time to_time
	set from_time [$w.min_time get]
	set to_time [$w.max_time get]
	destroy $w
    }

    set w .plot_options
    catch {destroy $w}
    toplevel $w
    wm title $w "Waveform generation preferences"
    wm geometry $w +200+100

    label $w.msg1 -justify left -text "Minimum time for waveform: "
    entry $w.min_time -relief sunken
    $w.min_time insert 0 $from_time
    label $w.msg2 -justify left -text "Maximum time for waveform: "
    entry $w.max_time -relief sunken
    $w.max_time insert 0 $to_time

    button $w.cancel -text Cancel -command "destroy $w; return"
    button $w.ok -text "Set options" -command "set_plot_pref $w"

    pack $w.msg1 $w.min_time -side top -padx 10 -fill x
    pack $w.msg2 $w.max_time -side top -padx 10 -fill x
    pack $w.cancel -side left -padx 10
    pack $w.ok
    focus $w.cancel
}


proc set_options {} {
    global c top_levels shownames showvalues fullwindow

    proc set_the_values {w old_top_levels old_shownames old_showvalues old_fullwindow } {
	global c top_levels shownames showvalues fullwindow \
		max_window_width max_window_height
	set top_levels [$w.depth get]
	destroy $w
	if {$top_levels != $old_top_levels } {
	    re_draw
	} else {
	    if {$shownames != $old_shownames } {
		re_draw
	    } else {
		if {$showvalues != $old_showvalues } {
		    re_draw
		} else {
		    if {$fullwindow != $old_fullwindow } {
			set size [$c bbox all]
			set x1 [lindex $size 0]
			set y1 [lindex $size 1]
			set x2 [lindex $size 2]
			set y2 [lindex $size 3]
			set xsize [expr $x2-$x1+10]
			set ysize [expr $y2-$y1+100]
			if {$fullwindow == 0} {
			    if {$xsize > $max_window_width} {
				set xsize $max_window_width
			    }
			    if {$ysize > $max_window_height} {
				set ysize $max_window_height
			    }
			}
			if { $xsize < 300 } {
			    set xsize 300
			}
			if { $ysize < 300 } {
			    set ysize 300
			}
			wm geometry . [join [list = $xsize x $ysize +50+10] ""]
			$c config -scrollregion [grow_bbox $size]
		    } else {
			return
		    }
		}
	    }
	}
	return
    }
    set old_top_levels $top_levels
    set old_shownames $shownames
    set old_showvalues $showvalues
    set old_fullwindow $fullwindow
    set w .options
    catch {destroy $w}
    toplevel $w
    wm title $w "Preferences"
    wm geometry $w +200+100

    label $w.msg -justify left -text "Depth of fanin cone: "
    entry $w.depth -relief sunken
    $w.depth insert 0 $top_levels
    button $w.cancel -text Cancel -command "destroy $w; return"
    button $w.ok -text "Set options" -command \
	"set_the_values $w $old_top_levels $old_shownames $old_showvalues $old_fullwindow"
    checkbutton $w.nametoggle -text "Show node names" -variable shownames -relief flat
    checkbutton $w.valtoggle -text "Show node values" -variable showvalues -relief flat
    checkbutton $w.fullscreen -text "Full size window" -variable fullwindow -relief flat

    pack $w.msg $w.depth -side top -padx 10 -fill x
    pack $w.nametoggle -side top -pady 10
    pack $w.valtoggle -side top -pady 10
    pack $w.fullscreen -side top -pady 10
    pack $w.cancel -side left -padx 10
    pack $w.ok
    focus $w.cancel
}



proc clear_selection {} {
	global c
	fl_delete_selected
 	set iteml [$c find all]
 	foreach item $iteml {
	    set tag [lindex [$c gettags $item] 0]
	    if {$tag != {current}} {
		deselect $tag
	    }
	}
}

proc select_all {} {
	global c
	fl_delete_selected
	catch {unset tag_done}
 	set iteml [$c find all]
 	foreach item $iteml {
	    set tag [lindex [$c gettags $item] 0]
	    if {[catch {set ans $tag_done($tag)}] != 0} {
		set tag_done($tag) "y"
		if {$tag != {current}} {
		    select $tag
		}
	    }
	}
}

proc post_name {tag} {
    global info_lbl
    $info_lbl configure -text "Node name: $tag"
}

proc post_excitation {tag} {
    global info_lbl
    $info_lbl configure -text "Excitation: [fl_excitation $tag]"
}

proc post_symb_value {val} {
    global info_lbl
    $info_lbl configure -text "Value:  $val"
}

proc unpost_name "" {
    global info_lbl
    $info_lbl configure -text ""
}

proc input {i gate} {
	global input_loc
	return $input_loc($gate,$i)
}

proc output {gate} {
	global output_loc
	return $output_loc($gate)
}

proc connect {xy1 ogate i igate} {
    global c gcolor output_loc input_loc wire_len nbr_inputs min_sep
    set x1 [lindex $xy1 0]
    set y1 [lindex $xy1 1]
    set xy2 $input_loc($igate,$i)
    set inputs $nbr_inputs($igate)
    set x2 [lindex $xy2 0]
    set y2 [lindex $xy2 1]
    if { $y1 < $y2 } {
	# Connection going upwards
	set xmid [expr $x2-($i+2)*$min_sep]
    } else {
	# Connection going downwards
	set xmid [expr $x2-($inputs-$i+3)*$min_sep]
    }
    return [$c create line $x1 $y1 $xmid $y1 $xmid $y2 $x2 $y2 \
		-fill $gcolor -tags $ogate]
}

proc select {node} {
	global c
	$c itemconfigure $node -fill green
	fl_select $node
}

proc deselect {node} {
	global c fc gcolor
	fl_deselect $node
 	set iteml [$c find withtag $node]
 	foreach item $iteml {
	    switch [$c type $item] {
		polygon		{$c itemconfigure $item -fill $fc}
		rectangle	{$c itemconfigure $item -fill $fc}
		oval		{$c itemconfigure $item -fill $fc}
		default		{$c itemconfigure $item -fill $gcolor}}
	}
}

proc draw_output {gate} {
	global c wire_len gcolor fc
	set xy [output $gate]
	set x [lindex $xy 0]
	set y [lindex $xy 1]
	set xl [expr $x+2*$wire_len]
	set r1 [$c create line $x $y $xl $y -fill $gcolor -tags $gate]
	set r2 [$c create text $xl $y -anchor w -text $gate -tags $gate -fill $gcolor]
	return [concat $r1 $r2]
}

proc draw_name_and_value {unnamed x y node} {
    global c vcolor showvalues shownames ncolor sfont mfont
    if {$unnamed != 0} {
	if {$shownames != 0 } {
	    set r1 [$c create text $x $y -anchor sw -fill $ncolor -text $node \
			    -font $sfont -tags name_label]
	} else {
	    set r1 ""
	}
    } else {
	set r1 ""
    }
    if {$showvalues != 0} {
	set value [fl_get_value $node]
	switch $value {
			0	{set scalar 1}
			1	{set scalar 1}
			X	{set scalar 1}
			"#"	{set scalar 1}
			default	{set scalar 0} }
	if {$scalar != 0 } {
	    set r2 [$c create text $x $y -anchor nw -fill $vcolor \
			-font $mfont -text $value]
	} else {
	    set ret [$c create text $x $y -anchor nw -fill $vcolor \
			-font $mfont -text S]
	    $c bind $ret <Enter> "post_symb_value $value"
	    $c bind $ret <Leave> "unpost_name"
	    set r2 $ret
	}
    } else {
	set r2 ""
    }
    return [concat $r1 $r2]
}


proc draw_input {xy tag} {
    global c buf_ht buf_wid inv_rad wire_len gcolor input_loc output_loc \
	    nbr_inputs fc inp_ht
    set output_loc($tag) $xy
    set x [lindex $xy 0]
    set y [lindex $xy 1]
    set xl [expr $x-$wire_len]
    set r1 [$c create line $x $y $xl $y -fill $gcolor \
		    -tags $tag]
    set r2 [$c create polygon	[expr $xl-$inp_ht] [expr $y+$inp_ht/2] \
			    [expr $xl-$inp_ht] [expr $y-$inp_ht/2] \
			    $xl $y \
			    [expr $xl-$inp_ht] [expr $y+$inp_ht/2] \
			    -outline $gcolor -fill $fc -tags $tag]
    set r3 [$c create text [expr $xl-$inp_ht-2] $y \
		-anchor e -text $tag -tags $tag]
    set nbr_inputs($tag) 0
    return [concat $r1 $r2 $r3 [draw_name_and_value 0 $x $y $tag]]
}

proc draw_repeat_nd {xy tag} {
	global c wire_len gcolor output_loc nbr_inputs ptr_ht
	set output_loc($tag) $xy
	set x [lindex $xy 0]
	set y [lindex $xy 1]
	set xl [expr $x-$wire_len/2]
	set f1 [$c create line $x $y $xl $y -fill $gcolor -tags $tag]
	set f2 [$c create rectangle [expr $xl-2*$ptr_ht] [expr $y+$ptr_ht] \
				    $xl [expr $y-$ptr_ht] \
				    -fill black -outline $gcolor \
				    -tags [list $tag RePeAt]]
	return [concat $f1 $f2 [draw_name_and_value 1 $x $y $tag]]
}

proc draw_incomplete {xy tag} {
    global c wire_len gcolor input_loc output_loc nbr_inputs top_levels \
		shownames showvalues
    set output_loc($tag) $xy
    set x [lindex $xy 0]
    set y [lindex $xy 1]
    set xl [expr $x-$wire_len]
    set r1 [$c create line $x $y $xl $y -fill $gcolor -tags $tag]
    set r2 [$c create text $xl $y -anchor e -text "... " -tags $tag]
    set nbr_inputs($tag) 0
    return [concat $r1 $r2 [draw_name_and_value 1 $x $y $tag]]
}

proc draw_box_pat {xy inputs tag pic_proc pic_proc_list} {
	global c rect_ht rect_wid wire_len gcolor input_loc output_loc \
		nbr_inputs fc deltax
	set output_loc($tag) $xy
	set x [lindex $xy 0]
	set y [lindex $xy 1]
	set rht [expr ($inputs+1)*$rect_ht]
	set r1 [$c create rectangle  \
				[expr $x-$rect_wid] [expr $y+$rht/2] \
				$x [expr $y-$rht/2] \
			 	-outline $gcolor -fill $fc \
				-tags $tag]
	set nx [expr $x-$rect_wid]
	set lowy [expr $y-$rht/2+$rht/($inputs*2)]
	set sep [expr $rht/$inputs]
	set r2 [$pic_proc $nx $y $tag]
	set inp_patterns [llength $pic_proc_list]
	for {set i 0} {$i < $inputs} {incr i} {
	    set yl [expr $lowy+$i*$sep]
	    if {$inp_patterns != 0} {
		append r2 " " [[lindex $pic_proc_list $i] $nx $yl $tag]
	    }
	    set input_loc($tag,[expr $i+1]) [list $nx $yl]
	}
	set nbr_inputs($tag) $inputs
	return [concat $r1 $r2 [draw_name_and_value 1 [expr $x+2] $y $tag]]
}

proc draw_buf_pat {xy out_neg tag} {
    global c buf_ht buf_wid inv_rad wire_len gcolor input_loc output_loc \
	    nbr_inputs fc
    set output_loc($tag) $xy
    set x [lindex $xy 0]
    set y [lindex $xy 1]
    if {$out_neg != ""} {
	set nx [expr $x-$inv_rad*2]
	set r1 [$c create oval [expr $x-2*$inv_rad] [expr $y-$inv_rad] \
		       $x [expr $y+$inv_rad] \
		       -outline $gcolor -fill $fc -tags $tag]
    } else {
	set nx $x
	set r1 ""
    }
    set r2 [$c create polygon \
			    [expr $nx-$buf_wid] [expr $y+$buf_ht/2] \
			    [expr $nx-$buf_wid] [expr $y-$buf_ht/2] \
			    $nx $y \
			    [expr $nx-$buf_wid] [expr $y+$buf_ht/2] \
			    -outline $gcolor -fill $fc -tags $tag]
    set nx [expr $nx-$buf_wid]
    set xl [expr $nx-$wire_len]
    set input_loc($tag,1) [list $nx $y]
    set nbr_inputs($tag) 1
    return [concat $r1 $r2 [draw_name_and_value 1 [expr $x+2] $y $tag]]
}

proc draw_and_pat {xy out_neg inputs tag} {
    global c and_ht and_wid inv_rad gcolor wire_len deltax \
	    input_loc output_loc nbr_inputs fc
    set output_loc($tag) $xy
    set x [lindex $xy 0]
    set y [lindex $xy 1]
    if {$out_neg != ""} {
	set nx [expr $x-$inv_rad*2+2]
	set r1 [$c create oval [expr $x-2*$inv_rad] [expr $y-$inv_rad] \
		       $x [expr $y+$inv_rad] \
		       -outline $gcolor -fill $fc -tags $tag]
    } else {
	set nx $x
	set r1 ""
    }
    set r2 [$c create polygon \
			    [expr $nx-$and_wid] [expr $y-$and_ht/2] \
			    [expr $nx-$and_wid] [expr $y+$and_ht/2] \
			    [expr $nx-$and_wid] [expr $y+$and_ht/2] \
			    [expr $nx-$and_wid/4] [expr $y+$and_ht/2] \
			    [expr 1.00*$nx] [expr $y] \
			    [expr $nx-$and_wid/4] [expr $y-$and_ht/2] \
			    [expr $nx-$and_wid] [expr $y-$and_ht/2] \
			    [expr $nx-$and_wid] [expr $y-$and_ht/2] \
			    -smooth yes -outline $gcolor -fill $fc \
			    -tags $tag]
    set nx [expr $nx-$and_wid]
    set lowy [expr $y-$and_ht/2+$and_ht/($inputs*2)]
    set sep [expr $and_ht/$inputs]
    for {set i 0} {$i < $inputs} {incr i} {
	set yl [expr $lowy+$i*$sep]
	set input_loc($tag,[expr $i+1]) [list $nx $yl]
    }
    set nbr_inputs($tag) $inputs
    return [concat $r1 $r2 [draw_name_and_value 1 [expr $x+2] $y $tag]]
}

proc draw_or_pat {xy out_neg is_xor inputs tag} {
    global c or_ht or_wid inv_rad xor_sep gcolor wire_len deltax \
	    input_loc output_loc nbr_inputs fc
    set output_loc($tag) $xy
    set x [lindex $xy 0]
    set y [lindex $xy 1]
    if {$out_neg != ""} {
	set nx [expr $x-$inv_rad*2]
	set r1 [$c create oval [expr $x-2*$inv_rad] [expr $y-$inv_rad] \
		       $x [expr $y+$inv_rad] \
		       -outline $gcolor -fill $fc -tags $tag]
    } else {
	set nx $x
	set r1 ""
    }
    set r2 [$c create polygon \
			[expr $nx-$or_wid] [expr $y+$or_ht/2] \
			[expr $nx-$or_wid] [expr $y+$or_ht/2] \
			[expr $nx-5*$or_wid/6] $y \
			[expr $nx-$or_wid] [expr $y-$or_ht/2] \
			[expr $nx-$or_wid] [expr $y-$or_ht/2] \
			[expr $nx-3*$or_wid/4] [expr $y-$or_ht/2] \
			[expr $nx-1*$or_wid/3] [expr $y-6*($or_ht/2)/7] \
			$nx $y \
			$nx $y \
			[expr $nx-1*$or_wid/3] [expr $y+6*($or_ht/2)/7] \
			[expr $nx-3*$or_wid/4] [expr $y+$or_ht/2] \
			[expr $nx-$or_wid] [expr $y+$or_ht/2] \
			[expr $nx-$or_wid] [expr $y+$or_ht/2] \
			-smooth yes -outline $gcolor -fill $fc \
			-tags $tag]
    if {$is_xor != ""} {
	set nx [expr $nx-$xor_sep]
	set r3 [$c create line  [expr $nx-$or_wid] [expr $y-$or_ht/2] \
			[expr $nx-$or_wid] [expr $y-$or_ht/2] \
			[expr $nx-5*$or_wid/6] $y \
			[expr $nx-$or_wid] [expr $y+$or_ht/2] \
			[expr $nx-$or_wid] [expr $y+$or_ht/2] \
			-fill $gcolor -smooth yes]
    } else {
	set r3 ""
    }
    set nx [expr $nx-$or_wid]
    set lowy [expr $y-$or_ht/2+$or_ht/($inputs*2)]
    set sep [expr $or_ht/$inputs]
    for {set i 0} {$i < $inputs} {incr i} {
	set yl [expr $lowy+$i*$sep]
	if {[expr 2*$i] <= [expr $inputs+1]} {
	    set xx [expr $nx+($or_wid*($i+1)/($inputs+1))/6]
	} else {
	    set xx [expr $nx+($or_wid*($inputs+1-$i)/($inputs+1))/6]
	}
	set input_loc($tag,[expr $i+1]) [list $xx $yl]
    }
    set nbr_inputs($tag) $inputs
    return [concat $r1 $r2 $r3 [draw_name_and_value 1 [expr $x+2] $y $tag]]
}

proc draw_unknown_gate {inputs xy tag} {
    proc unk {x y tag} {
	global c gcolor rect_wid
	set f [$c create text -2 0 -anchor w -text "?"]
	$c move $f [expr $x+$rect_wid/2] $y
	$c bind $f <Enter> "post_excitation [safe_name $tag]"
	$c bind $f <Leave> "unpost_name"
	return $f
    }
    draw_box_pat $xy $inputs $tag {unk} ""
}

proc draw_buffer {inputs xy tag} {
	draw_buf_pat $xy "" $tag
}

proc draw_inverter {inputs xy tag} {
	draw_buf_pat $xy "Y" $tag
}

proc draw_and2 {inputs xy tag} {
	draw_and_pat $xy "" 2 $tag
}


proc draw_and3 {inputs xy tag} {
	draw_and_pat $xy "" 3 $tag
}


proc draw_and4 {inputs xy tag} {
	draw_and_pat $xy "" 4 $tag
}


proc draw_nand2 {inputs xy tag} {
	draw_and_pat $xy "Y" 2 $tag
}


proc draw_nand3 {inputs xy tag} {
	draw_and_pat $xy "Y" 3 $tag
}

proc draw_nand4 {inputs xy tag} {
	draw_and_pat $xy "Y" 4 $tag
}


proc draw_or2 {inputs xy tag} {
	draw_or_pat $xy "" "" 2 $tag
}

proc draw_or3 {inputs xy tag} {
	draw_or_pat $xy "" "" 3 $tag
}

proc draw_or4 {inputs xy tag} {
	draw_or_pat $xy "" "" 4 $tag
}


proc draw_nor2 {inputs xy tag} {
	draw_or_pat $xy "Y" "" 2 $tag
}

proc draw_nor3 {inputs xy tag} {
	draw_or_pat $xy "Y" "" 3 $tag
}

proc draw_nor4 {inputs xy tag} {
	draw_or_pat $xy "Y" "" 4 $tag
}

proc draw_xor2 {inputs xy tag} {
	draw_or_pat $xy "" "Y" 2 $tag
}

proc draw_xor3 {inputs xy tag} {
	draw_or_pat $xy "" "Y" 3 $tag
}

proc draw_xor4 {inputs xy tag} {
	draw_or_pat $xy "" "Y" 4 $tag
}

proc draw_xnor2 {inputs xy tag} {
	draw_or_pat $xy "Y" "Y" 2 $tag
}

proc draw_xnor3 {inputs xy tag} {
	draw_or_pat $xy "Y" "Y" 3 $tag
}

proc draw_xnor4 {inputs xy tag} {
	draw_or_pat $xy "Y" "Y" 4 $tag
}

proc Dlabel {x y tag} {
    global c mfont
    $c create text [expr $x+1] $y -anchor w -text D -font $mfont
}
proc Elabel {x y tag} {
    global c mfont
    $c create text [expr $x+1] $y -anchor w -text E -font $mfont
}
proc Rlabel {x y tag} {
    global c mfont
    $c create text [expr $x+1] $y -anchor w -text R -font $mfont
}
proc Slabel {x y tag} {
    global c mfont
    $c create text [expr $x+1] $y -anchor w -text S -font $mfont
}
proc ZeroLabel {x y tag} {
    global c mfont
    $c create text [expr $x+1] $y -anchor w -text 0 -font $mfont
}
proc OneLabel {x y tag} {
    global c mfont
    $c create text [expr $x+1] $y -anchor w -text 1 -font $mfont
}
proc UnLabeled {x y tag} {
}
proc EdgeLabel {x y tag} {
    global c gcolor rect_wid mfont
    set s [expr $rect_wid/20]
    return [$c create line $x [expr $y-3*$s] [expr $x+4*$s] $y $x [expr $y+3*$s] \
	-fill $gcolor]
}
proc ActHighWaveForm {x y tag} {
    global c gcolor rect_wid
    set s [expr $rect_wid/20]
    set f1 [$c create line 0 0 [expr 4*$s] 0 [expr 4*$s] [expr -8*$s] [expr 8*$s] [expr -8*$s] [expr 8*$s] 0 [expr 12*$s] 0 -fill $gcolor]
    $c move $f1 [expr $x+4*$s] [expr $y+4*$s]
    return $f1
}
proc ActLowWaveForm {x y tag} {
    global c gcolor rect_wid
    set s [expr $rect_wid/20]
    set f1 [$c create line 0 [expr -4*$s] [expr 4*$s] [expr -4*$s] [expr 4*$s] [expr 4*$s] [expr 8*$s] [expr 4*$s] [expr 8*$s] [expr -4*$s] [expr 12*$s] [expr -4*$s] -fill $gcolor]
    $c move $f1 [expr $x+4*$s] [expr $y+4*$s]
    return $f1
}
proc RisingEdge {x y tag} {
    global c gcolor rect_wid mfont
    set s [expr $rect_wid/20]
    set f1 [$c create line 0 0 [expr 4*$s] 0 [expr 4*$s] [expr -8*$s] [expr 8*$s] [expr -8*$s] -fill $gcolor]
    $c move $f1 [expr $x+8*$s] [expr $y+4*$s]
    return $f1
}
proc FallingEdge {x y tag} {
    global c gcolor rect_wid
    set s [expr $rect_wid/20]
    set f1 [$c create line 0 [expr -4*$s] [expr 4*$s] [expr -4*$s] [expr 4*$s] [expr 4*$s] [expr 8*$s] [expr 4*$s] -fill $gcolor]
    $c move $f1 [expr $x+8*$s] [expr $y-1*$s]
    return $f1
}

proc draw_MUX2 {inputs xy tag } {
    proc MUXpic {x y tag} {
	global c gcolor rect_wid mfont
	set s [expr $rect_wid/20]
	set f1 [$c create text 0 0 -anchor w -text MUX -font $mfont]
	$c move $f1 [expr $x+1] $y
	return $f1
    }
    draw_box_pat $xy 3 $tag {MUXpic} {ZeroLabel UnLabeled OneLabel}
}

proc draw_pos_D_latch {inputs xy tag } {
    draw_box_pat $xy 2 $tag {ActHighWaveForm} {Dlabel EdgeLabel}
}

proc draw_neg_D_latch {inputs xy tag } {
    draw_box_pat $xy 2 $tag {ActLowWaveForm} {Dlabel EdgeLabel}
}

proc draw_master_slave {inputs xy tag } {
    draw_box_pat $xy 2 $tag {RisingEdge} {Dlabel EdgeLabel}
}

proc draw_ff_re_with_en {inputs xy tag } {
    draw_box_pat $xy 3 $tag {RisingEdge} {Dlabel Elabel EdgeLabel}
}

proc draw_ff_fe_with_en {inputs xy tag } {
    draw_box_pat $xy 3 $tag {FallingEdge} {Dlabel Elabel EdgeLabel}
}

proc draw_ff_fe {inputs xy tag } {
    draw_box_pat $xy 2 $tag {FallingEdge} {Dlabel EdgeLabel}
}

proc draw_ff_re {inputs xy tag } {
    draw_box_pat $xy 2 $tag {RisingEdge} {Dlabel EdgeLabel}
}

proc draw_ff_re_with_en_reset {inputs xy tag} {
    draw_box_pat $xy 4 $tag {RisingEdge} {Dlabel Elabel Rlabel EdgeLabel}
}

proc draw_ff_fe_with_en_reset {inputs xy tag} {
    draw_box_pat $xy 4 $tag {FallingEdge} {Dlabel Elabel Rlabel EdgeLabel}
}

proc draw_ff_re_with_en_set {inputs xy tag} {
    draw_box_pat $xy 4 $tag {RisingEdge} {Dlabel Elabel Slabel EdgeLabel}
}

proc draw_ff_fe_with_en_set {inputs xy tag} {
    draw_box_pat $xy 4 $tag {FallingEdge} {Dlabel Elabel Slabel EdgeLabel}
}

proc draw_ff_re_with_en_reset_set {inputs xy tag} {
    draw_box_pat $xy 5 $tag {RisingEdge} {Dlabel Elabel Rlabel Slabel EdgeLabel}
}

proc draw_ff_fe_with_en_reset_set {inputs xy tag} {
    draw_box_pat $xy 5 $tag {FallingEdge} {Dlabel Elabel Rlabel Slabel EdgeLabel}
}

proc draw_ff_re_reset {inputs xy tag} {
    draw_box_pat $xy 3 $tag {RisingEdge} {Dlabel Rlabel EdgeLabel}
}
proc draw_ff_fe_reset {inputs xy tag} {
    draw_box_pat $xy 3 $tag {FallingEdge} {Dlabel Rlabel EdgeLabel}
}

proc draw_ff_re_set {inputs xy tag} {
    draw_box_pat $xy 3 $tag {RisingEdge} {Dlabel Slabel EdgeLabel}
}
proc draw_ff_fe_set {inputs xy tag} {
    draw_box_pat $xy 3 $tag {FallingEdge} {Dlabel Slabel EdgeLabel}
}

proc draw_ff_re_reset_set {inputs xy tag} {
    draw_box_pat $xy 4 $tag {RisingEdge} {Dlabel Slabel Rlabel EdgeLabel}
}
proc draw_ff_fe_reset_set {inputs xy tag} {
    draw_box_pat $xy 4 $tag {FallingEdge} {Dlabel Slabel Rlabel EdgeLabel}
}

proc draw_posedge_ff {inputs xy tag } {
    proc pic {x y tag} {
	global c gcolor rect_wid
	set s [expr $rect_wid/20]
	set f1 [$c create line 0 0 [expr 4*$s] 0 [expr 4*$s] [expr -8*$s] [expr 8*$s] [expr -8*$s] -fill $gcolor]
	$c move $f1 [expr $x+2*$s] [expr $y+4*$s]
	set f2 [$c create text 0 -10 -anchor w -text D]
	$c move $f2 [expr $x+1] $y
	return [list $f1 $f2]
    }
    draw_box_pat $xy 2 $tag {pic} ""
}

proc draw_negedge_ff {inputs xy tag } {
    proc pic {x y tag} {
	global c gcolor rect_wid
	set s [expr $rect_wid/20]
	set f1 [$c create line 0 [expr -4*$s] [expr 4*$s] [expr -4*$s] [expr 4*$s] [expr 4*$s] [expr 8*$s] [expr 4*$s] -fill $gcolor]
	$c move $f1 [expr $x+2*$s] [expr $y+4*$s]
	set f2 [$c create text 0 -10 -anchor w -text D]
	$c move $f2 [expr $x+1] $y
	return [list $f1 $f2]
    }
    draw_box_pat $xy 2 $tag {pic} ""
}

# Drawing canvas and scroll bars
canvas $c -background white  -xscrollcommand ".hscroll set" \
			    -yscrollcommand ".vscroll set"
#			    -scrollregion {-5c -5c 10c 10c}
scrollbar .hscroll -orient horiz -command ".c xview"
scrollbar .vscroll -command ".c yview"
# Information window
set info_lbl	[label $c.info_text -text "" -anchor w]

# Menus
frame .menu -relief raised -bd 1
set m .menu.file.m
menubutton .menu.file -text "File" -menu $m -underline 0
menu $m
# $m add separator
$m add command -label "Preferences" -command "set_options" -accelerator "P"
$m add command -label "Save" -command "print_proc" -accelerator "S"
$m add command -label "Quit" -command "destroy ." -accelerator "Q"
bind all <Q> "destroy ."
bind all <S> "print_proc"
bind all <P> "set_options"

set m .menu.selection.m
menubutton .menu.selection -text "Selection" -menu $m -underline 0
menu $m
$m add command -label "Select all" -command "select_all"
$m add command -label "Clear Selection" -command "clear_selection" \
	-accelerator "R"
bind all <R> "clear_selection"

set m .menu.waveforms.m
menubutton .menu.waveforms -text "Waveforms" -menu $m -underline 0
menu $m
$m add command -label "Set plot options" -command "set_plot_options"
$m add command -label "Plot selected nodes" -command "draw_waveforms"

proc draw_waveforms {} {
    global from_time to_time
    fl_draw_waveform $from_time $to_time
}

# Layout
pack .menu -side top -fill x
pack $c.info_text -side top -fill x
pack .hscroll -side bottom -fill x
pack .vscroll -side right -fill y
pack $c -fill both -expand yes
pack .menu.file -side left
pack .menu.selection -side left
pack .menu.waveforms -side left


proc min_max {obj_list} {
    global c
    set xmin 999999999
    set xmax -999999999
    set ymin 999999999
    set ymax -999999999
    foreach obj $obj_list {
	set b_box [$c bbox $obj]
	set x1 [lindex $b_box 0]
	set y1 [lindex $b_box 1]
	set x2 [lindex $b_box 2]
	set y2 [lindex $b_box 3]
	if {$x1 < $xmin} {
	    set xmin $x1
	}
	if {$x2 > $xmax} {
	    set xmax $x2
	}
	if {$y1 < $ymin} {
	    set ymin $y1
	}
	if {$y2 > $ymax} {
	    set ymax $y2
	}
    }
    return [list $xmin $ymin $xmax $ymax]
}

proc move_all {obj_list xdelta ydelta} {
    global c
    foreach obj $obj_list {
	$c move $obj $xdelta $ydelta
    }
}

proc grow_bbox {bb} {
    set x1 [lindex $bb 0]
    set y1 [lindex $bb 1]
    set x2 [lindex $bb 2]
    set y2 [lindex $bb 3]
    return [list [expr $x1-10] [expr $y1-50] [expr $x2] [expr $y2]]
}

proc re_draw {} {
    global c top_node top_levels done fullwindow \
		max_window_width max_window_height
    catch {unset done}
    $c delete all
    draw_fanincone $top_node $top_levels
    draw_output $top_node
    set size [$c bbox all]
    set x1 [lindex $size 0]
    set y1 [lindex $size 1]
    set x2 [lindex $size 2]
    set y2 [lindex $size 3]
    set xsize [expr $x2-$x1+30]
    set ysize [expr $y2-$y1+100]
    set is_scrolled 0
    if {$fullwindow == 0} {
	if {$xsize > $max_window_width} {
	    set xsize $max_window_width
	    set is_scrolled 1
	}
	if {$ysize > $max_window_height} {
	    set ysize $max_window_height
	    set is_scrolled 1
	}
    }
    if { $xsize < 300 } {
	set xsize 300
    }
    if { $ysize < 300 } {
	set ysize 300
    }
    wm geometry . [join [list = $xsize x $ysize +50+10] ""]
    $c config -scrollregion [grow_bbox $size]
    if {$is_scrolled > 0} {
	$c xview moveto 0.9
	$c yview moveto 0.4
    }
}

proc draw_fanincone {node level} {
    global c EP deltax wire_len done min_sep
    set type_and_faninl [fl_type_and_faninlist $node]
    set type [lindex $type_and_faninl 0]
    switch $type {
	draw_input {
		# An input 
		return [draw_input $EP $node]
	}
    }
    if {[catch {set ans $done($node)}] == 0} {
	# A repeat
	return [draw_repeat_nd $EP $node]
    }
    if {$level == 0} {
	# An incomplete
	return [draw_incomplete $EP $node]
    }
    # Standard gate
    set done($node) "y"
    set finl [lindex $type_and_faninl 1]
    set inputs [llength $finl]
    set cur [$type $inputs $EP $node]
    set b_box [min_max $cur]
    set xl [lindex $b_box 0]
    set yb [lindex $b_box 1]
    set xr [lindex $b_box 2]
    set yt [lindex $b_box 3]
    set gwid [expr $xr-$xl]
    set ght  [expr $yt-$yb]
    set nlevel [expr $level-1]
    set i 0
    set ret ""
    set totht 0
    foreach igate $finl {
	set stree [draw_fanincone $igate $nlevel]
	set strees($i) $stree
	append ret " " $stree
	set sb_box [min_max $stree]
	set sy1 [lindex $sb_box 1]
	set sy2 [lindex $sb_box 3]
	set sght  [expr $sy2 - $sy1]
	if {$sght < $deltax} {
	    set sght $deltax
	}
	set hts($i) $sght
	set totht [expr $totht+$sght]
	incr i
    }
    set xdelta [expr -1*$gwid-($wire_len+($inputs+1)*$min_sep)]
    set ydelta [expr -1*$totht/2]
    for {set i 0} {$i < $inputs} {incr i} {
	set yy [expr $ydelta+$hts($i)/2]
	move_all $strees($i) $xdelta $yy
	set inp_tag [$c gettags [lindex $strees($i) 0]]
	append ret " " [connect [list [expr [lindex $EP 0]+$xdelta] \
				  [expr [lindex $EP 1]+$yy]] \
			    $inp_tag [expr $i+1] $node]
	set ydelta [expr $ydelta+$hts($i)]
    }
    set ret $cur$ret
    return $ret
}

proc draw_fanin {node levels names values} {
    global c top_node top_levels shownames showvalues fullwindow EP
    wm title . [concat "Input cone for node:" $node]
    set top_node "$node"
    set top_levels $levels
    set shownames $names
    set showvalues $values
    set fullwindow 0
    set EP {500 500}	;# Final output position
    re_draw
}

$c bind name_label <Button-3> {
        set curX %x
        set curY %y
}

$c bind name_label <B3-Motion> {
	$c move [$c find withtag current] [expr %x-$curX] [expr %y-$curY]
        set curX %x
        set curY %y
}

$c bind name_label <Button-2> {
	$c delete [$c find withtag current]
}

$c bind RePeAt <Button-1> {
 	global c
 	set tag [lindex [$c gettags current] 0]
	if {$tag != {current}} {
	    $c itemconfigure $tag -fill red
	}
	break
}

$c bind all <Button-1> {
 	global c
 	set tag [lindex [$c gettags current] 0]
	if {$tag != {current}} {
	    select $tag
	}
#	break
}

$c bind all <Control-1> {
 	global c top_levels shownames showvalues
 	set tag [lindex [$c gettags current] 0]
	if {$tag != {current}} {
	    fl_expand [safe_name $tag] $top_levels $shownames $showvalues
	}
	break
}

$c bind all <Shift-Button-1> {
 	global c
 	set tag [lindex [$c gettags current] 0]
	if {$tag != {current}} {
	    deselect $tag
	}
	break
}

$c bind all <Enter> {
 	global c
 	set tag [lindex [$c gettags current] 0]
	if {$tag != {current}} {
	    post_name $tag
	}
}

$c bind all <Leave> {
 	global c
 	set tag [lindex [$c gettags current] 0]
	if {$tag != {current}} {
	    unpost_name
	}
}
