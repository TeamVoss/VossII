
# set ::scaling_factor 1.0
# set ::base_sc     0.85
# source /home/cseger/VossII/bin/utils.tcl
# set ::imagedir /home/cseger/VossII/IDVII/src/fl/gui/sch_draw/images
# load /home/cseger/VossII/IDVII/schematic_draw_module.so
# source /home/cseger/VossII/IDVII/src/fl/gui/sch_draw/draw_sch.tcl
# source /home/cseger/VossII/IDVII/src/fl/gui/sch_draw/waveform.tcl
# source /home/cseger/VossII/IDVII/src/fl/gui/sch_draw/idv_gui.tcl


proc new_draw_circuit_canvas {w final_res} {
    set c $w.c
    scrollbar $w.hscroll -orient horiz -command [list $w.c xview]
    pack $w.hscroll -side bottom -fill x
    scrollbar $w.vscroll -command [list $w.c yview]
    pack $w.vscroll -side right -fill y
    canvas $c -background white  -xscrollcommand [list $w.hscroll set] \
				 -yscrollcommand [list $w.vscroll set]  \
				 -xscrollincrement 1 \
				 -yscrollincrement 1 -width 1000 -height 800
    pack $c -fill both -expand yes

    bind $c <2> [list %W scan mark %x %y]
    bind $c <B2-Motion> [list %W scan dragto %x %y]

    set ::cur_zoom_factor($c) 100.0
    set ::sc($c)	    $::base_sc
    set ::tfont($c)	    $::base_tfont
    set ::mfont($c)	    $::base_mfont
    set ::sfont($c)	    $::base_sfont

    # Zoom bindings
    bind $c <ButtonPress-3> "zoom_lock %W %x %y"
    bind $c <B3-Motion> "zoom_move %W %x %y"
    bind $c <ButtonRelease-3>  "zoom_execute %W %x %y %X %Y cb:sch_canvas_menu"

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
	    catch {unset ::old_repeat_color}
	    foreach t $taglist {
		lappend ::old_repeat_color "$t [%W itemcget $t -fill]"
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
	    foreach tcpair $::old_repeat_color {
		lappend ::old_repeat_color [%W itemcget $t -fill]
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

    set_zoom_factor $c 100.0
    $c delete all
    draw_network $c $final_res
    update
}
