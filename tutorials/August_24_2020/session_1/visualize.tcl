set scale 2
set wcnt 0

set w_width 1024
set w_height 256

proc gen_draw_canvas {c name x y ll} {
    global scale
    if { $name != "" } {
	$c create text [expr $x+$::w_width*$scale/2]\
		       [expr $y+$::w_height*$scale+1*$scale] \
		       -text $name -anchor n -justify left
    } else {
    }
    set row 0
    set x [expr $x+0*$scale]
    set y [expr $y+0*$scale]
    foreach l $ll {
	set col 0
	foreach p $l {
	    if { $p != "" } {
		set x1 [expr $x+$col*$scale]
		set y1 [expr $y+$row*$scale]
		set x2 [expr $x+($col+1)*$scale]
		set y2 [expr $y+($row+1)*$scale]
		set b [$c create rectangle $x1 $y1 $x2 $y2 -fill $p -outline $p]
		$c bind $b <ButtonPress-1> [list display_color $p]
	    }
	    incr col
	}
	incr row
    }
}

set ::color_display_cnt 0

proc display_color {color} {
    incr ::color_display_cnt
    set w .color_$::color_display_cnt
    toplevel $w
    label $w.l -text "Color is: $color"
    button $w.b -text Ok -command [list destroy $w]
    pack $w.l -side top -fill x -expand yes
    pack $w.b -side top -fill x -expand yes
}

proc create_example_canvas {image_w image_h rows columns} {
    global scale
    set ::w_width $image_w
    set ::w_height $image_h
#    set ::pw_width [expr $::w_width + 50]
#    set ::pw_height [expr $::w_height + 40]
 set ::pw_width [expr $::w_width + 30]
 set ::pw_height [expr $::w_height + 20]
    set w [format {.c%d} $::wcnt]
    incr ::wcnt
    catch {destroy $w}
    toplevel $w
    set ::display(rows,$w.c) $rows
    set ::display(columns,$w.c) $columns

    scrollbar $w.hscroll -orient horiz -command "$w.c xview"
    pack $w.hscroll -side bottom -fill x
    scrollbar $w.vscroll -command "$w.c yview"
    pack $w.vscroll -side right -fill y

    set wh [expr min(700,$rows*$::pw_height*$scale)]

    canvas $w.c -background white -width [expr $columns*$::pw_width*$scale] \
				  -height $wh \
				  -cursor crosshair \
				  -xscrollcommand [list $w.hscroll set] \
                                  -yscrollcommand [list $w.vscroll set] \
                                  -xscrollincrement 1 \
                                  -yscrollincrement 1

    pack $w.c -side top -padx 10 -pady 10 -fill both -expand yes
    return $w.c
}

proc draw_example {c name cnt ll} {
    global scale
    set x [expr ($cnt % $::display(columns,$c))*$::pw_width*$scale]
    set y [expr ($cnt / $::display(columns,$c))*$::pw_height*$scale]
    gen_draw_canvas $c $name $x $y $ll
}
