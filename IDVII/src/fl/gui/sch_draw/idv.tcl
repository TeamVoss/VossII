;#########################################################################
;# Copyright 2020 Carl-Johan Seger
;# SPDX-License-Identifier: Apache-2.0
;#########################################################################

# ---------------------------------------------------
# Constants
# ---------------------------------------------------

proc idv:create_idv_gui {w} {
    catch {destroy $w}
    toplevel $w
    wm geometry $w -20+100
    set nb $w.nb
    ttk::notebook $nb -width 1200 -height 700
    bind $nb <<NotebookTabChanged>> [list idv:inform_canvas_change $w]
    pack $nb -side top -expand y -fill both
    set ::sch_window_cnt($w) 0
    bindtags $w top_level_idv_window
    bind top_level_idv_window <Destroy> [list fl_save_idv_db exit]
    $nb add [frame $nb.idv] -text "IDV Home"

    # Now make the front page
    $nb select 0
    set pw $nb.idv.pw
    panedwindow $pw -orient horizontal -showhandle yes
    pack $pw -side top -fill both -expand y
        frame $pw.model_browser
        $pw add $pw.model_browser -width 400
        frame $pw.tr_dag
        $pw add $pw.tr_dag
	set ww $pw.tr_dag
	set ::idv(front_page) $ww
	#
	# Canvas for transformations
	#
        set c $ww.c
	frame $ww.tf
	pack $ww.tf -side top -fill x
	    label $ww.tf.shn_lbl -text "Show model name"
	    set ::idv(show_model_name) 1
	    ttk::checkbutton $ww.tf.shn_cb -variable ::idv(show_model_name) \
		-command "idv:update_transf_canvas $c"
	    pack $ww.tf.shn_lbl -side left 
	    pack $ww.tf.shn_cb -side left 
	    frame $ww.tf.sp -width 10
	    pack $ww.tf.sp -side left
	    label $ww.tf.she_lbl -text "Show transformation name"
	    set ::idv(show_transform_name) 0
	    ttk::checkbutton $ww.tf.she_cb \
		-variable ::idv(show_transform_name) \
		-command "idv:update_transf_canvas $c"
	    pack $ww.tf.she_lbl -side left 
	    pack $ww.tf.she_cb -side left 
        scrollbar $ww.yscroll -command "$c yview"
        scrollbar $ww.xscroll -orient horizontal -command "$c xview"
        canvas $c -background white \
                -yscrollcommand "$ww.yscroll set" \
                -xscrollcommand "$ww.xscroll set"
        pack $ww.yscroll -side right -fill y
        pack $ww.xscroll -side bottom -fill x
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
	#
	# Model browser
	#
	set p $pw.model_browser
	set ::idv(model_browser) $p
	ttk::labelframe $p.search -text "Search:"
	    #
	    ttk::labelframe $p.search.lbl -relief flat -text \
		    "Select database: " \
		    -labelanchor w
	    set dbs [fl_idv_get_db_names $w.c]
	    ttk::combobox $p.search.lbl.c -textvariable \
		    ::modelbrowser(db) \
		-state readonly \
		-values $dbs \
		-font $::voss2_txtfont
	    set ::modelbrowser(db) [lindex $dbs 0]

	    #
	    ttk::labelframe $p.search.pat_lbl -relief flat -text "Pattern: " \
		    -labelanchor w
	    ttk::combobox $p.search.pat_lbl.c \
		    -textvariable ::modelbrowser(pattern) \
		    -font $::voss2_txtfont
	    bind $p.search.pat_lbl.c <KeyPress-Return> \
		    [list $p.search.refresh invoke]
	    set ::modelbrowser(pattern) {*}
	    ttk::button $p.search.refresh -text Refresh \
		    -command idv:update_idv_list
	pack $p.search -side top -pady 10 -fill x
	    pack $p.search.lbl -side top -fill x
		pack $p.search.lbl.c -side left -fill x -expand yes
	    pack $p.search.pat_lbl -side top -fill x
		pack $p.search.pat_lbl.c -side left -fill x -expand yes
	    pack $p.search.refresh -side top -fill x
	set f $p.lf
	set ::idv(front_page_listbox) $f.list
	frame $f -relief flat
	scrollbar $f.yscroll -command "$f.list yview"
	scrollbar $f.xscroll -orient horizontal -command "$f.list xview"
	listbox $f.list -setgrid 1 \
	    -yscroll "$f.yscroll set" -xscroll "$f.xscroll set" \
	    -selectmode single -font $::voss2_txtfont
	bind $f.list <<ListboxSelect>> "idv:display_transformations"
	pack $f.yscroll -side right -fill y
	pack $f.xscroll -side bottom -fill x
	pack $f.list -side top -fill both -expand yes
	pack $f -side top -fill both -expand yes
	set b $p.buttons
	frame $b -relief flat
	    button $b.quit -text Exit -command "destroy $w"
	    frame $b.sp1 -relief flat
	    button $b.save -text Save -command "fl_save_idv_db save"
	    frame $b.sp2 -relief flat
	    button $b.import -text "Import model" \
		    -command "idv:import_model $w"
	    frame $b.sp3 -relief flat
	    button $b.new -text "New transform" \
		    -command "idv:new_toplevel_transf $w $f.list"
	    pack $b.quit -side left -expand yes
	    pack $b.sp1 -side left -expand yes
	    pack $b.save -side left -expand yes
	    pack $b.sp2 -side left -expand yes
	    pack $b.import -side left -expand yes
	    pack $b.sp3 -side left -expand yes
	    pack $b.new -side left -expand yes
	pack $b -side top -fill x

	# Populate listbox (probably need to limit thenumber of models...)
	idv:update_idv_list
}

proc idv:inform_canvas_change {w} {
    set nb $w.nb
    set cur_idx [$nb index [$nb select]]
    if { $cur_idx == 0 } {
	after idle idv:display_transformations
    }
}


proc idv:display_transformations {} {
    if { ![info exists ::idv(front_page_listbox)] } { return } 
    set sl $::idv(front_page_listbox)
    set idx [$sl curselection]
    if { $idx != "" } {
	set cur [$sl get [$sl curselection]]
	set db $::modelbrowser(db)
	fl_display_transform_tree $::idv(front_page) $db $cur
    }
}

proc idv:update_idv_list {} {
    set w $::idv(model_browser)
    set lb $w.lf.list
    $lb delete 0 end
    set db $::modelbrowser(db)
    set pat $::modelbrowser(pattern)
    if { ![catch {fl_get_idv_models $w.c $db $pat} vecs] } {
	foreach v $vecs {
	    $lb insert end $v
	}
    }
}


proc idv:perform_model_save {w npw version} {
    if { $::idv_prompt_name == "" } {
	$npw.error.l configure -text "Must provide a name." -fg red
    } elseif { [fl_model_name_used $::idv_prompt_name] } {
	$npw.error.l configure -text "Name already in use!" -fg red
    } else {
	fl_do_name_model $w.c $::idv_prompt_name $version
	destroy $npw
    }
}

proc idv:name_and_save_model {w version {default ""} } {
    set npw .idv_name_prompt
    catch {destroy $npw}
    set ::idv_prompt_name $default

    vis_toplevel $npw $w {} {} "Name current $version model"
        frame $npw.t -relief flat
        pack $npw.t -side top -fill x
            label $npw.t.l -text "Name of $version model: "
            ttk::entry $npw.t.e -textvariable ::idv_prompt_name -width 20
            bind $npw.t.e <KeyPress-Return> \
		"idv:perform_model_save $w $npw $version"
            pack $npw.t.l -side left
            pack $npw.t.e -side left -fill x -expand yes
        frame $npw.error -relief flat
        pack $npw.error -side top -fill x
	    label $npw.error.l -text ""
	    pack $npw.error.l -side left -fill x

        frame $npw.b -relief flat
        pack $npw.b -side top -fill x
            button $npw.b.cancel -text Cancel -command "destroy $npw"
            frame $npw.b.sep -relief flat
            button $npw.b.ok -text Ok -command \
		"idv:perform_model_save $w $npw $version"
            pack $npw.b.cancel -side left -fill x
            pack $npw.b.sep -side left -fill x -expand yes
            pack $npw.b.ok -side left -fill x
}

proc idv:create_idv_menu {nb w} {

	ttk::menubutton $w.menu.file -text File -menu $w.menu.file.menu
        pack $w.menu.file -side left -padx 5
	set m $w.menu.file.menu
	menu $m
        $m add command -label "Name current model" \
            -command "idv:name_and_save_model $w implementation"
        $m add command -label "Name initial model" \
            -command "idv:name_and_save_model $w specification"


        button $w.menu.new_transf -image $::icon(new_transf) \
                -command "idv:new_transf $w"
        balloon $w.menu.new_transf \
		"Start new transformation sequence from selected instances"
        pack $w.menu.new_transf -side left -padx 5

        button $w.menu.fold -image $::icon(fold) \
                -command "idv:fold $w"
        balloon $w.menu.fold "Fold selected instances"
        pack $w.menu.fold -side left -padx 5

        button $w.menu.unfold -image $::icon(unfold) \
                -command "idv:unfold $w"
        balloon $w.menu.unfold "Unfold selected instance"
        pack $w.menu.unfold -side left -padx 5

        button $w.menu.flatten -image $::icon(flatten) \
                -command "idv:flatten $w"
        balloon $w.menu.flatten "Flatten model"
        pack $w.menu.flatten -side left -padx 5

        button $w.menu.duplicate -image $::icon(duplicate) \
                -command "idv:duplicate $w"
        balloon $w.menu.duplicate "Duplicate selected instance to every fanout"
        pack $w.menu.duplicate -side left -padx 5

        button $w.menu.merge -image $::icon(merge) \
                -command "idv:merge $w"
        balloon $w.menu.merge "Merge selected identical instances"
        pack $w.menu.merge -side left -padx 5

        button $w.menu.fev -image $::icon(fev) \
                -command "idv:fev $w"
        balloon $w.menu.fev "Replace with new design that has been FEV-ed"
        pack $w.menu.fev -side left -padx 5

}

proc idv:perform_fold {w npw} {
    fl_do_fold $w.c $::idv_prompt_name
    destroy $npw
}

proc idv:fold {w} {
    set npw .idv_name_prompt
    catch {destroy $npw}
    set ::idv_prompt_name ""

    vis_toplevel $npw $w {} {} "Fold name"
	frame $npw.t -relief flat
	pack $npw.t -side top
	    label $npw.t.l -text "Name of folded hierarchy: "
	    ttk::entry $npw.t.e -textvariable ::idv_prompt_name -width 20
	    bind $npw.t.e <KeyPress-Return> "idv:perform_fold $w $npw"
	    pack $npw.t.l -side left
	    pack $npw.t.e -side left -fill x -expand yes
	frame $npw.b -relief flat
	pack $npw.b -side top
	    button $npw.b.cancel -text Cancel -command "destroy $npw"
	    frame $npw.b.sep -relief flat
	    button $npw.b.ok -text Ok -command "idv:perform_fold $w $npw"
	    pack $npw.b.cancel -side left -fill x
	    pack $npw.b.sep -side left -fill x -expand yes
	    pack $npw.b.ok -side left -fill x
}

proc idv:unfold {w} { fl_do_unfold $w.c }

proc idv:flatten {w} { fl_do_flatten $w.c }

proc idv:duplicate {w} { fl_do_duplicate $w.c }

proc idv:merge {w} { fl_do_merge $w.c }

proc idv:new_transf {w} { fl_do_new_tranf $w.c }

proc idv:fev {w} {
    set types {
        {{pexlif}      {.pexlif}        }
        {{All Files}        *             }
    }
    set file [tk_getOpenFile -filetypes $types -defaultextension ".pexlif" \
                                 -title "Pexlif file to load"]
    if {$file ne ""} {
	fl_do_fev $w.c $file
    }
}

proc idv:new_toplevel_transf {w sl} {
    set idx [$sl curselection]
    if { $idx != "" } {
        set cur [$sl get [$sl curselection]]
        set db $::modelbrowser(db)
	fl_do_new_toplevel_transf $w $db $cur
    }
}   

proc idv:perform_name_transf {w c op} {
    set ::idv(transf_op) $op
    if { $op == "Cancel" } {
	destroy $w
	return
    }
    if { $::idv(transf_name) == "" } {
	$w.errors.l configure \
	    -text "Error: Must provide a name for the transformation" \
			-fg red
    } elseif { [fl_is_toplevel_transform $c] \
		&& $::idv(imp_name) == "" } {
	$w.errors.l configure \
	    -text "Error: Must provide a name for toplevel models" -fg red
    
    } else {
	destroy $w
    }
}

proc idv:name_transform_and_use {c} {
    set w .idv_name
    catch {destroy $w}
    i_am_busy
    vis_toplevel $w $c {} {} "Name transform"
    set toplevel_transf [fl_is_toplevel_transform $c]
    set ::idv(spec_name) ""
    set ::idv(imp_name) ""
    #
    if { $toplevel_transf == 0 } {
	frame $w.spec_name
	pack $w.spec_name -side top -fill x
	    label $w.spec_name.l -text "Optional name of specification: "
	    entry $w.spec_name.e -textvariable ::idv(spec_name) -width 30
	    pack $w.spec_name.e -side right
	    pack $w.spec_name.l -side left -fill x -anchor w
    }
    #
    frame $w.namef
    set ::idv(transf_name) ""
    pack $w.namef -side top -fill x
	label $w.namef.l -text "Name of transformation: "
	entry $w.namef.e -textvariable ::idv(transf_name) -width 30
	pack $w.namef.e -side right
	pack $w.namef.l -side left -fill x -anchor w

    #
    frame $w.imp_name
    pack $w.imp_name -side top -fill x
	if { $toplevel_transf } {
	    label $w.imp_name.l -text "Name of final model: "
	} else {
	    label $w.imp_name.l -text "Optional name of final model: "
	}
	entry $w.imp_name.e -textvariable ::idv(imp_name) -width 30
	pack $w.imp_name.e -side right
	pack $w.imp_name.l -side left -fill x -anchor w

    frame $w.buttons
    pack $w.buttons -side top
	button $w.buttons.cancel -text Cancel \
	    -command "idv:perform_name_transf $w $c Cancel"
	pack $w.buttons.cancel -side left -padx 10
	button $w.buttons.save -text Save \
	    -command "idv:perform_name_transf $w $c Save"
	pack $w.buttons.save -side left -padx 10
	# Only if started from a transformation window
	if { $toplevel_transf == 0 } {
	    button $w.buttons.appl1 -text "Save & Apply Once" \
		-command "idv:perform_name_transf $w $c SaveAndApplyOnce"
	    pack $w.buttons.appl1 -side left -padx 10
	    button $w.buttons.appln -text "Save & Apply Everywhere" \
		-command "idv:perform_name_transf $w $c SaveAndApplyEverywhere"
	    pack $w.buttons.appln -side left -padx 10
	}
    frame $w.errors
	label $w.errors.l -text ""
	pack $w.errors.l -side top -fill x
    pack $w.errors -side top

    tkwait window $w
    i_am_free
    if { $::idv(spec_name) == "" } {
	set ::idv(spec_name) "."
    }
    if { $::idv(imp_name) == "" } {
	set ::idv(imp_name) "."
    }
    return [list $::idv(transf_op) $::idv(transf_name) $::idv(imp_name)]
}

proc idv:update_transf_canvas {c} {
    foreach node [array names ::idv_transf_node_map] {
	if { $::idv(show_model_name) } {
	    regexp {([^:]*):(.*)} $::idv_transf_node_map($node) -> db model_name
	    $c itemconfigure $::dot_node2text_tag($c,$node) \
		    -text " $model_name" -anchor w
	} else {
	    $c itemconfigure $::dot_node2text_tag($c,$node) \
		    -text ""
	}
    }
    foreach edge [array names ::idv_transf_edge_map] {
	if { $::idv(show_transform_name) } {
	    regexp {([^:]*):(.*)} $::idv_transf_edge_map($edge) -> db edge_name
	    $c itemconfigure $::dot_edge2text_tag($c,$edge) \
		    -text $edge_name -anchor w
	} else {
	    $c itemconfigure $::dot_edge2text_tag($c,$edge) -text ""
	}
    }

}

proc idv:show_model_menu {c node x y} {
    set m $c.sm
    catch {destroy $m}
    menu $m -tearoff 0
    set full_name  $::idv_transf_node_map($node)
    set matches [regexp {([^:]*):(.*)} $full_name -> db model_name]
    $m add command -label "Rename model" \
	-command "fl_do_rename_model $c $db $model_name"
    $m add command -label "New transformation" \
	-command "fl_do_new_toplevel_transf [winfo toplevel $c] $db $model_name"
    tk_popup $m $x $y
}

proc idv:show_transformations {dot_file w} {
    set c $w.c
    display_dot $dot_file $w
    idv:update_transf_canvas $c

    foreach node [array names ::idv_transf_node_map] {
	set fig_tag $::dot_node2node_fig_tag($c,$node)
	$c bind $fig_tag <ButtonPress-3> \
	    "idv:show_model_menu $c $node %X %Y; break"
	$c itemconfigure $fig_tag -fill yellow
	set txt_tag $::dot_node2text_tag($c,$node)
	$c bind $txt_tag <ButtonPress-3> \
	    "idv:show_model_menu $c $node %X %Y; break"
    }
}

proc idv:import_model {w} {
    set types {
        {{pexlif}      {.pexlif}        }
        {{All Files}        *             }
    }
    set file [tk_getOpenFile -filetypes $types -defaultextension ".pexlif" \
                                 -title "Pexlif file to load"]
    if {$file ne ""} {
	fl_import_model $w $file
    }
}

proc idv:ask_for_model_name {w} {
    set npw .idv_name_prompt
    catch {destroy $npw}
    set ::idv_prompt_name ""

    vis_toplevel $npw $w {} {} "Name model"
        frame $npw.t -relief flat
        pack $npw.t -side top -fill x
            label $npw.t.l -text "Name of model: "
            ttk::entry $npw.t.e -textvariable ::idv_prompt_name -width 20
            bind $npw.t.e <KeyPress-Return> "destroy $npw"
            pack $npw.t.l -side left
            pack $npw.t.e -side left -fill x -expand yes
        frame $npw.error -relief flat
        pack $npw.error -side top -fill x
            label $npw.error.l -text ""
            pack $npw.error.l -side left -fill x

        frame $npw.b -relief flat
        pack $npw.b -side top -fill x
            button $npw.b.ok -text Ok -command "destroy $npw"
            pack $npw.b.ok -side left -fill x

    tkwait window $npw
    return $::idv_prompt_name
}
