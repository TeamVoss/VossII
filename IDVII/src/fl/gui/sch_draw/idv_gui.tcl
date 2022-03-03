;#########################################################################
;# Copyright 2020 Carl-Johan Seger
;# SPDX-License-Identifier: Apache-2.0
;#########################################################################

# ---------------------------------------------------
# Constants
# ---------------------------------------------------

proc idv:create_idv_gui {w rw_db} {
    catch {destroy $w}
    toplevel $w
    wm geometry $w -20+100
    set nb $w.nb
    set ::idv(code_dir) "$rw_db/code"
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

	    ttk::labelframe $p.search.visible -relief flat -text \
		    "Model class: " \
		    -labelanchor w
	    tk_optionMenu $p.search.visible.ch ::modelbrowser(visible) \
		    Imported "User given" All
	    set ::modelbrowser(visible) Imported

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
	    pack $p.search.visible -side top -fill x
		pack $p.search.visible.ch -side left -fill x -expand yes
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
    set cur_sel [$nb select]
    set cur_idx [$nb index $cur_sel]
    if { $cur_idx == 0 } {
	after idle idv:display_transformations
    } else {
	fl_set_current_tab_selected $w $cur_sel 
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
    set vis $::modelbrowser(visible)
    if { ![catch {fl_get_idv_models $w.c $db $vis $pat} vecs] } {
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
        $m add command -label "Write out pexlif model" \
            -command "idv:write_pexlif $w"
        $m add command -label "Write out Verilog model" \
            -command "idv:write_verilog $w 0"
        $m add command -label "Write out Verilog netlist" \
            -command "idv:write_verilog $w 1"

        button $w.menu.new_transf -image $::icon(new_transf) \
                -command "idv:new_transf $w"
        balloon $w.menu.new_transf \
		"Start new transformation sequence from selected instances"
        pack $w.menu.new_transf -side left -padx 5

        button $w.menu.db_replace -image $::icon(db_replace) \
                -command "idv:db_replace $w"
        balloon $w.menu.db_replace \
		"Find and apply a database transformation"
        pack $w.menu.db_replace -side left -padx 5

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

        button $w.menu.rename_w -image $::icon(rename_wires) \
                -command "idv:do_rename_wires $w"
        balloon $w.menu.rename_w "Rename selected wires"
        pack $w.menu.rename_w -side left -padx 5

        button $w.menu.fev -image $::icon(fev) \
                -command "idv:do_fev $w"
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

proc idv:db_replace {w} { fl_do_replacement $w.c }

proc idv:new_toplevel_transf {w sl} {
    set idx [$sl curselection]
    if { $idx != "" } {
        set cur [$sl get [$sl curselection]]
        set db $::modelbrowser(db)
	fl_do_new_toplevel_transf $w $db $cur
    }
}   

proc idv:return_select_replacement {w op} {
    set ::idv(replacement_command) $op
    set slb $w.lf.list
    if { $op == "Cancel" } { 
	set ::idv(replacement_idx) -1
    } else {
	set ::idv(replacement_idx) [$slb curselection]
    }
    destroy $w
}

proc idv:select_replacement {c alts} {
    set w .select_transf
    catch {destroy $w}
    vis_toplevel $w $c {} {} "Select transform"
    set f $w.lf
    frame $f -relief flat
    scrollbar $f.yscroll -command "$f.list yview"
    scrollbar $f.xscroll -orient horizontal -command "$f.list xview"
    listbox $f.list -setgrid 1 \
	-yscroll "$f.yscroll set" -xscroll "$f.xscroll set" \
	-selectmode single -font $::voss2_txtfont
    pack $f.yscroll -side right -fill y
    pack $f.xscroll -side bottom -fill x
    pack $f.list -side top -fill both -expand yes
    pack $f -side top -fill both -expand yes

    set b $w.buttons
    frame $b
    pack $b -side top
	button $b.cancel -text Cancel \
	    -command "idv:return_select_replacement $w Cancel"
	pack $b.cancel -side left -padx 10
	button $b.appl1 -text "Apply Once" \
	    -command "idv:return_select_replacement $w ApplyOnce"
	pack $b.appl1 -side left -padx 10
	button $b.appln -text "Apply Everywhere" \
	    -command "idv:return_select_replacement $w ApplyEverywhere"
	pack $b.appln -side left -padx 10
	button $b.repappln -text "Repeatedly Apply Everywhere" \
	    -command "idv:return_select_replacement $w RepeatApplyEverywhere"
	pack $b.repappln -side left -padx 10
    #
    foreach nm $alts {
	$f.list insert end $nm
    }


    tkwait window $w
    return [list $::idv(replacement_command) $::idv(replacement_idx)]
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

proc idv:name_transform_and_use {inside c tr_name model_name} {
    set w .idv_name
    catch {destroy $w}
    i_am_busy
    vis_toplevel $w $c {} {} "Name and apply transform"
    set toplevel_transf [fl_is_toplevel_transform $c]
    set ::idv(transf_name) $tr_name
    set ::idv(imp_name) $model_name
    #
    frame $w.namef
    pack $w.namef -side top -fill x
	label $w.namef.l -text "Name of transformation: "
	entry $w.namef.e -textvariable ::idv(transf_name) -width 30
	pack $w.namef.e -side right
	pack $w.namef.l -side left -fill x -anchor w
    #
    frame $w.imp_name
    pack $w.imp_name -side top -fill x
	label $w.imp_name.l -text "Name of final model: "
	entry $w.imp_name.e -textvariable ::idv(imp_name) -width 30
	pack $w.imp_name.e -side right
	pack $w.imp_name.l -side left -fill x -anchor w
    #
    frame $w.buttons
    pack $w.buttons -side top
	button $w.buttons.cancel -text Cancel \
	    -command "idv:perform_name_transf $w $c Cancel"
	pack $w.buttons.cancel -side left -padx 10
	button $w.buttons.discard -text Discard \
	    -command "idv:perform_name_transf $w $c Discard"
	pack $w.buttons.discard -side left -padx 10
	button $w.buttons.save -text Save \
	    -command "idv:perform_name_transf $w $c Save"
	pack $w.buttons.save -side left -padx 10
	# Only if started from a transformation window
	if { $toplevel_transf == 0 } {
	    if { $inside == "1" } {
		button $w.buttons.appl1 -text "Save & Apply" \
		 -command "idv:perform_name_transf $w $c SaveAndApplyOnce"
		pack $w.buttons.appl1 -side left -padx 10
	    } else {
		button $w.buttons.appl1 -text "Save & Apply Once" \
		 -command "idv:perform_name_transf $w $c SaveAndApplyOnce"
		pack $w.buttons.appl1 -side left -padx 10
		button $w.buttons.appln -text "Save & Apply Everywhere" \
		 -command "idv:perform_name_transf $w $c SaveAndApplyEverywhere"
		pack $w.buttons.appln -side left -padx 10
	    }
	}
    frame $w.errors
	label $w.errors.l -text ""
	pack $w.errors.l -side top -fill x
    pack $w.errors -side top
    tkwait window $w
    i_am_free
    return [list $::idv(transf_op) $::idv(transf_name) $::idv(imp_name)]
}

proc idv:update_transf_canvas {c} {
    foreach node [array names ::idv_transf_node_map] {
	if { $::idv(show_model_name) } {
	    regexp {([^:]*):(.*)} $::idv_transf_node_map($node) -> db model_name
	    $c itemconfigure $::dot_node2text_tag($c,$node) \
		    -text " $model_name" -anchor c
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
;#    $m add command -label "Rename model" \
;#	-command "fl_do_rename_model $c $db $model_name"
    $m add command -label "New transformation" \
	-command "fl_do_new_toplevel_transf [winfo toplevel $c] $db [list $model_name]"
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
	$c itemconfigure $fig_tag -outline white
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
                                 -title "Pexlif file to load" -parent $w]
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

proc idv:edit_and_load {w file load_file pexlif_file type} {
    set ew .editor
    while 1 {
        catch {destroy $ew}
	if { $type != "synthesis" } {
	    toplevel $ew -container 1 -width 580 -height 564
	    set x [winfo x $w]
	    set y [winfo y $w]
	    wm geometry $ew +$x+$y
	    update idletasks
	    set wid [expr [winfo id $ew]]
	    set edit "/usr/bin/X11/xterm -into $wid -sb -sl 20000 -j \
		      -rw -ls -bg white -fg black -fn 7x14 -geometry 80x40 \
		      -e /usr/bin/vi $file"
	    # Edit the file
	    util:bg_exec $edit 0 {} $w
	    # Try compile the model
	    set tmp_dir [fl_mktempd "loading"]
	    set res [util:try_program $w "Loading failed" \
			      {{Cancel cancel} {{Re-edit} again}} \
			      fl -noX -unbuf_stdout -T $tmp_dir -F $load_file]
	    switch $res {
		ok  {
			destroy $ew;
			return $pexlif_file;
		    }
		cancel {
			destroy $ew;
			return ""
		    }
		again {}
	    }
	} else {
	    fl_run_fl_loader $load_file
	    return $pexlif_file;
	}
    }
}



proc idv:make_template {w c type} {
    set ::idv(fev_imp_file) ""
    set file $::idv(fev_template_file)
    set base [file tail [file rootname $file]]
    set ext [file extension $file]
    if { $file != "" } {
	switch $type {
	    verilog	{ set file "$base.v" }
	    hfl		{ set file "$base.fl" }
	    synthesis	{ set file "$base" }
	    default	{}
	}
	if { ![regexp {^/.*} $file] && ![regexp {\.\./.*} $file] } {
	    set file "$::idv(code_dir)/$file"
	}
	set create_file 1
	if { [file exists $file] } {
	    set reply [tk_messageBox \
			    -message "File exists. Overwrite it?:" \
			    -icon question -default no -type yesnocancel \
			    -parent $w]
	    switch $reply {
		yes	{ set create_file 1;  }
		no	{ set create_file 0; }
		cancel  {return}
	    }
	}
	val {pexlif_file load_file} \
		[fl_make_template $c $type $file $base $create_file]
	set ::idv(fev_imp_file) \
		[idv:edit_and_load $w $file $load_file $pexlif_file $type]
    }
}

proc idv:do_bdd_var_order {} {
    set pexlif_file $::idv(fev_imp_file) 
    fl_bdd_var_order $pexlif_file
}

proc idv:do_verify {w canvas type} {
    set pexlif_file $::idv(fev_imp_file) 
    if { ![file exists $pexlif_file] } { return; }
    set res [fl_do_verify $canvas $pexlif_file $type]
    if { $res == "ok" } {
	destroy $w
	set ::idv(fev_template_file) ""
	set ::idv(fev_imp_file) ""
    } elseif { $res == "cex" } {
	tk_messageBox \
	    -message "Verification failed.\nCounterexample simulated." \
	    -type ok -parent $w
    } else {
	tk_messageBox \
	    -message "Verification failed.\n$res." -type ok -parent $w
    }
}

proc idv:do_fev {ww} {
    set w .fev
    catch {destroy $w}
    vis_toplevel $w $ww {} {} "IDV FEV"

    frame $w.f1 -relief flat 
    pack $w.f1 -side top -fill x -pady 10
        label $w.f1.l -text FEV -font $::voss2_txtfont6
        button $w.f1.cancel -text Cancel -command "destroy $w"
        pack $w.f1.cancel -side right -padx 10
        pack $w.f1.l -side left -fill x


    labelframe $w.f2 -relief groove -text Template
    pack $w.f2 -side top -fill x -pady 10
	label $w.f2.l -text "Make template in:" -width 20 -justify left \
	    -anchor w
	entry $w.f2.e -textvariable ::idv(fev_template_file) -width 30
	button $w.f2.dir -image $::icon(folder) \
	    -command "idv:fev_template_file $::idv(code_dir) $w $ww.c"
	button $w.f2.verilog -text "Verilog" \
	    -command "idv:make_template $w $ww.c verilog"
	button $w.f2.hfl -text "HFL" \
	    -command "idv:make_template $w $ww.c hfl"
	button $w.f2.synth -text "Synthesis" \
	    -command "idv:make_template $w $ww.c synthesis"
	pack $w.f2.l -side left -anchor w
	pack $w.f2.e -side left -fill x -expand yes
	pack $w.f2.dir -side left
	pack $w.f2.verilog -side left -padx 5
	pack $w.f2.hfl -side left -padx 5
	pack $w.f2.synth -side left -padx 5

    labelframe $w.f3 -relief groove -text Implementation
    pack $w.f3 -side top -fill x -pady 10
	label $w.f3.l -text "Load pexlif:" -width 20 -justify left \
	    -anchor w
	entry $w.f3.e -textvariable ::idv(fev_imp_file) -width 30
	button $w.f3.dir -image $::icon(folder) -command "idv:fev_pexlif $w"
	pack $w.f3.l -side left -anchor w -anchor w
	pack $w.f3.e -side left -fill x -expand yes
	pack $w.f3.dir -side left

    labelframe $w.f4 -relief groove -text Check
    pack $w.f4 -side top -fill x -pady 10
	labelframe $w.f4.sim -relief groove -text Simulation
	    button $w.f4.sim.light -text "Light" \
		-command "idv:do_verify $w $ww.c Sim_Light"
	    button $w.f4.sim.medium -text "Medium" \
		-command "idv:do_verify $w $ww.c Sim_Medium"
	    button $w.f4.sim.heavy -text "Heavy" \
		-command "idv:do_verify $w $ww.c Sim_Heavy"
	pack $w.f4.sim -side left -padx 10 -fill x -expand yes
	    pack $w.f4.sim.light -side left -padx 5 -fill x -expand yes
	    pack $w.f4.sim.medium -side left -padx 5 -fill x -expand yes
	    pack $w.f4.sim.heavy -side left -padx 5 -fill x -expand yes

    labelframe $w.f5 -relief groove -text Verify
    pack $w.f5 -side top -fill x -pady 10
	labelframe $w.f5.bdd -relief groove -text BDD
	    button $w.f5.bdd.order -text "Variable ordering" \
		-command idv:do_bdd_var_order
	    button $w.f5.bdd.verify -text "Verify" \
		-command "idv:do_verify $w $ww.c BDD"
	labelframe $w.f5.sat -relief groove -text SAT
	    button $w.f5.sat.verify -text "Verify" \
		-command "idv:do_verify $w $ww.c SAT"
	pack $w.f5.bdd -side left -padx 10 -fill x -expand yes
	pack $w.f5.sat -side left -padx 10 -fill x -expand yes
	    pack $w.f5.bdd.order -side left -padx 5
	    pack $w.f5.bdd.verify -side left -padx 5 -fill x -expand yes
	    pack $w.f5.sat.verify -side left -padx 5 -fill x -expand yes

}

proc idv:fev_template_file {code_dir w c} {
    set types {
        {{All Files}        *             }
    }
    set file [tk_getSaveFile -filetypes $types \
		-initialdir $code_dir -title "Template file to load" \
	        -confirmoverwrite 0]
    set ::idv(fev_template_file) $file
    if [file exists $file] {
	if { [file extension $file] == ".fl" } {
	    idv:make_template $w $c hfl
	} elseif { [file extension $file] == ".v" } {
	    idv:make_template $w $c verilog
	}
    }
}

proc idv:fev_pexlif {w} {
    set types {
        {{pexlif}      {.pexlif}        }
        {{All Files}        *             }
    }
    set file [tk_getOpenFile -filetypes $types -defaultextension ".pexlif" \
                                 -title "Pexlif file to load" -parent $w]
    set ::idv(fev_imp_file) $file
}

proc idv:do_rename_wires {w} { fl_rename_wires $w.c }

proc done_edit_proc {args} { }

proc idv:write_verilog {w only_netlist} {
    set types {
        {{Verilog}      {.v}        }
        {{All Files}     *          }
    }
    set file [tk_getSaveFile -filetypes $types \
	    -initialdir $::idv(code_dir) -title "Name file to write Verilog" \
	    -confirmoverwrite 1]
    if { $file != "" } {
	set name [file rootname [file tail $file]]
	fl_save_verilog $w.c $name $file $only_netlist
    }
}

proc idv:write_pexlif {w} {
    set types {
        {{pexlif}      {.fl}        }
        {{All Files}     *          }
    }
    set file [tk_getSaveFile -filetypes $types \
	    -initialdir $::idv(code_dir) -title "Name file to write pexlif" \
	    -confirmoverwrite 1]
    if { $file != "" } {
	set name [file rootname [file tail $file]]
	fl_save_pexlif $w.c $file
    }
}
