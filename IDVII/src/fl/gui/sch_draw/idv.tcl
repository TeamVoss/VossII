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
    pack $nb -side top -expand y -fill both
    set ::sch_window_cnt($w) 0
}


proc idv:create_idv_menu {nb w} {

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
        balloon $w.menu.fev "Run FEV"
        pack $w.menu.fev -side left -padx 5

}

proc idv:fold {w} {
    set npw .idv_name_prompt
    catch {destroy $npw}
    set ::idv_prompt_name ""
    toplevel $npw
	frame $npw.t -relief flat
	pack $npw.t -side top
	    label $npw.t.l -text "Name of folded hierarchy: "
	    ttk::entry $npw.t.e -textvariable ::idv_prompt_name -width 20
	    bind $npw.t.e <KeyPress-Return> "destroy $npw"
	    pack $npw.t.l -side left
	    pack $npw.t.e -side left -fill x -expand yes
	frame $npw.b -relief flat
	pack $npw.b -side top
	    button $npw.b.cancel -text Cancel \
		-command "set ::idv_prompt_name {}; destroy $npw"
	    frame $npw.b.sep -relief flat
	    button $npw.b.ok -text Ok -command "destroy $npw"
	    pack $npw.b.cancel -side left -fill x
	    pack $npw.b.sep -side left -fill x -expand yes
	    pack $npw.b.ok -side left -fill x
    tkwait window $npw
    fl_do_fold $w.c $::idv_prompt_name
}

proc idv:unfold {w} { fl_do_unfold $w.c }

proc idv:flatten {w} { fl_do_flatten $w.c }

proc idv:duplicate {w} { fl_do_duplicate $w.c }

proc idv:merge {w} { fl_do_merge $w.c }

proc idv:new_transf {w} { fl_do_new_tranf $w.c }
