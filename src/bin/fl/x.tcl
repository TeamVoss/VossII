toplevel .foo
label .foo.l -text "Hiiiii000000"
button .foo.b -text Ok -command {destroy .foo}
pack .foo.l
pack .foo.b
update
