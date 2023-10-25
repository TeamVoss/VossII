
template: ../voss_strings.h ../fsm.h ../table.h mk_serialize.awk
	cat ../voss_strings.h ../fsm.h ../table.h | gawk -f mk_serialize.awk > template

clean:
	@-/bin/rm template

