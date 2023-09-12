
template: ../strings.h ../fsm.h ../table.h mk_serialize.awk
	cat ../strings.h ../fsm.h ../table.h | gawk -f mk_serialize.awk > template

clean:
	@-/bin/rm template

