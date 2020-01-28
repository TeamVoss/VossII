#
userguide.ps: userguide.dvi
	dvips -o userguide.ps userguide.dvi

tex:
	@touch userguide.tex
	@make userguide.dvi
	@echo "userguide.dvi up-to-date"

userguide.dvi: userguide.tex
	latex userguide.tex

clean:
	-@rm userguide.ps
	-@rm userguide.dvi

glossary:
	sed -e "s/ }/}/" userguide.idx \
	| sed -e "s/\\\\unhbox \\\\voidb@x \\\\hbox//g" > userguide.idx2
	makeindex -o glossary.tex -s my_index_style userguide.idx2

install: userguide.ps
	cp userguide.ps /isd/local/generic/lib/voss/doc/userguide.ps
	chmod go+r /isd/local/generic/lib/voss/doc/userguide.ps
