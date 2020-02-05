#
DOCS =  fl_guide.tex 02_fl.tex 03_bv.tex 04_hfl.tex \
	05_ste.tex \
	06_fl-syntax.tex 07_built-in-fl.tex \
	08_vossrc.tex 09_standard-library.tex \
	16_biblio.tex

FIGS = figs/forte_start.jpg figs/forte_with_args_start.jpg figs/forte_help.jpg

all: fl_guide.pdf fl_tutorial.html

fl_guide.pdf: $(DOCS) $(FIGS)
	touch glossary.tex
	pdflatex fl_guide.tex
	sed -e "s/ }/}/" fl_guide.idx \
	| sed -e "s/\\\\unhbox \\\\voidb@x \\\\hbox//g" > fl_guide.idx2
	makeindex -o glossary.tex -s my_index_style fl_guide.idx2
	pdflatex fl_guide.tex
	pdflatex fl_guide.tex

fl_tutorial.html: fl_tutorial.md $(FIGS) tut_img/*.jpg
	@-mkdir -p tut_code
	@-gawk -v code_cnt=`ls tut_code | egrep .fl$ | wc -l` -f tut_pre_process.awk < $< > fl_tutorial_tmp.md
	pandoc -i fl_tutorial_tmp.md -o fl_tutorial_tmp.html
	awk '/<\/main>/{while(getline line<"fl_tutorial_tmp.html"){print line}} //' fl_tutorial_template.html > fl_tutorial.html

preview: fl_guide.pdf
	xpdf fl_guide.pdf


install: fl_guide.pdf
	/bin/cp fl_guide.pdf ../../../doc/fl_guide.pdf

%.tex: %.pre_tex
	@-mkdir -p code
	@-gawk -v code_cnt=`ls code/*.fl | wc -l` -f pre_process.awk < $< > $@


figs/forte_start.jpg:
	@-mkdir -p figs
	wish ./full_window_capture figs/forte_start.jpg

figs/forte_with_args_start.jpg:
	@-mkdir -p figs
	wish ./full_window_capture figs/forte_with_args_start.jpg We must do 123 situps

figs/forte_help.jpg:
	@-mkdir -p figs
	wish ./full_window_capture figs/forte_help.jpg --help "*AND*" 0

clean:
	-@rm -f fl_guide.pdf $(DOCS)
	-@rm -f fl_tutorial_tmp.md fl_tutorial_tmp.html fl_tutorial.html
	-@rm -rf code
	-@rm -rf tut_code
	-@rm -rf figs
	-@rm -f fl_guide.aux
	-@rm -f fl_guide.ilg
	-@rm -f fl_guide.idx
	-@rm -f fl_guide.idx2
	-@rm -f fl_guide.lof
	-@rm -f fl_guide.log
	-@rm -f fl_guide.lot
	-@rm -f fl_guide.toc
	-@rm -f glossary.tex
	-@rm -f fl_guide.out
	-@rm -f fl_guide.ind
