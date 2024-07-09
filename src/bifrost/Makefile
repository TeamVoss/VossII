all:
	ghc --make Main.hs -o bifrost

install: all
	cp bifrost ../../bin/bifrost

grammar:
	-rm -rf Lang
	bnfc --haskell -p Lang Lang.cf
	happy -gca Lang/ParLang.y
	alex -g Lang/LexLang.x

# No display of executed commands.
$(VERBOSE).SILENT:

#testgrammar: grammar
#	runhaskell Lang/TestLang.hs < test/newtest.bfst

clean:
	-rm -f *.hi *.o
	-rm -f Lang/*.hi Lang/*.o Lang/*.x Lang/*.y
	-rm -f bifrost

distclean: clean
	-rm -f bifrost
	-find examples -name '*.bfst.fl' -delete
	-find examples -name '*.bfst.log' -delete
	-find examples -name '*.pexlif' -delete
