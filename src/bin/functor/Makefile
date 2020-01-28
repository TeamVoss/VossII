
functor: lex.yy.c
	$(CC) $(DEBUG_FLAG) $(OPTIMIZATION_FLAG) $(LINKFLAG) -o functor lex.yy.c

lex.yy.c: lexer
	flex lexer

clean:
	@-/bin/rm -f lex.yy.c functor
