tranqc: y.tab.c lex.yy.c
	cc -o tranqc y.tab.c -ll

y.tab.c: tranq.y
	yacc -v tranq.y

lex.yy.c: tranq.l
	lex tranq.l
