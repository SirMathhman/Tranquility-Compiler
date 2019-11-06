%token VAR FUN IF ELSE UNTIL LOOP RETURN
%token <s> ID ICONST LBRACE RBRACE LPAREN RPAREN
%token <s> ASGN NL COMMA
%token <s> CHAR STRING
%token <s> CALL
%left <s> AND OR XOR
%left <s> EQ NEQ LT LEQ GT GEQ
%left <s> PLUS MINUS
%left <s> TIMES SLASH MOD
%left <s> LSHIFT RSHIFT
%right <s> DOT NOT
%type <e> expr expr_list expr_list_e
%type <st> stmt_list stmt if_stmt if_rest else_part
%type <f> fun_list fun_decl
%type <il> var_list id_list id_list_e
%start program
%{
#include <stdio.h>
#include <errno.h>

typedef struct enode enode;
typedef struct stnode stnode;
typedef struct idlist idlist;
typedef struct func func;

struct enode {
	int op;
	char *val;
	idlist *sym;
	int fnum;
	int lineno;
	enode *left, *right, *next;
};

struct stnode {
	int op;
	int lineno;
	enode *e1, *e2;
	stnode *st1, *st2;
	stnode *next;
};

struct idlist {
	char *id;
	char scope;
	int addr;
	idlist *next;
};

struct func {
	char *name;
	int fnum, narg, nloc;
	idlist *args, *locals;
	stnode *body;
	func *next;
};

typedef union {
	char *s;
	enode *e;
	stnode *st;
	func *f;
	idlist *il;
} YYSTYPE;

enode *mkenode(int, char *, enode *, enode *, enode *);
void generate(idlist *, func *);
stnode *mkstnode(int, enode *, enode *, stnode *, stnode *, stnode *);
void idappend(idlist *, idlist *);
void yyerror(char *);
int yylex(void);

%}
%%
program	:	var_list fun_list
			{ generate($1, $2); }
		;
var_list	:	/* epsilon */
			{ $$ = NULL; }
		|	VAR id_list NL var_list
			{ $$ = $2;
			  idappend($2, $4);
			}
		|	VAR error NL var_list
			{ yyerror("Syntax error in variable declaration"); $$ = $4; }
		;
id_list	:	ID
			{ $$ = malloc(sizeof(idlist));
			  $$->id = $1;
			  $$->next = NULL;
			}
		|	ID COMMA id_list
			{ $$ = malloc(sizeof(idlist));
			  $$->id = $1;
			  $$->next = $3;
			}
		;
id_list_e	:	/* epsilon */
			{ $$ = NULL; }
		|	id_list
		;
fun_list	:	fun_decl
		|	fun_decl fun_list
			{ $1->next = $2;
			  $$ = $1;
			}
		|	fun_decl NL fun_list
			{ $1->next = $3;
			  $$ = $1;
			}
		;
fun_decl	:	FUN ID LPAREN id_list_e RPAREN LBRACE NL var_list stmt_list RBRACE NL
			{ $$ = malloc(sizeof(func));
			  $$->name = $2;
			  $$->args = $4;
			  $$->locals = $8;
			  $$->body = $9;
			  $$->next = NULL;
			}
		|	FUN error NL
			{ yyerror("Invalid function declaration"); $$ = NULL; }
		;
stmt_list	:	/* epsilon */
			{ $$ = NULL; }
		|	NL stmt_list
			{ $$ = $2; }
		|	stmt stmt_list
			{ $1->next = $2;
			  $$ = $1;
			}
		;
stmt		:	expr ASGN expr NL
			{ $$ = mkstnode(ASGN, $1, $3, NULL, NULL, NULL); }
		|	error ASGN expr NL
			{
				yyerror("Invalid expression on left side of assignment");
				$$ = mkstnode(ASGN, NULL, $3, NULL, NULL, NULL);
			}
		|	expr ASGN error NL
			{
				yyerror("Invalid expression on right side of assignment");
				$$ = mkstnode(ASGN, $1, NULL, NULL, NULL, NULL);
			}
		|	expr NL
			{ $$ = mkstnode(CALL, $1, NULL, NULL, NULL, NULL); }
		|	if_stmt
		|	UNTIL expr NL
			{ $$ = mkstnode(UNTIL, $2, NULL, NULL, NULL, NULL); }
		|	UNTIL error NL
			{
				yyerror("Invalid expression in until");
				$$ = mkstnode(UNTIL, NULL, NULL, NULL, NULL, NULL);
			}
		|	LOOP LBRACE NL stmt_list RBRACE NL
			{ $$ = mkstnode(LOOP, NULL, NULL, $4, NULL, NULL); }
		|	LOOP { yyerror("Bad brace placement"); } NL LBRACE stmt_list RBRACE NL
			{
				$$ = mkstnode(LOOP, NULL, NULL, $5, NULL, NULL);
			}
		|	LOOP error NL
			{
				yyerror("Bad loop syntax");
				$$ = mkstnode(LOOP, NULL, NULL, NULL, NULL, NULL);
			}
		|	RETURN NL
			{ $$ = mkstnode(RETURN, NULL, NULL, NULL, NULL, NULL); }
		|	RETURN expr NL
			{ $$ = mkstnode(RETURN, $2, NULL, NULL, NULL, NULL); }
		|	RETURN error NL
			{
				yyerror("Invalid expression in return");
				$$ = mkstnode(RETURN, NULL, NULL, NULL, NULL, NULL);
			}
		|	error NL
			{
				yyerror("Unknown statement");
				$$ = mkstnode(RETURN, NULL, NULL, NULL, NULL, NULL);
			}
		;
if_rest	:	stmt_list RBRACE NL
			{ $$ = $1; }
		;
else_part	:	/* epsilon */
			{ $$ = NULL; }
		|	ELSE LBRACE NL stmt_list RBRACE NL
			{ $$ = $4; }
		|	ELSE if_stmt
			{ $$ = $2; }
		;
if_stmt	:	IF expr LBRACE NL if_rest else_part
			{ $$ = mkstnode(IF, $2, NULL, $5, $6, NULL); }
		|	IF expr NL LBRACE { yyerror("Bad brace placement"); } if_rest else_part
			{
				$$ = mkstnode(IF, $2, NULL, $6, $7, NULL);
			}
		|	IF error NL
			{
				yyerror("Bad if syntax");
				$$ = mkstnode(IF, NULL, NULL, NULL, NULL, NULL);
			}
		;
expr_list_e	:	/* epsilon */
			{ $$ = NULL; }
		|	expr_list
		;
expr_list	:	expr
		|	expr COMMA expr_list
			{ $1->next = $3;
			  $$ = $1;
			}
		;
expr		:	ICONST
			{ $$ = mkenode(ICONST, $1, NULL, NULL, NULL); }
		|	CHAR
			{ $$ = mkenode(CHAR, $1, NULL, NULL, NULL); }
		|	STRING
			{ $$ = mkenode(STRING, $1, NULL, NULL, NULL); }
		|	ID
			{ $$ = mkenode(ID, $1, NULL, NULL, NULL); }
		|	ID LPAREN expr_list_e RPAREN
			{ $$ = mkenode(CALL, $1, $3, NULL, NULL); }
		|	ID LPAREN error RPAREN
			{
				yyerror("Argument list syntax error");
				$$ = mkenode(ID, $1, NULL, NULL, NULL);
			}
		|	DOT expr
			{ $$ = mkenode(DOT, NULL, NULL, $2, NULL); }
		|	DOT error
			{
				yyerror("Invalid dot expression");
				$$ = mkenode(DOT, NULL, NULL, NULL, NULL);
			}
		|	LPAREN expr RPAREN
			{ $$ = $2; }
		|	LPAREN error RPAREN
			{ yyerror("Expression syntax error"); $$ = NULL; }
		|	MINUS expr	%prec DOT
			{ $$ = mkenode(MINUS, NULL, NULL, $2, NULL); }
		|	MINUS error
			{ yyerror("Invalid negation expression"); $$ = NULL; }
		|	NOT expr
			{ $$ = mkenode(NOT, NULL, NULL, $2, NULL); }
		|	NOT error
			{ yyerror("Invalid complement expression"); $$ = NULL; }
		|	expr PLUS expr
			{ $$ = mkenode(PLUS, NULL, $1, $3, NULL); }
		|	expr PLUS error
			{ yyerror("Invalid expresson on right side of plus"); $$ = $1; }
		|	expr MINUS expr
			{ $$ = mkenode(MINUS, NULL, $1, $3, NULL); }
		|	expr MINUS error
			{ yyerror("Invalid expression on right side of minus"); $$ = $1; }
		|	expr TIMES expr
			{ $$ = mkenode(TIMES, NULL, $1, $3, NULL); }
		|	expr TIMES error
			{ yyerror("Invalid expression on right side of times"); $$ = $1; }
		|	expr SLASH expr
			{ $$ = mkenode(SLASH, NULL, $1, $3, NULL); }
		|	expr SLASH error
			{ yyerror("Invalid expression on right side of divide"); $$ = $1; }
		|	expr AND expr
			{ $$ = mkenode(AND, NULL, $1, $3, NULL); }
		|	expr AND error
			{ yyerror("Invalid expression on right side of and"); $$ = $1; }
		|	expr OR expr
			{ $$ = mkenode(OR, NULL, $1, $3, NULL); }
		|	expr OR error
			{ yyerror("Invalid expression on right side of or"); $$ = $1; }
		|	expr XOR expr
			{ $$ = mkenode(XOR, NULL, $1, $3, NULL); }
		|	expr XOR error
			{ yyerror("Invalid expression on right side of xor"); $$ = $1; }
		|	expr LSHIFT expr
			{ $$ = mkenode(LSHIFT, NULL, $1, $3, NULL); }
		|	expr LSHIFT error
			{ yyerror("Invalid expression on right side of shift"); $$ = $1; }
		|	expr RSHIFT expr
			{ $$ = mkenode(RSHIFT, NULL, $1, $3, NULL); }
		|	expr RSHIFT error
			{ yyerror("Invalid expression on right side of shift"); $$ = $1; }
		|	expr MOD expr
			{ $$ = mkenode(MOD, NULL, $1, $3, NULL); }
		|	expr MOD error
			{ yyerror("Invalid expression on right side of mod"); $$ = $1; }
		|	expr EQ expr
			{ $$ = mkenode(EQ, NULL, $1, $3, NULL); }
		|	expr EQ error
			{ yyerror("Invalid expression on right side of eq"); $$ = $1; }
		|	expr NEQ expr
			{ $$ = mkenode(NEQ, NULL, $1, $3, NULL); }
		|	expr NEQ error
			{ yyerror("Invalid expression on right side of neq"); $$ = $1; }
		|	expr LT expr
			{ $$ = mkenode(LT, NULL, $1, $3, NULL); }
		|	expr LT error
			{ yyerror("Invalid expression on right side of lt"); $$ = $1; }
		|	expr LEQ expr
			{ $$ = mkenode(LEQ, NULL, $1, $3, NULL); }
		|	expr LEQ error
			{ yyerror("Invalid expression on right side of leq"); $$ = $1; }
		|	expr GT expr
			{ $$ = mkenode(GT, NULL, $1, $3, NULL); }
		|	expr GT error
			{ yyerror("Invalid expression on right side of gt"); $$ = $1; }
		|	expr	GEQ expr
			{ $$ = mkenode(GEQ, NULL, $1, $3, NULL); }
		|	expr GEQ error
			{ yyerror("Invalid expression on right side of geq"); $$ = $1; }
		;
%%
#include "lex.yy.c"

enum {
	Push = 1,
	Fetch,
	Store,
	If,
	Loop,
	Break,
	Return,
	Call,
	FPplus,
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Not,
	And,
	Or,
	Xor,
	Eq,
	Neq,
	Lt,
	Leq,
	Gt,
	Geq,
	Pop,
	Lshift,
	Rshift,
};

void ejson(FILE *, enode *, int);

int vflag;
int founderror;
char *src;
int addr;

void
yyerror(char *msg) {
	founderror = 1;
	fprintf(stderr, "%s:%d: %s\n", src, yylineno, msg);
}

enode *
mkenode(int op, char *val, enode *left, enode *right, enode *next) {
	enode *e;

	e = malloc(sizeof(enode));
	e->op = op;
	e->val = val;
	e->left = left;
	e->right = right;
	e->next = next;
	e->lineno = yylineno;
	return e;
}

stnode *
mkstnode(int op, enode *e1, enode *e2, stnode *st1, stnode *st2, stnode *next) {
	stnode *s;

	s = malloc(sizeof(stnode));
	s->op = op;
	s->e1 = e1;
	s->e2 = e2;
	s->st1 = st1;
	s->st2 = st2;
	s->next = next;
	s->lineno = yylineno - 1;
	return s;
}

void
idappend(idlist *l1, idlist *l2) {
	idlist *t;

	if(l1 == NULL)
		return;
	for(t = l1; t->next != NULL; t = t->next) ;
	t->next = l2;
}

void
prtexpr(enode *e) {
	enode *e2;

	switch(e->op) {
	case ICONST:
		printf("%s(I) ", e->val);
		break;
	case CHAR:
		printf("%s(C) ", e->val);
		break;
	case STRING:
		printf("\"%s\"(S) ", e->val);
		break;
	case ID:
		if(e->sym != NULL) {
			printf("0x%x(%s:%c) ", e->sym->addr, e->val, e->sym->scope);
		}
		else if(e->fnum >= 0) {
			printf("%d(%s) ", e->fnum, e->val);
		}
		else {
			printf("?(%s:%c) ", e->val, e->sym->scope);
		}
		break;
	case CALL:
		for(e2 = e->left; e2 != NULL; e2 = e2->next) {
			prtexpr(e2);
		}
		if(e->fnum != -99) {
			printf("%d(%s) CALL ", e->fnum, e->val);
		}
		else {
			printf("?(%s) CALL ", e->val);
		}
		break;
	case DOT:
		prtexpr(e->right);
		printf("@ ");
		break;
	case MINUS:
		if(e->left != NULL) {
			prtexpr(e->left);
		}
		else {
			printf("0 ");
		}
		prtexpr(e->right);
		printf("- ");
		break;
	case NOT:
		prtexpr(e->right);
		printf("~ ");
		break;
	case PLUS:
		prtexpr(e->left);
		prtexpr(e->right);
		printf("+ ");
		break;
	case TIMES:
		prtexpr(e->left);
		prtexpr(e->right);
		printf("* ");
		break;
	case SLASH:
		prtexpr(e->left);
		prtexpr(e->right);
		printf("/ ");
		break;
	case AND:
		prtexpr(e->left);
		prtexpr(e->right);
		printf("& ");
		break;
	case OR:
		prtexpr(e->left);
		prtexpr(e->right);
		printf("| ");
		break;
	case XOR:
		prtexpr(e->left);
		prtexpr(e->right);
		printf("^ ");
		break;
	case LSHIFT:
		prtexpr(e->left);
		prtexpr(e->right);
		printf("<< ");
		break;
	case RSHIFT:
		prtexpr(e->left);
		prtexpr(e->right);
		printf(">> ");
		break;
	case MOD:
		prtexpr(e->left);
		prtexpr(e->right);
		printf("%% ");
		break;
	case EQ:
		prtexpr(e->left);
		prtexpr(e->right);
		printf("== ");
		break;
	case NEQ:
		prtexpr(e->left);
		prtexpr(e->right);
		printf("!= ");
		break;
	case LT:
		prtexpr(e->left);
		prtexpr(e->right);
		printf("< ");
		break;
	case LEQ:
		prtexpr(e->left);
		prtexpr(e->right);
		printf("<= ");
		break;
	case GT:
		prtexpr(e->left);
		prtexpr(e->right);
		printf("> ");
		break;
	case GEQ:
		prtexpr(e->left);
		prtexpr(e->right);
		printf(">= ");
		break;
	}
}

void
indent(int level) {
	int i;

	for(i = 0; i < level; i++) {
		printf("   ");
	}
}

void
traverse(stnode *s, int level) {
	stnode *s2;

	switch(s->op) {
	case ASGN:
		indent(level);
		prtexpr(s->e1);
		prtexpr(s->e2);
		printf("!\n");
		break;
	case IF:
		indent(level);
		prtexpr(s->e1);
		printf("IF [\n");
		for(s2 = s->st1; s2 != NULL; s2 = s2->next) {
			traverse(s2, level + 1);
		}
		indent(level);
		printf("]\n");
		if(s->st2 != NULL) {
			indent(level);
			printf("ELSE [\n");
			for(s2 = s->st2; s2 != NULL; s2 = s2->next) {
				traverse(s2, level + 1);
			}
			indent(level);
			printf("]\n");
		}
		break;
	case UNTIL:
		indent(level);
		prtexpr(s->e1);
		printf("BREAK\n");
		break;
	case LOOP:
		indent(level);
		printf("[\n");
		for(s2 = s->st1; s2 != NULL; s2 = s2->next) {
			traverse(s2, level + 1);
		}
		indent(level);
		printf("]GO\n");
		break;
	case RETURN:
		indent(level);
		if(s->e1 != NULL) {
			prtexpr(s->e1);
		}
		printf("RETURN\n");
		break;
	case CALL:
		indent(level);
		prtexpr(s->e1);
		printf(" POP\n");
		break;
	}
}

idlist *
lookup(char *sym, idlist *glist, func *flist, func *f) {
	idlist *v;
	func *f2;

	for(v = f->locals; v != NULL && strcmp(v->id, sym) != 0; v = v->next) ;
	if(v != NULL)
		return v;
	for(v = f->args; v != NULL && strcmp(v->id, sym) != 0; v = v->next) ;
	if(v != NULL)
		return v;
	for(v = glist; v != NULL && strcmp(v->id, sym) != 0; v = v->next) ;
	return v;
}

void
eresolve(enode *e, idlist *glist, func *f, func *flist) {
	func *f2;

	if(e == NULL)
		return;
	if(e->op == ID) {
		e->fnum = -1;
		e->sym = lookup(e->val, glist, flist, f);
		if(e->sym == NULL) {
			for(f2 = flist; f2 != NULL && strcmp(e->val, f2->name) != 0; f2 = f2->next) ;
			if(f2 != NULL)
				e->fnum = f2->fnum;
			else {
				founderror = 1;
				fprintf(stderr, "%s:%d: unresolved symbol %s\n", src, e->lineno, e->val);
			}
		}
	}
	else if(e->op == CALL) {
		for(f2 = flist; f2 != NULL && strcmp(e->val, f2->name) != 0; f2 = f2->next) ;
		if(f2 != NULL) {
			e->fnum = f2->fnum;
		}
		else {
			if(strcmp(e->val, "iprint") == 0)
				e->fnum = -1;
			else if(strcmp(e->val, "sprint") == 0)
				e->fnum = -2;
			else if(strcmp(e->val, "iread") == 0)
				e->fnum = -3;
			else if(strcmp(e->val, "sread") == 0)
				e->fnum = -4;
			else if(strcmp(e->val, "nl") == 0)
				e->fnum = -5;
			else if(strcmp(e->val, "random") == 0)
				e->fnum = -10;
			else if(strcmp(e->val, "timer") == 0)
				e->fnum = -11;
			else if(strcmp(e->val, "stoptimer") == 0)
				e->fnum = -12;
			else if(strcmp(e->val, "makeimg") == 0)
				e->fnum = -13;
			else if(strcmp(e->val, "setimg") == 0)
				e->fnum = -14;
			else if(strcmp(e->val, "button") == 0)
				e->fnum = -15;
			else if(strcmp(e->val, "html") == 0)
				e->fnum = -16;
			else if(strcmp(e->val, "makelabel") == 0)
				e->fnum = -17;
			else if(strcmp(e->val, "setlabel") == 0)
				e->fnum = -18;
			else if(strcmp(e->val, "alloc") == 0)
				e->fnum = -19;
			else if(strcmp(e->val, "free") == 0)
				e->fnum = -20;
			else if(strcmp(e->val, "i2s") == 0)
				e->fnum = -21;
			else {
				e->fnum = -99;
				founderror = 1;
				fprintf(stderr, "%s:%d: unresolved symbol %s\n", src, e->lineno, e->val);
			}
		}
	}
	eresolve(e->left, glist, f, flist);
	eresolve(e->right, glist, f, flist);
	eresolve(e->next, glist, f, flist);
}

void
unescape(char *s) {
	char *p, *q;

	p = s;
	q = s;
	while(*p != '\0') {
		if(*p != '\\') {
			*q++ = *p++;
		}
		else {
			p++;
			switch(*p) {
			case 'b':
				*q++ = '\b';
				break;
			case 'n':
				*q++ = '\n';
				break;
			case 'r':
				*q++ = '\r';
				break;
			case 't':
				*q++ = '\t';
				break;
			case '\\':
				*q++ = '\\';
				break;
			default:
				*q++ = *p;
			}
			p++;
		}
	}
	*q = '\0';
}

idlist *
doestrings(enode *e, idlist *gstrings) {
	idlist *ns;

	if(e == NULL)
		return gstrings;
	if(e->op == STRING) {
		unescape(e->val);
		e->fnum = addr;
		addr += strlen(e->val) + 1;
		ns = malloc(sizeof(idlist));
		ns->id = e->val;
		ns->scope = 'S';
		ns->addr = e->fnum;
		ns->next = gstrings;
		gstrings = ns;
	}
	gstrings = doestrings(e->left, gstrings);
	gstrings = doestrings(e->right, gstrings);
	gstrings = doestrings(e->next, gstrings);
	return gstrings;
}

idlist *
dostrings(stnode *st, idlist *gstrings) {
	if(st == NULL)
		return gstrings;
	gstrings = doestrings(st->e1, gstrings);
	gstrings = doestrings(st->e2, gstrings);
	gstrings = dostrings(st->st1, gstrings);
	gstrings = dostrings(st->st2, gstrings);
	gstrings = dostrings(st->next, gstrings);
	return gstrings;
}

void
stresolve(stnode *st, idlist *glist, func *f, func *flist) {
	stnode *s2;

	if(st == NULL)
		return;
	eresolve(st->e1, glist, f, flist);
	eresolve(st->e2, glist, f, flist);
	for(s2 = st->st1; s2 != NULL; s2 = s2->next) {
		stresolve(s2, glist, f, flist);
	}
	for(s2 = st->st2; s2 != NULL; s2 = s2->next) {
		stresolve(s2, glist, f, flist);
	}
}

idlist *
assign(idlist *glist, func *flist) {
	idlist *v, *gstrings;
	func *f;
	stnode *st;
	int fnum, n, faddr;

	addr = 0;
	fnum = 0;
	for(v = glist; v != NULL; v = v->next) {
		v->scope = 'G';
		v->addr = addr++;
	}
	gstrings = NULL;
	for(f = flist; f != NULL; f = f->next) {
		gstrings = dostrings(f->body, gstrings);
	}
	for(f = flist; f != NULL; f = f->next) {
		f->fnum = fnum++;
		faddr = 1;
		n = 0;
		for(v = f->locals; v != NULL; v = v->next) {
			v->scope = 'L';
			v->addr = faddr++;
			n++;
		}
		f->nloc = n;
		n = 0;
		for(v = f->args; v != NULL; v = v->next) {
			v->scope = 'L';
			v->addr = faddr++;
			n++;
		}
		f->narg = n;
	}
	for(f = flist; f != NULL; f = f->next) {
		for(st = f->body; st != NULL; st = st->next) {
			stresolve(st, glist, f, flist);
		}
	}
	return gstrings;
}

void
revejson(FILE *jfp, enode *e) {
	if(e == NULL)
		return;
	revejson(jfp, e->next);
	ejson(jfp, e, 0);
	fprintf(jfp, ",");
}

void
ejson(FILE *jfp, enode *e, int comma) {
	enode *e2;
	char *eptr;
	long x;

	if(e == NULL)
		return;
	if(comma)
		fprintf(jfp, ",");
	switch(e->op) {
	case ICONST:
		x = strtol(e->val, &eptr, 0);
		if(*eptr == '\0') {
			fprintf(jfp, "%d,%ld", Push, x);
		}
		else {
			founderror = 1;
			fprintf(stderr, "%s:%d: Invalid integer constant %s\n", src, e->lineno, e->val);
		}
		break;
	case CHAR:
		if(e->val[0] == '\\') {
			switch(e->val[1]) {
			case 'b':
				fprintf(jfp, "%d,%d", Push, '\b');
				break;
			case 'n':
				fprintf(jfp, "%d,%d", Push, '\n');
				break;
			case 'r':
				fprintf(jfp, "%d,%d", Push, '\r');
				break;
			case 't':
				fprintf(jfp, "%d,%d", Push, '\t');
				break;
			case '\\':
				fprintf(jfp, "%d,%d", Push, '\\');
				break;
			default:
				fprintf(jfp, "%d,%d", Push, e->val[1]);
			}
		}
		else {
			fprintf(jfp, "%d,%d", Push, e->val[1]);
		}
		break;
	case STRING:
		fprintf(jfp, "%d,%d", Push, e->fnum);
		break;
	case ID:
		if(e->sym != NULL) {
			fprintf(jfp, "%d,%d", Push, e->sym->addr);
			if(e->sym->scope == 'L') {
				fprintf(jfp, ",%d", FPplus);
			}
		} else if(e->fnum >= 0) {
			fprintf(jfp, "%d,%d", Push, e->fnum);
		}
		break;
	case CALL:
		x = 0;
		for(e2 = e->left; e2 != NULL; e2 = e2->next)
			x++;
		revejson(jfp, e->left);
		fprintf(jfp, "%d,%d", Call, e->fnum);
		break;
	case DOT:
		ejson(jfp, e->right, 0);
		fprintf(jfp, ",%d", Fetch);
		break;
	case MINUS:
		if(e->left != NULL) {
			ejson(jfp, e->left, 0);
		}
		else {
			fprintf(jfp, "0");
		}
		ejson(jfp, e->right, 1);
		fprintf(jfp, ",%d", Sub);
		break;
	case NOT:
		ejson(jfp, e->right, 0);
		fprintf(jfp, ",%d", Not);
		break;
	case PLUS:
		ejson(jfp, e->left, 0);
		ejson(jfp, e->right, 1);
		fprintf(jfp,",%d", Add);
		break;
	case TIMES:
		ejson(jfp, e->left, 0);
		ejson(jfp, e->right, 1);
		fprintf(jfp,",%d", Mul);
		break;
	case SLASH:
		ejson(jfp, e->left, 0);
		ejson(jfp, e->right, 1);
		fprintf(jfp,",%d", Div);
		break;
	case AND:
		ejson(jfp, e->left, 0);
		ejson(jfp, e->right, 1);
		fprintf(jfp,",%d", And);
		break;
	case OR:
		ejson(jfp, e->left, 0);
		ejson(jfp, e->right, 1);
		fprintf(jfp,",%d", Or);
		break;
	case XOR:
		ejson(jfp, e->left, 0);
		ejson(jfp, e->right, 1);
		fprintf(jfp,",%d", Xor);
		break;
	case LSHIFT:
		ejson(jfp, e->left, 0);
		ejson(jfp, e->right, 1);
		fprintf(jfp, ",%d", Lshift);
		break;
	case RSHIFT:
		ejson(jfp, e->left, 0);
		ejson(jfp, e->right, 1);
		fprintf(jfp, ",%d", Rshift);
		break;
	case MOD:
		ejson(jfp, e->left, 0);
		ejson(jfp, e->right, 1);
		fprintf(jfp,",%d", Mod);
		break;
	case EQ:
		ejson(jfp, e->left, 0);
		ejson(jfp, e->right, 1);
		fprintf(jfp,",%d", Eq);
		break;
	case NEQ:
		ejson(jfp, e->left, 0);
		ejson(jfp, e->right, 1);
		fprintf(jfp,",%d", Neq);
		break;
	case LT:
		ejson(jfp, e->left, 0);
		ejson(jfp, e->right, 1);
		fprintf(jfp,",%d", Lt);
		break;
	case LEQ:
		ejson(jfp, e->left, 0);
		ejson(jfp, e->right, 1);
		fprintf(jfp,",%d", Leq);
		break;
	case GT:
		ejson(jfp, e->left, 0);
		ejson(jfp, e->right, 1);
		fprintf(jfp,",%d", Gt);
		break;
	case GEQ:
		ejson(jfp, e->left, 0);
		ejson(jfp, e->right, 1);
		fprintf(jfp,",%d", Geq);
		break;
	}
}

void stjson(FILE *jfp, stnode *st, int comma) {
	stnode *s2;

	if(comma)
		fprintf(jfp, ",");
	switch(st->op) {
	case ASGN:
		ejson(jfp, st->e1, 0);
		ejson(jfp, st->e2, 1);
		fprintf(jfp, ",%d", Store);
		break;
	case IF:
		ejson(jfp, st->e1, 0);
		fprintf(jfp, ",%d,[", If);
		for(s2 = st->st1; s2 != NULL; s2 = s2->next) {
			stjson(jfp, s2, 0);
			if(s2->next != NULL)
				fprintf(jfp, ",");
		}
		fprintf(jfp, "],[");
		for(s2 = st->st2; s2 != NULL; s2 = s2->next) {
			stjson(jfp, s2, 0);
			if(s2->next != NULL)
				fprintf(jfp, ",");
		}
		fprintf(jfp, "]");
		break;
	case UNTIL:
		ejson(jfp, st->e1, 0);
		fprintf(jfp, ",%d", Break);
		break;
	case LOOP:
		fprintf(jfp, "%d,[", Loop);
		s2 = st->st1;
		while(1) {
			stjson(jfp, s2, 0);
			s2 = s2->next;
			if(s2 == NULL)
				break;
			fprintf(jfp, ",");
		}
		fprintf(jfp, "]");
		break;
	case RETURN:
		if(st->e1 != NULL) {
			ejson(jfp, st->e1, 0);
		}
		else {
			fprintf(jfp, "%d,0", Push);
		}
		fprintf(jfp, ",%d", Return);
		break;
	case CALL:
		ejson(jfp, st->e1, 0);
		fprintf(jfp, ",%d", Pop);
		break;
	}
}

void
dojson(idlist *glist, func *flist, idlist *gstrings) {
	FILE *jfp;
	idlist *v;
	stnode *st;
	func *f;
	char *p, *dpos, *outname;
	int n, comma;

	dpos = strrchr(src, '.');
	if(dpos != NULL) {
		*dpos = '\0';
	}
	n = strlen(src) + 6;
	outname = malloc(n);
	snprintf(outname, n, "%s.json", src);
	jfp = fopen(outname, "w");
	fprintf(jfp, "var tape='[");
	for(f = flist; f != NULL && strcmp(f->name, "init") != 0; f = f->next) ;
	if(f == NULL) {
		fprintf(stderr, "No init function found\n");
		fclose(jfp);
		return;
	}
	fprintf(jfp, "[%d,%d],", f->fnum, addr);
	fprintf(jfp, "[[65535,0]");
	for(v = glist; v != NULL; v = v->next) {
		fprintf(jfp, ",[%d,0]", v->addr);
	}
	for(v = gstrings; v != NULL; v = v->next) {
		n = v->addr;
		for(p = v->id; *p != '\0'; p++) {
			fprintf(jfp, ",[%d,%d]", n++, *p);
		}
		fprintf(jfp, ",[%d,0]", n);
	}
	fprintf(jfp, "],\\\n");
	f = flist;
	while(1) {
		fprintf(jfp, "[%d,\"%s\",%d,%d,[", f->fnum, f->name, f->narg, f->nloc);
		st = f->body;
		while(1) {
			stjson(jfp, st, 0);
			st = st->next;
			if(st == NULL)
				break;
			fprintf(jfp, ",");
		}
		f = f->next;
		if(f != NULL) {
			fprintf(jfp, ",%d,0]],\\\n", Push);
		}
		else {
			fprintf(jfp, ",%d,0]]", Push);
			break;
		}
	}
	fprintf(jfp, "]'\n");
	fclose(jfp);
}

void
generate(idlist *glist, func *flist) {
	idlist *v, *gstrings;
	func *f;
	stnode *st;

	gstrings = assign(glist, flist);
	if(vflag) {
		printf("Globals:\n");
		for(v = glist; v != NULL; v = v->next) {
			printf("%s:0x%x ", v->id, v->addr);
		}
		printf("\n\nFunctions:\n");
		for(f = flist; f != NULL; f = f->next) {
			printf("%d %s na:%d nl:%d:  ", f->fnum, f->name, f->narg, f->nloc);
			printf("args: ");
			for(v = f->args; v != NULL; v = v->next) {
				printf("%s ", v->id);
			}
			printf("  local: ");
			for(v = f->locals; v != NULL; v = v->next) {
				printf("%s ", v->id);
			}
			printf("\n");
			for(st = f->body; st != NULL; st = st->next) {
				traverse(st, 1);
			}
		}
	}
	if(!founderror)
		dojson(glist, flist, gstrings);
}

void
usage(void) {
	fprintf(stderr, "Usage: tranqc [-v] source\n");
	exit(1);
}

int
main(int argc, char *argv[]) {
	FILE *fp;
	int n;

	if(argc < 2 || strcmp(argv[1], "-h") == 0) {
		usage();
	}
	n = 1;
	if(strcmp(argv[1], "-v") == 0) {
		vflag = 1;
		n++;
		if(argc < n+1) {
			usage();
		}
	}
	src = argv[n];
	fp = fopen(argv[n], "r");
	if(fp == NULL) {
		fprintf(stderr, "tranqc: %s: %s\n", argv[n], strerror(errno));
		exit(1);
	}
	yyset_in(fp);
	return(yyparse());
}
