%%
var	{ return VAR; }
fun	{ return FUN; }
if	{ return IF; }
else	{ return ELSE; }
until	{ return UNTIL; }
loop	{ return LOOP; }
return	{ return RETURN; }
"{"	{ return LBRACE; }
"}"	{ return RBRACE; }
"("	{ return LPAREN; }
")"	{ return RPAREN; }
	/* "="	{ return ASGN; } */
":"	{ return ASGN; }
"\n"+	{ yylineno += strlen(yytext); return NL; }
","	{ return COMMA; }
"."	{ return DOT; }
"-"	{ return MINUS; }
"~"	{ return NOT; }
"+"	{ return PLUS; }
"*"	{ return TIMES; }
"/"	{ return SLASH; }
"&"	{ return AND; }
"|"	{ return OR; }
"^"	{ return XOR; }
"%"	{ return MOD; }
"=="	{ return EQ; }
"!="	{ return NEQ; }
"<"	{ return LT; }
"<="	{ return LEQ; }
">"	{ return GT; }
">="	{ return GEQ; }
"<<"	{ return LSHIFT; }
">>"	{ return RSHIFT; }
\'.\'	{ yylval.s = strdup(yytext); return CHAR; }
\'\\.\'	{ yylval.s = strdup(yytext); return CHAR; }
[A-Za-z_][0-9A-Za-z_]*	{ yylval.s = strdup(yytext); return ID; }
[0-9]+	{ yylval.s = strdup(yytext); return ICONST; }
0x[0-9a-f]+	{ yylval.s = strdup(yytext); return ICONST; }
\"[^"]*	{ if(yytext[yyleng-1] == '\\')
			yymore();
		  else {
			input();
			yylval.s = strdup(yytext+1);
			return STRING;
		  }
		}
^[ \t]*\#[^\n]*"\n"	{ yylineno++; }
\#[^\n]*"\n"	{ yylineno++; return NL; }
[ \t]	;
%%
