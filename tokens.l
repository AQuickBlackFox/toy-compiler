%{
#include <string>
#include "node.h"
#include "parser.hpp"

#define SAVE_TOKEN  yylval.string = new std::string(yytext, yyleng)
#define TOKEN(t)    (yylval.token = t)
%}

%option noyywrap

%%

[ \t\n]					        ;
"extern"                return TOKEN(TEXTERN);
"return"	        return TOKEN(TRETURN);
"double"                return TOKEN(TDOUBLEPRIM);
"__global__"            return TOKEN(TGLOBAL);
"__device__"            return TOKEN(TDEVICE);
"void"                  return TOKEN(TVOID);
"threadIdx.x"           SAVE_TOKEN; return TTIDX;
"threadIdx.y"           SAVE_TOKEN; return TTIDY;
"threadIdx.z"           SAVE_TOKEN; return TTIDZ;
"blockIdx.x"            SAVE_TOKEN; return TBIDX;
"blockIdx.y"            SAVE_TOKEN; return TBIDY;
"blockIdx.z"            SAVE_TOKEN; return TBIDZ;
[a-zA-Z_][a-zA-Z0-9_]*  SAVE_TOKEN; return TIDENTIFIER;
[0-9]+\.[0-9]*          SAVE_TOKEN; return TDOUBLE;
[0-9]+                  SAVE_TOKEN; return TINTEGER;
"="                     return TOKEN(TEQUAL);
"=="                    return TOKEN(TCEQ);
"!="                    return TOKEN(TCNE);
"<"                     return TOKEN(TCLT);
"<="                    return TOKEN(TCLE);
">"                     return TOKEN(TCGT);
">="                    return TOKEN(TCGE);

"("                     return TOKEN(TLPAREN);
")"                     return TOKEN(TRPAREN);
"{"                     return TOKEN(TLBRACE);
"}"                     return TOKEN(TRBRACE);

"."                     return TOKEN(TDOT);
","                     return TOKEN(TCOMMA);

"+"                     return TOKEN(TPLUS);
"-"                     return TOKEN(TMINUS);
"*"                     return TOKEN(TMUL);
"/"                     return TOKEN(TDIV);

.                       printf("Unknown token!\n"); yyterminate();

%%
