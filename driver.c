#include"tokendef.h"
#include<stdio.h>

extern char *yytext;

extern int yylineno;
extern int yycol;

void printToken(int tokenNum) {
  switch(tokenNum) {
  
  /* IDs */
  case ID:
    printf("<ID, %s>", yytext);
    break;
	
  /* Literals */
  case INTCONST:
    printf("<INT, %i>", atoi(yytext));
    break;
  case CHARCONST:
    printf("<CHAR, %i>", *(yytext+1));
    break;
  case STRCONST:
    printf("<STRING, %s>", yytext);
    break;
	
  /* Keywords */
  case KWD_IF:
  case KWD_ELSE:
  case KWD_WHILE:
  case KWD_INT:
  case KWD_STRING:
  case KWD_CHAR:
  case KWD_RETURN:
  case KWD_VOID:
    printf("<KEYWORD, %s>", yytext);
    break;
	
  /* operators */
  case OPER_ADD:
  case OPER_SUB:
  case OPER_MUL:
  case OPER_DIV:
  case OPER_LT:
  case OPER_GT:
  case OPER_GTE:
  case OPER_LTE:
  case OPER_EQ:
  case OPER_NEQ:
  case OPER_ASGN:
    printf("<OP, %s>", yytext);
    break;
	
  /* brackets & parens */
  case LSQ_BRKT:
    printf("<LSQ_BRKT>");
    break;
  case RSQ_BRKT:
    printf("<RSQ_BRKT>");
    break;
  case LCRLY_BRKT:
    printf("<LCRLY_BRKT>");
    break;
  case RCRLY_BRKT:
    printf("<RCRLY_BRKT>");
    break;
  case LPAREN:
    printf("<LPAREN>");
    break;
  case RPAREN:
    printf("<RPAREN>");
    break;
	
  /* punctuation */
  case COMMA:
    printf("<,>");
    break;
  case SEMICLN:
    printf("<;>");
    break;
	
  /* Other */
  case ERROR:
    printf("[ERROR]\n");
    break;
  case ILLEGAL_TOK:
    printf("<ILLEGAL_TOK>");
    break;
  default:
    printf("[UNHANDLED TOKEN]");
  }
  return;
}


int main() {

  int ret = yylex();
  while (ret) {
    printToken(ret);
    ret = yylex();
  }
  printf("\n");
  return 0;
}


