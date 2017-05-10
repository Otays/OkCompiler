/*============================================================================*
 *	SCANNER : scanner.l
 *			  This is a lexical analyzer implemented with lex/flex.
 *===========================================================================*/
 
%{
#include <stdio.h>
#include "y.tab.h"
#include "tree.h"			// For TOKEN_DEBUG flag
#include "colorlogs.h"
#define ERROR       302 	// Error token ID (Unused, so far)

int yycol = 1;				// Column number tracking
const int TABLENGTH = 8;	// Tab length for column number tracking
char* yycodeline;			// Stores a line of code for the code table

%}

yylineno = 1;

digit       	[0-9]
number      	0|([1-9]{digit}*)
hexnumber      	0x([a-fA-F0-9]+)
binnumber      	0b([0-1]+)

letter      	[a-zA-Z]
alphanumeric	{letter}|{digit}
identifier  	{letter}{alphanumeric}*

bliteral  		false|true|FALSE|TRUE

escapechar		\\n|\\t|\\\"|\\\\
character		'.'|'{escapechar}'

whitespace  	[ ]+
tabspace  		[\t]+
newline  		\n

%%

 /* Rules */ 

if         		{ yycol += yyleng; updateCodeline(); return KWD_IF; }
else       		{ yycol += yyleng; updateCodeline(); return KWD_ELSE; }
while     		{ yycol += yyleng; updateCodeline(); return KWD_WHILE; }

 /** yyval used as a flag for the type **/
int         	{ yycol += yyleng; yylval.value = INT_TYPE; 	updateCodeline(); return KWD_INT; }
string   		{ yycol += yyleng; yylval.value = STRING_TYPE; 	updateCodeline(); return KWD_STRING; }
char       		{ yycol += yyleng; yylval.value = CHAR_TYPE; 	updateCodeline(); return KWD_CHAR; }
bool         	{ yycol += yyleng; yylval.value = BOOL_TYPE; 	updateCodeline(); return KWD_BOOL; }
void   		    { yycol += yyleng; yylval.value = VOID_TYPE; 	updateCodeline(); return KWD_VOID; }
return  		{ yycol += yyleng; updateCodeline(); return KWD_RETURN; }

{hexnumber}     { yylval.literal[1] = yycol;
				  yylval.literal[0] = strtol(yytext, NULL, 16);
				  yycol += yyleng; 
				  updateCodeline(); 
				  return HEXCONST; }
				  
{binnumber}     { int sum = 0; int shift = 0; int i;
				  yylval.literal[1] = yycol;
				  for (i = yyleng-1; i >= 2; i--) 
				    sum += (*(yytext+i)-48)*(1 << shift++);
				  yylval.literal[0] = sum;
				  yycol += yyleng; 
				  updateCodeline(); 
				  return BINCONST; }
				  
{bliteral}      { yylval.literal[1] = yycol;
				  yylval.literal[0] = ( strcmp(yytext, "true") == 0 || 
								        strcmp(yytext, "TRUE") == 0 );
				  yycol += yyleng; 
				  updateCodeline(); 
				  return BOOLCONST; }

{identifier}    { yylval.table_id[1] = yycol; /* pass col info thru struct */
				  yylval.table_id[0] = string_table_insert(yytext);
				  yycol += yyleng; 
				  updateCodeline(); 
				  return ID; }

{number}        { yylval.literal[0] = atoi(yytext);
				  yylval.literal[1] = yycol;
				  yycol += yyleng; 
				  updateCodeline(); 
				  return INTCONST; }

{character}		{ yylval.literal[1] = yycol;
				  yycol += yyleng; 
				  if (yyleng == 3) {
				    yylval.literal[0] = *(yytext+1);
				  } else {
				    switch (*(yytext+2))
					{
					  case 'n':
					    yylval.literal[0] = '\n';
					    break;
					  case 't':
					    yylval.literal[0] = '\t';
					    break;
					  case '\\':
					    yylval.literal[0] = '\\';
					    break;
					  case '"':
					    yylval.literal[0] = '"';
					    break;
					}
				  }
				  updateCodeline(); 
				  return CHARCONST; }


\/\/[^\n]* { 
  /* Single line comment */
  int* p = &yyleng;
  int lines;
  
  lines = getLinesOf();
  if (lines)
  {
    yylineno += lines;
    yycol = yyleng+1;
  } else {
    yycol += yyleng;
  }
  /* No token */
}

\/\*([^*]|\*+[^*/])*\*+\/ { 
  /* Multiline comment */
  int lines;
  lines = getLinesOf();
  
  if (lines)
  {
    yylineno += lines;
    yycol = yyleng+1;
  } else {
    yycol += yyleng;
  }
  /* No Token */
}

\/\*([^*]|\*+[^*/])* { 
  /* Unterminated comment (Error) */
  LOG_RED("\nERROR: ");
  printf("Unterminated comment (%d:%d)\n", yylineno, yycol);
  yycol += yyleng; 
  return ERROR;
}

"+"             { yycol += yyleng; updateCodeline(); return OPER_ADD;}
"-"             { yycol += yyleng; updateCodeline(); return OPER_SUB;}
"*"             { yycol += yyleng; updateCodeline(); return OPER_MUL;}
"/"             { yycol += yyleng; updateCodeline(); return OPER_DIV;}
"%"             { yycol += yyleng; updateCodeline(); return OPER_MOD;}
"<"             { yycol += yyleng; updateCodeline(); return OPER_LT;}
">"             { yycol += yyleng; updateCodeline(); return OPER_GT;}
"<="            { yycol += yyleng; updateCodeline(); return OPER_GTE;}
">="            { yycol += yyleng; updateCodeline(); return OPER_LTE;}
"=="            { yycol += yyleng; updateCodeline(); return OPER_EQ;}
"!="            { yycol += yyleng; updateCodeline(); return OPER_NEQ;}
"="             { yylval.value = yycol; yycol += yyleng; updateCodeline(); return OPER_ASGN;}
"["             { yycol += yyleng; updateCodeline(); return LSQ_BRKT;}
"]"             { yycol += yyleng; updateCodeline(); return RSQ_BRKT;}
"{"             { yycol += yyleng; updateCodeline(); return LCRLY_BRKT;}
"}"             { yycol += yyleng; updateCodeline(); return RCRLY_BRKT;}
"("             { yycol += yyleng; updateCodeline(); return LPAREN;}
")"             { yycol += yyleng; updateCodeline(); return RPAREN;}
","             { yycol += yyleng; updateCodeline(); return COMMA;}
";"             { yycol += yyleng; updateCodeline(); return SEMICLN;}
"@"             { yycol += yyleng; updateCodeline(); return EULERS_TOTIENT;}

\"((\\.)|[^\\"\n])+\" { 
  /* String */
  yylval.table_id[1] = yycol;
  yycol += yyleng; 
  bufferedstring();
  updateCodeline();
  yylval.table_id[0] = string_table_insert(yytext);
  
  return STRCONST;
}

\"((\\.)|[^\\"])+\" { 
  /* String (Line break error) */
  int* p = &yyleng;
  int lines;
  
  LOG_RED("\nERROR: ");
  printf("Break in string (%d:%d)\n", yylineno, yycol);
  
  lines = getLinesOf();
  if (lines) {
    yylineno += lines;
    yycol = yyleng+1;
  } else {
    yycol += yyleng;
  }
  return ERROR;
}

\"(\\.|[^\\"])* { 
  /* String (Unterminated error) */
  LOG_RED("\nERROR: ");
  printf("Unterminated string (%d:%d)\n", yylineno, yycol);
  yycol += yyleng; 
  return ERROR;
}

{whitespace}    { yycol += yyleng; updateCodeline();}
{tabspace}    	{ yycol += TABLENGTH*yyleng - (yycol-1) % TABLENGTH; updateCodeline(); }
{newline}    	{ if (TOKEN_DEBUG) printf("\n"); 
				  if (yycodeline != NULL) code_table_insert(yycodeline);
				  yycodeline = NULL;
				  yylineno++; 
				  yycol = 1;
				  
				}

. { 
  /* Catch all: Unhandled tokens */
  LOG_RED("\nERROR: ");
  printf("Invalid Token (%d:%d)\n", yylineno, yycol);
  yycol += yyleng;
  updateCodeline();
  return ILLEGAL_TOK; 
}

%%

/*=============================================================================
 * Counts new lines characters in broken strings or comments
 *===========================================================================*/
int getLinesOf()
{
  int i;
  int size = *(int*)&yyleng;
  int result = 0;
  int length = 0;
  
  for (i = 0; i < size; i++) {
    if (yytext[i] == '\n') {
      result++;
      length = 0;
    } else if (yytext[i] == '\t') {
      length += 4 - (length%4);
    } else {
      length++;
    }
  }
  *(int*)&yyleng = length;
  return result;
  
}

/*=============================================================================
 * Replaces escape character sequences, "\n", "\t", "\"", "\\"
 *===========================================================================*/
bufferedstring()
{
  int i, k;
  
  // Scan through c string
  for (i = 0; i < yyleng; i++) 
  {
    if (yytext[i] == '\\') 
	{
      // Escape replacement
      switch (yytext[i+1]) {
        /* Special characters */
        case 'n':
          yytext[i] = '\n';
          break;
        case 't':
          yytext[i] = '\t';
          break;
        /* The character itself */
        case '"':
        case '\\':
          yytext[i] = yytext[i+1];
          break;
        default:
          // ???
          break;
      }
      
      // Adjust for character replacement
      for (k = i+1; k < yyleng; k++) {
        yytext[k] = yytext[k+1];
      }
      yyleng--;
    }
  }
  
  // Remove first quotation
  for (i = 0; i < yyleng; i++)
    yytext[i] = yytext[i+1];
  
  yyleng--;

  // Replace last quotation with null terminator
  yytext[yyleng-1] = yytext[yyleng];
  yyleng--;

  return;
}

/*=============================================================================
 * Stores an entire line of code for debugging
 *===========================================================================*/
updateCodeline()
{
  char* tempstr;
  
  if (yycodeline == NULL)
  {
	yycodeline = malloc( strlen(yytext)*sizeof(char)+1 );
    strcpy(yycodeline, yytext);
  } else {
	int sizer = strlen(yytext) + strlen(yycodeline) + 1;
	tempstr = malloc(sizer);
	strcpy(tempstr, yycodeline);
	strcat(tempstr, yytext);
	
	yycodeline = (char*)realloc(yycodeline, sizer);
	strcpy(yycodeline, tempstr);
	
  }
}

