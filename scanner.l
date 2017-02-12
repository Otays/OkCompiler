
 /* Definitions */

%{
	#include<stdio.h>
	#include"tokendef.h"
	
	int yycol = 1;
	const int TABLENGTH = 4;
%}

yylineno = 1;

digit       	[0-9]
number      	0|([1-9]{digit}*)

letter      	[a-zA-Z]
alphanumeric	{letter}|{digit}
identifier  	{letter}{alphanumeric}*

escapechar		\\n|\\t|\\\"|\\\\
character		'.'|'{escapechar}'

whitespace  	[ ]+
tabspace  		[\t]+
newline  		\n

%%

 /* Rules */ 

if         		{ yycol += yyleng; return KWD_IF; }
else       		{ yycol += yyleng; return KWD_ELSE; }
while     		{ yycol += yyleng; return KWD_WHILE; }
int         	{ yycol += yyleng; return KWD_INT; }
string   		{ yycol += yyleng; return KWD_STRING; }
char       		{ yycol += yyleng; return KWD_CHAR; }
return  		{ yycol += yyleng; return KWD_RETURN; }
void   		    { yycol += yyleng; return KWD_VOID; }

{identifier}    { yycol += yyleng; return ID; }

{number}        { yycol += yyleng; return INTCONST; }

{character}		{ yycol += yyleng; return CHARCONST; }


\/\*([^*]|\*+[^*/])*\*+\/ { 
	int* p = &yyleng;
	int lines;
	
	lines = getLinesOf(yytext, p);
	if (lines)
	{
		yylineno += lines;
		yycol = yyleng+1;
	} else {
		yycol += yyleng;
	}
	/* Do nothing */
}

\/\*([^*]|\*+[^*/])* { 
	printf("\nERROR: Unterminated comment (%d:%d)\n", yylineno, yycol);
	yycol += yyleng; 
	return ERROR;
	/* Report unterminated comment */
}

"+"             { yycol += yyleng; return OPER_ADD;}
"-"             { yycol += yyleng; return OPER_SUB;}
"*"             { yycol += yyleng; return OPER_MUL;}
"/"             { yycol += yyleng; return OPER_DIV;}
"<"             { yycol += yyleng; return OPER_LT;}
">"             { yycol += yyleng; return OPER_GT;}
"<="            { yycol += yyleng; return OPER_GTE;}
">="            { yycol += yyleng; return OPER_LTE;}
"=="            { yycol += yyleng; return OPER_EQ;}
"!="            { yycol += yyleng; return OPER_NEQ;}
"="             { yycol += yyleng; return OPER_ASGN;}
"["             { yycol += yyleng; return LSQ_BRKT;}
"]"             { yycol += yyleng; return RSQ_BRKT;}
"{"             { yycol += yyleng; return LCRLY_BRKT;}
"}"             { yycol += yyleng; return RCRLY_BRKT;}
"("             { yycol += yyleng; return LPAREN;}
")"             { yycol += yyleng; return RPAREN;}
","             { yycol += yyleng; return COMMA;}
";"             { yycol += yyleng; return SEMICLN;}
"@"             { yycol += yyleng; return ERROR;}

\"((\\.)|[^\\"\n])+\" { 
	yycol += yyleng; 
	bufferedstring(yytext, yyleng);
	return STRCONST;	// Good string
}

\"((\\.)|[^\\"])+\" { 
	int* p = &yyleng;
	int lines;
	
	printf("\nERROR: Break in string (%d:%d)\n", yylineno, yycol);
	lines = getLinesOf(yytext, p);
	if (lines)
	{
		yylineno += lines;
		yycol = yyleng+1;
	} else {
		yycol += yyleng;
	}
	return ERROR;		// Break between lines
}

\"(\\.|[^\\"])* { 
	printf("\nERROR: Unterminated string (%d:%d)\n", yylineno, yycol);
	yycol += yyleng; 
	return ERROR;		// Unterminated string
}


{whitespace}    { yycol += yyleng; 	 /* Do nothing else */ }
{tabspace}    	{ yycol += TABLENGTH*yyleng - (yycol-1) % TABLENGTH; /* Do nothing else */ }
{newline}    	{ printf("\n"); yylineno++; yycol = 1; }

 /* Catch all: Unhandled tokens */
. { 
	printf("\nERROR: Invalid Token (%d:%d)\n", yylineno, yycol);
	yycol += yyleng;
	return ILLEGAL_TOK; 
}

%%

/*=============================================================================
 * Counts new lines characters in broken strings or comments
 *===========================================================================*/
int getLinesOf( const char* yytext, int* yyleng )
{
	int i;
	int size = *yyleng;
	int result = 0;
	int length = 0;
	
	for (i = 0; i < size; i++)
	{
		if (yytext[i] == '\n')
		{
			result++;
			length = 0;
			
		} else if (yytext[i] == '\t') {
			length += 4 - (length%4);
			
		} else {
			length++;
		}
	}
	*yyleng = length;
	return result;
}

/*=============================================================================
 * Replaces escape character sequences, "\n", "\t", "\"", "\\"
 *===========================================================================*/
bufferedstring( char* yytext, int yyleng )
{
	int i, k;
	
	// Scan through c string
	for (i = 0; i < yyleng; i++)
	{
		if (yytext[i] == '\\')
		{
			// Escape replacement
			switch (yytext[i+1])
			{
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
			for (k = i+1; k < yyleng; k++)
			{
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




