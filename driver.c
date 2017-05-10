/*============================================================================*
 *	DRIVER : driver.c
 *
 *  Author 		: Ben Pogrund
 *  Instructor 	: Apan Qasem
 *
 *===========================================================================*/
 
// Version
#define  MAJOR 	  0
#define  MINOR 	  4
#define  REVISION 3
 
// Libraries
#include <stdio.h>
#include <sys/time.h>
#include <stdarg.h>
#include "y.tab.h"
#include "tree.h"
#include "colorlogs.h"

#define ERROR 302		// Not sure why this doesnt translate over to y.tab.h from tokendef.h

// Frontend Global
extern char *yytext;
extern int yyleng;
extern int yylineno;
extern int yycol;
extern strtable  table;						// Lookup table of strings & IDs
extern codetable ctable;					// Lookup table of source code
extern tree 	*ast;						// Abstract Syntax Tree Root
extern symrec 	*sym_table = (symrec *)0;	// Linked List Head

// Backend Global
FILE *asm_output;							// MIPS Output file
int uniqID;									// Used for unique MIPS labeling
int nestID;									// Used for unique MIPS nesting

/*============================================================================*
 *	main()
 *		description: calls tokenizer and parser phases
 *===========================================================================*/
int main(int argc, char **argv) {
  struct timeval start;			// Used to calculate runtime
  gettimeofday(&start, NULL);	// Get start time
  int enable_cgen = 0;			// front end will store 1 to enable code gen
  
  /** ======================== Welcome Message ========================== **/
  
  LOG_WHITE("--- Running OKCompiler (%i.%i.%i) ---", MAJOR, MINOR, REVISION);

  /** =================================================================== **/
  
  initialize_tables();
  
  // Front end
  if ( !TOKEN_DEBUG ) {
	scan_and_parse(start);						// Lexical analysis & parsing
	if (ast)
	  enable_cgen = semantic_analysis(start);	// Semantic analysis
  } else {
	scan_only();								// Lexical only, token debug
  }
  
  // Table debug
  if (TABLE_DEBUG) {
    show_string_table();
    show_code_table();
  }
  
  // Back end
  if (enable_cgen)
	code_gen(ast, start);						// MIPS Code generation
  
  // Clean up
  clean_up_tables();
  if (ast) clean_up_tree(ast);
  
  LOG_WHITE("\n\n--- End of Program ---\n\n");
  return 0;
}

/*============================================================================*
 *	scan_only()
 *		Description: This calls the scanner described in scanner.l and outputs
 * 					 the tokens for debugging.
 *
 * 							--- DEBUG USE ONLY ---
 *
 *===========================================================================*/
void scan_only()
{
  int tokenID = 0;				// Stores a Token ID read from 'yylex()' 
  
  LOG_CYAN("\n\n[Lexical Analysis]\n\n");
  tokenID = yylex();
  while (tokenID) {
    printToken(tokenID);
    tokenID = yylex();
  }
}

/*============================================================================*
 *	scan_and_parse()
 *		Description	: Computes lexical analysis and parsing using Lex and 
 *					  YACC files (scanner.l, parser.y)
 *		Parameters	: start - Initial timestamp from main()
 *
 *===========================================================================*/
void scan_and_parse(struct timeval start)
{
  double runtime;				// Stores runtime for display
  struct timeval end;			// Used to calculate runtime
  
  LOG_CYAN("\n\n[Parser Phase]\n");
  
  parserinit(); // Prints info, like debug state
  
  switch (yyparse()) {
    case 0:
      printf("\n---\n\nParsing terminated with status:\n");
      LOG_GREEN(" ACCEPTED ");
      break;
    case 1:
      printf("\n---\n\nParsing terminated with status:\n");
      LOG_RED(" REJECTED ");
      break;
    case 2:
      printf("\n---\n\nParsing failure, status:\n");
      LOG_RED(" MEMORY EXHAUSTION ");
      break;
  }
  
  // Compute runtime checkmark
  gettimeofday(&end, NULL);
  runtime = end.tv_sec + end.tv_usec / 1000000.0;
  runtime -= start.tv_sec + start.tv_usec / 1000000.0;
  printf("with compute time: %.4f s", runtime);
  printf("\n\n---\n");
}

/*============================================================================*
 *	semantic_analysis
 *		Description : Generates semantic warnings and errors
 *		Parameters	: start - Initial timestamp from main()
 *		Return val  : 1 to enable code gen, 0 otherwise
 *
 *===========================================================================*/
int semantic_analysis(struct timeval start)
{
  struct timeval end;			// Used to calculate runtime
  double runtime;				// Stores runtime for display
  int warningCount = 0;			// Used to report number of warnings
  int errorCount = 0;			// Used to report number of errors
  
  LOG_CYAN("\n[Semantic Analysis]\n");
  
  if (ast) {
	// AST Maintenance
    semantic_crawl_quiet(ast);
    
	// AST Output
    if ( AST_DEBUG ) {
      printf("[AST debug: on]\n");
      print_ast(ast, 1);
    } else {
      printf("[AST debug: off]\n");
	}
	printf("\n---\n\n");
	
	// Warning and error report
    semantic_crawl(ast, &warningCount, &errorCount);
	if (warningCount+errorCount) printf("\n---\n\n"); 
  } 
  
  // Show total warnings and errors
  printf("Semantic Analysis completed:\n");
  if (errorCount) {
    LOG_CRIM(" %i error(s), %i warning(s) ", errorCount, warningCount);
  } else if (warningCount) {
    LOG_YELLOW(" 0 error(s), %i warning(s) ", warningCount);
  } else {
    LOG_GREEN(" 0 error(s), 0 warning(s) ");
  }
    
  // Compute 2nd runtime checkmark
  gettimeofday(&end, NULL);
  runtime = end.tv_sec + end.tv_usec / 1000000.0;
  runtime -= start.tv_sec + start.tv_usec / 1000000.0;
  printf("with compute time: %.4f s", runtime);
  printf("\n\n---\n");
  
  return (errorCount == 0);	// Generates code if no errors
}

/*============================================================================*
 *	printToken()
 *		Description: Prints to console the token information
 * 		
 * 		Token format: 
 * 			< CLASS, String >
 * 		
 *===========================================================================*/
void printToken(int tokenNum) {
  switch(tokenNum) {
	
  /* IDs */
  case ID:
    printf("<ID, "); LOG_CYAN(yytext); printf(">", yytext);
    break;
	
  /* Literals */
  case INTCONST: 
    printf("<INT, "); LOG_CRIM("%i", atoi(yytext)); printf(">", yytext);
    break;
  case CHARCONST:
    printf("<CHAR, "); LOG_CRIM("%c", *(yytext+1)); if(yyleng==4) LOG_CRIM("%c", *(yytext+2)); printf(">", yytext);
    break;
  case STRCONST:
    printf("<STRING, "); LOG_CRIM("\"%s\"", yytext); printf(">", yytext);
    break;
  case HEXCONST:
    printf("<HEX, "); LOG_CRIM("%s", yytext); printf(">", yytext);
    break;
  case BINCONST:
    printf("<BINARY, "); LOG_CRIM("%s", yytext); printf(">", yytext);
    break;
  case BOOLCONST:
    printf("<BOOL, "); LOG_CRIM("%s", yytext); printf(">", yytext);
    break;
	
  /* Keywords */
  case KWD_IF:
  case KWD_ELSE:
  case KWD_WHILE:
  case KWD_INT:
  case KWD_BOOL:
  case KWD_STRING:
  case KWD_CHAR:
  case KWD_RETURN:
  case KWD_VOID:
    printf("<KEYWORD, "); LOG_GOLD(yytext); printf(">", yytext);
    break;
	
  /* binary operators */
  case OPER_ADD:
  case OPER_SUB:
  case OPER_MUL:
  case OPER_DIV:
  case OPER_MOD:
  case OPER_LT:
  case OPER_GT:
  case OPER_GTE:
  case OPER_LTE:
  case OPER_EQ:
  case OPER_NEQ:
  case OPER_ASGN:
    printf("<OP, "); LOG_GREEN(yytext); printf(">", yytext);
    break;
	
  /* unary operators */
  case EULERS_TOTIENT:
    printf("<OP, "); LOG_GREEN(yytext); printf(">", yytext);
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
    LOG_RED("[ERROR]\n");
    break;
  case ILLEGAL_TOK:
    LOG_RED("<ILLEGAL_TOK>");
    break;
  default:
    LOG_YELLOW("[UNHANDLED TOKEN]");
  }
  return;
}

/*============================================================================*
 *	putsym()
 *		Description: Inserts a symbol to the head of the linked list
 * 		
 *===========================================================================*/
symrec *putsym (sym_name, sym_type, sym_kind, sym_scope)
     char *sym_name;
     int sym_type;
     int sym_kind;
     char *sym_scope;
{
  symrec *ptr;
  ptr = (symrec*) malloc (sizeof(symrec));
  ptr->name = (char*) malloc (sizeof(char*) * (strlen (sym_name) + 1));
  strncpy (ptr->name, sym_name, (strlen (sym_name) + 1));
  ptr->type = sym_type;
  ptr->kind = sym_kind;
  ptr->scope = sym_scope;
  ptr->signature_index = -1;
  
  /* initialize symbol data ... work in progress */
  switch (ptr->type)
  {
    case INT_TYPE:
	  ptr->symdata = (int*) malloc(sizeof(int));
	  *((int*)ptr->symdata) = 0;
	  break;
	default:
	  ptr->symdata = NULL;
	  break;
  }
  
  // Insert to head of list
  ptr->next = (struct symrec*) sym_table;
  sym_table = ptr;
  return ptr;
}

/*============================================================================*
 *	getsym()
 *		Description: Traverses the Linked List searching for 'sym_name'
 * 		
 *===========================================================================*/
symrec *getsym (sym_name, sym_kind, sym_scope)
     char *sym_name;
	 int   sym_kind;
	 char *sym_scope;
{
  symrec *ptr;
  
  if (sym_name == 0) return 0;
  
  for (ptr = sym_table; ptr != (symrec *) 0; ptr = (symrec *)ptr->next) {
    if (strcmp (ptr->name, sym_name) == 0) {
	  /** For unspecified scope: return any match **/
      if (sym_scope == NULL) 
	  {
	    if ((sym_kind == FUNCTION_CALL) && ptr->kind == FUNCTION_CALL)
	      return ptr;
		if ((sym_kind != FUNCTION_CALL) && ptr->kind != FUNCTION_CALL)
	      return ptr;
	  }
	  /** Otherwise check matching scope **/
	  if (ptr->scope != NULL && strcmp (ptr->scope, sym_scope) == 0) 
	  {
	    if (sym_kind == FUNCTION_CALL && ptr->kind == FUNCTION_CALL)
	      return ptr;
		if (sym_kind != FUNCTION_CALL && ptr->kind != FUNCTION_CALL)
	      return ptr;
	  }
	} 
  }
  return (symrec*) 0;  // NULL if not found
}

/*============================================================================*
 *	modsym()
 *		Description: Modifies a symbol
 * 		
 *===========================================================================*/
int modsym (sym_target, sym_type)
     symrec *sym_target;
     int sym_type;
{
  if (sym_target->type == 0)
  {
    sym_target->type = sym_type;
	return 0;
  } else {
    //LOG_RED("\nWARNING: Attempted an illegal type change\n");
    return 1;
  }
}

/*============================================================================*
 *	clean_up_tree()
 *		Description: frees dynamically allocated tree nodes
 * 		
 *===========================================================================*/
void clean_up_tree (tree* node)
{
  int i;
  for (i = 0; i < node->numChildren; i++) {
	clean_up_tree(node->children[i]);
  }
  if (!node) free(node);
}

/*============================================================================*
 *	clean_up_tables()
 *		Description: frees dynamically allocated table strings
 * 		
 *===========================================================================*/
void clean_up_tables()
{
  // Symbol table
  if (sym_table) 
  {
    symrec *target = sym_table;
    symrec *traverse = sym_table->next;
	while (traverse)
	{
	  free(target->name);
	  if (target->symdata) free(target->symdata);
	  free(target);
	  target = traverse;
	  traverse = traverse->next;
	}
  }
  
  // String table
  int i;
  for (i = 0; i < table.capacity; i++) {
	if (!table.entries[i]) free(table.entries[i]);
  }
  
  // Code table
  for (i = 0; i < ctable.count; i++) {
	if (!ctable.entries[i]) free(ctable.entries[i]);
  }
}

/*============================================================================*
 *	show_code_table()
 *		Description: For debug purposes only, displays the original input file
 * 		
 *===========================================================================*/
void show_code_table()
{
  LOG_WHITE("\n--- Code table ---\n\n");
  int i;
  for (i = 0; i < ctable.count; i++)
    if (ctable.entries[i]) { LOG_WHITE("[%i] ", i); LOG_GOLD("%s\n", ctable.entries[i]); }
  LOG_WHITE("\n");
}

/*============================================================================*
 *	show_string_table()
 *		Description: For debug purposes only, displays string table.
 *					 String table includes IDs and string literals.
 * 		
 *===========================================================================*/
void show_string_table()
{
  LOG_WHITE("\n--- String table ---\n\n");
  int i;
  for (i = 0; i < table.count; i++)
    if (table.entries[i]) { LOG_WHITE("#%i ", i); LOG_CYAN("%s\n", table.entries[i]); }
  LOG_WHITE("\n");
}

/*============================================================================*
 *	initialize_tables()
 *		Description: Sets up initial values for dynamic tables
 * 		
 *===========================================================================*/
void initialize_tables()
{
  // Code table
  ctable.count = 500;
  ctable.entries = malloc(sizeof(char*)*ctable.count);
  
  // String table
  table.capacity = 500;
  table.count = 0;
  table.entries = malloc(sizeof(char*)*table.capacity);
}

/*============================================================================*
 *	cgen()
 *		Description: Writes a line of assembly to the output file
 * 		
 * 		Supports Deb
 * 		
 *===========================================================================*/
void cgen(const char* string, ...)
{
  va_list args;
  
  va_start(args, string);				// Allows argument list
  vfprintf(asm_output, string, args);	// Prints to assembly file
  va_end(args);
  
  if (CGEN_DEBUG) {
    va_start(args, string);				// Reset argument list
	vprintf(string, args);				// Print to console
    va_end(args);
  }
}

/*============================================================================*
 *	code_gen()
 *		Description: Master call for generating MIPS assembly code
 * 		
 *===========================================================================*/
void code_gen(tree* root, struct timeval start)
{
  struct timeval end;			// Used to calculate runtime
  double runtime;				// Stores runtime for display
  
  LOG_CYAN("\n[Code Generation]\n");				// Output phase to console
  asm_output = fopen("code_gen.mips", "wb+");		// Initialize output file
  
  // Write header
  cgen("################################################\n"); 
  cgen("##### Code generated by OKCompiler (%i.%i.%i) #####\n", 
	   MAJOR, MINOR, REVISION);
  cgen("################################################\n"); 
  
  int i;						// Iterator
  root = root->children[0];		// Traverse from "program" root to "declList"
  tree* node;					// Stores a node of interest
  uniqID = 0;					// Provides unique label IDs
  nestID = 0;					// Provides unique labels within nests
  
  // Process global vars
  cgen("\t.data\n");
  for (i = 0; i < root->numChildren; i++) {
	node = root->children[i]->children[0];		// child of interest
	if (node->nodeKind == VARDECL) {
	  switch (symType(node)) {
		case INT_TYPE:
		  cgen("_%s:\t.word 0\n", symName(node));
		  break;
		default:
		  cgen("## Omitted unsupported type for %s ##\n", symName(node));
	  }
    }
  }
  
  // Process main function
  //cgen("\t.text\n");
  cgen("\t.globl\tmain\n\n");
  for (i = 0; i < root->numChildren; i++) {
	node = root->children[i]->children[0];		// child of interest
	if (node->nodeKind == FUNDECL
	&& strcmp (symName(node), "main") == 0) {
	  cgen_func_def(node);
    }
  }
  
  // Process all other functions
  for (i = 0; i < root->numChildren; i++) {
	node = root->children[i]->children[0];		// child of interest
	if (node->nodeKind == FUNDECL
	&& strcmp(symName(node), "main") != 0) {
	  cgen_func_def(node);
    }
  }
  
  fclose(asm_output);
  
  // Compute 3rd runtime checkmark
  gettimeofday(&end, NULL);
  runtime = end.tv_sec + end.tv_usec / 1000000.0;
  runtime -= start.tv_sec + start.tv_usec / 1000000.0;
  printf("Code Generation completed:\n");
  LOG_GREEN(" Generated MIPS file");
  printf(" (\"code_gen.mips\") with compute time: %.4f s", runtime);
  printf("\n\n---\n");
  
}

/*============================================================================*
 *	cgen_main_def()
 *		Description: Defines the body of main in assembly
 * 		
 *===========================================================================
void cgen_main_def( tree* node )
{
  int arg_count = 0;
  int var_count = 0;
  
  // Header
  cgen("#####  begin main ()  #####\n");
  cgen("%s:\n", symName(node));				// Entry label 
  
  // Prologue
  cgen("\t#  function entry for main ()  #\n");
  cgen("\tmove $fp $sp\n");					// New frame pointer position
  var_count = cgen_locals(node->children[0]);// Local variable preparation
  cgen("\taddiu $sp $sp -%i\n", var_count*4);// Allocate local vars to stack
  cgen("\tsw $ra 0($sp)\n");				// Store return address to stack
  cgen("\taddiu $sp $sp -4\n");				// Increment stack size
  
  // Body
  cgen("\t#  function body for main ()  #\n");
  arg_count = cgen_args(node, var_count);	// Finds formalDeclList, generates code
  cgen_body(node);							// Finds funBody, generates code
  
  cgen("#####  end main ()  #####\n\n");
}*/

/*============================================================================*
 *	cgen_func_def()
 *		Description: Defines the body of a function in assembly
 * 		
 *===========================================================================*/
void cgen_func_def( const tree* node )
{
  int size = 0;							// Argument count * 4 + 8
  int arg_count = 0;					// Arg count
  int var_count = 0;					// Local var count
  
  // Header
  cgen("#####  begin %s ()  #####\n", symName(node) );
  cgen("\t.text\n");					// Marks instruction segment
  cgen("%s:\n", symName(node));		// Entry label 
  
  // Prologue
  cgen("\t#  function entry for %s ()  #\n", symName(node));
  var_count = cgen_locals(node);		// Local variable preparation
  cgen("\taddiu $sp $sp -%i\n", var_count*4);// Allocate local vars to stack
  cgen("\tmove $fp $sp\n");				// New frame pointer position
  cgen("\tsw $ra 0($sp)\n");			// Store return address to stack
  cgen("\taddiu $sp $sp -4\n");			// Increment stack size
  
  // Body
  cgen("\t#  function body for %s ()  #\n\n", symName(node));
  arg_count = cgen_args(node, var_count);// Finds formalDeclList
  cgen_body(node);						// Finds funBody, generates code
  
  // Epilogue
  size = (arg_count + var_count + 2) * 4;
  cgen("\t#  function epilogue for %s ()  #\n", symName(node));
  cgen("%s_exit:\n", symName(node));	// Exit label 
  cgen("\tmove $v1 $a0\n");				// Move return value from a0 to v1
  cgen("\tlw $ra 0($fp)\n");			// Retrieve return address
  cgen("\tlw $fp %i($sp)\n", size);		// Restore old frame pointer to fp
  cgen("\taddiu $sp $sp %i\n", size);	// Pop frame off the stack ... size=n*4+8
  cgen("\tjr $ra\n");					// Resume execution of calling function
  
  cgen("#####  end %s ()  #####\n\n", symName(node) );
}
/*=============================================================================
==             Activation record 											 ==
===============================================================================
			On entry	  Before exit

	$fp  |¯¯¯¯¯¯¯¯¯¯¯¯|  |¯¯¯¯¯¯¯¯¯¯¯¯|  		(x,y,z) function parameters,
		 |   Stack	  |  |   Stack	  |  				respectively
		 |____________|  |____________|  
		 |   Old FP   |  |   Old FP   |
		 |     z      |  |     z      |
		 |     y      |  |     y      |
		 |     x      |  |     x      |
	$sp   ¯¯¯¯¯¯¯¯¯¯¯¯   |	   c      |     	(a,b,c) local variables,
	                     |	   b      | 				respectively
	                     |	   a      | 
	                     |	  ra      | $fp
						  ¯¯¯¯¯¯¯¯¯¯¯¯  $sp
=============================================================================*/


/*============================================================================*
 *	cgen_args()
 *		Description: Searches for formalDeclList node, process arguments
 * 		
 *===========================================================================*/
int cgen_args( tree* node, int offset )
{
  int arg_count = 0;
  int i;
  tree* declList;
  
  for (i = 0; i < node->numChildren; i++) {
	if (node->children[i]->nodeKind == FORMALDECLLIST) {
	  declList = node->children[i];
	  arg_count = declList->numChildren;
	  
	  // Enumerate Symbols
      tree* varNode = node->children[0];
      int i;
      for (i = 0; i < varNode->numChildren; i++) {
	    varNode->children[i]->mySymbol->index = i+1+offset;
      }
	}
  }
  return arg_count;
}

/*============================================================================*
 *	cgen_locals()
 *		Description: Searches for localDeclList node, amends activation record
 * 		
 *===========================================================================*/
int cgen_locals( tree* node )
{
  int var_count = 0;
  int i;
  tree* localDeclList;
  tree* funBody;
  
  for (i = 0; i < node->numChildren; i++) {
	if (node->children[i]->nodeKind == FUNBODY) {
	  funBody = node->children[i];
	}
  }
  
  for (i = 0; i < funBody->numChildren; i++) {
	if (funBody->children[i]->nodeKind == LOCALDECLLIST) {
	  localDeclList = funBody->children[i];
	  var_count = localDeclList->numChildren;
	  
	  // Enumerate Symbols
      tree* varNode = funBody->children[0];
      int i;
      for (i = 0; i < varNode->numChildren; i++) {
	    varNode->children[i]->mySymbol->index = i+1;
      }
	  return var_count;
	}
  }
  
  return 0;
}

/*============================================================================*
 *	cgen_body()
 *		Description: Searches for funBody node, processes body
 * 		
 *===========================================================================*/
void cgen_body( tree* node )
{
  int i;
  tree* funBody = NULL;
  for (i = 0; i < node->numChildren; i++) {			// Find funBody node
	if (node->children[i]->nodeKind == FUNBODY) {
	  funBody = node->children[i];
	}
  }
  cgen_block(funBody);
}

/*============================================================================*
 *	cgen_block()
 *		Description: Process funBody's subtree with order preserved
 * 		
 *===========================================================================*/
int cgen_block( tree* node )
{
  if (node == NULL) return;	
  
  switch (node->nodeKind) {
	// Core statements & control structures
	case ASSIGNSTMT:	cgen_asgn(node);		break; 
	case CONDSTMT:	 	cgen_cond(node);		break; 
	case LOOPSTMT:	 	cgen_loop(node);		break; 
	case FUNCCALLEXPR:	cgen_func_call(node);	break;
	case RETURNSTMT:
      cgen_return(node);
	  break;
	  
	// Expression subtypes
    case ADDITION:	
	case SUBTRACTION:
	case MULTIPLICATION:
	case DIVISION:
	case MODULUS:
	  cgen_binary_op(node, node->nodeKind);
	  break;
	  
	// Literals and vars
	case FACTOR:
	  cgen_factor(node);
	  break;
	  
	// Intermediary nodes
	default:
	  cgen_block_all(node);
	  break;
  }
}

/*============================================================================*
 *	cgen_block_all()
 *		Description: 
 * 		
 *===========================================================================*/
void cgen_block_all( tree* node )
{
  int i;
  for (i = 0; i < node->numChildren; i++) {
	cgen_block(node->children[i]);
  }
}

/*============================================================================*
 *	cgen_return()
 *		Description: 
 * 		
 *===========================================================================*/
void cgen_return( tree* node )
{
  tree* traverse = node->parent;
  while (traverse->nodeKind != FUNDECL && traverse != NULL) {
	traverse = traverse->parent;
  }
  
  cgen("\t\n\t#  Return (Exit) #\n");
  if (strcmp(symName(traverse), "main") == 0)
  {
    cgen("\tli $v0 17\n");				// Set sysID 17 : Terminate with Value
	cgen_block_all(node);				// Evaluate return value for a0
    cgen("\tsyscall\n");				// Perform exit with a0 value
  } else {
	cgen_block_all(node);				// Evaluate return value for a0
    cgen("\tj %s_exit\n", symName(traverse));			// Exit label 
  }
}

/*============================================================================*
 *	cgen_binary_op()
 *		Description: 
 * 		
 *===========================================================================*/
void cgen_binary_op( tree* node, int op )
{
  cgen_block(node->children[0]); 	// Expression 1
  cgen("\tsw $a0 0($sp)\n");		// Store from accumulator to stack
  cgen("\taddiu $sp $sp -4\n");		// Increment stack size

  cgen_block(node->children[1]);	// Expression 2 --- Result is in a0
  cgen("\tlw $t1 4($sp)\n");		// Load result of first expression to t1
  switch (op)
  {
	case ADDITION:
	  cgen("\tadd $a0 $t1 $a0\n");	// perform addition
	  break;
	case SUBTRACTION:
	  cgen("\tsub $a0 $t1 $a0\n");	// perform subtraction
	  break;
	case MULTIPLICATION:
	  cgen("\tmult $t1 $a0\n");		// perform multiplication
	  cgen("\tmflo $a0\n");			// Move result to a0
	  break;
	case DIVISION:
	  cgen("\tdiv $t1 $a0\n");		// perform division
	  cgen("\tmflo $a0\n");			// Move quotient to a0
	  break;
	case MODULUS:
	  cgen("\tdiv $t1 $a0\n");		// perform modulus
	  cgen("\tmfhi $a0\n");			// Move remainder to a0
	  break;
	
  }
  cgen("\taddiu $sp $sp 4\n");		// decrement stack size
}

/*============================================================================*
 *	cgen_asgn()
 *		Description: Generates code for an assignment statement
 * 		
 *===========================================================================*/
void cgen_asgn( tree* node )
{
  tree* LHS = node->children[0];
  tree* RHS = node->children[1];
  
  cgen("\t#  Assignment statement  #\n");
  cgen_block(RHS);								// Evaluate RHS to a0
  
  if (strcmp (symScope(LHS), "Global") == 0)
    cgen("\tsw $a0 _%s\n", symName(LHS));		// Store result in global var
  else
	cgen("\tsw $a0 %i($fp)\n", 4*symIndex(LHS));// Store result in local var
  
  cgen("\t#  End of assignment  #\n");
  
}

/*============================================================================*
 *	cgen_loop()
 *		Description: Generates code for a while loop
 * 		
 *===========================================================================*/
void cgen_loop( tree* node )
{
  // Iterative subtree structure
  tree* LHS = node->children[0]->children[0];
  tree* RHS = node->children[0]->children[1];
  int rel_op = node->children[0]->nodeKind;
  tree* block = node->children[1];
  int myID = uniqID++;				// Provides unique label via enumeration
  
  // Code generation
  cgen("  loop_%i:\n", myID);		// Entry point of loop
  
  cgen_block( LHS );				// Left hand side evaluated
  cgen("\tsw $a0 0($sp)\n");		// Store from accumulator to stack
  cgen("\taddiu $sp $sp -4\n");		// Increment stack size

  cgen_block( RHS );				// Right hand side evaluated
  cgen("\tlw $t1 4($sp)\n");		// Load result of first expression to t1
  cgen("\taddiu $sp $sp 4\n");		// decrement stack size
  switch (rel_op)
  {
	/** Branches are negated to satisfy jump to exit condition **/
	case RELATION_EQ:
	  cgen("\tbne $t1 $a0 out_%i\n", myID);	// break to branch if equal
	  break;
	case RELATION_NEQ:
	  cgen("\tbeq $t1 $a0 out_%i\n", myID);	// break to branch if not equal
	  break;
	case RELATION_LTE:	cgen("\t# Unimplemented relation\n");	break; // Coming soon...
	case RELATION_LT:	cgen("\t# Unimplemented relation\n");	break; 
	case RELATION_GT:	cgen("\t# Unimplemented relation\n");	break; 
	case RELATION_GTE:	cgen("\t# Unimplemented relation\n");	break; 
	default:
	  LOG_CRIM("\nERROR: Unhandled relation.\n");	
	  cgen("\t# ERROR: Unhandled relation\n");	
	  break;
  }
  cgen_block(block);				// Generate block body
  cgen("\tj loop_%i\n", myID);		// Store return variable in var
  cgen("  out_%i:\n", myID);		// Exit point of loop
}

/*============================================================================*
 *	cgen_cond()
 *		Description: Generates code for "if" statements
 * 		
 *===========================================================================*/
void cgen_cond( tree* node )
{
  // Conditional subtree structure
  tree* LHS = node->children[0]->children[0];
  tree* RHS = node->children[0]->children[1];
  int rel_op = node->children[0]->nodeKind;
  tree* block1 = node->children[1];
  tree* block2 = NULL;
  if (node->numChildren == 3) block2 = node->children[2];
  
  int myID = uniqID++; 				// Provides unique label via enumeration
  
  // Code generation
  cgen_block( LHS );				// Left hand side evaluated
  cgen("\tsw $a0 0($sp)\n");		// Store from accumulator to stack
  cgen("\taddiu $sp $sp -4\n");		// Increment stack size

  cgen_block( RHS );				// Right hand side evaluated
  cgen("\tlw $t1 4($sp)\n");		// Load result of first expression to t1
  cgen("\taddiu $sp $sp 4\n");		// decrement stack size
  switch (rel_op)
  {
	case RELATION_EQ:
	  cgen("\tbeq $t1 $a0 tbranch_%i\n", myID);	// break to branch if equal
	  break;
	case RELATION_NEQ:
	  cgen("\tbne $t1 $a0 tbranch_%i\n", myID);	// break to branch if not equal
	  break;
	case RELATION_LTE:	cgen("\t# Unimplemented relation\n");	break;
	case RELATION_LT:	cgen("\t# Unimplemented relation\n");	break; 
	case RELATION_GT:	cgen("\t# Unimplemented relation\n");	break; 
	case RELATION_GTE:	cgen("\t# Unimplemented relation\n");	break; 
	default:
	  LOG_CRIM("\nERROR: Unhandled relation.\n");	
	  cgen("\t# ERROR: Unhandled relation\n");	
	  break;
  }
  
  cgen("  fbranch_%i:\n", myID);	// label, FALSE code
  if (block2 != NULL) 
	cgen_block(block2);				// Generate "else" block
  cgen("\tb endif_%i\n", myID);		// Break to the end
  cgen("  tbranch_%i:\n", myID);	// label, TRUE code
  cgen_block(block1);				// Generate "true" block
  cgen("  endif_%i:\n", myID);		// End label

}

/*============================================================================*
 *	cgen_func_call()
 *		Description: Builds an activation record for a new function
 * 		
 *===========================================================================*/
void cgen_func_call( tree* node )
{
  tree* argList;
  int i;
  
  cgen("\t# function call %s #\n", symName(node));
  cgen("\tsw $fp 0($sp)\n");			// Store old frame pointer to stack
  cgen("\taddiu $sp $sp -4\n");			// Increment stack size
  
  // Check for parameters
  if (node->numChildren != 0) { 
    argList = node->children[0];
	
    // Populate parameters to activation record in reverse order
    for (i = argList->numChildren - 1; i >= 0; i--) {
      cgen_block(argList->children[i]);	// Evaluate argument to a0
      cgen("\tsw $a0 0($sp)\n");			// Store argument to stack
      cgen("\taddiu $sp $sp -4\n");		// Increment stack size
    }
  }
  cgen("\tjal %s\n", symName(node));	// jump and link ($ra holds return link)
  cgen("\t# end function call %s #\n", symName(node));
}

/*============================================================================*
 *	cgen_factor()
 *		Description: Evaluates literals and variables given a "factor" node
 * 		
 *===========================================================================*/
void cgen_factor( tree* node ) {
  if (node->numChildren == 0) {
	// Immediate value
    cgen("\tli $a0 %i\n", node->val);	
	
  } else {
	tree* varNode = node->children[0];
	
	// Global var
	if (strcmp (symScope(varNode), "Global") == 0) {
      cgen("\tlw $a0 _%s\n", symName(node->children[0]));	
	  return;
	}
	
	// Parameters and local variables
    cgen("\tlw $a0 %i($fp)\n", 4*symIndex(varNode));	
  }
  
}









