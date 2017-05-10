/*============================================================================*
 *	TREE : tree.c
 *	
 *	Description : Abstract Syntax Tree (AST)
 *				 (Intermediate representation tool for the parser)
 *===========================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include "tree.h"
#include "colorlogs.h"

extern codetable ctable;
extern strtable  table;

#define GLOBALSCOPE NULL

/* string values for ast node types, makes tree output more readable */
char *nodeNames[38] = {"program",
                      "declList",
                      "decl",
                      "varDecl",
                      "arrayDecl",
                      "typeSpecifier",
                      "funDecl",
                      "formalDeclList",
                      "formalDecl",
                      "funBody",
                      "localDeclList",
                      "statementList",
                      "statement",
                      "compoundStmt",
                      "assignStmt",
                      "condStmt",
                      "loopStmt",
                      "returnStmt",
                      "var",
                      "array",
                      "relation (LTE)",
                      "relation (LT)",
                      "relation (GT)",
                      "relation (GTE)",
                      "relation (EQ)",
                      "relation (NEQ)",
                      "relop",
                      "addition",
                      "subtraction",
                      "multiplication",
                      "division",
                      "modulus",
                      "totient",
                      "term",
                      "factor",
                      "character",
                      "funcCallExpr",
                      "argList"
					  };


tree *maketree(int kind) {
  tree *this = (tree *) malloc(sizeof(struct treenode));
  this->nodeKind = kind;
  this->numChildren = 0;
  this->mySymbol = NULL;
  this->flag = INIT_FLAG;
  return this;
}

tree *maketreeWithVal(int kind, int val) {
  tree *this = (tree *) malloc(sizeof(struct treenode));
  this->nodeKind = kind;
  this->numChildren = 0;
  this->mySymbol = NULL;
  this->val = val;
  this->flag = INIT_FLAG;
  return this;
}

tree *maketreeWithSym(int kind, char sym[]) {
  tree *this = (tree *) malloc(sizeof(struct treenode));
  this->nodeKind = kind;
  this->numChildren = 0;
  this->mySymbol = sym;
  this->val = 0;
  this->flag = INIT_FLAG;
  return this;
}

void addChild(tree *parent, tree *child) {
  if (parent->numChildren == MAXCHILDREN) {
    printf("Cannot add child to parent node\n");
    exit(1);
  }
  nextAvailChild(parent) = child;
  parent->numChildren++;
  child->parent = parent;
}

void addChildReverse(tree *parent, tree *child) {
  if (parent->numChildren == MAXCHILDREN) {
    printf("Cannot add child to parent node\n");
    exit(1);
  }
  
  // Swap children forward
  int i;
  for (i = parent->numChildren-1; i >= 0; i--) {
    parent->children[i+1] = parent->children[i];
  }
  
  // Add new child to front
  parent->children[0] = child;
  parent->numChildren++;
  child->parent = parent;
}

void print_ast(tree *node, int nestLevel) {
  
  // Print name of node
  if (node->nodeKind < 38)
    printf("%s", nodeNames[node->nodeKind]);
  else
    LOG_RED("[UNHANDLED] -- %d", node->nodeKind);
  
  // Check for factors with no children (literals)
  if (AST_DEBUG == 2 && !node->mySymbol && !node->numChildren) {
	switch (node->nodeKind)
	{
	  case FACTOR:
	    LOG_CYAN(" %d", node->val);
		break;
	  case CHARNODE:
	    LOG_CYAN(" %c", node->val);
		break;
	}
  }
  
  printf("\n");
  
  int i, j;
  /** Print symbol information **/
  if (AST_DEBUG == 2 && node->mySymbol)
  {
    // ID
    for (j = 0; j < nestLevel; j++) 
  	  printf("  ");
    LOG_YELLOW("%s", symName(node));
    if (node->nodeKind == ARRAYDECL) LOG_YELLOW(" [%i]", node->val);
	printf("\n");
	
	// TYPE
	for (j = 0; j < nestLevel; j++) 
  	  printf("  ");
    void (*LOG_COLOR)(const char*, ...);
	LOG_COLOR = (symKind(node) == FUNCTION_CALL)? &LOG_MAGENTA : &LOG_CYAN;
	switch (symType(node))
	{
	  /** Variables **/
      case NO_TYPE:
	    LOG_RED("[ ]");
	    break;
      case INT_TYPE:
	    LOG_COLOR("int");
	    break;
      case CHAR_TYPE:
	    LOG_COLOR("char");
	    break;
      case VOID_TYPE:
	    LOG_COLOR("void");
	    break;
      case BOOL_TYPE:
	    LOG_COLOR("bool");
	    break;
	  /** Unhandled **/
      default:
	    LOG_RED("[ UNHANDLED TYPE : %d ]", symType(node));
	    break;
	}
	
	printf("\n");
	
	// SCOPE
	tree *traverse = get_scope(node);
	
	/** Declaration Scope **/
	for (j = 0; j < nestLevel; j++) 
  	  printf("  ");
	LOG_COLOR = (symScope(node)) ? &LOG_GREEN : &LOG_RED;
	LOG_COLOR("Declared in: %s\n", (symScope(node)) ? symScope(node) : "NULL");
	
	/** Instigation Scope **/
	for (j = 0; j < nestLevel; j++) 
  	  printf("  ");
	if (traverse == NULL) {
	  LOG_COLOR("Found in:    %s\n", "Global");
	} else {
	  LOG_COLOR("Found in:    %s\n", symName(traverse));
	}
	
	printf("\n");
  }
  
  /** Print children recursively **/
  for (i = 0; i < node->numChildren; i++)  {
    // Indent
    for (j = 0; j < nestLevel; j++) 
      printf("  ");
    
	// Recurse
	print_ast(getChild(node, i), nestLevel + 1);
  }
}

/*============================================================================*
 *	semantic_crawl()
 *		Description: Traverses the AST, disambiguates scopes, and reports
 * 					 warnings/errors.
 * 		
 *===========================================================================*/
void semantic_crawl_quiet(tree* node)
{
  int i;								// iterator
  symrec const *symbol = NULL;			// placeholder
  
  /** Examine symbols with declaration types **/
  if (node->mySymbol)
  {
	// Traverse up to nearest function scope
	tree *traverse = get_scope(node);
	
    // Declaration type nodes:
	if ( node->nodeKind == VARDECL || 
		 node->nodeKind == ARRAYDECL || 
		 node->nodeKind == FORMALDECL )
	{
	  // Error Check :: Multiple Declarations (Variables)
	  if (traverse)
	    symbol = getsym(symName(node), symKind(node), symName(traverse));  // Local scope
	  else
		symbol = getsym(symName(node), symKind(node), "Global");			// Global scope
	  
	  if (symbol != NULL)
		node->flag = MULTIPLE_DECL;
	
	  // Edit scopes
	  if (symScope(node) == NULL) {
		// Write scope to blank symbol
		symScope(node) = (traverse == NULL) ? "Global" : symName(traverse);
	  } else {
	    // Create a new symbol instead of overwriting an existing declaration
		node->mySymbol = putsym(symName(node), symType(node), symKind(node),
						(traverse == NULL) ? "Global" : symName(traverse));
						
	  }
	} else if (node->nodeKind == FUNDECL) { 
	  
	  // Function Declarations can only be global
	  symScope(node) = "Global";
	  // Errors are handled during parsing
	  
	} else {
	  // Update Nondeclaration references
	  if (symScope(node) != symName(traverse)) {
		
		symbol = scope_crawl(node, traverse);
		
	    // Check Attempt Success
		if (symbol != NULL) {
		  node->mySymbol = symbol;
		} else {
		  node->flag = (node->nodeKind == FUNCCALLEXPR)? UNDECL_FUNCTN : UNDECLARED_ID ;
		}
	  }
	}
  }
  
  /** Recurse through children : depth first **/
  for (i = 0; i < node->numChildren; i++)  {
	semantic_crawl_quiet(getChild(node, i));
  }
}

/*============================================================================*
 *	semantic_crawl()
 *		Description: Traverses the AST, disambiguates scopes, and reports
 * 					 warnings/errors.
 * 		
 *===========================================================================*/
void semantic_crawl(tree* node, int* warnings, int* errors)
{
  int i;					// iterator
  tree *traverse;			// crawler
  symrec *temprec = NULL;	// place holder
  
  if (node && node->mySymbol)
  {
	/** Flagged cases **/
    traverse = get_scope(node);
    switch(node->flag)
    {
	  case MULTIPLE_DECL:
        // Multiple declarations in the same scope
        LOG_WHITE("[%i:%i] ", node->linenumb, node->colnumb);
        LOG_CRIM("error: "); LOG_WHITE("%s", symName(node)); printf(" already declared in this scope.\n");
        LOG_YELLOW("%s\n", ctable.entries[node->linenumb]);
		print_cursor(node->colnumb);
        (*errors)++;
        break;
      case UNDECLARED_ID:
        // Undeclared Identifier
		// Double check, its possible the ID was declared after the first pass
        temprec = scope_crawl(node, traverse);
        if (temprec == NULL) 
        {
          LOG_WHITE("[%i:%i] ", node->linenumb, node->colnumb);
          LOG_CRIM("error: "); LOG_WHITE("%s", symName(node)); printf(" undeclared variable.\n");
          LOG_YELLOW("%s\n", ctable.entries[node->linenumb]);
		  print_cursor(node->colnumb);
          (*errors)++;
        } else {
		  print_symbol(temprec);
          node->flag = INIT_FLAG;
        }
        break;
	  case UNDECL_FUNCTN:
        // Undeclared function
		// Double check, its possible the function call may occur before its defintion
        temprec = scope_crawl(node, traverse); 
        if (temprec == NULL) 
        {
          LOG_WHITE("[%i:%i] ", node->linenumb, node->colnumb);
          LOG_CRIM("error: "); LOG_WHITE("%s", symName(node)); printf(" undeclared function.\n");
          LOG_YELLOW("%s\n", ctable.entries[node->linenumb]);
		  print_cursor(node->colnumb);
          (*errors)++;
        } else {
          node->flag = INIT_FLAG;
        }
        break;
    }
	
	/** Array semantics **/
	if (node->nodeKind == ARRAY && node->mySymbol->kind == ARRAY_VAR)
	{
	  // Array Usage :: Literal constant out of bounds
	  if (node->val > node->mySymbol->max) {
	    LOG_WHITE("[%i:%i] ", node->linenumb, node->children[0]->colnumb);
	    LOG_CRIM("error: "); LOG_WHITE("%s[%i]", symName(node), node->val); 
		printf(" array index out of bounds for "); LOG_CRIM("%s[%i]\n", symName(node), node->mySymbol->max);
	    LOG_YELLOW("%s\n", ctable.entries[node->linenumb]);
	    print_cursor(node->children[0]->colnumb);
	    (*errors)++;
	  }
	  
      /** Array :: Index mismatch **/
      {
        // Get characters based on signature legend
        char LHS = 'i';	// Always an integer
        char RHS = post_order_match(node->children[0]);
	  
        // Ignore RHS with mismatch type, denoted '_'
        //			or undefined type, denoted '*'
        if (LHS != RHS && RHS != '_'&& RHS != '*') {
          //LOG_MAGENTA("[%c:%c] \n", LHS, RHS);
          LOG_WHITE("[%i:%i] ", node->children[0]->linenumb, node->children[0]->colnumb);
          LOG_CRIM("error: "); printf("Type mismatch in array index.\n");
          LOG_YELLOW("%s\n", ctable.entries[node->children[0]->linenumb]);
          print_cursor(node->children[0]->colnumb);
          (*errors)++;
        }
      }
	
	} else if (node->nodeKind == VAR && node->mySymbol->kind == ARRAY_VAR) {
	  // Array Usage :: Index missing
      LOG_WHITE("[%i:%i] ", node->linenumb, node->colnumb);
      LOG_CRIM("warning: "); LOG_WHITE("%s", symName(node)); printf(" array used without index. ");
	  LOG_CRIM("(Compiler assumes index 0)\n");
      LOG_YELLOW("%s\n", ctable.entries[node->linenumb]);
	  print_cursor(node->colnumb);
      (*warnings)++;
	  
	  // Augment with implicit index
	  node->nodeKind = ARRAY;
	  addChild(node, maketreeWithVal(FACTOR, 0));
	}
	
	/** Function calls **/
	if (node->nodeKind == FUNCCALLEXPR)
	{
	  if (node->mySymbol->signature_index != -1) {
	    // Check for signature mismatch
	    char* defined_sig = table.entries[node->mySymbol->signature_index];
	    char* caller_sig = check_signature(node);
	    // Signature Debugging
	    if (FSIGN_DEBUG) {
	      LOG_WHITE(" Caller sig : "); 
	  	  LOG_CYAN("%s\n", (strlen(caller_sig) == 0)? "(none)" : caller_sig);
	      LOG_WHITE(" Defined as : "); 
		  LOG_CYAN("%s\n\n", (strlen(defined_sig) == 0)? "(none)" : defined_sig);
	    }
	    // Error message
	    if (sigcmp(defined_sig, caller_sig) != 0) {
		  LOG_WHITE("[%i:%i] ", node->linenumb, node->colnumb);
          LOG_CRIM("error: "); LOG_WHITE("%s", symName(node)); printf(" arguments mismatch.\n");
          LOG_YELLOW("%s\n", ctable.entries[node->linenumb]);
		  print_cursor(node->colnumb);
          (*errors)++;
	    }
	  }
	}
	
  } else if (node) {
	// Nodes without symbols
	
	// Expression :: Type mismatch 
	if (node->flag == EXPR_TYPE_MISMATCH) {
      LOG_WHITE("[%i:%i] ", node->linenumb, node->colnumb);
      LOG_CRIM("warning: "); printf("Type mismatch in expression.\n");
      LOG_YELLOW("%s\n", ctable.entries[node->linenumb]);
	  print_cursor(node->colnumb);
	  (*warnings)++;
	}
	
	/** Assignment :: Type mismatch **/
	if (node->nodeKind == ASSIGNSTMT && node->numChildren == 2)
	{
	  // Get characters based on signature legend
	  char LHS = post_order_match(node->children[0]);
	  char RHS = post_order_match(node->children[1]);
	  
	  // Remove uppercase "array flag"
	  if (node->children[0]->mySymbol && 
		  node->children[0]->mySymbol->kind == ARRAY_VAR
		  && LHS >= 65 && LHS <= 90) 
		  LHS += 32;
	  
	  // Ignore RHS with mismatch type, denoted '_'
	  //			or undefined type, denoted '*'
	  if (LHS != RHS && RHS != '_'&& RHS != '*') {
		//LOG_MAGENTA("[%c:%c] \n", LHS, RHS);
		LOG_WHITE("[%i:%i] ", node->linenumb, node->colnumb);
        LOG_CRIM("warning: "); printf("Type mismatch in assignment.\n");
        LOG_YELLOW("%s\n", ctable.entries[node->linenumb]);
		print_cursor(node->colnumb);
        (*warnings)++;
	  }
	}
	
	/** Return :: Type mismatch **/
	if (node->nodeKind == RETURNSTMT)
	{
	  char declType = post_order_match(get_scope(node)); // Return type
	  char rtrnType;
	  if (node->numChildren != 0)
	  {
		rtrnType = post_order_match(node->children[0]);
	  } else {
		rtrnType = 'v';
	  }
	  
	  if (declType != rtrnType) {
		LOG_WHITE("[%i:%i] ", node->linenumb, node->colnumb);
        LOG_CRIM("warning: "); printf("Return type mismatch.\n");
        LOG_YELLOW("%s\n", ctable.entries[node->linenumb]);
		print_cursor(node->colnumb);
        (*warnings)++;
	  }
	}
	
  }
  
  
  /** Recurse through children : depth first **/
  for (i = 0; i < node->numChildren; i++)  {
	semantic_crawl(getChild(node, i), warnings, errors);
  }
}

/*============================================================================*
 *	scope_crawl()
 *		Description: Crawls through each scope looking for a symbol match.
 * 					 Returns the first matching symbol, or NULL.
 * 		
 *===========================================================================*/
symrec* scope_crawl(tree *node, tree *traverse)
{
  symrec *temprec = NULL;	// placeholder
  
  do {
    // Attempt to find record in local scope
    temprec = getsym(symName(node), symKind(node), symName(traverse));
    
    // Go to next scope up
    if (temprec == NULL) {
      traverse = traverse->parent;
      while ( traverse && traverse->nodeKind != FUNDECL ) {
        traverse = traverse->parent;
      }
    }
  } while (temprec == NULL && traverse);
  
  // Attempt to get symbol in global scope
  if (temprec == NULL) 
    temprec = getsym(symName(node), symKind(node), "Global");

  return temprec;
}


/*============================================================================*
 *	get_scope()
 *		Description: Backtracks up the AST to find the nearest scope.
 *					 Returns the node for that scope.
 * 		
 *===========================================================================*/
tree* get_scope(tree *node)
{
  tree* traverse = node->parent;
  while ( traverse && traverse->nodeKind != FUNDECL )	{
    traverse = traverse->parent;
  }
  return traverse;
}


/*============================================================================*
 *	print_cursor()
 *		Description: Prints a cursor pointer for warning/error messages.
 * 		
 *===========================================================================*/
void print_cursor(int index)
{
  int i;
  for (i = 0; i < index-1; i++)
	printf(" ");
  LOG_CRIM("¯\n");
}


/*============================================================================*
 *	print_symbol()
 *		Description: Prints a an entry from the symbol table.
 * 		
 *===========================================================================*/
void print_symbol(symrec* target)
{
  int i;
  printf(" ");
  LOG_WHITE("%s\n", target->name);
  LOG_CYAN("");
  LOG_CYAN("%s\n", target->scope);
}

/*============================================================================*
 *	print_node()
 *		Description: Prints the name of a node from the AST
 * 		
 *===========================================================================*/
void print_node(tree* node)
{
  LOG_WHITE("%s\n", nodeNames[node->nodeKind]);
}

/*============================================================================*
 *	sigcmp()
 *		Description: Compares signatures of function declarations and function
 * 					 calls.
 *
 *		Returns 0 if compatable, 1 otherwise (to mimic strcmp)
 * 		
 *===========================================================================*/
int sigcmp(char* sigA, char* sigB)
{
  int i;
  int length;
  length = strlen(sigA);
  
  // Check length match  
  if (length != strlen(sigB))
	return 1;

  for (i = 0; i < length; i++)
  {
	if (type_class(sigA[i]) != type_class(sigB[i]))
	  return 1;
  }
  
  return 0;
}

/*============================================================================*
 *	type_class()
 *		Description: Classifies a character into a class for loose typing
 *
 *		Returns integer corresponding to the type class
 * 		
 *===========================================================================*/
int type_class(char sig_char)
{
  switch (sig_char)
  {
	/* Booleans and arrays loosely typed **/
	case 'i':	
	case 'b':
	  return INT_VAR_CLASS;
	case 'I':
	case 'B':
	  return INT_ARRAY_CLASS;
	
	/* All other types are strict */
	case 'v':
	  return VOID_VAR_CLASS;
	case 'V':
	  return VOID_ARRAY_CLASS;
	case 'c':
	  return CHAR_VAR_CLASS;
	case 'C':
	  return CHAR_ARRAY_CLASS;
	default:
	  LOG_CRIM("\n UNHANDLED signature character. (%c)\n", sig_char);
	  return -1;
  }
}

