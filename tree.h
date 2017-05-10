/*============================================================================*
 *	TREE HEADER : tree.h
 *				  Abstract Syntax Tree (AST)
 *===========================================================================*/
#ifndef TREE_H
#define TREE_H

#define MAXCHILDREN 25

/** ================================ Debug ================================ **/
#define TOKEN_DEBUG 0	// Set to 0 to hide tokenizer
						// 		  1 to show tokenizer
#define AST_DEBUG 	2	// Set to 0 to hide abstract syntax tree
						// 		  1 to show abstract syntax tree
						// 		  2 to show more info (symbols, constants)
#define DDEBUG 		0	// Set to 0 to hide derivation
						//		  1 to show derivation
#define TABLE_DEBUG	0	// Set to 0 to hide source code and id string tables
						//		  1 to show tables
#define FSIGN_DEBUG	0	// Set to 0 to hide function signature matching
						//		  1 to show matching
#define CGEN_DEBUG	1   // Set to 0 to hide assembly from console
						//		  1 to show assembly in console
#define COLOR_MODE	1   // Set to 0 to use all gray terminal text
						//		  1 to use colors in terminal
/** ======================================================================= **/


/** ============= Tree Node ============= **/
typedef struct treenode tree; 
struct treenode // Tree Node (Non-Terminal)
{
  int nodeKind;		// Type of node  (by index)
  int numChildren;	// Children count
  int val;			// Used for array length
  
  /** Warning & Error info **/
  int   flag;		// used to mark error nodes
  int 	linenumb;   // line number
  int 	colnumb;    // column number
  
  struct symrec *mySymbol;
  tree *parent;
  tree *children[MAXCHILDREN];
};
/** ===================================== **/


/** =========== Symbol Record =========== **/
typedef struct symrec symrec;
struct symrec // Symbol Record (Terminal Node)
{
  char *name;  				// name of symbol
  int 	type;    			// type of symbol
  int 	kind;    			// var, array, fnctn
  char *scope;    			// scope of symbol
  int   index;				// MIPS AR offset
  
  void* symdata;			// stores variable from code
  
  /*
  union {
    double var;           	// value of a VAR 
    double (*fnctptr)();  	// value of a FNCT    
  } value;
  */
  
  // Might want to union these, they are mutually exclusive
  int signature_index;		// function signature lookup
  int max;    				// max value of array
  
  struct tree *parent;		// parent node
  symrec *next;    			// link field
  
};  
/** ===================================== **/

/** ========= String Lookup Table ========= **/
struct strtable 
{
  char **entries;  			// c strings
  int 	 count;    			// used
  int 	 capacity;    		// total
};  
typedef struct strtable strtable;
/** ======================================= **/

/** ========== Code Lookup Table ========== **/
struct codetable 
{
  char **entries;  			// c strings
  int 	 count;    			// used
  int 	 capacity;    		// total
};  
typedef struct codetable codetable;
/** ======================================= **/


/* builds sub tree with zero children  */
tree *maketree(int kind);

/* builds sub tree with leaf node */
tree *maketreeWithVal(int kind, int val);

void addChild(tree *parent, tree *child);

void print_ast(tree *root, int nestLevel);

void semantic_crawl_quiet(tree *node);

void semantic_crawl(tree *node, int* warnings, int* errors);

/* Crawls through each scope looking for a symbol match. */
symrec* scope_crawl(tree *node, tree *traverse);

/* Backtracks up the AST to find the nearest scope node. */
tree* get_scope(tree *node);

/* signature checking */
int sigcmp(char* sigA, char* sigB);
int type_class(char sig_char);


/* tree manipulation macros */ 
/* if you are writing your compiler in C, you would want to have a large collection of these */

#define nextAvailChild(node) node->children[node->numChildren] 
#define getChild(node, index) node->children[index]

#define symName(node) 	node->mySymbol->name
#define symType(node) 	node->mySymbol->type
#define symKind(node) 	node->mySymbol->kind
#define symScope(node) 	node->mySymbol->scope
#define symIndex(node) 	node->mySymbol->index


/** tree fields **/
enum nodeTypes 	 {PROGRAM,
				  DECLLIST,
				  DECL,
				  VARDECL,
				  ARRAYDECL,
				  TYPESPECIFIER,
				  FUNDECL,
				  FORMALDECLLIST,
				  FORMALDECL,
				  FUNBODY,
				  LOCALDECLLIST,
				  STATEMENTLIST,
				  STATEMENT,
				  COMPOUNDSTMT,
				  ASSIGNSTMT,
				  CONDSTMT,
				  LOOPSTMT,
				  RETURNSTMT,
				  VAR,
				  ARRAY,
				  RELATION_LTE,
				  RELATION_LT,
				  RELATION_GT,
				  RELATION_GTE,
				  RELATION_EQ,
				  RELATION_NEQ,
				  RELOP,
				  ADDITION,
				  SUBTRACTION,
				  MULTIPLICATION,
				  DIVISION,
				  MODULUS,
				  TOTIENT,
				  TERM,
				  FACTOR,
				  CHARNODE,
				  FUNCCALLEXPR,
				  ARGLIST
				  };

/** Symbol fields **/
enum dataType {NO_TYPE,
			   VOID_TYPE,
			   BOOL_TYPE,
			   INT_TYPE,
			   CHAR_TYPE,
			   STRING_TYPE
			  };

enum symKinds {NORMAL_VAR,
			   ARRAY_VAR,
			   FUNCTION_CALL
			   };

enum opFlags  	{GTE,
				 GT,
				 LT,
				 LTE,
				 EQ,
				 NEQ,
				 MULT,
				 DIVD,
				 MODU,
				 ADDI,
				 SUBT
				};

enum errorFlags  {INIT_FLAG,
				  UNDECLARED_ID,
				  UNDECL_FUNCTN,
				  MULTIPLE_DECL,
				  EXPR_TYPE_MISMATCH
				 };

enum typeClasses  {INT_VAR_CLASS,
				   INT_ARRAY_CLASS,
				   VOID_VAR_CLASS,
				   VOID_ARRAY_CLASS,
				   CHAR_VAR_CLASS,
				   CHAR_ARRAY_CLASS
				  };

#endif









