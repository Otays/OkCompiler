/**===========================================================================*
 *    PARSER : parser.y
 *             This is a context free grammar implemented with yacc/bison.
 *==========================================================================**/
 
%{
#include <stdio.h>
#include <limits.h>
#include "tree.h"
#include "y.tab.h"
#include "colorlogs.h"
#define  DEFAULT_COL -1

extern int yylineno;
extern int yycol;
extern char* yycodeline;

int lastID;

tree *ast;                  /* pointer to AST root */
strtable table;              /* lookup table for IDs and string literals */
codetable ctable;              /* lookup table for original compilation code */

/* prototypes */
char post_order_match(tree* node);

%}

/** Token Types **/
%union 
{
  int value;
  int literal[2];    // literal[0]  := literal value
                    // literal[1]  := column index for warnings/errors
  int table_id[2];    // table_id[0] := string table index
                    // table_id[1] := column index for warnings/errors
  struct treenode *node;
  struct symrec   *symbol;
}

/* === AST Nodes ============================================================*/
/** Non Terminals **/
%type <node> program declList decl varDecl typeSpecifier funDecl formalDeclList
%type <node> funBody localDeclList statementList statement expression addExpr
%type <node> addExpr formalDecl compoundStmt condStmt loopStmt returnStmt
%type <node> term factor funcCallExpr argList assignStmt constValue

/** Flag passer nodes **/
%type <value> addop mulop relop

/** Symbol reference nodes **/
%type <symbol> var

//=============================================================================

/** Named tokens **/
%token <table_id> ID

/** Constant tokens **/    
%token <literal> INTCONST CHARCONST HEXCONST BINCONST BOOLCONST 
%token <table_id> STRCONST

/** Keyword Types **/
%token <value> KWD_INT KWD_CHAR KWD_VOID KWD_BOOL 

/** Typeless **/
%token COMMA SEMICLN
%token LSQ_BRKT RSQ_BRKT
%token LCRLY_BRKT RCRLY_BRKT
%right LPAREN RPAREN KWD_IF KWD_ELSE
%token KWD_WHILE KWD_STRING KWD_RETURN

/**    Operator Precedence, Ascending */
%left <value> OPER_ADD OPER_SUB
%left <value> OPER_MUL OPER_DIV OPER_MOD 
%left <value> OPER_LT OPER_GT OPER_GTE OPER_LTE OPER_EQ OPER_NEQ 
%right <value> EULERS_TOTIENT
%token <value> OPER_ASGN

/** Illegal token errors **/
%token <table_id> ILLEGAL_TOK

/** Initial Production **/
%start program 

%%

program         : declList
                  {
                    if (DDEBUG) printf("declList\n");
                    tree *progNode = maketree(PROGRAM);
                    addChild(progNode, $1);
                    ast = progNode;
                  }
                ;
declList         : decl
                  {
                    if (DDEBUG) printf("decl\n");
                    tree *declListNode = maketree(DECLLIST);
                    addChild(declListNode, $1);
                    $$ = declListNode;
                  }
                | declList decl
                  {
                    if (DDEBUG) printf("declList decl\n");
                    addChild($1, $2);
                    $$ = $1;
                  }
                ;
decl             : varDecl
                  {
                    if (DDEBUG) printf("varDecl\n");
                    tree *declNode = maketree(DECL);
                    addChild(declNode, $1);
                    $$ = declNode;
                  }
                | funDecl
                  {
                    if (DDEBUG) printf("funDecl\n");
                    tree *declNode = maketree(DECL);
                    addChild(declNode, $1);
                    $$ = declNode;
                  }
                ;
varDecl         : typeSpecifier ID LSQ_BRKT constValue RSQ_BRKT SEMICLN
                  {
                    /** ===== Array declaration ====== **/
                    if (DDEBUG) printf("typeSpecifier ID LSQ_BRKT constValue RSQ_BRKT SEMICLN\n");
                    
                    struct symrec *ptr = getsym(table.entries[$2[0]], ARRAY_VAR, NULL);    // Lookup symbol ID
                    if (!ptr) ptr = putsym(table.entries[$2[0]], $1, ARRAY_VAR, 0);    // Create new symbol
                    tree *someNode = maketreeWithSym(ARRAYDECL, ptr);                // Attach symbol to table
                    someNode->val = $4->val;                                        // Assign array length
                    ptr->max = $4->val;                                                // Assign max array access
                    update_debug(someNode, $2[1]);
                    
                    $$ = someNode;
                    
                    /** ============================== **/
                  }
                | typeSpecifier ID SEMICLN
                  {
                    /** ===== Variable Declaration ====== **/
                    if (DDEBUG) printf("typeSpecifier ID SEMICLN\n");
                    
                    struct symrec *ptr;// = getsym(table.entries[$2[0]], NORMAL_VAR, NULL);  // Lookup symbol ID
                    /* if (!ptr) */ 
                    ptr = putsym(table.entries[$2[0]], $1, NORMAL_VAR, 0);                    // Create new symbol
                    tree *someNode = maketreeWithSym(VARDECL, ptr);                            // Attach symbol to table
                    
                    update_debug(someNode, $2[1]);
                    
                    $$ = someNode;
                    /** ================================= **/
                  }
                ;
typeSpecifier     : KWD_INT
                  {
                    if (DDEBUG) printf("KWD_INT\n");
                    $$ = $1;
                  }
                | KWD_CHAR
                  {
                    if (DDEBUG) printf("KWD_CHAR\n");
                    $$ = $1;
                  }
                | KWD_VOID
                  {
                    if (DDEBUG) printf("KWD_VOID\n");
                    $$ = $1;
                  }
                | KWD_BOOL
                  {
                    if (DDEBUG) printf("KWD_BOOL\n");
                    $$ = $1;
                  }
                ;
funDecl         : typeSpecifier ID LPAREN formalDeclList RPAREN funBody
                  {
                    /** =================== Declare Function ================== **/
                    if (DDEBUG) printf("typeSpecifier ID LPAREN formalDeclList RPAREN funBody\n");
                    struct symrec *ptr = getsym(table.entries[$2[0]], FUNCTION_CALL, NULL);
                    int errcode = 0;
                    if (!ptr) 
                    {
                      ptr = putsym(table.entries[$2[0]], (int)$1, FUNCTION_CALL, 0);
                    } else {
                      errcode = modsym(ptr, (int)$1);
                    }
                    
                    if (!errcode) ptr->signature_index = string_table_insert(make_signature($4));
                      
                    tree *someNode = maketreeWithSym(FUNDECL, ptr);
                    if (errcode) someNode->flag = MULTIPLE_DECL;
                    update_debug(someNode, $2[1]);
                    
                    addChild(someNode, $4);
                    addChild(someNode, $6);
                    
                    $$ = someNode;
                  }
                | typeSpecifier ID LPAREN RPAREN funBody
                  {
                    /** ========== Declare Function :: No Arguments ========= **/
                    if (DDEBUG) printf("typeSpecifier ID LPAREN RPAREN funBody\n");
                    
                    struct symrec *ptr = getsym(table.entries[$2[0]], FUNCTION_CALL, NULL);
                    int errcode = 0;
                    if (!ptr) 
                    {
                      ptr = putsym(table.entries[$2[0]], (int)$1, FUNCTION_CALL, 0);
                    } else {
                      errcode = modsym(ptr, (int)$1);
                    }
                    if (!errcode) ptr->signature_index = string_table_insert("\0");
                    tree *someNode = maketreeWithSym(FUNDECL, ptr);
                    if (errcode) someNode->flag = MULTIPLE_DECL;
                    addChild(someNode, $5);
                    update_debug(someNode, $2[1]);
                    
                    $$ = someNode;
                  }
                ;
formalDeclList     : formalDecl
                  {
                    if (DDEBUG) LOG_GOLD("formalDecl\n");
                    tree *someNode = maketree(FORMALDECLLIST);
                    addChild(someNode, $1);
                    //update_debug(someNode, DEFAULT_COL);
                    $$ = someNode;
                  }
                | formalDeclList COMMA formalDecl
                  {
                    if (DDEBUG) LOG_GOLD("formalDecl COMMA formalDeclList\n");
                    addChild($1, $3);
                    $$ = $1;
                  }
                ;
formalDecl         : typeSpecifier ID
                  {
                    /** ============== Argument Declaration ============== **/
                    if (DDEBUG) printf("typeSpecifier ID\n");
                    
                    struct symrec *ptr;
                    ptr = putsym(table.entries[$2[0]], $1, NORMAL_VAR, 0);
                    tree *someNode = maketreeWithSym(FORMALDECL, ptr);
                    update_debug(someNode, $2[1]);
                    
                    $$ = someNode;
                  }
                | typeSpecifier ID LSQ_BRKT RSQ_BRKT
                  {
                    /** ========== Argument Declaration :: Array ========== **/
                    if (DDEBUG) printf("typeSpecifier ID LSQ_BRKT RSQ_BRKT\n");
                    
                    struct symrec *ptr;
                    ptr = putsym(table.entries[$2[0]], $1, ARRAY_VAR, 0);
                    tree *someNode = maketreeWithSym(FORMALDECL, ptr);
                    update_debug(someNode, $2[1]);
                    
                    ptr->max = INT_MAX;
                    $$ = someNode;
                  }
                ;
funBody         : LCRLY_BRKT localDeclList statementList RCRLY_BRKT
                  {
                    if (DDEBUG) printf("LCRLY_BRKT localDeclList statementList RCRLY_BRKT\n");
                    tree *someNode = maketree(FUNBODY);
                    update_debug(someNode, DEFAULT_COL);
                    if ($2 != NULL) addChild(someNode, $2);
                    if ($3 != NULL) addChild(someNode, $3);
                    $$ = someNode;
                  }
                ;
localDeclList     :
                  {
                    if (DDEBUG) printf("{E}\n");
                    $$ = NULL;
                  }
                | localDeclList varDecl
                  {
                    if (DDEBUG) printf("varDecl localDeclList\n");
                    tree *someNode = 0;
                    if ($1 != NULL) {
                      addChild($1, $2);
                    } else {
                      someNode = maketree(LOCALDECLLIST);
                      update_debug(someNode, DEFAULT_COL);
                      addChild(someNode, $2);
                    }
                    $$ = ($1 == NULL) ? someNode : $1;
                  }
                ;
statementList     :
                  {
                    if (DDEBUG) printf("{E}\n");
                    $$ = NULL;
                  }
                | statementList statement
                  {
                    if (DDEBUG) printf("statement statementList\n");
                    tree *someNode = 0;
                    if ($1 != NULL) {
                      addChild($1, $2);
                    } else {
                      someNode = maketree(STATEMENTLIST);
                      update_debug(someNode, DEFAULT_COL);
                      if ($2 != NULL) addChild(someNode, $2);
                    }
                    $$ = ($1 == NULL) ? someNode : $1;
                  }
                ;
statement         : compoundStmt { /** Crutch nodes **/ }
                | assignStmt
                | condStmt
                | loopStmt
                | returnStmt
                ;
compoundStmt     : LCRLY_BRKT statementList RCRLY_BRKT
                  {
                    if (DDEBUG) printf("LCRLY_BRKT statementList RCRLY_BRKT\n");
                    tree *someNode = maketree(COMPOUNDSTMT);
                    if ($2 != NULL) addChild(someNode, $2);
                    update_debug(someNode, DEFAULT_COL);
                    $$ = someNode;
                  }
                ;
assignStmt         : var OPER_ASGN expression SEMICLN
                  {
                    /** ============== Assignment Statement ============== **/
                    if (DDEBUG) printf("var OPER_ASGN expression SEMICLN\n");
                    // Note: $1 here is a ptr to a symbol (struct symrec *ptr)
                    tree *someNode = maketree(ASSIGNSTMT);
                    update_debug(someNode, $2);
                    addChild(someNode, $1);
                    addChild(someNode, $3);
                    $$ = someNode;
                  }
                | expression SEMICLN
                  {
                    /** Degenerate expression? **/
                    if (DDEBUG) printf("expression SEMICLN\n");
                    tree *someNode = maketree(ASSIGNSTMT);
                    update_debug(someNode, DEFAULT_COL);
                    addChild(someNode, $1);
                    $$ = someNode;
                  }
                ;
condStmt         : KWD_IF LPAREN expression RPAREN statement KWD_ELSE statement     
                  {
                    if (DDEBUG) printf("KWD_IF LPAREN expression RPAREN statement KWD_ELSE statement \n");
                    tree *someNode = maketree(CONDSTMT);
                    update_debug(someNode, DEFAULT_COL);
                    addChild(someNode, $3);
                    addChild(someNode, $5);
                    addChild(someNode, $7);
                    $$ = someNode;
                  }
                | KWD_IF LPAREN expression RPAREN statement     
                  {
                    if (DDEBUG) printf("KWD_IF LPAREN expression RPAREN statement \n");
                    tree *someNode = maketree(CONDSTMT);
                    update_debug(someNode, DEFAULT_COL);
                    addChild(someNode, $3);
                    addChild(someNode, $5);
                    $$ = someNode;
                  }                 
                ;
loopStmt         : KWD_WHILE LPAREN expression RPAREN statement
                  {
                    if (DDEBUG) printf("KWD_WHILE LPAREN expression RPAREN statement\n");
                    tree *someNode = maketree(LOOPSTMT);
                    update_debug(someNode, DEFAULT_COL);
                    addChild(someNode, $3);
                    addChild(someNode, $5);
                    $$ = someNode;
                  }
                ;
returnStmt         : KWD_RETURN SEMICLN
                  {
                    if (DDEBUG) printf("KWD_RETURN SEMICLN\n");
                    $$ = maketree(RETURNSTMT);
                  }
                | KWD_RETURN expression SEMICLN
                  {
                    if (DDEBUG) printf("KWD_RETURN expression SEMICLN\n");
                    tree *someNode = maketree(RETURNSTMT);
                    update_debug(someNode, DEFAULT_COL);
                    addChild(someNode, $2);
                    $$ = someNode;
                  }
                ;
var             : ID
                  {
                    /** ======================= Variable ======================= **/
                    if (DDEBUG) printf("ID\n");
                    
                    struct symrec *ptr = getsym(table.entries[$1[0]], NORMAL_VAR, NULL);
                    if (!ptr) ptr = putsym(table.entries[$1[0]], 0, NORMAL_VAR, 0);
                    lastID = $1[1];
                    
                    tree *someNode = maketreeWithSym(VAR, ptr);
                    update_debug(someNode, $1[1]);
                    
                    $$ = someNode;
                  }
                | ID LSQ_BRKT addExpr RSQ_BRKT
                  {
                    /** =================== Variable :: Array =================== **/
                    if (DDEBUG) printf("ID LSQ_BRKT addExpr RSQ_BRKT\n");
                    
                    struct symrec *ptr = getsym(table.entries[$1[0]], ARRAY_VAR, NULL);
                    if (!ptr) ptr = putsym(table.entries[$1[0]], 0, ARRAY_VAR, 0); // $3
                    lastID = $1[1];
                    
                    tree *someNode = maketreeWithSym(ARRAY, ptr);
                    someNode->val = $3->val;
                    addChild(someNode, $3);
                    update_debug(someNode, $1[1]);
                    
                    $$ = someNode;
                  }
                ;
expression      : addExpr
                  {
                    if (DDEBUG) printf("addExpr\n");
                    $$ = $1;
                  }
                | expression relop addExpr
                  {
                    if (DDEBUG) printf("expression relop addExpr\n");
                    tree *someNode;
                    switch ((int)$2)
                    {
                      case LTE:
                        someNode = maketree(RELATION_LTE);
                        break;
                      case LT:
                        someNode = maketree(RELATION_LT);
                        break;
                      case GT:
                        someNode = maketree(RELATION_GT);
                        break;
                      case GTE:
                        someNode = maketree(RELATION_GTE);
                        break;
                      case EQ:
                        someNode = maketree(RELATION_EQ);
                        break;
                      case NEQ:
                        someNode = maketree(RELATION_NEQ);
                        break;
                      default:
                        LOG_RED("Unhandled relationship");
                    }
                    update_debug(someNode, DEFAULT_COL);
                    addChild(someNode, $1);
                    addChild(someNode, $3);
                    $$ = someNode;
                  }
                ;
relop             : OPER_LTE
                  {
                    if (DDEBUG) printf("OPER_LTE\n");
                    $$ = LTE;
                  }
                | OPER_LT
                  {
                    if (DDEBUG) printf("OPER_LT\n");
                    $$ = LT;
                  }
                | OPER_GT
                  {
                    if (DDEBUG) printf("OPER_GT\n");
                    $$ = GT;
                  }
                | OPER_GTE
                  {
                    if (DDEBUG) printf("OPER_GTE\n");
                    $$ = GTE;
                  }
                | OPER_EQ
                  {
                    if (DDEBUG) printf("OPER_EQ\n");
                    $$ = EQ;
                  }
                | OPER_NEQ
                  {
                    if (DDEBUG) printf("OPER_NEQ\n");
                    $$ = NEQ;
                  }
                ;
addExpr         : term
                  {
                    if (DDEBUG) printf("term\n");
                    if ($1->nodeKind != FUNCCALLEXPR) update_debug($1, lastID);  // Test
                    $$ = $1;    /** Always returns treenode **/
                  }
                | addExpr addop term
                  {
                    if (DDEBUG) printf("addExpr addop term\n");
                    
                    tree *someNode;
                    switch ((int)$2)
                    {
                        case ADDI:    // OPER_ADD
                            someNode = maketree(ADDITION);
                            break;
                        case SUBT:    // OPER_SUB
                            someNode = maketree(SUBTRACTION);
                            break;
                    }
                    
                    update_debug(someNode, lastID); // Test
                    addChild(someNode, $1);
                    addChild(someNode, $3);
                    $$ = someNode;
                  }
                ;
addop             : OPER_ADD
                  {
                    if (DDEBUG) printf("OPER_ADD\n");
                    $$ = ADDI;
                  }
                | OPER_SUB
                  {
                    if (DDEBUG) printf("OPER_SUB\n");
                    $$ = SUBT;
                  }
                ;
term             : factor
                  {
                    if (DDEBUG) printf("factor\n");
                    /*
                    A term is composed of factors
                        e.g.: 5*x, y*x*x, 4*f(x), etc
                    */
                    if ($1->nodeKind != FUNCCALLEXPR) update_debug($1, lastID);    // Test
                    $$ = $1;
                  }
                | term mulop factor
                  {
                    if (DDEBUG) printf("term mulop factor\n");
                    
                    tree *someNode;
                    switch ($2)
                    {
                        case MULT:  // OPER_MUL
                            someNode = maketree(MULTIPLICATION);
                            break;
                        case DIVD:    // OPER_DIV
                            someNode = maketree(DIVISION);
                            break;
                        case MODU:    // OPER_MOD
                            someNode = maketree(MODULUS);
                            break;
                    }
                    
                    update_debug(someNode, lastID); // Test
                    addChild(someNode, $1);
                    addChild(someNode, $3);
                    $$ = someNode;
                  }
                ;
mulop             : OPER_MUL 
                  {
                    if (DDEBUG) printf("OPER_MUL\n");
                    $$ = MULT;
                  }
                | OPER_DIV
                  {
                    if (DDEBUG) printf("OPER_DIV\n");
                    $$ = DIVD;
                  }
                | OPER_MOD
                  {
                    if (DDEBUG) printf("OPER_MOD\n");
                    $$ = MODU;
                  }
                ;
factor             : LPAREN expression RPAREN 
                  {
                    if (DDEBUG) printf("LPAREN expression RPAREN\n");
                    $$ = $2;
                  }
                | var 
                  {
                    if (DDEBUG) printf("var\n");
                    tree *someNode = maketree(FACTOR);
                    addChild(someNode, $1);
                    update_debug(someNode, lastID);
                    $$ = someNode;
                  }
                | funcCallExpr 
                  {
                    if (DDEBUG) printf("funcCallExpr\n");
                    $$ = $1;
                  }
                | CHARCONST 
                  {
                    if (DDEBUG) printf("CHARCONST\n");
                    lastID = $1[1];
                    $$ = maketreeWithVal(CHARNODE, $1[0]); 
                  }
                | STRCONST 
                  {
                    if (DDEBUG) printf("STRCONST\n");
                    $$ = maketreeWithVal(FACTOR, strlen(table.entries[$1[0]]));    // String Length Placeholder
                  }
                | constValue 
                  {
                    if (DDEBUG) printf("constValue\n");
                    $$ = $1; 
                  }
                | EULERS_TOTIENT LPAREN expression RPAREN
                  {
                  /***********************************************************
                       --- TOTIENT EXAMPLES ---
                       
                    9 = 32, φ(9) = 9 * (1-1/3) = 6

                    4 = 22, φ(4) = 4 * (1-1/2) = 2

                    15 = 3*5, φ(15) = 15 * (1-1/3)*(1-1/5) = 15*(2/3)*(4/5) = 8
                    
                  ************************************************************/
                    
                    if (DDEBUG) printf("EULERS_TOTIENT\n");
                    tree *someNode = maketree(TOTIENT);
                    update_debug(someNode, DEFAULT_COL);
                    addChild(someNode, $3);
                    $$ = someNode;
                  }
                ;
constValue        : INTCONST 
                  {
                    /** All of these should work in array declarations, they are constant **/
                    if (DDEBUG) printf("INTCONST\n");
                    lastID = $1[1];
                    $$ = maketreeWithVal(FACTOR, $1[0]); 
                  }
                | HEXCONST 
                  {
                    if (DDEBUG) printf("HEXCONST\n");
                    lastID = $1[1];
                    $$ = maketreeWithVal(FACTOR, $1[0]);
                  }
                | BINCONST 
                  {
                    if (DDEBUG) printf("BINCONST\n");
                    lastID = $1[1];
                    $$ = maketreeWithVal(FACTOR, $1[0]);
                  }
                | BOOLCONST 
                  {
                    if (DDEBUG) printf("BOOLCONST\n");
                    lastID = $1[1];
                    $$ = maketreeWithVal(FACTOR, $1[0]);
                  }
                ;
funcCallExpr     : ID LPAREN argList RPAREN
                  {
                    if (DDEBUG) printf("ID LPAREN argList RPAREN\n");
                    
                    struct symrec *ptr = getsym(table.entries[$1[0]], FUNCTION_CALL, NULL);
                    if (ptr == NULL) ptr = putsym(table.entries[$1[0]], (int)0, FUNCTION_CALL, 0);
                    
                    tree *someNode = maketreeWithSym(FUNCCALLEXPR, ptr);
                    update_debug(someNode, $1[1]);
                    
                    addChild(someNode, $3);
                    $$ = someNode;
                  }
                | ID LPAREN RPAREN
                  {
                    if (DDEBUG) printf("ID LPAREN RPAREN\n");
                    
                    struct symrec *ptr = getsym(table.entries[$1[0]], FUNCTION_CALL, NULL);
                    if (!ptr) ptr = putsym(table.entries[$1[0]], (int)0, FUNCTION_CALL, 0);
                    
                    tree *someNode = maketreeWithSym(FUNCCALLEXPR, ptr);
                    update_debug(someNode, $1[1]);
                    $$ = someNode;
                  }
                ;
argList         : expression
                  {
                    if (DDEBUG) printf("expression\n");
                    tree *someNode = maketree(ARGLIST);
                    update_debug(someNode, DEFAULT_COL);
                    addChild(someNode, $1);
                    $$ = someNode;
                  }
                | argList COMMA expression
                  {
                    if (DDEBUG) printf("argList COMMA expression\n");
                    addChild($1, $3);
                    $$ = $1;
                  }
                ;

%%

/*=============================================================================
 * Reports parsing error to console
 *===========================================================================*/
int yyerror(char *msg) {
  LOG_YELLOW(" Syntax error (line %d)\n", yylineno);
}

/*=============================================================================
 * Displays some initial parser information
 *===========================================================================*/
extern void parserinit () {
  if (DDEBUG) 
    printf("[Derivation debug: on]\n\n");
  else 
    printf("[Derivation debug: off]\n");
}

/*=============================================================================
 * Column and line number tracking for semantic errors
 *===========================================================================*/
void update_debug(tree* node, int col_override)
{
  node->linenumb = yylineno;
  
  if (col_override == -1)
    node->colnumb = yycol - 1; // Subtract for lookahead
  else
    node->colnumb = col_override;
  
}


/*=============================================================================
 * Inserts a line into the string table
 *===========================================================================*/
int string_table_insert(char* item)
{
  if (!item) return NULL;
  
  // Resize
  if (table.count >= table.capacity)
  {
    table.capacity *= 2;
    table.entries = (char*)realloc(table.entries, sizeof(char*) * table.capacity);
  }
  
  // Non empty line
  table.entries[table.count] = malloc(sizeof(char*) * (strlen(item)+1));
  strncpy(table.entries[table.count], item, (strlen(item)+1));
    
  table.count++;
  return table.count-1;
}

/*=============================================================================
 * Inserts a line into the code table
 *===========================================================================*/
void code_table_insert(char* item)
{
  if (!yylineno) return;
  if (!item) return;
  
  // Resize
  if (yylineno >= ctable.count)
  {
    ctable.count *= 2;
    ctable.entries = (char*)realloc(ctable.entries, sizeof(char*) * ctable.count);
  }
  
  // Insert item
  ctable.entries[yylineno] = malloc(strlen(item));
  strcpy(ctable.entries[yylineno], item);
}

/*=============================================================================
 * Returns a character for a function signature
 *
 *   ________________________    
 *  |         LEGEND         |
 *  |    Bool       :   b    |
 *  |    Char       :   c    |
 *  |    Int        :   i    |
 *  |    Void       :   v    |
 *  |                        |
 *  |    Bool[]     :   B    |
 *  |    Char[]     :   C    |
 *  |    Int[]      :   I    |
 *  |    Void[]     :   V    |
 *  |                        |
 *  |  TYPE ERRORS           |
 *  |    Mismatch   :   *    |
 *  |    Undeclared :   _    |
 *  |                        |
 *   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
 *
 *===========================================================================*/
char get_sig_char(symrec* symbol)
{
  if (symbol == NULL) return 'i';    // Integer literals have no symbols
  
  _Bool array = (symbol->kind == ARRAY_VAR);
  switch (symbol->type)
  {
    case BOOL_TYPE: return (array)? 'B' : 'b';
    case CHAR_TYPE: return (array)? 'C' : 'c';
    case INT_TYPE:  return (array)? 'I' : 'i';
    case VOID_TYPE: return (array)? 'V' : 'v';
    default:
      //LOG_CRIM("\nSignature write failed, unhandled type\n");
      break;
  }
  
  return '*';
}

/*=============================================================================
 * Creates a function signature
 *===========================================================================*/
char* make_signature(tree* node)
{
  char* signature;
  int i;
  //int index = 0;
  //int size = 2;
  
  if (node->nodeKind != FORMALDECLLIST) {
    LOG_RED("\n Bad call to make_signature.\n");
    LOG_GOLD("\t( Must use FORMALDECLLIST node )\n\n");
    exit(-1);
    return NULL;
  }
  
  signature = malloc(sizeof(char*) * node->numChildren);
  for (i = 0; i < node->numChildren; i++) {
    signature[i] = get_sig_char(node->children[i]->mySymbol);
  }
  
  return signature;
}


/*=============================================================================
 * Creates a signature for any function call expression
 *===========================================================================*/
char* check_signature(tree* node)
{
  char* signature;
  int i;
  tree* arglist;
  if (node->numChildren > 0)
  {
    arglist = node->children[0];
  } else {
    return table.entries[string_table_insert("\0")];
  }
  
  if (node->nodeKind != FUNCCALLEXPR) {
    LOG_RED("\n Bad call to check_signature.\n");
    LOG_GOLD("\t( Must use FUNCCALLEXPR node )\n\n");
    exit(-1);
    return NULL;
  }
  
  signature = malloc(sizeof(char*) * arglist->numChildren);
  for (i = 0; i < arglist->numChildren; i++) {
    signature[i] = post_order_match(arglist->children[i]);
  }
  
  return signature;
}

/*=============================================================================
 * Evaluates the type of an expression given a root.
 *        Return type is a character representing the type (for signatures)
 *===========================================================================*/
char post_order_match(tree* node)
{
  char type1;
  char type2;
  
  switch (node->nodeKind)
  {
    /** Unary Non Terminals **/
    case FACTOR:
      if (node->numChildren != 0)
        return get_sig_char(node->children[0]->mySymbol);
      else
        return get_sig_char(node->mySymbol);
    
    case CHARNODE:    return 'c';
    case VAR:          return get_sig_char(node->mySymbol);
    case ARRAY:          return get_sig_char(node->mySymbol);
    
    /** Binary Non Terminals **/
    case ADDITION:
    case RELOP:
    case SUBTRACTION:
    case MULTIPLICATION:
    case DIVISION:
    case MODULUS:
      type1 = post_order_match(node->children[0]);
      type2 = post_order_match(node->children[1]);
      break;
    
    /** Comparison **/
    case RELATION_LTE:    
    case RELATION_LT:    
    case RELATION_GT:    
    case RELATION_GTE:    
    case RELATION_EQ:    
    case RELATION_NEQ:    
      // Catch nested mismatching
      type1 = post_order_match(node->children[0]);
      type2 = post_order_match(node->children[1]);
      //...
      
      // Return bool always
      return 'b';
    
    /** Function terminals **/
    case FUNDECL:        return get_sig_char(node->mySymbol);
    case FUNCCALLEXPR:     return get_sig_char(node->mySymbol);
    case TOTIENT:          return 'i';
    default:
      LOG_CRIM("\n UNHANDLED node in post_order_match (%i)\n", node->nodeKind);
      break;
  }
  
  /** Type matching **/
  if (type1 == type2)
  {
    return type1;
  } else {
    /** This is where we would decide to be strong or weakly typed **/
    
    // Show error or warning
    node->flag = EXPR_TYPE_MISMATCH;
    return '_';
  }
}

/** ========== OLD tokendef.h ========== **

// major stuff //
#define ID         251
#define INTCONST   252
#define CHARCONST  253
#define STRCONST   254

// keywords //
#define KWD_IF     255
#define KWD_ELSE   256
#define KWD_WHILE  257
#define KWD_INT    258
#define KWD_STRING 259
#define KWD_CHAR   260
#define KWD_RETURN 261
#define KWD_VOID   262

#define KWD_BOOL   303    // Custom token
#define BOOLCONST  304    // Custom token 
#define BINCONST   305    // Custom token
#define HEXCONST   306    // Custom token
#define OPER_MOD   307    // Custom token

#define EULERS_TOTIENT   307    // Custom token

// operators //
#define OPER_ADD    263
#define OPER_SUB    264
#define OPER_MUL    265
#define OPER_DIV    266
#define OPER_LT     267
#define OPER_GT     268
#define OPER_GTE    269
#define OPER_LTE    270
#define OPER_EQ     271
#define OPER_NEQ    272
#define OPER_ASGN    273

// brackets & parens //
#define LSQ_BRKT    274
#define RSQ_BRKT    275
#define LCRLY_BRKT  276
#define RCRLY_BRKT  277
#define LPAREN     278
#define RPAREN     279

// punctuation //
#define COMMA      280
#define SEMICLN    281

#define ILLEGAL_TOK 301
#define ERROR       302

**/



