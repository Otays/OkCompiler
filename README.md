# OKCompiler (0.4.3) README #

OKCompiler is a miniature version of the C language, with a few extra features.<br />
It was developed for educational purposes.

## Language Description ##
__C-Like features:__
 - Conditional statements
 - Iterative control structures
 - Functions
 - Global variables, local variables, and literals
 - Types
   - Int, string, char, bool
 - Operators
   - Addition, denoted by +
   - Subtraction, denoted by -
   - Multiplication, denoted by *
   - Division, denoted by /
   - Modulus, denoted by %<br />
	(Multiplication level precedence) 

__Additional features:<br />
 - Hexadecimal constants, denoted by prefix '0x'<br />
   (e.g. int x; x = 0x5F;)
 - Binary constants, denoted by prefix '0b'<br />
   (e.g. int y; y = 0b11001;)
 - Type "Void"<br />
   (Expiremental data type that challenges the semantic analysis, the  
    implementation does not follow through to code gen)
 - Operators
   - Unary "Phi" operator, aka Eulers totient function.<br />
     Denoted by @(n) for an integer n.
	
 
## Compiler Description ##
__Compiler features:__
 - Compilation runtime is provided
 - Optional color coded console output is available for readability.
 - Extra verbose debug options (these are meant to expose the theory used)
   - Token debug: Isolates compiler to its lexical analysis phase and shows
     class and content information for all tokens.
   - AST debug: Displays full abstract syntax tree, detailing the full topology,
     including symbol references and literal values.
   - Derivation debug: Shows which productions are selected from the grammar
     during bottom up AST construction of the parsing phase.
   - Table debug: Contents for the string table and source code table are 
     printed to the console. 
   - Function signature debug: OKCompiler creates function signatures to
     help match function calls to function definitions, and this mode shows them.  
   - Code generation debug: Prints the generated assembly to the console.

   (These debug options can be modified in the Tree.h file)
   
__Semantic error checking supported:__
 - Undeclared variables and functions
 - Multiply declared variables and functions (No overloading)
 - Argument mismatch, or bad number of arguments in function calls
 - Indexing arrays with non integer types
 - Indexing arrays out of bounds (No dynamic memory, so this is detectable)
 - Type mismatch in assignments
 - Return type mismatch

__Semantics defined by the author:__
 - Integer arithmetic involving string literals will substitute in their length 
 - bool and int are loosely typed
	
__Code Generation:__
 - Supports any number of local variables, global variables, and parameters
 - Compiles in MARS 4.5 (Does not compile in MARS 4.2)
 - The assembly produced is horribly unoptimized so far
	
## Quirks / todo list: ##
 - Symbol table is still not a hash table
 - String table is still not a hash table yet (and uses multiple entries)
 - Need to make a print_symbol_table function
 - Many features excluded from code gen
 
## Usage: ##
 - Highly recommend a black background terminal for the color coded output.<br />
	(I've tested with Putty)
 - Recommend using the test files provided<br />
	./mcc < test.mc

## Required files: ##
 - makefile
 - scanner.l
 - parser.y
 - driver.c
 - tree.h
 - tree.c
 - colorlogs.h
 - colorlogs.c
  








