#include <stdio.h>

#define NRW        12     // number of reserved words
#define TXMAX      500    // length of identifier table
#define MAXNUMLEN  14     // maximum number of digits in numbers
#define NSYM       13     // maximum number of symbols in array ssym and csym
#define MAXIDLEN   10     // length of identifiers

#define MAXADDRESS 32767  // maximum address
#define MAXLEVEL   32     // maximum depth of nesting block
#define CXMAX      500    // size of code array

#define MAXSYM     30     // maximum number of symbols  

#define STACKSIZE  10000   // maximum storage

#define MAX_ARRAY_DIM_LEN 32 //
#define MAX_ARRAY_DIM 8 // 

#define MAX_POINTER_DEP 10 

typedef enum symtype
{
	SYM_NULL,
	SYM_IDENTIFIER,
	SYM_NUMBER,
	SYM_PLUS,
	SYM_MINUS,
	SYM_TIMES,
	SYM_SLASH,
	SYM_ODD,
	SYM_EQU,
	SYM_NEQ,
	SYM_LES,
	SYM_LEQ,
	SYM_GTR,
	SYM_GEQ,
	SYM_LPAREN,
	SYM_RPAREN,
	SYM_COMMA,
	SYM_SEMICOLON,
	SYM_PERIOD,
	SYM_BECOMES,
    SYM_BEGIN,
	SYM_END,
	SYM_IF,
	SYM_THEN,
	SYM_WHILE,
	SYM_DO,
	SYM_CALL,
	SYM_CONST,
	SYM_VAR,
	SYM_PROCEDURE,
	SYM_PRINT,	// print
	SYM_LBRACK, // [
	SYM_RBRACK, // ]
	SYM_AMPERSAND // &
}symtype;

typedef enum idtype
{
	ID_CONSTANT, ID_VARIABLE, ID_PROCEDURE, ID_ARRAY, ID_POINTER
}idtype;

typedef enum opcode
{
	LIT, OPR, LOD, STO, CAL, INT, JMP, JPC,
	PRT, // 
	LODA, // 
	LEA, // 
	STOA, // 
}opcode;

enum oprcode
{
	OPR_RET, OPR_NEG, OPR_ADD, OPR_MIN,
	OPR_MUL, OPR_DIV, OPR_ODD, OPR_EQU,
	OPR_NEQ, OPR_LES, OPR_LEQ, OPR_GTR,
	OPR_GEQ
};


typedef struct
{
	int f; // function code
	int l; // level
	int a; // displacement address
} instruction;

//////////////////////////////////////////////////////////////////////
char* err_msg[] =
{
/*  0 */    "",
/*  1 */    "Found ':=' when expecting '='.",
/*  2 */    "There must be a number to follow '='.",
/*  3 */    "There must be an '=' to follow the identifier.",
/*  4 */    "There must be an identifier to follow 'const', 'var', or 'procedure'.",
/*  5 */    "Missing ',' or ';'.",
/*  6 */    "Incorrect procedure name.",
/*  7 */    "Statement expected.",
/*  8 */    "Follow the statement is an incorrect symbol.",
/*  9 */    "'.' expected.",
/* 10 */    "';' expected.",
/* 11 */    "Undeclared identifier.",
/* 12 */    "Illegal assignment.",
/* 13 */    "':=' expected.",
/* 14 */    "There must be an identifier to follow the 'call'.",
/* 15 */    "A constant or variable can not be called.",
/* 16 */    "'then' expected.",
/* 17 */    "';' or 'end' expected.",
/* 18 */    "'do' expected.",
/* 19 */    "Incorrect symbol.",
/* 20 */    "Relative operators expected.",
/* 21 */    "Procedure identifier can not be in an expression.",
/* 22 */    "Missing ')'.",
/* 23 */    "The symbol can not be followed by a factor.",
/* 24 */    "The symbol can not be as the beginning of an expression.",
/* 25 */    "The number is too great.",

/* 26 */    "NEW ERROR",
/* 27 */    "Array dimension exceeded.",
/* 28 */    "Lengthes of array must be constant.",
/* 29 */    "The maximum length of each dimension of the array exceeded.",
/* 30 */    "The symbol can not follow an array declaration.",
/* 31 */    "You cannot assign an array directly.",
/* 32 */    "There are too many levels.",
/* 33 */	"Missing '('.",
/* 34 */	"Missing ':'.",
/* 35 */	"Missing 'var'.",
/* 36 */	"Missing ','.",
/* 37 */    "There are too many levels."
/* 38 */    "The '&' can not be followed by a non-variable."

};

//////////////////////////////////////////////////////////////////////
char ch;         // last character read
int  sym;        // last symbol read
char id[MAXIDLEN + 1]; // last identifier read
int  num;        // last number read
int  cc;         // character count
int  ll;         // line length
int  kk;
int  err;
int  cx;         // index of current instruction to be generated.
int  level = 0;
int  tx = 0;

char line[80];

instruction code[CXMAX];

char* word[NRW + 1] =
{
	"", /* place holder */
	"begin", "call", "const", "do", "end","if",
	"odd", "procedure", "then", "var", "while",
	"print"
};

int wsym[NRW + 1] =
{
	SYM_NULL, SYM_BEGIN, SYM_CALL, SYM_CONST, SYM_DO, SYM_END,
	SYM_IF, SYM_ODD, SYM_PROCEDURE, SYM_THEN, SYM_VAR, SYM_WHILE,
	SYM_PRINT
};

int ssym[NSYM + 1] =
{
	SYM_NULL, SYM_PLUS, SYM_MINUS, SYM_TIMES, SYM_SLASH,
	SYM_LPAREN, SYM_RPAREN, SYM_EQU, SYM_COMMA, SYM_PERIOD, SYM_SEMICOLON,
	SYM_LBRACK, SYM_RBRACK, SYM_AMPERSAND
};

char csym[NSYM + 1] =
{
	' ', '+', '-', '*', '/', '(', ')', '=', ',', '.', ';',
	'[', ']', '&'
};

#define MAXINS   12
char* mnemonic[MAXINS] =
{
	"LIT", "OPR", "LOD", "STO", "CAL", "INT", "JMP", "JPC",
	"PRT", "LODA", "LEA", "STOA"
};

typedef struct
{
	char name[MAXIDLEN + 1];
	int  kind;
	int  value;
	int  dimension[MAX_ARRAY_DIM]; //每一维的大小
	int  star;//指针*嵌套层数

} comtab;

comtab table[TXMAX];

typedef struct
{
	char  name[MAXIDLEN + 1];
	int   kind;
	short level;
	short address;
	int  dimension[MAX_ARRAY_DIM];
	int  star;//指针*嵌套层数

} mask;

int cnt_star;//指针类型有几个*

typedef struct node{//每个依次记录数组每一维的大小
	int dim; //该维大小
	struct node *next;
}ptr;
ptr* front = NULL;
ptr* rear = NULL;

FILE* infile;

// EOF PL0.h
