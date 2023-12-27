// pl0 compiler source code

// #pragma warning(disable:4996)


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "PL0.h"
#include "set.c"

void expression(symset fsys);

//////////////////////////////////////////////////////////////////////
// print error message.
void error(int n)
{
	int i;

	printf("      ");
	for (i = 1; i <= cc - 1; i++)
		printf(" ");
	printf("^\n");
	printf("Error %3d: %s\n", n, err_msg[n]);
	err++; // 全局
} // error

//////////////////////////////////////////////////////////////////////
void getch(void) // getch()为获取单个字符的过程
{
	if (cc == ll) // character count; line length
	{
		if (feof(infile))
		{
			printf("\nPROGRAM INCOMPLETE\n");
			exit(1);
		}
		ll = cc = 0;
		printf("%5d  ", cx);
		while ( (!feof(infile)) // added & modified by alex 01-02-09
			    && ((ch = getc(infile)) != '\n')) 
		{
			printf("%c", ch); // ch:last character read
			line[++ll] = ch;
		} // while
		printf("\n");
		line[++ll] = ' ';
	}
	ch = line[++cc];
} // getch

//////////////////////////////////////////////////////////////////////
// gets a symbol from input stream.
void getsym(void)
{
	int i, k;
	char a[MAXIDLEN + 1]; // MAXIDLEN: length of identifiers

	while (ch == ' '||ch == '\t')
		getch();

	if (isalpha(ch)) //ctype.h中，用于判断字符是否为字母
	{ // symbol is a reserved word or an identifier.
		k = 0;
		do
		{
			if (k < MAXIDLEN)
				a[k++] = ch;
			getch();
		}
		while (isalpha(ch) || isdigit(ch));
		a[k] = 0;
		strcpy(id, a); //id字符串: last identifier read
		word[0] = id; //保留字
		i = NRW;
		while (strcmp(id, word[i--])); //直到相等
		if (++i)
			sym = wsym[i]; // symbol is a reserved word
		else
			sym = SYM_IDENTIFIER;   // symbol is an identifier
	}
	else if (isdigit(ch))
	{ // symbol is a number.
		k = num = 0;
		sym = SYM_NUMBER;
		do
		{
			num = num * 10 + ch - '0';
			k++;
			getch();
		}
		while (isdigit(ch));
		if (k > MAXNUMLEN)
			error(25);     // The number is too great.
	}
	else if (ch == ':')
	{
		getch();
		if (ch == '=')
		{
			sym = SYM_BECOMES; // :=
			getch();
		}
		else
		{
			sym = SYM_NULL;       // illegal?
		}
	}
	else if (ch == '>')
	{
		getch();
		if (ch == '=')
		{
			sym = SYM_GEQ;     // >=
			getch();
		}
		else
		{
			sym = SYM_GTR;     // >
		}
	}
	else if (ch == '<')
	{
		getch();
		if (ch == '=')
		{
			sym = SYM_LEQ;     // <=
			getch();
		}
		else if (ch == '>')
		{
			sym = SYM_NEQ;     // <>
			getch();
		}
		else
		{
			sym = SYM_LES;     // <
		}
	}
	else
	{ // other tokens
		i = NSYM; // maximum number of symbols in array ssym and csym
		csym[0] = ch; //csym: symbol集合，',' '+'...
		while (csym[i--] != ch);
		if (++i)
		{
			sym = ssym[i]; //ssym: sym对应csym的状态集合
			getch();
		}
		else
		{
			printf("Fatal Error: Unknown character.\n");
			exit(1);
		}
	}
} // getsym

//////////////////////////////////////////////////////////////////////
// generates (assembles) an instruction.
void gen(int x, int y, int z)
{
	if (cx > CXMAX) // cx: index of current instruction to be generated.
	{// CXMAX: size of code array
		printf("Fatal Error: Program too long.\n");
		exit(1);
	}
	code[cx].f = x; // f: function code
	code[cx].l = y; // l: level
	code[cx++].a = z; // a: displacement address
} // gen

//////////////////////////////////////////////////////////////////////
// tests if error occurs and skips all symbols that do not belongs to s1 or s2.
void test(symset s1, symset s2, int n) // P14
{//可允许的下一个符号集合 S1 ; 另加的停止符号集合 S2
	symset s;

	if (! inset(sym, s1))
	{
		error(n); //如果当前符号不s1中，当即得到一个错误号；
		s = uniteset(s1, s2);
		while(! inset(sym, s))
			getsym();
		destroyset(s);
	}
} // test

//////////////////////////////////////////////////////////////////////
int dx;  // data allocation index

// enter object(constant, variable or procedre) into table.
void enter(int kind)
{
	mask* mk;

	tx++;
	strcpy(table[tx].name, id); //id字符串: last identifier read
	table[tx].kind = kind;
	switch (kind)
	{
	case ID_CONSTANT:
		if (num > MAXADDRESS)
		{
			error(25); // The number is too great.
			num = 0;
		}
		table[tx].value = num;
		break;
	case ID_VARIABLE:
		mk = (mask*) &table[tx];
		mk->level = level;
		mk->address = dx++;
		break;
	case ID_PROCEDURE:
		mk = (mask*) &table[tx];
		mk->level = level;
		break;
	case ID_ARRAY:
		mk = (mask*)&table[tx];
		mk->level = level;
		mk->address = dx;
		mk->star = cnt_star; //数组元素可能为指针 
		break;
	case ID_POINTER:
		mk = (mask*)&table[tx];
		mk->level = level;
		mk->address = dx++;
		mk->star = cnt_star;
		break;
	} // switch
	cnt_star = 0;
} // enter

//////////////////////////////////////////////////////////////////////
// locates identifier in symbol table.
int position(char* id)
{
	int i;
	strcpy(table[0].name, id);
	i = tx + 1;
	while (strcmp(table[--i].name, id) != 0);
	return i;
} // position

//////////////////////////////////////////////////////////////////////
void constdeclaration()
{
	if (sym == SYM_IDENTIFIER)
	{
		getsym();
		if (sym == SYM_EQU || sym == SYM_BECOMES)
		{
			if (sym == SYM_BECOMES)
				error(1); // Found ':=' when expecting '='.
			getsym();
			if (sym == SYM_NUMBER)
			{
				enter(ID_CONSTANT);
				getsym();
			}
			else
			{
				error(2); // There must be a number to follow '='.
			}
		}
		else
		{
			error(3); // There must be an '=' to follow the identifier.
		}
	} else	error(4);
	 // There must be an identifier to follow 'const', 'var', or 'procedure'.
} // constdeclaration

//////////////////////////////////////////////////////////////////////
void dim_declaration(int dimension) {
	if (dimension > MAX_ARRAY_DIM) {
		error(27); // Array dimension exceeded.
	}
	getsym();
	if (sym == SYM_IDENTIFIER) {
		int i;
		if ((i = position(id)) == 0) {
			error(11); // Undeclared identifier.
		}
		else {
			switch (table[i].kind) {
				case ID_CONSTANT:
					table[tx].dimension[dimension] = table[i].value;
					break;
				case ID_VARIABLE:
					error(28); // Lengthes of array must be constant.
					break;
				case ID_PROCEDURE:
					error(28); // Lengthes of array must be constant.
					break;
			} // switch
		}
	}
	else if (sym == SYM_NUMBER) {
		if (num >= MAX_ARRAY_DIM_LEN) error(29); // The maximum length of each dimension of the array exceeded.
		else {
			table[tx].dimension[dimension] = num;
		}
	}
	else error(28); // Lengthes of array must be constant.

	getsym();
	if (sym == SYM_RBRACK) {
		getsym();
		if (sym == SYM_LBRACK)
			dim_declaration(dimension + 1);
		else if (sym == SYM_COMMA || sym == SYM_SEMICOLON) {
			table[tx].dimension[dimension + 1] = 0;
			int array_size = 1;
			for (int i = 0; table[tx].dimension[i]; ++i)
				array_size *= table[tx].dimension[i];
			dx += array_size;
		}
		else error(30); // The symbol can not follow an array declaration.
	}
}

//////////////////////////////////////////////////////////////////////
void dim_position(symset fsys, int i, int dimension) {//获取数组坐标
	if (table[i].dimension[dimension] == 0)
		error(27); // Array dimension exceeded.
	getsym();
	symset set = uniteset(fsys, createset(SYM_RBRACK, SYM_NULL));
	expression(set);
	destroyset(set);
	if (sym == SYM_RBRACK) {
		gen(OPR, 0, OPR_ADD);
		if (table[i].dimension[dimension + 1] == 0)
			gen(LIT, 0, 1);
		else gen(LIT, 0, table[i].dimension[dimension + 1]);
		gen(OPR, 0, OPR_MUL);
		
		getsym();
		if (sym == SYM_LBRACK)
			dim_position(fsys, i, dimension + 1);
	}
}

//////////////////////////////////////////////////////////////////////
void vardeclaration(void)
{
	cnt_star = 0; // 计算指针类型声明时有几个*

	while(sym == SYM_TIMES){ //指针
		cnt_star ++;
		getsym();
	}
	if (sym == SYM_IDENTIFIER)
	{
		getsym();
		if(cnt_star && (sym == SYM_COMMA || sym == SYM_SEMICOLON)){
			enter(ID_POINTER); //指针
		}
		else if (!cnt_star && (sym == SYM_COMMA || sym == SYM_SEMICOLON)) {
			enter(ID_VARIABLE);
		}
		else if (sym == SYM_LBRACK) {
			enter(ID_ARRAY);
			dim_declaration(0);
	}

	else error(5); // Missing ',' or ';'.
	}
	else
	{
		error(4); // There must be an identifier to follow 'const', 'var', or 'procedure'.
	}
} // vardeclaration

//////////////////////////////////////////////////////////////////////
void listcode(int from, int to) //每一个分程序（过程）被编译结束后，将列出该部分PL/0程序代码
{
	int i;
	
	printf("\n");
	for (i = from; i < to; i++)
	{
		printf("%5d %s\t%d\t%d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
	}
	printf("\n");
} // listcode

void arraysize(mask* mk){ //根据数组生成链表：链表每个结点存有每一维的大小
	int i = 2;
	free(front);
	if(mk->dimension[1]){
		front = rear = (ptr*)malloc(sizeof(struct node));
		rear->dim = mk->dimension[1];
		rear->next = NULL;
		while(mk->dimension[i]){
			rear->next = (ptr*)malloc(sizeof(struct node));
			rear = rear->next;
			rear->dim = mk->dimension[i];
			rear->next = NULL;
			i++;
		}
	}
}

int dequeue(){
	ptr* p = front;
	if(front){ 
		front = front->next;
		p->next = NULL;
		free(p);
		return 1;
	}
	else return 0;
}

//////////////////////////////////////////////////////////////////////
void factor(symset fsys) //因子
{
	void expression(symset fsys);
	int i;
	symset set;
	mask* mk;


	test(facbegsys, fsys, 24); // 24:The symbol can not be as the beginning of an expression.

	if (inset(sym, facbegsys))
	{
		if (sym == SYM_IDENTIFIER)
		{
			if ((i = position(id)) == 0)
			{
				error(11); // Undeclared identifier.
			}
			else
			{
				switch (table[i].kind)
				{
				case ID_CONSTANT:
					gen(LIT, 0, table[i].value);
					getsym();
					break;
				case ID_VARIABLE:
					mk = (mask*) &table[i];
					gen(LOD, level - mk->level, mk->address);
					getsym();
					break;
				case ID_ARRAY:
					getsym();
					if (sym == SYM_LBRACK) {
						mk = (mask*)&table[i];
						gen(LIT, 0, 0);
						dim_position(fsys, i, 0);
						gen(LEA, level - mk->level, mk->address);
						gen(OPR, 0, OPR_ADD);
						gen(LODA, 0, 0);
					}
					else {// access an array directly like a variable.
						mk = (mask*) &table[i];
						arraysize(mk);
						gen(LEA, level - mk->level, mk->address);
					}
					// else error(37); // You cannot access an array directly.
					break;
				case ID_POINTER:
					mk = (mask*) &table[i];
					gen(LOD, level - mk->level, mk->address);
					getsym();
					break;
				case ID_PROCEDURE:
					error(21); // Procedure identifier can not be in an expression.
					getsym();
					break;
				} // switch
			}
		}
		else if (sym == SYM_NUMBER)
		{
			if (num > MAXADDRESS)
			{
				error(25); // The number is too great.
				num = 0;
			}
			gen(LIT, 0, num);
			getsym();
		}
		else if (sym == SYM_LPAREN) // 左括号
		{
			getsym();
			set = uniteset(createset(SYM_RPAREN, SYM_NULL), fsys);
			expression(set);
			destroyset(set);
			if (sym == SYM_RPAREN)
			{
				getsym();
			}
			else
			{
				error(22); // Missing ')'.
			}
		}
		else if(sym == SYM_MINUS) // UMINUS,  Expr -> '-' Expr
		{  
			getsym();
			factor(fsys);
			gen(OPR, 0, OPR_NEG);
		}
		else if(sym == SYM_TIMES) //*
		{
			getsym();
			factor(fsys);
			if(!dequeue()) gen(LODA, 0, 0);
		}
		else if(sym == SYM_AMPERSAND) //&
		{	
			getsym();
			if ((i = position(id)) == 0){
				error(11); // Undeclared identifier.
			}
			else{
				if(sym == ID_VARIABLE || sym == ID_POINTER){
					mk = (mask*) &table[i];
					gen(LEA, level - mk->level, mk->address);
					getsym();
				}
				else{
					error(38); //"The '&' can not be followed by a non-variable."
				}
			}
		}
		test(fsys, createset(SYM_LPAREN, SYM_NULL), 23); //"The symbol can not be followed by a factor."
	} // if
} // factor

//////////////////////////////////////////////////////////////////////
void term(symset fsys) //项
{
	int mulop;
	symset set;

	set = uniteset(fsys, createset(SYM_TIMES, SYM_SLASH, SYM_NULL));
	factor(set);
	while (sym == SYM_TIMES || sym == SYM_SLASH) // slash 斜杠
	{
		mulop = sym;
		getsym();
		factor(set);
		if (mulop == SYM_TIMES)
		{
			gen(OPR, 0, OPR_MUL);
		}
		else
		{
			gen(OPR, 0, OPR_DIV);
		}
	} // while
	destroyset(set);
} // term

//////////////////////////////////////////////////////////////////////
void expression(symset fsys) // 表达式
{
	int addop;
	symset set;
	ptr* p = NULL;

	set = uniteset(fsys, createset(SYM_PLUS, SYM_MINUS, SYM_NULL));
	
	term(set);
	while (sym == SYM_PLUS || sym == SYM_MINUS)
	{
		addop = sym;
		getsym();
		term(set);
		if (addop == SYM_PLUS)
		{	
			if(front) p = front;
			while(p)
			{	gen(LIT, 0, p->dim);
				gen(OPR, 0, OPR_MUL);
				p = p->next;
			}
			gen(OPR, 0, OPR_ADD);
		}
		else
		{
			gen(OPR, 0, OPR_MIN);
		}
	} // while

	destroyset(set);
} // expression

//////////////////////////////////////////////////////////////////////
void condition(symset fsys) // 条件
{
	int relop;
	symset set;

	if (sym == SYM_ODD) //ODD + 表达式
	{
		getsym();
		expression(fsys);
		gen(OPR, 0, 6);
	}
	else
	{
		set = uniteset(relset, fsys); // relset?
		expression(set);
		destroyset(set);
		if (! inset(sym, relset))
		{
			error(20); // Relative operators expected.
		}
		else
		{
			relop = sym;
			getsym();
			expression(fsys);
			switch (relop)
			{
			case SYM_EQU:
				gen(OPR, 0, OPR_EQU);
				break;
			case SYM_NEQ:
				gen(OPR, 0, OPR_NEQ);
				break;
			case SYM_LES:
				gen(OPR, 0, OPR_LES);
				break;
			case SYM_GEQ:
				gen(OPR, 0, OPR_GEQ);
				break;
			case SYM_GTR:
				gen(OPR, 0, OPR_GTR);
				break;
			case SYM_LEQ:
				gen(OPR, 0, OPR_LEQ);
				break;
			} // switch
		} // else
	} // else
} // condition

//////////////////////////////////////////////////////////////////////
void statement(symset fsys) // 语句
{
	int i, cx1, cx2;
	symset set1, set;
	int cnt = 0;

	while(sym == SYM_TIMES){
		cnt ++;
		getsym();
	}
	if (sym == SYM_IDENTIFIER)
	{ // variable assignment
		mask* mk;
		if (! (i = position(id)))
		{
			error(11); // Undeclared identifier.
		}
		else if (table[i].kind == ID_VARIABLE || table[i].kind == ID_POINTER) {
			getsym();
			if (sym == SYM_BECOMES)
			{
				getsym();
			}
			else
			{
				error(13); // ':=' expected.
			}
			mk = (mask*) &table[i];
			if (cnt)
			{	
				gen(LEA, level - mk->level, mk->address);
				while (cnt --)
				{
					gen(LODA, 0, 0);
				}
				expression(fsys);
				gen(STOA, 0, 0);
			}
			else{
				expression(fsys);
				gen(STO, level - mk->level, mk->address);
			}
		}
		else if (table[i].kind == ID_ARRAY) {
			getsym();
			if (sym == SYM_LBRACK) {
				mk = (mask*)&table[i];
				gen(LIT, 0, 0);
				dim_position(fsys, i, 0);
				gen(LEA, level - mk->level, mk->address);
				gen(OPR, 0, OPR_ADD);
				
				if (sym == SYM_BECOMES) {
					getsym();
				}
				else {
					error(13); // ':=' expected.
				}
				expression(fsys);
				if (i) {
					gen(STOA, 0, 0);
				}
			}
			else error(31); // You cannot assign an array directly.
		}
		else {
			error(12); // Illegal assignment.
			i = 0;
		}
	}
	else if (sym == SYM_CALL)
	{ // procedure call
		getsym();
		if (sym != SYM_IDENTIFIER)
		{
			error(14); // There must be an identifier to follow the 'call'.
		}
		else
		{
			if (! (i = position(id)))
			{
				error(11); // Undeclared identifier.
			}
			else if (table[i].kind == ID_PROCEDURE)
			{
				mask* mk;
				mk = (mask*) &table[i];
				gen(CAL, level - mk->level, mk->address);
			}
			else
			{
				error(15); // A constant or variable can not be called. 
			}
			getsym();
		}
	} 
	else if (sym == SYM_IF)
	{ // if statement
		getsym();
		set1 = createset(SYM_THEN, SYM_DO, SYM_NULL);
		set = uniteset(set1, fsys);
		condition(set);
		destroyset(set1);
		destroyset(set);
		if (sym == SYM_THEN)
		{
			getsym();
		}
		else
		{
			error(16); // 'then' expected.
		}
		cx1 = cx; // cx: index of current instruction to be generated.
		gen(JPC, 0, 0);
		statement(fsys);
		code[cx1].a = cx; //a: displacement address
	}
	else if (sym == SYM_BEGIN)
	{ // block
		getsym();
		set1 = createset(SYM_SEMICOLON, SYM_END, SYM_NULL);
		set = uniteset(set1, fsys);
		statement(set);
		while (sym == SYM_SEMICOLON || inset(sym, statbegsys))
		{
			if (sym == SYM_SEMICOLON)
			{
				getsym();
			}
			else
			{
				error(10); //';' expected.
			}
			statement(set);
		} // while
		destroyset(set1);
		destroyset(set);
		if (sym == SYM_END)
		{
			getsym();
		}
		else
		{
			error(17); // ';' or 'end' expected.
		}
	}
	else if (sym == SYM_WHILE)
	{ // while statement
		cx1 = cx; // cx: index of current instruction to be generated.
		getsym();
		set1 = createset(SYM_DO, SYM_NULL);
		set = uniteset(set1, fsys);
		condition(set);
		destroyset(set1);
		destroyset(set);
		cx2 = cx;
		gen(JPC, 0, 0);
		if (sym == SYM_DO)
		{
			getsym();
		}
		else
		{
			error(18); // 'do' expected.
		}
		statement(fsys);
		gen(JMP, 0, cx1);
		code[cx2].a = cx;
	}
	else if (sym == SYM_PRINT) {
		getsym();
		if (sym == SYM_LPAREN) {
			getsym();
			if (sym == SYM_RPAREN) {
				gen(PRT, 255, 0);
			}
			else {
				set1 = createset(SYM_RPAREN, SYM_COMMA, SYM_NULL);
				set = uniteset(set1, fsys);
				expression(set);
				gen(PRT, 0, 0);
				while (sym == SYM_COMMA) {
					getsym();
					expression(set);
					gen(PRT, 0, 0);
				}
				destroyset(set1);
				destroyset(set);
				if (sym == SYM_RPAREN) gen(PRT, 255, 0);
				else error(22); // Missing ')'.
			}
			getsym();
		}
		else error(33); // Missing '('.
	}
	test(fsys, phi, 19);
} // statement
			
//////////////////////////////////////////////////////////////////////
void block(symset fsys)
{
	int cx0; // initial code index
	mask* mk;
	int block_dx;
	int savedTx;
	symset set1, set;


	dx = 3; // 数据单元的下标
	block_dx = dx;
	mk = (mask*) &table[tx];
	mk->address = cx;
	gen(JMP, 0, 0);
	if (level > MAXLEVEL)
	{
		error(37); // There are too many levels.
	}
	do
	{
		if (sym == SYM_CONST) // part 1
		{ // constant declarations
			getsym();
			do
			{
				constdeclaration();
				while (sym == SYM_COMMA)
				{
					getsym();
					constdeclaration();
				}
				if (sym == SYM_SEMICOLON)
				{
					getsym();
				}
				else
				{
					error(5); // Missing ',' or ';'.
				}
			}
			while (sym == SYM_IDENTIFIER);
		} // if

		while (sym == SYM_VAR) // part2
		{ // variable declarations
			getsym();
			do
			{
				vardeclaration();
				while (sym == SYM_COMMA)
				{
					getsym();
					vardeclaration();
				}
				if (sym == SYM_SEMICOLON)
				{
					getsym();
				}
				else
				{
					error(5); // Missing ',' or ';'.
				}
			}
			while (sym == SYM_IDENTIFIER);
		} // if
		block_dx = dx; //save dx before handling procedure call!
		while (sym == SYM_PROCEDURE) //part 3
		{ // procedure declarations
			getsym();
			if (sym == SYM_IDENTIFIER)
			{
				enter(ID_PROCEDURE);
				getsym();
			}
			else
			{
				error(4); // There must be an identifier to follow 'const', 'var', or 'procedure'.
			}


			if (sym == SYM_SEMICOLON)
			{
				getsym();
			}
			else
			{
				error(5); // Missing ',' or ';'.
			}

			level++;
			savedTx = tx;
			set1 = createset(SYM_SEMICOLON, SYM_NULL);
			set = uniteset(set1, fsys);
			block(set);
			destroyset(set1);
			destroyset(set);
			tx = savedTx;
			level--; //level!!!

			if (sym == SYM_SEMICOLON)
			{
				getsym();
				set1 = createset(SYM_IDENTIFIER, SYM_PROCEDURE, SYM_NULL);
				set = uniteset(statbegsys, set1);
				test(set, fsys, 6);
				destroyset(set1);
				destroyset(set);
			}
			else
			{
				error(5); // Missing ',' or ';'.
			}
		} // while
		dx = block_dx; //restore dx after handling procedure call!
		set1 = createset(SYM_IDENTIFIER, SYM_NULL);
		set = uniteset(statbegsys, set1);
		test(set, declbegsys, 7);
		destroyset(set1);
		destroyset(set);
	}
	while (inset(sym, declbegsys));

	code[mk->address].a = cx;
	mk->address = cx;
	cx0 = cx;
	gen(INT, 0, block_dx);
	set1 = createset(SYM_SEMICOLON, SYM_END, SYM_NULL);
	set = uniteset(set1, fsys);
	statement(set); // 语句
	destroyset(set1);
	destroyset(set);
	gen(OPR, 0, OPR_RET); // return
	test(fsys, phi, 8); // test for error: Follow the statement is an incorrect symbol.
	listcode(cx0, cx);
} // block

//////////////////////////////////////////////////////////////////////
int base(int stack[], int currentLevel, int levelDiff)
{
	int b = currentLevel;
	
	while (levelDiff--)
		b = stack[b];
	return b;
} // base

//////////////////////////////////////////////////////////////////////
// interprets and executes codes.
void interpret()
{
	int pc;        // program counter
	int stack[STACKSIZE];
	int top;       // top of stack
	int b;         // program, base, and top-stack register
	instruction i; // instruction register

	printf("Begin executing PL/0 program.\n");

	pc = 0;
	b = 1;
	top = 3;
	stack[1] = stack[2] = stack[3] = 0;
	do
	{
		i = code[pc++]; // i: instruction
		switch (i.f) //f: function code
		{
		case LIT:
			stack[++top] = i.a;
			break;
		case OPR:
			switch (i.a) // operator
			{
			case OPR_RET:
				top = b - 1;
				pc = stack[top + 3];
				b = stack[top + 2];
				break;
			case OPR_NEG:
				stack[top] = -stack[top];
				break;
			case OPR_ADD:
				top--;
				stack[top] += stack[top + 1];
				break;
			case OPR_MIN:
				top--;
				stack[top] -= stack[top + 1];
				break;
			case OPR_MUL:
				top--;
				stack[top] *= stack[top + 1];
				break;
			case OPR_DIV:
				top--;
				if (stack[top + 1] == 0)
				{
					fprintf(stderr, "Runtime Error: Divided by zero.\n");
					fprintf(stderr, "Program terminated.\n");
					continue;
				}
				stack[top] /= stack[top + 1];
				break;
			case OPR_ODD:
				stack[top] %= 2;
				break;
			case OPR_EQU:
				top--;
				stack[top] = stack[top] == stack[top + 1];
				break;
			case OPR_NEQ:
				top--;
				stack[top] = stack[top] != stack[top + 1];
				break;
			case OPR_LES:
				top--;
				stack[top] = stack[top] < stack[top + 1];
				break;
			case OPR_GEQ:
				top--;
				stack[top] = stack[top] >= stack[top + 1];
				break;
			case OPR_GTR:
				top--;
				stack[top] = stack[top] > stack[top + 1];
				break;
			case OPR_LEQ:
				top--;
				stack[top] = stack[top] <= stack[top + 1];
				break;
			} // switch
			break;
		case LOD:
			stack[++top] = stack[base(stack, b, i.l) + i.a];
			break;
		case STO:
			stack[base(stack, b, i.l) + i.a] = stack[top];
			// printf("%d\n", stack[top]);
			top--;
			break;
		case CAL:
			stack[top + 1] = base(stack, b, i.l);
			// generate new block mark
			stack[top + 2] = b;
			stack[top + 3] = pc;
			b = top + 1;
			pc = i.a;
			break;
		case INT:
			top += i.a;
			break;
		case JMP:
			pc = i.a;
			break;
		case JPC:
			if (stack[top] == 0)
				pc = i.a;
			top--;
			break;
		case LEA: // lab1
				stack[++top] = base(stack, b, i.l) + i.a;
				break;
		case LODA: // lab1
			stack[top] = stack[stack[top]];
			break;
		case STOA:
			stack[stack[top - 1]] = stack[top];
			top -= 2;
			break;
		case PRT: // lab1
			if (i.l == 255) putchar('\n');
			else printf("%d ", stack[top--]);
			break;
		} // switch
	}
	while (pc);

	printf("End executing PL/0 program.\n");
} // interpret

//////////////////////////////////////////////////////////////////////
int main ()
{
	FILE* hbin;
	char s[80];
	int i;
	symset set, set1, set2;

	printf("Please input source file name: "); // get file name to be compiled
	scanf("%s", s);
	if ((infile = fopen(s, "r")) == NULL)
	{
		printf("File %s can't be opened.\n", s);
		exit(1);
	}

	phi = createset(SYM_NULL);
	relset = createset(SYM_EQU, SYM_NEQ, SYM_LES, SYM_LEQ, SYM_GTR, SYM_GEQ, SYM_NULL);
	
	// create begin symbol sets
	declbegsys = createset(SYM_CONST, SYM_VAR, SYM_PROCEDURE, SYM_NULL);
	statbegsys = createset(SYM_BEGIN, SYM_CALL, SYM_IF, SYM_WHILE, SYM_NULL);
	facbegsys = createset(SYM_IDENTIFIER, SYM_NUMBER, SYM_LPAREN, SYM_MINUS, SYM_TIMES, SYM_AMPERSAND, SYM_NULL);

	err = cc = cx = ll = 0; // initialize global variables
	ch = ' ';
	kk = MAXIDLEN;

	getsym();

	set1 = createset(SYM_PERIOD, SYM_NULL);
	set2 = uniteset(declbegsys, statbegsys);
	set = uniteset(set1, set2);
	block(set);
	destroyset(set1);
	destroyset(set2);
	destroyset(set);
	destroyset(phi);
	destroyset(relset);
	destroyset(declbegsys);
	destroyset(statbegsys);
	destroyset(facbegsys);

	if (sym != SYM_PERIOD)
		error(9); // '.' expected.
	if (err == 0)
	{
		hbin = fopen("hbin.txt", "w");
		for (i = 0; i < cx; i++)
			fwrite(&code[i], sizeof(instruction), 1, hbin);
		fclose(hbin);
	}
	if (err == 0)
		interpret();
	else
		printf("There are %d error(s) in PL/0 program.\n", err);
	listcode(0, cx);
} // main

//////////////////////////////////////////////////////////////////////
// eof pl0.c
