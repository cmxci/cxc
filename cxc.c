#include <math.h>
#include <stdlib.h>
#include <stdint.h>

#define T double
#define SIZE 0x10000

typedef union {
	T tvalue;
	uint64_t intvalue;
	char charvalue;
	char* strvalue;
} operand;

typedef enum {
	OP_ADD,
	OP_SUB,
	OP_MUL,
	OP_DIV,
	OP_REM,
	OP_PSHT,
	OP_PSHI,
	OP_PSHC,
	OP_PSHSTR,
	OP_TPRINT,
	OP_IPRINT,
	OP_CPRINT,
	OP_STRPRINT,
	OP_TGET,
	OP_IGET,
	OP_CGET,
	OP_STRGET,
	OP_FREESTR,
	OP_AND,
	OP_OR,
	OP_XOR,
	OP_NOT,
	OP_EXP,
	OP_SIN,
	OP_COS,
	OP_TAN,
	OP_RT,
	OP_TEST,
	OP_IFBEGIN,
	OP_IFEND,
	OP_LOOPBEGIN,
	OP_LOOPEND,
	OP_FDEFBEGIN,
	OP_FDEFEND,
	OP_JMP,
	OP_STORE,
	OP_LOAD
} optype;

typedef struct {
	optype op;
	operand arg;
} operation;

typedef struct {
	operand data[SIZE];
	unsigned long sp;
} stack;

void push(stack* s, T i) {
	if ((s->sp) < SIZE) {
		s->data[s->sp] = i;
		(s->sp)++;
	}
}

void reset(stack* s) {
	(s->sp) = 0;
}

void init(stack** s) {
	*s = malloc(sizeof(stack));
	reset(*s);
}

T pop(stack* s) {
	if ((s->sp) > 0) {
		(s->sp)--;
		return s->data[(s->sp)];
	}
	return 0;
}

T peek(stack* s) {
	if ((s->sp) > 0) return s->data[(s->sp) - 1];
	return 0;
}

stack* working_stack;
stack* call_stack;
struct {
	operation code[SIZE];
	unsigned long ip = 0;
} state;

void readop(FILE* f) {
	char c = fgetc(f);
	while (isspace(c)) {
		c = fgetc(f);
	}
	operation op;
	switch(c) {
		case '+':
			op.op = OP_ADD;
			break;
		case '-':
			op.op = OP_SUB;
			break;
		case '*':
			if (fgetc(f) == '*') op.op = OP_EXP;
			else op.op = OP_MUL;
			break;
		case '/':
			op.op = OP_DIV;
			break;
		case '%':
			op.op = OP_REM;
			break;
		case '&':
			op.op = OP_AND;
			break;
		case '|':
			op.op = OP_OR;
			break;
		case '^':
			op.op = OP_XOR;
			break;
		case 'R':
			op.op = OP_RT;
			break;
		case '.':
			switch(fgetc(f)) {
				case 'F':
					op.op = OP_TPRINT;
					break;
				case 'I':
					op.op = OP_IPRINT;
					break;
				case 'C':
					op.op = OP_CPRINT;
					break;
				case 'S':
					op.op = OP_STRPRINT;
					break;
			}
			break;
		case ',':
			switch(fgetc(f)) {
				case 'F':
					op.op = OP_TGET;
					break;
				case 'I':
					op.op = OP_IGET;
					break;
				case 'C':
					op.op = OP_CGET;	
					break;
				case 'S':
					op.op = OP_STRGET;
					break;
			}
			break;
		case 'f':
			op.op = OP_FREESTR;
			break;
		case 'F':
			op.op = OP_PSHT;
			fscanf(f, "%lf", &(op.arg.tvalue));
			break;
		case 'I':
			op.op = OP_PSHI;
			fscanf(f, "%ld", &(op.arg.intvalue));
			break;
		case 'C':
			op.op = OP_CGET;
			if (fgetc(f) == '\'') op.arg.charvalue = fgetc(f);
			else if (fgetc(f) == 'x') scanf("%hhx", &(op.arg.charvalue));
			break;
		case 'S':
			op.op = OP_STRGET;
			op.arg.strvalue = (char*)malloc(256);
			fscanf(f, "%255[^\n]" op.arg.strvalue);
			break;
		case '_':
			char opstr[4];
			fgets(opstr, 3, f);
			if (!strcmp(opstr, "sin")) op.op = OP_SIN;
			if (!strcmp(opstr, "cos")) op.op = OP_COS;
			if (!strcmp(opstr, "tan")) op.op = OP_TAN;
			break;
		case '?':
			op.op = OP_TEST;
			break;
		case '(':
			op.op = OP_IFBEGIN;
			break;
		case ')':
			op.op = OP_IFEND;
			break;
		case '[':
			op.op = OP_LOOPBEGIN;
			break;
		case ']':
			op.op = OP_LOOPEND;
			break;
		case '{':
			op.op = OP_FDEFBEGIN;
			break;
		case '}':
			op.op = OP_FDEFEND;
			break;
		case ':':
			op.op = OP_FCALL;
			break;
		case 's':
			op.op = OP_STORE;
			op.arg.charvalue = fgetc(f);
			break;
		case 'l':
			op.op = OP_LOAD;
			op.arg.charvalue = fgetc(f);
	}
	if (loc < SIZE) state.code[state.ip] = op;
	// else error
	state.ip++;
}

void parseop() {
	switch (state.code[state.ip].op) {
		case OP_ADD:
			operand result = {.tvalue = pop(working_stack).tvalue + pop(working_stack).tvalue};
			push(working_stack, result);
			break;
		case OP_SUB:
			T sub1 = pop(working_stack).tvalue;
			T sub2 = pop(working_stack).tvalue;
			operand result = {.tvalue = sub1 % sub2};
			push(working_stack, result);
			break;
		case OP_MUL:
			operand result = {.tvalue = pop(working_stack).tvalue * pop(working_stack).tvalue};
			push(working_stack, result);
			break;
		case OP_DIV:
			T div1 = pop(working_stack).tvalue;
			T div2 = pop(working_stack).tvalue;
			operand result = {.tvalue = div1 % div2};
			push(working_stack, result);
			break;
		case OP_REM:
			T rem1 = pop(working_stack).tvalue;
			T rem2 = pop(working_stack).tvalue;
			operand result = {.tvalue = rem1 % rem2};
			push(working_stack, result);
			break;
		case OP_EXP:
			T exp1 = pop(working_stack).tvalue;
			T exp2 = pop(working_stack).tvalue;
			operand result = {.tvalue = pow(exp1, exp2)};
			push(working_stack, result);
			break;
		case OP_RT:
			T rt1 = pop(working_stack).tvalue;
			T rt2 = pop(working_stack).tvalue;
			operand result = {.tvalue = pow(rt1, 1 / rt2)};
			push(working_stack, result);
			break;
		case OP_SIN:
			operand result = {.tvalue = sin(pop(working_stack).tvalue)};
			push(working_stack, result);
			break;
		case OP_COS:
			operand result = {.tvalue = tan(pop(working_stack).tvalue)};
			push(working_stack, result);
			break;
		case OP_TAN:
			operand result = {.tvalue = tan(pop(working_stack).tvalue)};
			push(working_stack, result);
			break;
		case OP_AND:
			operand result = {.intvalue = pop(working_stack).intvalue & pop(working_stack).intvalue};
			push(working_stack, result);
			break;
		case OP_OR:
			operand result = {.intvalue = pop(working_stack).intvalue | pop(working_stack).intvalue};
			push(working_stack, result);
			break;
		case OP_XOR:
			operand result = {.intvalue = pop(working_stack).intvalue ^ pop(working_stack).intvalue};
			push(working_stack, result);
			break;
		case OP_PSHT:
		case OP_PSHI:
		case OP_PSHC:
		case OP_PSHSTR:
			push(working_stack, state.code[state.ip].op.arg);
			break;
		case OP_TGET:
			scanf("%lf", &(op.arg.tvalue));
			break;
		case OP_IGET:
			scanf("%lx", &(op.arg.tvalue));
			break;
		case OP_CGET:
			if (fgetc(f) == '\'') op.arg.charvalue = fgetc(f);
			else if (fgetc(f) == 'x') scanf("%hhx", &(op.arg.charvalue));
			break;
		case OP_STRGET:
			op.arg.strvalue = (char*)malloc(256);
			scanf("%255[^\n]", op.arg.strvalue);
			break;
		case TPRINT:
			break;
		case IPRINT:
			break;
		case CPRINT:
			break;
		case STRPRINT:
			break;
	}
}
