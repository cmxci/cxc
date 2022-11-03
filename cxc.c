#include <math.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

#define T double
#define SIZE 0x100000

typedef union {
	T tvalue;
	uint64_t intvalue;
	char charvalue;
	uint16_t regvalue;
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
	OP_IFBEGIN,
	OP_IFEND,
	OP_LOOPBEGIN,
	OP_LOOPEND,
	OP_FDEFBEGIN,
	OP_FDEFEND,
	OP_FCALL,
	OP_JMP,
	OP_PUSHIP,
	OP_STORE,
	OP_LOAD,
	OP_DUP,
	OP_TESTEQ,
	OP_TESTGT,
	OP_TESTLT,
	OP_EXIT
} optype;

typedef struct {
	optype op;
	operand arg;
} operation;

typedef struct entry {
	char* key;
	uint64_t value;
	struct entry* next;
} entry;

typedef struct hashtable {
	int size;
	struct entry** table; 
} hashtable;

hashtable* ht_create(int size) {
	hashtable* ht = NULL;
	int i;
	if (size < 1) return NULL;
	if ((ht = malloc(sizeof(hashtable))) == NULL) {
		free(ht);
		return NULL;
	}
	if ((ht->table = malloc(sizeof(entry*) * size)) == NULL) {
		free(ht);
		return NULL;
	}
	for (i = 0; i < size; i++) {
		ht->table[i] == NULL;
	}
	ht->size = size;
	return ht;
}

int32_t ht_hash(size_t ht_size, char* key) {
	uint32_t hash = 0, g;
	register char* p;
	for (p = key; *p; p++) {
		hash = (hash << 4) + *p;
		if ((g = hash & 0xf0000000)) {
			hash ^= g >> 24;
			hash ^= g;
		}
	}
	return hash % ht_size;
}

entry* ht_newpair(char* key, uint64_t value) {
	entry* newpair;
	if ((newpair = malloc(sizeof(entry))) == NULL) return NULL;
	if ((newpair->key = strdup(key)) == NULL) return NULL;
	newpair->value = value;
	newpair->next = NULL;
	return newpair;
}

void ht_set(hashtable* ht, char* key, uint64_t value) {
	int bin = 0;
	entry *newpair = NULL;
	entry *next = NULL;
	entry *last = NULL;
	bin = ht_hash(ht->size, key);
	next = ht->table[bin];
	while (next != NULL && next->key != NULL && strcmp(key, next->key) > 0) {
		last = next;
		next = next->next;
	}
	if (next != NULL && next->key != NULL && strcmp(key, next->key) == 0) {
		next->value = value;
	}
	else {
		newpair = ht_newpair(key, value);
		if (next == ht->table[bin]) {
			newpair->next = next;
			ht->table[bin] = newpair;
		}
		else if (next == NULL) last->next = newpair;
		else {
			newpair->next = next;
			last->next = newpair;
		}
	}
}

uint64_t ht_get(hashtable* ht, char* key) {
	int bin = 0;
	entry* pair;
	bin = ht_hash(ht->size, key);
	pair = ht->table[bin];
	while (pair != NULL && pair->key != NULL && strcmp(key, pair->key) > 0) {
		pair = pair->next;
	}
	if (pair == NULL || pair->key == NULL || strcmp(key, pair->key) != 0) return 0;
	else return pair->value;
}

typedef struct {
	operand data[SIZE];
	unsigned long sp;
} stack;

void push(stack* s, operand i) {
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

operand pop(stack* s) {
	if ((s->sp) > 0) {
		(s->sp)--;
		return s->data[(s->sp)];
	}
	return (operand){.intvalue = 0};
}

operand peek(stack* s) {
	if ((s->sp) > 0) return s->data[(s->sp) - 1];
	return (operand){.intvalue = 0};
}

stack* working_stack;
stack* call_stack;
operand regs[0x10000];
hashtable* functab;

struct {
	operation code[SIZE];
	uint64_t ip;
} state = {
	.ip = 0
};

void readop(FILE* f) {
	char c = fgetc(f);
	while (isspace(c)) {
		c = fgetc(f);
	}
	if (feof(f)) return;
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
			else {
				fseek(f, -1, SEEK_CUR);
				op.op = OP_MUL;
			}
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
			op.op = OP_PSHC;
			if (fgetc(f) == '\'') op.arg.charvalue = fgetc(f);
			else if (fgetc(f) == 'x') scanf("%hhx", &(op.arg.charvalue));
			break;
		case '"':
			op.op = OP_PSHSTR;
			op.arg.strvalue = (char*)malloc(256);
			fscanf(f, "%255[^\"]", op.arg.strvalue);
			break;
		case '_':
			char opstr[4];
			fgets(opstr, 3, f);
			if (!strcmp(opstr, "sin")) op.op = OP_SIN;
			if (!strcmp(opstr, "cos")) op.op = OP_COS;
			if (!strcmp(opstr, "tan")) op.op = OP_TAN;
			break;
		case '=':
			op.op = OP_TESTEQ;
			break;
		case '>': 
			op.op = OP_TESTGT;
			break;
		case '<':
			op.op = OP_TESTLT;
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
			op.arg.regvalue = (fgetc(f) << 8) & fgetc(f);
			break;
		case 'j':
			op.op = OP_JMP;
			break;
		case '$':
			op.op = OP_PUSHIP;
			break;
		case 'l':
			op.op = OP_LOAD;
			op.arg.regvalue = (fgetc(f) << 8) & fgetc(f);
			break;
		case 'd':
			op.op = OP_DUP;
			break;
		case 'q':
			op.op = OP_EXIT;
			break;
	}
	if (state.ip < SIZE) state.code[state.ip] = op;
	else exit(1);
	state.ip++;
}

void tonext(optype skipop, optype op) {
	uint64_t skip = 0;
	state.ip++;
	while (state.code[state.ip].op != op || skip > 0) {
		if (state.code[state.ip].op == skipop) skip += 1;
		state.ip++;
	}
	state.ip--;
}

void parseop() {
	operand result, jloc;
	switch (state.code[state.ip].op) {
		case OP_ADD:
			result = (operand){.tvalue = pop(working_stack).tvalue + pop(working_stack).tvalue};
			push(working_stack, result);
			break;
		case OP_SUB:
			T sub1 = pop(working_stack).tvalue;
			T sub2 = pop(working_stack).tvalue;
			result = (operand){.tvalue = sub2 - sub1};
			push(working_stack, result);
			break;
		case OP_MUL:
			result = (operand){.tvalue = pop(working_stack).tvalue * pop(working_stack).tvalue};
			push(working_stack, result);
			break;
		case OP_DIV:
			T div1 = pop(working_stack).tvalue;
			T div2 = pop(working_stack).tvalue;
			result = (operand){.tvalue = div2 / div1};
			push(working_stack, result);
			break;
		case OP_REM:
			uint64_t rem1 = pop(working_stack).intvalue;
			uint64_t rem2 = pop(working_stack).intvalue;
			result = (operand){.intvalue = rem2 % rem1};
			push(working_stack, result);
			break;
		case OP_EXP:
			T exp1 = pop(working_stack).tvalue;
			T exp2 = pop(working_stack).tvalue;
			result = (operand){.tvalue = pow(exp2, exp1)};
			push(working_stack, result);
			break;
		case OP_RT:
			T rt1 = pop(working_stack).tvalue;
			T rt2 = pop(working_stack).tvalue;
			result = (operand){.tvalue = pow(rt2, 1 / rt1)};
			push(working_stack, result);
			break;
		case OP_SIN:
			result = (operand){.tvalue = sin(pop(working_stack).tvalue)};
			push(working_stack, result);
			break;
		case OP_COS:
			result = (operand){.tvalue = tan(pop(working_stack).tvalue)};
			push(working_stack, result);
			break;
		case OP_TAN:
			result = (operand){.tvalue = tan(pop(working_stack).tvalue)};
			push(working_stack, result);
			break;
		case OP_AND:
			result = (operand){.intvalue = pop(working_stack).intvalue & pop(working_stack).intvalue};
			push(working_stack, result);
			break;
		case OP_OR:
			result = (operand){.intvalue = pop(working_stack).intvalue | pop(working_stack).intvalue};
			push(working_stack, result);
			break;
		case OP_XOR:
			result = (operand){.intvalue = pop(working_stack).intvalue ^ pop(working_stack).intvalue};
			push(working_stack, result);
			break;
		case OP_PSHT:
		case OP_PSHI:
		case OP_PSHC:
		case OP_PSHSTR:
			push(working_stack, state.code[state.ip].arg);
			break;
		case OP_TGET:
			scanf("%lf", &(result.tvalue));
			push(working_stack, result);
			break;
		case OP_IGET:
			scanf("%lx", &(result.intvalue));
			push(working_stack, result);
			break;
		case OP_CGET:
			if (fgetc(stdin) == '\'') result.charvalue = fgetc(stdin);
			else if (fgetc(stdin) == 'x') scanf("%hhx", &(result.charvalue));
			push(working_stack, result);
			break;
		case OP_STRGET:
			result.strvalue = (char*)malloc(256);
			scanf("%255[^\n]", result.strvalue);
			push(working_stack, result);
			break;
		case OP_TPRINT:
			printf("%lf", pop(working_stack).tvalue);
			break;
		case OP_IPRINT:
			printf("%lx", pop(working_stack).intvalue);
			break;
		case OP_CPRINT:
			printf("%c", pop(working_stack).charvalue);
			break;
		case OP_STRPRINT:
			printf("%s", peek(working_stack).strvalue);
			break;
		case OP_TESTEQ:
			result = (operand){.intvalue = pop(working_stack).tvalue == pop(working_stack).tvalue ? 1 : 0};
			push(working_stack, result);
			break;
		case OP_TESTGT:
			result = (operand){.intvalue = pop(working_stack).tvalue < pop(working_stack).tvalue ? 1 : 0};
			push(working_stack, result);
			break;
		case OP_TESTLT:
			result = (operand){.intvalue = pop(working_stack).tvalue > pop(working_stack).tvalue ? 1 : 0};
			push(working_stack, result);
			break;
		case OP_IFBEGIN:
			if (!pop(working_stack).intvalue) tonext(OP_IFBEGIN, OP_IFEND);
		case OP_IFEND:
			break;
		case OP_LOOPBEGIN:
			if (!peek(working_stack).intvalue) tonext(OP_LOOPBEGIN, OP_LOOPEND);
			jloc = (operand){.intvalue = state.ip};
			push(call_stack, jloc);
			break;
		case OP_LOOPEND:
			if (!peek(working_stack).intvalue) state.ip = pop(call_stack).intvalue;
			break;
		case OP_FDEFBEGIN:
			ht_set(functab, peek(working_stack).strvalue, state.ip);
			tonext(OP_FDEFBEGIN, OP_FDEFEND);
			state.ip++;
			break;
		case OP_FDEFEND:
			state.ip = pop(call_stack).intvalue;
			break;
		case OP_FCALL:
			jloc = (operand){.intvalue = state.ip};
			push(call_stack, jloc);
			state.ip = ht_get(functab, peek(working_stack).strvalue);
			break;
		case OP_LOAD:
			regs[state.code[state.ip].arg.regvalue] = pop(working_stack);
			break;
		case OP_STORE:
			push(working_stack, regs[state.code[state.ip].arg.regvalue]);
			break;
		case OP_JMP:
			state.ip = pop(working_stack).intvalue;
			break;
		case OP_PUSHIP:
			operand ip = (operand){.intvalue = state.ip};
			push(working_stack, ip);
			break;
		case OP_DUP:
			push(working_stack, peek(working_stack));
			break;
		case OP_FREESTR:
			free(pop(working_stack).strvalue);
			break;
		case OP_EXIT:
			fputc('\n', stdout);
			exit(0);
			break;
	}
	state.ip++;
}

int main(int argc, char** argv) {
	FILE* f;
	if (argc < 2) exit(-1);
	if (argv[1][0] == '-') f = stdin;
	else f = fopen(argv[1], "r");
	init(&working_stack);
	init(&call_stack);
	functab = ht_create(0x10000);
	while (!feof(f)) readop(f);
	state.ip = 0;
	while(1) parseop();	
}
