#include <math.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include <gmp.h>
#include <mpfr.h>

#define SIZE 0x100000
#define DEBUG 1

typedef struct {
	union {
		mpfr_t tvalue;
		uint64_t intvalue;
		char charvalue;
		uint16_t regvalue;
		char* strvalue;
	};
	unsigned char clear_mpfr;
} operand;

typedef enum {
	OP_ADD = 1,
	OP_SUB,
	OP_MUL,
	OP_DIV,
	OP_REM,
	OP_IADD,
	OP_ISUB,
	OP_IRSH,
	OP_ILSH,
	OP_IINC,
	OP_IDEC,
	OP_IMUL,
	OP_IDIV,
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
		ht->table[i] = NULL;
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

operand operand_mpfr_init() {
	operand r;
	r.clear_mpfr = 0xFF;
	mpfr_init2(r.tvalue, 256);
	mpfr_set_d(r.tvalue, 0.0, MPFR_RNDN);
	return r;
}

void push(stack* s, operand i) {
	if (s->data[(s->sp) + 1].clear_mpfr == 0xFF) mpfr_clear(s->data[(s->sp) + 1].tvalue);
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

operand pop_mpfrdefault(stack* s) {
	if ((s->sp) > 0) {
		(s->sp)--;
		return s->data[(s->sp)];
	}
	return operand_mpfr_init();
}

operand peek(stack* s) {
	if ((s->sp) > 0) return s->data[(s->sp) - 1];
	return (operand){.intvalue = 0};
}

operand peek_mpfrdefault(stack* s) {
	if ((s->sp) > 0) return s->data[(s->sp) - 1];
	return operand_mpfr_init();
}

stack* working_stack;
stack* call_stack;
operand regs[0x10000];
hashtable* functab;

struct {
	operation code[SIZE];
	uint64_t rip, wip;
} state = {
	.rip = 0, .wip = 0
};

void readop(FILE* f) {
	char c = fgetc(f);
	while (isspace(c)) {
		c = fgetc(f);
	}
	if (feof(f)) return;
	operation op;
	char opstr[4];
	if (DEBUG) printf("'%c',", c);
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
		case 'I':
			op.op = OP_PSHI;
			fscanf(f, "%lx", &(op.arg.intvalue));
			break;
		case 'C':
			op.op = OP_PSHC;
			if (fgetc(f) == '\'') {
				op.arg.charvalue = fgetc(f);
				break;
			}
			else if (fgetc(f) == 'x') scanf("%2hhx", &(op.arg.charvalue));
			fgetc(f);
			break;
		case '"':
			op.op = OP_PSHSTR;
			op.arg.strvalue = (char*)malloc(256);
			fscanf(f, "%255[^\"]", op.arg.strvalue);
			fgetc(f);
			break;
		case '_':
			fgets(opstr, 4, f);
			if (!strcmp(opstr, "sin")) op.op = OP_SIN;
			if (!strcmp(opstr, "cos")) op.op = OP_COS;
			if (!strcmp(opstr, "tan")) op.op = OP_TAN;
			if (!strcmp(opstr, "Iad")) op.op = OP_IADD;
			if (!strcmp(opstr, "Isb")) op.op = OP_ISUB;
			if (!strcmp(opstr, "Isr")) op.op = OP_IRSH;
			if (!strcmp(opstr, "Isl")) op.op = OP_ILSH;
			if (!strcmp(opstr, "Iin")) op.op = OP_IINC;
			if (!strcmp(opstr, "Ide")) op.op = OP_IDEC;
			if (!strcmp(opstr, "Iml")) op.op = OP_IMUL;
			if (!strcmp(opstr, "Idv")) op.op = OP_IDIV;
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
		case 'F':
		default:
			op.op = OP_PSHT;
			operand psht = operand_mpfr_init();
			char* frstr = (char*)malloc(256);
			fscanf(f, "%255[^;]", frstr);
			fgetc(f);
			mpfr_strtofr(psht.tvalue, frstr, NULL, 0, MPFR_RNDN);
			op.arg = psht;
			break;
	}
	if (state.wip < SIZE) state.code[state.wip] = op;
	else exit(1);
	state.wip++;
}

void tonext(optype skipop, optype op) {
	uint64_t skip = 0;
	state.rip++;
	while (state.code[state.rip].op != op || skip > 0) {
		if (state.code[state.rip].op == skipop) skip += 1;
		state.rip++;
	}
	state.rip--;
}

void parseop() {
	operand result, jloc;
	switch (state.code[state.rip].op) {
		case OP_ADD:;
			result = operand_mpfr_init();
			mpfr_add(result.tvalue, pop_mpfrdefault(working_stack).tvalue, pop_mpfrdefault(working_stack).tvalue, MPFR_RNDN);
			push(working_stack, result);
			break;
		case OP_SUB:;
			result = operand_mpfr_init();
			operand sub1 = pop_mpfrdefault(working_stack);
			operand sub2 = pop_mpfrdefault(working_stack);
			mpfr_sub(result.tvalue, sub2.tvalue, sub1.tvalue, MPFR_RNDN);
			push(working_stack, result);
			break;
		case OP_MUL:;
			result = operand_mpfr_init();
			mpfr_mul(result.tvalue, pop_mpfrdefault(working_stack).tvalue, pop_mpfrdefault(working_stack).tvalue, MPFR_RNDN);
			push(working_stack, result);
			break;
		case OP_DIV:;
			result = operand_mpfr_init();
			operand div1 = pop_mpfrdefault(working_stack);
			operand div2 = pop_mpfrdefault(working_stack);
			mpfr_div(result.tvalue, div2.tvalue, div1.tvalue, MPFR_RNDN);
			push(working_stack, result);
			break;
		case OP_IADD:;
			result = (operand){.intvalue = pop(working_stack).intvalue + pop(working_stack).intvalue};
			push(working_stack, result);
			break;
		case OP_ISUB:;
			uint64_t isub1 = pop(working_stack).intvalue;
			uint64_t isub2 = pop(working_stack).intvalue;
			result = (operand){.intvalue = isub2 - isub1};
			push(working_stack, result);
			break;
		case OP_IRSH:;
			uint64_t irsh1 = pop(working_stack).intvalue;
			uint64_t irsh2 = pop(working_stack).intvalue;
			result = (operand){.intvalue = isub2 >> isub1};
			push(working_stack, result);
			break;
		case OP_ILSH:;
			uint64_t ilsh1 = pop(working_stack).intvalue;
			uint64_t ilsh2 = pop(working_stack).intvalue;
			result = (operand){.intvalue = isub2 << isub1};
			push(working_stack, result);
			break;
		case OP_IINC:;
			result = (operand){.intvalue = pop(working_stack).intvalue + 1};
			push(working_stack, result);
			break;
		case OP_IDEC:;
			result = (operand){.intvalue = pop(working_stack).intvalue - 1};
			push(working_stack, result);
			break;
		case OP_IMUL:;
			result = (operand){.intvalue = pop(working_stack).intvalue * pop(working_stack).intvalue};
			push(working_stack, result);
			break;
		case OP_IDIV:;
			uint64_t idiv1 = pop(working_stack).intvalue;
			uint64_t idiv2 = pop(working_stack).intvalue;
			result = (operand){.intvalue = idiv2 / idiv1};
			push(working_stack, result);
			break;
		case OP_REM:;
			uint64_t rem1 = pop_mpfrdefault(working_stack).intvalue;
			uint64_t rem2 = pop_mpfrdefault(working_stack).intvalue;
			result = (operand){.intvalue = rem2 % rem1};
			push(working_stack, result);
			break;
		case OP_EXP:;
			result = operand_mpfr_init();
			operand exp1 = pop_mpfrdefault(working_stack);
			operand exp2 = pop_mpfrdefault(working_stack);
			mpfr_pow(result.tvalue, exp2.tvalue, exp1.tvalue, MPFR_RNDN);
			push(working_stack, result);
			break;
		case OP_RT:;
			result = operand_mpfr_init();
			operand rt1 = pop_mpfrdefault(working_stack);
			operand rt2 = pop(working_stack);
			mpfr_rootn_ui(result.tvalue, rt2.tvalue, rt1.intvalue, MPFR_RNDN);
			push(working_stack, result);
			break;
		case OP_SIN:;
			result = operand_mpfr_init();
			mpfr_sin(result.tvalue, pop_mpfrdefault(working_stack).tvalue, MPFR_RNDN);
			push(working_stack, result);
			break;
		case OP_COS:;
			result = operand_mpfr_init();
			mpfr_cos(result.tvalue, pop_mpfrdefault(working_stack).tvalue, MPFR_RNDN);
			push(working_stack, result);
			break;
		case OP_TAN:;
			result = operand_mpfr_init();
			mpfr_tan(result.tvalue, pop_mpfrdefault(working_stack).tvalue, MPFR_RNDN);
			push(working_stack, result);
			break;
		case OP_AND:;
			result = (operand){.intvalue = pop(working_stack).intvalue & pop(working_stack).intvalue};
			push(working_stack, result);
			break;
		case OP_OR:;
			result = (operand){.intvalue = pop(working_stack).intvalue | pop(working_stack).intvalue};
			push(working_stack, result);
			break;
		case OP_XOR:;
			result = (operand){.intvalue = pop(working_stack).intvalue ^ pop(working_stack).intvalue};
			push(working_stack, result);
			break;
		case OP_NOT:
			result = (operand){.intvalue = ~pop(working_stack).intvalue};
            		break;
		case OP_PSHT:;
		case OP_PSHI:;
		case OP_PSHC:;
		case OP_PSHSTR:;
			push(working_stack, state.code[state.rip].arg);
			break;
		case OP_TGET:;
			result = operand_mpfr_init();
			char* frstr = (char*)malloc(256);
			scanf("%255[^;]", frstr);
			mpfr_strtofr(result.tvalue, frstr, NULL, 0, MPFR_RNDN);
			push(working_stack, result);
			break;
		case OP_IGET:;
			scanf("%lx", &(result.intvalue));
			push(working_stack, result);
			break;
		case OP_CGET:;
			if (fgetc(stdin) == '\'') result.charvalue = fgetc(stdin);
			else if (fgetc(stdin) == 'x') scanf("%2hhx", &(result.charvalue));
			push(working_stack, result);
			break;
		case OP_STRGET:;
			result.strvalue = (char*)malloc(256);
			scanf("%255[^\n]", result.strvalue);
			push(working_stack, result);
			break;
		case OP_TPRINT:;
			mpfr_out_str(stdout, 10, 0, pop_mpfrdefault(working_stack).tvalue, MPFR_RNDN);
			break;
		case OP_IPRINT:;
			printf("%lx", pop(working_stack).intvalue);
			break;
		case OP_CPRINT:;
			fputc(pop(working_stack).charvalue, stdout);
			break;
		case OP_STRPRINT:;
			printf("%s", peek(working_stack).strvalue);
			break;
		case OP_TESTEQ:;
			result = (operand){.intvalue = mpfr_cmp(pop_mpfrdefault(working_stack).tvalue, pop_mpfrdefault(working_stack).tvalue) == 0 ? 1 : 0};
			push(working_stack, result);
			break;
		case OP_TESTGT:;
			result = (operand){.intvalue = mpfr_cmp(pop_mpfrdefault(working_stack).tvalue, pop_mpfrdefault(working_stack).tvalue) < 0 ? 1 : 0};
			push(working_stack, result);
			break;
		case OP_TESTLT:;
			result = (operand){.intvalue = mpfr_cmp(pop_mpfrdefault(working_stack).tvalue, pop_mpfrdefault(working_stack).tvalue) > 0 ? 1 : 0};
			push(working_stack, result);
			break;
		case OP_IFBEGIN:;
			if (!pop(working_stack).intvalue) tonext(OP_IFBEGIN, OP_IFEND);
		case OP_IFEND:;
			break;
		case OP_LOOPBEGIN:;
			if (!peek(working_stack).intvalue) tonext(OP_LOOPBEGIN, OP_LOOPEND);
			jloc = (operand){.intvalue = state.rip - 1};
			push(call_stack, jloc);
			break;
		case OP_LOOPEND:;
			if (peek(working_stack).intvalue) state.rip = pop(call_stack).intvalue;
			break;
		case OP_FDEFBEGIN:;
			ht_set(functab, peek(working_stack).strvalue, state.rip);
			tonext(OP_FDEFBEGIN, OP_FDEFEND);
			state.rip++;
			break;
		case OP_FDEFEND:;
			state.rip = pop(call_stack).intvalue;
			break;
		case OP_FCALL:;
			jloc = (operand){.intvalue = state.rip};
			push(call_stack, jloc);
			state.rip = ht_get(functab, peek(working_stack).strvalue);
			break;
		case OP_LOAD:;
			regs[state.code[state.rip].arg.regvalue] = pop_mpfrdefault(working_stack);
			break;
		case OP_STORE:;
			push(working_stack, regs[state.code[state.rip].arg.regvalue]);
			break;
		case OP_JMP:;
			state.rip = pop(working_stack).intvalue;
			break;
		case OP_PUSHIP:;
			operand ip = (operand){.intvalue = state.rip};
			push(working_stack, rip);
			break;
		case OP_DUP:;
			push(working_stack, peek_mpfrdefault(working_stack));
			break;
		case OP_FREESTR:;
			free(pop(working_stack).strvalue);
			break;
		case OP_EXIT:;
			fputc('\n', stdout);
			exit(0);
			break;
    }
    state.rip++;
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
	while(1) parseop();	
}
