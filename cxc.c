#include <math.h>

#define T long double
#define STACK_SIZE 0x10000

typedef struct {
	T data[STACK_SIZE];
	unsigned long sp;
} stack;

void push(stack* s, T i) {
	if ((s->sp) < STACK_SIZE) {
		s->data[s->sp] = i;
		(s->sp)++;
	}
}

void reset(stack* s) {
	(s->sp) = &(s->data[0]);
}

void init(stack** s) {
	*s = malloc(sizeof(stack));
	reset(*s);
}

uint64_t pop(stack* s) {
	if ((s->sp) > 0) {
		(s->sp)--;
		return s->data[(s->sp)];
	}
	return 0;
}
