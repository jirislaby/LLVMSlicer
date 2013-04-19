#include <assert.h>
#include <stdlib.h>

struct list {
	int n;
	struct list *next;
};

static struct list *append(struct list *l, int n)
{
	struct list *new_el;

	new_el = malloc(sizeof(*new_el));

	new_el->next = l;
	new_el->n = n;

	return new_el;
}

int main(void)
{
	struct list *l, m = {};

	l = append(&m, 1);
//	l = append(l, 2);

	assert(l->next->n != 0);

	return 0;
}
