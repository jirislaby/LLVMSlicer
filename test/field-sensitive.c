#include <assert.h>

struct X {
	char x;
	int a;
	int b;
};

int main()
{
	struct X x;
	int *Xa = &x.a, *Xb = &x.b;

	*Xa = 1;

	assert(*Xb);

	return 0;
}
