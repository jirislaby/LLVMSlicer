struct X {
	int a;
	int b;
};

int main()
{
	struct X x;
	int *Xa, *Xb;

	Xa = &x.a;
	Xb = &x.b;

	return 0;
}
