extern int __ai_state;

struct kun {
	int a[5];
};

void a(int x) {
	__ai_state = 0;
}

void b(void *x, int y) {
	__ai_state = 0;
}

void c(int x, void *y) {
	__ai_state = 0;
}

void d(struct kun k) {
	__ai_state = 0;
}
