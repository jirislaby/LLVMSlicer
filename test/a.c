extern int __ai_state;

void a(int x) {
	__ai_state = 0;
}

void b(void *x, int y) {
	__ai_state = 0;
}

void c(int x, void *y) {
	__ai_state = 0;
}
