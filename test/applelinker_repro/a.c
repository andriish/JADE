/* 131072 bytes, a tentative definition (with -fcommon this becomes a common symbol). */
int xx[32768];
void fa(void) { xx[32767] = 1; }
