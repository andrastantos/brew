volatile char *out_port;
const int speed = 1;
int blink(volatile char *port, int cnt)
{
	*port = cnt >> 1;
	return cnt + 1;
}
	
int main()
{
	int cnt = 0;
	out_port = (char*)(0x00010000);
	while (true) {
		cnt = blink(out_port, cnt);
	}
}
