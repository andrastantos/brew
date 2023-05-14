#include "platform.h"
#include "uart.h"

const int speed = 1;
int blink(volatile uint8_t *port, int cnt)
{
	*port = cnt >> 1;
	return cnt + 1;
}

int main()
{
	sim_uart_init(115200);
	sim_uart_write_str("Hello world!\n");
	int cnt = 0;
	volatile uint8_t *out_port = gpio1_base + gpio_data_reg_ofs;
	while (true) {
		cnt = blink(out_port, cnt);
		sim_uart_write_hex((uint16_t)cnt);
		sim_uart_write_str("\n");
	}
}
