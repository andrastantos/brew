#include "platform.h"
#include "uart.h"
#include "sim_utils.h"

void blink(volatile uint8_t *port, int cnt)
{
	*port = cnt;
}

int main()
{
	sim_uart_init(115200);
	sim_uart_write_str("Hello world!\n");
	int cnt = 0;
	volatile uint8_t *out_port = gpio1_base + gpio_data_reg_ofs;
	for (cnt=0;cnt<20;++cnt) {
		blink(out_port, cnt);
		sim_uart_write_hex((uint8_t)cnt);
		sim_uart_write_str("\n");
	}
	sim_terminate(0);
}
