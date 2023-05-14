# include "platform.h"

const int speed = 1;
int blink(volatile uint8_t *port, int cnt)
{
	*port = cnt >> 1;
	return cnt + 1;
}

int main()
{
	int cnt = 0;
	uart1_base[uart_config2_reg_ofs] = uart_config2_pre_scaler_32;
	//uart1_base[uart_divider_reg_ofs] = 80;
	uart1_base[uart_divider_reg_ofs] = 2;
	uart1_base[uart_data_buf_reg_ofs] = 0xaa;
	while ((uart1_base[uart_status_reg_ofs] & (1 << uart_status_tx_empty_bit)) == 0);
	uart1_base[uart_data_buf_reg_ofs] = 0x55;
	while ((uart1_base[uart_status_reg_ofs] & (1 << uart_status_tx_empty_bit)) == 0);
	volatile uint8_t *out_port = gpio1_base + gpio_data_reg_ofs;
	while (true) {
		cnt = blink(out_port, cnt);
	}
}
