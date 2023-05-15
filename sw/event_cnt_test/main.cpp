#include "platform.h"
#include "uart.h"
#include "sim_utils.h"
#include "event_counters.h"

int main()
{
	sim_uart_init(115200);

	event_select_event(0, event_clk_cycles);
	event_select_event(1, event_execute);

	uint32_t start0 = event_get_cnt(0);
	uint32_t start1 = event_get_cnt(1);

	unsigned int baud_rate = 19200;
	unsigned int prescaler = 32-__builtin_clz((uart1_clock_rate / baud_rate) >> 8);
	unsigned int divider = uart1_clock_rate / (1 << prescaler) / baud_rate / 2;

	sim_uart_write_str("S1: ");
	sim_uart_write_hex((uint32_t)(uart1_clock_rate / (1 << prescaler)));
	sim_uart_write_str("\n");
	sim_uart_write_str("prescaler: ");
	sim_uart_write_hex((uint32_t)prescaler);
	sim_uart_write_str("\n");
	sim_uart_write_str("divider: ");
	sim_uart_write_hex((uint32_t)divider);
	sim_uart_write_str("\n");

	uint32_t end0 = event_get_cnt(0);
	uint32_t end1 = event_get_cnt(1);

	sim_uart_write_str("clock cycles: ");
	sim_uart_write_hex(end0-start0);
	sim_uart_write_str(" instructions: ");
	sim_uart_write_hex(end1-start1);
	sim_uart_write_str("\n");

	sim_terminate(0);
}
