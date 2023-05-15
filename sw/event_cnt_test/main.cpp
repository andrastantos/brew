#include "platform.h"
#include "uart.h"
#include "sim_utils.h"
#include "event_counters.h"

int main()
{
	sim_uart_init(115200);

	event_select_event(0, event_clk_cycles);
	event_select_event(1, event_execute);
	event_select_event(2, event_fetch_wait_on_bus);
	event_select_event(3, event_mem_wait_on_bus);
	event_select_event(4, event_decode_wait_on_rf);
	event_select_event(5, event_branch_taken);
	event_select_event(6, event_load);
	event_select_event(7, event_store);


	uint32_t clocks = event_get_cnt(0);
	uint32_t instructions = event_get_cnt(1);
	uint32_t fetch_waits = event_get_cnt(2);
	uint32_t mem_waits = event_get_cnt(3);
	uint32_t rf_waits = event_get_cnt(4);
	uint32_t branch_taken = event_get_cnt(5);
	uint32_t loads = event_get_cnt(6);
	uint32_t stores = event_get_cnt(7);

	event_enable_events();

	volatile int d;
	for (int baud_rate = 5000;baud_rate<5010;++baud_rate) {
		unsigned int prescaler = 32-__builtin_clz((uart1_clock_rate / baud_rate) >> 8);
		unsigned int divider = uart1_clock_rate / (1 << prescaler) / baud_rate / 2;
		d = divider;
	}

	event_disable_events();

	clocks = event_get_cnt(0) - clocks;
	instructions = event_get_cnt(1) - instructions;
	fetch_waits = event_get_cnt(2) - fetch_waits;
	mem_waits = event_get_cnt(3) - mem_waits;
	rf_waits = event_get_cnt(4) - rf_waits;
	branch_taken = event_get_cnt(5) - branch_taken;
	loads = event_get_cnt(6) - loads;
	stores = event_get_cnt(7) - stores;

	//sim_uart_write_str("S1: ");
	//sim_uart_write_hex((uint32_t)(uart1_clock_rate / (1 << prescaler)));
	//sim_uart_write_str("\n");
	//sim_uart_write_str("prescaler: ");
	//sim_uart_write_hex((uint32_t)prescaler);
	//sim_uart_write_str("\n");
	//sim_uart_write_str("divider: ");
	//sim_uart_write_hex((uint32_t)divider);
	//sim_uart_write_str("\n");

	sim_uart_write_str("clock cycles: ");
	sim_uart_write_hex(clocks);
	sim_uart_write_str(" instructions: ");
	sim_uart_write_hex(instructions);
	sim_uart_write_str(" fetch waits: ");
	sim_uart_write_hex(fetch_waits);
	sim_uart_write_str(" mem waits: ");
	sim_uart_write_hex(mem_waits);
	sim_uart_write_str(" rf waits: ");
	sim_uart_write_hex(rf_waits);
	sim_uart_write_str(" branches taken: ");
	sim_uart_write_hex(branch_taken);
	sim_uart_write_str(" loads: ");
	sim_uart_write_hex(loads);
	sim_uart_write_str(" stores: ");
	sim_uart_write_hex(stores);
	sim_uart_write_str("\n");

	sim_terminate(0);
}
