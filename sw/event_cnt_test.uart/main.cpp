#include "platform.h"
#include "uart.h"
#include "sim_utils.h"
#include "event_counters.h"

#define ARRAY_SIZE(x) (sizeof(x)/sizeof(x[0]))
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define MIN(a,b) ((a) < (b) ? (a) : (b))

volatile char *out_port;

int blink(volatile char *port, int cnt)
{
	*port = cnt >> 10;
	return cnt + 1;
}


int main()
{
	int cnt;

	//cnt = 0;
	//out_port = (char*)(0x00010000);
	//while (true) {
	//	cnt = blink(out_port, cnt);
	//}

	//uart_init(115200);
	uart_init(96000); // We're running at 1/10th of the intended clock rate for now
	uart_write_str("Hello world!\n");

	struct ev_t {size_t counter; const char *name;};
	const ev_t events[] = {
		{event_clk_cycles,        "event_clk_cycles"},
		{event_execute,           "event_execute"},
		{event_branch,            "event_branch"},
		{event_mem_wait_on_bus,   "event_mem_wait_on_bus"},
		{event_fetch,             "event_fetch"},
		{event_fetch_drop,        "event_fetch_drop"},
		{event_fetch_wait_on_bus, "event_fetch_wait_on_bus"},
		{event_bus_idle,          "event_bus_idle"}
	};
	uint32_t ev_cnts[event_cnt_count];
	const size_t num_events = MIN(ARRAY_SIZE(events), event_cnt_count);

	for(size_t i=0; i<num_events;++i) {
		event_select_event(i, events[i].counter);
		ev_cnts[i] = event_get_cnt(i);
	}

	event_enable_events();

	volatile int d;
	for (int baud_rate = 5000;baud_rate<5010;++baud_rate) {
		unsigned int prescaler = 32-__builtin_clz((uart1_clock_rate / baud_rate) >> 8);
		unsigned int divider = uart1_clock_rate / (1 << prescaler) / baud_rate / 2;
		d = divider;
	}

	event_disable_events();

	for(size_t i=0; i<num_events;++i) {
		ev_cnts[i] = event_get_cnt(i) - ev_cnts[i];
	}
	//for(size_t i=0; i<num_events;++i) {
	//	if (i != 0) sim_uart_write_str(" ");
	//	sim_uart_write_str(events[i].name);
	//	sim_uart_write_str(": ");
	//	sim_uart_write_hex(ev_cnts[i]);
	//}
	//sim_uart_write_str("\n");
	for(size_t i=0; i<num_events;++i) {
		//if (i != 0) sim_uart_write_str(" ");
		uart_write_str(events[i].name);
		uart_write_str(": ");
		uart_write_dec(ev_cnts[i]);
		uart_write_str("\n");
	}

	cnt = 0;
	out_port = (char*)(0x00010000);
	while (true) {
		cnt = blink(out_port, cnt);
	}
}
