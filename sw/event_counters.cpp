#include "event_counters.h"
#include "platform.h"

void event_select_event(size_t counter, uint8_t event) {
    if (counter >= event_cnt_count) return;

    csr_event_regs[event_reg_size*counter+event_sel_ofs] = event;
}

uint32_t event_get_cnt(size_t counter) {
    if (counter >= event_cnt_count) return 0;


    return csr_event_regs[event_reg_size*counter+event_cnt_ofs];
}

void event_enable_events() {
    *csr_event_enable = 1;
}

void event_disable_events() {
    *csr_event_enable = 0;
}


