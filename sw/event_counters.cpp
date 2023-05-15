#include "event_counters.h"
#include "platform.h"

void event_select_event(size_t counter, uint8_t event) {
    uint32_t max_event = (1 << event_cnt_sel_size) - 1;

    if (counter >= event_cnt_count) return;
    if (event > max_event) return;


    uint32_t mask = max_event << (counter * event_cnt_sel_size);
    uint32_t old_val = *csr_event_sel;
    uint32_t new_val = old_val & mask | (event << (counter * event_cnt_sel_size));
    *csr_event_sel = new_val;
}

uint32_t event_get_cnt(size_t counter) {
    if (counter >= event_cnt_count) return 0;

    return csr_event_cnt0[counter];
}


