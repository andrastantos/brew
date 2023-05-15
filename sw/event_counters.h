#pragma once

#include <stdint.h>
#include <stddef.h>

void event_select_event(size_t counter, uint8_t event);
uint32_t event_get_cnt(size_t counter);
