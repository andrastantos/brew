#pragma once

#include <stdint.h>

const uint32_t rom_base = 0x00000000;
const uint32_t gpio_base = 0x00010000;
const uint32_t io_apb_base = 0x00020000;
const uint32_t io_apb_size = 4096*16;
const uint32_t gpio_size = 4096;
const uint32_t dram_base = 0x80000000;

const uint32_t wait_state_0  = 0x04000000;
const uint32_t wait_state_1  = 0x08000000;
const uint32_t wait_state_2  = 0x0c000000;
const uint32_t wait_state_3  = 0x10000000;
const uint32_t wait_state_4  = 0x14000000;
const uint32_t wait_state_5  = 0x18000000;
const uint32_t wait_state_6  = 0x1c000000;
const uint32_t wait_state_7  = 0x20000000;
const uint32_t wait_state_8  = 0x24000000;
const uint32_t wait_state_9  = 0x28000000;
const uint32_t wait_state_10 = 0x2c000000;
const uint32_t wait_state_11 = 0x30000000;
const uint32_t wait_state_12 = 0x34000000;
const uint32_t wait_state_13 = 0x38000000;
const uint32_t wait_state_14 = 0x3c000000;
const uint32_t wait_state_15 = 0x00000000;
const uint32_t wait_state_default = wait_state_15;

/////////////////////////////////////////////////////////////////////////////////////
// GPIO
/////////////////////////////////////////////////////////////////////////////////////
volatile uint8_t* const gpio1_base = (volatile uint8_t *)(gpio_base);
volatile uint8_t* const gpio2_base = (volatile uint8_t *)(gpio_base + gpio_size);

const uint32_t gpio_data_reg_ofs  = 0;

volatile uint8_t* const gpio3_base = (volatile uint8_t *)((io_apb_base + 0x0100) | wait_state_0);
volatile uint8_t* const gpio4_base = (volatile uint8_t *)((io_apb_base + 0x0200) | wait_state_0);

/////////////////////////////////////////////////////////////////////////////////////
// UART
/////////////////////////////////////////////////////////////////////////////////////

const uint32_t cpu_clock_rate    = 10000000; // 10MHz clock for the processor
const uint32_t system_clock_rate = 50000000; // 50MHz clock for the system

volatile uint8_t* const uart1_base = (volatile uint8_t *)((io_apb_base + 0x0000) | wait_state_0);
const uint32_t uart1_clock_rate = system_clock_rate;

// Baud rate = <clock freq> / <prescaler> / (<divider> + 1) / 2

const uint32_t uart_data_buf_reg_ofs                = 0;
const uint32_t uart_status_reg_ofs                  = 1;
const uint32_t uart_config1_reg_ofs                 = 2;
const uint32_t uart_config2_reg_ofs                 = 3;
const uint32_t uart_divider_reg_ofs                 = 4;

// Status register
const uint8_t uart_status_rx_full_bit               = 0;
const uint8_t uart_status_tx_empty_bit              = 1;
const uint8_t uart_status_parity_error_bit          = 2;
const uint8_t uart_status_framing_error_bit         = 3;
const uint8_t uart_status_overrun_error_bit         = 4;
const uint8_t uart_status_cts_pin_bit               = 5;

// Config1 register
const uint8_t uart_config1_parity_bit               = 0;
const uint8_t uart_config1_stop_cnt_bit             = 2;
const uint8_t uart_config1_word_size_bit            = 4;
const uint8_t uart_config1_flow_control_bit         = 6;
const uint8_t uart_config1_interrupt_enable_bit     = 7;

const uint8_t uart_config1_parity_none              = 0 * (1 << uart_config1_parity_bit);
const uint8_t uart_config1_parity_even              = 1 * (1 << uart_config1_parity_bit);
const uint8_t uart_config1_parity_odd               = 2 * (1 << uart_config1_parity_bit);

const uint8_t uart_config1_stop_one                 = 0 * (1 << uart_config1_stop_cnt_bit);
const uint8_t uart_config1_stop_one_and_half        = 1 * (1 << uart_config1_stop_cnt_bit);
const uint8_t uart_config1_stop_two                 = 2 * (1 << uart_config1_stop_cnt_bit);

const uint8_t uart_config1_word_size_8              = 0 * (1 << uart_config1_word_size_bit);
const uint8_t uart_config1_word_size_7              = 1 * (1 << uart_config1_word_size_bit);
const uint8_t uart_config1_word_size_6              = 2 * (1 << uart_config1_word_size_bit);
const uint8_t uart_config1_word_size_5              = 3 * (1 << uart_config1_word_size_bit);

const uint8_t uart_config1_flow_control_sw          = 0 * (1 << uart_config1_flow_control_bit);
const uint8_t uart_config1_flow_control_hw          = 1 * (1 << uart_config1_flow_control_bit);

const uint8_t uart_config1_interrupt_disable        = 0 * (1 << uart_config1_interrupt_enable_bit);
const uint8_t uart_config1_interrupt_enable         = 1 * (1 << uart_config1_interrupt_enable_bit);

// Config 2 register
const uint8_t uart_config2_pre_scaler_bit           = 0;
const uint8_t uart_config2_rts_pin_bit              = 4;
const uint8_t uart_config2_rx_enable_bit            = 5;
const uint8_t uart_config2_use_hw_tx_en_bit         = 6;
const uint8_t uart_config2_tx_en_bit                = 7;

const uint8_t uart_config2_pre_scaler_1             = 0 * (1 << uart_config2_pre_scaler_bit);
const uint8_t uart_config2_pre_scaler_2             = 1 * (1 << uart_config2_pre_scaler_bit);
const uint8_t uart_config2_pre_scaler_4             = 2 * (1 << uart_config2_pre_scaler_bit);
const uint8_t uart_config2_pre_scaler_8             = 3 * (1 << uart_config2_pre_scaler_bit);
const uint8_t uart_config2_pre_scaler_16            = 4 * (1 << uart_config2_pre_scaler_bit);
const uint8_t uart_config2_pre_scaler_32            = 5 * (1 << uart_config2_pre_scaler_bit);
const uint8_t uart_config2_pre_scaler_64            = 6 * (1 << uart_config2_pre_scaler_bit);
const uint8_t uart_config2_pre_scaler_128           = 7 * (1 << uart_config2_pre_scaler_bit);
const uint8_t uart_config2_rts                      = 1 * (1 << uart_config2_rts_pin_bit);
const uint8_t uart_config2_rx_enable                = 1 * (1 << uart_config2_rx_enable_bit);
const uint8_t uart_config2_use_hw_tx_en             = 1 * (1 << uart_config2_use_hw_tx_en_bit);
const uint8_t uart_config2_tx_en                    = 1 * (1 << uart_config2_tx_en_bit);

/*
inline uint32_t next_power_of_2(uint32_t v) {
    v--;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v++;
    return v;
}
*/
