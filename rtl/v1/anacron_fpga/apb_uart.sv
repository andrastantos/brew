////////////////////////////////////////////////////////////////////////////////
// Type definitions
////////////////////////////////////////////////////////////////////////////////
`define UartParityType__none 2'h0
`define UartParityType__even 2'h1
`define UartParityType__odd 2'h2


`define UartStopBits__one 2'h0
`define UartStopBits__one_and_half 2'h1
`define UartStopBits__two 2'h2


`define UartTxPhyStates__idle 3'h0
`define UartTxPhyStates__tx_en 3'h1
`define UartTxPhyStates__start 3'h2
`define UartTxPhyStates__data 3'h3
`define UartTxPhyStates__parity 3'h4
`define UartTxPhyStates__stop_half 3'h5
`define UartTxPhyStates__stop 3'h6
`define UartTxPhyStates__stop_two 3'h7


`define UartRxPhyStates__idle 3'h0
`define UartRxPhyStates__half_start 3'h1
`define UartRxPhyStates__start 3'h2
`define UartRxPhyStates__data 3'h3
`define UartRxPhyStates__parity 3'h4
`define UartRxPhyStates__stop_half 3'h5
`define UartRxPhyStates__stop 3'h6
`define UartRxPhyStates__stop_two 3'h7





////////////////////////////////////////////////////////////////////////////////
// ApbUart
////////////////////////////////////////////////////////////////////////////////
module ApbUart (
	input logic clk,
	input logic rst,
	input logic [2:0] bus_if_paddr,
	input logic bus_if_penable,
	output logic [7:0] bus_if_prdata,
	output logic bus_if_pready,
	input logic bus_if_psel,
	input logic [7:0] bus_if_pwdata,
	input logic bus_if_pwrite,

	output logic interrupt,
	input logic rxd,
	output logic txd,
	input logic cts,
	output logic rts,
	output logic n_tx_en
);

	logic [7:0] tx_data_data;
	logic [7:0] u13_output_port;
	logic tx_data_valid;
	logic data_reg_wr;
	logic rx_data_ready;
	logic data_reg_rd;
	logic status_reg_wr;
	logic config1_reg_wr;
	logic config2_reg_wr;
	logic divider_reg_wr;
	logic [7:0] config_reg;
	logic [2:0] prescaler_select;
	logic soft_rts;
	logic rx_enable;
	logic use_hw_tx_en;
	logic soft_tx_en;
	logic [7:0] divider_limit;
	logic [1:0] u64_output_port;
	logic [1:0] u66_output_port;
	logic [2:0] word_size;
	logic [1:0] u74_output_port;
	logic [1:0] u76_output_port;
	logic u79_output_port;
	logic rx_phy_data_out_valid;
	logic [7:0] rx_phy_data_out_data;
	logic rx_phy_parity_error;
	logic rx_phy_framing_error;
	logic rx_phy_overrun_error;
	logic rx_phy_rts;
	logic [7:0] rx_data_data;
	logic u85_output_port;
	logic rx_data_valid;
	logic rx_phy_data_out_ready;
	logic [7:0] u1_output_port_data;
	logic u87_output_port;
	logic u1_output_port_valid;
	logic tx_data_ready;
	logic tx_phy_cts_out;
	logic u1_output_port_ready;
	logic tx_phy_tx_en;
	logic hw_flow_ctrl;
	logic interrupt_en;

	always_ff @(posedge clk) bus_if_pready <= rst ? 1'h0 : bus_if_psel ? bus_if_paddr == 1'h0 ? bus_if_pwrite ? tx_data_ready : rx_data_valid : 1'h1 : 1'h1;
	always @(*) begin
		case (bus_if_paddr)
			3'd0: u13_output_port = rx_data_data;
			3'd1: u13_output_port = {tx_phy_cts_out, rx_phy_overrun_error, rx_phy_framing_error, rx_phy_parity_error, tx_data_ready, rx_data_valid};
			3'd2: u13_output_port = config_reg;
			3'd3: u13_output_port = {prescaler_select, 1'h0, soft_rts, rx_enable, use_hw_tx_en, soft_tx_en};
			3'd4: u13_output_port = divider_limit;
			default: u13_output_port = 8'hx;
		endcase
	end
	always_ff @(posedge clk) bus_if_prdata <= rst ? 8'h0 : u13_output_port;
	assign config1_reg_wr = bus_if_psel & bus_if_penable & bus_if_pwrite & bus_if_paddr == 2'h2;
	always_ff @(posedge clk) config_reg <= rst ? 8'h0 : config1_reg_wr ? bus_if_pwdata : config_reg;
	assign config2_reg_wr = bus_if_psel & bus_if_penable & bus_if_pwrite & bus_if_paddr == 2'h3;
	always_ff @(posedge clk) prescaler_select <= rst ? 3'h0 : config2_reg_wr ? bus_if_pwdata[2:0] : prescaler_select;
	always_ff @(posedge clk) soft_rts <= rst ? 1'h0 : config2_reg_wr ? bus_if_pwdata[4] : soft_rts;
	always_ff @(posedge clk) rx_enable <= rst ? 1'h0 : config2_reg_wr ? bus_if_pwdata[5] : rx_enable;
	always_ff @(posedge clk) use_hw_tx_en <= rst ? 1'h0 : config2_reg_wr ? bus_if_pwdata[6] : use_hw_tx_en;
	always_ff @(posedge clk) soft_tx_en <= rst ? 1'h0 : config2_reg_wr ? bus_if_pwdata[7] : soft_tx_en;
	assign divider_reg_wr = bus_if_psel & bus_if_penable & bus_if_pwrite & bus_if_paddr == 3'h4;
	always_ff @(posedge clk) divider_limit <= rst ? 8'h0 : divider_reg_wr ? bus_if_pwdata : divider_limit;
	assign data_reg_wr = bus_if_psel & bus_if_penable & bus_if_pwrite & bus_if_paddr == 1'h0;
	assign data_reg_rd = bus_if_psel & bus_if_penable &  ~ bus_if_pwrite & bus_if_paddr == 1'h0;
	assign status_reg_wr = bus_if_psel & bus_if_penable & bus_if_pwrite & bus_if_paddr == 1'h1;
	assign interrupt = (rx_phy_overrun_error | rx_phy_framing_error | rx_phy_parity_error | tx_data_ready | rx_data_valid) & interrupt_en;
	always @(*) begin
		case (config_reg[5:4])
			2'd0: word_size = 1'h0;
			2'd1: word_size = 3'h7;
			2'd2: word_size = 3'h6;
			2'd3: word_size = 3'h5;
			default: word_size = 3'hx;
		endcase
	end
	assign hw_flow_ctrl = config_reg[6];
	assign interrupt_en = config_reg[7];
	assign rts =  ~ (hw_flow_ctrl ? rx_phy_rts : soft_rts);
	assign n_tx_en =  ~ (use_hw_tx_en ? tx_phy_tx_en : soft_tx_en);

	apb_uart_UartRxPhy rx_phy (
		.clk(clk),
		.rst(rst),
		.data_out_data(rx_phy_data_out_data),
		.data_out_ready(rx_phy_data_out_ready),
		.data_out_valid(rx_phy_data_out_valid),

		.parity(u74_output_port),
		.stop_cnt(u76_output_port),
		.word_size(word_size),
		.hw_flow_ctrl(hw_flow_ctrl),
		.rxd(rxd),
		.rts(rx_phy_rts),
		.framing_error(rx_phy_framing_error),
		.parity_error(rx_phy_parity_error),
		.overrun_error(rx_phy_overrun_error),
		.clear(status_reg_wr),
		.prescaler_select(prescaler_select),
		.divider_limit(divider_limit),
		.enable(rx_enable)
	);

	apb_uart_ForwardBuf u (
		.input_port_data(rx_phy_data_out_data),
		.input_port_ready(rx_phy_data_out_ready),
		.input_port_valid(rx_phy_data_out_valid),

		.output_port_data(rx_data_data),
		.output_port_ready(data_reg_rd),
		.output_port_valid(rx_data_valid),

		.clock_port(clk),
		.reset_port(rst),
		.clear(u85_output_port)
	);

	apb_uart_ForwardBuf_2 u1 (
		.input_port_data(bus_if_pwdata),
		.input_port_ready(tx_data_ready),
		.input_port_valid(data_reg_wr),

		.output_port_data(u1_output_port_data),
		.output_port_ready(u1_output_port_ready),
		.output_port_valid(u1_output_port_valid),

		.clock_port(clk),
		.reset_port(rst),
		.clear(u87_output_port)
	);

	apb_uart_UartTxPhy tx_phy (
		.clk(clk),
		.rst(rst),
		.data_in_data(u1_output_port_data),
		.data_in_ready(u1_output_port_ready),
		.data_in_valid(u1_output_port_valid),

		.parity(u64_output_port),
		.stop_cnt(u66_output_port),
		.word_size(word_size),
		.hw_flow_ctrl(hw_flow_ctrl),
		.use_tx_en(use_hw_tx_en),
		.prescaler_select(prescaler_select),
		.divider_limit(divider_limit),
		.txd(txd),
		.cts(u79_output_port),
		.cts_out(tx_phy_cts_out),
		.tx_en(tx_phy_tx_en)
	);

	assign tx_data_data = bus_if_pwdata;
	assign tx_data_valid = data_reg_wr;
	assign rx_data_ready = data_reg_rd;
	assign u64_output_port = config_reg[1:0];
	assign u66_output_port = config_reg[3:2];
	assign u74_output_port = config_reg[1:0];
	assign u76_output_port = config_reg[3:2];
	assign u79_output_port =  ~ cts;
	assign u85_output_port = 1'h0;
	assign u87_output_port = 1'h0;
endmodule


////////////////////////////////////////////////////////////////////////////////
// apb_uart_UartTxPhy
////////////////////////////////////////////////////////////////////////////////
module apb_uart_UartTxPhy (
	input logic clk,
	input logic rst,
	input logic [7:0] data_in_data,
	output logic data_in_ready,
	input logic data_in_valid,

	input logic [1:0] parity,
	input logic [1:0] stop_cnt,
	input logic [2:0] word_size,
	input logic hw_flow_ctrl,
	input logic use_tx_en,
	input logic [2:0] prescaler_select,
	input logic [7:0] divider_limit,
	output logic txd,
	input logic cts,
	output logic cts_out,
	output logic tx_en
);

	logic u2_output_port;
	logic cts_1;
	logic [6:0] prescaler_counter;
	logic [6:0] u8_output_port;
	logic prescaler_tick;
	logic divider_tick;
	logic [7:0] divider_counter;
	logic oversampler;
	logic baud_tick;
	logic baud_half_tick;
	logic starting;
	logic signed [3:0] u50_output_port;
	logic [2:0] bit_counter;
	logic [7:0] shift_reg;
	logic parity_reg;
	logic u81_output_port;
	logic u82_output_port;
	logic u91_output_port;
	logic u100_output_port;
	logic u109_output_port;
	logic u115_output_port;
	logic u118_output_port;
	logic u121_output_port;
	logic u124_output_port;
	logic [2:0] state;
	logic [2:0] fsm_next_state;

	always_ff @(posedge clk) u2_output_port <= rst ? 1'h0 : cts;
	always_ff @(posedge clk) cts_1 <= rst ? 1'h0 : u2_output_port;
	always_ff @(posedge clk) prescaler_counter <= rst ? 7'h0 : 7'(prescaler_counter + 1'h1 + 8'b0);
	always @(*) begin
		case (prescaler_select)
			3'd0: u8_output_port = 1'h0;
			3'd1: u8_output_port = 1'h1;
			3'd2: u8_output_port = 2'h3;
			3'd3: u8_output_port = 3'h7;
			3'd4: u8_output_port = 4'hf;
			3'd5: u8_output_port = 5'h1f;
			3'd6: u8_output_port = 6'h3f;
			3'd7: u8_output_port = 7'h7f;
			default: u8_output_port = 7'hx;
		endcase
	end
	assign prescaler_tick = (prescaler_counter & u8_output_port) == 1'h0;
	assign divider_tick = divider_counter == 1'h0 & prescaler_tick;
	always_ff @(posedge clk) divider_counter <= rst ? 8'h0 : prescaler_tick ? (divider_tick | state == `UartTxPhyStates__idle) ? divider_limit : $unsigned(8'(divider_counter - 1'h1 + 9'b0)) : divider_counter;
	always_ff @(posedge clk) oversampler <= rst ? 1'h0 : divider_tick ? (state == `UartTxPhyStates__idle ? 1'h0 : 1'b0) | (state == `UartTxPhyStates__stop_half ? oversampler : 1'b0) | (state == `UartTxPhyStates__tx_en ? oversampler : 1'b0) | (state == `UartTxPhyStates__idle | state == `UartTxPhyStates__stop_half | state == `UartTxPhyStates__tx_en ? 1'b0 :  ~ oversampler) : oversampler;
	assign data_in_ready = state == `UartTxPhyStates__idle & (cts_1 |  ~ hw_flow_ctrl);
	assign baud_tick = oversampler & divider_tick;
	assign starting = data_in_ready & data_in_valid;
	always_ff @(posedge clk) bit_counter <= rst ? 3'h0 : starting ? word_size : baud_tick ? u50_output_port[2:0] : bit_counter;
	always_ff @(posedge clk) shift_reg <= rst ? 8'h0 : starting ? data_in_data : (baud_tick & state == `UartTxPhyStates__data) ? shift_reg >> 1'h1 : shift_reg;
	always_ff @(posedge clk) parity_reg <= rst ? 1'h0 : starting ? parity == `UartParityType__odd : baud_tick ? parity_reg ^ shift_reg[0] : parity_reg;
	always_ff @(posedge clk) tx_en <= rst ? 1'h0 : state == `UartTxPhyStates__tx_en ? 1'h1 : state == `UartTxPhyStates__stop ? 1'h0 : tx_en;
	assign baud_half_tick =  ~ oversampler & divider_tick;
	always_ff @(posedge clk) txd <= rst ? 1'h1 : (state == `UartTxPhyStates__start ? 1'h0 : 1'b0) | (state == `UartTxPhyStates__data ? shift_reg[0] : 1'b0) | (state == `UartTxPhyStates__parity ? parity_reg : 1'b0) | (state == `UartTxPhyStates__start | state == `UartTxPhyStates__data | state == `UartTxPhyStates__parity ? 1'b0 : 1'h1);
	initial txd <= 1'h1;

	apb_uart_FSM_2 fsm (
		.clock_port(clk),
		.reset_port(rst),
		.reset_value(`UartTxPhyStates__idle),
		.state(state),
		.next_state(fsm_next_state),
		.default_state(`UartTxPhyStates__idle),
		.input_idle_to_start(u81_output_port),
		.input_idle_to_tx_en(u82_output_port),
		.input_tx_en_to_start(baud_half_tick),
		.input_start_to_data(baud_tick),
		.input_data_to_stop(u91_output_port),
		.input_data_to_stop_half(u100_output_port),
		.input_data_to_stop_two(u109_output_port),
		.input_data_to_parity(u115_output_port),
		.input_parity_to_stop(u118_output_port),
		.input_parity_to_stop_half(u121_output_port),
		.input_parity_to_stop_two(u124_output_port),
		.input_stop_two_to_stop(baud_tick),
		.input_stop_half_to_stop(baud_half_tick),
		.input_stop_to_idle(baud_tick)
	);

	assign cts_out = cts_1;
	assign u50_output_port = bit_counter - 1'h1 + 4'b0;
	assign u81_output_port = starting &  ~ use_tx_en;
	assign u82_output_port = starting & use_tx_en;
	assign u91_output_port = bit_counter == 1'h0 & parity == `UartParityType__none & baud_tick & stop_cnt == `UartStopBits__one;
	assign u100_output_port = bit_counter == 1'h0 & parity == `UartParityType__none & baud_tick & stop_cnt == `UartStopBits__one_and_half;
	assign u109_output_port = bit_counter == 1'h0 & parity == `UartParityType__none & baud_tick & stop_cnt == `UartStopBits__two;
	assign u115_output_port = bit_counter == 1'h0 & parity != `UartParityType__none & baud_tick;
	assign u118_output_port = baud_tick & stop_cnt == `UartStopBits__one;
	assign u121_output_port = baud_tick & stop_cnt == `UartStopBits__one_and_half;
	assign u124_output_port = baud_tick & stop_cnt == `UartStopBits__two;
endmodule


////////////////////////////////////////////////////////////////////////////////
// apb_uart_ForwardBuf_2
////////////////////////////////////////////////////////////////////////////////
module apb_uart_ForwardBuf_2 (
	input logic [7:0] input_port_data,
	output logic input_port_ready,
	input logic input_port_valid,

	output logic [7:0] output_port_data,
	input logic output_port_ready,
	output logic output_port_valid,

	input logic clock_port,
	input logic reset_port,
	input logic clear
);

	logic [7:0] buf_data_data;
	logic fsm_out_reg_en;

	always_ff @(posedge clock_port) buf_data_data <= reset_port ? 8'h0 : fsm_out_reg_en ? input_port_data : buf_data_data;

	apb_uart_ForwardBufLogic_2 fsm (
		.clock_port(clock_port),
		.reset_port(reset_port),
		.input_valid(input_port_valid),
		.input_ready(input_port_ready),
		.output_valid(output_port_valid),
		.output_ready(output_port_ready),
		.out_reg_en(fsm_out_reg_en),
		.clear(clear)
	);

	assign output_port_data = buf_data_data;
endmodule


////////////////////////////////////////////////////////////////////////////////
// apb_uart_ForwardBuf
////////////////////////////////////////////////////////////////////////////////
module apb_uart_ForwardBuf (
	input logic [7:0] input_port_data,
	output logic input_port_ready,
	input logic input_port_valid,

	output logic [7:0] output_port_data,
	input logic output_port_ready,
	output logic output_port_valid,

	input logic clock_port,
	input logic reset_port,
	input logic clear
);

	logic [7:0] buf_data_data;
	logic fsm_out_reg_en;

	always_ff @(posedge clock_port) buf_data_data <= reset_port ? 8'h0 : fsm_out_reg_en ? input_port_data : buf_data_data;

	apb_uart_ForwardBufLogic fsm (
		.clock_port(clock_port),
		.reset_port(reset_port),
		.input_valid(input_port_valid),
		.input_ready(input_port_ready),
		.output_valid(output_port_valid),
		.output_ready(output_port_ready),
		.out_reg_en(fsm_out_reg_en),
		.clear(clear)
	);

	assign output_port_data = buf_data_data;
endmodule


////////////////////////////////////////////////////////////////////////////////
// apb_uart_ForwardBufLogic_2
////////////////////////////////////////////////////////////////////////////////
module apb_uart_ForwardBufLogic_2 (
	input logic clock_port,
	input logic reset_port,
	input logic input_valid,
	output logic input_ready,
	output logic output_valid,
	input logic output_ready,
	output logic out_reg_en,
	input logic clear
);

	logic buf_valid;

	assign out_reg_en = input_valid & input_ready;
	always_ff @(posedge clock_port) buf_valid <= reset_port ? 1'h0 : clear ? 1'h0 : (input_valid & input_ready) ? 1'h1 : (output_ready & buf_valid) ? 1'h0 : buf_valid;
	assign input_ready =  ~ buf_valid | output_ready;

	assign output_valid = buf_valid;
endmodule


////////////////////////////////////////////////////////////////////////////////
// apb_uart_ForwardBufLogic
////////////////////////////////////////////////////////////////////////////////
module apb_uart_ForwardBufLogic (
	input logic clock_port,
	input logic reset_port,
	input logic input_valid,
	output logic input_ready,
	output logic output_valid,
	input logic output_ready,
	output logic out_reg_en,
	input logic clear
);

	logic buf_valid;

	assign out_reg_en = input_valid & input_ready;
	always_ff @(posedge clock_port) buf_valid <= reset_port ? 1'h0 : clear ? 1'h0 : (input_valid & input_ready) ? 1'h1 : (output_ready & buf_valid) ? 1'h0 : buf_valid;
	assign input_ready =  ~ buf_valid | output_ready;

	assign output_valid = buf_valid;
endmodule


////////////////////////////////////////////////////////////////////////////////
// apb_uart_UartRxPhy
////////////////////////////////////////////////////////////////////////////////
module apb_uart_UartRxPhy (
	input logic clk,
	input logic rst,
	output logic [7:0] data_out_data,
	input logic data_out_ready,
	output logic data_out_valid,

	input logic [1:0] parity,
	input logic [1:0] stop_cnt,
	input logic [2:0] word_size,
	input logic hw_flow_ctrl,
	input logic rxd,
	output logic rts,
	output logic framing_error,
	output logic parity_error,
	output logic overrun_error,
	input logic clear,
	input logic [2:0] prescaler_select,
	input logic [7:0] divider_limit,
	input logic enable
);

	logic [6:0] prescaler_counter;
	logic [6:0] u6_output_port;
	logic prescaler_tick;
	logic divider_tick;
	logic [7:0] divider_counter;
	logic oversampler;
	logic baud_tick;
	logic baud_half_tick;
	logic starting;
	logic rx_full;
	logic signed [3:0] u58_output_port;
	logic [2:0] bit_counter;
	logic [7:0] shift_reg;
	logic ref_parity_reg;
	logic u118_output_port;
	logic u119_output_port;
	logic u129_output_port;
	logic u139_output_port;
	logic u149_output_port;
	logic u156_output_port;
	logic u160_output_port;
	logic u164_output_port;
	logic u168_output_port;
	logic u169_output_port;
	logic u170_output_port;
	logic u171_output_port;
	logic u172_output_port;
	logic u173_output_port;
	logic u174_output_port;
	logic u175_output_port;
	logic u176_output_port;
	logic u177_output_port;
	logic u178_output_port;
	logic u179_output_port;
	logic rxd_1;
	logic [2:0] state;
	logic [2:0] next_state;

	always_ff @(posedge clk) prescaler_counter <= rst ? 7'h0 : 7'(prescaler_counter + 1'h1 + 8'b0);
	always @(*) begin
		case (prescaler_select)
			3'd0: u6_output_port = 1'h0;
			3'd1: u6_output_port = 1'h1;
			3'd2: u6_output_port = 2'h3;
			3'd3: u6_output_port = 3'h7;
			3'd4: u6_output_port = 4'hf;
			3'd5: u6_output_port = 5'h1f;
			3'd6: u6_output_port = 6'h3f;
			3'd7: u6_output_port = 7'h7f;
			default: u6_output_port = 7'hx;
		endcase
	end
	assign prescaler_tick = (prescaler_counter & u6_output_port) == 1'h0;
	assign divider_tick = divider_counter == 1'h0 & prescaler_tick;
	always_ff @(posedge clk) divider_counter <= rst ? 8'h0 : prescaler_tick ? (divider_tick | state == `UartRxPhyStates__idle) ? divider_limit : $unsigned(8'(divider_counter - 1'h1 + 9'b0)) : divider_counter;
	always_ff @(posedge clk) oversampler <= rst ? 1'h0 : divider_tick ? (state == `UartRxPhyStates__idle ? 1'h0 : 1'b0) | (state == `UartRxPhyStates__stop_half ? oversampler : 1'b0) | (state == `UartRxPhyStates__idle | state == `UartRxPhyStates__stop_half ? 1'b0 :  ~ oversampler) : oversampler;
	assign baud_tick = oversampler & divider_tick;
	always_ff @(posedge clk) rx_full <= rst ? 1'h0 : (state == `UartRxPhyStates__data & bit_counter == 1'h0 & baud_tick) ? 1'h1 : data_out_ready ? 1'h0 : rx_full;
	assign starting =  ~ rx_full & state == `UartRxPhyStates__idle &  ~ rxd_1 & enable;
	always_ff @(posedge clk) bit_counter <= rst ? 3'h0 : starting ? word_size : baud_tick ? u58_output_port[2:0] : bit_counter;
	assign baud_half_tick =  ~ oversampler & divider_tick;
	always_ff @(posedge clk) shift_reg <= rst ? 8'h0 : starting ? 1'h0 : (baud_half_tick & next_state == `UartRxPhyStates__data) ? ({rxd_1, shift_reg[7:1]}) : shift_reg;
	always_ff @(posedge clk) ref_parity_reg <= rst ? 1'h0 : starting ? parity == `UartParityType__odd : baud_tick ? ref_parity_reg ^ rxd_1 : ref_parity_reg;
	always_ff @(posedge clk) parity_error <= rst ? 1'h0 : clear ? 1'h0 : (state == `UartRxPhyStates__parity & baud_tick) ? parity_error | rxd_1 != ref_parity_reg : parity_error;
	always_ff @(posedge clk) framing_error <= rst ? 1'h0 : clear ? 1'h0 : ((state == `UartRxPhyStates__stop_half | state == `UartRxPhyStates__stop_two | state == `UartRxPhyStates__stop) & rxd_1 == 1'h0) ? 1'h1 : framing_error;
	always_ff @(posedge clk) overrun_error <= rst ? 1'h0 : clear ? 1'h0 : (state == `UartRxPhyStates__data & bit_counter == 1'h0 & baud_tick & rx_full &  ~ data_out_ready) ? 1'h1 : overrun_error;
	always_ff @(posedge clk) u179_output_port <= rst ? 1'h1 : rxd;
	initial u179_output_port <= 1'h1;
	always_ff @(posedge clk) rxd_1 <= rst ? 1'h1 : u179_output_port;
	initial rxd_1 <= 1'h1;
	assign rts = hw_flow_ctrl ?  ~ rx_full : 1'h0;

	apb_uart_FSM fsm (
		.clock_port(clk),
		.reset_port(rst),
		.reset_value(`UartRxPhyStates__idle),
		.state(state),
		.next_state(next_state),
		.default_state(`UartRxPhyStates__idle),
		.input_idle_to_half_start(starting),
		.input_half_start_to_start(u118_output_port),
		.input_start_to_data(u119_output_port),
		.input_data_to_stop(u129_output_port),
		.input_data_to_stop_half(u139_output_port),
		.input_data_to_stop_two(u149_output_port),
		.input_data_to_parity(u156_output_port),
		.input_parity_to_stop(u160_output_port),
		.input_parity_to_stop_half(u164_output_port),
		.input_parity_to_stop_two(u168_output_port),
		.input_stop_two_to_stop(u169_output_port),
		.input_stop_half_to_stop(u170_output_port),
		.input_stop_to_idle(u171_output_port),
		.input_half_start_to_idle(u172_output_port),
		.input_start_to_idle(u173_output_port),
		.input_data_to_idle(u174_output_port),
		.input_parity_to_idle(u175_output_port),
		.input_stop_two_to_idle(u176_output_port),
		.input_stop_half_to_idle(u177_output_port),
		.input_stop_to_idle_1(u178_output_port)
	);

	assign data_out_valid = rx_full;
	assign u58_output_port = bit_counter - 1'h1 + 4'b0;
	assign data_out_data = shift_reg;
	assign u118_output_port = enable & baud_half_tick;
	assign u119_output_port = enable & baud_half_tick;
	assign u129_output_port = enable & baud_tick & bit_counter == 1'h0 & parity == `UartParityType__none & stop_cnt == `UartStopBits__one;
	assign u139_output_port = enable & baud_tick & bit_counter == 1'h0 & parity == `UartParityType__none & stop_cnt == `UartStopBits__one_and_half;
	assign u149_output_port = enable & baud_tick & bit_counter == 1'h0 & parity == `UartParityType__none & stop_cnt == `UartStopBits__two;
	assign u156_output_port = enable & baud_half_tick & bit_counter == 1'h0 & parity != `UartParityType__none;
	assign u160_output_port = enable & baud_tick & stop_cnt == `UartStopBits__one;
	assign u164_output_port = enable & baud_tick & stop_cnt == `UartStopBits__one_and_half;
	assign u168_output_port = enable & baud_tick & stop_cnt == `UartStopBits__two;
	assign u169_output_port = enable & baud_tick;
	assign u170_output_port = enable & baud_half_tick;
	assign u171_output_port = enable & baud_tick;
	assign u172_output_port =  ~ enable;
	assign u173_output_port =  ~ enable;
	assign u174_output_port =  ~ enable;
	assign u175_output_port =  ~ enable;
	assign u176_output_port =  ~ enable;
	assign u177_output_port =  ~ enable;
	assign u178_output_port =  ~ enable;
endmodule


////////////////////////////////////////////////////////////////////////////////
// apb_uart_FSM_2
////////////////////////////////////////////////////////////////////////////////
module apb_uart_FSM_2 (
	input logic clock_port,
	input logic reset_port,
	input logic [2:0] reset_value,
	output logic [2:0] state,
	output logic [2:0] next_state,
	input logic [2:0] default_state,
	input logic input_idle_to_start,
	input logic input_idle_to_tx_en,
	input logic input_tx_en_to_start,
	input logic input_start_to_data,
	input logic input_data_to_stop,
	input logic input_data_to_stop_half,
	input logic input_data_to_stop_two,
	input logic input_data_to_parity,
	input logic input_parity_to_stop,
	input logic input_parity_to_stop_half,
	input logic input_parity_to_stop_two,
	input logic input_stop_two_to_stop,
	input logic input_stop_half_to_stop,
	input logic input_stop_to_idle
);

	logic [2:0] local_state;
	logic [2:0] local_next_state;

	always_ff @(posedge clock_port) local_state <= reset_port ? reset_value : local_next_state;
	initial local_state <= reset_value;

	apb_uart_FSMLogic_2 fsm_logic (
		.state(local_state),
		.next_state(local_next_state),
		.default_state(default_state),
		.input_idle_to_start(input_idle_to_start),
		.input_idle_to_tx_en(input_idle_to_tx_en),
		.input_tx_en_to_start(input_tx_en_to_start),
		.input_start_to_data(input_start_to_data),
		.input_data_to_stop(input_data_to_stop),
		.input_data_to_stop_half(input_data_to_stop_half),
		.input_data_to_stop_two(input_data_to_stop_two),
		.input_data_to_parity(input_data_to_parity),
		.input_parity_to_stop(input_parity_to_stop),
		.input_parity_to_stop_half(input_parity_to_stop_half),
		.input_parity_to_stop_two(input_parity_to_stop_two),
		.input_stop_two_to_stop(input_start_to_data),
		.input_stop_half_to_stop(input_tx_en_to_start),
		.input_stop_to_idle(input_start_to_data)
	);

	assign state = local_state;
	assign next_state = local_next_state;
endmodule


////////////////////////////////////////////////////////////////////////////////
// apb_uart_FSM
////////////////////////////////////////////////////////////////////////////////
module apb_uart_FSM (
	input logic clock_port,
	input logic reset_port,
	input logic [2:0] reset_value,
	output logic [2:0] state,
	output logic [2:0] next_state,
	input logic [2:0] default_state,
	input logic input_idle_to_half_start,
	input logic input_half_start_to_start,
	input logic input_start_to_data,
	input logic input_data_to_stop,
	input logic input_data_to_stop_half,
	input logic input_data_to_stop_two,
	input logic input_data_to_parity,
	input logic input_parity_to_stop,
	input logic input_parity_to_stop_half,
	input logic input_parity_to_stop_two,
	input logic input_stop_two_to_stop,
	input logic input_stop_half_to_stop,
	input logic input_stop_to_idle,
	input logic input_half_start_to_idle,
	input logic input_start_to_idle,
	input logic input_data_to_idle,
	input logic input_parity_to_idle,
	input logic input_stop_two_to_idle,
	input logic input_stop_half_to_idle,
	input logic input_stop_to_idle_1
);

	logic [2:0] local_state;
	logic [2:0] local_next_state;

	always_ff @(posedge clock_port) local_state <= reset_port ? reset_value : local_next_state;
	initial local_state <= reset_value;

	apb_uart_FSMLogic fsm_logic (
		.state(local_state),
		.next_state(local_next_state),
		.default_state(default_state),
		.input_idle_to_half_start(input_idle_to_half_start),
		.input_half_start_to_start(input_half_start_to_start),
		.input_start_to_data(input_start_to_data),
		.input_data_to_stop(input_data_to_stop),
		.input_data_to_stop_half(input_data_to_stop_half),
		.input_data_to_stop_two(input_data_to_stop_two),
		.input_data_to_parity(input_data_to_parity),
		.input_parity_to_stop(input_parity_to_stop),
		.input_parity_to_stop_half(input_parity_to_stop_half),
		.input_parity_to_stop_two(input_parity_to_stop_two),
		.input_stop_two_to_stop(input_stop_two_to_stop),
		.input_stop_half_to_stop(input_stop_half_to_stop),
		.input_stop_to_idle(input_stop_to_idle),
		.input_half_start_to_idle(input_half_start_to_idle),
		.input_start_to_idle(input_start_to_idle),
		.input_data_to_idle(input_data_to_idle),
		.input_parity_to_idle(input_parity_to_idle),
		.input_stop_two_to_idle(input_stop_two_to_idle),
		.input_stop_half_to_idle(input_stop_half_to_idle),
		.input_stop_to_idle_1(input_stop_to_idle_1)
	);

	assign state = local_state;
	assign next_state = local_next_state;
endmodule


////////////////////////////////////////////////////////////////////////////////
// apb_uart_FSMLogic_2
////////////////////////////////////////////////////////////////////////////////
module apb_uart_FSMLogic_2 (
	input logic [2:0] state,
	output logic [2:0] next_state,
	input logic [2:0] default_state,
	input logic input_idle_to_start,
	input logic input_idle_to_tx_en,
	input logic input_tx_en_to_start,
	input logic input_start_to_data,
	input logic input_data_to_stop,
	input logic input_data_to_stop_half,
	input logic input_data_to_stop_two,
	input logic input_data_to_parity,
	input logic input_parity_to_stop,
	input logic input_parity_to_stop_half,
	input logic input_parity_to_stop_two,
	input logic input_stop_two_to_stop,
	input logic input_stop_half_to_stop,
	input logic input_stop_to_idle
);

	logic [2:0] state_idle_selector;
	logic [2:0] state_tx_en_selector;
	logic [2:0] state_start_selector;
	logic [2:0] state_data_selector;
	logic [2:0] state_parity_selector;
	logic [2:0] state_stop_two_selector;
	logic [2:0] state_stop_half_selector;
	logic [2:0] state_stop_selector;

	assign state_idle_selector = (input_idle_to_start ? `UartTxPhyStates__start : 3'b0) | (input_idle_to_tx_en ? `UartTxPhyStates__tx_en : 3'b0) | (input_idle_to_start | input_idle_to_tx_en ? 3'b0 : `UartTxPhyStates__idle);
	assign state_tx_en_selector = (input_tx_en_to_start ? `UartTxPhyStates__start : 3'b0) | (input_tx_en_to_start ? 3'b0 : `UartTxPhyStates__tx_en);
	assign state_start_selector = (input_start_to_data ? `UartTxPhyStates__data : 3'b0) | (input_start_to_data ? 3'b0 : `UartTxPhyStates__start);
	assign state_data_selector = 
		(input_data_to_stop ? `UartTxPhyStates__stop : 3'b0) | 
		(input_data_to_stop_half ? `UartTxPhyStates__stop_half : 3'b0) | 
		(input_data_to_stop_two ? `UartTxPhyStates__stop_two : 3'b0) | 
		(input_data_to_parity ? `UartTxPhyStates__parity : 3'b0) | 
		(input_data_to_stop | input_data_to_stop_half | input_data_to_stop_two | input_data_to_parity ? 3'b0 : `UartTxPhyStates__data);
	assign state_parity_selector = (input_parity_to_stop ? `UartTxPhyStates__stop : 3'b0) | (input_parity_to_stop_half ? `UartTxPhyStates__stop_half : 3'b0) | (input_parity_to_stop_two ? `UartTxPhyStates__stop_two : 3'b0) | (input_parity_to_stop | input_parity_to_stop_half | input_parity_to_stop_two ? 3'b0 : `UartTxPhyStates__parity);
	assign state_stop_two_selector = (input_start_to_data ? `UartTxPhyStates__stop : 3'b0) | (input_start_to_data ? 3'b0 : `UartTxPhyStates__stop_two);
	assign state_stop_half_selector = (input_tx_en_to_start ? `UartTxPhyStates__stop : 3'b0) | (input_tx_en_to_start ? 3'b0 : `UartTxPhyStates__stop_half);
	assign state_stop_selector = (input_start_to_data ? `UartTxPhyStates__idle : 3'b0) | (input_start_to_data ? 3'b0 : `UartTxPhyStates__stop);
	assign next_state = 
		(state == `UartTxPhyStates__idle ? state_idle_selector : 3'b0) | 
		(state == `UartTxPhyStates__tx_en ? state_tx_en_selector : 3'b0) | 
		(state == `UartTxPhyStates__start ? state_start_selector : 3'b0) | 
		(state == `UartTxPhyStates__data ? state_data_selector : 3'b0) | 
		(state == `UartTxPhyStates__parity ? state_parity_selector : 3'b0) | 
		(state == `UartTxPhyStates__stop_two ? state_stop_two_selector : 3'b0) | 
		(state == `UartTxPhyStates__stop_half ? state_stop_half_selector : 3'b0) | 
		(state == `UartTxPhyStates__stop ? state_stop_selector : 3'b0) | 
		(state == `UartTxPhyStates__idle | state == `UartTxPhyStates__tx_en | state == `UartTxPhyStates__start | state == `UartTxPhyStates__data | state == `UartTxPhyStates__parity | state == `UartTxPhyStates__stop_two | state == `UartTxPhyStates__stop_half | state == `UartTxPhyStates__stop ? 3'b0 : default_state);

endmodule


////////////////////////////////////////////////////////////////////////////////
// apb_uart_FSMLogic
////////////////////////////////////////////////////////////////////////////////
module apb_uart_FSMLogic (
	input logic [2:0] state,
	output logic [2:0] next_state,
	input logic [2:0] default_state,
	input logic input_idle_to_half_start,
	input logic input_half_start_to_start,
	input logic input_start_to_data,
	input logic input_data_to_stop,
	input logic input_data_to_stop_half,
	input logic input_data_to_stop_two,
	input logic input_data_to_parity,
	input logic input_parity_to_stop,
	input logic input_parity_to_stop_half,
	input logic input_parity_to_stop_two,
	input logic input_stop_two_to_stop,
	input logic input_stop_half_to_stop,
	input logic input_stop_to_idle,
	input logic input_half_start_to_idle,
	input logic input_start_to_idle,
	input logic input_data_to_idle,
	input logic input_parity_to_idle,
	input logic input_stop_two_to_idle,
	input logic input_stop_half_to_idle,
	input logic input_stop_to_idle_1
);

	logic [2:0] state_idle_selector;
	logic [2:0] state_half_start_selector;
	logic [2:0] state_start_selector;
	logic [2:0] state_data_selector;
	logic [2:0] state_parity_selector;
	logic [2:0] state_stop_two_selector;
	logic [2:0] state_stop_half_selector;
	logic [2:0] state_stop_selector;

	assign state_idle_selector = (input_idle_to_half_start ? `UartRxPhyStates__half_start : 3'b0) | (input_idle_to_half_start ? 3'b0 : `UartRxPhyStates__idle);
	assign state_half_start_selector = (input_half_start_to_start ? `UartRxPhyStates__start : 3'b0) | (input_half_start_to_idle ? `UartRxPhyStates__idle : 3'b0) | (input_half_start_to_start | input_half_start_to_idle ? 3'b0 : `UartRxPhyStates__half_start);
	assign state_start_selector = (input_start_to_data ? `UartRxPhyStates__data : 3'b0) | (input_start_to_idle ? `UartRxPhyStates__idle : 3'b0) | (input_start_to_data | input_start_to_idle ? 3'b0 : `UartRxPhyStates__start);
	assign state_data_selector = 
		(input_data_to_stop ? `UartRxPhyStates__stop : 3'b0) | 
		(input_data_to_stop_half ? `UartRxPhyStates__stop_half : 3'b0) | 
		(input_data_to_stop_two ? `UartRxPhyStates__stop_two : 3'b0) | 
		(input_data_to_parity ? `UartRxPhyStates__parity : 3'b0) | 
		(input_data_to_idle ? `UartRxPhyStates__idle : 3'b0) | 
		(input_data_to_stop | input_data_to_stop_half | input_data_to_stop_two | input_data_to_parity | input_data_to_idle ? 3'b0 : `UartRxPhyStates__data);
	assign state_parity_selector = 
		(input_parity_to_stop ? `UartRxPhyStates__stop : 3'b0) | 
		(input_parity_to_stop_half ? `UartRxPhyStates__stop_half : 3'b0) | 
		(input_parity_to_stop_two ? `UartRxPhyStates__stop_two : 3'b0) | 
		(input_parity_to_idle ? `UartRxPhyStates__idle : 3'b0) | 
		(input_parity_to_stop | input_parity_to_stop_half | input_parity_to_stop_two | input_parity_to_idle ? 3'b0 : `UartRxPhyStates__parity);
	assign state_stop_two_selector = (input_stop_two_to_stop ? `UartRxPhyStates__stop : 3'b0) | (input_stop_two_to_idle ? `UartRxPhyStates__idle : 3'b0) | (input_stop_two_to_stop | input_stop_two_to_idle ? 3'b0 : `UartRxPhyStates__stop_two);
	assign state_stop_half_selector = (input_stop_half_to_stop ? `UartRxPhyStates__stop : 3'b0) | (input_stop_half_to_idle ? `UartRxPhyStates__idle : 3'b0) | (input_stop_half_to_stop | input_stop_half_to_idle ? 3'b0 : `UartRxPhyStates__stop_half);
	assign state_stop_selector = (input_stop_to_idle ? `UartRxPhyStates__idle : 3'b0) | (input_stop_to_idle_1 ? `UartRxPhyStates__idle : 3'b0) | (input_stop_to_idle | input_stop_to_idle_1 ? 3'b0 : `UartRxPhyStates__stop);
	assign next_state = 
		(state == `UartRxPhyStates__idle ? state_idle_selector : 3'b0) | 
		(state == `UartRxPhyStates__half_start ? state_half_start_selector : 3'b0) | 
		(state == `UartRxPhyStates__start ? state_start_selector : 3'b0) | 
		(state == `UartRxPhyStates__data ? state_data_selector : 3'b0) | 
		(state == `UartRxPhyStates__parity ? state_parity_selector : 3'b0) | 
		(state == `UartRxPhyStates__stop_two ? state_stop_two_selector : 3'b0) | 
		(state == `UartRxPhyStates__stop_half ? state_stop_half_selector : 3'b0) | 
		(state == `UartRxPhyStates__stop ? state_stop_selector : 3'b0) | 
		(state == `UartRxPhyStates__idle | state == `UartRxPhyStates__half_start | state == `UartRxPhyStates__start | state == `UartRxPhyStates__data | state == `UartRxPhyStates__parity | state == `UartRxPhyStates__stop_two | state == `UartRxPhyStates__stop_half | state == `UartRxPhyStates__stop ? 3'b0 : default_state);

endmodule


