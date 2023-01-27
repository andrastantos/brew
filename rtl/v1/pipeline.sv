////////////////////////////////////////////////////////////////////////////////
// Type definitions
////////////////////////////////////////////////////////////////////////////////
`define exec__adder 3'h0
`define exec__mult 3'h1
`define exec__shift 3'h2
`define exec__bitwise 3'h3
`define exec__misc 3'h4
`define exec__cbranch 3'h5
`define exec__bbranch 3'h6


`define op__add 3'h0
`define op__a_sub_b 3'h1
`define op__b_sub_a 3'h2
`define op__addr 3'h3
`define op__pc_add 3'h4
`define op__cb_lt 3'h5
`define op__cb_ge 3'h6


`define ArbStates__idle 2'h0
`define ArbStates__fetch 2'h1
`define ArbStates__external 2'h2
`define ArbStates__memory 2'h3


`define MemoryStates__idle 4'h0
`define MemoryStates__read_1 4'h1
`define MemoryStates__read_2 4'h3
`define MemoryStates__write 4'h5
`define MemoryStates__csr_read 4'h8
`define MemoryStates__csr_write 4'h9


`define InstBufferStates__idle 3'h0
`define InstBufferStates__request_start 3'h1
`define InstBufferStates__request 3'h2
`define InstBufferStates__request_last 3'h3
`define InstBufferStates__flush_start 3'h4
`define InstBufferStates__flush 3'h5


`define InstAssembleStates__have_0_fragments 2'h0
`define InstAssembleStates__need_1_fragments 2'h1
`define InstAssembleStates__need_2_fragments 2'h2
`define InstAssembleStates__have_all_fragments 2'h3





////////////////////////////////////////////////////////////////////////////////
// Pipeline
////////////////////////////////////////////////////////////////////////////////
module Pipeline (
	input logic clk,
	input logic rst,
	output logic DRAM_nRAS,
	output logic DRAM_nCAS_l,
	output logic DRAM_nCAS_h,
	output logic [11:0] DRAM_ADDR,
	output logic DRAM_nWE,
	input logic [7:0] DRAM_DATA_rd_l,
	input logic [7:0] DRAM_DATA_rd_h,
	output logic [15:0] DRAM_DATA_wr,
	input logic ext_req,
	output logic ext_grnt,
	input logic interrupt
);

	logic exec_to_mem_result_reg_addr_valid;
	logic [30:0] spc;
	logic [30:0] tpc;
	logic task_mode;
	logic [11:0] ecause;
	logic [11:0] rcause;
	logic u7_output_port;
	logic csr_if_response;
	logic [3:0] u10_output_port;
	logic [31:0] csr_if_rd_data;
	logic [21:0] mem_base;
	logic [21:0] mem_limit;
	logic [3:0] wait_states_3;
	logic [3:0] wait_states_2;
	logic [3:0] wait_states_1;
	logic [3:0] wait_states_0;
	logic mem_to_bus_response;
	logic fetch_to_bus_response;
	logic mem_to_bus_last;
	logic [15:0] mem_to_bus_data_out;
	logic fetch_to_bus_last;
	logic [15:0] fetch_to_bus_data_out;
	logic [30:0] fetch_to_bus_addr;
	logic fetch_to_bus_read_not_write;
	logic [1:0] fetch_to_bus_burst_len;
	logic [1:0] fetch_to_bus_byte_en;
	/*X*/ logic fetch_to_bus_data_in;
	logic fetch_to_bus_request;
	logic fetch_to_decode_valid;
	logic fetch_to_decode_av;
	logic [15:0] fetch_to_decode_inst_0;
	logic [15:0] fetch_to_decode_inst_1;
	logic [15:0] fetch_to_decode_inst_2;
	logic [1:0] fetch_to_decode_inst_len;
	logic rf_read1_valid;
	logic rf_read2_valid;
	logic rf_rsv_valid;
	logic rf_request;
	logic fetch_to_decode_ready;
	logic decode_to_exec_do_bse;
	logic decode_to_exec_do_bze;
	logic decode_to_exec_do_wse;
	logic decode_to_exec_do_wze;
	logic [2:0] decode_to_exec_exec_unit;
	logic decode_to_exec_fetch_av;
	logic [1:0] decode_to_exec_inst_len;
	logic decode_to_exec_is_load;
	logic decode_to_exec_is_store;
	logic [1:0] decode_to_exec_mem_access_len;
	logic [31:0] decode_to_exec_op_a;
	logic [31:0] decode_to_exec_op_b;
	logic [31:0] decode_to_exec_op_imm;
	logic [2:0] decode_to_exec_opcode;
	logic [3:0] decode_to_exec_result_reg_addr;
	logic decode_to_exec_result_reg_addr_valid;
	logic decode_to_exec_valid;
	logic [3:0] rf_read1_addr;
	logic [3:0] rf_read2_addr;
	logic [3:0] rf_rsv_addr;
	logic do_branch;
	logic [1:0] exec_to_mem_mem_access_len;
	logic [31:0] exec_to_mem_result;
	logic [3:0] exec_to_mem_result_reg_addr;
	logic [31:0] exec_to_mem_mem_addr;
	logic exec_to_mem_is_load;
	logic exec_to_mem_is_store;
	logic exec_to_mem_do_bse;
	logic exec_to_mem_do_wse;
	logic exec_to_mem_do_bze;
	logic exec_to_mem_do_wze;
	logic [30:0] execute_stage_spc_out;
	logic [30:0] execute_stage_tpc_out;
	logic execute_stage_task_mode_out;
	logic [11:0] execute_stage_ecause_out;
	logic [11:0] execute_stage_rcause_out;
	logic exec_to_mem_valid;
	logic decode_to_exec_ready;
	logic exec_to_mem_ready;
	logic [31:0] wb_result;
	logic wb_request;
	logic mem_to_bus_request;
	logic [1:0] mem_to_bus_byte_en;
	logic [15:0] mem_to_bus_data_in;
	logic csr_if_request;
	logic mem_to_bus_read_not_write;
	logic csr_if_read_not_write;
	logic [31:0] csr_if_wr_data;
	logic mem_to_bus_burst_len;
	logic [30:0] mem_to_bus_addr;
	logic [10:0] csr_if_addr;
	logic [3:0] wb_result_reg_addr;
	logic [31:0] rf_read1_data;
	logic [31:0] rf_read2_data;
	logic rf_response;
	logic [3:0] csr_addr;

	always_ff @(posedge clk) spc <= rst ? 31'h0 : execute_stage_spc_out;
	always_ff @(posedge clk) tpc <= rst ? 31'h0 : execute_stage_tpc_out;
	always_ff @(posedge clk) task_mode <= rst ? 1'h0 : execute_stage_task_mode_out;
	always_ff @(posedge clk) ecause <= rst ? 12'h0 : execute_stage_ecause_out;
	always_ff @(posedge clk) rcause <= rst ? 12'h0 : execute_stage_rcause_out;
	assign csr_addr = csr_if_addr[3:0];
	always_ff @(posedge clk) u7_output_port <= rst ? 1'h0 : csr_if_request;
	assign csr_if_response = csr_if_request & ( ~ csr_if_read_not_write | u7_output_port);
	always_ff @(posedge clk) u10_output_port <= rst ? 4'h0 : csr_addr;
	assign csr_if_rd_data = u10_output_port == 0 ? 1'h0 : 32'b0 | u10_output_port == 1 ? ({mem_base, 10'h0}) : 32'b0 | u10_output_port == 2 ? ({mem_limit, 10'h0}) : 32'b0 | u10_output_port == 3 ? ecause : 32'b0 | u10_output_port == 4 ? rcause : 32'b0 | u10_output_port == 5 ? ({wait_states_3, wait_states_2, wait_states_1, wait_states_0}) : 32'b0;
	always_ff @(posedge clk) mem_base <= rst ? 22'h0 : csr_addr == 1'h1 &  ~ csr_if_read_not_write & csr_if_request ? csr_if_rd_data[31:10] : mem_base;
	always_ff @(posedge clk) mem_limit <= rst ? 22'h0 : csr_addr == 2'h2 &  ~ csr_if_read_not_write & csr_if_request ? csr_if_rd_data[31:10] : mem_limit;
	always_ff @(posedge clk) wait_states_3 <= rst ? 4'h0 : csr_addr == 3'h5 &  ~ csr_if_read_not_write & csr_if_request ? csr_if_rd_data[15:12] : wait_states_3;
	always_ff @(posedge clk) wait_states_2 <= rst ? 4'h0 : csr_addr == 3'h5 &  ~ csr_if_read_not_write & csr_if_request ? csr_if_rd_data[11:8] : wait_states_2;
	always_ff @(posedge clk) wait_states_1 <= rst ? 4'h0 : csr_addr == 3'h5 &  ~ csr_if_read_not_write & csr_if_request ? csr_if_rd_data[7:4] : wait_states_1;
	always_ff @(posedge clk) wait_states_0 <= rst ? 4'h0 : csr_addr == 3'h5 &  ~ csr_if_read_not_write & csr_if_request ? csr_if_rd_data[3:0] : wait_states_0;

	BusIf bus_if (
		.clk(clk),
		.rst(rst),
		.fetch_addr(fetch_to_bus_addr),
		.fetch_burst_len(fetch_to_bus_burst_len),
		.fetch_byte_en(fetch_to_bus_byte_en),
		.fetch_data_in(fetch_to_bus_data_in),
		.fetch_data_out(fetch_to_bus_data_out),
		.fetch_last(fetch_to_bus_last),
		.fetch_read_not_write(fetch_to_bus_read_not_write),
		.fetch_request(fetch_to_bus_request),
		.fetch_response(fetch_to_bus_response),

		.mem_addr(mem_to_bus_addr),
		.mem_burst_len(mem_to_bus_burst_len),
		.mem_byte_en(mem_to_bus_byte_en),
		.mem_data_in(mem_to_bus_data_in),
		.mem_data_out(mem_to_bus_data_out),
		.mem_last(mem_to_bus_last),
		.mem_read_not_write(mem_to_bus_read_not_write),
		.mem_request(mem_to_bus_request),
		.mem_response(mem_to_bus_response),

		.DRAM_nRAS(DRAM_nRAS),
		.DRAM_nCAS_l(DRAM_nCAS_l),
		.DRAM_nCAS_h(DRAM_nCAS_h),
		.DRAM_ADDR(DRAM_ADDR),
		.DRAM_nWE(DRAM_nWE),
		.DRAM_DATA_rd_h(DRAM_DATA_rd_h),
		.DRAM_DATA_rd_l(DRAM_DATA_rd_l),
		.DRAM_DATA_wr(DRAM_DATA_wr),
		.ext_req(ext_req),
		.ext_grnt(ext_grnt),
		.wait_states_0(wait_states_0),
		.wait_states_1(wait_states_1),
		.wait_states_2(wait_states_2),
		.wait_states_3(wait_states_3)
	);

	FetchStage fetch_stage (
		.clk(clk),
		.rst(rst),
		.bus_if_addr(fetch_to_bus_addr),
		.bus_if_burst_len(fetch_to_bus_burst_len),
		.bus_if_byte_en(fetch_to_bus_byte_en),
		.bus_if_data_in(fetch_to_bus_data_in),
		.bus_if_data_out(fetch_to_bus_data_out),
		.bus_if_last(fetch_to_bus_last),
		.bus_if_read_not_write(fetch_to_bus_read_not_write),
		.bus_if_request(fetch_to_bus_request),
		.bus_if_response(fetch_to_bus_response),

		.decode_av(fetch_to_decode_av),
		.decode_inst_0(fetch_to_decode_inst_0),
		.decode_inst_1(fetch_to_decode_inst_1),
		.decode_inst_2(fetch_to_decode_inst_2),
		.decode_inst_len(fetch_to_decode_inst_len),
		.decode_ready(fetch_to_decode_ready),
		.decode_valid(fetch_to_decode_valid),

		.mem_base(mem_base),
		.mem_limit(mem_limit),
		.spc(spc),
		.tpc(tpc),
		.task_mode(task_mode),
		.do_branch(do_branch)
	);

	DecodeStage decode_stage (
		.clk(clk),
		.rst(rst),
		.fetch_av(fetch_to_decode_av),
		.fetch_inst_0(fetch_to_decode_inst_0),
		.fetch_inst_1(fetch_to_decode_inst_1),
		.fetch_inst_2(fetch_to_decode_inst_2),
		.fetch_inst_len(fetch_to_decode_inst_len),
		.fetch_ready(fetch_to_decode_ready),
		.fetch_valid(fetch_to_decode_valid),

		.exec_do_bse(decode_to_exec_do_bse),
		.exec_do_bze(decode_to_exec_do_bze),
		.exec_do_wse(decode_to_exec_do_wse),
		.exec_do_wze(decode_to_exec_do_wze),
		.exec_exec_unit(decode_to_exec_exec_unit),
		.exec_fetch_av(decode_to_exec_fetch_av),
		.exec_inst_len(decode_to_exec_inst_len),
		.exec_is_load(decode_to_exec_is_load),
		.exec_is_store(decode_to_exec_is_store),
		.exec_mem_access_len(decode_to_exec_mem_access_len),
		.exec_op_a(decode_to_exec_op_a),
		.exec_op_b(decode_to_exec_op_b),
		.exec_op_imm(decode_to_exec_op_imm),
		.exec_opcode(decode_to_exec_opcode),
		.exec_ready(decode_to_exec_ready),
		.exec_result_reg_addr(decode_to_exec_result_reg_addr),
		.exec_result_reg_addr_valid(decode_to_exec_result_reg_addr_valid),
		.exec_valid(decode_to_exec_valid),

		.rf_request(rf_request),
		.rf_response(rf_response),
		.rf_read1_addr(rf_read1_addr),
		.rf_read1_data(rf_read1_data),
		.rf_read1_valid(rf_read1_valid),
		.rf_read2_addr(rf_read2_addr),
		.rf_read2_data(rf_read2_data),
		.rf_read2_valid(rf_read2_valid),
		.rf_rsv_addr(rf_rsv_addr),
		.rf_rsv_valid(rf_rsv_valid)
	);

	ExecuteStage execute_stage (
		.clk(clk),
		.rst(rst),
		.decode_do_bse(decode_to_exec_do_bse),
		.decode_do_bze(decode_to_exec_do_bze),
		.decode_do_wse(decode_to_exec_do_wse),
		.decode_do_wze(decode_to_exec_do_wze),
		.decode_exec_unit(decode_to_exec_exec_unit),
		.decode_fetch_av(decode_to_exec_fetch_av),
		.decode_inst_len(decode_to_exec_inst_len),
		.decode_is_load(decode_to_exec_is_load),
		.decode_is_store(decode_to_exec_is_store),
		.decode_mem_access_len(decode_to_exec_mem_access_len),
		.decode_op_a(decode_to_exec_op_a),
		.decode_op_b(decode_to_exec_op_b),
		.decode_op_imm(decode_to_exec_op_imm),
		.decode_opcode(decode_to_exec_opcode),
		.decode_ready(decode_to_exec_ready),
		.decode_result_reg_addr(decode_to_exec_result_reg_addr),
		.decode_result_reg_addr_valid(decode_to_exec_result_reg_addr_valid),
		.decode_valid(decode_to_exec_valid),

		.mem_do_bse(exec_to_mem_do_bse),
		.mem_do_bze(exec_to_mem_do_bze),
		.mem_do_wse(exec_to_mem_do_wse),
		.mem_do_wze(exec_to_mem_do_wze),
		.mem_is_load(exec_to_mem_is_load),
		.mem_is_store(exec_to_mem_is_store),
		.mem_mem_access_len(exec_to_mem_mem_access_len),
		.mem_mem_addr(exec_to_mem_mem_addr),
		.mem_ready(exec_to_mem_ready),
		.mem_result(exec_to_mem_result),
		.mem_result_reg_addr(exec_to_mem_result_reg_addr),
		.mem_result_reg_addr_valid(exec_to_mem_result_reg_addr_valid),
		.mem_valid(exec_to_mem_valid),

		.mem_base(mem_base),
		.mem_limit(mem_limit),
		.spc_in(spc),
		.spc_out(execute_stage_spc_out),
		.tpc_in(tpc),
		.tpc_out(execute_stage_tpc_out),
		.task_mode_in(task_mode),
		.task_mode_out(execute_stage_task_mode_out),
		.ecause_in(ecause),
		.ecause_out(execute_stage_ecause_out),
		.rcause_in(rcause),
		.rcause_out(execute_stage_rcause_out),
		.do_branch(do_branch),
		.interrupt(interrupt)
	);

	MemoryStage memory_stage (
		.clk(clk),
		.rst(rst),
		.exec_do_bse(exec_to_mem_do_bse),
		.exec_do_bze(exec_to_mem_do_bze),
		.exec_do_wse(exec_to_mem_do_wse),
		.exec_do_wze(exec_to_mem_do_wze),
		.exec_is_load(exec_to_mem_is_load),
		.exec_is_store(exec_to_mem_is_store),
		.exec_mem_access_len(exec_to_mem_mem_access_len),
		.exec_mem_addr(exec_to_mem_mem_addr),
		.exec_ready(exec_to_mem_ready),
		.exec_result(exec_to_mem_result),
		.exec_result_reg_addr(exec_to_mem_result_reg_addr),
		.exec_result_reg_addr_valid(exec_to_mem_result_reg_addr_valid),
		.exec_valid(exec_to_mem_valid),

		.w_result_reg_addr(wb_result_reg_addr),
		.w_result(wb_result),
		.w_request(wb_request),
		.bus_if_addr(mem_to_bus_addr),
		.bus_if_burst_len(mem_to_bus_burst_len),
		.bus_if_byte_en(mem_to_bus_byte_en),
		.bus_if_data_in(mem_to_bus_data_in),
		.bus_if_data_out(mem_to_bus_data_out),
		.bus_if_last(mem_to_bus_last),
		.bus_if_read_not_write(mem_to_bus_read_not_write),
		.bus_if_request(mem_to_bus_request),
		.bus_if_response(mem_to_bus_response),

		.csr_if_addr(csr_if_addr),
		.csr_if_rd_data(csr_if_rd_data),
		.csr_if_read_not_write(csr_if_read_not_write),
		.csr_if_request(csr_if_request),
		.csr_if_response(csr_if_response),
		.csr_if_wr_data(csr_if_wr_data)
	);

	RegFile reg_file (
		.clk(clk),
		.rst(rst),
		.request(rf_request),
		.response(rf_response),
		.read1_addr(rf_read1_addr),
		.read1_data(rf_read1_data),
		.read1_valid(rf_read1_valid),
		.read2_addr(rf_read2_addr),
		.read2_data(rf_read2_data),
		.read2_valid(rf_read2_valid),
		.rsv_addr(rf_rsv_addr),
		.rsv_valid(rf_rsv_valid),
		.write_data(wb_result),
		.write_addr(wb_result_reg_addr),
		.write_request(wb_request)
	);

endmodule


////////////////////////////////////////////////////////////////////////////////
// RegFile
////////////////////////////////////////////////////////////////////////////////
module RegFile (
	input logic clk,
	input logic rst,
	input logic request,
	output logic response,
	input logic [3:0] read1_addr,
	output logic [31:0] read1_data,
	input logic read1_valid,
	input logic [3:0] read2_addr,
	output logic [31:0] read2_data,
	input logic read2_valid,
	input logic [3:0] rsv_addr,
	input logic rsv_valid,
	input logic [31:0] write_data,
	input logic [3:0] write_addr,
	input logic write_request
);

	logic write_request_d;
	logic write_addr_d;
	logic [3:0] u2_output_port;
	logic [3:0] u3_output_port;
	logic [3:0] read1_addr_d;
	logic [3:0] read2_addr_d;
	logic [14:0] clear_mask;
	logic [14:0] set_mask;
	logic [14:0] rsv_board;
	logic read1_response;
	logic read2_response;
	logic rsv_response;
	logic decode_response;
	logic [31:0] mem1_port2_data_out;
	logic [31:0] mem2_port2_data_out;

	always_ff @(posedge clk) write_request_d <= rst ? 1'h0 : write_request;
	always_ff @(posedge clk) write_addr_d <= rst ? 1'h0 : write_request;
	always_ff @(posedge clk) u2_output_port <= rst ? 4'h0 : write_addr;
	always_ff @(posedge clk) u3_output_port <= rst ? 4'h0 : write_addr;
	always_ff @(posedge clk) read1_addr_d <= rst ? 4'h0 : read1_addr;
	always_ff @(posedge clk) read2_addr_d <= rst ? 4'h0 : read2_addr;
	assign read1_data = (write_addr == read1_addr_d & write_request_d) ? write_data : mem1_port2_data_out;
	assign read2_data = (write_addr == read2_addr_d & write_request_d) ? write_data : mem2_port2_data_out;
	assign clear_mask = write_request ? 1'h1 << write_addr : 1'h0;
	assign set_mask = (rsv_valid & decode_response & request) ? 1'h1 << rsv_addr : 1'h0;
	always_ff @(posedge clk) rsv_board <= rst ? 15'h0 : rsv_board &  ~ clear_mask | set_mask;
	assign read1_response = request & ( ~ read1_valid | (rsv_board & 1'h1 << read1_addr) == 1'h0 | write_addr == read1_addr & write_request);
	assign read2_response = request & ( ~ read2_valid | (rsv_board & 1'h1 << read2_addr) == 1'h0 | write_addr == read2_addr & write_request);
	assign rsv_response = request & ( ~ rsv_valid | (rsv_board & 1'h1 << rsv_addr) == 1'h0 | write_addr == rsv_addr & write_request);
	assign decode_response = read1_response & read2_response & rsv_response;

	SimpleDualPortMemory mem1 (
		.port1_addr(u2_output_port),
		.port1_clk(clk),
		.port2_addr(read1_addr),
		.port2_clk(clk),
		.port1_write_en(write_addr_d),
		.port1_data_in(write_data),
		.port2_data_out(mem1_port2_data_out)
	);

	SimpleDualPortMemory mem2 (
		.port1_addr(u3_output_port),
		.port1_clk(clk),
		.port2_addr(read2_addr),
		.port2_clk(clk),
		.port1_write_en(write_addr_d),
		.port1_data_in(write_data),
		.port2_data_out(mem2_port2_data_out)
	);

	assign response = decode_response;
endmodule


////////////////////////////////////////////////////////////////////////////////
// SimpleDualPortMemory
////////////////////////////////////////////////////////////////////////////////
module SimpleDualPortMemory (
	input logic [3:0] port1_addr,
	input logic port1_clk,
	input logic [3:0] port2_addr,
	input logic port2_clk,
	input logic port1_write_en,
	input logic [31:0] port1_data_in,
	output logic [31:0] port2_data_out
);

	reg [31:0] mem[0:15];

	always @(posedge port1_clk) begin
		if (port1_write_en) begin
			mem[port1_addr] <= port1_data_in;
		end
	end

	logic [3:0] port2_addr_reg;
	always @(posedge port1_clk) begin
		port2_addr_reg <= port2_addr;
	end
	assign port2_data_out = mem[port2_addr_reg];


endmodule


////////////////////////////////////////////////////////////////////////////////
// MemoryStage
////////////////////////////////////////////////////////////////////////////////
module MemoryStage (
	input logic clk,
	input logic rst,
	input logic exec_do_bse,
	input logic exec_do_bze,
	input logic exec_do_wse,
	input logic exec_do_wze,
	input logic exec_is_load,
	input logic exec_is_store,
	input logic [1:0] exec_mem_access_len,
	input logic [31:0] exec_mem_addr,
	output logic exec_ready,
	input logic [31:0] exec_result,
	input logic [3:0] exec_result_reg_addr,
	input logic exec_result_reg_addr_valid,
	input logic exec_valid,

	output logic [3:0] w_result_reg_addr,
	output logic [31:0] w_result,
	output logic w_request,
	output logic [30:0] bus_if_addr,
	output logic [1:0] bus_if_burst_len,
	output logic [1:0] bus_if_byte_en,
	output logic [15:0] bus_if_data_in,
	input logic [15:0] bus_if_data_out,
	input logic bus_if_last,
	output logic bus_if_read_not_write,
	output logic bus_if_request,
	input logic bus_if_response,

	output logic [9:0] csr_if_addr,
	input logic [31:0] csr_if_rd_data,
	output logic csr_if_read_not_write,
	output logic csr_if_request,
	input logic csr_if_response,
	output logic [31:0] csr_if_wr_data
);

	logic csr_response;
	logic is_csr;
	logic u5_output_port;
	logic u8_output_port;
	logic u11_output_port;
	logic u15_output_port;
	logic u19_output_port;
	logic u20_output_port;
	logic u21_output_port;
	logic u22_output_port;
	logic u24_output_port;
	logic u25_output_port;
	logic u26_output_port;
	logic u29_output_port;
	logic u32_output_port;
	logic u36_output_port;
	logic u40_output_port;
	logic u41_output_port;
	logic u43_output_port;
	logic u47_output_port;
	logic u51_output_port;
	logic u56_output_port;
	logic u61_output_port;
	logic u62_output_port;
	logic u64_output_port;
	logic u68_output_port;
	logic u72_output_port;
	logic u77_output_port;
	logic u82_output_port;
	logic exec_ready_1;
	logic accept_next;
	logic lsb;
	logic [15:0] u105_output_port;
	logic [15:0] data_h;
	logic first;
	logic first_and_last;
	logic [15:0] u128_output_port;
	logic [15:0] data_l;
	logic [31:0] full_result;
	logic do_bse;
	logic do_wse;
	logic do_bze;
	logic do_wze;
	logic [3:0] u190_output_port;
	logic csr_request;
	logic [3:0] state;
	logic [3:0] next_state;

	assign is_csr = exec_mem_addr[31:28] == 4'hc;
	assign exec_ready_1 = state == `MemoryStates__idle | state == `MemoryStates__read_2 | state == (`MemoryStates__csr_read & csr_if_response) | state == (`MemoryStates__csr_write & csr_if_response);
	assign accept_next = exec_valid & exec_ready_1;
	always_ff @(posedge clk) u105_output_port <= rst ? 16'h0 : state == `MemoryStates__idle ? exec_result[31:16] : u105_output_port;
	assign data_h = state == `MemoryStates__read_2 ? bus_if_data_out : (csr_request & csr_if_response & csr_if_read_not_write) ? csr_if_rd_data[31:16] : u105_output_port;
	always_ff @(posedge clk) first <= rst ? 1'h0 : (accept_next & (exec_is_store | exec_is_load)) ? 1'h1 : bus_if_response ? 1'h0 : first;
	always_ff @(posedge clk) first_and_last <= rst ? 1'h0 : first & bus_if_last;
	always_ff @(posedge clk) lsb <= rst ? 1'h0 : accept_next ? exec_mem_addr[0] : lsb;
	always_ff @(posedge clk) u128_output_port <= rst ? 16'h0 : state == `MemoryStates__idle ? exec_result[15:0] : state == `MemoryStates__read_1 ? bus_if_data_out : data_l;
	assign data_l = (csr_request & csr_if_response & csr_if_read_not_write) ? csr_if_rd_data[15:0] : first_and_last ? lsb ? ({bus_if_data_out[15:8], bus_if_data_out[15:8]}) : bus_if_data_out : u128_output_port;
	always_ff @(posedge clk) do_bse <= rst ? 1'h0 : accept_next ? exec_do_bse : do_bse;
	always_ff @(posedge clk) do_wse <= rst ? 1'h0 : accept_next ? exec_do_wse : do_wse;
	always_ff @(posedge clk) do_bze <= rst ? 1'h0 : accept_next ? exec_do_bze : do_bze;
	always_ff @(posedge clk) do_wze <= rst ? 1'h0 : accept_next ? exec_do_wze : do_wze;
	assign full_result = {data_h, data_l};
	assign w_result = do_bse ? ({data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7:0]}) : 32'b0 | do_wse ? ({data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15:0]}) : 32'b0 | do_bze ? data_l[7:0] : 32'b0 | do_wze ? data_l[15:0] : 32'b0 | full_result;
	always_ff @(posedge clk) u190_output_port <= rst ? 4'h0 : state == `MemoryStates__idle ? exec_result_reg_addr : u190_output_port;
	assign w_result_reg_addr = accept_next ? exec_result_reg_addr : u190_output_port;
	assign w_request =  ~ (exec_is_load | exec_is_store) & accept_next & exec_result_reg_addr_valid | bus_if_response & bus_if_last & (state == `MemoryStates__read_1 | state == `MemoryStates__read_2);
	assign bus_if_request = (accept_next | first) & (exec_is_store | exec_is_load) &  ~ is_csr;
	assign bus_if_burst_len = exec_mem_access_len[1];
	assign bus_if_byte_en = exec_mem_access_len == 1'h0 ? ({exec_mem_addr[0],  ~ exec_mem_addr[0]}) : 2'h3;
	assign bus_if_addr = exec_mem_addr[31:1];
	assign bus_if_data_in = state == `MemoryStates__idle ? ({(exec_mem_addr[0] & exec_mem_access_len == 1'h0) ? exec_result[7:0] : exec_result[15:8], exec_result[7:0]}) : data_h;
	assign csr_request = exec_valid & (exec_is_store | exec_is_load) & is_csr;
	assign csr_if_addr = exec_mem_addr[12:2];
	assign bus_if_read_not_write = exec_is_load;
	assign csr_if_read_not_write = exec_is_load;
	assign csr_if_wr_data = exec_result;

	FSM_4 fsm (
		.clock_port(clk),
		.reset_port(rst),
		.reset_value(`MemoryStates__idle),
		.state(state),
		.next_state(next_state),
		.default_state(`MemoryStates__idle),
		.input_idle_to_idle(u5_output_port),
		.input_idle_to_read_1(u8_output_port),
		.input_idle_to_write(u11_output_port),
		.input_idle_to_csr_read(u15_output_port),
		.input_idle_to_csr_write(u19_output_port),
		.input_write_to_write(u20_output_port),
		.input_write_to_idle(u21_output_port),
		.input_read_1_to_read_1(u22_output_port),
		.input_read_1_to_read_1_1(u24_output_port),
		.input_read_1_to_read_2(u25_output_port),
		.input_read_2_to_idle(u26_output_port),
		.input_read_2_to_read_1(u29_output_port),
		.input_read_2_to_write(u32_output_port),
		.input_read_2_to_csr_read(u36_output_port),
		.input_read_2_to_csr_write(u40_output_port),
		.input_csr_read_to_csr_read(u41_output_port),
		.input_csr_read_to_idle(u43_output_port),
		.input_csr_read_to_read_1(u47_output_port),
		.input_csr_read_to_write(u51_output_port),
		.input_csr_read_to_csr_read_1(u56_output_port),
		.input_csr_read_to_csr_write(u61_output_port),
		.input_csr_write_to_csr_write(u62_output_port),
		.input_csr_write_to_idle(u64_output_port),
		.input_csr_write_to_read_1(u68_output_port),
		.input_csr_write_to_write(u72_output_port),
		.input_csr_write_to_csr_read(u77_output_port),
		.input_csr_write_to_csr_write_1(u82_output_port)
	);

	assign csr_response = csr_if_response;
	assign u5_output_port =  ~ exec_valid;
	assign u8_output_port = exec_valid &  ~ is_csr & exec_is_load;
	assign u11_output_port = exec_valid &  ~ is_csr & exec_is_store;
	assign u15_output_port = exec_valid & is_csr & exec_is_load &  ~ csr_if_response;
	assign u19_output_port = exec_valid & is_csr & exec_is_store &  ~ csr_if_response;
	assign u20_output_port =  ~ bus_if_response;
	assign u21_output_port = bus_if_response & bus_if_last;
	assign u22_output_port =  ~ bus_if_response;
	assign u24_output_port = bus_if_response &  ~ bus_if_last;
	assign u25_output_port = bus_if_response & bus_if_last;
	assign u26_output_port =  ~ exec_valid;
	assign u29_output_port = exec_valid &  ~ is_csr & exec_is_load;
	assign u32_output_port = exec_valid &  ~ is_csr & exec_is_store;
	assign u36_output_port = exec_valid & is_csr & exec_is_load &  ~ csr_if_response;
	assign u40_output_port = exec_valid & is_csr & exec_is_store &  ~ csr_if_response;
	assign u41_output_port =  ~ csr_if_response;
	assign u43_output_port = csr_if_response &  ~ exec_valid;
	assign u47_output_port = csr_if_response & exec_valid &  ~ is_csr & exec_is_load;
	assign u51_output_port = csr_if_response & exec_valid &  ~ is_csr & exec_is_store;
	assign u56_output_port = csr_if_response & exec_valid & is_csr & exec_is_load &  ~ csr_if_response;
	assign u61_output_port = csr_if_response & exec_valid & is_csr & exec_is_store &  ~ csr_if_response;
	assign u62_output_port =  ~ csr_if_response;
	assign u64_output_port = csr_if_response &  ~ exec_valid;
	assign u68_output_port = csr_if_response & exec_valid &  ~ is_csr & exec_is_load;
	assign u72_output_port = csr_if_response & exec_valid &  ~ is_csr & exec_is_store;
	assign u77_output_port = csr_if_response & exec_valid & is_csr & exec_is_load &  ~ csr_if_response;
	assign u82_output_port = csr_if_response & exec_valid & is_csr & exec_is_store &  ~ csr_if_response;
	assign exec_ready = exec_ready_1;
	assign csr_if_request = csr_request;
endmodule


////////////////////////////////////////////////////////////////////////////////
// ExecuteStage
////////////////////////////////////////////////////////////////////////////////
module ExecuteStage (
	input logic clk,
	input logic rst,
	input logic decode_do_bse,
	input logic decode_do_bze,
	input logic decode_do_wse,
	input logic decode_do_wze,
	input logic [2:0] decode_exec_unit,
	input logic decode_fetch_av,
	input logic [1:0] decode_inst_len,
	input logic decode_is_load,
	input logic decode_is_store,
	input logic [1:0] decode_mem_access_len,
	input logic [31:0] decode_op_a,
	input logic [31:0] decode_op_b,
	input logic [31:0] decode_op_imm,
	input logic [2:0] decode_opcode,
	output logic decode_ready,
	input logic [3:0] decode_result_reg_addr,
	input logic decode_result_reg_addr_valid,
	input logic decode_valid,

	output logic mem_do_bse,
	output logic mem_do_bze,
	output logic mem_do_wse,
	output logic mem_do_wze,
	output logic mem_is_load,
	output logic mem_is_store,
	output logic [1:0] mem_mem_access_len,
	output logic [31:0] mem_mem_addr,
	input logic mem_ready,
	output logic [31:0] mem_result,
	output logic [3:0] mem_result_reg_addr,
	output logic mem_result_reg_addr_valid,
	output logic mem_valid,

	input logic [21:0] mem_base,
	input logic [21:0] mem_limit,
	input logic [30:0] spc_in,
	output logic [30:0] spc_out,
	input logic [30:0] tpc_in,
	output logic [30:0] tpc_out,
	input logic task_mode_in,
	output logic task_mode_out,
	input logic [11:0] ecause_in,
	output logic [11:0] ecause_out,
	input logic [11:0] rcause_in,
	output logic [11:0] rcause_out,
	output logic do_branch,
	input logic interrupt
);

	logic [30:0] pc;
	logic [32:0] u3_output_port;
	logic [32:0] u6_output_port;
	logic signed [32:0] u9_output_port;
	logic [33:0] u15_output_port;
	logic [33:0] u20_output_port;
	logic [31:0] u21_output_port;
	logic signed [95:0] u40_output_port;
	logic [31:0] logic_result;
	logic cbranch_result;
	logic bbranch_result;
	logic mem_av;
	logic mem_unaligned;
	logic [32:0] u131_output_port;
	logic [31:0] u144_output_port;
	logic [31:0] u147_output_port;
	logic [30:0] next_inst_addr;
	logic [63:0] mult_result_large;
	logic [31:0] exec_result;
	logic is_branch;
	logic is_exception;
	logic [31:0] u269_output_port;
	logic [11:0] hwi_mask;
	logic u80_output_port;
	logic u83_output_port;
	logic reg_en;
	logic [31:0] u92_output_port;

	assign pc = task_mode_in ? tpc_in : spc_in;
	assign mult_result_large = decode_op_a * decode_op_b;
	assign logic_result = decode_opcode == `op__b_sub_a ? decode_op_a | decode_op_b : 32'b0 | decode_opcode == `op__add ? decode_op_a & decode_op_b : 32'b0 | decode_opcode == `op__a_sub_b ?  ~ decode_op_a & decode_op_b : 32'b0 | decode_opcode == `op__addr ? decode_op_a ^ decode_op_b : 32'b0 ;
	assign mem_av = decode_exec_unit == `exec__adder & decode_opcode == `op__addr & u92_output_port[31:10] > mem_limit;
	assign cbranch_result = decode_opcode == `op__a_sub_b ? decode_op_a == decode_op_b : 1'b0 | decode_opcode == `op__b_sub_a ? decode_op_a != decode_op_b : 1'b0 | decode_opcode == `op__addr ? decode_op_a < decode_op_b : 1'b0 | decode_opcode == `op__pc_add ? decode_op_a >= decode_op_b : 1'b0 | decode_opcode == `op__cb_lt ? decode_op_a < decode_op_b : 1'b0 | decode_opcode == `op__cb_ge ? decode_op_a >= decode_op_b : 1'b0 ;
	assign bbranch_result = decode_opcode == `op__add ? u80_output_port : 1'b0 | decode_opcode == `op__a_sub_b ? u83_output_port : 1'b0 ;
	assign mem_unaligned = decode_exec_unit == `exec__adder & decode_opcode == `op__addr & (decode_mem_access_len == 0 ? 1'h0 : 1'b0 | decode_mem_access_len == 1 ? u92_output_port[0] : 1'b0 | decode_mem_access_len == 2 ? u92_output_port[0] | u92_output_port[1] : 1'b0 | decode_mem_access_len == 3 ? 1'h1 : 1'b0);
	assign is_branch = decode_exec_unit == `exec__adder ? mem_av : 1'b0 | decode_exec_unit == `exec__shift ? 1'h0 : 1'b0 | decode_exec_unit == `exec__mult ? 1'h0 : 1'b0 | decode_exec_unit == `exec__bitwise ? 1'h0 : 1'b0 | decode_exec_unit == `exec__cbranch ? cbranch_result : 1'b0 | decode_exec_unit == `exec__bbranch ? bbranch_result : 1'b0 | decode_exec_unit == `exec__misc ? decode_opcode == `op__add ? 1'h1 : 1'b0 | decode_opcode == `op__a_sub_b ? 1'h1 : 1'b0 | decode_opcode == `op__b_sub_a ? 1'h0 : 1'b0 | decode_opcode == `op__addr ? 1'h0 : 1'b0 | decode_opcode == `op__pc_add ? 1'h1 : 1'b0 | decode_opcode == `op__cb_lt ? task_mode_in : 1'b0  : 1'b0 ;
	assign do_branch = is_branch & reg_en;
	always_ff @(posedge clk) mem_mem_access_len <= rst ? 2'h0 : reg_en ? decode_mem_access_len : mem_mem_access_len;
	assign exec_result = decode_exec_unit == `exec__adder ? u21_output_port[31:0] : 32'b0 | decode_exec_unit == `exec__shift ? u40_output_port[31:0] : 32'b0 | decode_exec_unit == `exec__mult ? mult_result_large[31:0] : 32'b0 | decode_exec_unit == `exec__bitwise ? logic_result : 32'b0 | decode_exec_unit == `exec__misc ? decode_opcode == `op__add ? $signed(1'bX) : 31'b0 | decode_opcode == `op__a_sub_b ? $signed(1'bX) : 31'b0 | decode_opcode == `op__b_sub_a ? pc : 31'b0 | decode_opcode == `op__addr ? tpc_in : 31'b0 | decode_opcode == `op__pc_add ? $signed(1'bX) : 31'b0 | decode_opcode == `op__cb_lt ? $signed(1'bX) : 31'b0  : 32'b0 ;
	always_ff @(posedge clk) mem_result <= rst ? 32'h0 : reg_en ? exec_result : mem_result;
	always_ff @(posedge clk) mem_result_reg_addr <= rst ? 4'h0 : reg_en ? decode_result_reg_addr : mem_result_reg_addr;
	always_ff @(posedge clk) mem_mem_addr <= rst ? 32'h0 : reg_en ? u21_output_port[31:0] : mem_mem_addr;
	always_ff @(posedge clk) mem_is_load <= rst ? 1'h0 : reg_en ? decode_is_load : mem_is_load;
	always_ff @(posedge clk) mem_is_store <= rst ? 1'h0 : reg_en ? decode_is_store : mem_is_store;
	always_ff @(posedge clk) mem_do_bse <= rst ? 1'h0 : reg_en ? decode_do_bse : mem_do_bse;
	always_ff @(posedge clk) mem_do_wse <= rst ? 1'h0 : reg_en ? decode_do_wse : mem_do_wse;
	always_ff @(posedge clk) mem_do_bze <= rst ? 1'h0 : reg_en ? decode_do_bze : mem_do_bze;
	always_ff @(posedge clk) mem_do_wze <= rst ? 1'h0 : reg_en ? decode_do_wze : mem_do_wze;
	assign is_exception = decode_fetch_av | decode_exec_unit == `exec__adder ? mem_av | mem_unaligned : 1'b0 | decode_exec_unit == `exec__shift ? 1'h0 : 1'b0 | decode_exec_unit == `exec__mult ? 1'h0 : 1'b0 | decode_exec_unit == `exec__bitwise ? 1'h0 : 1'b0 | decode_exec_unit == `exec__cbranch ? 1'h0 : 1'b0 | decode_exec_unit == `exec__bbranch ? 1'h0 : 1'b0 | decode_exec_unit == `exec__misc ? decode_opcode == `op__add ? 1'h1 : 1'b0 | decode_opcode == `op__a_sub_b ? 1'h0 : 1'b0 | decode_opcode == `op__b_sub_a ? 1'h0 : 1'b0 | decode_opcode == `op__addr ? 1'h0 : 1'b0 | decode_opcode == `op__pc_add ? 1'h0 : 1'b0 | decode_opcode == `op__cb_lt ? 1'h0 : 1'b0  : 1'b0 ;
	assign next_inst_addr = decode_exec_unit == `exec__cbranch | decode_exec_unit == `exec__bbranch ? u131_output_port[30:0] : 31'b0 | decode_exec_unit == `exec__misc & decode_opcode == `op__pc_add ? decode_op_imm[31:1] : 31'b0 | decode_exec_unit == `exec__misc & decode_opcode == `op__cb_lt & task_mode_in ? decode_op_imm[31:1] : 31'b0 | task_mode_in ? u147_output_port[30:0] : u144_output_port[30:0];
	assign spc_out = reg_en ? task_mode_in ? spc_in : is_exception ? 1'h0 : next_inst_addr : spc_in;
	assign tpc_out = reg_en ? task_mode_in ? is_exception ? tpc_in : next_inst_addr : (decode_exec_unit == `exec__misc & decode_opcode == `op__cb_lt) ? decode_op_imm[31:1] : tpc_in : tpc_in;
	assign task_mode_out = reg_en ? task_mode_in ?  ~ is_exception :  ~ (decode_exec_unit == `exec__misc & decode_opcode == `op__a_sub_b) : task_mode_in;
	assign hwi_mask = interrupt ? 12'h800 : 1'h0;
	assign ecause_out = reg_en ? is_exception ? ecause_in | 1'h0 | hwi_mask : ecause_in | hwi_mask : ecause_in;
	assign rcause_out = reg_en ? (is_exception &  ~ task_mode_in) ? rcause_in | 1'h0 : rcause_in : rcause_in;

	DecoratorModule_6 u80 (
		.output_port(u80_output_port),
		.word(decode_op_a),
		.bit_code(decode_op_b)
	);

	DecoratorModule_7 u83 (
		.output_port(u83_output_port),
		.word(decode_op_b),
		.bit_code(decode_op_a)
	);

	ForwardBufLogic_2 handshake_fsm (
		.clock_port(clk),
		.reset_port(rst),
		.input_valid(decode_valid),
		.input_ready(decode_ready),
		.output_valid(mem_valid),
		.output_ready(mem_ready),
		.out_reg_en(reg_en)
	);

	assign mem_result_reg_addr_valid = 1'hx;
	assign u3_output_port = decode_op_a + decode_op_b;
	assign u6_output_port = decode_op_a + decode_op_b;
	assign u9_output_port = decode_op_b - decode_op_a;
	assign u15_output_port = decode_op_b + decode_op_imm + (mem_base << 4'ha);
	assign u20_output_port = pc + ({decode_op_a, 1'h0});
	assign u21_output_port = decode_opcode == `op__add ? u3_output_port[31:0] : 32'b0 | decode_opcode == `op__a_sub_b ? u6_output_port[31:0] : 32'b0 | decode_opcode == `op__b_sub_a ? u9_output_port[31:0] : 32'b0 | decode_opcode == `op__addr ? u15_output_port[31:0] : 32'b0 | decode_opcode == `op__pc_add ? u20_output_port[31:0] : 32'b0 ;
	assign u40_output_port = decode_opcode == `op__add ? decode_op_a << decode_op_b[5:0] : 96'b0 | decode_opcode == `op__a_sub_b ? decode_op_a >> decode_op_b[5:0] : 96'b0 | decode_opcode == `op__b_sub_a ? decode_op_a >>> decode_op_b[5:0] : 96'b0 ;
	assign u131_output_port = pc + ({decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[15:1], 1'h0});
	assign u144_output_port = spc_in + decode_inst_len + 1'h1;
	assign u147_output_port = tpc_in + decode_inst_len + 1'h1;
	assign u269_output_port = decode_op_a & 3'h7;
	assign u92_output_port = u21_output_port[31:0];
endmodule


////////////////////////////////////////////////////////////////////////////////
// DecodeStage
////////////////////////////////////////////////////////////////////////////////
module DecodeStage (
	input logic clk,
	input logic rst,
	input logic fetch_av,
	input logic [15:0] fetch_inst_0,
	input logic [15:0] fetch_inst_1,
	input logic [15:0] fetch_inst_2,
	input logic [1:0] fetch_inst_len,
	output logic fetch_ready,
	input logic fetch_valid,

	output logic exec_do_bse,
	output logic exec_do_bze,
	output logic exec_do_wse,
	output logic exec_do_wze,
	output logic [2:0] exec_exec_unit,
	output logic exec_fetch_av,
	output logic [1:0] exec_inst_len,
	output logic exec_is_load,
	output logic exec_is_store,
	output logic [1:0] exec_mem_access_len,
	output logic [31:0] exec_op_a,
	output logic [31:0] exec_op_b,
	output logic [31:0] exec_op_imm,
	output logic [2:0] exec_opcode,
	input logic exec_ready,
	output logic [3:0] exec_result_reg_addr,
	output logic exec_result_reg_addr_valid,
	output logic exec_valid,

	output logic rf_request,
	input logic rf_response,
	output logic [3:0] rf_read1_addr,
	input logic [31:0] rf_read1_data,
	output logic rf_read1_valid,
	output logic [3:0] rf_read2_addr,
	input logic [31:0] rf_read2_data,
	output logic rf_read2_valid,
	output logic [3:0] rf_rsv_addr,
	output logic rf_rsv_valid
);

	logic exec_out_fetch_av;
	logic [1:0] exec_out_inst_len;
	logic [31:0] field_e;
	logic field_a_is_f;
	logic field_b_is_f;
	logic field_c_is_f;
	logic field_d_is_f;
	logic [8:0] tiny_ofs;
	logic [4:0] u36_output_port;
	logic [31:0] ones_field_a;
	logic mask_for_swi;
	logic mask_for_stm;
	logic mask_for_woi;
	logic mask_for_fence;
	logic mask_for_pc_eq_rd;
	logic mask_for_tpc_eq_rd;
	logic mask_for_rd_eq_pc;
	logic mask_for_rd_eq_tpc;
	logic mask_for_rd_eq_tiny_field_a;
	logic mask_for_rd_eq_pc_plus_field_atimes2;
	logic mask_for_rd_eq_minus_ra;
	logic mask_for_rd_eq_notra;
	logic mask_for_rd_eq_bse_ra;
	logic mask_for_rd_eq_wse_ra;
	logic mask_for_rd_eq_ra_xor_rb;
	logic mask_for_rd_eq_ra_or_rb;
	logic mask_for_rd_eq_ra_and_rb;
	logic mask_for_rd_eq_ra_plus_rb;
	logic mask_for_rd_eq_ra_minus_rb;
	logic mask_for_rd_eq_ra_lsl_rb;
	logic mask_for_rd_eq_ra_lsr_rb;
	logic mask_for_rd_eq_ra_asr_rb;
	logic mask_for_rd_eq_ra_times_rb;
	logic mask_for_rd_eq_notra_and_rb;
	logic mask_for_rd_eq_tiny_rb_plus_field_a;
	logic mask_for_rd_eq_value;
	logic mask_for_pc_eq_value;
	logic mask_for_tpc_eq_value;
	logic mask_for_rd_eq_field_e_xor_rb;
	logic mask_for_rd_eq_field_e_or_rb;
	logic mask_for_rd_eq_field_e_and_rb;
	logic mask_for_rd_eq_field_e_plus_rb;
	logic mask_for_rd_eq_field_e_minus_rb;
	logic mask_for_rd_eq_field_e_lsl_rb;
	logic mask_for_rd_eq_field_e_lsr_rb;
	logic mask_for_rd_eq_field_e_asr_rb;
	logic mask_for_rd_eq_field_e_times_rb;
	logic mask_for_rd_eq_short_value;
	logic mask_for_pc_eq_short_value;
	logic mask_for_tpc_eq_short_value;
	logic mask_for_rd_eq_field_e_xor_ra;
	logic mask_for_rd_eq_field_e_or_ra;
	logic mask_for_rd_eq_field_e_and_ra;
	logic mask_for_rd_eq_field_e_plus_ra;
	logic mask_for_rd_eq_field_e_minus_ra;
	logic mask_for_rd_eq_field_e_lsl_ra;
	logic mask_for_rd_eq_field_e_lsr_ra;
	logic mask_for_rd_eq_field_e_asr_ra;
	logic mask_for_rd_eq_field_e_times_ra;
	logic mask_for_if_ra_eq_0;
	logic mask_for_if_ra_ne_0;
	logic mask_for_if_ra_lt_0;
	logic mask_for_if_ra_ge_0;
	logic mask_for_if_ra_gt_0;
	logic mask_for_if_ra_le_0;
	logic mask_for_if_rb_eq_ra;
	logic mask_for_if_rb_ne_ra;
	logic mask_for_if_signed_rb_lt_ra;
	logic mask_for_if_signed_rb_ge_ra;
	logic mask_for_if_rb_lt_ra;
	logic mask_for_if_rb_ge_ra;
	logic mask_for_if_ra_bit__eq_1;
	logic mask_for_if_rb_bit__eq_0;
	logic mask_for_mem_raplustiny_ofstimes4_eq_rd;
	logic mask_for_rd_eq_mem_raplustiny_ofstimes4;
	logic mask_for_rd_eq_mem8_ra;
	logic mask_for_rd_eq_mem16_ra;
	logic mask_for_rd_eq_mem32_ra;
	logic mask_for_rd_eq_memll32_ra;
	logic mask_for_mem8_ra_eq_rd;
	logic mask_for_mem16_ra_eq_rd;
	logic mask_for_mem32_ra_eq_rd;
	logic mask_for_memsr32_ra_eq_rd;
	logic mask_for_rd_eq_smem8_ra;
	logic mask_for_rd_eq_smem16_ra;
	logic mask_for_rd_eq_mem8_raplusfield_e;
	logic mask_for_rd_eq_mem16_raplusfield_e;
	logic mask_for_rd_eq_mem32_raplusfield_e;
	logic mask_for_rd_eq_memll32_raplusfield_e;
	logic mask_for_mem8_raplusfield_e_eq_rd;
	logic mask_for_mem16_raplusfield_e_eq_rd;
	logic mask_for_mem32_raplusfield_e_eq_rd;
	logic mask_for_memsr32_raplusfield_e_eq_rd;
	logic mask_for_rd_eq_smem8_raplusfield_e;
	logic mask_for_rd_eq_smem16_raplusfield_e;
	logic mask_for_rd_eq_mem8_field_e;
	logic mask_for_rd_eq_mem16_field_e;
	logic mask_for_rd_eq_mem32_field_e;
	logic mask_for_rd_eq_memll32_field_e;
	logic mask_for_mem8_field_e_eq_rd;
	logic mask_for_mem16_field_e_eq_rd;
	logic mask_for_mem32_field_e_eq_rd;
	logic mask_for_memsr32_field_e_eq_rd;
	logic mask_for_rd_eq_smem8_field_e;
	logic mask_for_rd_eq_smem16_field_e;
	logic mask_for_sii;
	logic expr;
	logic mask_expr;
	logic [2:0] exec_out_exec_unit;
	logic [2:0] exec_unit;
	logic [2:0] exec_out_opcode;
	logic [2:0] op_code;
	logic [3:0] rd1_addr;
	logic [3:0] rd2_addr;
	logic [3:0] res_addr;
	logic [31:0] exec_out_op_a;
	logic [31:0] op_a;
	logic [31:0] exec_out_op_b;
	logic [31:0] op_b;
	logic [31:0] exec_out_op_imm;
	logic [31:0] op_imm;
	logic [1:0] mem_len;
	logic exec_out_is_load;
	logic is_ld;
	logic exec_out_is_store;
	logic is_st;
	logic exec_out_do_bse;
	logic bse;
	logic exec_out_do_wse;
	logic wse;
	logic exec_out_do_bze;
	logic bze;
	logic exec_out_do_wze;
	logic wze;
	logic read1_needed;
	logic read2_needed;
	logic exec_out_result_reg_addr_valid;
	logic rsv_needed;
	logic exec_out_valid;
	logic [3:0] u22_output_port;
	logic exec_out_ready;
	logic [3:0] field_a_plus_one;
	logic [1:0] exec_out_mem_access_len;
	logic [1:0] mem_len_1;
	logic [3:0] exec_out_result_reg_addr;

	assign field_a_plus_one = u36_output_port[3:0];
	assign field_d_is_f = fetch_inst_0[15:12] == 4'hf;
	assign field_a_is_f = fetch_inst_0[3:0] == 4'hf;
	assign field_b_is_f = fetch_inst_0[7:4] == 4'hf;
	assign field_c_is_f = fetch_inst_0[11:8] == 4'hf;
	assign mask_for_swi = 1'h1 & fetch_inst_0[15:12] < 4'h8 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h0;
	assign mask_for_stm = 1'h1 & fetch_inst_0[15:12] == 4'h8 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h0;
	assign mask_for_woi = 1'h1 & fetch_inst_0[15:12] == 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h0;
	assign mask_for_fence = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h1;
	assign mask_for_pc_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 2'h2;
	assign mask_for_tpc_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 2'h3;
	assign mask_for_rd_eq_pc = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 3'h4;
	assign mask_for_rd_eq_tpc = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 3'h5;
	assign mask_for_rd_eq_tiny_field_a = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f;
	assign mask_for_rd_eq_pc_plus_field_atimes2 = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f;
	assign mask_for_rd_eq_minus_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f;
	assign mask_for_rd_eq_notra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f;
	assign mask_for_rd_eq_bse_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f;
	assign mask_for_rd_eq_wse_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h6 &  ~ field_a_is_f;
	assign mask_for_rd_eq_ra_xor_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_ra_or_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_ra_and_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_ra_plus_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_ra_minus_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_ra_lsl_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_ra_lsr_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h7 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_ra_asr_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'h8 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_ra_times_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'h9 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_notra_and_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_tiny_rb_plus_field_a = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_value = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_pc_eq_value = 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_tpc_eq_value = 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_xor_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_or_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_and_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_plus_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_minus_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_lsl_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_lsr_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h7 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_asr_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'h8 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_times_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'h9 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_short_value = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'hf & fetch_inst_0[3:0] == 1'h0;
	assign mask_for_pc_eq_short_value = 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'hf & fetch_inst_0[3:0] == 4'he;
	assign mask_for_tpc_eq_short_value = 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'hf & fetch_inst_0[3:0] == 4'he;
	assign mask_for_rd_eq_field_e_xor_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h1 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_or_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 2'h2 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_and_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 2'h3 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_plus_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h4 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_minus_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h5 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_lsl_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h6 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_lsr_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h7 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_asr_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'h8 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_times_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'h9 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_if_ra_eq_0 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'h8 &  ~ field_a_is_f;
	assign mask_for_if_ra_ne_0 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'h9 &  ~ field_a_is_f;
	assign mask_for_if_ra_lt_0 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'ha &  ~ field_a_is_f;
	assign mask_for_if_ra_ge_0 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'hb &  ~ field_a_is_f;
	assign mask_for_if_ra_gt_0 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'hc &  ~ field_a_is_f;
	assign mask_for_if_ra_le_0 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'hd &  ~ field_a_is_f;
	assign mask_for_if_rb_eq_ra = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'h9 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_rb_ne_ra = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'ha &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_signed_rb_lt_ra = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'hb &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_signed_rb_ge_ra = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'hc &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_rb_lt_ra = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'hd &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_rb_ge_ra = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'he &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_ra_bit__eq_1 = 1'h1 & fetch_inst_0[15:12] == 4'hf &  ~ field_c_is_f & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_if_rb_bit__eq_0 = 1'h1 & fetch_inst_0[15:12] == 4'hf &  ~ field_c_is_f &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_mem_raplustiny_ofstimes4_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hc;
	assign mask_for_rd_eq_mem_raplustiny_ofstimes4 = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hd;
	assign mask_for_rd_eq_mem8_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f;
	assign mask_for_rd_eq_mem16_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f;
	assign mask_for_rd_eq_mem32_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 3'h6 &  ~ field_a_is_f;
	assign mask_for_rd_eq_memll32_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 3'h7 &  ~ field_a_is_f;
	assign mask_for_mem8_ra_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'h8 &  ~ field_a_is_f;
	assign mask_for_mem16_ra_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'h9 &  ~ field_a_is_f;
	assign mask_for_mem32_ra_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'ha &  ~ field_a_is_f;
	assign mask_for_memsr32_ra_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'hb &  ~ field_a_is_f;
	assign mask_for_rd_eq_smem8_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'hc &  ~ field_a_is_f;
	assign mask_for_rd_eq_smem16_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'hd &  ~ field_a_is_f;
	assign mask_for_rd_eq_mem8_raplusfield_e = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f;
	assign mask_for_rd_eq_mem16_raplusfield_e = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f;
	assign mask_for_rd_eq_mem32_raplusfield_e = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 3'h6 &  ~ field_a_is_f;
	assign mask_for_rd_eq_memll32_raplusfield_e = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 3'h7 &  ~ field_a_is_f;
	assign mask_for_mem8_raplusfield_e_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'h8 &  ~ field_a_is_f;
	assign mask_for_mem16_raplusfield_e_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'h9 &  ~ field_a_is_f;
	assign mask_for_mem32_raplusfield_e_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'ha &  ~ field_a_is_f;
	assign mask_for_memsr32_raplusfield_e_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'hb &  ~ field_a_is_f;
	assign mask_for_rd_eq_smem8_raplusfield_e = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'hc &  ~ field_a_is_f;
	assign mask_for_rd_eq_smem16_raplusfield_e = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'hd &  ~ field_a_is_f;
	assign mask_for_rd_eq_mem8_field_e = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 3'h4 & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_mem16_field_e = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 3'h5 & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_mem32_field_e = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 3'h6 & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_memll32_field_e = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 3'h7 & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_mem8_field_e_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'h8 & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_mem16_field_e_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'h9 & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_mem32_field_e_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'ha & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_memsr32_field_e_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'hb & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_smem8_field_e = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'hc & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_smem16_field_e = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'hd & fetch_inst_0[3:0] == 4'hf;
	assign mask_expr = 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf;
	assign ones_field_a = u22_output_port[3] ? ({field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one}) : fetch_inst_0[3:0];
	assign field_e = fetch_inst_len == 2'h2 ? ({fetch_inst_2, fetch_inst_1}) : ({fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1});
	assign tiny_ofs = {fetch_inst_0[7:1], 2'h0};
	assign rd1_addr = mask_for_woi ? fetch_inst_0[3:0] : 4'b0 | mask_for_pc_eq_rd ? fetch_inst_0[15:12] : 4'b0 | mask_for_tpc_eq_rd ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_tpc ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_minus_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_notra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_bse_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_wse_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_xor_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_or_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_and_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_plus_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_minus_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_lsl_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_lsr_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_asr_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_times_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_notra_and_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_xor_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_or_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_and_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_plus_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_minus_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_lsl_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_lsr_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_asr_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_times_ra ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_ra_eq_0 ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_ra_ne_0 ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_ra_lt_0 ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_ra_ge_0 ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_rb_eq_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_rb_ne_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_signed_rb_lt_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_signed_rb_ge_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_rb_lt_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_rb_ge_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_ra_bit__eq_1 ? fetch_inst_0[3:0] : 4'b0 ;
	assign rf_read1_addr = rd1_addr;
	assign read1_needed = mask_for_woi ? 1'h1 : 1'b0 | mask_for_pc_eq_rd ? 1'h1 : 1'b0 | mask_for_tpc_eq_rd ? 1'h1 : 1'b0 | mask_for_rd_eq_tpc ? 1'h1 : 1'b0 | mask_for_rd_eq_minus_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_notra ? 1'h1 : 1'b0 | mask_for_rd_eq_bse_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_wse_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_xor_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_or_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_plus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_minus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsl_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_asr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_times_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_notra_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_xor_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_or_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_and_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_plus_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_minus_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsl_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsr_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_asr_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_times_ra ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? 1'h1 : 1'b0 | mask_for_if_ra_eq_0 ? 1'h1 : 1'b0 | mask_for_if_ra_ne_0 ? 1'h1 : 1'b0 | mask_for_if_ra_lt_0 ? 1'h1 : 1'b0 | mask_for_if_ra_ge_0 ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | mask_for_if_rb_eq_ra ? 1'h1 : 1'b0 | mask_for_if_rb_ne_ra ? 1'h1 : 1'b0 | mask_for_if_signed_rb_lt_ra ? 1'h1 : 1'b0 | mask_for_if_signed_rb_ge_ra ? 1'h1 : 1'b0 | mask_for_if_rb_lt_ra ? 1'h1 : 1'b0 | mask_for_if_rb_ge_ra ? 1'h1 : 1'b0 | mask_for_if_ra_bit__eq_1 ? 1'h1 : 1'b0 | 1'h0;
	assign rd2_addr = mask_for_woi ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_xor_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_or_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_and_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_plus_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_minus_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_lsl_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_lsr_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_asr_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_times_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_notra_and_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_xor_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_or_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_and_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_plus_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_minus_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_lsl_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_lsr_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_asr_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_times_rb ? fetch_inst_0[7:4] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_ra_gt_0 ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_ra_le_0 ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[7:4] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[7:4] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[7:4] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[7:4] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[7:4] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[7:4] : 4'b0 | mask_for_if_rb_eq_ra ? fetch_inst_0[7:4] : 4'b0 | mask_for_if_rb_ne_ra ? fetch_inst_0[7:4] : 4'b0 | mask_for_if_signed_rb_lt_ra ? fetch_inst_0[7:4] : 4'b0 | mask_for_if_signed_rb_ge_ra ? fetch_inst_0[7:4] : 4'b0 | mask_for_if_rb_lt_ra ? fetch_inst_0[7:4] : 4'b0 | mask_for_if_rb_ge_ra ? fetch_inst_0[7:4] : 4'b0 | mask_for_if_rb_bit__eq_0 ? fetch_inst_0[7:4] : 4'b0 | mask_for_mem_raplustiny_ofstimes4_eq_rd ? fetch_inst_0[0] : 4'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? fetch_inst_0[0] : 4'b0 | mask_for_rd_eq_mem8_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_mem16_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_mem32_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_memll32_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_mem8_ra_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_mem16_ra_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_mem32_ra_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_memsr32_ra_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_smem8_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_smem16_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_mem8_raplusfield_e ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_mem16_raplusfield_e ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_mem32_raplusfield_e ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_memll32_raplusfield_e ? fetch_inst_0[3:0] : 4'b0 | mask_for_mem8_raplusfield_e_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_mem16_raplusfield_e_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_mem32_raplusfield_e_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_smem8_raplusfield_e ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_smem16_raplusfield_e ? fetch_inst_0[3:0] : 4'b0 ;
	assign rf_read2_addr = rd2_addr;
	assign read2_needed = mask_for_woi ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_xor_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_or_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_plus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_minus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsl_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_asr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_times_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_notra_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_xor_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_or_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_plus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_minus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsl_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_asr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_times_rb ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f ? 1'h1 : 1'b0 | mask_for_if_ra_gt_0 ? 1'h1 : 1'b0 | mask_for_if_ra_le_0 ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | mask_for_if_rb_eq_ra ? 1'h1 : 1'b0 | mask_for_if_rb_ne_ra ? 1'h1 : 1'b0 | mask_for_if_signed_rb_lt_ra ? 1'h1 : 1'b0 | mask_for_if_signed_rb_ge_ra ? 1'h1 : 1'b0 | mask_for_if_rb_lt_ra ? 1'h1 : 1'b0 | mask_for_if_rb_ge_ra ? 1'h1 : 1'b0 | mask_for_if_rb_bit__eq_0 ? 1'h1 : 1'b0 | mask_for_mem_raplustiny_ofstimes4_eq_rd ? 1'h1 : 1'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_ra ? 1'h1 : 1'b0 | mask_for_mem8_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_mem16_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_mem32_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_memsr32_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_raplusfield_e ? 1'h1 : 1'b0 | mask_for_mem8_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem16_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem32_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_raplusfield_e ? 1'h1 : 1'b0 | 1'h0;
	assign res_addr = mask_for_rd_eq_pc ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_tiny_field_a ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_pc_plus_field_atimes2 ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_minus_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_notra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_bse_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_wse_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_xor_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_or_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_and_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_plus_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_minus_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_lsl_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_lsr_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_asr_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_times_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_notra_and_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_value ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_xor_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_or_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_and_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_plus_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_minus_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_lsl_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_lsr_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_asr_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_times_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_short_value ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_xor_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_or_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_and_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_plus_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_minus_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_lsl_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_lsr_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_asr_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_times_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem8_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem16_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem32_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_memll32_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_smem8_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_smem16_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem8_raplusfield_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem16_raplusfield_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem32_raplusfield_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_memll32_raplusfield_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_smem8_raplusfield_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_smem16_raplusfield_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem8_field_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem16_field_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem32_field_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_memll32_field_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_smem8_field_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_smem16_field_e ? fetch_inst_0[15:12] : 4'b0 ;
	assign rf_rsv_addr = res_addr;
	assign rsv_needed = mask_for_rd_eq_pc ? 1'h1 : 1'b0 | mask_for_rd_eq_tiny_field_a ? 1'h1 : 1'b0 | mask_for_rd_eq_pc_plus_field_atimes2 ? 1'h1 : 1'b0 | mask_for_rd_eq_minus_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_notra ? 1'h1 : 1'b0 | mask_for_rd_eq_bse_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_wse_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_xor_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_or_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_plus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_minus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsl_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_asr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_times_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_notra_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? 1'h1 : 1'b0 | mask_for_rd_eq_value ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_xor_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_or_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_plus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_minus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsl_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_asr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_times_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_short_value ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_xor_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_or_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_and_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_plus_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_minus_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsl_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsr_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_asr_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_times_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_field_e ? 1'h1 : 1'b0 | 1'h0;
	assign rf_request = exec_out_ready & fetch_valid;
	assign fetch_ready = rf_response & exec_out_ready;
	assign exec_out_valid = rf_response & fetch_valid;
	assign op_code = mask_for_swi ? `op__add : 3'b0 | mask_for_stm ? `op__a_sub_b : 3'b0 | mask_for_woi ? `op__a_sub_b : 3'b0 | 1'h1 & fetch_inst_0[15:12] > 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h0 ? `op__add : 3'b0 | mask_for_fence ? `op__b_sub_a : 3'b0 | mask_for_pc_eq_rd ? `op__pc_add : 3'b0 | mask_for_tpc_eq_rd ? `op__cb_lt : 3'b0 | mask_for_rd_eq_pc ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_tpc ? `op__addr : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] > 3'h5 ? `op__add : 3'b0 | mask_for_rd_eq_tiny_field_a ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_pc_plus_field_atimes2 ? `op__pc_add : 3'b0 | mask_for_rd_eq_minus_ra ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_notra ? `op__addr : 3'b0 | mask_for_rd_eq_bse_ra ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_wse_ra ? `op__b_sub_a : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] > 3'h6 &  ~ field_a_is_f ? `op__add : 3'b0 | mask_for_rd_eq_ra_xor_rb ? `op__addr : 3'b0 | mask_for_rd_eq_ra_or_rb ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_ra_and_rb ? `op__add : 3'b0 | mask_for_rd_eq_ra_plus_rb ? `op__add : 3'b0 | mask_for_rd_eq_ra_minus_rb ? `op__a_sub_b : 3'b0 | mask_for_rd_eq_ra_lsl_rb ? `op__add : 3'b0 | mask_for_rd_eq_ra_lsr_rb ? `op__a_sub_b : 3'b0 | mask_for_rd_eq_ra_asr_rb ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_notra_and_rb ? `op__a_sub_b : 3'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? `op__add : 3'b0 | mask_for_rd_eq_value ? `op__b_sub_a : 3'b0 | mask_for_pc_eq_value ? `op__pc_add : 3'b0 | mask_for_tpc_eq_value ? `op__cb_lt : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'h8 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `op__add : 3'b0 | mask_for_rd_eq_field_e_xor_rb ? `op__addr : 3'b0 | mask_for_rd_eq_field_e_or_rb ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_field_e_and_rb ? `op__add : 3'b0 | mask_for_rd_eq_field_e_plus_rb ? `op__add : 3'b0 | mask_for_rd_eq_field_e_minus_rb ? `op__a_sub_b : 3'b0 | mask_for_rd_eq_field_e_lsl_rb ? `op__add : 3'b0 | mask_for_rd_eq_field_e_lsr_rb ? `op__a_sub_b : 3'b0 | mask_for_rd_eq_field_e_asr_rb ? `op__b_sub_a : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf ? `op__add : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf ? `op__add : 3'b0 | mask_for_rd_eq_short_value ? `op__b_sub_a : 3'b0 | mask_for_pc_eq_short_value ? `op__pc_add : 3'b0 | mask_for_tpc_eq_short_value ? `op__cb_lt : 3'b0 | mask_for_rd_eq_field_e_xor_ra ? `op__addr : 3'b0 | mask_for_rd_eq_field_e_or_ra ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_field_e_and_ra ? `op__add : 3'b0 | mask_for_rd_eq_field_e_plus_ra ? `op__add : 3'b0 | mask_for_rd_eq_field_e_minus_ra ? `op__a_sub_b : 3'b0 | mask_for_rd_eq_field_e_lsl_ra ? `op__add : 3'b0 | mask_for_rd_eq_field_e_lsr_ra ? `op__a_sub_b : 3'b0 | mask_for_rd_eq_field_e_asr_ra ? `op__b_sub_a : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? `op__a_sub_b : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? `op__b_sub_a : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? `op__addr : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? `op__pc_add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f ? `op__addr : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f ? `op__pc_add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h6 &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h7 &  ~ field_a_is_f ? `op__add : 3'b0 | mask_for_if_ra_eq_0 ? `op__a_sub_b : 3'b0 | mask_for_if_ra_ne_0 ? `op__b_sub_a : 3'b0 | mask_for_if_ra_lt_0 ? `op__addr : 3'b0 | mask_for_if_ra_ge_0 ? `op__pc_add : 3'b0 | mask_for_if_ra_gt_0 ? `op__addr : 3'b0 | mask_for_if_ra_le_0 ? `op__pc_add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__a_sub_b : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__b_sub_a : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__addr : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__pc_add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__cb_lt : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__cb_ge : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h7 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'h8 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__add : 3'b0 | mask_for_if_rb_eq_ra ? `op__a_sub_b : 3'b0 | mask_for_if_rb_ne_ra ? `op__b_sub_a : 3'b0 | mask_for_if_signed_rb_lt_ra ? `op__addr : 3'b0 | mask_for_if_signed_rb_ge_ra ? `op__pc_add : 3'b0 | mask_for_if_rb_lt_ra ? `op__cb_lt : 3'b0 | mask_for_if_rb_ge_ra ? `op__cb_ge : 3'b0 | mask_for_if_ra_bit__eq_1 ? `op__add : 3'b0 | mask_for_if_rb_bit__eq_0 ? `op__add : 3'b0 | mask_for_mem_raplustiny_ofstimes4_eq_rd ? `op__addr : 3'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? `op__addr : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? `op__add : 3'b0 | mask_for_rd_eq_mem8_ra ? `op__addr : 3'b0 | mask_for_rd_eq_mem16_ra ? `op__addr : 3'b0 | mask_for_rd_eq_mem32_ra ? `op__addr : 3'b0 | mask_for_rd_eq_memll32_ra ? `op__addr : 3'b0 | mask_for_mem8_ra_eq_rd ? `op__addr : 3'b0 | mask_for_mem16_ra_eq_rd ? `op__addr : 3'b0 | mask_for_mem32_ra_eq_rd ? `op__addr : 3'b0 | mask_for_memsr32_ra_eq_rd ? `op__addr : 3'b0 | mask_for_rd_eq_smem8_ra ? `op__addr : 3'b0 | mask_for_rd_eq_smem16_ra ? `op__addr : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | mask_for_rd_eq_mem8_raplusfield_e ? `op__addr : 3'b0 | mask_for_rd_eq_mem16_raplusfield_e ? `op__addr : 3'b0 | mask_for_rd_eq_mem32_raplusfield_e ? `op__addr : 3'b0 | mask_for_rd_eq_memll32_raplusfield_e ? `op__addr : 3'b0 | mask_for_mem8_raplusfield_e_eq_rd ? `op__addr : 3'b0 | mask_for_mem16_raplusfield_e_eq_rd ? `op__addr : 3'b0 | mask_for_mem32_raplusfield_e_eq_rd ? `op__addr : 3'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? `op__addr : 3'b0 | mask_for_rd_eq_smem8_raplusfield_e ? `op__addr : 3'b0 | mask_for_rd_eq_smem16_raplusfield_e ? `op__addr : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | mask_for_rd_eq_mem8_field_e ? `op__addr : 3'b0 | mask_for_rd_eq_mem16_field_e ? `op__addr : 3'b0 | mask_for_rd_eq_mem32_field_e ? `op__addr : 3'b0 | mask_for_rd_eq_memll32_field_e ? `op__addr : 3'b0 | mask_for_mem8_field_e_eq_rd ? `op__addr : 3'b0 | mask_for_mem16_field_e_eq_rd ? `op__addr : 3'b0 | mask_for_mem32_field_e_eq_rd ? `op__addr : 3'b0 | mask_for_memsr32_field_e_eq_rd ? `op__addr : 3'b0 | mask_for_rd_eq_smem8_field_e ? `op__addr : 3'b0 | mask_for_rd_eq_smem16_field_e ? `op__addr : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `op__add : 3'b0 | mask_expr ? `op__add : 3'b0 ;
	assign exec_unit = mask_for_swi ? `exec__misc : 3'b0 | mask_for_stm ? `exec__misc : 3'b0 | mask_for_woi ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] > 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h0 ? `exec__misc : 3'b0 | mask_for_fence ? `exec__bitwise : 3'b0 | mask_for_pc_eq_rd ? `exec__misc : 3'b0 | mask_for_tpc_eq_rd ? `exec__misc : 3'b0 | mask_for_rd_eq_pc ? `exec__misc : 3'b0 | mask_for_rd_eq_tpc ? `exec__misc : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] > 3'h5 ? `exec__misc : 3'b0 | mask_for_rd_eq_tiny_field_a ? `exec__bitwise : 3'b0 | mask_for_rd_eq_pc_plus_field_atimes2 ? `exec__adder : 3'b0 | mask_for_rd_eq_minus_ra ? `exec__adder : 3'b0 | mask_for_rd_eq_notra ? `exec__bitwise : 3'b0 | mask_for_rd_eq_bse_ra ? `exec__bitwise : 3'b0 | mask_for_rd_eq_wse_ra ? `exec__bitwise : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] > 3'h6 &  ~ field_a_is_f ? `exec__misc : 3'b0 | mask_for_rd_eq_ra_xor_rb ? `exec__bitwise : 3'b0 | mask_for_rd_eq_ra_or_rb ? `exec__bitwise : 3'b0 | mask_for_rd_eq_ra_and_rb ? `exec__bitwise : 3'b0 | mask_for_rd_eq_ra_plus_rb ? `exec__adder : 3'b0 | mask_for_rd_eq_ra_minus_rb ? `exec__adder : 3'b0 | mask_for_rd_eq_ra_lsl_rb ? `exec__shift : 3'b0 | mask_for_rd_eq_ra_lsr_rb ? `exec__shift : 3'b0 | mask_for_rd_eq_ra_asr_rb ? `exec__shift : 3'b0 | mask_for_rd_eq_ra_times_rb ? `exec__mult : 3'b0 | mask_for_rd_eq_notra_and_rb ? `exec__bitwise : 3'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? `exec__adder : 3'b0 | mask_for_rd_eq_value ? `exec__bitwise : 3'b0 | mask_for_pc_eq_value ? `exec__misc : 3'b0 | mask_for_tpc_eq_value ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'h8 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `exec__misc : 3'b0 | mask_for_rd_eq_field_e_xor_rb ? `exec__bitwise : 3'b0 | mask_for_rd_eq_field_e_or_rb ? `exec__bitwise : 3'b0 | mask_for_rd_eq_field_e_and_rb ? `exec__bitwise : 3'b0 | mask_for_rd_eq_field_e_plus_rb ? `exec__adder : 3'b0 | mask_for_rd_eq_field_e_minus_rb ? `exec__adder : 3'b0 | mask_for_rd_eq_field_e_lsl_rb ? `exec__shift : 3'b0 | mask_for_rd_eq_field_e_lsr_rb ? `exec__shift : 3'b0 | mask_for_rd_eq_field_e_asr_rb ? `exec__shift : 3'b0 | mask_for_rd_eq_field_e_times_rb ? `exec__mult : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf ? `exec__misc : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf ? `exec__misc : 3'b0 | mask_for_rd_eq_short_value ? `exec__bitwise : 3'b0 | mask_for_pc_eq_short_value ? `exec__misc : 3'b0 | mask_for_tpc_eq_short_value ? `exec__misc : 3'b0 | mask_for_rd_eq_field_e_xor_ra ? `exec__bitwise : 3'b0 | mask_for_rd_eq_field_e_or_ra ? `exec__bitwise : 3'b0 | mask_for_rd_eq_field_e_and_ra ? `exec__bitwise : 3'b0 | mask_for_rd_eq_field_e_plus_ra ? `exec__adder : 3'b0 | mask_for_rd_eq_field_e_minus_ra ? `exec__adder : 3'b0 | mask_for_rd_eq_field_e_lsl_ra ? `exec__shift : 3'b0 | mask_for_rd_eq_field_e_lsr_ra ? `exec__shift : 3'b0 | mask_for_rd_eq_field_e_asr_ra ? `exec__shift : 3'b0 | mask_for_rd_eq_field_e_times_ra ? `exec__mult : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h6 &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h7 &  ~ field_a_is_f ? `exec__misc : 3'b0 | mask_for_if_ra_eq_0 ? `exec__cbranch : 3'b0 | mask_for_if_ra_ne_0 ? `exec__cbranch : 3'b0 | mask_for_if_ra_lt_0 ? `exec__cbranch : 3'b0 | mask_for_if_ra_ge_0 ? `exec__cbranch : 3'b0 | mask_for_if_ra_gt_0 ? `exec__cbranch : 3'b0 | mask_for_if_ra_le_0 ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h7 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'h8 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__misc : 3'b0 | mask_for_if_rb_eq_ra ? `exec__cbranch : 3'b0 | mask_for_if_rb_ne_ra ? `exec__cbranch : 3'b0 | mask_for_if_signed_rb_lt_ra ? `exec__cbranch : 3'b0 | mask_for_if_signed_rb_ge_ra ? `exec__cbranch : 3'b0 | mask_for_if_rb_lt_ra ? `exec__cbranch : 3'b0 | mask_for_if_rb_ge_ra ? `exec__cbranch : 3'b0 | mask_for_if_ra_bit__eq_1 ? `exec__bbranch : 3'b0 | mask_for_if_rb_bit__eq_0 ? `exec__bbranch : 3'b0 | mask_for_mem_raplustiny_ofstimes4_eq_rd ? `exec__adder : 3'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? `exec__adder : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? `exec__misc : 3'b0 | mask_for_rd_eq_mem8_ra ? `exec__adder : 3'b0 | mask_for_rd_eq_mem16_ra ? `exec__adder : 3'b0 | mask_for_rd_eq_mem32_ra ? `exec__adder : 3'b0 | mask_for_rd_eq_memll32_ra ? `exec__adder : 3'b0 | mask_for_mem8_ra_eq_rd ? `exec__adder : 3'b0 | mask_for_mem16_ra_eq_rd ? `exec__adder : 3'b0 | mask_for_mem32_ra_eq_rd ? `exec__adder : 3'b0 | mask_for_memsr32_ra_eq_rd ? `exec__adder : 3'b0 | mask_for_rd_eq_smem8_ra ? `exec__adder : 3'b0 | mask_for_rd_eq_smem16_ra ? `exec__adder : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | mask_for_rd_eq_mem8_raplusfield_e ? `exec__adder : 3'b0 | mask_for_rd_eq_mem16_raplusfield_e ? `exec__adder : 3'b0 | mask_for_rd_eq_mem32_raplusfield_e ? `exec__adder : 3'b0 | mask_for_rd_eq_memll32_raplusfield_e ? `exec__adder : 3'b0 | mask_for_mem8_raplusfield_e_eq_rd ? `exec__adder : 3'b0 | mask_for_mem16_raplusfield_e_eq_rd ? `exec__adder : 3'b0 | mask_for_mem32_raplusfield_e_eq_rd ? `exec__adder : 3'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? `exec__adder : 3'b0 | mask_for_rd_eq_smem8_raplusfield_e ? `exec__adder : 3'b0 | mask_for_rd_eq_smem16_raplusfield_e ? `exec__adder : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | mask_for_rd_eq_mem8_field_e ? `exec__adder : 3'b0 | mask_for_rd_eq_mem16_field_e ? `exec__adder : 3'b0 | mask_for_rd_eq_mem32_field_e ? `exec__adder : 3'b0 | mask_for_rd_eq_memll32_field_e ? `exec__adder : 3'b0 | mask_for_mem8_field_e_eq_rd ? `exec__adder : 3'b0 | mask_for_mem16_field_e_eq_rd ? `exec__adder : 3'b0 | mask_for_mem32_field_e_eq_rd ? `exec__adder : 3'b0 | mask_for_memsr32_field_e_eq_rd ? `exec__adder : 3'b0 | mask_for_rd_eq_smem8_field_e ? `exec__adder : 3'b0 | mask_for_rd_eq_smem16_field_e ? `exec__adder : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `exec__misc : 3'b0 | mask_expr ? `exec__misc : 3'b0 ;
	assign op_a = mask_for_swi ? fetch_inst_0[3:0] : 32'b0 | mask_for_woi ? rf_read1_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] > 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h0 ? 3'h7 : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] > 3'h5 ? 3'h7 : 32'b0 | mask_for_rd_eq_tiny_field_a ? ones_field_a : 32'b0 | mask_for_rd_eq_pc_plus_field_atimes2 ? ones_field_a : 32'b0 | mask_for_rd_eq_minus_ra ? rf_read1_data : 32'b0 | mask_for_rd_eq_notra ? rf_read1_data : 32'b0 | mask_for_rd_eq_bse_ra ? rf_read1_data : 32'b0 | mask_for_rd_eq_wse_ra ? rf_read1_data : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] > 3'h6 &  ~ field_a_is_f ? 3'h7 : 32'b0 | mask_for_rd_eq_ra_xor_rb ? rf_read1_data : 32'b0 | mask_for_rd_eq_ra_or_rb ? rf_read1_data : 32'b0 | mask_for_rd_eq_ra_and_rb ? rf_read1_data : 32'b0 | mask_for_rd_eq_ra_plus_rb ? rf_read1_data : 32'b0 | mask_for_rd_eq_ra_minus_rb ? rf_read1_data : 32'b0 | mask_for_rd_eq_ra_lsl_rb ? rf_read1_data : 32'b0 | mask_for_rd_eq_ra_lsr_rb ? rf_read1_data : 32'b0 | mask_for_rd_eq_ra_asr_rb ? rf_read1_data : 32'b0 | mask_for_rd_eq_ra_times_rb ? rf_read1_data : 32'b0 | mask_for_rd_eq_notra_and_rb ? rf_read1_data : 32'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? ones_field_a : 32'b0 | mask_for_rd_eq_value ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'h8 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? 3'h7 : 32'b0 | mask_for_rd_eq_field_e_xor_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_or_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_and_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_plus_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_minus_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_lsl_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_lsr_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_asr_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_times_rb ? field_e : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf ? 3'h7 : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf ? 3'h7 : 32'b0 | mask_for_rd_eq_short_value ? field_e : 32'b0 | mask_for_rd_eq_field_e_xor_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_or_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_and_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_plus_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_minus_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_lsl_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_lsr_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_asr_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_times_ra ? field_e : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? rf_read1_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? rf_read1_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? rf_read1_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? rf_read1_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f ? 1'h0 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f ? 1'h0 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h6 &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h7 &  ~ field_a_is_f ? 3'h7 : 32'b0 | mask_for_if_ra_eq_0 ? rf_read1_data : 32'b0 | mask_for_if_ra_ne_0 ? rf_read1_data : 32'b0 | mask_for_if_ra_lt_0 ? rf_read1_data : 32'b0 | mask_for_if_ra_ge_0 ? rf_read1_data : 32'b0 | mask_for_if_ra_gt_0 ? 1'h0 : 32'b0 | mask_for_if_ra_le_0 ? 1'h0 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? rf_read2_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? rf_read2_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? rf_read2_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? rf_read2_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? rf_read2_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? rf_read2_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h7 &  ~ field_b_is_f &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'h8 &  ~ field_b_is_f &  ~ field_a_is_f ? 3'h7 : 32'b0 | mask_for_if_rb_eq_ra ? rf_read2_data : 32'b0 | mask_for_if_rb_ne_ra ? rf_read2_data : 32'b0 | mask_for_if_signed_rb_lt_ra ? rf_read2_data : 32'b0 | mask_for_if_signed_rb_ge_ra ? rf_read2_data : 32'b0 | mask_for_if_rb_lt_ra ? rf_read2_data : 32'b0 | mask_for_if_rb_ge_ra ? rf_read2_data : 32'b0 | mask_for_if_ra_bit__eq_1 ? rf_read1_data : 32'b0 | mask_for_if_rb_bit__eq_0 ? fetch_inst_0[11:8] : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? 3'h7 : 32'b0 | mask_expr ? 3'h7 : 32'b0 ;
	assign op_b = mask_for_woi ? rf_read2_data : 32'b0 | mask_for_rd_eq_tiny_field_a ? 1'h0 : 32'b0 | mask_for_rd_eq_pc_plus_field_atimes2 ? 1'h0 : 32'b0 | mask_for_rd_eq_minus_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_notra ? 32'hffffffff : 32'b0 | mask_for_rd_eq_bse_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_wse_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_ra_xor_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_ra_or_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_ra_and_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_ra_plus_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_ra_minus_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_ra_lsl_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_ra_lsr_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_ra_asr_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_ra_times_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_notra_and_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? rf_read2_data : 32'b0 | mask_for_rd_eq_value ? 1'h0 : 32'b0 | mask_for_rd_eq_field_e_xor_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_field_e_or_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_field_e_and_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_field_e_plus_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_field_e_minus_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_field_e_lsl_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_field_e_lsr_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_field_e_asr_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_field_e_times_rb ? rf_read2_data : 32'b0 | mask_for_rd_eq_short_value ? 1'h0 : 32'b0 | mask_for_rd_eq_field_e_xor_ra ? rf_read1_data : 32'b0 | mask_for_rd_eq_field_e_or_ra ? rf_read1_data : 32'b0 | mask_for_rd_eq_field_e_and_ra ? rf_read1_data : 32'b0 | mask_for_rd_eq_field_e_plus_ra ? rf_read1_data : 32'b0 | mask_for_rd_eq_field_e_minus_ra ? rf_read1_data : 32'b0 | mask_for_rd_eq_field_e_lsl_ra ? rf_read1_data : 32'b0 | mask_for_rd_eq_field_e_lsr_ra ? rf_read1_data : 32'b0 | mask_for_rd_eq_field_e_asr_ra ? rf_read1_data : 32'b0 | mask_for_rd_eq_field_e_times_ra ? rf_read1_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? 1'h0 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? 1'h0 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? 1'h0 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? 1'h0 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f ? rf_read1_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f ? rf_read1_data : 32'b0 | mask_for_if_ra_eq_0 ? 1'h0 : 32'b0 | mask_for_if_ra_ne_0 ? 1'h0 : 32'b0 | mask_for_if_ra_lt_0 ? 1'h0 : 32'b0 | mask_for_if_ra_ge_0 ? 1'h0 : 32'b0 | mask_for_if_ra_gt_0 ? rf_read1_data : 32'b0 | mask_for_if_ra_le_0 ? rf_read1_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? rf_read1_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? rf_read1_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? rf_read1_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? rf_read1_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? rf_read1_data : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? rf_read1_data : 32'b0 | mask_for_if_rb_eq_ra ? rf_read1_data : 32'b0 | mask_for_if_rb_ne_ra ? rf_read1_data : 32'b0 | mask_for_if_signed_rb_lt_ra ? rf_read1_data : 32'b0 | mask_for_if_signed_rb_ge_ra ? rf_read1_data : 32'b0 | mask_for_if_rb_lt_ra ? rf_read1_data : 32'b0 | mask_for_if_rb_ge_ra ? rf_read1_data : 32'b0 | mask_for_if_ra_bit__eq_1 ? fetch_inst_0[11:8] : 32'b0 | mask_for_if_rb_bit__eq_0 ? rf_read2_data : 32'b0 | mask_for_mem_raplustiny_ofstimes4_eq_rd ? rf_read2_data : 32'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? rf_read2_data : 32'b0 | mask_for_rd_eq_mem8_ra ? rf_read2_data : 32'b0 | mask_for_rd_eq_mem16_ra ? rf_read2_data : 32'b0 | mask_for_rd_eq_mem32_ra ? rf_read2_data : 32'b0 | mask_for_rd_eq_memll32_ra ? rf_read2_data : 32'b0 | mask_for_mem8_ra_eq_rd ? rf_read2_data : 32'b0 | mask_for_mem16_ra_eq_rd ? rf_read2_data : 32'b0 | mask_for_mem32_ra_eq_rd ? rf_read2_data : 32'b0 | mask_for_memsr32_ra_eq_rd ? rf_read2_data : 32'b0 | mask_for_rd_eq_smem8_ra ? rf_read2_data : 32'b0 | mask_for_rd_eq_smem16_ra ? rf_read2_data : 32'b0 | mask_for_rd_eq_mem8_raplusfield_e ? rf_read2_data : 32'b0 | mask_for_rd_eq_mem16_raplusfield_e ? rf_read2_data : 32'b0 | mask_for_rd_eq_mem32_raplusfield_e ? rf_read2_data : 32'b0 | mask_for_rd_eq_memll32_raplusfield_e ? rf_read2_data : 32'b0 | mask_for_mem8_raplusfield_e_eq_rd ? rf_read2_data : 32'b0 | mask_for_mem16_raplusfield_e_eq_rd ? rf_read2_data : 32'b0 | mask_for_mem32_raplusfield_e_eq_rd ? rf_read2_data : 32'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? rf_read2_data : 32'b0 | mask_for_rd_eq_smem8_raplusfield_e ? rf_read2_data : 32'b0 | mask_for_rd_eq_smem16_raplusfield_e ? rf_read2_data : 32'b0 | mask_for_rd_eq_mem8_field_e ? 1'h0 : 32'b0 | mask_for_rd_eq_mem16_field_e ? 1'h0 : 32'b0 | mask_for_rd_eq_mem32_field_e ? 1'h0 : 32'b0 | mask_for_rd_eq_memll32_field_e ? 1'h0 : 32'b0 | mask_for_mem8_field_e_eq_rd ? 1'h0 : 32'b0 | mask_for_mem16_field_e_eq_rd ? 1'h0 : 32'b0 | mask_for_mem32_field_e_eq_rd ? 1'h0 : 32'b0 | mask_for_memsr32_field_e_eq_rd ? 1'h0 : 32'b0 | mask_for_rd_eq_smem8_field_e ? 1'h0 : 32'b0 | mask_for_rd_eq_smem16_field_e ? 1'h0 : 32'b0 ;
	assign op_imm = mask_for_woi ? 1'h0 : 32'b0 | mask_for_pc_eq_rd ? rf_read1_data : 32'b0 | mask_for_tpc_eq_rd ? rf_read1_data : 32'b0 | mask_for_rd_eq_tpc ? rf_read1_data : 32'b0 | mask_for_pc_eq_value ? field_e : 32'b0 | mask_for_tpc_eq_value ? field_e : 32'b0 | mask_for_pc_eq_short_value ? field_e : 32'b0 | mask_for_tpc_eq_short_value ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f ? field_e : 32'b0 | mask_for_if_ra_eq_0 ? field_e : 32'b0 | mask_for_if_ra_ne_0 ? field_e : 32'b0 | mask_for_if_ra_lt_0 ? field_e : 32'b0 | mask_for_if_ra_ge_0 ? field_e : 32'b0 | mask_for_if_ra_gt_0 ? field_e : 32'b0 | mask_for_if_ra_le_0 ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? field_e : 32'b0 | mask_for_if_rb_eq_ra ? field_e : 32'b0 | mask_for_if_rb_ne_ra ? field_e : 32'b0 | mask_for_if_signed_rb_lt_ra ? field_e : 32'b0 | mask_for_if_signed_rb_ge_ra ? field_e : 32'b0 | mask_for_if_rb_lt_ra ? field_e : 32'b0 | mask_for_if_rb_ge_ra ? field_e : 32'b0 | mask_for_if_ra_bit__eq_1 ? field_e : 32'b0 | mask_for_if_rb_bit__eq_0 ? field_e : 32'b0 | mask_for_mem_raplustiny_ofstimes4_eq_rd ? tiny_ofs : 32'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? tiny_ofs : 32'b0 | mask_for_rd_eq_mem8_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_mem16_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_mem32_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_memll32_ra ? 1'h0 : 32'b0 | mask_for_mem8_ra_eq_rd ? 1'h0 : 32'b0 | mask_for_mem16_ra_eq_rd ? 1'h0 : 32'b0 | mask_for_mem32_ra_eq_rd ? 1'h0 : 32'b0 | mask_for_memsr32_ra_eq_rd ? 1'h0 : 32'b0 | mask_for_rd_eq_smem8_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_smem16_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_mem8_raplusfield_e ? field_e : 32'b0 | mask_for_rd_eq_mem16_raplusfield_e ? field_e : 32'b0 | mask_for_rd_eq_mem32_raplusfield_e ? field_e : 32'b0 | mask_for_rd_eq_memll32_raplusfield_e ? field_e : 32'b0 | mask_for_mem8_raplusfield_e_eq_rd ? field_e : 32'b0 | mask_for_mem16_raplusfield_e_eq_rd ? field_e : 32'b0 | mask_for_mem32_raplusfield_e_eq_rd ? field_e : 32'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? field_e : 32'b0 | mask_for_rd_eq_smem8_raplusfield_e ? field_e : 32'b0 | mask_for_rd_eq_smem16_raplusfield_e ? field_e : 32'b0 | mask_for_rd_eq_mem8_field_e ? field_e : 32'b0 | mask_for_rd_eq_mem16_field_e ? field_e : 32'b0 | mask_for_rd_eq_mem32_field_e ? field_e : 32'b0 | mask_for_rd_eq_memll32_field_e ? field_e : 32'b0 | mask_for_mem8_field_e_eq_rd ? field_e : 32'b0 | mask_for_mem16_field_e_eq_rd ? field_e : 32'b0 | mask_for_mem32_field_e_eq_rd ? field_e : 32'b0 | mask_for_memsr32_field_e_eq_rd ? field_e : 32'b0 | mask_for_rd_eq_smem8_field_e ? field_e : 32'b0 | mask_for_rd_eq_smem16_field_e ? field_e : 32'b0 ;
	assign mem_len = mask_for_mem_raplustiny_ofstimes4_eq_rd ? 2'h3 : 2'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? 2'h3 : 2'b0 | mask_for_rd_eq_mem8_ra ? 1'h1 : 2'b0 | mask_for_rd_eq_mem16_ra ? 2'h2 : 2'b0 | mask_for_rd_eq_mem32_ra ? 2'h3 : 2'b0 | mask_for_rd_eq_memll32_ra ? 2'h3 : 2'b0 | mask_for_mem8_ra_eq_rd ? 1'h1 : 2'b0 | mask_for_mem16_ra_eq_rd ? 2'h2 : 2'b0 | mask_for_mem32_ra_eq_rd ? 2'h3 : 2'b0 | mask_for_memsr32_ra_eq_rd ? 2'h3 : 2'b0 | mask_for_rd_eq_smem8_ra ? 1'h1 : 2'b0 | mask_for_rd_eq_smem16_ra ? 2'h2 : 2'b0 | mask_for_rd_eq_mem8_raplusfield_e ? 1'h1 : 2'b0 | mask_for_rd_eq_mem16_raplusfield_e ? 2'h2 : 2'b0 | mask_for_rd_eq_mem32_raplusfield_e ? 2'h3 : 2'b0 | mask_for_rd_eq_memll32_raplusfield_e ? 2'h3 : 2'b0 | mask_for_mem8_raplusfield_e_eq_rd ? 1'h1 : 2'b0 | mask_for_mem16_raplusfield_e_eq_rd ? 2'h2 : 2'b0 | mask_for_mem32_raplusfield_e_eq_rd ? 2'h3 : 2'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? 2'h3 : 2'b0 | mask_for_rd_eq_smem8_raplusfield_e ? 1'h1 : 2'b0 | mask_for_rd_eq_smem16_raplusfield_e ? 2'h2 : 2'b0 | mask_for_rd_eq_mem8_field_e ? 1'h1 : 2'b0 | mask_for_rd_eq_mem16_field_e ? 2'h2 : 2'b0 | mask_for_rd_eq_mem32_field_e ? 2'h3 : 2'b0 | mask_for_rd_eq_memll32_field_e ? 2'h3 : 2'b0 | mask_for_mem8_field_e_eq_rd ? 1'h1 : 2'b0 | mask_for_mem16_field_e_eq_rd ? 2'h2 : 2'b0 | mask_for_mem32_field_e_eq_rd ? 2'h3 : 2'b0 | mask_for_memsr32_field_e_eq_rd ? 2'h3 : 2'b0 | mask_for_rd_eq_smem8_field_e ? 1'h1 : 2'b0 | mask_for_rd_eq_smem16_field_e ? 2'h2 : 2'b0 ;
	assign is_ld = mask_for_swi ? 1'h0 : 1'b0 | mask_for_stm ? 1'h0 : 1'b0 | mask_for_woi ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] > 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h0 ? 1'h0 : 1'b0 | mask_for_fence ? 1'h0 : 1'b0 | mask_for_pc_eq_rd ? 1'h0 : 1'b0 | mask_for_tpc_eq_rd ? 1'h0 : 1'b0 | mask_for_rd_eq_pc ? 1'h0 : 1'b0 | mask_for_rd_eq_tpc ? 1'h0 : 1'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] > 3'h5 ? 1'h0 : 1'b0 | mask_for_rd_eq_tiny_field_a ? 1'h0 : 1'b0 | mask_for_rd_eq_pc_plus_field_atimes2 ? 1'h0 : 1'b0 | mask_for_rd_eq_minus_ra ? 1'h0 : 1'b0 | mask_for_rd_eq_notra ? 1'h0 : 1'b0 | mask_for_rd_eq_bse_ra ? 1'h0 : 1'b0 | mask_for_rd_eq_wse_ra ? 1'h0 : 1'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] > 3'h6 &  ~ field_a_is_f ? 1'h0 : 1'b0 | mask_for_rd_eq_ra_xor_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_ra_or_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_ra_and_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_ra_plus_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_ra_minus_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_ra_lsl_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_ra_lsr_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_ra_asr_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_ra_times_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_notra_and_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? 1'h0 : 1'b0 | mask_for_rd_eq_value ? 1'h0 : 1'b0 | mask_for_pc_eq_value ? 1'h0 : 1'b0 | mask_for_tpc_eq_value ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'h8 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_xor_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_or_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_and_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_plus_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_minus_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_lsl_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_lsr_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_asr_rb ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_times_rb ? 1'h0 : 1'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf ? 1'h0 : 1'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf ? 1'h0 : 1'b0 | mask_for_rd_eq_short_value ? 1'h0 : 1'b0 | mask_for_pc_eq_short_value ? 1'h0 : 1'b0 | mask_for_tpc_eq_short_value ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_xor_ra ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_or_ra ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_and_ra ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_plus_ra ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_minus_ra ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_lsl_ra ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_lsr_ra ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_asr_ra ? 1'h0 : 1'b0 | mask_for_rd_eq_field_e_times_ra ? 1'h0 : 1'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h6 &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h7 &  ~ field_a_is_f ? 1'h0 : 1'b0 | mask_for_if_ra_eq_0 ? 1'h0 : 1'b0 | mask_for_if_ra_ne_0 ? 1'h0 : 1'b0 | mask_for_if_ra_lt_0 ? 1'h0 : 1'b0 | mask_for_if_ra_ge_0 ? 1'h0 : 1'b0 | mask_for_if_ra_gt_0 ? 1'h0 : 1'b0 | mask_for_if_ra_le_0 ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h7 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'h8 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h0 : 1'b0 | mask_for_if_rb_eq_ra ? 1'h0 : 1'b0 | mask_for_if_rb_ne_ra ? 1'h0 : 1'b0 | mask_for_if_signed_rb_lt_ra ? 1'h0 : 1'b0 | mask_for_if_signed_rb_ge_ra ? 1'h0 : 1'b0 | mask_for_if_rb_lt_ra ? 1'h0 : 1'b0 | mask_for_if_rb_ge_ra ? 1'h0 : 1'b0 | mask_for_if_ra_bit__eq_1 ? 1'h0 : 1'b0 | mask_for_if_rb_bit__eq_0 ? 1'h0 : 1'b0 | mask_for_mem_raplustiny_ofstimes4_eq_rd ? 1'h0 : 1'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? 1'h1 : 1'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? 1'h0 : 1'b0 | mask_for_rd_eq_mem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_ra ? 1'h1 : 1'b0 | mask_for_mem8_ra_eq_rd ? 1'h0 : 1'b0 | mask_for_mem16_ra_eq_rd ? 1'h0 : 1'b0 | mask_for_mem32_ra_eq_rd ? 1'h0 : 1'b0 | mask_for_memsr32_ra_eq_rd ? 1'h0 : 1'b0 | mask_for_rd_eq_smem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_ra ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 1'h0 : 1'b0 | mask_for_rd_eq_mem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_raplusfield_e ? 1'h1 : 1'b0 | mask_for_mem8_raplusfield_e_eq_rd ? 1'h0 : 1'b0 | mask_for_mem16_raplusfield_e_eq_rd ? 1'h0 : 1'b0 | mask_for_mem32_raplusfield_e_eq_rd ? 1'h0 : 1'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? 1'h0 : 1'b0 | mask_for_rd_eq_smem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_raplusfield_e ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 1'h0 : 1'b0 | mask_for_rd_eq_mem8_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_field_e ? 1'h1 : 1'b0 | mask_for_mem8_field_e_eq_rd ? 1'h0 : 1'b0 | mask_for_mem16_field_e_eq_rd ? 1'h0 : 1'b0 | mask_for_mem32_field_e_eq_rd ? 1'h0 : 1'b0 | mask_for_memsr32_field_e_eq_rd ? 1'h0 : 1'b0 | mask_for_rd_eq_smem8_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_field_e ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? 1'h0 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? 1'h0 : 1'b0 | mask_expr ? 1'h0 : 1'b0 | 1'h0;
	assign is_st = mask_for_mem_raplustiny_ofstimes4_eq_rd ? 1'h1 : 1'b0 | mask_for_mem8_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_mem16_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_mem32_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_memsr32_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_mem8_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem16_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem32_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem8_field_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem16_field_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem32_field_e_eq_rd ? 1'h1 : 1'b0 | mask_for_memsr32_field_e_eq_rd ? 1'h1 : 1'b0 | 1'h0;
	assign bse = mask_for_rd_eq_bse_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_field_e ? 1'h1 : 1'b0 | 1'h0;
	assign wse = mask_for_rd_eq_wse_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_field_e ? 1'h1 : 1'b0 | 1'h0;
	assign bze = mask_for_rd_eq_mem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_field_e ? 1'h1 : 1'b0 | 1'h0;
	assign wze = mask_for_rd_eq_mem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_field_e ? 1'h1 : 1'b0 | 1'h0;
	assign exec_out_result_reg_addr = res_addr;
	assign mem_len_1 = mem_len;

	ForwardBuf u2362 (
		.input_port_do_bse(bse),
		.input_port_do_bze(bze),
		.input_port_do_wse(wse),
		.input_port_do_wze(wze),
		.input_port_exec_unit(exec_unit),
		.input_port_fetch_av(fetch_av),
		.input_port_inst_len(fetch_inst_len),
		.input_port_is_load(is_ld),
		.input_port_is_store(is_st),
		.input_port_mem_access_len(mem_len_1),
		.input_port_op_a(op_a),
		.input_port_op_b(op_b),
		.input_port_op_imm(op_imm),
		.input_port_opcode(op_code),
		.input_port_ready(exec_out_ready),
		.input_port_result_reg_addr(exec_out_result_reg_addr),
		.input_port_result_reg_addr_valid(rsv_needed),
		.input_port_valid(exec_out_valid),

		.output_port_do_bse(exec_do_bse),
		.output_port_do_bze(exec_do_bze),
		.output_port_do_wse(exec_do_wse),
		.output_port_do_wze(exec_do_wze),
		.output_port_exec_unit(exec_exec_unit),
		.output_port_fetch_av(exec_fetch_av),
		.output_port_inst_len(exec_inst_len),
		.output_port_is_load(exec_is_load),
		.output_port_is_store(exec_is_store),
		.output_port_mem_access_len(exec_mem_access_len),
		.output_port_op_a(exec_op_a),
		.output_port_op_b(exec_op_b),
		.output_port_op_imm(exec_op_imm),
		.output_port_opcode(exec_opcode),
		.output_port_ready(exec_ready),
		.output_port_result_reg_addr(exec_result_reg_addr),
		.output_port_result_reg_addr_valid(exec_result_reg_addr_valid),
		.output_port_valid(exec_valid),

		.clock_port(clk),
		.reset_port(rst)
	);

	assign exec_out_fetch_av = fetch_av;
	assign exec_out_inst_len = fetch_inst_len;
	assign u36_output_port = fetch_inst_0[3:0] + 1'h1;
	assign mask_for_sii = mask_expr;
	assign expr = mask_expr;
	assign exec_out_exec_unit = exec_unit;
	assign exec_out_opcode = op_code;
	assign exec_out_op_a = op_a;
	assign exec_out_op_b = op_b;
	assign exec_out_op_imm = op_imm;
	assign exec_out_is_load = is_ld;
	assign exec_out_is_store = is_st;
	assign exec_out_do_bse = bse;
	assign exec_out_do_wse = wse;
	assign exec_out_do_bze = bze;
	assign exec_out_do_wze = wze;
	assign rf_read1_valid = read1_needed;
	assign rf_read2_valid = read2_needed;
	assign rf_rsv_valid = rsv_needed;
	assign exec_out_result_reg_addr_valid = rsv_needed;
	assign u22_output_port = fetch_inst_0[3:0];
	assign exec_out_mem_access_len = mem_len_1;
endmodule


////////////////////////////////////////////////////////////////////////////////
// ForwardBuf
////////////////////////////////////////////////////////////////////////////////
module ForwardBuf (
	input logic input_port_do_bse,
	input logic input_port_do_bze,
	input logic input_port_do_wse,
	input logic input_port_do_wze,
	input logic [2:0] input_port_exec_unit,
	input logic input_port_fetch_av,
	input logic [1:0] input_port_inst_len,
	input logic input_port_is_load,
	input logic input_port_is_store,
	input logic [1:0] input_port_mem_access_len,
	input logic [31:0] input_port_op_a,
	input logic [31:0] input_port_op_b,
	input logic [31:0] input_port_op_imm,
	input logic [2:0] input_port_opcode,
	output logic input_port_ready,
	input logic [3:0] input_port_result_reg_addr,
	input logic input_port_result_reg_addr_valid,
	input logic input_port_valid,

	output logic output_port_do_bse,
	output logic output_port_do_bze,
	output logic output_port_do_wse,
	output logic output_port_do_wze,
	output logic [2:0] output_port_exec_unit,
	output logic output_port_fetch_av,
	output logic [1:0] output_port_inst_len,
	output logic output_port_is_load,
	output logic output_port_is_store,
	output logic [1:0] output_port_mem_access_len,
	output logic [31:0] output_port_op_a,
	output logic [31:0] output_port_op_b,
	output logic [31:0] output_port_op_imm,
	output logic [2:0] output_port_opcode,
	input logic output_port_ready,
	output logic [3:0] output_port_result_reg_addr,
	output logic output_port_result_reg_addr_valid,
	output logic output_port_valid,

	input logic clock_port,
	input logic reset_port
);

	logic buf_data_do_bse;
	logic buf_data_do_bze;
	logic buf_data_do_wse;
	logic buf_data_do_wze;
	logic [2:0] buf_data_exec_unit;
	logic buf_data_fetch_av;
	logic [1:0] buf_data_inst_len;
	logic buf_data_is_load;
	logic buf_data_is_store;
	logic [1:0] buf_data_mem_access_len;
	logic [31:0] buf_data_op_a;
	logic [31:0] buf_data_op_b;
	logic [31:0] buf_data_op_imm;
	logic [2:0] buf_data_opcode;
	logic [3:0] buf_data_result_reg_addr;
	logic buf_data_result_reg_addr_valid;
	logic fsm_out_reg_en;

	always_ff @(posedge clock_port) buf_data_do_bse <= reset_port ? 1'h0 : fsm_out_reg_en ? input_port_do_bse : buf_data_do_bse;
	always_ff @(posedge clock_port) buf_data_do_bze <= reset_port ? 1'h0 : fsm_out_reg_en ? input_port_do_bze : buf_data_do_bze;
	always_ff @(posedge clock_port) buf_data_do_wse <= reset_port ? 1'h0 : fsm_out_reg_en ? input_port_do_wse : buf_data_do_wse;
	always_ff @(posedge clock_port) buf_data_do_wze <= reset_port ? 1'h0 : fsm_out_reg_en ? input_port_do_wze : buf_data_do_wze;
	always_ff @(posedge clock_port) buf_data_exec_unit <= reset_port ? 3'h0 : fsm_out_reg_en ? input_port_exec_unit : buf_data_exec_unit;
	always_ff @(posedge clock_port) buf_data_fetch_av <= reset_port ? 1'h0 : fsm_out_reg_en ? input_port_fetch_av : buf_data_fetch_av;
	always_ff @(posedge clock_port) buf_data_inst_len <= reset_port ? 2'h0 : fsm_out_reg_en ? input_port_inst_len : buf_data_inst_len;
	always_ff @(posedge clock_port) buf_data_is_load <= reset_port ? 1'h0 : fsm_out_reg_en ? input_port_is_load : buf_data_is_load;
	always_ff @(posedge clock_port) buf_data_is_store <= reset_port ? 1'h0 : fsm_out_reg_en ? input_port_is_store : buf_data_is_store;
	always_ff @(posedge clock_port) buf_data_mem_access_len <= reset_port ? 2'h0 : fsm_out_reg_en ? input_port_mem_access_len : buf_data_mem_access_len;
	always_ff @(posedge clock_port) buf_data_op_a <= reset_port ? 32'h0 : fsm_out_reg_en ? input_port_op_a : buf_data_op_a;
	always_ff @(posedge clock_port) buf_data_op_b <= reset_port ? 32'h0 : fsm_out_reg_en ? input_port_op_b : buf_data_op_b;
	always_ff @(posedge clock_port) buf_data_op_imm <= reset_port ? 32'h0 : fsm_out_reg_en ? input_port_op_imm : buf_data_op_imm;
	always_ff @(posedge clock_port) buf_data_opcode <= reset_port ? 3'h0 : fsm_out_reg_en ? input_port_opcode : buf_data_opcode;
	always_ff @(posedge clock_port) buf_data_result_reg_addr <= reset_port ? 4'h0 : fsm_out_reg_en ? input_port_result_reg_addr : buf_data_result_reg_addr;
	always_ff @(posedge clock_port) buf_data_result_reg_addr_valid <= reset_port ? 1'h0 : fsm_out_reg_en ? input_port_result_reg_addr_valid : buf_data_result_reg_addr_valid;

	ForwardBufLogic fsm (
		.clock_port(clock_port),
		.reset_port(reset_port),
		.input_valid(input_port_valid),
		.input_ready(input_port_ready),
		.output_valid(output_port_valid),
		.output_ready(output_port_ready),
		.out_reg_en(fsm_out_reg_en)
	);

	assign output_port_do_bse = buf_data_do_bse;
	assign output_port_do_bze = buf_data_do_bze;
	assign output_port_do_wse = buf_data_do_wse;
	assign output_port_do_wze = buf_data_do_wze;
	assign output_port_exec_unit = buf_data_exec_unit;
	assign output_port_fetch_av = buf_data_fetch_av;
	assign output_port_inst_len = buf_data_inst_len;
	assign output_port_is_load = buf_data_is_load;
	assign output_port_is_store = buf_data_is_store;
	assign output_port_mem_access_len = buf_data_mem_access_len;
	assign output_port_op_a = buf_data_op_a;
	assign output_port_op_b = buf_data_op_b;
	assign output_port_op_imm = buf_data_op_imm;
	assign output_port_opcode = buf_data_opcode;
	assign output_port_result_reg_addr = buf_data_result_reg_addr;
	assign output_port_result_reg_addr_valid = buf_data_result_reg_addr_valid;
endmodule


////////////////////////////////////////////////////////////////////////////////
// ForwardBufLogic_2
////////////////////////////////////////////////////////////////////////////////
module ForwardBufLogic_2 (
	input logic clock_port,
	input logic reset_port,
	input logic input_valid,
	output logic input_ready,
	output logic output_valid,
	input logic output_ready,
	output logic out_reg_en
);

	logic buf_valid;

	assign out_reg_en = input_valid & input_ready;
	always_ff @(posedge clock_port) buf_valid <= reset_port ? 1'h0 : (input_valid & input_ready) ? 1'h1 : (output_ready & buf_valid) ? 1'h0 : buf_valid;
	assign input_ready =  ~ buf_valid | output_ready;

	assign output_valid = buf_valid;
endmodule


////////////////////////////////////////////////////////////////////////////////
// ForwardBufLogic
////////////////////////////////////////////////////////////////////////////////
module ForwardBufLogic (
	input logic clock_port,
	input logic reset_port,
	input logic input_valid,
	output logic input_ready,
	output logic output_valid,
	input logic output_ready,
	output logic out_reg_en
);

	logic buf_valid;

	assign out_reg_en = input_valid & input_ready;
	always_ff @(posedge clock_port) buf_valid <= reset_port ? 1'h0 : (input_valid & input_ready) ? 1'h1 : (output_ready & buf_valid) ? 1'h0 : buf_valid;
	assign input_ready =  ~ buf_valid | output_ready;

	assign output_valid = buf_valid;
endmodule


////////////////////////////////////////////////////////////////////////////////
// FetchStage
////////////////////////////////////////////////////////////////////////////////
module FetchStage (
	input logic clk,
	input logic rst,
	output logic [30:0] bus_if_addr,
	output logic [1:0] bus_if_burst_len,
	output logic [1:0] bus_if_byte_en,
	output logic [15:0] bus_if_data_in,
	input logic [15:0] bus_if_data_out,
	input logic bus_if_last,
	output logic bus_if_read_not_write,
	output logic bus_if_request,
	input logic bus_if_response,

	output logic decode_av,
	output logic [15:0] decode_inst_0,
	output logic [15:0] decode_inst_1,
	output logic [15:0] decode_inst_2,
	output logic [1:0] decode_inst_len,
	input logic decode_ready,
	output logic decode_valid,

	input logic [21:0] mem_base,
	input logic [21:0] mem_limit,
	input logic [30:0] spc,
	input logic [30:0] tpc,
	input logic task_mode,
	input logic do_branch
);

	logic inst_buf_queue_av;
	logic inst_buf_queue_valid;
	logic [15:0] inst_buf_queue_data;
	logic [2:0] inst_queue_queue_free_cnt;
	logic inst_buf_queue_ready;
	logic inst_queue_assemble_valid;
	logic inst_queue_assemble_av;
	logic [15:0] inst_queue_assemble_data;
	logic inst_queue_assemble_ready;

	InstBuffer inst_buf (
		.clk(clk),
		.rst(rst),
		.bus_if_addr(bus_if_addr),
		.bus_if_burst_len(bus_if_burst_len),
		.bus_if_byte_en(bus_if_byte_en),
		.bus_if_data_in(bus_if_data_in),
		.bus_if_data_out(bus_if_data_out),
		.bus_if_last(bus_if_last),
		.bus_if_read_not_write(bus_if_read_not_write),
		.bus_if_request(bus_if_request),
		.bus_if_response(bus_if_response),

		.queue_av(inst_buf_queue_av),
		.queue_data(inst_buf_queue_data),
		.queue_ready(inst_buf_queue_ready),
		.queue_valid(inst_buf_queue_valid),

		.queue_free_cnt(inst_queue_queue_free_cnt),
		.mem_base(mem_base),
		.mem_limit(mem_limit),
		.spc(spc),
		.tpc(tpc),
		.task_mode(task_mode),
		.do_branch(do_branch)
	);

	InstQueue inst_queue (
		.clk(clk),
		.rst(rst),
		.inst_av(inst_buf_queue_av),
		.inst_data(inst_buf_queue_data),
		.inst_ready(inst_buf_queue_ready),
		.inst_valid(inst_buf_queue_valid),

		.queue_free_cnt(inst_queue_queue_free_cnt),
		.assemble_av(inst_queue_assemble_av),
		.assemble_data(inst_queue_assemble_data),
		.assemble_ready(inst_queue_assemble_ready),
		.assemble_valid(inst_queue_assemble_valid),

		.do_branch(do_branch)
	);

	InstAssemble inst_assemble (
		.clk(clk),
		.rst(rst),
		.inst_buf_av(inst_queue_assemble_av),
		.inst_buf_data(inst_queue_assemble_data),
		.inst_buf_ready(inst_queue_assemble_ready),
		.inst_buf_valid(inst_queue_assemble_valid),

		.decode_av(decode_av),
		.decode_inst_0(decode_inst_0),
		.decode_inst_1(decode_inst_1),
		.decode_inst_2(decode_inst_2),
		.decode_inst_len(decode_inst_len),
		.decode_ready(decode_ready),
		.decode_valid(decode_valid),

		.do_branch(do_branch)
	);

endmodule


////////////////////////////////////////////////////////////////////////////////
// InstAssemble
////////////////////////////////////////////////////////////////////////////////
module InstAssemble (
	input logic clk,
	input logic rst,
	input logic inst_buf_av,
	input logic [15:0] inst_buf_data,
	output logic inst_buf_ready,
	input logic inst_buf_valid,

	output logic decode_av,
	output logic [15:0] decode_inst_0,
	output logic [15:0] decode_inst_1,
	output logic [15:0] decode_inst_2,
	output logic [1:0] decode_inst_len,
	input logic decode_ready,
	output logic decode_valid,

	input logic do_branch
);

	logic u7_output_port;
	logic u12_output_port;
	logic u17_output_port;
	logic u20_output_port;
	logic u22_output_port;
	logic u25_output_port;
	logic u27_output_port;
	logic u30_output_port;
	logic u36_output_port;
	logic u42_output_port;
	logic u48_output_port;
	logic u52_output_port;
	logic u55_output_port;
	logic terminal_fsm_state;
	logic fetch_ready;
	logic fsm_advance;
	logic fetch_av;
	logic [15:0] inst_reg_0;
	logic [15:0] inst_reg_1;
	logic [15:0] inst_reg_2;
	logic [1:0] inst_len_reg;
	logic [1:0] inst_len;
	logic [1:0] fsm_state;
	logic [1:0] decode_fsm_next_state;

	assign terminal_fsm_state = fsm_state == `InstAssembleStates__have_all_fragments;
	assign fetch_ready =  ~ terminal_fsm_state | decode_ready | do_branch;
	assign decode_valid = terminal_fsm_state &  ~ do_branch;
	assign fsm_advance =  ~ terminal_fsm_state & inst_buf_valid & fetch_ready | terminal_fsm_state & decode_ready;
	always_ff @(posedge clk) fetch_av <= rst ? 1'h0 : fsm_advance ? (fsm_state == `InstAssembleStates__have_0_fragments | fsm_state == `InstAssembleStates__have_all_fragments) ? inst_buf_av : fetch_av : fetch_av;
	always_ff @(posedge clk) inst_reg_0 <= rst ? 16'h0 : fsm_advance ? (fsm_state == `InstAssembleStates__have_0_fragments | fsm_state == `InstAssembleStates__have_all_fragments) ? inst_buf_data : inst_reg_0 : inst_reg_0;
	always_ff @(posedge clk) inst_reg_1 <= rst ? 16'h0 : fsm_advance ? (fsm_state == `InstAssembleStates__need_1_fragments & inst_len_reg == 1'h1 | fsm_state == `InstAssembleStates__need_2_fragments) ? inst_buf_data : inst_reg_1 : inst_reg_1;
	always_ff @(posedge clk) inst_reg_2 <= rst ? 16'h0 : fsm_advance ? inst_buf_data : inst_reg_2;
	always_ff @(posedge clk) inst_len_reg <= rst ? 2'h0 : fsm_advance ? (fsm_state == `InstAssembleStates__have_0_fragments | fsm_state == `InstAssembleStates__have_all_fragments) ? inst_len : inst_len_reg : inst_len_reg;

	FSM_3 decode_fsm (
		.clock_port(clk),
		.reset_port(rst),
		.reset_value(`InstAssembleStates__have_0_fragments),
		.state(fsm_state),
		.next_state(decode_fsm_next_state),
		.default_state(`InstAssembleStates__have_0_fragments),
		.input_have_0_fragments_to_have_all_fragments(u7_output_port),
		.input_have_0_fragments_to_need_1_fragments(u12_output_port),
		.input_have_0_fragments_to_need_2_fragments(u17_output_port),
		.input_have_0_fragments_to_have_0_fragments(u20_output_port),
		.input_have_0_fragments_to_have_0_fragments_1(do_branch),
		.input_need_1_fragments_to_have_all_fragments(u22_output_port),
		.input_need_1_fragments_to_need_1_fragments(u25_output_port),
		.input_need_1_fragments_to_have_0_fragments(do_branch),
		.input_need_2_fragments_to_need_1_fragments(u27_output_port),
		.input_need_2_fragments_to_need_2_fragments(u30_output_port),
		.input_need_2_fragments_to_have_0_fragments(do_branch),
		.input_have_all_fragments_to_have_all_fragments(u36_output_port),
		.input_have_all_fragments_to_need_1_fragments(u42_output_port),
		.input_have_all_fragments_to_need_2_fragments(u48_output_port),
		.input_have_all_fragments_to_have_0_fragments(u52_output_port),
		.input_have_all_fragments_to_have_all_fragments_1(u55_output_port),
		.input_have_all_fragments_to_have_0_fragments_1(do_branch)
	);

	DecoratorModule_5 u (
		.output_port(inst_len),
		.inst_word(inst_buf_data)
	);

	assign u7_output_port =  ~ do_branch & fsm_advance & inst_len == 1'h0;
	assign u12_output_port =  ~ do_branch & fsm_advance & inst_len == 1'h1;
	assign u17_output_port =  ~ do_branch & fsm_advance & inst_len == 2'h2;
	assign u20_output_port =  ~ do_branch &  ~ fsm_advance;
	assign u22_output_port =  ~ do_branch & fsm_advance;
	assign u25_output_port =  ~ do_branch &  ~ fsm_advance;
	assign u27_output_port =  ~ do_branch & fsm_advance;
	assign u30_output_port =  ~ do_branch &  ~ fsm_advance;
	assign u36_output_port =  ~ do_branch & fsm_advance & inst_buf_valid & inst_len == 1'h0;
	assign u42_output_port =  ~ do_branch & fsm_advance & inst_buf_valid & inst_len == 1'h1;
	assign u48_output_port =  ~ do_branch & fsm_advance & inst_buf_valid & inst_len == 2'h2;
	assign u52_output_port =  ~ do_branch & fsm_advance &  ~ inst_buf_valid;
	assign u55_output_port =  ~ do_branch &  ~ fsm_advance;
	assign inst_buf_ready = fetch_ready;
	assign decode_av = fetch_av;
	assign decode_inst_0 = inst_reg_0;
	assign decode_inst_1 = inst_reg_1;
	assign decode_inst_2 = inst_reg_2;
	assign decode_inst_len = inst_len_reg;
endmodule


////////////////////////////////////////////////////////////////////////////////
// DecoratorModule_7
////////////////////////////////////////////////////////////////////////////////
module DecoratorModule_7 (
	output logic output_port,
	input logic [31:0] word,
	input logic [31:0] bit_code
);

	assign output_port = bit_code == 1'h0 ? word[0] : 1'b0 | bit_code == 1'h1 ? word[1] : 1'b0 | bit_code == 2'h2 ? word[2] : 1'b0 | bit_code == 2'h3 ? word[3] : 1'b0 | bit_code == 3'h4 ? word[4] : 1'b0 | bit_code == 3'h5 ? word[5] : 1'b0 | bit_code == 3'h6 ? word[6] : 1'b0 | bit_code == 3'h7 ? word[7] : 1'b0 | bit_code == 4'h8 ? word[8] : 1'b0 | bit_code == 4'h9 ? word[9] : 1'b0 | bit_code == 4'ha ? word[14] : 1'b0 | bit_code == 4'hb ? word[15] : 1'b0 | bit_code == 4'hc ? word[16] : 1'b0 | bit_code == 4'hd ? word[30] : 1'b0 | bit_code == 4'he ? word[31] : 1'b0 ;

endmodule


////////////////////////////////////////////////////////////////////////////////
// DecoratorModule_6
////////////////////////////////////////////////////////////////////////////////
module DecoratorModule_6 (
	output logic output_port,
	input logic [31:0] word,
	input logic [31:0] bit_code
);

	assign output_port = bit_code == 1'h0 ? word[0] : 1'b0 | bit_code == 1'h1 ? word[1] : 1'b0 | bit_code == 2'h2 ? word[2] : 1'b0 | bit_code == 2'h3 ? word[3] : 1'b0 | bit_code == 3'h4 ? word[4] : 1'b0 | bit_code == 3'h5 ? word[5] : 1'b0 | bit_code == 3'h6 ? word[6] : 1'b0 | bit_code == 3'h7 ? word[7] : 1'b0 | bit_code == 4'h8 ? word[8] : 1'b0 | bit_code == 4'h9 ? word[9] : 1'b0 | bit_code == 4'ha ? word[14] : 1'b0 | bit_code == 4'hb ? word[15] : 1'b0 | bit_code == 4'hc ? word[16] : 1'b0 | bit_code == 4'hd ? word[30] : 1'b0 | bit_code == 4'he ? word[31] : 1'b0 ;

endmodule


////////////////////////////////////////////////////////////////////////////////
// DecoratorModule_5
////////////////////////////////////////////////////////////////////////////////
module DecoratorModule_5 (
	output logic [1:0] output_port,
	input logic [15:0] inst_word
);

	logic [3:0] u_output_port;
	logic [3:0] u3_output_port;
	logic [3:0] u6_output_port;
	logic [3:0] u9_output_port;
	logic [3:0] u15_output_port;
	logic [3:0] u18_output_port;
	logic [3:0] u23_output_port;
	logic [3:0] u26_output_port;
	logic [3:0] u29_output_port;
	logic [3:0] u35_output_port;
	logic [3:0] u38_output_port;

	assign output_port = {(u_output_port == 4'hf | u3_output_port == 4'hf & (u6_output_port != 4'hf | u9_output_port == 4'hf) | u15_output_port == 4'he & u18_output_port == 4'hf | u23_output_port < 4'hc & (u26_output_port == 4'hf | u29_output_port == 4'hf)) &  ~ (u35_output_port == 4'hf | u38_output_port != 4'hf), (u_output_port == 4'hf | u3_output_port == 4'hf & (u6_output_port != 4'hf | u9_output_port == 4'hf) | u15_output_port == 4'he & u18_output_port == 4'hf | u23_output_port < 4'hc & (u26_output_port == 4'hf | u29_output_port == 4'hf)) & (u35_output_port == 4'hf | u38_output_port != 4'hf)};

	DecoratorModule u (
		.output_port(u_output_port),
		.inst_word(inst_word)
	);

	DecoratorModule_2 u3 (
		.output_port(u3_output_port),
		.inst_word(inst_word)
	);

	DecoratorModule_3 u6 (
		.output_port(u6_output_port),
		.inst_word(inst_word)
	);

	DecoratorModule_4 u9 (
		.output_port(u9_output_port),
		.inst_word(inst_word)
	);

	DecoratorModule_2 u15 (
		.output_port(u15_output_port),
		.inst_word(inst_word)
	);

	DecoratorModule_4 u18 (
		.output_port(u18_output_port),
		.inst_word(inst_word)
	);

	DecoratorModule_2 u23 (
		.output_port(u23_output_port),
		.inst_word(inst_word)
	);

	DecoratorModule_3 u26 (
		.output_port(u26_output_port),
		.inst_word(inst_word)
	);

	DecoratorModule_4 u29 (
		.output_port(u29_output_port),
		.inst_word(inst_word)
	);

	DecoratorModule u35 (
		.output_port(u35_output_port),
		.inst_word(inst_word)
	);

	DecoratorModule_4 u38 (
		.output_port(u38_output_port),
		.inst_word(inst_word)
	);

endmodule


////////////////////////////////////////////////////////////////////////////////
// DecoratorModule_4
////////////////////////////////////////////////////////////////////////////////
module DecoratorModule_4 (
	output logic [3:0] output_port,
	input logic [15:0] inst_word
);

	assign output_port = inst_word[3:0];

endmodule


////////////////////////////////////////////////////////////////////////////////
// DecoratorModule_3
////////////////////////////////////////////////////////////////////////////////
module DecoratorModule_3 (
	output logic [3:0] output_port,
	input logic [15:0] inst_word
);

	assign output_port = inst_word[7:4];

endmodule


////////////////////////////////////////////////////////////////////////////////
// DecoratorModule_2
////////////////////////////////////////////////////////////////////////////////
module DecoratorModule_2 (
	output logic [3:0] output_port,
	input logic [15:0] inst_word
);

	assign output_port = inst_word[11:8];

endmodule


////////////////////////////////////////////////////////////////////////////////
// DecoratorModule
////////////////////////////////////////////////////////////////////////////////
module DecoratorModule (
	output logic [3:0] output_port,
	input logic [15:0] inst_word
);

	assign output_port = inst_word[15:12];

endmodule


////////////////////////////////////////////////////////////////////////////////
// InstQueue
////////////////////////////////////////////////////////////////////////////////
module InstQueue (
	input logic clk,
	input logic rst,
	input logic inst_av,
	input logic [15:0] inst_data,
	output logic inst_ready,
	input logic inst_valid,

	output logic [2:0] queue_free_cnt,
	output logic assemble_av,
	output logic [15:0] assemble_data,
	input logic assemble_ready,
	output logic assemble_valid,

	input logic do_branch
);

	logic dec;
	logic inc;
	logic signed [4:0] u4_output_port;
	logic [2:0] empty_cnt;

	assign inc = assemble_ready & assemble_valid;
	assign dec = inst_ready & inst_valid;
	always_ff @(posedge clk) empty_cnt <= rst ? 3'h7 : do_branch ? 3'h7 : u4_output_port[2:0];

	Fifo u (
		.input_port_av(inst_av),
		.input_port_data(inst_data),
		.input_port_ready(inst_ready),
		.input_port_valid(inst_valid),

		.output_port_av(assemble_av),
		.output_port_data(assemble_data),
		.output_port_ready(assemble_ready),
		.output_port_valid(assemble_valid),

		.clock_port(clk),
		.reset_port(rst),
		.clear(do_branch)
	);

	assign u4_output_port = empty_cnt + inc - dec;
	assign queue_free_cnt = empty_cnt;
endmodule


////////////////////////////////////////////////////////////////////////////////
// Fifo
////////////////////////////////////////////////////////////////////////////////
module Fifo (
	input logic input_port_av,
	input logic [15:0] input_port_data,
	output logic input_port_ready,
	input logic input_port_valid,

	output logic output_port_av,
	output logic [15:0] output_port_data,
	input logic output_port_ready,
	output logic output_port_valid,

	input logic clock_port,
	input logic reset_port,
	input logic clear
);

	logic input_data_av;
	logic [15:0] input_data_data;
	logic push;
	logic pop;
	logic push_will_wrap;
	logic pop_will_wrap;
	logic [2:0] next_push_addr;
	logic [2:0] next_pop_addr;
	logic next_looped;
	logic next_empty_or_full;
	logic next_empty;
	logic next_full;
	logic [2:0] push_addr;
	logic [2:0] pop_addr;
	logic empty;
	logic full;
	logic looped;
	logic u93_output_port_av;
	logic [15:0] u93_output_port_data;
	logic output_data_av;
	logic [15:0] output_data_data;
	logic buffer_mem_port2_data_out_av;
	logic [15:0] buffer_mem_port2_data_out_data;

	assign input_port_ready =  ~ full;
	assign output_port_valid =  ~ empty;
	assign push_will_wrap = push_addr == 3'h6;
	assign push =  ~ full & input_port_valid;
	assign next_push_addr = push ? push_will_wrap ? 1'h0 : push_addr + 1'h1 : push_addr;
	assign pop_will_wrap = pop_addr == 3'h6;
	assign pop =  ~ empty & output_port_ready;
	assign next_pop_addr = pop ? pop_will_wrap ? 1'h0 : pop_addr + 1'h1 : pop_addr;
	assign next_looped = push != 1'h1 & pop != 1'h1 ? looped : 1'b0 | push == 1'h1 & pop != 1'h1 ? push_will_wrap ? 1'h1 : looped : 1'b0 | push != 1'h1 & pop == 1'h1 ? pop_will_wrap ? 1'h0 : looped : 1'b0 | push == 1'h1 & pop == 1'h1 ? push_will_wrap != 1'h1 & pop_will_wrap != 1'h1 ? looped : 1'b0 | push_will_wrap == 1'h1 & pop_will_wrap != 1'h1 ? 1'h1 : 1'b0 | push_will_wrap != 1'h1 & pop_will_wrap == 1'h1 ? 1'h0 : 1'b0 | push_will_wrap == 1'h1 & pop_will_wrap == 1'h1 ? looped : 1'b0  : 1'b0 ;
	assign next_empty_or_full = next_push_addr == next_pop_addr;
	assign next_empty = next_empty_or_full ?  ~ next_looped : 1'h0;
	assign next_full = next_empty_or_full ? next_looped : 1'h0;
	always_ff @(posedge clock_port) push_addr <= reset_port ? 3'h0 : clear ? 1'h0 : next_push_addr;
	always_ff @(posedge clock_port) pop_addr <= reset_port ? 3'h0 : clear ? 1'h0 : next_pop_addr;
	always_ff @(posedge clock_port) empty <= reset_port ? 1'h1 : clear ? 1'h1 : next_empty;
	always_ff @(posedge clock_port) full <= reset_port ? 1'h0 : clear ? 1'h0 : next_full;
	always_ff @(posedge clock_port) looped <= reset_port ? 1'h0 : clear ? 1'h0 : next_looped;
	always_ff @(posedge clock_port) u93_output_port_av <= reset_port ? 1'h0 : input_port_av;
	always_ff @(posedge clock_port) u93_output_port_data <= reset_port ? 16'h0 : input_port_data;
	assign output_data_av = push_addr == next_pop_addr ? u93_output_port_av : buffer_mem_port2_data_out_av;
	assign output_data_data = push_addr == next_pop_addr ? u93_output_port_data : buffer_mem_port2_data_out_data;

	Memory buffer_mem (
		.port1_addr(push_addr),
		.port1_clk(clock_port),
		.port2_addr(next_pop_addr),
		.port2_clk(clock_port),
		.port1_data_in_av(input_port_av),
		.port1_data_in_data(input_port_data),

		.port1_write_en(push),
		.port2_data_out_av(buffer_mem_port2_data_out_av),
		.port2_data_out_data(buffer_mem_port2_data_out_data)
	);

	assign input_data_av = input_port_av;
	assign input_data_data = input_port_data;
	assign output_port_av = output_data_av;
	assign output_port_data = output_data_data;
endmodule


////////////////////////////////////////////////////////////////////////////////
// Memory
////////////////////////////////////////////////////////////////////////////////
module Memory (
	input logic [2:0] port1_addr,
	input logic port1_clk,
	input logic [2:0] port2_addr,
	input logic port2_clk,
	input logic port1_data_in_av,
	input logic [15:0] port1_data_in_data,

	input logic port1_write_en,
	output logic port2_data_out_av,
	output logic [15:0] port2_data_out_data
);

	logic [16:0] real_mem_port2_data_out;

	reg [16:0] mem[0:7];

	always @(posedge port1_clk) begin
		if (port1_write_en) begin
			mem[port1_addr] <= {port1_data_in_av, port1_data_in_data};
		end
	end

	logic [2:0] port2_addr_reg;
	always @(posedge port1_clk) begin
		port2_addr_reg <= port2_addr;
	end
	assign real_mem_port2_data_out = mem[port2_addr_reg];

	assign {port2_data_out_av, port2_data_out_data} = real_mem_port2_data_out;
endmodule


////////////////////////////////////////////////////////////////////////////////
// InstBuffer
////////////////////////////////////////////////////////////////////////////////
module InstBuffer (
	input logic clk,
	input logic rst,
	output logic [30:0] bus_if_addr,
	output logic [1:0] bus_if_burst_len,
	output logic [1:0] bus_if_byte_en,
	output logic [15:0] bus_if_data_in,
	input logic [15:0] bus_if_data_out,
	input logic bus_if_last,
	output logic bus_if_read_not_write,
	output logic bus_if_request,
	input logic bus_if_response,

	output logic queue_av,
	output logic [15:0] queue_data,
	input logic queue_ready,
	output logic queue_valid,

	input logic [2:0] queue_free_cnt,
	input logic [21:0] mem_base,
	input logic [21:0] mem_limit,
	input logic [30:0] spc,
	input logic [30:0] tpc,
	input logic task_mode,
	input logic do_branch
);

	logic [32:0] u3_output_port;
	logic [31:0] u5_output_port;
	logic [30:0] u8_output_port;
	logic [30:0] fetch_addr;
	logic fetch_av;
	logic start_new_request;
	logic u17_output_port;
	logic u20_output_port;
	logic u24_output_port;
	logic u27_output_port;
	logic u29_output_port;
	logic u32_output_port;
	logic u34_output_port;
	logic u37_output_port;
	logic u41_output_port;
	logic u44_output_port;
	logic u46_output_port;
	logic u49_output_port;
	logic u51_output_port;
	logic u52_output_port;
	logic u53_output_port;
	logic u54_output_port;
	logic u56_output_port;
	logic u57_output_port;
	logic fetch_increment;
	logic response_d;
	logic [30:0] branch_target;
	logic [2:0] state;
	logic [2:0] fsm_next_state;

	assign branch_target = u3_output_port[30:0];
	always_ff @(posedge clk) u8_output_port <= rst ? 31'h0 : do_branch ? branch_target : u5_output_port[30:0];
	assign fetch_addr = do_branch ? branch_target : u8_output_port;
	assign start_new_request = queue_free_cnt >= 3'h4 | do_branch;
	assign bus_if_read_not_write = 1'h1;
	assign bus_if_burst_len = 2'h3 - fetch_addr[1:0];
	assign bus_if_byte_en = 2'h3;
	assign bus_if_data_in = $signed(1'bX);
	assign bus_if_request = state == `InstBufferStates__request_start | state == `InstBufferStates__flush_start;
	assign fetch_increment = bus_if_response & (state == `InstBufferStates__request_start | state == `InstBufferStates__request);
	always_ff @(posedge clk) response_d <= rst ? 1'h0 : bus_if_response & (state == `InstBufferStates__request_start | state == `InstBufferStates__request) &  ~ do_branch;
	assign fetch_av = fetch_addr[30:10] > mem_limit;
	assign queue_data = bus_if_data_out;

	FSM_2 fsm (
		.clock_port(clk),
		.reset_port(rst),
		.reset_value(`InstBufferStates__idle),
		.state(state),
		.next_state(fsm_next_state),
		.default_state(`InstBufferStates__idle),
		.input_idle_to_idle(u17_output_port),
		.input_idle_to_request_start(start_new_request),
		.input_request_start_to_request_start(u20_output_port),
		.input_request_start_to_request(u24_output_port),
		.input_request_start_to_request_last(u27_output_port),
		.input_request_start_to_flush_start(u29_output_port),
		.input_request_start_to_flush(u32_output_port),
		.input_request_start_to_idle(u34_output_port),
		.input_request_to_request(u37_output_port),
		.input_request_to_request_1(u41_output_port),
		.input_request_to_request_last(u44_output_port),
		.input_request_to_flush(u46_output_port),
		.input_request_to_flush_1(u49_output_port),
		.input_request_to_idle(u51_output_port),
		.input_request_last_to_idle(u52_output_port),
		.input_request_last_to_request_start(start_new_request),
		.input_flush_start_to_flush_start(u53_output_port),
		.input_flush_start_to_flush(bus_if_response),
		.input_flush_to_flush(u54_output_port),
		.input_flush_to_flush_1(u56_output_port),
		.input_flush_to_idle(u57_output_port)
	);

	assign u3_output_port = (task_mode ? tpc : spc) + (mem_base << 4'ha);
	assign u5_output_port = fetch_addr + fetch_increment;
	assign bus_if_addr = fetch_addr;
	assign queue_av = fetch_av;
	assign u17_output_port =  ~ start_new_request;
	assign u20_output_port =  ~ do_branch &  ~ bus_if_response;
	assign u24_output_port =  ~ do_branch & bus_if_response &  ~ bus_if_last;
	assign u27_output_port =  ~ do_branch & bus_if_response & bus_if_last;
	assign u29_output_port = do_branch &  ~ bus_if_response;
	assign u32_output_port = do_branch & bus_if_response &  ~ bus_if_last;
	assign u34_output_port = do_branch & bus_if_response & bus_if_last;
	assign u37_output_port =  ~ do_branch &  ~ bus_if_response;
	assign u41_output_port =  ~ do_branch & bus_if_response &  ~ bus_if_last;
	assign u44_output_port =  ~ do_branch & bus_if_response & bus_if_last;
	assign u46_output_port = do_branch &  ~ bus_if_response;
	assign u49_output_port = do_branch & bus_if_response &  ~ bus_if_last;
	assign u51_output_port = do_branch & bus_if_response & bus_if_last;
	assign u52_output_port =  ~ start_new_request;
	assign u53_output_port =  ~ bus_if_response;
	assign u54_output_port =  ~ bus_if_response;
	assign u56_output_port = bus_if_response &  ~ bus_if_last;
	assign u57_output_port = bus_if_response & bus_if_last;
	assign queue_valid = response_d;
endmodule


////////////////////////////////////////////////////////////////////////////////
// BusIf
////////////////////////////////////////////////////////////////////////////////
module BusIf (
	input logic clk,
	input logic rst,
	input logic [30:0] fetch_addr,
	input logic [1:0] fetch_burst_len,
	input logic [1:0] fetch_byte_en,
	input logic [15:0] fetch_data_in,
	output logic [15:0] fetch_data_out,
	output logic fetch_last,
	input logic fetch_read_not_write,
	input logic fetch_request,
	output logic fetch_response,

	input logic [30:0] mem_addr,
	input logic [1:0] mem_burst_len,
	input logic [1:0] mem_byte_en,
	input logic [15:0] mem_data_in,
	output logic [15:0] mem_data_out,
	output logic mem_last,
	input logic mem_read_not_write,
	input logic mem_request,
	output logic mem_response,

	output logic DRAM_nRAS,
	output logic DRAM_nCAS_l,
	output logic DRAM_nCAS_h,
	output logic [11:0] DRAM_ADDR,
	output logic DRAM_nWE,
	input logic [7:0] DRAM_DATA_rd_h,
	input logic [7:0] DRAM_DATA_rd_l,
	output logic [15:0] DRAM_DATA_wr,
	input logic ext_req,
	output logic ext_grnt,
	input logic [3:0] wait_states_0,
	input logic [3:0] wait_states_1,
	input logic [3:0] wait_states_2,
	input logic [3:0] wait_states_3
);

	logic u3_output_port;
	logic u7_output_port;
	logic u11_output_port;
	logic u16_output_port;
	logic u17_output_port;
	logic u18_output_port;
	logic u19_output_port;
	logic start;
	logic advance;
	logic signed [4:0] u40_output_port;
	logic signed [4:0] u49_output_port;
	logic [3:0] wait_states;
	logic read_not_write;
	logic [22:0] page_addr;
	logic [8:0] u62_output_port;
	logic [7:0] page_offs;
	logic signed [2:0] u76_output_port;
	logic [1:0] beats_remaining;
	logic [1:0] byte_en;
	logic [15:0] data_in;
	logic last;
	logic [9:0] row_addr;
	logic [9:0] col_addr;
	logic DRAM_nRASd;
	logic CAS_nEN;
	logic [15:0] data_out;
	logic [1:0] arb_state;
	logic [1:0] arb_next_state;

	assign ext_grnt = arb_state == `ArbStates__external;
	assign mem_response = arb_state == (`ArbStates__memory & wait_states == 1'h0);
	assign fetch_response = arb_state == (`ArbStates__fetch & wait_states == 1'h0);
	assign start = arb_state == `ArbStates__idle & (fetch_request | mem_request);
	always_ff @(posedge clk) wait_states <= rst ? 4'h0 : start ? u49_output_port[3:0] : wait_states == 1'h0 ? 1'h0 : u40_output_port[3:0];
	always_ff @(posedge clk) read_not_write <= rst ? 1'h0 : start ? fetch_request ? fetch_read_not_write : mem_read_not_write : read_not_write;
	always_ff @(posedge clk) page_addr <= rst ? 23'h0 : start ? fetch_request ? fetch_addr[30:8] : mem_addr[30:8] : page_addr;
	assign advance = arb_state != `ArbStates__idle;
	always_ff @(posedge clk) page_offs <= rst ? 8'h0 : start ? fetch_request ? fetch_addr[7:0] : mem_addr[7:0] : advance ? u62_output_port[7:0] : page_offs;
	always_ff @(posedge clk) beats_remaining <= rst ? 2'h0 : start ? fetch_request ? fetch_burst_len : mem_burst_len : advance ? u76_output_port[1:0] : beats_remaining;
	always_ff @(posedge clk) byte_en <= rst ? 2'h0 : start ? fetch_request ? fetch_byte_en : mem_byte_en : wait_states == 1'h0 ? 2'h3 : byte_en;
	always_ff @(posedge clk) data_in <= rst ? 16'h0 : arb_next_state == `ArbStates__fetch ? fetch_data_in : mem_data_in;
	assign last = beats_remaining == 1'h0 & arb_state != `ArbStates__idle;
	always_ff @(posedge clk) DRAM_nRAS <= rst ? 1'h1 : arb_next_state == `ArbStates__idle;
	always_ff @(negedge clk) DRAM_nRASd <= rst ? 1'h1 : DRAM_nRAS;
	assign CAS_nEN = DRAM_nRAS | DRAM_nRASd;
	assign DRAM_nCAS_h =  ~ byte_en[1] | CAS_nEN | clk;
	assign DRAM_nCAS_l =  ~ byte_en[0] | CAS_nEN | clk;
	assign col_addr = {page_addr[10], page_addr[8], page_offs[7:0]};
	assign row_addr = {page_addr[11], page_addr[9], page_addr[7:0]};
	always_ff @(negedge clk) DRAM_DATA_wr <= rst ? 16'h0 : data_in;
	always_ff @(posedge clk) data_out <= rst ? 16'h0 : ({DRAM_DATA_rd_h, DRAM_DATA_rd_l});
	assign fetch_last = last;
	assign fetch_data_out = data_out;
	assign DRAM_ADDR = clk ? row_addr : col_addr;

	FSM arb_fsm (
		.clock_port(clk),
		.reset_port(rst),
		.reset_value(`ArbStates__idle),
		.state(arb_state),
		.next_state(arb_next_state),
		.default_state(`ArbStates__idle),
		.input_idle_to_fetch(u3_output_port),
		.input_idle_to_memory(u7_output_port),
		.input_idle_to_external(u11_output_port),
		.input_idle_to_idle(u16_output_port),
		.input_fetch_to_fetch(u17_output_port),
		.input_fetch_to_idle(last),
		.input_memory_to_memory(u18_output_port),
		.input_memory_to_idle(last),
		.input_external_to_external(ext_req),
		.input_external_to_idle(u19_output_port)
	);

	assign u3_output_port = fetch_request &  ~ ext_req;
	assign u7_output_port =  ~ fetch_request & mem_request &  ~ ext_req;
	assign u11_output_port =  ~ fetch_request &  ~ mem_request & ext_req;
	assign u16_output_port =  ~ fetch_request &  ~ mem_request &  ~ ext_req;
	assign u17_output_port =  ~ last;
	assign u18_output_port =  ~ last;
	assign u19_output_port =  ~ ext_req;
	assign u40_output_port = wait_states - 1'h1;
	assign u49_output_port = ((fetch_request ? fetch_addr[30:29] : mem_addr[30:29]) == 0 ? wait_states_0 : 4'b0 | (fetch_request ? fetch_addr[30:29] : mem_addr[30:29]) == 1 ? wait_states_1 : 4'b0 | (fetch_request ? fetch_addr[30:29] : mem_addr[30:29]) == 2 ? wait_states_2 : 4'b0 | (fetch_request ? fetch_addr[30:29] : mem_addr[30:29]) == 3 ? wait_states_3 : 4'b0) - 1'h1;
	assign DRAM_nWE = read_not_write;
	assign u62_output_port = page_offs + 1'h1;
	assign u76_output_port = beats_remaining - (wait_states == 1'h0 ? 1'h1 : 1'h0);
	assign mem_last = last;
	assign mem_data_out = data_out;
endmodule


////////////////////////////////////////////////////////////////////////////////
// FSM_4
////////////////////////////////////////////////////////////////////////////////
module FSM_4 (
	input logic clock_port,
	input logic reset_port,
	input logic [3:0] reset_value,
	output logic [3:0] state,
	output logic [3:0] next_state,
	input logic [3:0] default_state,
	input logic input_idle_to_idle,
	input logic input_idle_to_read_1,
	input logic input_idle_to_write,
	input logic input_idle_to_csr_read,
	input logic input_idle_to_csr_write,
	input logic input_write_to_write,
	input logic input_write_to_idle,
	input logic input_read_1_to_read_1,
	input logic input_read_1_to_read_1_1,
	input logic input_read_1_to_read_2,
	input logic input_read_2_to_idle,
	input logic input_read_2_to_read_1,
	input logic input_read_2_to_write,
	input logic input_read_2_to_csr_read,
	input logic input_read_2_to_csr_write,
	input logic input_csr_read_to_csr_read,
	input logic input_csr_read_to_idle,
	input logic input_csr_read_to_read_1,
	input logic input_csr_read_to_write,
	input logic input_csr_read_to_csr_read_1,
	input logic input_csr_read_to_csr_write,
	input logic input_csr_write_to_csr_write,
	input logic input_csr_write_to_idle,
	input logic input_csr_write_to_read_1,
	input logic input_csr_write_to_write,
	input logic input_csr_write_to_csr_read,
	input logic input_csr_write_to_csr_write_1
);

	logic [3:0] local_state;
	logic [3:0] local_next_state;

	always_ff @(posedge clock_port) local_state <= reset_port ? reset_value : local_next_state;

	FSMLogic_4 fsm_logic (
		.state(local_state),
		.next_state(local_next_state),
		.default_state(default_state),
		.input_idle_to_idle(input_idle_to_idle),
		.input_idle_to_read_1(input_idle_to_read_1),
		.input_idle_to_write(input_idle_to_write),
		.input_idle_to_csr_read(input_idle_to_csr_read),
		.input_idle_to_csr_write(input_idle_to_csr_write),
		.input_write_to_write(input_write_to_write),
		.input_write_to_idle(input_write_to_idle),
		.input_read_1_to_read_1(input_read_1_to_read_1),
		.input_read_1_to_read_1_1(input_read_1_to_read_1_1),
		.input_read_1_to_read_2(input_read_1_to_read_2),
		.input_read_2_to_idle(input_read_2_to_idle),
		.input_read_2_to_read_1(input_read_2_to_read_1),
		.input_read_2_to_write(input_read_2_to_write),
		.input_read_2_to_csr_read(input_read_2_to_csr_read),
		.input_read_2_to_csr_write(input_read_2_to_csr_write),
		.input_csr_read_to_csr_read(input_csr_read_to_csr_read),
		.input_csr_read_to_idle(input_csr_read_to_idle),
		.input_csr_read_to_read_1(input_csr_read_to_read_1),
		.input_csr_read_to_write(input_csr_read_to_write),
		.input_csr_read_to_csr_read_1(input_csr_read_to_csr_read_1),
		.input_csr_read_to_csr_write(input_csr_read_to_csr_write),
		.input_csr_write_to_csr_write(input_csr_write_to_csr_write),
		.input_csr_write_to_idle(input_csr_write_to_idle),
		.input_csr_write_to_read_1(input_csr_write_to_read_1),
		.input_csr_write_to_write(input_csr_write_to_write),
		.input_csr_write_to_csr_read(input_csr_write_to_csr_read),
		.input_csr_write_to_csr_write_1(input_csr_write_to_csr_write_1)
	);

	assign state = local_state;
	assign next_state = local_next_state;
endmodule


////////////////////////////////////////////////////////////////////////////////
// FSM_3
////////////////////////////////////////////////////////////////////////////////
module FSM_3 (
	input logic clock_port,
	input logic reset_port,
	input logic [1:0] reset_value,
	output logic [1:0] state,
	output logic [1:0] next_state,
	input logic [1:0] default_state,
	input logic input_have_0_fragments_to_have_all_fragments,
	input logic input_have_0_fragments_to_need_1_fragments,
	input logic input_have_0_fragments_to_need_2_fragments,
	input logic input_have_0_fragments_to_have_0_fragments,
	input logic input_have_0_fragments_to_have_0_fragments_1,
	input logic input_need_1_fragments_to_have_all_fragments,
	input logic input_need_1_fragments_to_need_1_fragments,
	input logic input_need_1_fragments_to_have_0_fragments,
	input logic input_need_2_fragments_to_need_1_fragments,
	input logic input_need_2_fragments_to_need_2_fragments,
	input logic input_need_2_fragments_to_have_0_fragments,
	input logic input_have_all_fragments_to_have_all_fragments,
	input logic input_have_all_fragments_to_need_1_fragments,
	input logic input_have_all_fragments_to_need_2_fragments,
	input logic input_have_all_fragments_to_have_0_fragments,
	input logic input_have_all_fragments_to_have_all_fragments_1,
	input logic input_have_all_fragments_to_have_0_fragments_1
);

	logic [1:0] local_state;
	logic [1:0] local_next_state;

	always_ff @(posedge clock_port) local_state <= reset_port ? reset_value : local_next_state;

	FSMLogic_3 fsm_logic (
		.state(local_state),
		.next_state(local_next_state),
		.default_state(default_state),
		.input_have_0_fragments_to_have_all_fragments(input_have_0_fragments_to_have_all_fragments),
		.input_have_0_fragments_to_need_1_fragments(input_have_0_fragments_to_need_1_fragments),
		.input_have_0_fragments_to_need_2_fragments(input_have_0_fragments_to_need_2_fragments),
		.input_have_0_fragments_to_have_0_fragments(input_have_0_fragments_to_have_0_fragments),
		.input_have_0_fragments_to_have_0_fragments_1(input_have_0_fragments_to_have_0_fragments_1),
		.input_need_1_fragments_to_have_all_fragments(input_need_1_fragments_to_have_all_fragments),
		.input_need_1_fragments_to_need_1_fragments(input_need_1_fragments_to_need_1_fragments),
		.input_need_1_fragments_to_have_0_fragments(input_have_0_fragments_to_have_0_fragments_1),
		.input_need_2_fragments_to_need_1_fragments(input_need_2_fragments_to_need_1_fragments),
		.input_need_2_fragments_to_need_2_fragments(input_need_2_fragments_to_need_2_fragments),
		.input_need_2_fragments_to_have_0_fragments(input_have_0_fragments_to_have_0_fragments_1),
		.input_have_all_fragments_to_have_all_fragments(input_have_all_fragments_to_have_all_fragments),
		.input_have_all_fragments_to_need_1_fragments(input_have_all_fragments_to_need_1_fragments),
		.input_have_all_fragments_to_need_2_fragments(input_have_all_fragments_to_need_2_fragments),
		.input_have_all_fragments_to_have_0_fragments(input_have_all_fragments_to_have_0_fragments),
		.input_have_all_fragments_to_have_all_fragments_1(input_have_all_fragments_to_have_all_fragments_1),
		.input_have_all_fragments_to_have_0_fragments_1(input_have_0_fragments_to_have_0_fragments_1)
	);

	assign state = local_state;
	assign next_state = local_next_state;
endmodule


////////////////////////////////////////////////////////////////////////////////
// FSM_2
////////////////////////////////////////////////////////////////////////////////
module FSM_2 (
	input logic clock_port,
	input logic reset_port,
	input logic [2:0] reset_value,
	output logic [2:0] state,
	output logic [2:0] next_state,
	input logic [2:0] default_state,
	input logic input_idle_to_idle,
	input logic input_idle_to_request_start,
	input logic input_request_start_to_request_start,
	input logic input_request_start_to_request,
	input logic input_request_start_to_request_last,
	input logic input_request_start_to_flush_start,
	input logic input_request_start_to_flush,
	input logic input_request_start_to_idle,
	input logic input_request_to_request,
	input logic input_request_to_request_1,
	input logic input_request_to_request_last,
	input logic input_request_to_flush,
	input logic input_request_to_flush_1,
	input logic input_request_to_idle,
	input logic input_request_last_to_idle,
	input logic input_request_last_to_request_start,
	input logic input_flush_start_to_flush_start,
	input logic input_flush_start_to_flush,
	input logic input_flush_to_flush,
	input logic input_flush_to_flush_1,
	input logic input_flush_to_idle
);

	logic [2:0] local_state;
	logic [2:0] local_next_state;

	always_ff @(posedge clock_port) local_state <= reset_port ? reset_value : local_next_state;

	FSMLogic_2 fsm_logic (
		.state(local_state),
		.next_state(local_next_state),
		.default_state(default_state),
		.input_idle_to_idle(input_idle_to_idle),
		.input_idle_to_request_start(input_idle_to_request_start),
		.input_request_start_to_request_start(input_request_start_to_request_start),
		.input_request_start_to_request(input_request_start_to_request),
		.input_request_start_to_request_last(input_request_start_to_request_last),
		.input_request_start_to_flush_start(input_request_start_to_flush_start),
		.input_request_start_to_flush(input_request_start_to_flush),
		.input_request_start_to_idle(input_request_start_to_idle),
		.input_request_to_request(input_request_to_request),
		.input_request_to_request_1(input_request_to_request_1),
		.input_request_to_request_last(input_request_to_request_last),
		.input_request_to_flush(input_request_to_flush),
		.input_request_to_flush_1(input_request_to_flush_1),
		.input_request_to_idle(input_request_to_idle),
		.input_request_last_to_idle(input_request_last_to_idle),
		.input_request_last_to_request_start(input_idle_to_request_start),
		.input_flush_start_to_flush_start(input_flush_start_to_flush_start),
		.input_flush_start_to_flush(input_flush_start_to_flush),
		.input_flush_to_flush(input_flush_to_flush),
		.input_flush_to_flush_1(input_flush_to_flush_1),
		.input_flush_to_idle(input_flush_to_idle)
	);

	assign state = local_state;
	assign next_state = local_next_state;
endmodule


////////////////////////////////////////////////////////////////////////////////
// FSM
////////////////////////////////////////////////////////////////////////////////
module FSM (
	input logic clock_port,
	input logic reset_port,
	input logic [1:0] reset_value,
	output logic [1:0] state,
	output logic [1:0] next_state,
	input logic [1:0] default_state,
	input logic input_idle_to_fetch,
	input logic input_idle_to_memory,
	input logic input_idle_to_external,
	input logic input_idle_to_idle,
	input logic input_fetch_to_fetch,
	input logic input_fetch_to_idle,
	input logic input_memory_to_memory,
	input logic input_memory_to_idle,
	input logic input_external_to_external,
	input logic input_external_to_idle
);

	logic [1:0] local_state;
	logic [1:0] local_next_state;

	always_ff @(posedge clock_port) local_state <= reset_port ? reset_value : local_next_state;

	FSMLogic fsm_logic (
		.state(local_state),
		.next_state(local_next_state),
		.default_state(default_state),
		.input_idle_to_fetch(input_idle_to_fetch),
		.input_idle_to_memory(input_idle_to_memory),
		.input_idle_to_external(input_idle_to_external),
		.input_idle_to_idle(input_idle_to_idle),
		.input_fetch_to_fetch(input_fetch_to_fetch),
		.input_fetch_to_idle(input_fetch_to_idle),
		.input_memory_to_memory(input_memory_to_memory),
		.input_memory_to_idle(input_fetch_to_idle),
		.input_external_to_external(input_external_to_external),
		.input_external_to_idle(input_external_to_idle)
	);

	assign state = local_state;
	assign next_state = local_next_state;
endmodule


////////////////////////////////////////////////////////////////////////////////
// FSMLogic_4
////////////////////////////////////////////////////////////////////////////////
module FSMLogic_4 (
	input logic [3:0] state,
	output logic [3:0] next_state,
	input logic [3:0] default_state,
	input logic input_idle_to_idle,
	input logic input_idle_to_read_1,
	input logic input_idle_to_write,
	input logic input_idle_to_csr_read,
	input logic input_idle_to_csr_write,
	input logic input_write_to_write,
	input logic input_write_to_idle,
	input logic input_read_1_to_read_1,
	input logic input_read_1_to_read_1_1,
	input logic input_read_1_to_read_2,
	input logic input_read_2_to_idle,
	input logic input_read_2_to_read_1,
	input logic input_read_2_to_write,
	input logic input_read_2_to_csr_read,
	input logic input_read_2_to_csr_write,
	input logic input_csr_read_to_csr_read,
	input logic input_csr_read_to_idle,
	input logic input_csr_read_to_read_1,
	input logic input_csr_read_to_write,
	input logic input_csr_read_to_csr_read_1,
	input logic input_csr_read_to_csr_write,
	input logic input_csr_write_to_csr_write,
	input logic input_csr_write_to_idle,
	input logic input_csr_write_to_read_1,
	input logic input_csr_write_to_write,
	input logic input_csr_write_to_csr_read,
	input logic input_csr_write_to_csr_write_1
);

	logic [3:0] state_idle_selector;
	logic [3:0] state_write_selector;
	logic [3:0] state_read_1_selector;
	logic [3:0] state_read_2_selector;
	logic [3:0] state_csr_read_selector;
	logic [3:0] state_csr_write_selector;

	assign state_idle_selector = input_idle_to_idle ? `MemoryStates__idle : 4'b0 | input_idle_to_read_1 ? `MemoryStates__read_1 : 4'b0 | input_idle_to_write ? `MemoryStates__write : 4'b0 | input_idle_to_csr_read ? `MemoryStates__csr_read : 4'b0 | input_idle_to_csr_write ? `MemoryStates__csr_write : 4'b0 | `MemoryStates__idle;
	assign state_write_selector = input_write_to_write ? `MemoryStates__write : 4'b0 | input_write_to_idle ? `MemoryStates__idle : 4'b0 | `MemoryStates__write;
	assign state_read_1_selector = input_read_1_to_read_1 ? `MemoryStates__read_1 : 4'b0 | input_read_1_to_read_1_1 ? `MemoryStates__read_1 : 4'b0 | input_read_1_to_read_2 ? `MemoryStates__read_2 : 4'b0 | `MemoryStates__read_1;
	assign state_read_2_selector = input_read_2_to_idle ? `MemoryStates__idle : 4'b0 | input_read_2_to_read_1 ? `MemoryStates__read_1 : 4'b0 | input_read_2_to_write ? `MemoryStates__write : 4'b0 | input_read_2_to_csr_read ? `MemoryStates__csr_read : 4'b0 | input_read_2_to_csr_write ? `MemoryStates__csr_write : 4'b0 | `MemoryStates__read_2;
	assign state_csr_read_selector = input_csr_read_to_csr_read ? `MemoryStates__csr_read : 4'b0 | input_csr_read_to_csr_read_1 ? `MemoryStates__csr_read : 4'b0 | input_csr_read_to_idle ? `MemoryStates__idle : 4'b0 | input_csr_read_to_read_1 ? `MemoryStates__read_1 : 4'b0 | input_csr_read_to_write ? `MemoryStates__write : 4'b0 | input_csr_read_to_csr_write ? `MemoryStates__csr_write : 4'b0 | `MemoryStates__csr_read;
	assign state_csr_write_selector = input_csr_write_to_csr_write ? `MemoryStates__csr_write : 4'b0 | input_csr_write_to_csr_write_1 ? `MemoryStates__csr_write : 4'b0 | input_csr_write_to_idle ? `MemoryStates__idle : 4'b0 | input_csr_write_to_read_1 ? `MemoryStates__read_1 : 4'b0 | input_csr_write_to_write ? `MemoryStates__write : 4'b0 | input_csr_write_to_csr_read ? `MemoryStates__csr_read : 4'b0 | `MemoryStates__csr_write;
	assign next_state = state == `MemoryStates__idle ? state_idle_selector : 4'b0 | state == `MemoryStates__write ? state_write_selector : 4'b0 | state == `MemoryStates__read_1 ? state_read_1_selector : 4'b0 | state == `MemoryStates__read_2 ? state_read_2_selector : 4'b0 | state == `MemoryStates__csr_read ? state_csr_read_selector : 4'b0 | state == `MemoryStates__csr_write ? state_csr_write_selector : 4'b0 | default_state;

endmodule


////////////////////////////////////////////////////////////////////////////////
// FSMLogic_3
////////////////////////////////////////////////////////////////////////////////
module FSMLogic_3 (
	input logic [1:0] state,
	output logic [1:0] next_state,
	input logic [1:0] default_state,
	input logic input_have_0_fragments_to_have_all_fragments,
	input logic input_have_0_fragments_to_need_1_fragments,
	input logic input_have_0_fragments_to_need_2_fragments,
	input logic input_have_0_fragments_to_have_0_fragments,
	input logic input_have_0_fragments_to_have_0_fragments_1,
	input logic input_need_1_fragments_to_have_all_fragments,
	input logic input_need_1_fragments_to_need_1_fragments,
	input logic input_need_1_fragments_to_have_0_fragments,
	input logic input_need_2_fragments_to_need_1_fragments,
	input logic input_need_2_fragments_to_need_2_fragments,
	input logic input_need_2_fragments_to_have_0_fragments,
	input logic input_have_all_fragments_to_have_all_fragments,
	input logic input_have_all_fragments_to_need_1_fragments,
	input logic input_have_all_fragments_to_need_2_fragments,
	input logic input_have_all_fragments_to_have_0_fragments,
	input logic input_have_all_fragments_to_have_all_fragments_1,
	input logic input_have_all_fragments_to_have_0_fragments_1
);

	logic [1:0] state_have_0_fragments_selector;
	logic [1:0] state_need_1_fragments_selector;
	logic [1:0] state_need_2_fragments_selector;
	logic [1:0] state_have_all_fragments_selector;

	assign state_have_0_fragments_selector = input_have_0_fragments_to_have_all_fragments ? `InstAssembleStates__have_all_fragments : 2'b0 | input_have_0_fragments_to_need_1_fragments ? `InstAssembleStates__need_1_fragments : 2'b0 | input_have_0_fragments_to_need_2_fragments ? `InstAssembleStates__need_2_fragments : 2'b0 | input_have_0_fragments_to_have_0_fragments ? `InstAssembleStates__have_0_fragments : 2'b0 | input_have_0_fragments_to_have_0_fragments_1 ? `InstAssembleStates__have_0_fragments : 2'b0 | `InstAssembleStates__have_0_fragments;
	assign state_need_1_fragments_selector = input_need_1_fragments_to_have_all_fragments ? `InstAssembleStates__have_all_fragments : 2'b0 | input_need_1_fragments_to_need_1_fragments ? `InstAssembleStates__need_1_fragments : 2'b0 | input_have_0_fragments_to_have_0_fragments_1 ? `InstAssembleStates__have_0_fragments : 2'b0 | `InstAssembleStates__need_1_fragments;
	assign state_need_2_fragments_selector = input_need_2_fragments_to_need_1_fragments ? `InstAssembleStates__need_1_fragments : 2'b0 | input_need_2_fragments_to_need_2_fragments ? `InstAssembleStates__need_2_fragments : 2'b0 | input_have_0_fragments_to_have_0_fragments_1 ? `InstAssembleStates__have_0_fragments : 2'b0 | `InstAssembleStates__need_2_fragments;
	assign state_have_all_fragments_selector = input_have_all_fragments_to_have_all_fragments ? `InstAssembleStates__have_all_fragments : 2'b0 | input_have_all_fragments_to_have_all_fragments_1 ? `InstAssembleStates__have_all_fragments : 2'b0 | input_have_all_fragments_to_need_1_fragments ? `InstAssembleStates__need_1_fragments : 2'b0 | input_have_all_fragments_to_need_2_fragments ? `InstAssembleStates__need_2_fragments : 2'b0 | input_have_all_fragments_to_have_0_fragments ? `InstAssembleStates__have_0_fragments : 2'b0 | input_have_0_fragments_to_have_0_fragments_1 ? `InstAssembleStates__have_0_fragments : 2'b0 | `InstAssembleStates__have_all_fragments;
	assign next_state = state == `InstAssembleStates__have_0_fragments ? state_have_0_fragments_selector : 2'b0 | state == `InstAssembleStates__need_1_fragments ? state_need_1_fragments_selector : 2'b0 | state == `InstAssembleStates__need_2_fragments ? state_need_2_fragments_selector : 2'b0 | state == `InstAssembleStates__have_all_fragments ? state_have_all_fragments_selector : 2'b0 | default_state;

endmodule


////////////////////////////////////////////////////////////////////////////////
// FSMLogic_2
////////////////////////////////////////////////////////////////////////////////
module FSMLogic_2 (
	input logic [2:0] state,
	output logic [2:0] next_state,
	input logic [2:0] default_state,
	input logic input_idle_to_idle,
	input logic input_idle_to_request_start,
	input logic input_request_start_to_request_start,
	input logic input_request_start_to_request,
	input logic input_request_start_to_request_last,
	input logic input_request_start_to_flush_start,
	input logic input_request_start_to_flush,
	input logic input_request_start_to_idle,
	input logic input_request_to_request,
	input logic input_request_to_request_1,
	input logic input_request_to_request_last,
	input logic input_request_to_flush,
	input logic input_request_to_flush_1,
	input logic input_request_to_idle,
	input logic input_request_last_to_idle,
	input logic input_request_last_to_request_start,
	input logic input_flush_start_to_flush_start,
	input logic input_flush_start_to_flush,
	input logic input_flush_to_flush,
	input logic input_flush_to_flush_1,
	input logic input_flush_to_idle
);

	logic [2:0] state_idle_selector;
	logic [2:0] state_request_start_selector;
	logic [2:0] state_request_selector;
	logic [2:0] state_request_last_selector;
	logic [2:0] state_flush_start_selector;
	logic [2:0] state_flush_selector;

	assign state_idle_selector = input_idle_to_idle ? `InstBufferStates__idle : 3'b0 | input_idle_to_request_start ? `InstBufferStates__request_start : 3'b0 | `InstBufferStates__idle;
	assign state_request_start_selector = input_request_start_to_request_start ? `InstBufferStates__request_start : 3'b0 | input_request_start_to_request ? `InstBufferStates__request : 3'b0 | input_request_start_to_request_last ? `InstBufferStates__request_last : 3'b0 | input_request_start_to_flush_start ? `InstBufferStates__flush_start : 3'b0 | input_request_start_to_flush ? `InstBufferStates__flush : 3'b0 | input_request_start_to_idle ? `InstBufferStates__idle : 3'b0 | `InstBufferStates__request_start;
	assign state_request_selector = input_request_to_request ? `InstBufferStates__request : 3'b0 | input_request_to_request_1 ? `InstBufferStates__request : 3'b0 | input_request_to_request_last ? `InstBufferStates__request_last : 3'b0 | input_request_to_flush ? `InstBufferStates__flush : 3'b0 | input_request_to_flush_1 ? `InstBufferStates__flush : 3'b0 | input_request_to_idle ? `InstBufferStates__idle : 3'b0 | `InstBufferStates__request;
	assign state_request_last_selector = input_request_last_to_idle ? `InstBufferStates__idle : 3'b0 | input_idle_to_request_start ? `InstBufferStates__request_start : 3'b0 | `InstBufferStates__request_last;
	assign state_flush_start_selector = input_flush_start_to_flush_start ? `InstBufferStates__flush_start : 3'b0 | input_flush_start_to_flush ? `InstBufferStates__flush : 3'b0 | `InstBufferStates__flush_start;
	assign state_flush_selector = input_flush_to_flush ? `InstBufferStates__flush : 3'b0 | input_flush_to_flush_1 ? `InstBufferStates__flush : 3'b0 | input_flush_to_idle ? `InstBufferStates__idle : 3'b0 | `InstBufferStates__flush;
	assign next_state = state == `InstBufferStates__idle ? state_idle_selector : 3'b0 | state == `InstBufferStates__request_start ? state_request_start_selector : 3'b0 | state == `InstBufferStates__request ? state_request_selector : 3'b0 | state == `InstBufferStates__request_last ? state_request_last_selector : 3'b0 | state == `InstBufferStates__flush_start ? state_flush_start_selector : 3'b0 | state == `InstBufferStates__flush ? state_flush_selector : 3'b0 | default_state;

endmodule


////////////////////////////////////////////////////////////////////////////////
// FSMLogic
////////////////////////////////////////////////////////////////////////////////
module FSMLogic (
	input logic [1:0] state,
	output logic [1:0] next_state,
	input logic [1:0] default_state,
	input logic input_idle_to_fetch,
	input logic input_idle_to_memory,
	input logic input_idle_to_external,
	input logic input_idle_to_idle,
	input logic input_fetch_to_fetch,
	input logic input_fetch_to_idle,
	input logic input_memory_to_memory,
	input logic input_memory_to_idle,
	input logic input_external_to_external,
	input logic input_external_to_idle
);

	logic [1:0] state_idle_selector;
	logic [1:0] state_fetch_selector;
	logic [1:0] state_memory_selector;
	logic [1:0] state_external_selector;

	assign state_idle_selector = input_idle_to_fetch ? `ArbStates__fetch : 2'b0 | input_idle_to_memory ? `ArbStates__memory : 2'b0 | input_idle_to_external ? `ArbStates__external : 2'b0 | input_idle_to_idle ? `ArbStates__idle : 2'b0 | `ArbStates__idle;
	assign state_fetch_selector = input_fetch_to_fetch ? `ArbStates__fetch : 2'b0 | input_fetch_to_idle ? `ArbStates__idle : 2'b0 | `ArbStates__fetch;
	assign state_memory_selector = input_memory_to_memory ? `ArbStates__memory : 2'b0 | input_fetch_to_idle ? `ArbStates__idle : 2'b0 | `ArbStates__memory;
	assign state_external_selector = input_external_to_external ? `ArbStates__external : 2'b0 | input_external_to_idle ? `ArbStates__idle : 2'b0 | `ArbStates__external;
	assign next_state = state == `ArbStates__idle ? state_idle_selector : 2'b0 | state == `ArbStates__fetch ? state_fetch_selector : 2'b0 | state == `ArbStates__memory ? state_memory_selector : 2'b0 | state == `ArbStates__external ? state_external_selector : 2'b0 | default_state;

endmodule


