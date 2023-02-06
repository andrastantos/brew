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
`define op__misc_tpc_w_r 3'h7


`define BusIfStates__idle 3'h0
`define BusIfStates__first 3'h1
`define BusIfStates__middle 3'h2
`define BusIfStates__external 3'h3
`define BusIfStates__precharge 3'h4
`define BusIfStates__pre_external 3'h5
`define BusIfStates__non_dram_first 3'h6
`define BusIfStates__non_dram_wait 3'h7


`define MemoryStates__idle 3'h0
`define MemoryStates__mem_read_1 3'h1
`define MemoryStates__mem_read_2 3'h2
`define MemoryStates__mem_write_1 3'h3
`define MemoryStates__mem_write_2 3'h4
`define MemoryStates__csr_read 3'h5
`define MemoryStates__csr_write 3'h7


`define InstBufferStates__idle 2'h0
`define InstBufferStates__request 2'h1
`define InstBufferStates__flush_start 2'h2


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
	output logic DRAM_nCAS_a,
	output logic DRAM_nCAS_b,
	output logic [11:0] DRAM_ADDR,
	output logic DRAM_nWE,
	input logic [7:0] DRAM_DATA_rd,
	output logic [7:0] DRAM_DATA_wr,
	output logic DRAM_nNREN,
	input logic DRAM_nWAIT,
	input logic ext_req,
	output logic ext_grnt,
	input logic interrupt
);

	logic [7:0] ext_bus_if_data_in;
	logic ext_bus_if_nWAIT;
	logic exec_to_mem_result_reg_addr_valid;
	logic bus_to_mem_ready;
	logic [30:0] spc;
	logic [30:0] tpc;
	logic task_mode;
	logic [11:0] ecause;
	logic [11:0] rcause;
	logic [31:0] csr_if_rd_data;
	logic [21:0] mem_base;
	logic [21:0] mem_limit;
	logic mem_to_bus_ready;
	logic fetch_to_bus_ready;
	logic ext_bus_if_nWE;
	logic ext_bus_if_nRAS;
	logic ext_bus_if_nNREN;
	logic ext_bus_if_nCAS_a;
	logic ext_bus_if_nCAS_b;
	logic [10:0] ext_bus_if_addr;
	logic [7:0] ext_bus_if_data_out;
	logic [15:0] bus_to_mem_data;
	logic bus_to_mem_valid;
	logic bus_to_fetch_valid;
	logic [15:0] bus_to_fetch_data;
	logic fetch_to_bus_valid;
	logic [1:0] fetch_to_bus_byte_en;
	logic [15:0] fetch_to_bus_data;
	logic fetch_to_bus_read_not_write;
	logic bus_to_fetch_ready;
	logic fetch_to_bus_dram_not_ext;
	logic [25:0] fetch_to_bus_addr;
	logic fetch_to_decode_valid;
	logic fetch_to_decode_av;
	logic [15:0] fetch_to_decode_inst_0;
	logic [15:0] fetch_to_decode_inst_1;
	logic [15:0] fetch_to_decode_inst_2;
	logic [1:0] fetch_to_decode_inst_len;
	logic rf_req_read1_valid;
	logic rf_req_read2_valid;
	logic rf_req_rsv_valid;
	logic rf_req_valid;
	logic [2:0] decode_to_exec_opcode;
	logic [2:0] decode_to_exec_exec_unit;
	logic [31:0] decode_to_exec_op_a;
	logic [31:0] decode_to_exec_op_b;
	logic [31:0] decode_to_exec_op_imm;
	logic [1:0] decode_to_exec_inst_len;
	logic decode_to_exec_is_load;
	logic decode_to_exec_is_store;
	logic decode_to_exec_do_bse;
	logic decode_to_exec_do_wse;
	logic decode_to_exec_do_bze;
	logic decode_to_exec_do_wze;
	logic [3:0] decode_to_exec_result_reg_addr;
	logic decode_to_exec_result_reg_addr_valid;
	logic decode_to_exec_fetch_av;
	logic decode_to_exec_valid;
	logic rf_rsp_ready;
	logic fetch_to_decode_ready;
	logic [3:0] rf_req_read1_addr;
	logic [3:0] rf_req_read2_addr;
	logic [3:0] rf_req_rsv_addr;
	logic [1:0] decode_to_exec_mem_access_len;
	logic do_branch;
	logic [1:0] exec_to_mem_mem_access_len;
	logic [31:0] exec_to_mem_result;
	logic exec_to_mem_result_data_valid;
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
	logic [31:0] rf_write_data;
	logic rf_write_valid;
	logic mem_to_bus_valid;
	logic [1:0] mem_to_bus_byte_en;
	logic [15:0] mem_to_bus_data;
	logic csr_if_request;
	logic mem_to_bus_dram_not_ext;
	logic mem_to_bus_read_not_write;
	logic csr_if_read_not_write;
	logic [31:0] csr_if_wr_data;
	logic rf_write_data_en;
	logic [9:0] csr_if_addr;
	logic [3:0] rf_write_addr;
	logic [25:0] mem_to_bus_addr;
	logic reg_file_read1_rsv_bit;
	logic reg_file_read2_rsv_bit;
	logic reg_file_rsv_rsv_bit;
	logic rf_req_ready;
	logic rf_rsp_valid;
	logic [31:0] rf_rsp_read1_data;
	logic [31:0] rf_rsp_read2_data;
	logic [3:0] csr_addr;

	always_ff @(posedge clk) spc <= rst ? 31'h0 : execute_stage_spc_out;
	always_ff @(posedge clk) tpc <= rst ? 31'h0 : execute_stage_tpc_out;
	always_ff @(posedge clk) task_mode <= rst ? 1'h0 : execute_stage_task_mode_out;
	always_ff @(posedge clk) ecause <= rst ? 12'h0 : execute_stage_ecause_out;
	always_ff @(posedge clk) rcause <= rst ? 12'h0 : execute_stage_rcause_out;
	assign csr_addr = csr_if_addr[3:0];
	always_ff @(posedge clk) csr_if_rd_data <= rst ? 32'h0 : csr_addr == 0 ? 1'h0 : 32'b0 | csr_addr == 1 ? ({mem_base, 10'h0}) : 32'b0 | csr_addr == 2 ? ({mem_limit, 10'h0}) : 32'b0 | csr_addr == 3 ? ecause : 32'b0 | csr_addr == 4 ? rcause : 32'b0;
	always_ff @(posedge clk) mem_base <= rst ? 22'h0 : csr_addr == 1'h1 &  ~ csr_if_read_not_write & csr_if_request ? csr_if_rd_data[31:10] : mem_base;
	always_ff @(posedge clk) mem_limit <= rst ? 22'h0 : csr_addr == 2'h2 &  ~ csr_if_read_not_write & csr_if_request ? csr_if_rd_data[31:10] : mem_limit;
	assign DRAM_ADDR = ext_bus_if_addr;

	BusIf bus_if (
		.clk(clk),
		.rst(rst),
		.fetch_request_addr(fetch_to_bus_addr),
		.fetch_request_byte_en(fetch_to_bus_byte_en),
		.fetch_request_data(fetch_to_bus_data),
		.fetch_request_dram_not_ext(fetch_to_bus_dram_not_ext),
		.fetch_request_read_not_write(fetch_to_bus_read_not_write),
		.fetch_request_ready(fetch_to_bus_ready),
		.fetch_request_valid(fetch_to_bus_valid),

		.fetch_response_data(bus_to_fetch_data),
		.fetch_response_ready(bus_to_fetch_ready),
		.fetch_response_valid(bus_to_fetch_valid),

		.mem_request_addr(mem_to_bus_addr),
		.mem_request_byte_en(mem_to_bus_byte_en),
		.mem_request_data(mem_to_bus_data),
		.mem_request_dram_not_ext(mem_to_bus_dram_not_ext),
		.mem_request_read_not_write(mem_to_bus_read_not_write),
		.mem_request_ready(mem_to_bus_ready),
		.mem_request_valid(mem_to_bus_valid),

		.mem_response_data(bus_to_mem_data),
		.mem_response_ready(bus_to_mem_ready),
		.mem_response_valid(bus_to_mem_valid),

		.dram_addr(ext_bus_if_addr),
		.dram_data_in(DRAM_DATA_rd),
		.dram_data_out(ext_bus_if_data_out),
		.dram_nCAS_a(ext_bus_if_nCAS_a),
		.dram_nCAS_b(ext_bus_if_nCAS_b),
		.dram_nNREN(ext_bus_if_nNREN),
		.dram_nRAS(ext_bus_if_nRAS),
		.dram_nWAIT(DRAM_nWAIT),
		.dram_nWE(ext_bus_if_nWE),

		.ext_req(ext_req),
		.ext_grnt(ext_grnt)
	);

	FetchStage fetch_stage (
		.clk(clk),
		.rst(rst),
		.bus_if_request_addr(fetch_to_bus_addr),
		.bus_if_request_byte_en(fetch_to_bus_byte_en),
		.bus_if_request_data(fetch_to_bus_data),
		.bus_if_request_dram_not_ext(fetch_to_bus_dram_not_ext),
		.bus_if_request_read_not_write(fetch_to_bus_read_not_write),
		.bus_if_request_ready(fetch_to_bus_ready),
		.bus_if_request_valid(fetch_to_bus_valid),

		.bus_if_response_data(bus_to_fetch_data),
		.bus_if_response_ready(bus_to_fetch_ready),
		.bus_if_response_valid(bus_to_fetch_valid),

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

		.reg_file_req_read1_addr(rf_req_read1_addr),
		.reg_file_req_read1_valid(rf_req_read1_valid),
		.reg_file_req_read2_addr(rf_req_read2_addr),
		.reg_file_req_read2_valid(rf_req_read2_valid),
		.reg_file_req_ready(rf_req_ready),
		.reg_file_req_rsv_addr(rf_req_rsv_addr),
		.reg_file_req_rsv_valid(rf_req_rsv_valid),
		.reg_file_req_valid(rf_req_valid),

		.reg_file_rsp_read1_data(rf_rsp_read1_data),
		.reg_file_rsp_read2_data(rf_rsp_read2_data),
		.reg_file_rsp_ready(rf_rsp_ready),
		.reg_file_rsp_valid(rf_rsp_valid),

		.do_branch(do_branch)
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
		.mem_result_data_valid(exec_to_mem_result_data_valid),
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
		.exec_result_data_valid(exec_to_mem_result_data_valid),
		.exec_result_reg_addr(exec_to_mem_result_reg_addr),
		.exec_result_reg_addr_valid(exec_to_mem_result_reg_addr_valid),
		.exec_valid(exec_to_mem_valid),

		.reg_file_addr(rf_write_addr),
		.reg_file_data(rf_write_data),
		.reg_file_data_en(rf_write_data_en),
		.reg_file_valid(rf_write_valid),

		.bus_req_if_addr(mem_to_bus_addr),
		.bus_req_if_byte_en(mem_to_bus_byte_en),
		.bus_req_if_data(mem_to_bus_data),
		.bus_req_if_dram_not_ext(mem_to_bus_dram_not_ext),
		.bus_req_if_read_not_write(mem_to_bus_read_not_write),
		.bus_req_if_ready(mem_to_bus_ready),
		.bus_req_if_valid(mem_to_bus_valid),

		.bus_rsp_if_data(bus_to_mem_data),
		.bus_rsp_if_ready(bus_to_mem_ready),
		.bus_rsp_if_valid(bus_to_mem_valid),

		.csr_if_addr(csr_if_addr),
		.csr_if_rd_data(csr_if_rd_data),
		.csr_if_read_not_write(csr_if_read_not_write),
		.csr_if_request(csr_if_request),
		.csr_if_wr_data(csr_if_wr_data)
	);

	RegFile reg_file (
		.clk(clk),
		.rst(rst),
		.read_req_read1_addr(rf_req_read1_addr),
		.read_req_read1_valid(rf_req_read1_valid),
		.read_req_read2_addr(rf_req_read2_addr),
		.read_req_read2_valid(rf_req_read2_valid),
		.read_req_ready(rf_req_ready),
		.read_req_rsv_addr(rf_req_rsv_addr),
		.read_req_rsv_valid(rf_req_rsv_valid),
		.read_req_valid(rf_req_valid),

		.read_rsp_read1_data(rf_rsp_read1_data),
		.read_rsp_read2_data(rf_rsp_read2_data),
		.read_rsp_ready(rf_rsp_ready),
		.read_rsp_valid(rf_rsp_valid),

		.write_addr(rf_write_addr),
		.write_data(rf_write_data),
		.write_data_en(rf_write_data_en),
		.write_valid(rf_write_valid),

		.read1_rsv_bit(reg_file_read1_rsv_bit),
		.read2_rsv_bit(reg_file_read2_rsv_bit),
		.rsv_rsv_bit(reg_file_rsv_rsv_bit)
	);

	assign ext_bus_if_data_in = DRAM_DATA_rd;
	assign ext_bus_if_nWAIT = DRAM_nWAIT;
	assign DRAM_nWE = ext_bus_if_nWE;
	assign DRAM_nRAS = ext_bus_if_nRAS;
	assign DRAM_nNREN = ext_bus_if_nNREN;
	assign DRAM_nCAS_a = ext_bus_if_nCAS_a;
	assign DRAM_nCAS_b = ext_bus_if_nCAS_b;
	assign DRAM_DATA_wr = ext_bus_if_data_out;
endmodule


////////////////////////////////////////////////////////////////////////////////
// RegFile
////////////////////////////////////////////////////////////////////////////////
module RegFile (
	input logic clk,
	input logic rst,
	input logic [3:0] read_req_read1_addr,
	input logic read_req_read1_valid,
	input logic [3:0] read_req_read2_addr,
	input logic read_req_read2_valid,
	output logic read_req_ready,
	input logic [3:0] read_req_rsv_addr,
	input logic read_req_rsv_valid,
	input logic read_req_valid,

	output logic [31:0] read_rsp_read1_data,
	output logic [31:0] read_rsp_read2_data,
	input logic read_rsp_ready,
	output logic read_rsp_valid,

	input logic [3:0] write_addr,
	input logic [31:0] write_data,
	input logic write_data_en,
	input logic write_valid,

	output logic read1_rsv_bit,
	output logic read2_rsv_bit,
	output logic rsv_rsv_bit
);

	logic req_advance;
	logic rsp_advance;
	logic u2_output_port;
	logic read1_valid;
	logic [3:0] u4_output_port;
	logic [3:0] read1_addr;
	logic u6_output_port;
	logic read2_valid;
	logic [3:0] u8_output_port;
	logic [3:0] read2_addr;
	logic u10_output_port;
	logic rsv_valid;
	logic [3:0] u12_output_port;
	logic [3:0] rsv_addr;
	logic [31:0] write_data_d;
	logic wait_for_read1;
	logic wait_for_read2;
	logic wait_for_rsv;
	logic wait_for_some;
	logic u102_output_port;
	logic wait_for_write;
	logic rsv_set_valid;
	logic rsv_clr_valid;
	logic u119_output_port;
	logic u131_output_port;
	logic u143_output_port;
	logic u155_output_port;
	logic u167_output_port;
	logic u179_output_port;
	logic u191_output_port;
	logic u203_output_port;
	logic u215_output_port;
	logic u227_output_port;
	logic u239_output_port;
	logic u251_output_port;
	logic u263_output_port;
	logic u275_output_port;
	logic u287_output_port;
	logic outstanding_req;
	logic wait_for_write_d;
	logic out_buf_full;
	logic [14:0] rsv_board;

	assign req_advance = read_req_ready & read_req_valid;
	always_ff @(posedge clk) u2_output_port <= rst ? 1'h0 : req_advance ? read_req_read1_valid : u2_output_port;
	always_ff @(posedge clk) u4_output_port <= rst ? 4'h0 : req_advance ? read_req_read1_addr : u4_output_port;
	always_ff @(posedge clk) u6_output_port <= rst ? 1'h0 : req_advance ? read_req_read2_valid : u6_output_port;
	always_ff @(posedge clk) u8_output_port <= rst ? 4'h0 : req_advance ? read_req_read2_addr : u8_output_port;
	always_ff @(posedge clk) u10_output_port <= rst ? 1'h0 : req_advance ? read_req_rsv_valid : u10_output_port;
	always_ff @(posedge clk) u12_output_port <= rst ? 4'h0 : req_advance ? read_req_rsv_addr : u12_output_port;
	always_ff @(posedge clk) write_data_d <= rst ? 32'h0 : write_data;
	assign read1_addr = req_advance ? read_req_read1_addr : u4_output_port;
	assign read2_addr = req_advance ? read_req_read2_addr : u8_output_port;
	assign read1_rsv_bit = read1_addr == 0 ? rsv_board[0] : 1'b0 | read1_addr == 1 ? rsv_board[1] : 1'b0 | read1_addr == 2 ? rsv_board[2] : 1'b0 | read1_addr == 3 ? rsv_board[3] : 1'b0 | read1_addr == 4 ? rsv_board[4] : 1'b0 | read1_addr == 5 ? rsv_board[5] : 1'b0 | read1_addr == 6 ? rsv_board[6] : 1'b0 | read1_addr == 7 ? rsv_board[7] : 1'b0 | read1_addr == 8 ? rsv_board[8] : 1'b0 | read1_addr == 9 ? rsv_board[9] : 1'b0 | read1_addr == 10 ? rsv_board[10] : 1'b0 | read1_addr == 11 ? rsv_board[11] : 1'b0 | read1_addr == 12 ? rsv_board[12] : 1'b0 | read1_addr == 13 ? rsv_board[13] : 1'b0 | read1_addr == 14 ? rsv_board[14] : 1'b0;
	assign read2_rsv_bit = read2_addr == 0 ? rsv_board[0] : 1'b0 | read2_addr == 1 ? rsv_board[1] : 1'b0 | read2_addr == 2 ? rsv_board[2] : 1'b0 | read2_addr == 3 ? rsv_board[3] : 1'b0 | read2_addr == 4 ? rsv_board[4] : 1'b0 | read2_addr == 5 ? rsv_board[5] : 1'b0 | read2_addr == 6 ? rsv_board[6] : 1'b0 | read2_addr == 7 ? rsv_board[7] : 1'b0 | read2_addr == 8 ? rsv_board[8] : 1'b0 | read2_addr == 9 ? rsv_board[9] : 1'b0 | read2_addr == 10 ? rsv_board[10] : 1'b0 | read2_addr == 11 ? rsv_board[11] : 1'b0 | read2_addr == 12 ? rsv_board[12] : 1'b0 | read2_addr == 13 ? rsv_board[13] : 1'b0 | read2_addr == 14 ? rsv_board[14] : 1'b0;
	assign rsv_addr = req_advance ? read_req_rsv_addr : u12_output_port;
	assign rsv_rsv_bit = rsv_addr == 0 ? rsv_board[0] : 1'b0 | rsv_addr == 1 ? rsv_board[1] : 1'b0 | rsv_addr == 2 ? rsv_board[2] : 1'b0 | rsv_addr == 3 ? rsv_board[3] : 1'b0 | rsv_addr == 4 ? rsv_board[4] : 1'b0 | rsv_addr == 5 ? rsv_board[5] : 1'b0 | rsv_addr == 6 ? rsv_board[6] : 1'b0 | rsv_addr == 7 ? rsv_board[7] : 1'b0 | rsv_addr == 8 ? rsv_board[8] : 1'b0 | rsv_addr == 9 ? rsv_board[9] : 1'b0 | rsv_addr == 10 ? rsv_board[10] : 1'b0 | rsv_addr == 11 ? rsv_board[11] : 1'b0 | rsv_addr == 12 ? rsv_board[12] : 1'b0 | rsv_addr == 13 ? rsv_board[13] : 1'b0 | rsv_addr == 14 ? rsv_board[14] : 1'b0;
	assign read1_valid = req_advance ? read_req_read1_valid : u2_output_port;
	assign read2_valid = req_advance ? read_req_read2_valid : u6_output_port;
	assign rsv_valid = req_advance ? read_req_rsv_valid : u10_output_port;
	assign wait_for_read1 = read1_valid & (read1_addr == 0 ? rsv_board[0] : 1'b0 | read1_addr == 1 ? rsv_board[1] : 1'b0 | read1_addr == 2 ? rsv_board[2] : 1'b0 | read1_addr == 3 ? rsv_board[3] : 1'b0 | read1_addr == 4 ? rsv_board[4] : 1'b0 | read1_addr == 5 ? rsv_board[5] : 1'b0 | read1_addr == 6 ? rsv_board[6] : 1'b0 | read1_addr == 7 ? rsv_board[7] : 1'b0 | read1_addr == 8 ? rsv_board[8] : 1'b0 | read1_addr == 9 ? rsv_board[9] : 1'b0 | read1_addr == 10 ? rsv_board[10] : 1'b0 | read1_addr == 11 ? rsv_board[11] : 1'b0 | read1_addr == 12 ? rsv_board[12] : 1'b0 | read1_addr == 13 ? rsv_board[13] : 1'b0 | read1_addr == 14 ? rsv_board[14] : 1'b0) &  ~ (write_addr == read1_addr & write_valid);
	assign wait_for_read2 = read2_valid & (read2_addr == 0 ? rsv_board[0] : 1'b0 | read2_addr == 1 ? rsv_board[1] : 1'b0 | read2_addr == 2 ? rsv_board[2] : 1'b0 | read2_addr == 3 ? rsv_board[3] : 1'b0 | read2_addr == 4 ? rsv_board[4] : 1'b0 | read2_addr == 5 ? rsv_board[5] : 1'b0 | read2_addr == 6 ? rsv_board[6] : 1'b0 | read2_addr == 7 ? rsv_board[7] : 1'b0 | read2_addr == 8 ? rsv_board[8] : 1'b0 | read2_addr == 9 ? rsv_board[9] : 1'b0 | read2_addr == 10 ? rsv_board[10] : 1'b0 | read2_addr == 11 ? rsv_board[11] : 1'b0 | read2_addr == 12 ? rsv_board[12] : 1'b0 | read2_addr == 13 ? rsv_board[13] : 1'b0 | read2_addr == 14 ? rsv_board[14] : 1'b0) &  ~ (write_addr == read2_addr & write_valid);
	assign wait_for_rsv = rsv_valid & (rsv_addr == 0 ? rsv_board[0] : 1'b0 | rsv_addr == 1 ? rsv_board[1] : 1'b0 | rsv_addr == 2 ? rsv_board[2] : 1'b0 | rsv_addr == 3 ? rsv_board[3] : 1'b0 | rsv_addr == 4 ? rsv_board[4] : 1'b0 | rsv_addr == 5 ? rsv_board[5] : 1'b0 | rsv_addr == 6 ? rsv_board[6] : 1'b0 | rsv_addr == 7 ? rsv_board[7] : 1'b0 | rsv_addr == 8 ? rsv_board[8] : 1'b0 | rsv_addr == 9 ? rsv_board[9] : 1'b0 | rsv_addr == 10 ? rsv_board[10] : 1'b0 | rsv_addr == 11 ? rsv_board[11] : 1'b0 | rsv_addr == 12 ? rsv_board[12] : 1'b0 | rsv_addr == 13 ? rsv_board[13] : 1'b0 | rsv_addr == 14 ? rsv_board[14] : 1'b0) &  ~ (write_addr == rsv_addr & write_valid);
	assign wait_for_some = wait_for_read1 | wait_for_read2 | wait_for_rsv;
	always_ff @(posedge clk) u102_output_port <= rst ? 1'h0 : req_advance | write_valid ? wait_for_some : u102_output_port;
	assign wait_for_write = (req_advance | write_valid) ? wait_for_some : u102_output_port;
	assign rsv_set_valid = rsv_valid &  ~ wait_for_write;
	assign rsv_clr_valid = write_valid &  ~ wait_for_rsv;
	always_ff @(posedge clk) u119_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 1'h0) ? 1'h1 : (rsv_clr_valid & write_addr == 1'h0) ? 1'h0 : rsv_board[0];
	always_ff @(posedge clk) u131_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 1'h1) ? 1'h1 : (rsv_clr_valid & write_addr == 1'h1) ? 1'h0 : rsv_board[1];
	always_ff @(posedge clk) u143_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 2'h2) ? 1'h1 : (rsv_clr_valid & write_addr == 2'h2) ? 1'h0 : rsv_board[2];
	always_ff @(posedge clk) u155_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 2'h3) ? 1'h1 : (rsv_clr_valid & write_addr == 2'h3) ? 1'h0 : rsv_board[3];
	always_ff @(posedge clk) u167_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 3'h4) ? 1'h1 : (rsv_clr_valid & write_addr == 3'h4) ? 1'h0 : rsv_board[4];
	always_ff @(posedge clk) u179_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 3'h5) ? 1'h1 : (rsv_clr_valid & write_addr == 3'h5) ? 1'h0 : rsv_board[5];
	always_ff @(posedge clk) u191_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 3'h6) ? 1'h1 : (rsv_clr_valid & write_addr == 3'h6) ? 1'h0 : rsv_board[6];
	always_ff @(posedge clk) u203_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 3'h7) ? 1'h1 : (rsv_clr_valid & write_addr == 3'h7) ? 1'h0 : rsv_board[7];
	always_ff @(posedge clk) u215_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 4'h8) ? 1'h1 : (rsv_clr_valid & write_addr == 4'h8) ? 1'h0 : rsv_board[8];
	always_ff @(posedge clk) u227_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 4'h9) ? 1'h1 : (rsv_clr_valid & write_addr == 4'h9) ? 1'h0 : rsv_board[9];
	always_ff @(posedge clk) u239_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 4'ha) ? 1'h1 : (rsv_clr_valid & write_addr == 4'ha) ? 1'h0 : rsv_board[10];
	always_ff @(posedge clk) u251_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 4'hb) ? 1'h1 : (rsv_clr_valid & write_addr == 4'hb) ? 1'h0 : rsv_board[11];
	always_ff @(posedge clk) u263_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 4'hc) ? 1'h1 : (rsv_clr_valid & write_addr == 4'hc) ? 1'h0 : rsv_board[12];
	always_ff @(posedge clk) u275_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 4'hd) ? 1'h1 : (rsv_clr_valid & write_addr == 4'hd) ? 1'h0 : rsv_board[13];
	always_ff @(posedge clk) u287_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 4'he) ? 1'h1 : (rsv_clr_valid & write_addr == 4'he) ? 1'h0 : rsv_board[14];
	assign rsp_advance = read_rsp_ready & read_rsp_valid;
	always_ff @(posedge clk) outstanding_req <= rst ? 1'h0 : req_advance ? 1'h1 : rsp_advance ? 1'h0 : outstanding_req;
	always_ff @(posedge clk) out_buf_full <= rst ? 1'h0 : req_advance ? 1'h1 : rsp_advance ? 1'h0 : out_buf_full;
	always_ff @(posedge clk) wait_for_write_d <= rst ? 1'h0 : wait_for_write;
	assign read_req_ready =  ~ wait_for_write_d & (read_rsp_ready |  ~ out_buf_full);
	assign read_rsp_valid =  ~ wait_for_write_d & out_buf_full;
	assign rsv_board = {u287_output_port, u275_output_port, u263_output_port, u251_output_port, u239_output_port, u227_output_port, u215_output_port, u203_output_port, u191_output_port, u179_output_port, u167_output_port, u155_output_port, u143_output_port, u131_output_port, u119_output_port};

	SimpleDualPortMemory mem1 (
		.port1_addr(write_addr),
		.port1_clk(clk),
		.port2_addr(read1_addr),
		.port2_clk(clk),
		.port1_write_en(write_valid & write_data_en),
		.port1_data_in(write_data),
		.port2_data_out(read_rsp_read1_data)
	);

	SimpleDualPortMemory mem2 (
		.port1_addr(write_addr),
		.port1_clk(clk),
		.port2_addr(read2_addr),
		.port2_clk(clk),
		.port1_write_en(write_valid & write_data_en),
		.port1_data_in(write_data),
		.port2_data_out(read_rsp_read2_data)
	);

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

	always @(posedge port1_clk) begin
		port2_data_out <= mem[port2_addr];
	end


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
	input logic exec_result_data_valid,
	input logic [3:0] exec_result_reg_addr,
	input logic exec_result_reg_addr_valid,
	input logic exec_valid,

	output logic [3:0] reg_file_addr,
	output logic [31:0] reg_file_data,
	output logic reg_file_data_en,
	output logic reg_file_valid,

	output logic [25:0] bus_req_if_addr,
	output logic [1:0] bus_req_if_byte_en,
	output logic [15:0] bus_req_if_data,
	output logic bus_req_if_dram_not_ext,
	output logic bus_req_if_read_not_write,
	input logic bus_req_if_ready,
	output logic bus_req_if_valid,

	input logic [15:0] bus_rsp_if_data,
	output logic bus_rsp_if_ready,
	input logic bus_rsp_if_valid,

	output logic [9:0] csr_if_addr,
	input logic [31:0] csr_if_rd_data,
	output logic csr_if_read_not_write,
	output logic csr_if_request,
	output logic [31:0] csr_if_wr_data
);

	logic is_csr;
	logic u10_output_port;
	logic u16_output_port;
	logic u22_output_port;
	logic u28_output_port;
	logic u30_output_port;
	logic u32_output_port;
	logic u34_output_port;
	logic u41_output_port;
	logic u48_output_port;
	logic u55_output_port;
	logic u62_output_port;
	logic u65_output_port;
	logic u68_output_port;
	logic u69_output_port;
	logic u71_output_port;
	logic u78_output_port;
	logic u85_output_port;
	logic u92_output_port;
	logic u99_output_port;
	logic u102_output_port;
	logic u105_output_port;
	logic u106_output_port;
	logic u112_output_port;
	logic u118_output_port;
	logic u124_output_port;
	logic u130_output_port;
	logic u132_output_port;
	logic u134_output_port;
	logic u135_output_port;
	logic u141_output_port;
	logic u147_output_port;
	logic u153_output_port;
	logic u159_output_port;
	logic u161_output_port;
	logic u163_output_port;
	logic exec_ready_1;
	logic accept_next;
	logic lsb;
	logic [15:0] u185_output_port;
	logic [15:0] data_h;
	logic [15:0] u196_output_port;
	logic [15:0] u204_output_port;
	logic [15:0] data_l;
	logic do_bse;
	logic do_wse;
	logic do_bze;
	logic do_wze;
	logic [3:0] u263_output_port;
	logic write_back_tick;
	logic [31:0] u301_output_port;
	logic [30:0] u304_output_port;
	logic [30:0] bus_addr;
	logic csr_request;
	logic [2:0] state;
	logic [2:0] next_state;

	assign is_csr = exec_mem_addr[31:28] == 4'hc;
	assign exec_ready_1 = (state == `MemoryStates__mem_write_2 | state == `MemoryStates__mem_read_2) & bus_req_if_ready | state == `MemoryStates__csr_write | state == `MemoryStates__csr_read | state == `MemoryStates__idle;
	assign accept_next = exec_valid & exec_ready_1;
	always_ff @(posedge clk) u185_output_port <= rst ? 16'h0 : state == accept_next ? exec_result[31:16] : u185_output_port;
	assign data_h = state == `MemoryStates__idle ? u185_output_port : 16'b0 | state == `MemoryStates__mem_read_2 ? bus_rsp_if_data : 16'b0 | state == `MemoryStates__csr_read ? csr_if_rd_data[31:16] : 16'b0 ;
	always_ff @(posedge clk) lsb <= rst ? 1'h0 : accept_next ? exec_mem_addr[0] : lsb;
	always_ff @(posedge clk) u196_output_port <= rst ? 16'h0 : state == accept_next ? exec_result[15:0] : u196_output_port;
	always_ff @(posedge clk) u204_output_port <= rst ? 16'h0 : bus_rsp_if_valid ? lsb ? ({bus_rsp_if_data[15:8], bus_rsp_if_data[15:8]}) : bus_rsp_if_data : u204_output_port;
	assign data_l = state == `MemoryStates__idle ? u196_output_port : 16'b0 | state == `MemoryStates__mem_read_1 ? u204_output_port : 16'b0 | state == `MemoryStates__csr_read ? csr_if_rd_data[15:0] : 16'b0 ;
	always_ff @(posedge clk) do_bse <= rst ? 1'h0 : accept_next ? exec_do_bse : do_bse;
	always_ff @(posedge clk) do_wse <= rst ? 1'h0 : accept_next ? exec_do_wse : do_wse;
	always_ff @(posedge clk) do_bze <= rst ? 1'h0 : accept_next ? exec_do_bze : do_bze;
	always_ff @(posedge clk) do_wze <= rst ? 1'h0 : accept_next ? exec_do_wze : do_wze;
	assign reg_file_data = do_bse ? ({data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7], data_l[7:0]}) : 32'b0 | do_wse ? ({data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15], data_l[15:0]}) : 32'b0 | do_bze ? data_l[7:0] : 32'b0 | do_wze ? data_l[15:0] : 32'b0 | ({data_h, data_l});
	always_ff @(posedge clk) u263_output_port <= rst ? 4'h0 : state == `MemoryStates__idle ? exec_result_reg_addr : u263_output_port;
	assign reg_file_addr = accept_next ? exec_result_reg_addr : u263_output_port;
	always_ff @(posedge clk) write_back_tick <= rst ? 1'h0 : accept_next;
	assign reg_file_valid = state == `MemoryStates__idle & next_state == `MemoryStates__idle ? write_back_tick : 1'b0 | state == `MemoryStates__mem_read_1 ? 1'h0 : 1'b0 | state == `MemoryStates__mem_read_2 ? bus_rsp_if_valid : 1'b0 | state == `MemoryStates__csr_read ? write_back_tick : 1'b0 | state == `MemoryStates__csr_write ? write_back_tick : 1'b0 ;
	assign bus_req_if_valid = state == `MemoryStates__mem_read_1 | state == `MemoryStates__mem_write_1 | state == `MemoryStates__mem_read_2 | state == `MemoryStates__mem_write_2;
	assign bus_req_if_byte_en = exec_mem_access_len == 1'h0 ? ({exec_mem_addr[0],  ~ exec_mem_addr[0]}) : 2'h3;
	always_ff @(posedge clk) u304_output_port <= rst ? 31'h0 : accept_next ? u301_output_port[30:0] : u304_output_port;
	assign bus_addr = accept_next ? exec_mem_addr[31:1] : u304_output_port;
	assign bus_req_if_data = state == `MemoryStates__idle ? ({(exec_mem_addr[0] & exec_mem_access_len == 1'h0) ? exec_result[7:0] : exec_result[15:8], exec_result[7:0]}) : data_h;
	assign csr_request = exec_valid & (exec_is_store | exec_is_load) & is_csr;
	assign csr_if_addr = exec_mem_addr[11:2];
	assign bus_req_if_dram_not_ext = 1'h0;
	assign bus_req_if_read_not_write = exec_is_load;
	assign csr_if_read_not_write = exec_is_load;
	assign csr_if_wr_data = exec_result;
	assign reg_file_data_en = exec_result_data_valid;
	assign bus_req_if_addr = bus_addr[21:0];

	FSM_4 fsm (
		.clock_port(clk),
		.reset_port(rst),
		.reset_value(`MemoryStates__idle),
		.state(state),
		.next_state(next_state),
		.default_state(`MemoryStates__idle),
		.input_idle_to_mem_read_1(u10_output_port),
		.input_idle_to_mem_read_2(u16_output_port),
		.input_idle_to_mem_write_1(u22_output_port),
		.input_idle_to_mem_write_2(u28_output_port),
		.input_idle_to_csr_read(u30_output_port),
		.input_idle_to_csr_write(u32_output_port),
		.input_mem_read_1_to_mem_read_2(bus_rsp_if_valid),
		.input_mem_read_2_to_idle(u34_output_port),
		.input_mem_read_2_to_mem_read_1(u41_output_port),
		.input_mem_read_2_to_mem_read_2(u48_output_port),
		.input_mem_read_2_to_mem_write_1(u55_output_port),
		.input_mem_read_2_to_mem_write_2(u62_output_port),
		.input_mem_read_2_to_csr_read(u65_output_port),
		.input_mem_read_2_to_csr_write(u68_output_port),
		.input_mem_write_1_to_mem_write_2(u69_output_port),
		.input_mem_write_2_to_idle(u71_output_port),
		.input_mem_write_2_to_mem_read_1(u78_output_port),
		.input_mem_write_2_to_mem_read_2(u85_output_port),
		.input_mem_write_2_to_mem_write_1(u92_output_port),
		.input_mem_write_2_to_mem_write_2(u99_output_port),
		.input_mem_write_2_to_csr_read(u102_output_port),
		.input_mem_write_2_to_csr_write(u105_output_port),
		.input_csr_read_to_idle(u106_output_port),
		.input_csr_read_to_mem_read_1(u112_output_port),
		.input_csr_read_to_mem_read_2(u118_output_port),
		.input_csr_read_to_mem_write_1(u124_output_port),
		.input_csr_read_to_mem_write_2(u130_output_port),
		.input_csr_read_to_csr_read(u132_output_port),
		.input_csr_read_to_csr_write(u134_output_port),
		.input_csr_write_to_idle(u135_output_port),
		.input_csr_write_to_mem_read_1(u141_output_port),
		.input_csr_write_to_mem_read_2(u147_output_port),
		.input_csr_write_to_mem_write_1(u153_output_port),
		.input_csr_write_to_mem_write_2(u159_output_port),
		.input_csr_write_to_csr_read(u161_output_port),
		.input_csr_write_to_csr_write(u163_output_port)
	);

	assign bus_rsp_if_ready = 1'hx;
	assign u10_output_port = exec_valid &  ~ is_csr & exec_is_load & exec_mem_access_len == 2'h2;
	assign u16_output_port = exec_valid &  ~ is_csr & exec_is_load & exec_mem_access_len != 2'h2;
	assign u22_output_port = exec_valid &  ~ is_csr & exec_is_store & exec_mem_access_len == 2'h2;
	assign u28_output_port = exec_valid &  ~ is_csr & exec_is_store & exec_mem_access_len != 2'h2;
	assign u30_output_port = exec_valid & is_csr & exec_is_load;
	assign u32_output_port = exec_valid & is_csr & exec_is_store;
	assign u34_output_port = bus_rsp_if_valid &  ~ exec_valid;
	assign u41_output_port = bus_rsp_if_valid & exec_valid &  ~ is_csr & exec_is_load & exec_mem_access_len == 2'h2;
	assign u48_output_port = bus_rsp_if_valid & exec_valid &  ~ is_csr & exec_is_load & exec_mem_access_len != 2'h2;
	assign u55_output_port = bus_rsp_if_valid & exec_valid &  ~ is_csr & exec_is_store & exec_mem_access_len == 2'h2;
	assign u62_output_port = bus_rsp_if_valid & exec_valid &  ~ is_csr & exec_is_store & exec_mem_access_len != 2'h2;
	assign u65_output_port = bus_rsp_if_valid & exec_valid & is_csr & exec_is_load;
	assign u68_output_port = bus_rsp_if_valid & exec_valid & is_csr & exec_is_store;
	assign u69_output_port = 1'h1;
	assign u71_output_port = bus_req_if_ready &  ~ exec_valid;
	assign u78_output_port = bus_req_if_ready & exec_valid &  ~ is_csr & exec_is_load & exec_mem_access_len == 2'h2;
	assign u85_output_port = bus_req_if_ready & exec_valid &  ~ is_csr & exec_is_load & exec_mem_access_len != 2'h2;
	assign u92_output_port = bus_req_if_ready & exec_valid &  ~ is_csr & exec_is_store & exec_mem_access_len == 2'h2;
	assign u99_output_port = bus_req_if_ready & exec_valid &  ~ is_csr & exec_is_store & exec_mem_access_len != 2'h2;
	assign u102_output_port = bus_req_if_ready & exec_valid & is_csr & exec_is_load;
	assign u105_output_port = bus_req_if_ready & exec_valid & is_csr & exec_is_store;
	assign u106_output_port =  ~ exec_valid;
	assign u112_output_port = exec_valid &  ~ is_csr & exec_is_load & exec_mem_access_len == 2'h2;
	assign u118_output_port = exec_valid &  ~ is_csr & exec_is_load & exec_mem_access_len != 2'h2;
	assign u124_output_port = exec_valid &  ~ is_csr & exec_is_store & exec_mem_access_len == 2'h2;
	assign u130_output_port = exec_valid &  ~ is_csr & exec_is_store & exec_mem_access_len != 2'h2;
	assign u132_output_port = exec_valid & is_csr & exec_is_load;
	assign u134_output_port = exec_valid & is_csr & exec_is_store;
	assign u135_output_port =  ~ exec_valid;
	assign u141_output_port = exec_valid &  ~ is_csr & exec_is_load & exec_mem_access_len == 2'h2;
	assign u147_output_port = exec_valid &  ~ is_csr & exec_is_load & exec_mem_access_len != 2'h2;
	assign u153_output_port = exec_valid &  ~ is_csr & exec_is_store & exec_mem_access_len == 2'h2;
	assign u159_output_port = exec_valid &  ~ is_csr & exec_is_store & exec_mem_access_len != 2'h2;
	assign u161_output_port = exec_valid & is_csr & exec_is_load;
	assign u163_output_port = exec_valid & is_csr & exec_is_store;
	assign exec_ready = exec_ready_1;
	assign u301_output_port = exec_mem_addr[31:1] + exec_mem_access_len[1];
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
	output logic mem_result_data_valid,
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
	logic [32:0] u13_output_port;
	logic [33:0] u19_output_port;
	logic [31:0] u20_output_port;
	logic signed [95:0] u39_output_port;
	logic [31:0] logic_result;
	logic cbranch_result;
	logic bbranch_result;
	logic mem_unaligned;
	logic [32:0] u122_output_port;
	logic [31:0] u146_output_port;
	logic [31:0] u149_output_port;
	logic [30:0] next_inst_addr;
	logic [31:0] u162_output_port;
	logic [31:0] u165_output_port;
	logic u169_output_port;
	logic [31:0] u173_output_port;
	logic [30:0] u182_output_port;
	logic [30:0] u185_output_port;
	logic [31:0] exec_result;
	logic is_branch;
	logic is_exception;
	logic [31:0] u297_output_port;
	logic cancel_write_back;
	logic [11:0] hwi_mask;
	logic u79_output_port;
	logic u82_output_port;
	logic reg_en;
	logic [31:0] u91_output_port;

	assign pc = task_mode_in ? tpc_in : spc_in;
	assign logic_result = decode_opcode == `op__b_sub_a ? decode_op_a | decode_op_b : 32'b0 | decode_opcode == `op__add ? decode_op_a & decode_op_b : 32'b0 | decode_opcode == `op__a_sub_b ?  ~ decode_op_a & decode_op_b : 32'b0 | decode_opcode == `op__addr ? decode_op_a ^ decode_op_b : 32'b0 ;
	always_ff @(posedge clk) u182_output_port <= rst ? 31'h0 : pc;
	always_ff @(posedge clk) u185_output_port <= rst ? 31'h0 : tpc_in;
	always_ff @(posedge clk) u162_output_port <= rst ? 32'h0 : u20_output_port[31:0];
	always_ff @(posedge clk) u165_output_port <= rst ? 32'h0 : u39_output_port[31:0];
	always_ff @(posedge clk) u169_output_port <= rst ? 1'h0 : 1'h0;
	always_ff @(posedge clk) u173_output_port <= rst ? 32'h0 : logic_result;
	assign cbranch_result = decode_opcode == `op__a_sub_b ? decode_op_a == decode_op_b : 1'b0 | decode_opcode == `op__b_sub_a ? decode_op_a != decode_op_b : 1'b0 | decode_opcode == `op__addr ? decode_op_a < decode_op_b : 1'b0 | decode_opcode == `op__pc_add ? decode_op_a >= decode_op_b : 1'b0 | decode_opcode == `op__cb_lt ? decode_op_a < decode_op_b : 1'b0 | decode_opcode == `op__cb_ge ? decode_op_a >= decode_op_b : 1'b0 ;
	assign bbranch_result = decode_opcode == `op__add ? u79_output_port : 1'b0 | decode_opcode == `op__a_sub_b ? u82_output_port : 1'b0 ;
	assign mem_unaligned = decode_exec_unit == `exec__adder & decode_opcode == `op__addr & (decode_mem_access_len == 0 ? 1'h0 : 1'b0 | decode_mem_access_len == 1 ? u91_output_port[0] : 1'b0 | decode_mem_access_len == 2 ? u91_output_port[0] | u91_output_port[1] : 1'b0 | decode_mem_access_len == 3 ? 1'h1 : 1'b0);
	assign is_branch = decode_exec_unit == `exec__adder ? 1'h0 : 1'b0 | decode_exec_unit == `exec__shift ? 1'h0 : 1'b0 | decode_exec_unit == `exec__mult ? 1'h0 : 1'b0 | decode_exec_unit == `exec__bitwise ? 1'h0 : 1'b0 | decode_exec_unit == `exec__cbranch ? cbranch_result : 1'b0 | decode_exec_unit == `exec__bbranch ? bbranch_result : 1'b0 | decode_exec_unit == `exec__misc ? decode_opcode == `op__add ? 1'h1 : 1'b0 | decode_opcode == `op__a_sub_b ? 1'h1 : 1'b0 | decode_opcode == `op__b_sub_a ? 1'h0 : 1'b0 | decode_opcode == `op__addr ? 1'h0 : 1'b0 | decode_opcode == `op__pc_add ? 1'h1 : 1'b0 | decode_opcode == `op__cb_lt ? task_mode_in : 1'b0 | decode_opcode == `op__cb_ge ? 1'h1 : 1'b0 | decode_opcode == `op__misc_tpc_w_r ? task_mode_in : 1'b0  : 1'b0 ;
	always_ff @(posedge clk) do_branch <= rst ? 1'h0 : reg_en ? is_branch : do_branch;
	always_ff @(posedge clk) mem_mem_access_len <= rst ? 2'h0 : reg_en ? decode_mem_access_len : mem_mem_access_len;
	assign exec_result = decode_exec_unit == `exec__adder ? u162_output_port : 32'b0 | decode_exec_unit == `exec__shift ? u165_output_port : 32'b0 | decode_exec_unit == `exec__mult ? u169_output_port : 32'b0 | decode_exec_unit == `exec__bitwise ? u173_output_port : 32'b0 | decode_exec_unit == `exec__misc ? decode_opcode == `op__add ? $signed(1'bX) : 31'b0 | decode_opcode == `op__a_sub_b ? $signed(1'bX) : 31'b0 | decode_opcode == `op__b_sub_a ? u182_output_port : 31'b0 | decode_opcode == `op__addr ? u185_output_port : 31'b0 | decode_opcode == `op__pc_add ? $signed(1'bX) : 31'b0 | decode_opcode == `op__cb_lt ? $signed(1'bX) : 31'b0 | decode_opcode == `op__cb_ge ? $signed(1'bX) : 31'b0 | decode_opcode == `op__misc_tpc_w_r ? $signed(1'bX) : 31'b0  : 32'b0 ;
	always_ff @(posedge clk) mem_result <= rst ? 32'h0 : reg_en ? exec_result : mem_result;
	assign cancel_write_back = is_branch | do_branch & decode_valid & decode_ready;
	always_ff @(posedge clk) mem_result_data_valid <= rst ? 1'h0 : reg_en ? cancel_write_back : mem_result_data_valid;
	always_ff @(posedge clk) mem_result_reg_addr <= rst ? 4'h0 : reg_en ? decode_result_reg_addr : mem_result_reg_addr;
	always_ff @(posedge clk) mem_mem_addr <= rst ? 32'h0 : reg_en ? u20_output_port[31:0] : mem_mem_addr;
	always_ff @(posedge clk) mem_is_load <= rst ? 1'h0 : reg_en ? decode_is_load &  ~ cancel_write_back : mem_is_load;
	always_ff @(posedge clk) mem_is_store <= rst ? 1'h0 : reg_en ? decode_is_store &  ~ cancel_write_back : mem_is_store;
	always_ff @(posedge clk) mem_do_bse <= rst ? 1'h0 : reg_en ? decode_do_bse : mem_do_bse;
	always_ff @(posedge clk) mem_do_wse <= rst ? 1'h0 : reg_en ? decode_do_wse : mem_do_wse;
	always_ff @(posedge clk) mem_do_bze <= rst ? 1'h0 : reg_en ? decode_do_bze : mem_do_bze;
	always_ff @(posedge clk) mem_do_wze <= rst ? 1'h0 : reg_en ? decode_do_wze : mem_do_wze;
	assign is_exception = decode_fetch_av | decode_exec_unit == `exec__adder ? 1'h0 | mem_unaligned : 1'b0 | decode_exec_unit == `exec__shift ? 1'h0 : 1'b0 | decode_exec_unit == `exec__mult ? 1'h0 : 1'b0 | decode_exec_unit == `exec__bitwise ? 1'h0 : 1'b0 | decode_exec_unit == `exec__cbranch ? 1'h0 : 1'b0 | decode_exec_unit == `exec__bbranch ? 1'h0 : 1'b0 | decode_exec_unit == `exec__misc ? decode_opcode == `op__add ? 1'h1 : 1'b0 | decode_opcode == `op__a_sub_b ? 1'h0 : 1'b0 | decode_opcode == `op__b_sub_a ? 1'h0 : 1'b0 | decode_opcode == `op__addr ? 1'h0 : 1'b0 | decode_opcode == `op__pc_add ? 1'h0 : 1'b0 | decode_opcode == `op__cb_lt ? 1'h0 : 1'b0 | decode_opcode == `op__cb_ge ? 1'h0 : 1'b0 | decode_opcode == `op__misc_tpc_w_r ? 1'h0 : 1'b0  : 1'b0 ;
	assign next_inst_addr = decode_exec_unit == `exec__cbranch | decode_exec_unit == `exec__bbranch ? u122_output_port[30:0] : 31'b0 | decode_exec_unit == `exec__misc & decode_opcode == `op__pc_add ? decode_op_imm[31:1] : 31'b0 | decode_exec_unit == `exec__misc & decode_opcode == `op__cb_lt & task_mode_in ? decode_op_imm[31:1] : 31'b0 | decode_exec_unit == `exec__misc & decode_opcode == `op__cb_ge ? decode_op_imm[31:1] : 31'b0 | decode_exec_unit == `exec__misc & decode_opcode == `op__misc_tpc_w_r & task_mode_in ? decode_op_imm[31:1] : 31'b0 | task_mode_in ? u149_output_port[30:0] : u146_output_port[30:0];
	assign spc_out = reg_en ? task_mode_in ? spc_in : is_exception ? 1'h0 : next_inst_addr : spc_in;
	assign tpc_out = reg_en ? task_mode_in ? is_exception ? tpc_in : next_inst_addr : (decode_exec_unit == `exec__misc & decode_opcode == `op__cb_lt) ? decode_op_imm[31:1] : tpc_in : tpc_in;
	assign task_mode_out = reg_en ? task_mode_in ?  ~ is_exception :  ~ (decode_exec_unit == `exec__misc & decode_opcode == `op__a_sub_b) : task_mode_in;
	assign hwi_mask = interrupt ? 12'h800 : 1'h0;
	assign ecause_out = reg_en ? is_exception ? ecause_in | 1'h0 | hwi_mask : ecause_in | hwi_mask : ecause_in;
	assign rcause_out = reg_en ? (is_exception &  ~ task_mode_in) ? rcause_in | 1'h0 : rcause_in : rcause_in;

	DecoratorModule_6 u79 (
		.output_port(u79_output_port),
		.word(decode_op_a),
		.bit_code(decode_op_b)
	);

	DecoratorModule_7 u82 (
		.output_port(u82_output_port),
		.word(decode_op_b),
		.bit_code(decode_op_a)
	);

	ForwardBufLogic handshake_fsm (
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
	assign u13_output_port = decode_op_b + decode_op_imm + 1'h0;
	assign u19_output_port = pc + ({decode_op_a, 1'h0});
	assign u20_output_port = decode_opcode == `op__add ? u3_output_port[31:0] : 32'b0 | decode_opcode == `op__a_sub_b ? u6_output_port[31:0] : 32'b0 | decode_opcode == `op__b_sub_a ? u9_output_port[31:0] : 32'b0 | decode_opcode == `op__addr ? u13_output_port[31:0] : 32'b0 | decode_opcode == `op__pc_add ? u19_output_port[31:0] : 32'b0 ;
	assign u39_output_port = decode_opcode == `op__add ? decode_op_a << decode_op_b[5:0] : 96'b0 | decode_opcode == `op__a_sub_b ? decode_op_a >> decode_op_b[5:0] : 96'b0 | decode_opcode == `op__b_sub_a ? decode_op_a >>> decode_op_b[5:0] : 96'b0 ;
	assign u122_output_port = pc + ({decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[0], decode_op_imm[15:1], 1'h0});
	assign u146_output_port = spc_in + decode_inst_len + 1'h1;
	assign u149_output_port = tpc_in + decode_inst_len + 1'h1;
	assign u297_output_port = decode_op_a & 3'h7;
	assign u91_output_port = u20_output_port[31:0];
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

	output logic [3:0] reg_file_req_read1_addr,
	output logic reg_file_req_read1_valid,
	output logic [3:0] reg_file_req_read2_addr,
	output logic reg_file_req_read2_valid,
	input logic reg_file_req_ready,
	output logic [3:0] reg_file_req_rsv_addr,
	output logic reg_file_req_rsv_valid,
	output logic reg_file_req_valid,

	input logic [31:0] reg_file_rsp_read1_data,
	input logic [31:0] reg_file_rsp_read2_data,
	output logic reg_file_rsp_ready,
	input logic reg_file_rsp_valid,

	input logic do_branch
);

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
	logic [2:0] exec_unit;
	logic [2:0] op_code;
	logic [3:0] rd1_addr;
	logic [3:0] rd2_addr;
	logic [3:0] res_addr;
	logic use_reg_a;
	logic use_reg_b;
	logic [31:0] op_a;
	logic [31:0] op_b;
	logic [31:0] op_imm;
	logic [1:0] mem_len;
	logic is_ld;
	logic is_st;
	logic bse;
	logic wse;
	logic bze;
	logic wze;
	logic read1_needed;
	logic read2_needed;
	logic rsv_needed;
	logic register_outputs;
	logic u2356_output_port;
	logic [31:0] u2357_output_port;
	logic u2359_output_port;
	logic [31:0] u2360_output_port;
	logic [1:0] u2363_output_port;
	logic [3:0] u22_output_port;
	logic [3:0] field_a_plus_one;

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
	assign reg_file_req_valid = fetch_valid &  ~ do_branch;
	assign rd1_addr = mask_for_woi ? fetch_inst_0[3:0] : 4'b0 | mask_for_pc_eq_rd ? fetch_inst_0[15:12] : 4'b0 | mask_for_tpc_eq_rd ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_minus_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_notra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_bse_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_wse_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_xor_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_or_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_and_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_plus_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_minus_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_lsl_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_lsr_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_asr_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_ra_times_rb ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_notra_and_rb ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_ra_eq_0 ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_ra_ne_0 ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_ra_lt_0 ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_ra_ge_0 ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[7:4] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[7:4] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[7:4] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[7:4] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[7:4] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[7:4] : 4'b0 | mask_for_if_rb_eq_ra ? fetch_inst_0[7:4] : 4'b0 | mask_for_if_rb_ne_ra ? fetch_inst_0[7:4] : 4'b0 | mask_for_if_signed_rb_lt_ra ? fetch_inst_0[7:4] : 4'b0 | mask_for_if_signed_rb_ge_ra ? fetch_inst_0[7:4] : 4'b0 | mask_for_if_rb_lt_ra ? fetch_inst_0[7:4] : 4'b0 | mask_for_if_rb_ge_ra ? fetch_inst_0[7:4] : 4'b0 | mask_for_if_ra_bit__eq_1 ? fetch_inst_0[3:0] : 4'b0 ;
	assign reg_file_req_read1_addr = rd1_addr;
	assign read1_needed = mask_for_woi ? 1'h1 : 1'b0 | mask_for_pc_eq_rd ? 1'h1 : 1'b0 | mask_for_tpc_eq_rd ? 1'h1 : 1'b0 | mask_for_rd_eq_minus_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_notra ? 1'h1 : 1'b0 | mask_for_rd_eq_bse_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_wse_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_xor_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_or_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_plus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_minus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsl_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_asr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_times_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_notra_and_rb ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? 1'h1 : 1'b0 | mask_for_if_ra_eq_0 ? 1'h1 : 1'b0 | mask_for_if_ra_ne_0 ? 1'h1 : 1'b0 | mask_for_if_ra_lt_0 ? 1'h1 : 1'b0 | mask_for_if_ra_ge_0 ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | mask_for_if_rb_eq_ra ? 1'h1 : 1'b0 | mask_for_if_rb_ne_ra ? 1'h1 : 1'b0 | mask_for_if_signed_rb_lt_ra ? 1'h1 : 1'b0 | mask_for_if_signed_rb_ge_ra ? 1'h1 : 1'b0 | mask_for_if_rb_lt_ra ? 1'h1 : 1'b0 | mask_for_if_rb_ge_ra ? 1'h1 : 1'b0 | mask_for_if_ra_bit__eq_1 ? 1'h1 : 1'b0 | 1'h0;
	assign rd2_addr = mask_for_woi ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_xor_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_or_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_and_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_plus_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_minus_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_lsl_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_lsr_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_asr_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_ra_times_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_notra_and_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_xor_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_or_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_and_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_plus_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_minus_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_lsl_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_lsr_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_asr_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_times_rb ? fetch_inst_0[7:4] : 4'b0 | mask_for_rd_eq_field_e_xor_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_or_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_and_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_plus_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_minus_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_lsl_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_lsr_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_asr_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_field_e_times_ra ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_ra_gt_0 ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_ra_le_0 ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_rb_eq_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_rb_ne_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_signed_rb_lt_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_signed_rb_ge_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_rb_lt_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_rb_ge_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_if_rb_bit__eq_0 ? fetch_inst_0[7:4] : 4'b0 | mask_for_mem_raplustiny_ofstimes4_eq_rd ? fetch_inst_0[0] : 4'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? fetch_inst_0[0] : 4'b0 | mask_for_rd_eq_mem8_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_mem16_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_mem32_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_memll32_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_mem8_ra_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_mem16_ra_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_mem32_ra_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_memsr32_ra_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_smem8_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_smem16_ra ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_mem8_raplusfield_e ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_mem16_raplusfield_e ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_mem32_raplusfield_e ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_memll32_raplusfield_e ? fetch_inst_0[3:0] : 4'b0 | mask_for_mem8_raplusfield_e_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_mem16_raplusfield_e_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_mem32_raplusfield_e_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_smem8_raplusfield_e ? fetch_inst_0[3:0] : 4'b0 | mask_for_rd_eq_smem16_raplusfield_e ? fetch_inst_0[3:0] : 4'b0 ;
	assign reg_file_req_read2_addr = rd2_addr;
	assign read2_needed = mask_for_woi ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_xor_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_or_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_plus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_minus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsl_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_asr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_times_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_notra_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_xor_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_or_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_plus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_minus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsl_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_asr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_times_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_xor_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_or_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_and_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_plus_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_minus_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsl_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsr_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_asr_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_times_ra ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f ? 1'h1 : 1'b0 | mask_for_if_ra_gt_0 ? 1'h1 : 1'b0 | mask_for_if_ra_le_0 ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | mask_for_if_rb_eq_ra ? 1'h1 : 1'b0 | mask_for_if_rb_ne_ra ? 1'h1 : 1'b0 | mask_for_if_signed_rb_lt_ra ? 1'h1 : 1'b0 | mask_for_if_signed_rb_ge_ra ? 1'h1 : 1'b0 | mask_for_if_rb_lt_ra ? 1'h1 : 1'b0 | mask_for_if_rb_ge_ra ? 1'h1 : 1'b0 | mask_for_if_rb_bit__eq_0 ? 1'h1 : 1'b0 | mask_for_mem_raplustiny_ofstimes4_eq_rd ? 1'h1 : 1'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_ra ? 1'h1 : 1'b0 | mask_for_mem8_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_mem16_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_mem32_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_memsr32_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_raplusfield_e ? 1'h1 : 1'b0 | mask_for_mem8_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem16_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem32_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_raplusfield_e ? 1'h1 : 1'b0 | 1'h0;
	assign res_addr = mask_for_rd_eq_pc ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_tpc ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_tiny_field_a ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_pc_plus_field_atimes2 ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_minus_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_notra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_bse_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_wse_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_xor_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_or_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_and_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_plus_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_minus_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_lsl_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_lsr_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_asr_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_ra_times_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_notra_and_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_value ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_xor_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_or_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_and_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_plus_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_minus_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_lsl_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_lsr_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_asr_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_times_rb ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_short_value ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_xor_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_or_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_and_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_plus_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_minus_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_lsl_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_lsr_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_asr_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_field_e_times_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem8_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem16_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem32_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_memll32_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_smem8_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_smem16_ra ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem8_raplusfield_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem16_raplusfield_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem32_raplusfield_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_memll32_raplusfield_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_smem8_raplusfield_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_smem16_raplusfield_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem8_field_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem16_field_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_mem32_field_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_memll32_field_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_smem8_field_e ? fetch_inst_0[15:12] : 4'b0 | mask_for_rd_eq_smem16_field_e ? fetch_inst_0[15:12] : 4'b0 ;
	assign reg_file_req_rsv_addr = res_addr;
	assign rsv_needed = mask_for_rd_eq_pc ? 1'h1 : 1'b0 | mask_for_rd_eq_tpc ? 1'h1 : 1'b0 | mask_for_rd_eq_tiny_field_a ? 1'h1 : 1'b0 | mask_for_rd_eq_pc_plus_field_atimes2 ? 1'h1 : 1'b0 | mask_for_rd_eq_minus_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_notra ? 1'h1 : 1'b0 | mask_for_rd_eq_bse_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_wse_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_xor_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_or_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_plus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_minus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsl_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_asr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_times_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_notra_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? 1'h1 : 1'b0 | mask_for_rd_eq_value ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_xor_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_or_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_plus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_minus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsl_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_asr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_times_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_short_value ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_xor_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_or_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_and_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_plus_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_minus_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsl_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsr_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_asr_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_times_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_field_e ? 1'h1 : 1'b0 | 1'h0;
	assign op_code = mask_for_swi ? `op__add : 3'b0 | mask_for_stm ? `op__a_sub_b : 3'b0 | mask_for_woi ? `op__a_sub_b : 3'b0 | 1'h1 & fetch_inst_0[15:12] > 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h0 ? `op__add : 3'b0 | mask_for_fence ? `op__b_sub_a : 3'b0 | mask_for_pc_eq_rd ? `op__cb_ge : 3'b0 | mask_for_tpc_eq_rd ? `op__misc_tpc_w_r : 3'b0 | mask_for_rd_eq_pc ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_tpc ? `op__addr : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] > 3'h5 ? `op__add : 3'b0 | mask_for_rd_eq_tiny_field_a ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_pc_plus_field_atimes2 ? `op__pc_add : 3'b0 | mask_for_rd_eq_minus_ra ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_notra ? `op__addr : 3'b0 | mask_for_rd_eq_bse_ra ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_wse_ra ? `op__b_sub_a : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] > 3'h6 &  ~ field_a_is_f ? `op__add : 3'b0 | mask_for_rd_eq_ra_xor_rb ? `op__addr : 3'b0 | mask_for_rd_eq_ra_or_rb ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_ra_and_rb ? `op__add : 3'b0 | mask_for_rd_eq_ra_plus_rb ? `op__add : 3'b0 | mask_for_rd_eq_ra_minus_rb ? `op__a_sub_b : 3'b0 | mask_for_rd_eq_ra_lsl_rb ? `op__add : 3'b0 | mask_for_rd_eq_ra_lsr_rb ? `op__a_sub_b : 3'b0 | mask_for_rd_eq_ra_asr_rb ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_notra_and_rb ? `op__a_sub_b : 3'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? `op__add : 3'b0 | mask_for_rd_eq_value ? `op__b_sub_a : 3'b0 | mask_for_pc_eq_value ? `op__pc_add : 3'b0 | mask_for_tpc_eq_value ? `op__cb_lt : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'h8 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `op__add : 3'b0 | mask_for_rd_eq_field_e_xor_rb ? `op__addr : 3'b0 | mask_for_rd_eq_field_e_or_rb ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_field_e_and_rb ? `op__add : 3'b0 | mask_for_rd_eq_field_e_plus_rb ? `op__add : 3'b0 | mask_for_rd_eq_field_e_minus_rb ? `op__a_sub_b : 3'b0 | mask_for_rd_eq_field_e_lsl_rb ? `op__add : 3'b0 | mask_for_rd_eq_field_e_lsr_rb ? `op__a_sub_b : 3'b0 | mask_for_rd_eq_field_e_asr_rb ? `op__b_sub_a : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf ? `op__add : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf ? `op__add : 3'b0 | mask_for_rd_eq_short_value ? `op__b_sub_a : 3'b0 | mask_for_pc_eq_short_value ? `op__pc_add : 3'b0 | mask_for_tpc_eq_short_value ? `op__cb_lt : 3'b0 | mask_for_rd_eq_field_e_xor_ra ? `op__addr : 3'b0 | mask_for_rd_eq_field_e_or_ra ? `op__b_sub_a : 3'b0 | mask_for_rd_eq_field_e_and_ra ? `op__add : 3'b0 | mask_for_rd_eq_field_e_plus_ra ? `op__add : 3'b0 | mask_for_rd_eq_field_e_minus_ra ? `op__a_sub_b : 3'b0 | mask_for_rd_eq_field_e_lsl_ra ? `op__add : 3'b0 | mask_for_rd_eq_field_e_lsr_ra ? `op__a_sub_b : 3'b0 | mask_for_rd_eq_field_e_asr_ra ? `op__b_sub_a : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? `op__a_sub_b : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? `op__b_sub_a : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? `op__addr : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? `op__pc_add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f ? `op__addr : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f ? `op__pc_add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h6 &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h7 &  ~ field_a_is_f ? `op__add : 3'b0 | mask_for_if_ra_eq_0 ? `op__a_sub_b : 3'b0 | mask_for_if_ra_ne_0 ? `op__b_sub_a : 3'b0 | mask_for_if_ra_lt_0 ? `op__addr : 3'b0 | mask_for_if_ra_ge_0 ? `op__pc_add : 3'b0 | mask_for_if_ra_gt_0 ? `op__addr : 3'b0 | mask_for_if_ra_le_0 ? `op__pc_add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__a_sub_b : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__b_sub_a : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__addr : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__pc_add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__cb_lt : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__cb_ge : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h7 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'h8 &  ~ field_b_is_f &  ~ field_a_is_f ? `op__add : 3'b0 | mask_for_if_rb_eq_ra ? `op__a_sub_b : 3'b0 | mask_for_if_rb_ne_ra ? `op__b_sub_a : 3'b0 | mask_for_if_signed_rb_lt_ra ? `op__addr : 3'b0 | mask_for_if_signed_rb_ge_ra ? `op__pc_add : 3'b0 | mask_for_if_rb_lt_ra ? `op__cb_lt : 3'b0 | mask_for_if_rb_ge_ra ? `op__cb_ge : 3'b0 | mask_for_if_ra_bit__eq_1 ? `op__add : 3'b0 | mask_for_if_rb_bit__eq_0 ? `op__add : 3'b0 | mask_for_mem_raplustiny_ofstimes4_eq_rd ? `op__addr : 3'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? `op__addr : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? `op__add : 3'b0 | mask_for_rd_eq_mem8_ra ? `op__addr : 3'b0 | mask_for_rd_eq_mem16_ra ? `op__addr : 3'b0 | mask_for_rd_eq_mem32_ra ? `op__addr : 3'b0 | mask_for_rd_eq_memll32_ra ? `op__addr : 3'b0 | mask_for_mem8_ra_eq_rd ? `op__addr : 3'b0 | mask_for_mem16_ra_eq_rd ? `op__addr : 3'b0 | mask_for_mem32_ra_eq_rd ? `op__addr : 3'b0 | mask_for_memsr32_ra_eq_rd ? `op__addr : 3'b0 | mask_for_rd_eq_smem8_ra ? `op__addr : 3'b0 | mask_for_rd_eq_smem16_ra ? `op__addr : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | mask_for_rd_eq_mem8_raplusfield_e ? `op__addr : 3'b0 | mask_for_rd_eq_mem16_raplusfield_e ? `op__addr : 3'b0 | mask_for_rd_eq_mem32_raplusfield_e ? `op__addr : 3'b0 | mask_for_rd_eq_memll32_raplusfield_e ? `op__addr : 3'b0 | mask_for_mem8_raplusfield_e_eq_rd ? `op__addr : 3'b0 | mask_for_mem16_raplusfield_e_eq_rd ? `op__addr : 3'b0 | mask_for_mem32_raplusfield_e_eq_rd ? `op__addr : 3'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? `op__addr : 3'b0 | mask_for_rd_eq_smem8_raplusfield_e ? `op__addr : 3'b0 | mask_for_rd_eq_smem16_raplusfield_e ? `op__addr : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `op__add : 3'b0 | mask_for_rd_eq_mem8_field_e ? `op__addr : 3'b0 | mask_for_rd_eq_mem16_field_e ? `op__addr : 3'b0 | mask_for_rd_eq_mem32_field_e ? `op__addr : 3'b0 | mask_for_rd_eq_memll32_field_e ? `op__addr : 3'b0 | mask_for_mem8_field_e_eq_rd ? `op__addr : 3'b0 | mask_for_mem16_field_e_eq_rd ? `op__addr : 3'b0 | mask_for_mem32_field_e_eq_rd ? `op__addr : 3'b0 | mask_for_memsr32_field_e_eq_rd ? `op__addr : 3'b0 | mask_for_rd_eq_smem8_field_e ? `op__addr : 3'b0 | mask_for_rd_eq_smem16_field_e ? `op__addr : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `op__add : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `op__add : 3'b0 | mask_expr ? `op__add : 3'b0 ;
	assign register_outputs = reg_file_req_ready & reg_file_req_valid;
	always_ff @(posedge clk) exec_opcode <= rst ? 3'h0 : register_outputs ? op_code : exec_opcode;
	assign exec_unit = mask_for_swi ? `exec__misc : 3'b0 | mask_for_stm ? `exec__misc : 3'b0 | mask_for_woi ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] > 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h0 ? `exec__misc : 3'b0 | mask_for_fence ? `exec__bitwise : 3'b0 | mask_for_pc_eq_rd ? `exec__misc : 3'b0 | mask_for_tpc_eq_rd ? `exec__misc : 3'b0 | mask_for_rd_eq_pc ? `exec__misc : 3'b0 | mask_for_rd_eq_tpc ? `exec__misc : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] > 3'h5 ? `exec__misc : 3'b0 | mask_for_rd_eq_tiny_field_a ? `exec__bitwise : 3'b0 | mask_for_rd_eq_pc_plus_field_atimes2 ? `exec__adder : 3'b0 | mask_for_rd_eq_minus_ra ? `exec__adder : 3'b0 | mask_for_rd_eq_notra ? `exec__bitwise : 3'b0 | mask_for_rd_eq_bse_ra ? `exec__bitwise : 3'b0 | mask_for_rd_eq_wse_ra ? `exec__bitwise : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] > 3'h6 &  ~ field_a_is_f ? `exec__misc : 3'b0 | mask_for_rd_eq_ra_xor_rb ? `exec__bitwise : 3'b0 | mask_for_rd_eq_ra_or_rb ? `exec__bitwise : 3'b0 | mask_for_rd_eq_ra_and_rb ? `exec__bitwise : 3'b0 | mask_for_rd_eq_ra_plus_rb ? `exec__adder : 3'b0 | mask_for_rd_eq_ra_minus_rb ? `exec__adder : 3'b0 | mask_for_rd_eq_ra_lsl_rb ? `exec__shift : 3'b0 | mask_for_rd_eq_ra_lsr_rb ? `exec__shift : 3'b0 | mask_for_rd_eq_ra_asr_rb ? `exec__shift : 3'b0 | mask_for_rd_eq_ra_times_rb ? `exec__mult : 3'b0 | mask_for_rd_eq_notra_and_rb ? `exec__bitwise : 3'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? `exec__adder : 3'b0 | mask_for_rd_eq_value ? `exec__bitwise : 3'b0 | mask_for_pc_eq_value ? `exec__misc : 3'b0 | mask_for_tpc_eq_value ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'h8 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `exec__misc : 3'b0 | mask_for_rd_eq_field_e_xor_rb ? `exec__bitwise : 3'b0 | mask_for_rd_eq_field_e_or_rb ? `exec__bitwise : 3'b0 | mask_for_rd_eq_field_e_and_rb ? `exec__bitwise : 3'b0 | mask_for_rd_eq_field_e_plus_rb ? `exec__adder : 3'b0 | mask_for_rd_eq_field_e_minus_rb ? `exec__adder : 3'b0 | mask_for_rd_eq_field_e_lsl_rb ? `exec__shift : 3'b0 | mask_for_rd_eq_field_e_lsr_rb ? `exec__shift : 3'b0 | mask_for_rd_eq_field_e_asr_rb ? `exec__shift : 3'b0 | mask_for_rd_eq_field_e_times_rb ? `exec__mult : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf ? `exec__misc : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf ? `exec__misc : 3'b0 | mask_for_rd_eq_short_value ? `exec__bitwise : 3'b0 | mask_for_pc_eq_short_value ? `exec__misc : 3'b0 | mask_for_tpc_eq_short_value ? `exec__misc : 3'b0 | mask_for_rd_eq_field_e_xor_ra ? `exec__bitwise : 3'b0 | mask_for_rd_eq_field_e_or_ra ? `exec__bitwise : 3'b0 | mask_for_rd_eq_field_e_and_ra ? `exec__bitwise : 3'b0 | mask_for_rd_eq_field_e_plus_ra ? `exec__adder : 3'b0 | mask_for_rd_eq_field_e_minus_ra ? `exec__adder : 3'b0 | mask_for_rd_eq_field_e_lsl_ra ? `exec__shift : 3'b0 | mask_for_rd_eq_field_e_lsr_ra ? `exec__shift : 3'b0 | mask_for_rd_eq_field_e_asr_ra ? `exec__shift : 3'b0 | mask_for_rd_eq_field_e_times_ra ? `exec__mult : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h6 &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h7 &  ~ field_a_is_f ? `exec__misc : 3'b0 | mask_for_if_ra_eq_0 ? `exec__cbranch : 3'b0 | mask_for_if_ra_ne_0 ? `exec__cbranch : 3'b0 | mask_for_if_ra_lt_0 ? `exec__cbranch : 3'b0 | mask_for_if_ra_ge_0 ? `exec__cbranch : 3'b0 | mask_for_if_ra_gt_0 ? `exec__cbranch : 3'b0 | mask_for_if_ra_le_0 ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__cbranch : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h7 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'h8 &  ~ field_b_is_f &  ~ field_a_is_f ? `exec__misc : 3'b0 | mask_for_if_rb_eq_ra ? `exec__cbranch : 3'b0 | mask_for_if_rb_ne_ra ? `exec__cbranch : 3'b0 | mask_for_if_signed_rb_lt_ra ? `exec__cbranch : 3'b0 | mask_for_if_signed_rb_ge_ra ? `exec__cbranch : 3'b0 | mask_for_if_rb_lt_ra ? `exec__cbranch : 3'b0 | mask_for_if_rb_ge_ra ? `exec__cbranch : 3'b0 | mask_for_if_ra_bit__eq_1 ? `exec__bbranch : 3'b0 | mask_for_if_rb_bit__eq_0 ? `exec__bbranch : 3'b0 | mask_for_mem_raplustiny_ofstimes4_eq_rd ? `exec__adder : 3'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? `exec__adder : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? `exec__misc : 3'b0 | mask_for_rd_eq_mem8_ra ? `exec__adder : 3'b0 | mask_for_rd_eq_mem16_ra ? `exec__adder : 3'b0 | mask_for_rd_eq_mem32_ra ? `exec__adder : 3'b0 | mask_for_rd_eq_memll32_ra ? `exec__adder : 3'b0 | mask_for_mem8_ra_eq_rd ? `exec__adder : 3'b0 | mask_for_mem16_ra_eq_rd ? `exec__adder : 3'b0 | mask_for_mem32_ra_eq_rd ? `exec__adder : 3'b0 | mask_for_memsr32_ra_eq_rd ? `exec__adder : 3'b0 | mask_for_rd_eq_smem8_ra ? `exec__adder : 3'b0 | mask_for_rd_eq_smem16_ra ? `exec__adder : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | mask_for_rd_eq_mem8_raplusfield_e ? `exec__adder : 3'b0 | mask_for_rd_eq_mem16_raplusfield_e ? `exec__adder : 3'b0 | mask_for_rd_eq_mem32_raplusfield_e ? `exec__adder : 3'b0 | mask_for_rd_eq_memll32_raplusfield_e ? `exec__adder : 3'b0 | mask_for_mem8_raplusfield_e_eq_rd ? `exec__adder : 3'b0 | mask_for_mem16_raplusfield_e_eq_rd ? `exec__adder : 3'b0 | mask_for_mem32_raplusfield_e_eq_rd ? `exec__adder : 3'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? `exec__adder : 3'b0 | mask_for_rd_eq_smem8_raplusfield_e ? `exec__adder : 3'b0 | mask_for_rd_eq_smem16_raplusfield_e ? `exec__adder : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? `exec__misc : 3'b0 | mask_for_rd_eq_mem8_field_e ? `exec__adder : 3'b0 | mask_for_rd_eq_mem16_field_e ? `exec__adder : 3'b0 | mask_for_rd_eq_mem32_field_e ? `exec__adder : 3'b0 | mask_for_rd_eq_memll32_field_e ? `exec__adder : 3'b0 | mask_for_mem8_field_e_eq_rd ? `exec__adder : 3'b0 | mask_for_mem16_field_e_eq_rd ? `exec__adder : 3'b0 | mask_for_mem32_field_e_eq_rd ? `exec__adder : 3'b0 | mask_for_memsr32_field_e_eq_rd ? `exec__adder : 3'b0 | mask_for_rd_eq_smem8_field_e ? `exec__adder : 3'b0 | mask_for_rd_eq_smem16_field_e ? `exec__adder : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `exec__misc : 3'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? `exec__misc : 3'b0 | mask_expr ? `exec__misc : 3'b0 ;
	always_ff @(posedge clk) exec_exec_unit <= rst ? 3'h0 : register_outputs ? exec_unit : exec_exec_unit;
	assign use_reg_a = mask_for_woi ? 1'h1 : 1'b0 | mask_for_pc_eq_rd ? 1'h1 : 1'b0 | mask_for_tpc_eq_rd ? 1'h1 : 1'b0 | mask_for_rd_eq_minus_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_notra ? 1'h1 : 1'b0 | mask_for_rd_eq_bse_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_wse_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_xor_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_or_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_plus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_minus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsl_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_asr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_times_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_notra_and_rb ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? 1'h1 : 1'b0 | mask_for_if_ra_eq_0 ? 1'h1 : 1'b0 | mask_for_if_ra_ne_0 ? 1'h1 : 1'b0 | mask_for_if_ra_lt_0 ? 1'h1 : 1'b0 | mask_for_if_ra_ge_0 ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | mask_for_if_rb_eq_ra ? 1'h1 : 1'b0 | mask_for_if_rb_ne_ra ? 1'h1 : 1'b0 | mask_for_if_signed_rb_lt_ra ? 1'h1 : 1'b0 | mask_for_if_signed_rb_ge_ra ? 1'h1 : 1'b0 | mask_for_if_rb_lt_ra ? 1'h1 : 1'b0 | mask_for_if_rb_ge_ra ? 1'h1 : 1'b0 | mask_for_if_ra_bit__eq_1 ? 1'h1 : 1'b0 | 1'h0;
	assign op_a = mask_for_swi ? fetch_inst_0[3:0] : 32'b0 | 1'h1 & fetch_inst_0[15:12] > 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h0 ? 3'h7 : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] > 3'h5 ? 3'h7 : 32'b0 | mask_for_rd_eq_tiny_field_a ? ones_field_a : 32'b0 | mask_for_rd_eq_pc_plus_field_atimes2 ? ones_field_a : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] > 3'h6 &  ~ field_a_is_f ? 3'h7 : 32'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? ones_field_a : 32'b0 | mask_for_rd_eq_value ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'h8 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? 3'h7 : 32'b0 | mask_for_rd_eq_field_e_xor_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_or_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_and_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_plus_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_minus_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_lsl_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_lsr_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_asr_rb ? field_e : 32'b0 | mask_for_rd_eq_field_e_times_rb ? field_e : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf ? 3'h7 : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf ? 3'h7 : 32'b0 | mask_for_rd_eq_short_value ? field_e : 32'b0 | mask_for_rd_eq_field_e_xor_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_or_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_and_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_plus_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_minus_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_lsl_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_lsr_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_asr_ra ? field_e : 32'b0 | mask_for_rd_eq_field_e_times_ra ? field_e : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f ? 1'h0 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f ? 1'h0 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h6 &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h7 &  ~ field_a_is_f ? 3'h7 : 32'b0 | mask_for_if_ra_gt_0 ? 1'h0 : 32'b0 | mask_for_if_ra_le_0 ? 1'h0 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h7 &  ~ field_b_is_f &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'h8 &  ~ field_b_is_f &  ~ field_a_is_f ? 3'h7 : 32'b0 | mask_for_if_rb_bit__eq_0 ? fetch_inst_0[11:8] : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? 3'h7 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf ? 3'h7 : 32'b0 | mask_expr ? 3'h7 : 32'b0 ;
	always_ff @(posedge clk) u2356_output_port <= rst ? 1'h0 : register_outputs ? use_reg_a : u2356_output_port;
	always_ff @(posedge clk) u2357_output_port <= rst ? 32'h0 : register_outputs ? op_a : u2357_output_port;
	assign exec_op_a = u2356_output_port ? reg_file_rsp_read1_data : u2357_output_port;
	assign use_reg_b = mask_for_woi ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_xor_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_or_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_plus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_minus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsl_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_lsr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_asr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_ra_times_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_notra_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_tiny_rb_plus_field_a ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_xor_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_or_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_and_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_plus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_minus_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsl_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_asr_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_times_rb ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_xor_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_or_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_and_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_plus_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_minus_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsl_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_lsr_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_asr_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_field_e_times_ra ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f ? 1'h1 : 1'b0 | mask_for_if_ra_gt_0 ? 1'h1 : 1'b0 | mask_for_if_ra_le_0 ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? 1'h1 : 1'b0 | mask_for_if_rb_eq_ra ? 1'h1 : 1'b0 | mask_for_if_rb_ne_ra ? 1'h1 : 1'b0 | mask_for_if_signed_rb_lt_ra ? 1'h1 : 1'b0 | mask_for_if_signed_rb_ge_ra ? 1'h1 : 1'b0 | mask_for_if_rb_lt_ra ? 1'h1 : 1'b0 | mask_for_if_rb_ge_ra ? 1'h1 : 1'b0 | mask_for_if_rb_bit__eq_0 ? 1'h1 : 1'b0 | mask_for_mem_raplustiny_ofstimes4_eq_rd ? 1'h1 : 1'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_ra ? 1'h1 : 1'b0 | mask_for_mem8_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_mem16_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_mem32_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_memsr32_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_raplusfield_e ? 1'h1 : 1'b0 | mask_for_mem8_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem16_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem32_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_raplusfield_e ? 1'h1 : 1'b0 | 1'h0;
	assign op_b = mask_for_rd_eq_tiny_field_a ? 1'h0 : 32'b0 | mask_for_rd_eq_pc_plus_field_atimes2 ? 1'h0 : 32'b0 | mask_for_rd_eq_minus_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_notra ? 32'hffffffff : 32'b0 | mask_for_rd_eq_bse_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_wse_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_value ? 1'h0 : 32'b0 | mask_for_rd_eq_short_value ? 1'h0 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? 1'h0 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? 1'h0 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? 1'h0 : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? 1'h0 : 32'b0 | mask_for_if_ra_eq_0 ? 1'h0 : 32'b0 | mask_for_if_ra_ne_0 ? 1'h0 : 32'b0 | mask_for_if_ra_lt_0 ? 1'h0 : 32'b0 | mask_for_if_ra_ge_0 ? 1'h0 : 32'b0 | mask_for_if_ra_bit__eq_1 ? fetch_inst_0[11:8] : 32'b0 | mask_for_rd_eq_mem8_field_e ? 1'h0 : 32'b0 | mask_for_rd_eq_mem16_field_e ? 1'h0 : 32'b0 | mask_for_rd_eq_mem32_field_e ? 1'h0 : 32'b0 | mask_for_rd_eq_memll32_field_e ? 1'h0 : 32'b0 | mask_for_mem8_field_e_eq_rd ? 1'h0 : 32'b0 | mask_for_mem16_field_e_eq_rd ? 1'h0 : 32'b0 | mask_for_mem32_field_e_eq_rd ? 1'h0 : 32'b0 | mask_for_memsr32_field_e_eq_rd ? 1'h0 : 32'b0 | mask_for_rd_eq_smem8_field_e ? 1'h0 : 32'b0 | mask_for_rd_eq_smem16_field_e ? 1'h0 : 32'b0 ;
	always_ff @(posedge clk) u2359_output_port <= rst ? 1'h0 : register_outputs ? use_reg_b : u2359_output_port;
	always_ff @(posedge clk) u2360_output_port <= rst ? 32'h0 : register_outputs ? op_b : u2360_output_port;
	assign exec_op_b = u2359_output_port ? reg_file_rsp_read2_data : u2360_output_port;
	assign op_imm = mask_for_woi ? 1'h0 : 32'b0 | mask_for_pc_eq_value ? field_e : 32'b0 | mask_for_tpc_eq_value ? field_e : 32'b0 | mask_for_pc_eq_short_value ? field_e : 32'b0 | mask_for_tpc_eq_short_value ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f ? field_e : 32'b0 | mask_for_if_ra_eq_0 ? field_e : 32'b0 | mask_for_if_ra_ne_0 ? field_e : 32'b0 | mask_for_if_ra_lt_0 ? field_e : 32'b0 | mask_for_if_ra_ge_0 ? field_e : 32'b0 | mask_for_if_ra_gt_0 ? field_e : 32'b0 | mask_for_if_ra_le_0 ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f ? field_e : 32'b0 | 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f ? field_e : 32'b0 | mask_for_if_rb_eq_ra ? field_e : 32'b0 | mask_for_if_rb_ne_ra ? field_e : 32'b0 | mask_for_if_signed_rb_lt_ra ? field_e : 32'b0 | mask_for_if_signed_rb_ge_ra ? field_e : 32'b0 | mask_for_if_rb_lt_ra ? field_e : 32'b0 | mask_for_if_rb_ge_ra ? field_e : 32'b0 | mask_for_if_ra_bit__eq_1 ? field_e : 32'b0 | mask_for_if_rb_bit__eq_0 ? field_e : 32'b0 | mask_for_mem_raplustiny_ofstimes4_eq_rd ? tiny_ofs : 32'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? tiny_ofs : 32'b0 | mask_for_rd_eq_mem8_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_mem16_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_mem32_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_memll32_ra ? 1'h0 : 32'b0 | mask_for_mem8_ra_eq_rd ? 1'h0 : 32'b0 | mask_for_mem16_ra_eq_rd ? 1'h0 : 32'b0 | mask_for_mem32_ra_eq_rd ? 1'h0 : 32'b0 | mask_for_memsr32_ra_eq_rd ? 1'h0 : 32'b0 | mask_for_rd_eq_smem8_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_smem16_ra ? 1'h0 : 32'b0 | mask_for_rd_eq_mem8_raplusfield_e ? field_e : 32'b0 | mask_for_rd_eq_mem16_raplusfield_e ? field_e : 32'b0 | mask_for_rd_eq_mem32_raplusfield_e ? field_e : 32'b0 | mask_for_rd_eq_memll32_raplusfield_e ? field_e : 32'b0 | mask_for_mem8_raplusfield_e_eq_rd ? field_e : 32'b0 | mask_for_mem16_raplusfield_e_eq_rd ? field_e : 32'b0 | mask_for_mem32_raplusfield_e_eq_rd ? field_e : 32'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? field_e : 32'b0 | mask_for_rd_eq_smem8_raplusfield_e ? field_e : 32'b0 | mask_for_rd_eq_smem16_raplusfield_e ? field_e : 32'b0 | mask_for_rd_eq_mem8_field_e ? field_e : 32'b0 | mask_for_rd_eq_mem16_field_e ? field_e : 32'b0 | mask_for_rd_eq_mem32_field_e ? field_e : 32'b0 | mask_for_rd_eq_memll32_field_e ? field_e : 32'b0 | mask_for_mem8_field_e_eq_rd ? field_e : 32'b0 | mask_for_mem16_field_e_eq_rd ? field_e : 32'b0 | mask_for_mem32_field_e_eq_rd ? field_e : 32'b0 | mask_for_memsr32_field_e_eq_rd ? field_e : 32'b0 | mask_for_rd_eq_smem8_field_e ? field_e : 32'b0 | mask_for_rd_eq_smem16_field_e ? field_e : 32'b0 ;
	always_ff @(posedge clk) exec_op_imm <= rst ? 32'h0 : register_outputs ? op_imm : exec_op_imm;
	assign mem_len = mask_for_mem_raplustiny_ofstimes4_eq_rd ? 2'h3 : 2'b0 | mask_for_rd_eq_mem_raplustiny_ofstimes4 ? 2'h3 : 2'b0 | mask_for_rd_eq_mem8_ra ? 1'h1 : 2'b0 | mask_for_rd_eq_mem16_ra ? 2'h2 : 2'b0 | mask_for_rd_eq_mem32_ra ? 2'h3 : 2'b0 | mask_for_rd_eq_memll32_ra ? 2'h3 : 2'b0 | mask_for_mem8_ra_eq_rd ? 1'h1 : 2'b0 | mask_for_mem16_ra_eq_rd ? 2'h2 : 2'b0 | mask_for_mem32_ra_eq_rd ? 2'h3 : 2'b0 | mask_for_memsr32_ra_eq_rd ? 2'h3 : 2'b0 | mask_for_rd_eq_smem8_ra ? 1'h1 : 2'b0 | mask_for_rd_eq_smem16_ra ? 2'h2 : 2'b0 | mask_for_rd_eq_mem8_raplusfield_e ? 1'h1 : 2'b0 | mask_for_rd_eq_mem16_raplusfield_e ? 2'h2 : 2'b0 | mask_for_rd_eq_mem32_raplusfield_e ? 2'h3 : 2'b0 | mask_for_rd_eq_memll32_raplusfield_e ? 2'h3 : 2'b0 | mask_for_mem8_raplusfield_e_eq_rd ? 1'h1 : 2'b0 | mask_for_mem16_raplusfield_e_eq_rd ? 2'h2 : 2'b0 | mask_for_mem32_raplusfield_e_eq_rd ? 2'h3 : 2'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? 2'h3 : 2'b0 | mask_for_rd_eq_smem8_raplusfield_e ? 1'h1 : 2'b0 | mask_for_rd_eq_smem16_raplusfield_e ? 2'h2 : 2'b0 | mask_for_rd_eq_mem8_field_e ? 1'h1 : 2'b0 | mask_for_rd_eq_mem16_field_e ? 2'h2 : 2'b0 | mask_for_rd_eq_mem32_field_e ? 2'h3 : 2'b0 | mask_for_rd_eq_memll32_field_e ? 2'h3 : 2'b0 | mask_for_mem8_field_e_eq_rd ? 1'h1 : 2'b0 | mask_for_mem16_field_e_eq_rd ? 2'h2 : 2'b0 | mask_for_mem32_field_e_eq_rd ? 2'h3 : 2'b0 | mask_for_memsr32_field_e_eq_rd ? 2'h3 : 2'b0 | mask_for_rd_eq_smem8_field_e ? 1'h1 : 2'b0 | mask_for_rd_eq_smem16_field_e ? 2'h2 : 2'b0 ;
	always_ff @(posedge clk) u2363_output_port <= rst ? 2'h0 : register_outputs ? mem_len : u2363_output_port;
	always_ff @(posedge clk) exec_inst_len <= rst ? 2'h0 : register_outputs ? fetch_inst_len : exec_inst_len;
	assign is_ld = mask_for_rd_eq_mem_raplustiny_ofstimes4 ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem32_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_memll32_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_field_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_field_e ? 1'h1 : 1'b0 | 1'h0;
	always_ff @(posedge clk) exec_is_load <= rst ? 1'h0 : register_outputs ? is_ld : exec_is_load;
	assign is_st = mask_for_mem_raplustiny_ofstimes4_eq_rd ? 1'h1 : 1'b0 | mask_for_mem8_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_mem16_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_mem32_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_memsr32_ra_eq_rd ? 1'h1 : 1'b0 | mask_for_mem8_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem16_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem32_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_memsr32_raplusfield_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem8_field_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem16_field_e_eq_rd ? 1'h1 : 1'b0 | mask_for_mem32_field_e_eq_rd ? 1'h1 : 1'b0 | mask_for_memsr32_field_e_eq_rd ? 1'h1 : 1'b0 | 1'h0;
	always_ff @(posedge clk) exec_is_store <= rst ? 1'h0 : register_outputs ? is_st : exec_is_store;
	assign bse = mask_for_rd_eq_bse_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem8_field_e ? 1'h1 : 1'b0 | 1'h0;
	always_ff @(posedge clk) exec_do_bse <= rst ? 1'h0 : register_outputs ? bse : exec_do_bse;
	assign wse = mask_for_rd_eq_wse_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_smem16_field_e ? 1'h1 : 1'b0 | 1'h0;
	always_ff @(posedge clk) exec_do_wse <= rst ? 1'h0 : register_outputs ? wse : exec_do_wse;
	assign bze = mask_for_rd_eq_mem8_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem8_field_e ? 1'h1 : 1'b0 | 1'h0;
	always_ff @(posedge clk) exec_do_bze <= rst ? 1'h0 : register_outputs ? bze : exec_do_bze;
	assign wze = mask_for_rd_eq_mem16_ra ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_raplusfield_e ? 1'h1 : 1'b0 | mask_for_rd_eq_mem16_field_e ? 1'h1 : 1'b0 | 1'h0;
	always_ff @(posedge clk) exec_do_wze <= rst ? 1'h0 : register_outputs ? wze : exec_do_wze;
	always_ff @(posedge clk) exec_result_reg_addr <= rst ? 4'h0 : register_outputs ? res_addr : exec_result_reg_addr;
	always_ff @(posedge clk) exec_result_reg_addr_valid <= rst ? 1'h0 : register_outputs ? rsv_needed : exec_result_reg_addr_valid;
	always_ff @(posedge clk) exec_fetch_av <= rst ? 1'h0 : register_outputs ? fetch_av : exec_fetch_av;
	assign exec_valid = reg_file_rsp_valid;
	assign reg_file_rsp_ready = exec_ready;
	assign fetch_ready = reg_file_req_ready;
	assign exec_mem_access_len = u2363_output_port;

	assign u36_output_port = fetch_inst_0[3:0] + 1'h1;
	assign mask_for_sii = mask_expr;
	assign expr = mask_expr;
	assign reg_file_req_read1_valid = read1_needed;
	assign reg_file_req_read2_valid = read2_needed;
	assign reg_file_req_rsv_valid = rsv_needed;
	assign u22_output_port = fetch_inst_0[3:0];
endmodule


////////////////////////////////////////////////////////////////////////////////
// FetchStage
////////////////////////////////////////////////////////////////////////////////
module FetchStage (
	input logic clk,
	input logic rst,
	output logic [25:0] bus_if_request_addr,
	output logic [1:0] bus_if_request_byte_en,
	output logic [15:0] bus_if_request_data,
	output logic bus_if_request_dram_not_ext,
	output logic bus_if_request_read_not_write,
	input logic bus_if_request_ready,
	output logic bus_if_request_valid,

	input logic [15:0] bus_if_response_data,
	output logic bus_if_response_ready,
	input logic bus_if_response_valid,

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
	logic [4:0] inst_queue_queue_free_cnt;
	logic inst_buf_queue_ready;
	logic inst_queue_assemble_valid;
	logic inst_queue_assemble_av;
	logic [15:0] inst_queue_assemble_data;
	logic inst_queue_assemble_ready;

	InstBuffer inst_buf (
		.clk(clk),
		.rst(rst),
		.bus_if_request_addr(bus_if_request_addr),
		.bus_if_request_byte_en(bus_if_request_byte_en),
		.bus_if_request_data(bus_if_request_data),
		.bus_if_request_dram_not_ext(bus_if_request_dram_not_ext),
		.bus_if_request_read_not_write(bus_if_request_read_not_write),
		.bus_if_request_ready(bus_if_request_ready),
		.bus_if_request_valid(bus_if_request_valid),

		.bus_if_response_data(bus_if_response_data),
		.bus_if_response_ready(bus_if_response_ready),
		.bus_if_response_valid(bus_if_response_valid),

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

	output logic [4:0] queue_free_cnt,
	output logic assemble_av,
	output logic [15:0] assemble_data,
	input logic assemble_ready,
	output logic assemble_valid,

	input logic do_branch
);

	logic dec;
	logic inc;
	logic signed [6:0] u4_output_port;
	logic [4:0] empty_cnt;

	assign inc = assemble_ready & assemble_valid;
	assign dec = inst_ready & inst_valid;
	always_ff @(posedge clk) empty_cnt <= rst ? 5'h10 : do_branch ? 5'h10 : u4_output_port[4:0];

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
	logic [3:0] next_push_addr;
	logic [3:0] next_pop_addr;
	logic next_looped;
	logic next_empty_or_full;
	logic next_empty;
	logic next_full;
	logic [3:0] push_addr;
	logic [3:0] pop_addr;
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
	assign push_will_wrap = push_addr == 4'hf;
	assign push =  ~ full & input_port_valid;
	assign next_push_addr = push ? 4'(push_will_wrap ? 1'h0 : push_addr + 1'h1) : push_addr;
	assign pop_will_wrap = pop_addr == 4'hf;
	assign pop =  ~ empty & output_port_ready;
	assign next_pop_addr = pop ? 4'(pop_will_wrap ? 1'h0 : pop_addr + 1'h1) : pop_addr;
	assign next_looped = push != 1'h1 & pop != 1'h1 ? looped : 1'b0 | push == 1'h1 & pop != 1'h1 ? push_will_wrap ? 1'h1 : looped : 1'b0 | push != 1'h1 & pop == 1'h1 ? pop_will_wrap ? 1'h0 : looped : 1'b0 | push == 1'h1 & pop == 1'h1 ? push_will_wrap != 1'h1 & pop_will_wrap != 1'h1 ? looped : 1'b0 | push_will_wrap == 1'h1 & pop_will_wrap != 1'h1 ? 1'h1 : 1'b0 | push_will_wrap != 1'h1 & pop_will_wrap == 1'h1 ? 1'h0 : 1'b0 | push_will_wrap == 1'h1 & pop_will_wrap == 1'h1 ? looped : 1'b0  : 1'b0 ;
	assign next_empty_or_full = next_push_addr == next_pop_addr;
	assign next_empty = next_empty_or_full ?  ~ next_looped : 1'h0;
	assign next_full = next_empty_or_full ? next_looped : 1'h0;
	always_ff @(posedge clock_port) push_addr <= reset_port ? 4'h0 : clear ? 1'h0 : next_push_addr;
	always_ff @(posedge clock_port) pop_addr <= reset_port ? 4'h0 : clear ? 1'h0 : next_pop_addr;
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
	input logic [3:0] port1_addr,
	input logic port1_clk,
	input logic [3:0] port2_addr,
	input logic port2_clk,
	input logic port1_data_in_av,
	input logic [15:0] port1_data_in_data,

	input logic port1_write_en,
	output logic port2_data_out_av,
	output logic [15:0] port2_data_out_data
);

	logic [16:0] real_mem_port2_data_out;

	reg [16:0] mem[0:15];

	always @(posedge port1_clk) begin
		if (port1_write_en) begin
			mem[port1_addr] <= {port1_data_in_av, port1_data_in_data};
		end
	end

	always @(posedge port1_clk) begin
		real_mem_port2_data_out <= mem[port2_addr];
	end

	assign {port2_data_out_av, port2_data_out_data} = real_mem_port2_data_out;
endmodule


////////////////////////////////////////////////////////////////////////////////
// InstBuffer
////////////////////////////////////////////////////////////////////////////////
module InstBuffer (
	input logic clk,
	input logic rst,
	output logic [25:0] bus_if_request_addr,
	output logic [1:0] bus_if_request_byte_en,
	output logic [15:0] bus_if_request_data,
	output logic bus_if_request_dram_not_ext,
	output logic bus_if_request_read_not_write,
	input logic bus_if_request_ready,
	output logic bus_if_request_valid,

	input logic [15:0] bus_if_response_data,
	output logic bus_if_response_ready,
	input logic bus_if_response_valid,

	output logic queue_av,
	output logic [15:0] queue_data,
	input logic queue_ready,
	output logic queue_valid,

	input logic [4:0] queue_free_cnt,
	input logic [21:0] mem_base,
	input logic [21:0] mem_limit,
	input logic [30:0] spc,
	input logic [30:0] tpc,
	input logic task_mode,
	input logic do_branch
);

	logic advance_request;
	logic advance_response;
	logic [32:0] u4_output_port;
	logic [30:0] branch_target;
	logic task_mode_fetch;
	logic [31:0] u9_output_port;
	logic [30:0] fetch_addr;
	logic fetch_page_limit;
	logic fetch_av;
	logic start_new_request;
	logic signed [5:0] u31_output_port;
	logic [4:0] req_len;
	logic req_av;
	logic flushing;
	logic u66_output_port;
	logic u69_output_port;
	logic u71_output_port;
	logic [1:0] state;
	logic [1:0] fsm_next_state;
	logic branch_req;

	assign branch_target = task_mode ? u4_output_port[30:0] : spc;
	always_ff @(posedge clk) task_mode_fetch <= rst ? 1'h0 : do_branch ? task_mode : task_mode_fetch;
	assign advance_request = bus_if_request_valid & bus_if_request_ready;
	always_ff @(posedge clk) fetch_addr <= rst ? 31'h0 : do_branch ? branch_target : u9_output_port[30:0];
	always_ff @(posedge clk) fetch_page_limit <= rst ? 1'h0 :  ~ fetch_addr[7:0] == 1'h0;
	assign start_new_request = (queue_free_cnt >= 4'h8 | do_branch) & state == `InstBufferStates__idle;
	always_ff @(posedge clk) req_len <= rst ? 4'h8 : start_new_request ? do_branch ? 4'h8 : queue_free_cnt : advance_request ? req_len > 1'h0 ? u31_output_port[4:0] : 1'h0 : req_len;
	assign fetch_av = task_mode_fetch & fetch_addr[30:10] > mem_limit;
	always_ff @(posedge clk) req_av <= rst ? 1'h0 : start_new_request ? fetch_av : req_av;
	assign advance_response = bus_if_response_valid & bus_if_response_ready;
	always_ff @(posedge clk) flushing <= rst ? 1'h0 : (do_branch | state == `InstBufferStates__flush_start) ? 1'h1 : advance_response ? flushing : 1'h0;
	assign bus_if_request_valid = state == `InstBufferStates__request &  ~ fetch_page_limit;
	assign bus_if_request_dram_not_ext = fetch_addr[22];
	assign queue_valid = bus_if_response_valid &  ~ flushing;
	assign bus_if_request_byte_en = 2'h3;
	assign bus_if_request_data = 16'hx;
	assign bus_if_request_read_not_write = 1'h1;
	assign bus_if_response_ready = 1'h1;
	assign queue_data = bus_if_response_data;
	assign bus_if_request_addr = fetch_addr[21:0];

	FSM_2 fsm (
		.clock_port(clk),
		.reset_port(rst),
		.reset_value(`InstBufferStates__idle),
		.state(state),
		.next_state(fsm_next_state),
		.default_state(`InstBufferStates__idle),
		.input_idle_to_request(start_new_request),
		.input_request_to_idle(u66_output_port),
		.input_request_to_flush_start(u69_output_port),
		.input_request_to_idle_1(u71_output_port),
		.input_flush_start_to_idle(advance_response)
	);

	assign u4_output_port = tpc + (mem_base << 4'ha);
	assign u9_output_port = fetch_addr + advance_request;
	assign u31_output_port = req_len - 1'h1;
	assign queue_av = req_av;
	assign u66_output_port =  ~ do_branch & ( ~ advance_request | req_len == 1'h0);
	assign u69_output_port = do_branch & advance_request &  ~ advance_response;
	assign u71_output_port = do_branch & advance_request & advance_response;
	assign branch_req = do_branch;
endmodule


////////////////////////////////////////////////////////////////////////////////
// BusIf
////////////////////////////////////////////////////////////////////////////////
module BusIf (
	input logic clk,
	input logic rst,
	input logic [25:0] fetch_request_addr,
	input logic [1:0] fetch_request_byte_en,
	input logic [15:0] fetch_request_data,
	input logic fetch_request_dram_not_ext,
	input logic fetch_request_read_not_write,
	output logic fetch_request_ready,
	input logic fetch_request_valid,

	output logic [15:0] fetch_response_data,
	input logic fetch_response_ready,
	output logic fetch_response_valid,

	input logic [25:0] mem_request_addr,
	input logic [1:0] mem_request_byte_en,
	input logic [15:0] mem_request_data,
	input logic mem_request_dram_not_ext,
	input logic mem_request_read_not_write,
	output logic mem_request_ready,
	input logic mem_request_valid,

	output logic [15:0] mem_response_data,
	input logic mem_response_ready,
	output logic mem_response_valid,

	output logic [10:0] dram_addr,
	input logic [7:0] dram_data_in,
	output logic [7:0] dram_data_out,
	output logic dram_nCAS_a,
	output logic dram_nCAS_b,
	output logic dram_nNREN,
	output logic dram_nRAS,
	input logic dram_nWAIT,
	output logic dram_nWE,

	input logic ext_req,
	output logic ext_grnt
);

	logic req_ready;
	logic req_valid;
	logic start;
	logic [25:0] req_addr;
	logic [15:0] req_data;
	logic req_read_not_write;
	logic [1:0] req_byte_en;
	logic req_dram_not_ext;
	logic u27_output_port;
	logic arb_mem_not_fetch;
	logic u30_output_port;
	logic u32_output_port;
	logic u33_output_port;
	logic u34_output_port;
	logic u35_output_port;
	logic u36_output_port;
	logic u37_output_port;
	logic u38_output_port;
	logic u39_output_port;
	logic u40_output_port;
	logic signed [4:0] u43_output_port;
	logic [3:0] wait_states;
	logic [10:0] input_row_addr;
	logic [10:0] row_addr;
	logic [10:0] col_addr;
	logic read_not_write;
	logic [1:0] byte_en;
	logic [15:0] data_out;
	logic dram_not_ext;
	logic DRAM_nRAS;
	logic nNREN;
	logic NR_nCAS_a;
	logic NR_nCAS_b;
	logic CAS_nWINDOW_A_a;
	logic CAS_nWINDOW_A_b;
	logic CAS_nWINDOW_C_a;
	logic CAS_nWINDOW_B_a;
	logic CAS_nWINDOW_C_b;
	logic CAS_nWINDOW_B_b;
	logic DRAM_nCAS_a;
	logic DRAM_nCAS_b;
	logic [10:0] u169_output_port;
	logic [7:0] data_out_low;
	logic [7:0] data_out_high;
	logic read_active;
	logic [7:0] data_in_low;
	logic [7:0] data_in_high;
	logic [15:0] resp_data;
	logic u193_output_port;
	logic u197_output_port;
	logic [2:0] state;
	logic [2:0] next_state;

	assign req_ready = state == `BusIfStates__idle | state == `BusIfStates__first | state == `BusIfStates__middle;
	assign mem_request_ready = req_ready & arb_mem_not_fetch;
	assign fetch_request_ready = req_ready &  ~ arb_mem_not_fetch;
	assign req_valid = arb_mem_not_fetch ? mem_request_valid : fetch_request_valid;
	assign start = state == `BusIfStates__idle & (arb_mem_not_fetch ? mem_request_valid : fetch_request_valid);
	assign req_addr = arb_mem_not_fetch ? mem_request_addr : fetch_request_addr;
	assign req_data = arb_mem_not_fetch ? mem_request_data : fetch_request_data;
	assign req_read_not_write = arb_mem_not_fetch ? mem_request_read_not_write : fetch_request_read_not_write;
	assign req_byte_en = arb_mem_not_fetch ? mem_request_byte_en : fetch_request_byte_en;
	assign req_dram_not_ext = arb_mem_not_fetch ? mem_request_dram_not_ext : fetch_request_dram_not_ext;
	always_ff @(posedge clk) u27_output_port <= rst ? 1'h0 : state == `BusIfStates__idle ? mem_request_valid : u27_output_port;
	assign arb_mem_not_fetch = state == `BusIfStates__idle ? mem_request_valid : u27_output_port;
	always_ff @(posedge clk) wait_states <= rst ? 4'h0 : start ? req_addr[25:22] : wait_states == 1'h0 ? 1'h0 : u43_output_port[3:0];
	assign input_row_addr = {req_addr[21], req_addr[19], req_addr[17], req_addr[15:8]};
	always_ff @(posedge clk) row_addr <= rst ? 11'h0 : start ? input_row_addr : row_addr;
	always_ff @(posedge clk) col_addr <= rst ? 11'h0 : req_valid & req_ready ? ({req_addr[20], req_addr[18], req_addr[16], req_addr[7:0]}) : col_addr;
	always_ff @(posedge clk) read_not_write <= rst ? 1'h0 : req_valid ? req_read_not_write : read_not_write;
	always_ff @(posedge clk) byte_en <= rst ? 2'h0 : req_valid ? req_byte_en : byte_en;
	always_ff @(posedge clk) data_out <= rst ? 16'h0 : req_valid ? req_data : data_out;
	always_ff @(posedge clk) dram_not_ext <= rst ? 1'h0 : req_valid ? req_dram_not_ext : dram_not_ext;
	always_ff @(posedge clk) DRAM_nRAS <= rst ? 1'h1 : next_state == `BusIfStates__idle | next_state == `BusIfStates__external | next_state == `BusIfStates__non_dram_first | next_state == `BusIfStates__non_dram_wait | next_state == `BusIfStates__pre_external;
	always_ff @(posedge clk) nNREN <= rst ? 1'h1 : next_state != `BusIfStates__non_dram_first & next_state != `BusIfStates__non_dram_wait;
	always_ff @(posedge clk) NR_nCAS_a <= rst ? 1'h1 : state != (`BusIfStates__non_dram_first |  ~ byte_en[0]);
	always_ff @(posedge clk) NR_nCAS_b <= rst ? 1'h1 : state != (`BusIfStates__non_dram_first |  ~ byte_en[1]);
	always_ff @(posedge clk) CAS_nWINDOW_A_a <= rst ? 1'h1 :  ~ byte_en[0] | next_state == `BusIfStates__idle | next_state == `BusIfStates__precharge | next_state == `BusIfStates__pre_external | next_state == `BusIfStates__non_dram_first | next_state == `BusIfStates__non_dram_wait;
	always_ff @(posedge clk) CAS_nWINDOW_A_b <= rst ? 1'h1 :  ~ byte_en[1] | next_state == `BusIfStates__idle | next_state == `BusIfStates__precharge | next_state == `BusIfStates__pre_external | next_state == `BusIfStates__non_dram_first | next_state == `BusIfStates__non_dram_wait;
	always_ff @(posedge clk) CAS_nWINDOW_C_a <= rst ? 1'h1 : CAS_nWINDOW_A_a;
	always_ff @(negedge clk) CAS_nWINDOW_B_a <= rst ? 1'h1 : CAS_nWINDOW_A_a;
	always_ff @(posedge clk) CAS_nWINDOW_C_b <= rst ? 1'h1 : CAS_nWINDOW_A_b;
	always_ff @(negedge clk) CAS_nWINDOW_B_b <= rst ? 1'h1 : CAS_nWINDOW_A_b;
	assign DRAM_nCAS_a = CAS_nWINDOW_A_a | CAS_nWINDOW_B_a | clk;
	assign dram_nCAS_a = DRAM_nCAS_a & NR_nCAS_a;
	assign DRAM_nCAS_b = CAS_nWINDOW_B_b | CAS_nWINDOW_C_b |  ~ clk;
	assign dram_nCAS_b = DRAM_nCAS_b & NR_nCAS_b;
	always_ff @(negedge clk) u169_output_port <= rst ? 11'h0 : col_addr;
	assign dram_addr = ((state == `BusIfStates__first | state == `BusIfStates__non_dram_first) & clk) ? row_addr : u169_output_port;
	always_ff @(negedge clk) data_out_low <= rst ? 8'h0 : data_out[7:0];
	always_ff @(posedge clk) data_out_high <= rst ? 8'h0 : data_out[15:8];
	assign dram_data_out = clk ? data_out_high : data_out_low;
	assign read_active = state != `BusIfStates__idle & state != `BusIfStates__precharge & state != `BusIfStates__pre_external & state != `BusIfStates__external & read_not_write;
	always_ff @(posedge clk) data_in_low <= rst ? 8'h0 : dram_data_in;
	always_ff @(negedge clk) data_in_high <= rst ? 8'h0 : dram_data_in;
	always_ff @(posedge clk) resp_data <= rst ? 16'h0 : ({data_in_high, data_in_low});
	always_ff @(posedge clk) u193_output_port <= rst ? 1'h0 : read_active & arb_mem_not_fetch;
	always_ff @(posedge clk) mem_response_valid <= rst ? 1'h0 : u193_output_port;
	always_ff @(posedge clk) u197_output_port <= rst ? 1'h0 : read_active &  ~ arb_mem_not_fetch;
	always_ff @(posedge clk) fetch_response_valid <= rst ? 1'h0 : u197_output_port;
	assign fetch_response_data = resp_data;

	FSM fsm (
		.clock_port(clk),
		.reset_port(rst),
		.reset_value(`BusIfStates__idle),
		.state(state),
		.next_state(next_state),
		.default_state(`BusIfStates__idle),
		.input_idle_to_external(u30_output_port),
		.input_idle_to_non_dram_first(u32_output_port),
		.input_idle_to_first(u33_output_port),
		.input_external_to_idle(u34_output_port),
		.input_first_to_precharge(u35_output_port),
		.input_first_to_middle(req_valid),
		.input_middle_to_precharge(u36_output_port),
		.input_precharge_to_idle(u37_output_port),
		.input_precharge_to_pre_external(ext_req),
		.input_pre_external_to_external(ext_req),
		.input_pre_external_to_idle(u38_output_port),
		.input_non_dram_first_to_non_dram_wait(u39_output_port),
		.input_non_dram_wait_to_non_dram_wait(u40_output_port),
		.input_non_dram_wait_to_idle(dram_nWAIT)
	);

	GenericAssertOnClk u72 (
		.clk(clk),
		.rst(rst),
		.input_port(input_row_addr == row_addr | state == `BusIfStates__idle)
	);

	assign ext_grnt = 1'hx;
	assign u30_output_port = ext_req &  ~ req_valid;
	assign u32_output_port = req_valid &  ~ req_dram_not_ext;
	assign u33_output_port = req_valid & req_dram_not_ext;
	assign u34_output_port =  ~ ext_req;
	assign u35_output_port =  ~ req_valid;
	assign u36_output_port =  ~ req_valid;
	assign u37_output_port =  ~ ext_req;
	assign u38_output_port =  ~ ext_req;
	assign u39_output_port = 1'h1;
	assign u40_output_port =  ~ dram_nWAIT;
	assign u43_output_port = wait_states - 1'h1;
	assign dram_nWE = read_not_write;
	assign dram_nRAS = DRAM_nRAS;
	assign dram_nNREN = nNREN;
	assign mem_response_data = resp_data;
endmodule


////////////////////////////////////////////////////////////////////////////////
// GenericAssertOnClk
////////////////////////////////////////////////////////////////////////////////
module GenericAssertOnClk (
	input logic clk,
	input logic rst,
	input logic input_port
);

endmodule


////////////////////////////////////////////////////////////////////////////////
// FSM_4
////////////////////////////////////////////////////////////////////////////////
module FSM_4 (
	input logic clock_port,
	input logic reset_port,
	input logic [2:0] reset_value,
	output logic [2:0] state,
	output logic [2:0] next_state,
	input logic [2:0] default_state,
	input logic input_idle_to_mem_read_1,
	input logic input_idle_to_mem_read_2,
	input logic input_idle_to_mem_write_1,
	input logic input_idle_to_mem_write_2,
	input logic input_idle_to_csr_read,
	input logic input_idle_to_csr_write,
	input logic input_mem_read_1_to_mem_read_2,
	input logic input_mem_read_2_to_idle,
	input logic input_mem_read_2_to_mem_read_1,
	input logic input_mem_read_2_to_mem_read_2,
	input logic input_mem_read_2_to_mem_write_1,
	input logic input_mem_read_2_to_mem_write_2,
	input logic input_mem_read_2_to_csr_read,
	input logic input_mem_read_2_to_csr_write,
	input logic input_mem_write_1_to_mem_write_2,
	input logic input_mem_write_2_to_idle,
	input logic input_mem_write_2_to_mem_read_1,
	input logic input_mem_write_2_to_mem_read_2,
	input logic input_mem_write_2_to_mem_write_1,
	input logic input_mem_write_2_to_mem_write_2,
	input logic input_mem_write_2_to_csr_read,
	input logic input_mem_write_2_to_csr_write,
	input logic input_csr_read_to_idle,
	input logic input_csr_read_to_mem_read_1,
	input logic input_csr_read_to_mem_read_2,
	input logic input_csr_read_to_mem_write_1,
	input logic input_csr_read_to_mem_write_2,
	input logic input_csr_read_to_csr_read,
	input logic input_csr_read_to_csr_write,
	input logic input_csr_write_to_idle,
	input logic input_csr_write_to_mem_read_1,
	input logic input_csr_write_to_mem_read_2,
	input logic input_csr_write_to_mem_write_1,
	input logic input_csr_write_to_mem_write_2,
	input logic input_csr_write_to_csr_read,
	input logic input_csr_write_to_csr_write
);

	logic [2:0] local_state;
	logic [2:0] local_next_state;

	always_ff @(posedge clock_port) local_state <= reset_port ? reset_value : local_next_state;

	FSMLogic_4 fsm_logic (
		.state(local_state),
		.next_state(local_next_state),
		.default_state(default_state),
		.input_idle_to_mem_read_1(input_idle_to_mem_read_1),
		.input_idle_to_mem_read_2(input_idle_to_mem_read_2),
		.input_idle_to_mem_write_1(input_idle_to_mem_write_1),
		.input_idle_to_mem_write_2(input_idle_to_mem_write_2),
		.input_idle_to_csr_read(input_idle_to_csr_read),
		.input_idle_to_csr_write(input_idle_to_csr_write),
		.input_mem_read_1_to_mem_read_2(input_mem_read_1_to_mem_read_2),
		.input_mem_read_2_to_idle(input_mem_read_2_to_idle),
		.input_mem_read_2_to_mem_read_1(input_mem_read_2_to_mem_read_1),
		.input_mem_read_2_to_mem_read_2(input_mem_read_2_to_mem_read_2),
		.input_mem_read_2_to_mem_write_1(input_mem_read_2_to_mem_write_1),
		.input_mem_read_2_to_mem_write_2(input_mem_read_2_to_mem_write_2),
		.input_mem_read_2_to_csr_read(input_mem_read_2_to_csr_read),
		.input_mem_read_2_to_csr_write(input_mem_read_2_to_csr_write),
		.input_mem_write_1_to_mem_write_2(input_mem_write_1_to_mem_write_2),
		.input_mem_write_2_to_idle(input_mem_write_2_to_idle),
		.input_mem_write_2_to_mem_read_1(input_mem_write_2_to_mem_read_1),
		.input_mem_write_2_to_mem_read_2(input_mem_write_2_to_mem_read_2),
		.input_mem_write_2_to_mem_write_1(input_mem_write_2_to_mem_write_1),
		.input_mem_write_2_to_mem_write_2(input_mem_write_2_to_mem_write_2),
		.input_mem_write_2_to_csr_read(input_mem_write_2_to_csr_read),
		.input_mem_write_2_to_csr_write(input_mem_write_2_to_csr_write),
		.input_csr_read_to_idle(input_csr_read_to_idle),
		.input_csr_read_to_mem_read_1(input_csr_read_to_mem_read_1),
		.input_csr_read_to_mem_read_2(input_csr_read_to_mem_read_2),
		.input_csr_read_to_mem_write_1(input_csr_read_to_mem_write_1),
		.input_csr_read_to_mem_write_2(input_csr_read_to_mem_write_2),
		.input_csr_read_to_csr_read(input_csr_read_to_csr_read),
		.input_csr_read_to_csr_write(input_csr_read_to_csr_write),
		.input_csr_write_to_idle(input_csr_write_to_idle),
		.input_csr_write_to_mem_read_1(input_csr_write_to_mem_read_1),
		.input_csr_write_to_mem_read_2(input_csr_write_to_mem_read_2),
		.input_csr_write_to_mem_write_1(input_csr_write_to_mem_write_1),
		.input_csr_write_to_mem_write_2(input_csr_write_to_mem_write_2),
		.input_csr_write_to_csr_read(input_csr_write_to_csr_read),
		.input_csr_write_to_csr_write(input_csr_write_to_csr_write)
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
	input logic [1:0] reset_value,
	output logic [1:0] state,
	output logic [1:0] next_state,
	input logic [1:0] default_state,
	input logic input_idle_to_request,
	input logic input_request_to_idle,
	input logic input_request_to_flush_start,
	input logic input_request_to_idle_1,
	input logic input_flush_start_to_idle
);

	logic [1:0] local_state;
	logic [1:0] local_next_state;

	always_ff @(posedge clock_port) local_state <= reset_port ? reset_value : local_next_state;

	FSMLogic_2 fsm_logic (
		.state(local_state),
		.next_state(local_next_state),
		.default_state(default_state),
		.input_idle_to_request(input_idle_to_request),
		.input_request_to_idle(input_request_to_idle),
		.input_request_to_flush_start(input_request_to_flush_start),
		.input_request_to_idle_1(input_request_to_idle_1),
		.input_flush_start_to_idle(input_flush_start_to_idle)
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
	input logic [2:0] reset_value,
	output logic [2:0] state,
	output logic [2:0] next_state,
	input logic [2:0] default_state,
	input logic input_idle_to_external,
	input logic input_idle_to_non_dram_first,
	input logic input_idle_to_first,
	input logic input_external_to_idle,
	input logic input_first_to_precharge,
	input logic input_first_to_middle,
	input logic input_middle_to_precharge,
	input logic input_precharge_to_idle,
	input logic input_precharge_to_pre_external,
	input logic input_pre_external_to_external,
	input logic input_pre_external_to_idle,
	input logic input_non_dram_first_to_non_dram_wait,
	input logic input_non_dram_wait_to_non_dram_wait,
	input logic input_non_dram_wait_to_idle
);

	logic [2:0] local_state;
	logic [2:0] local_next_state;

	always_ff @(posedge clock_port) local_state <= reset_port ? reset_value : local_next_state;

	FSMLogic fsm_logic (
		.state(local_state),
		.next_state(local_next_state),
		.default_state(default_state),
		.input_idle_to_external(input_idle_to_external),
		.input_idle_to_non_dram_first(input_idle_to_non_dram_first),
		.input_idle_to_first(input_idle_to_first),
		.input_external_to_idle(input_external_to_idle),
		.input_first_to_precharge(input_first_to_precharge),
		.input_first_to_middle(input_first_to_middle),
		.input_middle_to_precharge(input_middle_to_precharge),
		.input_precharge_to_idle(input_precharge_to_idle),
		.input_precharge_to_pre_external(input_precharge_to_pre_external),
		.input_pre_external_to_external(input_precharge_to_pre_external),
		.input_pre_external_to_idle(input_pre_external_to_idle),
		.input_non_dram_first_to_non_dram_wait(input_non_dram_first_to_non_dram_wait),
		.input_non_dram_wait_to_non_dram_wait(input_non_dram_wait_to_non_dram_wait),
		.input_non_dram_wait_to_idle(input_non_dram_wait_to_idle)
	);

	assign state = local_state;
	assign next_state = local_next_state;
endmodule


////////////////////////////////////////////////////////////////////////////////
// FSMLogic_4
////////////////////////////////////////////////////////////////////////////////
module FSMLogic_4 (
	input logic [2:0] state,
	output logic [2:0] next_state,
	input logic [2:0] default_state,
	input logic input_idle_to_mem_read_1,
	input logic input_idle_to_mem_read_2,
	input logic input_idle_to_mem_write_1,
	input logic input_idle_to_mem_write_2,
	input logic input_idle_to_csr_read,
	input logic input_idle_to_csr_write,
	input logic input_mem_read_1_to_mem_read_2,
	input logic input_mem_read_2_to_idle,
	input logic input_mem_read_2_to_mem_read_1,
	input logic input_mem_read_2_to_mem_read_2,
	input logic input_mem_read_2_to_mem_write_1,
	input logic input_mem_read_2_to_mem_write_2,
	input logic input_mem_read_2_to_csr_read,
	input logic input_mem_read_2_to_csr_write,
	input logic input_mem_write_1_to_mem_write_2,
	input logic input_mem_write_2_to_idle,
	input logic input_mem_write_2_to_mem_read_1,
	input logic input_mem_write_2_to_mem_read_2,
	input logic input_mem_write_2_to_mem_write_1,
	input logic input_mem_write_2_to_mem_write_2,
	input logic input_mem_write_2_to_csr_read,
	input logic input_mem_write_2_to_csr_write,
	input logic input_csr_read_to_idle,
	input logic input_csr_read_to_mem_read_1,
	input logic input_csr_read_to_mem_read_2,
	input logic input_csr_read_to_mem_write_1,
	input logic input_csr_read_to_mem_write_2,
	input logic input_csr_read_to_csr_read,
	input logic input_csr_read_to_csr_write,
	input logic input_csr_write_to_idle,
	input logic input_csr_write_to_mem_read_1,
	input logic input_csr_write_to_mem_read_2,
	input logic input_csr_write_to_mem_write_1,
	input logic input_csr_write_to_mem_write_2,
	input logic input_csr_write_to_csr_read,
	input logic input_csr_write_to_csr_write
);

	logic [2:0] state_idle_selector;
	logic [2:0] state_mem_read_1_selector;
	logic [2:0] state_mem_read_2_selector;
	logic [2:0] state_mem_write_1_selector;
	logic [2:0] state_mem_write_2_selector;
	logic [2:0] state_csr_read_selector;
	logic [2:0] state_csr_write_selector;

	assign state_idle_selector = input_idle_to_mem_read_1 ? `MemoryStates__mem_read_1 : 3'b0 | input_idle_to_mem_read_2 ? `MemoryStates__mem_read_2 : 3'b0 | input_idle_to_mem_write_1 ? `MemoryStates__mem_write_1 : 3'b0 | input_idle_to_mem_write_2 ? `MemoryStates__mem_write_2 : 3'b0 | input_idle_to_csr_read ? `MemoryStates__csr_read : 3'b0 | input_idle_to_csr_write ? `MemoryStates__csr_write : 3'b0 | `MemoryStates__idle;
	assign state_mem_read_1_selector = input_mem_read_1_to_mem_read_2 ? `MemoryStates__mem_read_2 : 3'b0 | `MemoryStates__mem_read_1;
	assign state_mem_read_2_selector = input_mem_read_2_to_idle ? `MemoryStates__idle : 3'b0 | input_mem_read_2_to_mem_read_1 ? `MemoryStates__mem_read_1 : 3'b0 | input_mem_read_2_to_mem_read_2 ? `MemoryStates__mem_read_2 : 3'b0 | input_mem_read_2_to_mem_write_1 ? `MemoryStates__mem_write_1 : 3'b0 | input_mem_read_2_to_mem_write_2 ? `MemoryStates__mem_write_2 : 3'b0 | input_mem_read_2_to_csr_read ? `MemoryStates__csr_read : 3'b0 | input_mem_read_2_to_csr_write ? `MemoryStates__csr_write : 3'b0 | `MemoryStates__mem_read_2;
	assign state_mem_write_1_selector = input_mem_write_1_to_mem_write_2 ? `MemoryStates__mem_write_2 : 3'b0 | `MemoryStates__mem_write_1;
	assign state_mem_write_2_selector = input_mem_write_2_to_idle ? `MemoryStates__idle : 3'b0 | input_mem_write_2_to_mem_read_1 ? `MemoryStates__mem_read_1 : 3'b0 | input_mem_write_2_to_mem_read_2 ? `MemoryStates__mem_read_2 : 3'b0 | input_mem_write_2_to_mem_write_1 ? `MemoryStates__mem_write_1 : 3'b0 | input_mem_write_2_to_mem_write_2 ? `MemoryStates__mem_write_2 : 3'b0 | input_mem_write_2_to_csr_read ? `MemoryStates__csr_read : 3'b0 | input_mem_write_2_to_csr_write ? `MemoryStates__csr_write : 3'b0 | `MemoryStates__mem_write_2;
	assign state_csr_read_selector = input_csr_read_to_idle ? `MemoryStates__idle : 3'b0 | input_csr_read_to_mem_read_1 ? `MemoryStates__mem_read_1 : 3'b0 | input_csr_read_to_mem_read_2 ? `MemoryStates__mem_read_2 : 3'b0 | input_csr_read_to_mem_write_1 ? `MemoryStates__mem_write_1 : 3'b0 | input_csr_read_to_mem_write_2 ? `MemoryStates__mem_write_2 : 3'b0 | input_csr_read_to_csr_read ? `MemoryStates__csr_read : 3'b0 | input_csr_read_to_csr_write ? `MemoryStates__csr_write : 3'b0 | `MemoryStates__csr_read;
	assign state_csr_write_selector = input_csr_write_to_idle ? `MemoryStates__idle : 3'b0 | input_csr_write_to_mem_read_1 ? `MemoryStates__mem_read_1 : 3'b0 | input_csr_write_to_mem_read_2 ? `MemoryStates__mem_read_2 : 3'b0 | input_csr_write_to_mem_write_1 ? `MemoryStates__mem_write_1 : 3'b0 | input_csr_write_to_mem_write_2 ? `MemoryStates__mem_write_2 : 3'b0 | input_csr_write_to_csr_read ? `MemoryStates__csr_read : 3'b0 | input_csr_write_to_csr_write ? `MemoryStates__csr_write : 3'b0 | `MemoryStates__csr_write;
	assign next_state = state == `MemoryStates__idle ? state_idle_selector : 3'b0 | state == `MemoryStates__mem_read_1 ? state_mem_read_1_selector : 3'b0 | state == `MemoryStates__mem_read_2 ? state_mem_read_2_selector : 3'b0 | state == `MemoryStates__mem_write_1 ? state_mem_write_1_selector : 3'b0 | state == `MemoryStates__mem_write_2 ? state_mem_write_2_selector : 3'b0 | state == `MemoryStates__csr_read ? state_csr_read_selector : 3'b0 | state == `MemoryStates__csr_write ? state_csr_write_selector : 3'b0 | default_state;

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
	input logic [1:0] state,
	output logic [1:0] next_state,
	input logic [1:0] default_state,
	input logic input_idle_to_request,
	input logic input_request_to_idle,
	input logic input_request_to_flush_start,
	input logic input_request_to_idle_1,
	input logic input_flush_start_to_idle
);

	logic [1:0] state_idle_selector;
	logic [1:0] state_request_selector;
	logic [1:0] state_flush_start_selector;

	assign state_idle_selector = input_idle_to_request ? `InstBufferStates__request : 2'b0 | `InstBufferStates__idle;
	assign state_request_selector = input_request_to_idle ? `InstBufferStates__idle : 2'b0 | input_request_to_idle_1 ? `InstBufferStates__idle : 2'b0 | input_request_to_flush_start ? `InstBufferStates__flush_start : 2'b0 | `InstBufferStates__request;
	assign state_flush_start_selector = input_flush_start_to_idle ? `InstBufferStates__idle : 2'b0 | `InstBufferStates__flush_start;
	assign next_state = state == `InstBufferStates__idle ? state_idle_selector : 2'b0 | state == `InstBufferStates__request ? state_request_selector : 2'b0 | state == `InstBufferStates__flush_start ? state_flush_start_selector : 2'b0 | default_state;

endmodule


////////////////////////////////////////////////////////////////////////////////
// FSMLogic
////////////////////////////////////////////////////////////////////////////////
module FSMLogic (
	input logic [2:0] state,
	output logic [2:0] next_state,
	input logic [2:0] default_state,
	input logic input_idle_to_external,
	input logic input_idle_to_non_dram_first,
	input logic input_idle_to_first,
	input logic input_external_to_idle,
	input logic input_first_to_precharge,
	input logic input_first_to_middle,
	input logic input_middle_to_precharge,
	input logic input_precharge_to_idle,
	input logic input_precharge_to_pre_external,
	input logic input_pre_external_to_external,
	input logic input_pre_external_to_idle,
	input logic input_non_dram_first_to_non_dram_wait,
	input logic input_non_dram_wait_to_non_dram_wait,
	input logic input_non_dram_wait_to_idle
);

	logic [2:0] state_idle_selector;
	logic [2:0] state_external_selector;
	logic [2:0] state_first_selector;
	logic [2:0] state_middle_selector;
	logic [2:0] state_precharge_selector;
	logic [2:0] state_pre_external_selector;
	logic [2:0] state_non_dram_first_selector;
	logic [2:0] state_non_dram_wait_selector;

	assign state_idle_selector = input_idle_to_external ? `BusIfStates__external : 3'b0 | input_idle_to_non_dram_first ? `BusIfStates__non_dram_first : 3'b0 | input_idle_to_first ? `BusIfStates__first : 3'b0 | `BusIfStates__idle;
	assign state_external_selector = input_external_to_idle ? `BusIfStates__idle : 3'b0 | `BusIfStates__external;
	assign state_first_selector = input_first_to_precharge ? `BusIfStates__precharge : 3'b0 | input_first_to_middle ? `BusIfStates__middle : 3'b0 | `BusIfStates__first;
	assign state_middle_selector = input_middle_to_precharge ? `BusIfStates__precharge : 3'b0 | `BusIfStates__middle;
	assign state_precharge_selector = input_precharge_to_idle ? `BusIfStates__idle : 3'b0 | input_precharge_to_pre_external ? `BusIfStates__pre_external : 3'b0 | `BusIfStates__precharge;
	assign state_pre_external_selector = input_precharge_to_pre_external ? `BusIfStates__external : 3'b0 | input_pre_external_to_idle ? `BusIfStates__idle : 3'b0 | `BusIfStates__pre_external;
	assign state_non_dram_first_selector = input_non_dram_first_to_non_dram_wait ? `BusIfStates__non_dram_wait : 3'b0 | `BusIfStates__non_dram_first;
	assign state_non_dram_wait_selector = input_non_dram_wait_to_non_dram_wait ? `BusIfStates__non_dram_wait : 3'b0 | input_non_dram_wait_to_idle ? `BusIfStates__idle : 3'b0 | `BusIfStates__non_dram_wait;
	assign next_state = state == `BusIfStates__idle ? state_idle_selector : 3'b0 | state == `BusIfStates__external ? state_external_selector : 3'b0 | state == `BusIfStates__first ? state_first_selector : 3'b0 | state == `BusIfStates__middle ? state_middle_selector : 3'b0 | state == `BusIfStates__precharge ? state_precharge_selector : 3'b0 | state == `BusIfStates__pre_external ? state_pre_external_selector : 3'b0 | state == `BusIfStates__non_dram_first ? state_non_dram_first_selector : 3'b0 | state == `BusIfStates__non_dram_wait ? state_non_dram_wait_selector : 3'b0 | default_state;

endmodule


