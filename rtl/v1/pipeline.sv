////////////////////////////////////////////////////////////////////////////////
// Type definitions
////////////////////////////////////////////////////////////////////////////////
`define alu_ops__a_plus_b 3'h0
`define alu_ops__a_minus_b 3'h1
`define alu_ops__a_and_b 3'h2
`define alu_ops__n_b_and_a 3'h3
`define alu_ops__a_or_b 3'h4
`define alu_ops__a_xor_b 3'h5
`define alu_ops__tpc 3'h6
`define alu_ops__pc_plus_b 3'h7


`define branch_ops__cb_eq 4'h1
`define branch_ops__cb_ne 4'h2
`define branch_ops__cb_lts 4'h3
`define branch_ops__cb_ges 4'h4
`define branch_ops__cb_lt 4'h5
`define branch_ops__cb_ge 4'h6
`define branch_ops__bb_one 4'h7
`define branch_ops__bb_zero 4'h8
`define branch_ops__swi 4'h9
`define branch_ops__stm 4'ha
`define branch_ops__pc_w 4'hb
`define branch_ops__tpc_w 4'hc


`define op_class__alu 3'h0
`define op_class__mult 3'h1
`define op_class__shift 3'h2
`define op_class__branch 3'h3
`define op_class__ld_st 3'h4


`define ldst_ops__store 1'h0
`define ldst_ops__load 1'h1


`define shifter_ops__shll 2'h0
`define shifter_ops__shlr 2'h1
`define shifter_ops__shar 2'h2


`define BusIfStates__idle 3'h0
`define BusIfStates__first 3'h1
`define BusIfStates__middle 3'h2
`define BusIfStates__external 3'h3
`define BusIfStates__precharge 3'h4
`define BusIfStates__pre_external 3'h5
`define BusIfStates__non_dram_first 3'h6
`define BusIfStates__non_dram_wait 3'h7


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
	output logic ext_grnt,
	input logic interrupt
);

	logic [30:0] spc;
	logic [30:0] tpc;
	logic task_mode;
	logic [11:0] ecause;
	logic csr_write_strobe;
	logic [31:0] u12_output_port;
	logic [31:0] csr_if_prdata;
	logic [21:0] mem_base;
	logic [21:0] mem_limit;
	logic csr_if_pready;
	logic mem_to_bus_ready;
	logic fetch_to_bus_ready;
	logic [15:0] bus_to_mem_data;
	logic bus_to_mem_valid;
	logic bus_to_fetch_valid;
	logic [15:0] bus_to_fetch_data;
	logic fetch_to_bus_valid;
	logic [1:0] fetch_to_bus_byte_en;
	logic [15:0] fetch_to_bus_data;
	logic fetch_to_bus_read_not_write;
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
	logic [2:0] decode_to_exec_exec_unit;
	logic [2:0] decode_to_exec_alu_op;
	logic [3:0] decode_to_exec_branch_op;
	logic decode_to_exec_ldst_op;
	logic [31:0] decode_to_exec_op_a;
	logic [31:0] decode_to_exec_op_b;
	logic [31:0] decode_to_exec_op_c;
	logic [1:0] decode_to_exec_inst_len;
	logic decode_to_exec_do_bse;
	logic decode_to_exec_do_wse;
	logic decode_to_exec_do_bze;
	logic decode_to_exec_do_wze;
	logic [3:0] decode_to_exec_result_reg_addr;
	logic decode_to_exec_result_reg_addr_valid;
	logic decode_to_exec_fetch_av;
	logic [1:0] decode_to_exec_shifter_op;
	logic decode_to_exec_valid;
	logic rf_rsp_ready;
	logic fetch_to_decode_ready;
	logic [3:0] rf_req_read1_addr;
	logic [3:0] rf_req_read2_addr;
	logic [3:0] rf_req_rsv_addr;
	logic [1:0] decode_to_exec_mem_access_len;
	logic decode_to_exec_ready;
	logic execute_stage_complete;
	logic exec_to_extend_valid;
	logic [15:0] exec_to_extend_data_l;
	logic [15:0] exec_to_extend_data_h;
	logic exec_to_extend_data_en;
	logic [3:0] exec_to_extend_addr;
	logic exec_to_extend_do_bse;
	logic exec_to_extend_do_wse;
	logic exec_to_extend_do_bze;
	logic exec_to_extend_do_wze;
	logic do_branch;
	logic [30:0] execute_stage_tpc_out;
	logic [30:0] execute_stage_spc_out;
	logic execute_stage_task_mode_out;
	logic [11:0] execute_stage_ecause_out;
	logic mem_to_bus_valid;
	logic mem_to_bus_read_not_write;
	logic [1:0] mem_to_bus_byte_en;
	logic [25:0] mem_to_bus_addr;
	logic mem_to_bus_dram_not_ext;
	logic [15:0] mem_to_bus_data;
	logic csr_if_penable;
	logic csr_if_psel;
	logic csr_if_pwrite;
	logic [9:0] csr_if_paddr;
	logic [31:0] csr_if_pwdata;
	logic [31:0] rf_write_data;
	logic [3:0] rf_write_addr;
	logic rf_write_data_en;
	logic rf_write_valid;
	logic rf_req_ready;
	logic rf_rsp_valid;
	logic [31:0] rf_rsp_read1_data;
	logic [31:0] rf_rsp_read2_data;
	logic [3:0] csr_addr;

	always_ff @(posedge clk) spc <= rst ? 31'h0 : execute_stage_spc_out;
	always_ff @(posedge clk) tpc <= rst ? 31'h0 : execute_stage_tpc_out;
	always_ff @(posedge clk) task_mode <= rst ? 1'h0 : execute_stage_task_mode_out;
	always_ff @(posedge clk) ecause <= rst ? 12'h0 : execute_stage_ecause_out;
	assign csr_addr = csr_if_paddr[3:0];
	always @(*) begin
		unique case (csr_addr)
			4'd0: u12_output_port = 1'h0;
			4'd1: u12_output_port = {mem_base, 10'h0};
			4'd2: u12_output_port = {mem_limit, 10'h0};
			4'd3: u12_output_port = ecause;
		endcase
	end
	always_ff @(posedge clk) csr_if_prdata <= rst ? 32'h0 : u12_output_port;
	assign csr_write_strobe = csr_if_psel & csr_if_pwrite & csr_if_penable;
	always_ff @(posedge clk) mem_base <= rst ? 22'h0 : csr_addr == 1'h1 & csr_write_strobe ? csr_if_pwdata[31:10] : mem_base;
	always_ff @(posedge clk) mem_limit <= rst ? 22'h0 : csr_addr == 2'h2 & csr_write_strobe ? csr_if_pwdata[31:10] : mem_limit;
	assign csr_if_pready = 1'h1;

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
		.fetch_response_valid(bus_to_fetch_valid),

		.mem_request_addr(mem_to_bus_addr),
		.mem_request_byte_en(mem_to_bus_byte_en),
		.mem_request_data(mem_to_bus_data),
		.mem_request_dram_not_ext(mem_to_bus_dram_not_ext),
		.mem_request_read_not_write(mem_to_bus_read_not_write),
		.mem_request_ready(mem_to_bus_ready),
		.mem_request_valid(mem_to_bus_valid),

		.mem_response_data(bus_to_mem_data),
		.mem_response_valid(bus_to_mem_valid),

		.dram_addr(dram_addr),
		.dram_data_in(dram_data_in),
		.dram_data_out(dram_data_out),
		.dram_nCAS_a(dram_nCAS_a),
		.dram_nCAS_b(dram_nCAS_b),
		.dram_nNREN(dram_nNREN),
		.dram_nRAS(dram_nRAS),
		.dram_nWAIT(dram_nWAIT),
		.dram_nWE(dram_nWE),

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

		.output_port_alu_op(decode_to_exec_alu_op),
		.output_port_branch_op(decode_to_exec_branch_op),
		.output_port_do_bse(decode_to_exec_do_bse),
		.output_port_do_bze(decode_to_exec_do_bze),
		.output_port_do_wse(decode_to_exec_do_wse),
		.output_port_do_wze(decode_to_exec_do_wze),
		.output_port_exec_unit(decode_to_exec_exec_unit),
		.output_port_fetch_av(decode_to_exec_fetch_av),
		.output_port_inst_len(decode_to_exec_inst_len),
		.output_port_ldst_op(decode_to_exec_ldst_op),
		.output_port_mem_access_len(decode_to_exec_mem_access_len),
		.output_port_op_a(decode_to_exec_op_a),
		.output_port_op_b(decode_to_exec_op_b),
		.output_port_op_c(decode_to_exec_op_c),
		.output_port_ready(decode_to_exec_ready),
		.output_port_result_reg_addr(decode_to_exec_result_reg_addr),
		.output_port_result_reg_addr_valid(decode_to_exec_result_reg_addr_valid),
		.output_port_shifter_op(decode_to_exec_shifter_op),
		.output_port_valid(decode_to_exec_valid),

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
		.input_port_alu_op(decode_to_exec_alu_op),
		.input_port_branch_op(decode_to_exec_branch_op),
		.input_port_do_bse(decode_to_exec_do_bse),
		.input_port_do_bze(decode_to_exec_do_bze),
		.input_port_do_wse(decode_to_exec_do_wse),
		.input_port_do_wze(decode_to_exec_do_wze),
		.input_port_exec_unit(decode_to_exec_exec_unit),
		.input_port_fetch_av(decode_to_exec_fetch_av),
		.input_port_inst_len(decode_to_exec_inst_len),
		.input_port_ldst_op(decode_to_exec_ldst_op),
		.input_port_mem_access_len(decode_to_exec_mem_access_len),
		.input_port_op_a(decode_to_exec_op_a),
		.input_port_op_b(decode_to_exec_op_b),
		.input_port_op_c(decode_to_exec_op_c),
		.input_port_ready(decode_to_exec_ready),
		.input_port_result_reg_addr(decode_to_exec_result_reg_addr),
		.input_port_result_reg_addr_valid(decode_to_exec_result_reg_addr_valid),
		.input_port_shifter_op(decode_to_exec_shifter_op),
		.input_port_valid(decode_to_exec_valid),

		.output_port_addr(exec_to_extend_addr),
		.output_port_data_en(exec_to_extend_data_en),
		.output_port_data_h(exec_to_extend_data_h),
		.output_port_data_l(exec_to_extend_data_l),
		.output_port_do_bse(exec_to_extend_do_bse),
		.output_port_do_bze(exec_to_extend_do_bze),
		.output_port_do_wse(exec_to_extend_do_wse),
		.output_port_do_wze(exec_to_extend_do_wze),
		.output_port_valid(exec_to_extend_valid),

		.bus_req_if_addr(mem_to_bus_addr),
		.bus_req_if_byte_en(mem_to_bus_byte_en),
		.bus_req_if_data(mem_to_bus_data),
		.bus_req_if_dram_not_ext(mem_to_bus_dram_not_ext),
		.bus_req_if_read_not_write(mem_to_bus_read_not_write),
		.bus_req_if_ready(mem_to_bus_ready),
		.bus_req_if_valid(mem_to_bus_valid),

		.bus_rsp_if_data(bus_to_mem_data),
		.bus_rsp_if_valid(bus_to_mem_valid),

		.csr_if_paddr(csr_if_paddr),
		.csr_if_penable(csr_if_penable),
		.csr_if_prdata(csr_if_prdata),
		.csr_if_pready(csr_if_pready),
		.csr_if_psel(csr_if_psel),
		.csr_if_pwdata(csr_if_pwdata),
		.csr_if_pwrite(csr_if_pwrite),

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
		.do_branch(do_branch),
		.interrupt(interrupt),
		.complete(execute_stage_complete)
	);

	ResultExtendStage result_extend_stage (
		.input_port_addr(exec_to_extend_addr),
		.input_port_data_en(exec_to_extend_data_en),
		.input_port_data_h(exec_to_extend_data_h),
		.input_port_data_l(exec_to_extend_data_l),
		.input_port_do_bse(exec_to_extend_do_bse),
		.input_port_do_bze(exec_to_extend_do_bze),
		.input_port_do_wse(exec_to_extend_do_wse),
		.input_port_do_wze(exec_to_extend_do_wze),
		.input_port_valid(exec_to_extend_valid),

		.output_port_addr(rf_write_addr),
		.output_port_data(rf_write_data),
		.output_port_data_en(rf_write_data_en),
		.output_port_valid(rf_write_valid)
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
		.write_valid(rf_write_valid)
	);

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
	input logic write_valid
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
	logic u17_output_port;
	logic wait_for_read1;
	logic u39_output_port;
	logic wait_for_read2;
	logic u61_output_port;
	logic wait_for_rsv;
	logic wait_for_some;
	logic u87_output_port;
	logic wait_for_write;
	logic rsv_set_valid;
	logic rsv_clr_valid;
	logic u104_output_port;
	logic u116_output_port;
	logic u128_output_port;
	logic u140_output_port;
	logic u152_output_port;
	logic u164_output_port;
	logic u176_output_port;
	logic u188_output_port;
	logic u200_output_port;
	logic u212_output_port;
	logic u224_output_port;
	logic u236_output_port;
	logic u248_output_port;
	logic u260_output_port;
	logic u272_output_port;
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
	assign read1_valid = req_advance ? read_req_read1_valid : u2_output_port;
	always @(*) begin
		unique case (read1_addr)
			4'd0: u17_output_port = rsv_board[0];
			4'd1: u17_output_port = rsv_board[1];
			4'd2: u17_output_port = rsv_board[2];
			4'd3: u17_output_port = rsv_board[3];
			4'd4: u17_output_port = rsv_board[4];
			4'd5: u17_output_port = rsv_board[5];
			4'd6: u17_output_port = rsv_board[6];
			4'd7: u17_output_port = rsv_board[7];
			4'd8: u17_output_port = rsv_board[8];
			4'd9: u17_output_port = rsv_board[9];
			4'd10: u17_output_port = rsv_board[10];
			4'd11: u17_output_port = rsv_board[11];
			4'd12: u17_output_port = rsv_board[12];
			4'd13: u17_output_port = rsv_board[13];
			4'd14: u17_output_port = rsv_board[14];
		endcase
	end
	assign read2_valid = req_advance ? read_req_read2_valid : u6_output_port;
	always @(*) begin
		unique case (read2_addr)
			4'd0: u39_output_port = rsv_board[0];
			4'd1: u39_output_port = rsv_board[1];
			4'd2: u39_output_port = rsv_board[2];
			4'd3: u39_output_port = rsv_board[3];
			4'd4: u39_output_port = rsv_board[4];
			4'd5: u39_output_port = rsv_board[5];
			4'd6: u39_output_port = rsv_board[6];
			4'd7: u39_output_port = rsv_board[7];
			4'd8: u39_output_port = rsv_board[8];
			4'd9: u39_output_port = rsv_board[9];
			4'd10: u39_output_port = rsv_board[10];
			4'd11: u39_output_port = rsv_board[11];
			4'd12: u39_output_port = rsv_board[12];
			4'd13: u39_output_port = rsv_board[13];
			4'd14: u39_output_port = rsv_board[14];
		endcase
	end
	assign rsv_addr = req_advance ? read_req_rsv_addr : u12_output_port;
	assign rsv_valid = req_advance ? read_req_rsv_valid : u10_output_port;
	always @(*) begin
		unique case (rsv_addr)
			4'd0: u61_output_port = rsv_board[0];
			4'd1: u61_output_port = rsv_board[1];
			4'd2: u61_output_port = rsv_board[2];
			4'd3: u61_output_port = rsv_board[3];
			4'd4: u61_output_port = rsv_board[4];
			4'd5: u61_output_port = rsv_board[5];
			4'd6: u61_output_port = rsv_board[6];
			4'd7: u61_output_port = rsv_board[7];
			4'd8: u61_output_port = rsv_board[8];
			4'd9: u61_output_port = rsv_board[9];
			4'd10: u61_output_port = rsv_board[10];
			4'd11: u61_output_port = rsv_board[11];
			4'd12: u61_output_port = rsv_board[12];
			4'd13: u61_output_port = rsv_board[13];
			4'd14: u61_output_port = rsv_board[14];
		endcase
	end
	always_ff @(posedge clk) wait_for_read1 <= rst ? 1'h0 : read1_valid & u17_output_port &  ~ (write_addr == read1_addr & write_valid);
	always_ff @(posedge clk) wait_for_read2 <= rst ? 1'h0 : read2_valid & u39_output_port &  ~ (write_addr == read2_addr & write_valid);
	always_ff @(posedge clk) wait_for_rsv <= rst ? 1'h0 : rsv_valid & u61_output_port &  ~ (write_addr == rsv_addr & write_valid);
	assign wait_for_some = wait_for_read1 | wait_for_read2 | wait_for_rsv;
	always_ff @(posedge clk) u87_output_port <= rst ? 1'h0 : req_advance | write_valid ? wait_for_some : u87_output_port;
	assign wait_for_write = (req_advance | write_valid) ? wait_for_some : u87_output_port;
	assign rsv_set_valid = rsv_valid &  ~ wait_for_write;
	assign rsv_clr_valid = write_valid &  ~ wait_for_rsv;
	always_ff @(posedge clk) u104_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 1'h0) ? 1'h1 : (rsv_clr_valid & write_addr == 1'h0) ? 1'h0 : rsv_board[0];
	always_ff @(posedge clk) u116_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 1'h1) ? 1'h1 : (rsv_clr_valid & write_addr == 1'h1) ? 1'h0 : rsv_board[1];
	always_ff @(posedge clk) u128_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 2'h2) ? 1'h1 : (rsv_clr_valid & write_addr == 2'h2) ? 1'h0 : rsv_board[2];
	always_ff @(posedge clk) u140_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 2'h3) ? 1'h1 : (rsv_clr_valid & write_addr == 2'h3) ? 1'h0 : rsv_board[3];
	always_ff @(posedge clk) u152_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 3'h4) ? 1'h1 : (rsv_clr_valid & write_addr == 3'h4) ? 1'h0 : rsv_board[4];
	always_ff @(posedge clk) u164_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 3'h5) ? 1'h1 : (rsv_clr_valid & write_addr == 3'h5) ? 1'h0 : rsv_board[5];
	always_ff @(posedge clk) u176_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 3'h6) ? 1'h1 : (rsv_clr_valid & write_addr == 3'h6) ? 1'h0 : rsv_board[6];
	always_ff @(posedge clk) u188_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 3'h7) ? 1'h1 : (rsv_clr_valid & write_addr == 3'h7) ? 1'h0 : rsv_board[7];
	always_ff @(posedge clk) u200_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 4'h8) ? 1'h1 : (rsv_clr_valid & write_addr == 4'h8) ? 1'h0 : rsv_board[8];
	always_ff @(posedge clk) u212_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 4'h9) ? 1'h1 : (rsv_clr_valid & write_addr == 4'h9) ? 1'h0 : rsv_board[9];
	always_ff @(posedge clk) u224_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 4'ha) ? 1'h1 : (rsv_clr_valid & write_addr == 4'ha) ? 1'h0 : rsv_board[10];
	always_ff @(posedge clk) u236_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 4'hb) ? 1'h1 : (rsv_clr_valid & write_addr == 4'hb) ? 1'h0 : rsv_board[11];
	always_ff @(posedge clk) u248_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 4'hc) ? 1'h1 : (rsv_clr_valid & write_addr == 4'hc) ? 1'h0 : rsv_board[12];
	always_ff @(posedge clk) u260_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 4'hd) ? 1'h1 : (rsv_clr_valid & write_addr == 4'hd) ? 1'h0 : rsv_board[13];
	always_ff @(posedge clk) u272_output_port <= rst ? 1'h0 : (rsv_set_valid & rsv_addr == 4'he) ? 1'h1 : (rsv_clr_valid & write_addr == 4'he) ? 1'h0 : rsv_board[14];
	assign rsp_advance = read_rsp_ready & read_rsp_valid;
	always_ff @(posedge clk) outstanding_req <= rst ? 1'h0 : req_advance ? 1'h1 : rsp_advance ? 1'h0 : outstanding_req;
	always_ff @(posedge clk) out_buf_full <= rst ? 1'h0 : req_advance ? 1'h1 : rsp_advance ? 1'h0 : out_buf_full;
	always_ff @(posedge clk) wait_for_write_d <= rst ? 1'h0 : wait_for_write;
	assign read_req_ready =  ~ wait_for_write_d & (read_rsp_ready |  ~ out_buf_full);
	assign read_rsp_valid =  ~ wait_for_write_d & out_buf_full;
	assign rsv_board = {u272_output_port, u260_output_port, u248_output_port, u236_output_port, u224_output_port, u212_output_port, u200_output_port, u188_output_port, u176_output_port, u164_output_port, u152_output_port, u140_output_port, u128_output_port, u116_output_port, u104_output_port};

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
// ResultExtendStage
////////////////////////////////////////////////////////////////////////////////
module ResultExtendStage (
	input logic [3:0] input_port_addr,
	input logic input_port_data_en,
	input logic [15:0] input_port_data_h,
	input logic [15:0] input_port_data_l,
	input logic input_port_do_bse,
	input logic input_port_do_bze,
	input logic input_port_do_wse,
	input logic input_port_do_wze,
	input logic input_port_valid,

	output logic [3:0] output_port_addr,
	output logic [31:0] output_port_data,
	output logic output_port_data_en,
	output logic output_port_valid
);

	assign output_port_data = 
		(input_port_do_bse ? ({input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7], input_port_data_l[7:0]}) : 32'b0) | 
		(input_port_do_wse ? ({input_port_data_l[15], input_port_data_l[15], input_port_data_l[15], input_port_data_l[15], input_port_data_l[15], input_port_data_l[15], input_port_data_l[15], input_port_data_l[15], input_port_data_l[15], input_port_data_l[15], input_port_data_l[15], input_port_data_l[15], input_port_data_l[15], input_port_data_l[15], input_port_data_l[15], input_port_data_l[15], input_port_data_l[15:0]}) : 32'b0) | 
		(input_port_do_bze ? input_port_data_l[7:0] : 32'b0) | 
		(input_port_do_wze ? input_port_data_l[15:0] : 32'b0) | 
		({input_port_data_h, input_port_data_l});
	assign output_port_addr = input_port_addr;
	assign output_port_data_en = input_port_data_en;
	assign output_port_valid = input_port_valid;

endmodule


////////////////////////////////////////////////////////////////////////////////
// ExecuteStage
////////////////////////////////////////////////////////////////////////////////
module ExecuteStage (
	input logic clk,
	input logic rst,
	input logic [2:0] input_port_alu_op,
	input logic [3:0] input_port_branch_op,
	input logic input_port_do_bse,
	input logic input_port_do_bze,
	input logic input_port_do_wse,
	input logic input_port_do_wze,
	input logic [2:0] input_port_exec_unit,
	input logic input_port_fetch_av,
	input logic [1:0] input_port_inst_len,
	input logic input_port_ldst_op,
	input logic [1:0] input_port_mem_access_len,
	input logic [31:0] input_port_op_a,
	input logic [31:0] input_port_op_b,
	input logic [31:0] input_port_op_c,
	output logic input_port_ready,
	input logic [3:0] input_port_result_reg_addr,
	input logic input_port_result_reg_addr_valid,
	input logic [1:0] input_port_shifter_op,
	input logic input_port_valid,

	output logic [3:0] output_port_addr,
	output logic output_port_data_en,
	output logic [15:0] output_port_data_h,
	output logic [15:0] output_port_data_l,
	output logic output_port_do_bse,
	output logic output_port_do_bze,
	output logic output_port_do_wse,
	output logic output_port_do_wze,
	output logic output_port_valid,

	output logic [25:0] bus_req_if_addr,
	output logic [1:0] bus_req_if_byte_en,
	output logic [15:0] bus_req_if_data,
	output logic bus_req_if_dram_not_ext,
	output logic bus_req_if_read_not_write,
	input logic bus_req_if_ready,
	output logic bus_req_if_valid,

	input logic [15:0] bus_rsp_if_data,
	input logic bus_rsp_if_valid,

	output logic [9:0] csr_if_paddr,
	output logic csr_if_penable,
	input logic [31:0] csr_if_prdata,
	input logic csr_if_pready,
	output logic csr_if_psel,
	output logic [31:0] csr_if_pwdata,
	output logic csr_if_pwrite,

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
	output logic do_branch,
	input logic interrupt,
	output logic complete
);

	logic branch_input_interrupt;
	logic [30:0] pc;
	logic multi_cycle_exec_lockout;
	logic stage_1_reg_en;
	logic s1_alu_output_f_carry;
	logic branch_input_f_carry;
	logic s1_alu_output_f_overflow;
	logic branch_input_f_overflow;
	logic s1_alu_output_f_sign;
	logic branch_input_f_sign;
	logic s1_alu_output_f_zero;
	logic branch_input_f_zero;
	logic [31:0] result;
	logic [31:0] s1_alu_output_result;
	logic s1_ldst_output_mem_av;
	logic branch_input_mem_av;
	logic s1_ldst_output_mem_unaligned;
	logic branch_input_mem_unaligned;
	logic [31:0] s1_ldst_output_phy_addr;
	logic [31:0] mem_input_addr;
	logic [30:0] s1_branch_target_output_branch_addr;
	logic [30:0] branch_input_branch_addr;
	logic [30:0] s1_branch_target_output_straight_addr;
	logic [2:0] s1_exec_unit;
	logic [3:0] branch_input_opcode;
	logic [3:0] s1_branch_op;
	logic s1_ldst_op;
	logic [31:0] mem_input_data;
	logic [31:0] branch_input_op_a;
	logic [31:0] s1_op_a;
	logic [31:0] branch_input_op_b;
	logic [31:0] s1_op_b;
	logic [31:0] branch_input_op_c;
	logic [31:0] s1_op_c;
	logic s1_do_bse;
	logic s1_do_wse;
	logic s1_do_bze;
	logic s1_do_wze;
	logic [3:0] s1_result_reg_addr;
	logic s1_result_reg_addr_valid;
	logic branch_input_fetch_av;
	logic s1_fetch_av;
	logic [30:0] branch_input_tpc;
	logic [30:0] s1_tpc;
	logic [30:0] branch_input_spc;
	logic [30:0] s1_spc;
	logic branch_input_task_mode;
	logic s1_task_mode;
	logic [1:0] mem_input_access_len;
	logic [1:0] s1_mem_access_len;
	logic [30:0] branch_input_pc;
	logic [30:0] s2_pc;
	logic block_mem;
	logic mem_input_valid;
	logic stage_2_ready;
	logic stage_2_valid;
	logic branch_input_is_branch_insn;
	logic mem_input_read_not_write;
	logic s2_result_reg_addr_valid;
	logic [15:0] u61_output_port;
	logic [15:0] u66_output_port;
	logic [30:0] s1_tpc_out;
	logic [30:0] s1_spc_out;
	logic u93_output_port;
	logic stage_1_fsm_out_reg_en;
	logic stage_1_valid;
	logic stage_1_fsm_input_ready;
	logic alu_output_f_zero;
	logic alu_output_f_carry;
	logic alu_output_f_overflow;
	logic [31:0] alu_output_result;
	logic alu_output_f_sign;
	logic ldst_output_mem_av;
	logic ldst_output_mem_unaligned;
	logic [31:0] ldst_output_phy_addr;
	logic [30:0] branch_target_output_straight_addr;
	logic [30:0] branch_target_output_branch_addr;
	logic stage_2_reg_en;
	logic stage_2_fsm_output_valid;
	logic stage_2_fsm_input_ready;
	logic [30:0] branch_output_tpc;
	logic [30:0] branch_output_spc;
	logic branch_output_spc_changed;
	logic branch_output_tpc_changed;
	logic branch_output_task_mode_changed;
	logic branch_output_task_mode;
	logic branch_output_do_branch;
	logic [11:0] branch_output_ecause;
	logic mem_input_ready;
	logic s2_mem_output_valid;
	logic [15:0] s2_mem_output_data_l;
	logic [15:0] s2_mem_output_data_h;

	always_ff @(posedge clk) multi_cycle_exec_lockout <= rst ? 1'h0 : input_port_ready & input_port_valid & input_port_exec_unit == `op_class__mult;
	assign input_port_ready =  ~ multi_cycle_exec_lockout & stage_1_fsm_input_ready;
	assign stage_1_reg_en =  ~ multi_cycle_exec_lockout & stage_1_fsm_out_reg_en;
	assign pc = task_mode_in ? tpc_in : spc_in;
	always_ff @(posedge clk) branch_input_f_carry <= rst ? 1'h0 : stage_1_reg_en ? alu_output_f_carry : branch_input_f_carry;
	always_ff @(posedge clk) branch_input_f_overflow <= rst ? 1'h0 : stage_1_reg_en ? alu_output_f_overflow : branch_input_f_overflow;
	always_ff @(posedge clk) branch_input_f_sign <= rst ? 1'h0 : stage_1_reg_en ? alu_output_f_sign : branch_input_f_sign;
	always_ff @(posedge clk) branch_input_f_zero <= rst ? 1'h0 : stage_1_reg_en ? alu_output_f_zero : branch_input_f_zero;
	always_ff @(posedge clk) s1_alu_output_result <= rst ? 32'h0 : stage_1_reg_en ? alu_output_result : s1_alu_output_result;
	always_ff @(posedge clk) branch_input_mem_av <= rst ? 1'h0 : stage_1_reg_en ? ldst_output_mem_av : branch_input_mem_av;
	always_ff @(posedge clk) branch_input_mem_unaligned <= rst ? 1'h0 : stage_1_reg_en ? ldst_output_mem_unaligned : branch_input_mem_unaligned;
	always_ff @(posedge clk) mem_input_addr <= rst ? 32'h0 : stage_1_reg_en ? ldst_output_phy_addr : mem_input_addr;
	always_ff @(posedge clk) branch_input_branch_addr <= rst ? 31'h0 : stage_1_reg_en ? branch_target_output_branch_addr : branch_input_branch_addr;
	always_ff @(posedge clk) s1_branch_target_output_straight_addr <= rst ? 31'h0 : stage_1_reg_en ? branch_target_output_straight_addr : s1_branch_target_output_straight_addr;
	always_ff @(posedge clk) s1_task_mode <= rst ? 1'h0 : stage_1_reg_en ? task_mode_in : s1_task_mode;
	always_ff @(posedge clk) s1_spc <= rst ? 31'h0 : stage_1_reg_en ? spc_in : s1_spc;
	always_ff @(posedge clk) s1_tpc <= rst ? 31'h0 : stage_1_reg_en ? tpc_in : s1_tpc;
	always_ff @(posedge clk) s1_fetch_av <= rst ? 1'h0 : stage_1_reg_en ? input_port_fetch_av : s1_fetch_av;
	assign block_mem = branch_input_mem_av | branch_input_mem_unaligned | s1_fetch_av;
	assign mem_input_valid = stage_1_valid &  ~ block_mem;
	always_ff @(posedge clk) s1_exec_unit <= rst ? 3'h0 : stage_1_reg_en ? input_port_exec_unit : s1_exec_unit;
	assign stage_2_ready = (s1_exec_unit == `op_class__ld_st &  ~ block_mem) ? mem_input_ready : stage_2_fsm_input_ready;
	assign stage_2_valid = (s1_exec_unit == `op_class__ld_st &  ~ block_mem) ? s2_mem_output_valid : stage_2_fsm_output_valid;
	always_ff @(posedge clk) s1_branch_op <= rst ? 4'h0 : stage_1_reg_en ? input_port_branch_op : s1_branch_op;
	assign s2_pc = s1_task_mode ? s1_tpc : s1_spc;
	always_ff @(posedge clk) s1_op_a <= rst ? 32'h0 : stage_1_reg_en ? input_port_op_a : s1_op_a;
	always_ff @(posedge clk) s1_op_b <= rst ? 32'h0 : stage_1_reg_en ? input_port_op_b : s1_op_b;
	always_ff @(posedge clk) s1_op_c <= rst ? 32'h0 : stage_1_reg_en ? input_port_op_c : s1_op_c;
	assign branch_input_is_branch_insn = s1_exec_unit == `op_class__branch;
	always_ff @(posedge clk) s1_ldst_op <= rst ? 1'h0 : stage_1_reg_en ? input_port_ldst_op : s1_ldst_op;
	assign mem_input_read_not_write = s1_exec_unit == `op_class__ld_st & s1_ldst_op == `ldst_ops__load;
	always_ff @(posedge clk) s1_mem_access_len <= rst ? 2'h0 : stage_1_reg_en ? input_port_mem_access_len : s1_mem_access_len;
	always_ff @(posedge clk) s1_result_reg_addr_valid <= rst ? 1'h0 : stage_1_reg_en ? input_port_result_reg_addr_valid : s1_result_reg_addr_valid;
	always_ff @(posedge clk) s2_result_reg_addr_valid <= rst ? 1'h0 : stage_2_reg_en ? s1_result_reg_addr_valid : s2_result_reg_addr_valid;
	assign output_port_valid = stage_2_valid & s2_result_reg_addr_valid;
	always_ff @(posedge clk) u61_output_port <= rst ? 16'h0 : stage_2_reg_en ? s1_alu_output_result[15:0] : u61_output_port;
	assign output_port_data_l = s1_exec_unit == `op_class__ld_st ? s2_mem_output_data_l : u61_output_port;
	always_ff @(posedge clk) u66_output_port <= rst ? 16'h0 : stage_2_reg_en ? s1_alu_output_result[31:16] : u66_output_port;
	assign output_port_data_h = s1_exec_unit == `op_class__ld_st ? s2_mem_output_data_h : u66_output_port;
	always_ff @(posedge clk) output_port_data_en <= rst ? 1'h0 : stage_2_reg_en ?  ~ branch_output_do_branch : output_port_data_en;
	always_ff @(posedge clk) s1_result_reg_addr <= rst ? 4'h0 : stage_1_reg_en ? input_port_result_reg_addr : s1_result_reg_addr;
	always_ff @(posedge clk) output_port_addr <= rst ? 4'h0 : stage_2_reg_en ? s1_result_reg_addr : output_port_addr;
	always_ff @(posedge clk) s1_do_bse <= rst ? 1'h0 : stage_1_reg_en ? input_port_do_bse : s1_do_bse;
	always_ff @(posedge clk) output_port_do_bse <= rst ? 1'h0 : stage_2_reg_en ? s1_do_bse : output_port_do_bse;
	always_ff @(posedge clk) s1_do_wse <= rst ? 1'h0 : stage_1_reg_en ? input_port_do_wse : s1_do_wse;
	always_ff @(posedge clk) output_port_do_wse <= rst ? 1'h0 : stage_2_reg_en ? s1_do_wse : output_port_do_wse;
	always_ff @(posedge clk) s1_do_bze <= rst ? 1'h0 : stage_1_reg_en ? input_port_do_bze : s1_do_bze;
	always_ff @(posedge clk) output_port_do_bze <= rst ? 1'h0 : stage_2_reg_en ? s1_do_bze : output_port_do_bze;
	always_ff @(posedge clk) s1_do_wze <= rst ? 1'h0 : stage_1_reg_en ? input_port_do_wze : s1_do_wze;
	always_ff @(posedge clk) output_port_do_wze <= rst ? 1'h0 : stage_2_reg_en ? s1_do_wze : output_port_do_wze;
	always_ff @(posedge clk) do_branch <= rst ? 1'h0 : branch_output_do_branch & stage_2_reg_en;
	always_ff @(posedge clk) s1_tpc_out <= rst ? 31'h0 : stage_1_reg_en ? task_mode_in ? branch_target_output_straight_addr : tpc_in : tpc_in;
	assign tpc_out = stage_2_reg_en ? branch_output_tpc_changed ? branch_output_tpc : s1_tpc_out : s1_tpc_out;
	always_ff @(posedge clk) s1_spc_out <= rst ? 31'h0 : stage_1_reg_en ?  ~ task_mode_in ? branch_target_output_straight_addr : spc_in : spc_in;
	assign spc_out = stage_2_reg_en ? branch_output_spc_changed ? branch_output_spc : s1_spc_out : s1_spc_out;
	assign task_mode_out = stage_2_reg_en ? branch_output_task_mode_changed ? branch_output_task_mode : s1_task_mode : s1_task_mode;
	assign ecause_out = stage_2_reg_en ? ecause_in | branch_output_ecause : ecause_in;

	ForwardBufLogic stage_1_fsm (
		.clock_port(clk),
		.reset_port(rst),
		.input_valid( ~ multi_cycle_exec_lockout & input_port_valid),
		.input_ready(stage_1_fsm_input_ready),
		.output_valid(stage_1_valid),
		.output_ready(stage_2_ready),
		.out_reg_en(stage_1_fsm_out_reg_en)
	);

	AluUnit alu_unit (
		.clk(clk),
		.rst(rst),
		.input_port_op_a(input_port_op_a),
		.input_port_op_b(input_port_op_b),
		.input_port_opcode(input_port_alu_op),
		.input_port_pc(pc),
		.input_port_tpc(tpc_in),

		.output_port_f_carry(alu_output_f_carry),
		.output_port_f_overflow(alu_output_f_overflow),
		.output_port_f_sign(alu_output_f_sign),
		.output_port_f_zero(alu_output_f_zero),
		.output_port_result(alu_output_result)
	);

	LoadStoreUnit ldst_unit (
		.clk(clk),
		.rst(rst),
		.input_port_is_ldst(input_port_exec_unit == `op_class__ld_st),
		.input_port_mem_access_len(input_port_mem_access_len),
		.input_port_mem_base(mem_base),
		.input_port_mem_limit(mem_limit),
		.input_port_op_b(input_port_op_b),
		.input_port_op_c(input_port_op_c),
		.input_port_task_mode(task_mode_in),

		.output_port_mem_av(ldst_output_mem_av),
		.output_port_mem_unaligned(ldst_output_mem_unaligned),
		.output_port_phy_addr(ldst_output_phy_addr)
	);

	BranchTargetUnit branch_target_unit (
		.input_port_inst_len(input_port_inst_len),
		.input_port_op_c(input_port_op_c),
		.input_port_pc(pc),

		.output_port_branch_addr(branch_target_output_branch_addr),
		.output_port_straight_addr(branch_target_output_straight_addr)
	);

	ForwardBufLogic_2 stage_2_fsm (
		.clock_port(clk),
		.reset_port(rst),
		.input_valid(stage_1_valid),
		.input_ready(stage_2_fsm_input_ready),
		.output_valid(stage_2_fsm_output_valid),
		.output_ready(u93_output_port),
		.out_reg_en(stage_2_reg_en)
	);

	BranchUnit branch_unit (
		.input_port_branch_addr(branch_input_branch_addr),
		.input_port_f_carry(branch_input_f_carry),
		.input_port_f_overflow(branch_input_f_overflow),
		.input_port_f_sign(branch_input_f_sign),
		.input_port_f_zero(branch_input_f_zero),
		.input_port_fetch_av(s1_fetch_av),
		.input_port_interrupt(interrupt),
		.input_port_is_branch_insn(branch_input_is_branch_insn),
		.input_port_mem_av(branch_input_mem_av),
		.input_port_mem_unaligned(branch_input_mem_unaligned),
		.input_port_op_a(s1_op_a),
		.input_port_op_b(s1_op_b),
		.input_port_op_c(s1_op_c),
		.input_port_opcode(s1_branch_op),
		.input_port_pc(s2_pc),
		.input_port_spc(s1_spc),
		.input_port_task_mode(s1_task_mode),
		.input_port_tpc(s1_tpc),

		.output_port_do_branch(branch_output_do_branch),
		.output_port_ecause(branch_output_ecause),
		.output_port_spc(branch_output_spc),
		.output_port_spc_changed(branch_output_spc_changed),
		.output_port_task_mode(branch_output_task_mode),
		.output_port_task_mode_changed(branch_output_task_mode_changed),
		.output_port_tpc(branch_output_tpc),
		.output_port_tpc_changed(branch_output_tpc_changed)
	);

	MemoryStage memory_unit (
		.clk(clk),
		.rst(rst),
		.input_port_access_len(s1_mem_access_len),
		.input_port_addr(mem_input_addr),
		.input_port_data(s1_op_a),
		.input_port_read_not_write(mem_input_read_not_write),
		.input_port_ready(mem_input_ready),
		.input_port_valid(mem_input_valid),

		.output_port_data_h(s2_mem_output_data_h),
		.output_port_data_l(s2_mem_output_data_l),
		.output_port_valid(s2_mem_output_valid),

		.bus_req_if_addr(bus_req_if_addr),
		.bus_req_if_byte_en(bus_req_if_byte_en),
		.bus_req_if_data(bus_req_if_data),
		.bus_req_if_dram_not_ext(bus_req_if_dram_not_ext),
		.bus_req_if_read_not_write(bus_req_if_read_not_write),
		.bus_req_if_ready(bus_req_if_ready),
		.bus_req_if_valid(bus_req_if_valid),

		.bus_rsp_if_data(bus_rsp_if_data),
		.bus_rsp_if_valid(bus_rsp_if_valid),

		.csr_if_paddr(csr_if_paddr),
		.csr_if_penable(csr_if_penable),
		.csr_if_prdata(csr_if_prdata),
		.csr_if_pready(csr_if_pready),
		.csr_if_psel(csr_if_psel),
		.csr_if_pwdata(csr_if_pwdata),
		.csr_if_pwrite(csr_if_pwrite)
	);

	assign branch_input_interrupt = interrupt;
	assign s1_alu_output_f_carry = branch_input_f_carry;
	assign s1_alu_output_f_overflow = branch_input_f_overflow;
	assign s1_alu_output_f_sign = branch_input_f_sign;
	assign s1_alu_output_f_zero = branch_input_f_zero;
	assign result = s1_alu_output_result;
	assign s1_ldst_output_mem_av = branch_input_mem_av;
	assign s1_ldst_output_mem_unaligned = branch_input_mem_unaligned;
	assign s1_ldst_output_phy_addr = mem_input_addr;
	assign s1_branch_target_output_branch_addr = branch_input_branch_addr;
	assign branch_input_opcode = s1_branch_op;
	assign mem_input_data = s1_op_a;
	assign branch_input_op_a = s1_op_a;
	assign branch_input_op_b = s1_op_b;
	assign branch_input_op_c = s1_op_c;
	assign branch_input_fetch_av = s1_fetch_av;
	assign branch_input_tpc = s1_tpc;
	assign branch_input_spc = s1_spc;
	assign branch_input_task_mode = s1_task_mode;
	assign mem_input_access_len = s1_mem_access_len;
	assign branch_input_pc = s2_pc;
	assign complete = stage_2_valid;
	assign u93_output_port = 1'h1;
endmodule


////////////////////////////////////////////////////////////////////////////////
// MemoryStage
////////////////////////////////////////////////////////////////////////////////
module MemoryStage (
	input logic clk,
	input logic rst,
	input logic [1:0] input_port_access_len,
	input logic [31:0] input_port_addr,
	input logic [31:0] input_port_data,
	input logic input_port_read_not_write,
	output logic input_port_ready,
	input logic input_port_valid,

	output logic [15:0] output_port_data_h,
	output logic [15:0] output_port_data_l,
	output logic output_port_valid,

	output logic [25:0] bus_req_if_addr,
	output logic [1:0] bus_req_if_byte_en,
	output logic [15:0] bus_req_if_data,
	output logic bus_req_if_dram_not_ext,
	output logic bus_req_if_read_not_write,
	input logic bus_req_if_ready,
	output logic bus_req_if_valid,

	input logic [15:0] bus_rsp_if_data,
	input logic bus_rsp_if_valid,

	output logic [9:0] csr_if_paddr,
	output logic csr_if_penable,
	input logic [31:0] csr_if_prdata,
	input logic csr_if_pready,
	output logic csr_if_psel,
	output logic [31:0] csr_if_pwdata,
	output logic csr_if_pwrite
);

	logic is_nram;
	logic input_advance;
	logic bus_request_advance;
	logic multi_cycle;
	logic active;
	logic pending;
	logic gap;
	logic csr_select;
	logic u47_output_port;
	logic is_csr;
	logic [25:0] u62_output_port;
	logic [25:0] next_addr;
	logic [15:0] data_store;
	logic u83_output_port;
	logic [1:0] u87_output_port;
	logic u93_output_port;
	logic csr_pen;
	logic u112_output_port;
	logic [9:0] u116_output_port;
	logic [31:0] u121_output_port;
	logic [1:0] byte_en;

	assign input_advance = input_port_ready & input_port_valid;
	always_ff @(posedge clk) multi_cycle <= rst ? 1'h0 : input_advance ? input_port_access_len == 2'h2 &  ~ is_csr : multi_cycle;
	assign bus_request_advance = bus_req_if_ready & bus_req_if_valid;
	always_ff @(posedge clk) active <= rst ? 1'h0 : (input_port_access_len == 2'h2 &  ~ is_csr & input_advance) ? 1'h1 : bus_request_advance ? 1'h0 : active;
	always_ff @(posedge clk) pending <= rst ? 1'h0 : (input_port_access_len == 2'h2 &  ~ is_csr & input_advance & input_port_read_not_write) ? 1'h1 : bus_rsp_if_valid ? 1'h0 : pending;
	always_ff @(posedge clk) gap <= rst ? 1'h0 : (input_port_access_len != 2'h2 &  ~ is_csr & input_advance | active) ? 1'h1 : 1'h0;
	assign csr_select = input_port_addr[31:28] == 4'hc;
	always_ff @(posedge clk) u47_output_port <= rst ? 1'h0 : input_advance ? csr_select : output_port_valid ? 1'h0 : is_csr;
	assign is_csr = input_advance ? csr_select : u47_output_port;
	assign input_port_ready = bus_req_if_ready &  ~ active;
	assign bus_req_if_valid = (input_port_valid &  ~ csr_select | active) &  ~ gap;
	assign output_port_valid = bus_rsp_if_valid &  ~ pending | csr_pen & csr_if_pready &  ~ csr_if_pwrite;
	always_ff @(posedge clk) u83_output_port <= rst ? 1'h0 : input_port_ready & input_port_valid ? input_port_read_not_write : u83_output_port;
	assign bus_req_if_read_not_write = (input_port_ready & input_port_valid) ? input_port_read_not_write : u83_output_port;
	always_ff @(posedge clk) u87_output_port <= rst ? 2'h0 : input_port_ready & input_port_valid ? byte_en : u87_output_port;
	assign bus_req_if_byte_en = (input_port_ready & input_port_valid) ? byte_en : u87_output_port;
	always_ff @(posedge clk) next_addr <= rst ? 26'h0 : input_advance ? u62_output_port[25:0] : next_addr;
	assign bus_req_if_addr = input_advance ? input_port_addr[26:1] : next_addr;
	assign is_nram = input_port_addr[31:28] == 4'hf;
	always_ff @(posedge clk) u93_output_port <= rst ? 1'h0 : input_port_ready & input_port_valid ?  ~ is_nram : u93_output_port;
	assign bus_req_if_dram_not_ext = (input_port_ready & input_port_valid) ?  ~ is_nram : u93_output_port;
	always_ff @(posedge clk) data_store <= rst ? 16'h0 : input_advance ? input_port_data[31:16] : (bus_rsp_if_valid & pending) ? bus_rsp_if_data : 16'hx;
	assign bus_req_if_data = input_advance ? input_port_data[15:0] : data_store;
	assign output_port_data_l = csr_pen ? csr_if_prdata[15:0] : multi_cycle ? data_store : bus_rsp_if_data;
	assign output_port_data_h = csr_pen ? csr_if_prdata[31:16] : bus_rsp_if_data;
	always_ff @(posedge clk) csr_pen <= rst ? 1'h0 : input_advance ? is_csr : (csr_if_pready & is_csr) ? 1'h0 : csr_pen;
	assign csr_if_psel = input_advance & is_csr | csr_pen;
	always_ff @(posedge clk) u112_output_port <= rst ? 1'h0 : input_port_ready & input_port_valid ?  ~ input_port_read_not_write : u112_output_port;
	assign csr_if_pwrite = (input_port_ready & input_port_valid) ?  ~ input_port_read_not_write : u112_output_port;
	always_ff @(posedge clk) u116_output_port <= rst ? 10'h0 : input_port_ready & input_port_valid ? input_port_addr[11:2] : u116_output_port;
	assign csr_if_paddr = (input_port_ready & input_port_valid) ? input_port_addr[11:2] : u116_output_port;
	always_ff @(posedge clk) u121_output_port <= rst ? 32'h0 : input_port_ready & input_port_valid ? input_port_data : u121_output_port;
	assign csr_if_pwdata = (input_port_ready & input_port_valid) ? input_port_data : u121_output_port;
	assign byte_en = {input_port_access_len != 1'h0 | input_port_addr[0], input_port_access_len != 1'h0 |  ~ input_port_addr[0]};

	assign u62_output_port = input_port_addr[26:1] | 1'h1;
	assign csr_if_penable = csr_pen;
endmodule


////////////////////////////////////////////////////////////////////////////////
// BranchUnit
////////////////////////////////////////////////////////////////////////////////
module BranchUnit (
	input logic [30:0] input_port_branch_addr,
	input logic input_port_f_carry,
	input logic input_port_f_overflow,
	input logic input_port_f_sign,
	input logic input_port_f_zero,
	input logic input_port_fetch_av,
	input logic input_port_interrupt,
	input logic input_port_is_branch_insn,
	input logic input_port_mem_av,
	input logic input_port_mem_unaligned,
	input logic [31:0] input_port_op_a,
	input logic [31:0] input_port_op_b,
	input logic [31:0] input_port_op_c,
	input logic [3:0] input_port_opcode,
	input logic [30:0] input_port_pc,
	input logic [30:0] input_port_spc,
	input logic input_port_task_mode,
	input logic [30:0] input_port_tpc,

	output logic output_port_do_branch,
	output logic [11:0] output_port_ecause,
	output logic [30:0] output_port_spc,
	output logic output_port_spc_changed,
	output logic output_port_task_mode,
	output logic output_port_task_mode_changed,
	output logic [30:0] output_port_tpc,
	output logic output_port_tpc_changed
);

	logic condition_result;
	logic is_exception;
	logic in_mode_branch;
	logic [30:0] branch_target;
	logic [10:0] exception_mask;
	logic [11:0] interrupt_mask;
	logic u21_output_port;
	logic u18_output_port;

	assign is_exception = input_port_is_branch_insn & input_port_opcode == `branch_ops__swi | input_port_mem_av | input_port_mem_unaligned | input_port_fetch_av;
	assign condition_result = input_port_is_branch_insn & (
		(input_port_opcode == `branch_ops__cb_eq ? input_port_f_zero : 1'b0) | 
		(input_port_opcode == `branch_ops__cb_ne ?  ~ input_port_f_zero : 1'b0) | 
		(input_port_opcode == `branch_ops__cb_lts ? input_port_f_sign != input_port_f_overflow : 1'b0) | 
		(input_port_opcode == `branch_ops__cb_ges ? input_port_f_sign == input_port_f_overflow : 1'b0) | 
		(input_port_opcode == `branch_ops__cb_lt ? input_port_f_carry : 1'b0) | 
		(input_port_opcode == `branch_ops__cb_ge ?  ~ input_port_f_carry : 1'b0) | 
		(input_port_opcode == `branch_ops__bb_one ? u18_output_port : 1'b0) | 
		(input_port_opcode == `branch_ops__bb_zero ?  ~ u21_output_port : 1'b0) );
	assign branch_target = (input_port_is_branch_insn & input_port_opcode == `branch_ops__pc_w ? input_port_op_a[31:1] : 31'b0) | (input_port_is_branch_insn & input_port_opcode == `branch_ops__tpc_w ? input_port_op_a[31:1] : 31'b0) | (input_port_is_branch_insn & input_port_opcode == `branch_ops__stm ? input_port_tpc : 31'b0) | (is_exception | input_port_interrupt) ? input_port_tpc : input_port_branch_addr;
	assign output_port_spc = is_exception ? 1'h0 : branch_target;
	assign in_mode_branch = 
		(input_port_is_branch_insn & input_port_opcode == `branch_ops__pc_w ? 1'h1 : 1'b0) | 
		(input_port_is_branch_insn & input_port_opcode == `branch_ops__tpc_w ? input_port_task_mode : 1'b0) | 
		(is_exception ?  ~ input_port_task_mode : 1'b0) | 
		(input_port_is_branch_insn & input_port_opcode == `branch_ops__stm ? 1'h0 : 1'b0) | 
		condition_result;
	assign output_port_spc_changed =  ~ input_port_task_mode & (is_exception | in_mode_branch);
	assign output_port_tpc_changed = input_port_task_mode ? in_mode_branch | is_exception | input_port_interrupt : input_port_is_branch_insn & input_port_opcode == `branch_ops__tpc_w;
	assign output_port_task_mode_changed = input_port_task_mode ? is_exception | input_port_interrupt : input_port_is_branch_insn & input_port_opcode == `branch_ops__stm;
	assign output_port_task_mode = input_port_task_mode ^ output_port_task_mode_changed;
	assign output_port_do_branch = in_mode_branch | output_port_task_mode_changed;
	assign exception_mask = input_port_fetch_av ? 11'h400 : ((input_port_is_branch_insn & input_port_opcode == `branch_ops__swi) ? 1'h1 << input_port_op_a[2:0] : 1'h0) | (input_port_mem_av ? 10'h200 : 1'h0) | (input_port_mem_unaligned ? 9'h100 : 1'h0);
	assign interrupt_mask = (input_port_interrupt & input_port_task_mode) ? 12'h800 : 1'h0;
	assign output_port_ecause = exception_mask | interrupt_mask;

	DecoratorModule_6 u21 (
		.output_port(u21_output_port),
		.word(input_port_op_a),
		.bit_code(input_port_op_b)
	);

	DecoratorModule_7 u18 (
		.output_port(u18_output_port),
		.word(input_port_op_a),
		.bit_code(input_port_op_b)
	);

	assign output_port_tpc = branch_target;
endmodule


////////////////////////////////////////////////////////////////////////////////
// BranchTargetUnit
////////////////////////////////////////////////////////////////////////////////
module BranchTargetUnit (
	input logic [1:0] input_port_inst_len,
	input logic [31:0] input_port_op_c,
	input logic [30:0] input_port_pc,

	output logic [30:0] output_port_branch_addr,
	output logic [30:0] output_port_straight_addr
);

	logic [31:0] u18_output_port;
	logic [31:0] u21_output_port;

	assign output_port_branch_addr = u18_output_port[30:0];
	assign output_port_straight_addr = u21_output_port[30:0];

	assign u18_output_port = input_port_pc + ({input_port_op_c[0], input_port_op_c[0], input_port_op_c[0], input_port_op_c[0], input_port_op_c[0], input_port_op_c[0], input_port_op_c[0], input_port_op_c[0], input_port_op_c[0], input_port_op_c[0], input_port_op_c[0], input_port_op_c[0], input_port_op_c[0], input_port_op_c[0], input_port_op_c[0], input_port_op_c[0], input_port_op_c[15:1]});
	assign u21_output_port = input_port_pc + input_port_inst_len + 1'h1;
endmodule


////////////////////////////////////////////////////////////////////////////////
// LoadStoreUnit
////////////////////////////////////////////////////////////////////////////////
module LoadStoreUnit (
	input logic clk,
	input logic rst,
	input logic input_port_is_ldst,
	input logic [1:0] input_port_mem_access_len,
	input logic [21:0] input_port_mem_base,
	input logic [21:0] input_port_mem_limit,
	input logic [31:0] input_port_op_b,
	input logic [31:0] input_port_op_c,
	input logic input_port_task_mode,

	output logic output_port_mem_av,
	output logic output_port_mem_unaligned,
	output logic [31:0] output_port_phy_addr
);

	logic [32:0] u_output_port;
	logic [31:0] eff_addr;
	logic [32:0] u7_output_port;
	logic mem_av;
	logic u14_output_port;
	logic mem_unaligned;

	always_ff @(posedge clk) eff_addr <= rst ? 32'h0 : u_output_port[31:0];
	always @(*) begin
		unique case (input_port_mem_access_len)
			2'd0: u14_output_port = 1'h0;
			2'd1: u14_output_port = eff_addr[0];
			2'd2: u14_output_port = eff_addr[0] | eff_addr[1];
			2'd3: u14_output_port = 1'h1;
		endcase
	end
	assign output_port_phy_addr = u7_output_port[31:0];
	assign mem_av = input_port_is_ldst & eff_addr[31:10] > input_port_mem_limit;
	assign mem_unaligned = input_port_is_ldst & u14_output_port;

	assign u_output_port = input_port_op_b + input_port_op_c;
	assign u7_output_port = eff_addr + (input_port_task_mode ? input_port_mem_base << 4'ha : 1'h0);
	assign output_port_mem_av = mem_av;
	assign output_port_mem_unaligned = mem_unaligned;
endmodule


////////////////////////////////////////////////////////////////////////////////
// AluUnit
////////////////////////////////////////////////////////////////////////////////
module AluUnit (
	input logic clk,
	input logic rst,
	input logic [31:0] input_port_op_a,
	input logic [31:0] input_port_op_b,
	input logic [2:0] input_port_opcode,
	input logic [30:0] input_port_pc,
	input logic [30:0] input_port_tpc,

	output logic output_port_f_carry,
	output logic output_port_f_overflow,
	output logic output_port_f_sign,
	output logic output_port_f_zero,
	output logic [31:0] output_port_result
);

	logic c_in;
	logic [31:0] xor_b;
	logic [31:0] b;
	logic [31:0] a;
	logic [32:0] sum;
	logic [31:0] and_result;
	logic [31:0] xor_result;
	logic use_adder;
	logic use_and;
	logic [32:0] adder_result;

	assign c_in = input_port_opcode == `alu_ops__a_minus_b;
	assign xor_b = (input_port_opcode == `alu_ops__a_minus_b | input_port_opcode == `alu_ops__n_b_and_a) ? 32'hffffffff : 1'h0;
	assign a = input_port_opcode == `alu_ops__pc_plus_b ? ({input_port_pc, 1'h0}) : input_port_op_a;
	assign b = xor_b ^ input_port_op_b;
	assign use_adder = input_port_opcode == `alu_ops__a_plus_b | input_port_opcode == `alu_ops__a_minus_b;
	assign sum = a + b + c_in;
	assign use_and = input_port_opcode == `alu_ops__a_and_b | input_port_opcode == `alu_ops__n_b_and_a;
	assign and_result = a & b;
	assign xor_result = input_port_op_a ^ input_port_op_b;
	assign adder_result = 
		(use_adder ? sum : 33'b0) | 
		(input_port_opcode == `alu_ops__a_or_b ? input_port_op_a | input_port_op_b : 33'b0) | 
		(use_and ? and_result : 33'b0) | 
		(input_port_opcode == `alu_ops__a_xor_b ? xor_result : 33'b0) | 
		(input_port_opcode == `alu_ops__tpc ? ({input_port_tpc, 1'h0}) : 33'b0) | 
		(input_port_opcode == `alu_ops__pc_plus_b ? sum : 33'b0) ;
	assign output_port_result = adder_result[31:0];
	assign output_port_f_zero = xor_result == 1'h0;
	assign output_port_f_sign = adder_result[31];
	assign output_port_f_carry = adder_result[32] ^ input_port_opcode == `alu_ops__a_minus_b;
	assign output_port_f_overflow = input_port_op_a[31] != input_port_op_b[31] & input_port_op_a[31] != adder_result[31];

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

	output logic [2:0] output_port_alu_op,
	output logic [3:0] output_port_branch_op,
	output logic output_port_do_bse,
	output logic output_port_do_bze,
	output logic output_port_do_wse,
	output logic output_port_do_wze,
	output logic [2:0] output_port_exec_unit,
	output logic output_port_fetch_av,
	output logic [1:0] output_port_inst_len,
	output logic output_port_ldst_op,
	output logic [1:0] output_port_mem_access_len,
	output logic [31:0] output_port_op_a,
	output logic [31:0] output_port_op_b,
	output logic [31:0] output_port_op_c,
	input logic output_port_ready,
	output logic [3:0] output_port_result_reg_addr,
	output logic output_port_result_reg_addr_valid,
	output logic [1:0] output_port_shifter_op,
	output logic output_port_valid,

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
	logic mask_for_rd_eq_ra_lsl_rb;
	logic mask_for_rd_eq_ra_lsr_rb;
	logic mask_for_rd_eq_ra_asr_rb;
	logic mask_for_rd_eq_field_e_lsl_rb;
	logic mask_for_rd_eq_field_e_lsr_rb;
	logic mask_for_rd_eq_field_e_asr_rb;
	logic mask_for_rd_eq_field_e_lsl_ra;
	logic mask_for_rd_eq_field_e_lsr_ra;
	logic mask_for_rd_eq_field_e_asr_ra;
	logic mask_for_rd_eq_ra_times_rb;
	logic mask_for_rd_eq_field_e_times_rb;
	logic mask_for_rd_eq_field_e_times_ra;
	logic mask_for_swi;
	logic mask_for_stm;
	logic mask_for_woi;
	logic mask_for_sii;
	logic mask_for_fence;
	logic mask_for_pc_eq_rd;
	logic mask_for_tpc_eq_rd;
	logic mask_for_rd_eq_pc;
	logic mask_for_rd_eq_tpc;
	logic mask_for_sii_1;
	logic mask_for_rd_eq_tiny_field_a;
	logic mask_for_rd_eq_pc_plus_field_atimes2;
	logic mask_for_rd_eq_minus_ra;
	logic mask_for_rd_eq_notra;
	logic mask_for_rd_eq_bse_ra;
	logic mask_for_rd_eq_wse_ra;
	logic mask_for_sii_2;
	logic mask_for_rd_eq_ra_xor_rb;
	logic mask_for_rd_eq_ra_or_rb;
	logic mask_for_rd_eq_ra_and_rb;
	logic mask_for_rd_eq_ra_plus_rb;
	logic mask_for_rd_eq_ra_minus_rb;
	logic mask_for_rd_eq_notra_and_rb;
	logic mask_for_rd_eq_tiny_rb_plus_field_a;
	logic mask_for_rd_eq_value;
	logic mask_for_pc_eq_value;
	logic mask_for_tpc_eq_value;
	logic mask_for_sii_3;
	logic mask_for_sii_4;
	logic mask_for_rd_eq_field_e_xor_rb;
	logic mask_for_rd_eq_field_e_or_rb;
	logic mask_for_rd_eq_field_e_and_rb;
	logic mask_for_rd_eq_field_e_plus_rb;
	logic mask_for_rd_eq_field_e_minus_rb;
	logic mask_for_sii_5;
	logic mask_for_sii_6;
	logic mask_for_rd_eq_short_value;
	logic mask_for_pc_eq_short_value;
	logic mask_for_tpc_eq_short_value;
	logic mask_for_rd_eq_field_e_xor_ra;
	logic mask_for_rd_eq_field_e_or_ra;
	logic mask_for_rd_eq_field_e_and_ra;
	logic mask_for_rd_eq_field_e_plus_ra;
	logic mask_for_rd_eq_field_e_minus_ra;
	logic mask_for_sii_7;
	logic mask_for_sii_8;
	logic mask_for_if_ra_eq_0;
	logic mask_for_if_ra_ne_0;
	logic mask_for_if_ra_lt_0;
	logic mask_for_if_ra_ge_0;
	logic mask_for_if_ra_gt_0;
	logic mask_for_if_ra_le_0;
	logic mask_for_sii_9;
	logic mask_for_sii_10;
	logic mask_for_if_ra_eq_0_1;
	logic mask_for_if_ra_ne_0_1;
	logic mask_for_if_ra_lt_0_1;
	logic mask_for_if_ra_ge_0_1;
	logic mask_for_if_ra_gt_0_1;
	logic mask_for_if_ra_le_0_1;
	logic mask_for_sii_11;
	logic mask_for_if_rb_eq_ra;
	logic mask_for_if_rb_ne_ra;
	logic mask_for_if_signed_rb_lt_ra;
	logic mask_for_if_signed_rb_ge_ra;
	logic mask_for_if_rb_lt_ra;
	logic mask_for_if_rb_ge_ra;
	logic mask_for_sii_12;
	logic mask_for_sii_13;
	logic mask_for_if_rb_eq_ra_1;
	logic mask_for_if_rb_ne_ra_1;
	logic mask_for_if_signed_rb_lt_ra_1;
	logic mask_for_if_signed_rb_ge_ra_1;
	logic mask_for_if_rb_lt_ra_1;
	logic mask_for_if_rb_ge_ra_1;
	logic mask_for_if_ra_bit__eq_1;
	logic mask_for_if_rb_bit__eq_0;
	logic mask_for_mem_raplustiny_ofstimes4_eq_rd;
	logic mask_for_rd_eq_mem_raplustiny_ofstimes4;
	logic mask_for_sii_14;
	logic mask_for_sii_15;
	logic mask_for_sii_16;
	logic mask_for_sii_17;
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
	logic mask_for_sii_18;
	logic mask_for_sii_19;
	logic mask_for_sii_20;
	logic mask_for_sii_21;
	logic mask_for_sii_22;
	logic mask_for_sii_23;
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
	logic mask_for_sii_24;
	logic mask_for_sii_25;
	logic mask_for_sii_26;
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
	logic mask_for_sii_27;
	logic mask_for_sii_28;
	logic mask_for_sii_29;
	logic expr;
	logic mask_expr;
	logic group_1_for_exec_unit;
	logic group_2_for_exec_unit;
	logic group_3_for_exec_unit;
	logic [2:0] exec_unit;
	logic group_1_for_alu_op;
	logic group_2_for_alu_op;
	logic group_3_for_alu_op;
	logic group_4_for_alu_op;
	logic group_5_for_alu_op;
	logic group_6_for_alu_op;
	logic group_7_for_alu_op;
	logic group_8_for_alu_op;
	logic [2:0] alu_op;
	logic group_1_for_branch_op;
	logic group_2_for_branch_op;
	logic group_3_for_branch_op;
	logic group_4_for_branch_op;
	logic group_5_for_branch_op;
	logic group_6_for_branch_op;
	logic group_7_for_branch_op;
	logic group_8_for_branch_op;
	logic group_9_for_branch_op;
	logic group_10_for_branch_op;
	logic group_11_for_branch_op;
	logic [3:0] branch_op;
	logic group_1_for_ldst_op;
	logic group_2_for_ldst_op;
	logic ldst_op;
	logic group_1_for_rd1_addr;
	logic group_2_for_rd1_addr;
	logic group_3_for_rd1_addr;
	logic [3:0] rd1_addr;
	logic group_1_for_res_addr;
	logic [3:0] res_addr;
	logic group_1_for_rd2_addr;
	logic group_2_for_rd2_addr;
	logic group_3_for_rd2_addr;
	logic [3:0] rd2_addr;
	logic group_1_for_use_reg_a;
	logic use_reg_a;
	logic group_1_for_use_reg_b;
	logic use_reg_b;
	logic group_1_for_op_a;
	logic group_2_for_op_a;
	logic group_3_for_op_a;
	logic group_4_for_op_a;
	logic group_5_for_op_a;
	logic [31:0] op_a;
	logic group_1_for_op_b;
	logic group_2_for_op_b;
	logic group_3_for_op_b;
	logic [31:0] op_b;
	logic group_1_for_op_c;
	logic group_2_for_op_c;
	logic group_3_for_op_c;
	logic [31:0] op_c;
	logic group_1_for_mem_len;
	logic group_2_for_mem_len;
	logic group_3_for_mem_len;
	logic [1:0] mem_len;
	logic group_1_for_bse;
	logic bse;
	logic group_1_for_wse;
	logic wse;
	logic group_1_for_bze;
	logic bze;
	logic group_1_for_wze;
	logic wze;
	logic group_1_for_read1_needed;
	logic read1_needed;
	logic group_1_for_read2_needed;
	logic read2_needed;
	logic group_1_for_rsv_needed;
	logic rsv_needed;
	logic register_outputs;
	logic u1766_output_port;
	logic [31:0] u1767_output_port;
	logic u1769_output_port;
	logic [31:0] u1770_output_port;
	logic [1:0] u1773_output_port;
	logic [3:0] u22_output_port;
	logic [3:0] field_a_plus_one;

	assign field_a_plus_one = u36_output_port[3:0];
	assign field_d_is_f = fetch_inst_0[15:12] == 4'hf;
	assign field_b_is_f = fetch_inst_0[7:4] == 4'hf;
	assign field_a_is_f = fetch_inst_0[3:0] == 4'hf;
	assign field_c_is_f = fetch_inst_0[11:8] == 4'hf;
	assign mask_for_rd_eq_ra_lsl_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_ra_lsr_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h7 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_ra_asr_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'h8 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_lsl_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_lsr_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h7 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_asr_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'h8 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_lsl_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h6 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_lsr_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h7 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_asr_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'h8 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_rd_eq_ra_times_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'h9 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_times_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'h9 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_times_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'h9 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_swi = 1'h1 & fetch_inst_0[15:12] < 4'h8 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h0;
	assign mask_for_stm = 1'h1 & fetch_inst_0[15:12] == 4'h8 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h0;
	assign mask_for_woi = 1'h1 & fetch_inst_0[15:12] == 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h0;
	assign mask_for_sii = 1'h1 & fetch_inst_0[15:12] > 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h0;
	assign mask_for_pc_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 2'h2;
	assign mask_for_tpc_eq_rd = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 2'h3;
	assign mask_for_rd_eq_pc = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 3'h4;
	assign mask_for_rd_eq_tpc = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 3'h5;
	assign mask_for_sii_1 = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] > 3'h5;
	assign mask_for_sii_2 = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] > 3'h6 &  ~ field_a_is_f;
	assign mask_for_pc_eq_value = 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_tpc_eq_value = 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_sii_3 = 1'h1 & fetch_inst_0[15:12] == 4'h8 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_sii_4 = 1'h1 & fetch_inst_0[15:12] == 4'h9 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_sii_5 = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_sii_6 = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_pc_eq_short_value = 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'hf & fetch_inst_0[3:0] == 4'he;
	assign mask_for_tpc_eq_short_value = 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'hf & fetch_inst_0[3:0] == 4'he;
	assign mask_for_sii_7 = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_sii_8 = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_if_ra_eq_0 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f;
	assign mask_for_if_ra_ne_0 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f;
	assign mask_for_if_ra_lt_0 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f;
	assign mask_for_if_ra_ge_0 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f;
	assign mask_for_if_ra_gt_0 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h4 &  ~ field_a_is_f;
	assign mask_for_if_ra_le_0 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h5 &  ~ field_a_is_f;
	assign mask_for_sii_9 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h6 &  ~ field_a_is_f;
	assign mask_for_sii_10 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 3'h7 &  ~ field_a_is_f;
	assign mask_for_if_ra_eq_0_1 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'h8 &  ~ field_a_is_f;
	assign mask_for_if_ra_ne_0_1 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'h9 &  ~ field_a_is_f;
	assign mask_for_if_ra_lt_0_1 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'ha &  ~ field_a_is_f;
	assign mask_for_if_ra_ge_0_1 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'hb &  ~ field_a_is_f;
	assign mask_for_if_ra_gt_0_1 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'hc &  ~ field_a_is_f;
	assign mask_for_if_ra_le_0_1 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'hd &  ~ field_a_is_f;
	assign mask_for_sii_11 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f;
	assign mask_for_if_rb_eq_ra = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_rb_ne_ra = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_signed_rb_lt_ra = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_signed_rb_ge_ra = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_rb_lt_ra = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_rb_ge_ra = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h6 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_sii_12 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 3'h7 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_sii_13 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'h8 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_rb_eq_ra_1 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'h9 &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_rb_ne_ra_1 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'ha &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_signed_rb_lt_ra_1 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'hb &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_signed_rb_ge_ra_1 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'hc &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_rb_lt_ra_1 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'hd &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_rb_ge_ra_1 = 1'h1 & fetch_inst_0[15:12] == 4'hf & fetch_inst_0[11:8] == 4'he &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_if_ra_bit__eq_1 = 1'h1 & fetch_inst_0[15:12] == 4'hf &  ~ field_c_is_f & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_if_rb_bit__eq_0 = 1'h1 & fetch_inst_0[15:12] == 4'hf &  ~ field_c_is_f &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_sii_14 = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h0 &  ~ field_a_is_f;
	assign mask_for_sii_15 = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 1'h1 &  ~ field_a_is_f;
	assign mask_for_sii_16 = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h2 &  ~ field_a_is_f;
	assign mask_for_sii_17 = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 2'h3 &  ~ field_a_is_f;
	assign mask_for_sii_18 = 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f;
	assign mask_for_sii_19 = 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f;
	assign mask_for_sii_20 = 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f;
	assign mask_for_sii_21 = 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f;
	assign mask_for_sii_22 = 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f;
	assign mask_for_sii_23 = 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'he & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f;
	assign mask_for_sii_24 = 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f;
	assign mask_for_sii_25 = 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f;
	assign mask_for_sii_26 = 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he &  ~ field_a_is_f;
	assign mask_for_sii_27 = 1'h1 & fetch_inst_0[15:12] == 1'h1 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_sii_28 = 1'h1 & fetch_inst_0[15:12] == 2'h2 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf;
	assign mask_expr = 1'h1 & fetch_inst_0[15:12] == 2'h3 & fetch_inst_0[11:8] == 4'hf & fetch_inst_0[7:4] == 4'he & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_fence = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 1'h1;
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
	assign mask_for_rd_eq_notra_and_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'ha &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_tiny_rb_plus_field_a = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 4'hb &  ~ field_b_is_f &  ~ field_a_is_f;
	assign mask_for_rd_eq_value = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 1'h0 & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_xor_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h1 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_or_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 2'h2 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_and_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 2'h3 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_plus_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h4 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_field_e_minus_rb = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h5 &  ~ field_b_is_f & fetch_inst_0[3:0] == 4'hf;
	assign mask_for_rd_eq_short_value = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h0 & fetch_inst_0[7:4] == 4'hf & fetch_inst_0[3:0] == 1'h0;
	assign mask_for_rd_eq_field_e_xor_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 1'h1 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_or_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 2'h2 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_and_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 2'h3 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_plus_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h4 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
	assign mask_for_rd_eq_field_e_minus_ra = 1'h1 &  ~ field_d_is_f & fetch_inst_0[11:8] == 3'h5 & fetch_inst_0[7:4] == 4'hf &  ~ field_a_is_f;
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
	assign group_1_for_exec_unit = mask_for_rd_eq_ra_lsl_rb | mask_for_rd_eq_ra_lsr_rb | mask_for_rd_eq_ra_asr_rb | mask_for_rd_eq_field_e_lsl_rb | mask_for_rd_eq_field_e_lsr_rb | mask_for_rd_eq_field_e_asr_rb | mask_for_rd_eq_field_e_lsl_ra | mask_for_rd_eq_field_e_lsr_ra | mask_for_rd_eq_field_e_asr_ra | mask_for_rd_eq_ra_times_rb | mask_for_rd_eq_field_e_times_rb | mask_for_rd_eq_field_e_times_ra | mask_for_swi | mask_for_stm | mask_for_woi | mask_for_sii | mask_for_pc_eq_rd | mask_for_tpc_eq_rd | mask_for_rd_eq_pc | mask_for_rd_eq_tpc | mask_for_sii_1 | mask_for_sii_2 | mask_for_pc_eq_value | mask_for_tpc_eq_value | mask_for_sii_3 | mask_for_sii_4 | mask_for_sii_5 | mask_for_sii_6 | mask_for_pc_eq_short_value | mask_for_tpc_eq_short_value | mask_for_sii_7 | mask_for_sii_8 | mask_for_if_ra_eq_0 | mask_for_if_ra_ne_0 | mask_for_if_ra_lt_0 | mask_for_if_ra_ge_0 | mask_for_if_ra_gt_0 | mask_for_if_ra_le_0 | mask_for_sii_9 | mask_for_sii_10 | mask_for_if_ra_eq_0_1 | mask_for_if_ra_ne_0_1 | mask_for_if_ra_lt_0_1 | mask_for_if_ra_ge_0_1 | mask_for_if_ra_gt_0_1 | mask_for_if_ra_le_0_1 | mask_for_sii_11 | mask_for_if_rb_eq_ra | mask_for_if_rb_ne_ra | mask_for_if_signed_rb_lt_ra | mask_for_if_signed_rb_ge_ra | mask_for_if_rb_lt_ra | mask_for_if_rb_ge_ra | mask_for_sii_12 | mask_for_sii_13 | mask_for_if_rb_eq_ra_1 | mask_for_if_rb_ne_ra_1 | mask_for_if_signed_rb_lt_ra_1 | mask_for_if_signed_rb_ge_ra_1 | mask_for_if_rb_lt_ra_1 | mask_for_if_rb_ge_ra_1 | mask_for_if_ra_bit__eq_1 | mask_for_if_rb_bit__eq_0 | mask_for_sii_14 | mask_for_sii_15 | mask_for_sii_16 | mask_for_sii_17 | mask_for_sii_18 | mask_for_sii_19 | mask_for_sii_20 | mask_for_sii_21 | mask_for_sii_22 | mask_for_sii_23 | mask_for_sii_24 | mask_for_sii_25 | mask_for_sii_26 | mask_for_sii_27 | mask_for_sii_28 | mask_expr;
	assign group_2_for_exec_unit = mask_for_fence | mask_for_rd_eq_tiny_field_a | mask_for_rd_eq_pc_plus_field_atimes2 | mask_for_rd_eq_minus_ra | mask_for_rd_eq_notra | mask_for_rd_eq_bse_ra | mask_for_rd_eq_wse_ra | mask_for_rd_eq_ra_xor_rb | mask_for_rd_eq_ra_or_rb | mask_for_rd_eq_ra_and_rb | mask_for_rd_eq_ra_plus_rb | mask_for_rd_eq_ra_minus_rb | mask_for_rd_eq_notra_and_rb | mask_for_rd_eq_tiny_rb_plus_field_a | mask_for_rd_eq_value | mask_for_rd_eq_field_e_xor_rb | mask_for_rd_eq_field_e_or_rb | mask_for_rd_eq_field_e_and_rb | mask_for_rd_eq_field_e_plus_rb | mask_for_rd_eq_field_e_minus_rb | mask_for_rd_eq_short_value | mask_for_rd_eq_field_e_xor_ra | mask_for_rd_eq_field_e_or_ra | mask_for_rd_eq_field_e_and_ra | mask_for_rd_eq_field_e_plus_ra | mask_for_rd_eq_field_e_minus_ra;
	assign group_3_for_exec_unit = mask_for_mem_raplustiny_ofstimes4_eq_rd | mask_for_rd_eq_mem_raplustiny_ofstimes4 | mask_for_rd_eq_mem8_ra | mask_for_rd_eq_mem16_ra | mask_for_rd_eq_mem32_ra | mask_for_rd_eq_memll32_ra | mask_for_mem8_ra_eq_rd | mask_for_mem16_ra_eq_rd | mask_for_mem32_ra_eq_rd | mask_for_memsr32_ra_eq_rd | mask_for_rd_eq_smem8_ra | mask_for_rd_eq_smem16_ra | mask_for_rd_eq_mem8_raplusfield_e | mask_for_rd_eq_mem16_raplusfield_e | mask_for_rd_eq_mem32_raplusfield_e | mask_for_rd_eq_memll32_raplusfield_e | mask_for_mem8_raplusfield_e_eq_rd | mask_for_mem16_raplusfield_e_eq_rd | mask_for_mem32_raplusfield_e_eq_rd | mask_for_memsr32_raplusfield_e_eq_rd | mask_for_rd_eq_smem8_raplusfield_e | mask_for_rd_eq_smem16_raplusfield_e | mask_for_rd_eq_mem8_field_e | mask_for_rd_eq_mem16_field_e | mask_for_rd_eq_mem32_field_e | mask_for_rd_eq_memll32_field_e | mask_for_mem8_field_e_eq_rd | mask_for_mem16_field_e_eq_rd | mask_for_mem32_field_e_eq_rd | mask_for_memsr32_field_e_eq_rd | mask_for_rd_eq_smem8_field_e | mask_for_rd_eq_smem16_field_e;
	assign group_1_for_alu_op = mask_for_woi | mask_for_rd_eq_minus_ra | mask_for_rd_eq_ra_minus_rb | mask_for_rd_eq_field_e_minus_rb | mask_for_rd_eq_field_e_minus_ra;
	assign group_2_for_alu_op = mask_for_rd_eq_pc | mask_for_rd_eq_pc_plus_field_atimes2;
	assign group_3_for_alu_op = mask_for_rd_eq_tpc;
	assign group_4_for_alu_op = mask_for_rd_eq_tiny_field_a | mask_for_rd_eq_bse_ra | mask_for_rd_eq_wse_ra | mask_for_rd_eq_ra_or_rb | mask_for_rd_eq_value | mask_for_rd_eq_field_e_or_rb | mask_for_rd_eq_short_value | mask_for_rd_eq_field_e_or_ra;
	assign group_5_for_alu_op = mask_for_rd_eq_notra | mask_for_rd_eq_ra_xor_rb | mask_for_rd_eq_field_e_xor_rb | mask_for_rd_eq_field_e_xor_ra;
	assign group_6_for_alu_op = mask_for_rd_eq_ra_and_rb | mask_for_rd_eq_field_e_and_rb | mask_for_rd_eq_field_e_and_ra;
	assign group_7_for_alu_op = mask_for_rd_eq_ra_plus_rb | mask_for_rd_eq_tiny_rb_plus_field_a | mask_for_rd_eq_field_e_plus_rb | mask_for_rd_eq_field_e_plus_ra;
	assign group_8_for_alu_op = mask_for_rd_eq_notra_and_rb;
	assign group_1_for_branch_op = mask_for_rd_eq_ra_lsl_rb | mask_for_rd_eq_ra_lsr_rb | mask_for_rd_eq_ra_asr_rb | mask_for_rd_eq_field_e_lsl_rb | mask_for_rd_eq_field_e_lsr_rb | mask_for_rd_eq_field_e_asr_rb | mask_for_rd_eq_field_e_lsl_ra | mask_for_rd_eq_field_e_lsr_ra | mask_for_rd_eq_field_e_asr_ra | mask_for_rd_eq_ra_times_rb | mask_for_rd_eq_field_e_times_rb | mask_for_rd_eq_field_e_times_ra | mask_for_swi | mask_for_sii | mask_for_sii_1 | mask_for_sii_2 | mask_for_sii_3 | mask_for_sii_4 | mask_for_sii_5 | mask_for_sii_6 | mask_for_sii_7 | mask_for_sii_8 | mask_for_sii_9 | mask_for_sii_10 | mask_for_sii_11 | mask_for_sii_12 | mask_for_sii_13 | mask_for_sii_14 | mask_for_sii_15 | mask_for_sii_16 | mask_for_sii_17 | mask_for_sii_18 | mask_for_sii_19 | mask_for_sii_20 | mask_for_sii_21 | mask_for_sii_22 | mask_for_sii_23 | mask_for_sii_24 | mask_for_sii_25 | mask_for_sii_26 | mask_for_sii_27 | mask_for_sii_28 | mask_expr;
	assign group_2_for_branch_op = mask_for_stm;
	assign group_3_for_branch_op = mask_for_woi | mask_for_if_ra_eq_0 | mask_for_if_ra_eq_0_1 | mask_for_if_rb_eq_ra | mask_for_if_rb_eq_ra_1;
	assign group_4_for_branch_op = mask_for_pc_eq_rd | mask_for_pc_eq_value | mask_for_pc_eq_short_value;
	assign group_5_for_branch_op = mask_for_tpc_eq_rd | mask_for_tpc_eq_value | mask_for_tpc_eq_short_value;
	assign group_6_for_branch_op = mask_for_if_ra_ne_0 | mask_for_if_ra_ne_0_1 | mask_for_if_rb_ne_ra | mask_for_if_rb_ne_ra_1;
	assign group_7_for_branch_op = mask_for_if_ra_lt_0 | mask_for_if_ra_gt_0 | mask_for_if_ra_lt_0_1 | mask_for_if_ra_gt_0_1 | mask_for_if_signed_rb_lt_ra | mask_for_if_signed_rb_lt_ra_1;
	assign group_8_for_branch_op = mask_for_if_ra_ge_0 | mask_for_if_ra_le_0 | mask_for_if_ra_ge_0_1 | mask_for_if_ra_le_0_1 | mask_for_if_signed_rb_ge_ra | mask_for_if_signed_rb_ge_ra_1;
	assign group_9_for_branch_op = mask_for_if_rb_lt_ra | mask_for_if_rb_lt_ra_1;
	assign group_10_for_branch_op = mask_for_if_rb_ge_ra | mask_for_if_rb_ge_ra_1;
	assign group_11_for_branch_op = mask_for_if_ra_bit__eq_1 | mask_for_if_rb_bit__eq_0;
	assign group_1_for_ldst_op = mask_for_mem_raplustiny_ofstimes4_eq_rd | mask_for_mem8_ra_eq_rd | mask_for_mem16_ra_eq_rd | mask_for_mem32_ra_eq_rd | mask_for_memsr32_ra_eq_rd | mask_for_mem8_raplusfield_e_eq_rd | mask_for_mem16_raplusfield_e_eq_rd | mask_for_mem32_raplusfield_e_eq_rd | mask_for_memsr32_raplusfield_e_eq_rd | mask_for_mem8_field_e_eq_rd | mask_for_mem16_field_e_eq_rd | mask_for_mem32_field_e_eq_rd | mask_for_memsr32_field_e_eq_rd;
	assign group_2_for_ldst_op = mask_for_rd_eq_mem_raplustiny_ofstimes4 | mask_for_rd_eq_mem8_ra | mask_for_rd_eq_mem16_ra | mask_for_rd_eq_mem32_ra | mask_for_rd_eq_memll32_ra | mask_for_rd_eq_smem8_ra | mask_for_rd_eq_smem16_ra | mask_for_rd_eq_mem8_raplusfield_e | mask_for_rd_eq_mem16_raplusfield_e | mask_for_rd_eq_mem32_raplusfield_e | mask_for_rd_eq_memll32_raplusfield_e | mask_for_rd_eq_smem8_raplusfield_e | mask_for_rd_eq_smem16_raplusfield_e | mask_for_rd_eq_mem8_field_e | mask_for_rd_eq_mem16_field_e | mask_for_rd_eq_mem32_field_e | mask_for_rd_eq_memll32_field_e | mask_for_rd_eq_smem8_field_e | mask_for_rd_eq_smem16_field_e;
	assign group_1_for_rd1_addr = mask_for_woi | mask_for_rd_eq_ra_xor_rb | mask_for_rd_eq_ra_or_rb | mask_for_rd_eq_ra_and_rb | mask_for_rd_eq_ra_plus_rb | mask_for_rd_eq_ra_minus_rb | mask_for_rd_eq_notra_and_rb | mask_for_if_ra_eq_0 | mask_for_if_ra_ne_0 | mask_for_if_ra_lt_0 | mask_for_if_ra_ge_0 | mask_for_if_ra_eq_0_1 | mask_for_if_ra_ne_0_1 | mask_for_if_ra_lt_0_1 | mask_for_if_ra_ge_0_1 | mask_for_if_ra_bit__eq_1;
	assign group_2_for_rd1_addr = mask_for_pc_eq_rd | mask_for_tpc_eq_rd | mask_for_mem_raplustiny_ofstimes4_eq_rd | mask_for_mem8_ra_eq_rd | mask_for_mem16_ra_eq_rd | mask_for_mem32_ra_eq_rd | mask_for_memsr32_ra_eq_rd | mask_for_mem8_raplusfield_e_eq_rd | mask_for_mem16_raplusfield_e_eq_rd | mask_for_mem32_raplusfield_e_eq_rd | mask_for_memsr32_raplusfield_e_eq_rd | mask_for_mem8_field_e_eq_rd | mask_for_mem16_field_e_eq_rd | mask_for_mem32_field_e_eq_rd | mask_for_memsr32_field_e_eq_rd;
	assign group_3_for_rd1_addr = mask_for_rd_eq_tiny_rb_plus_field_a | mask_for_if_rb_eq_ra | mask_for_if_rb_ne_ra | mask_for_if_signed_rb_lt_ra | mask_for_if_signed_rb_ge_ra | mask_for_if_rb_lt_ra | mask_for_if_rb_ge_ra | mask_for_if_rb_eq_ra_1 | mask_for_if_rb_ne_ra_1 | mask_for_if_signed_rb_lt_ra_1 | mask_for_if_signed_rb_ge_ra_1 | mask_for_if_rb_lt_ra_1 | mask_for_if_rb_ge_ra_1 | mask_for_if_rb_bit__eq_0;
	assign group_1_for_res_addr = mask_for_rd_eq_pc | mask_for_rd_eq_tpc | mask_for_rd_eq_tiny_field_a | mask_for_rd_eq_pc_plus_field_atimes2 | mask_for_rd_eq_minus_ra | mask_for_rd_eq_notra | mask_for_rd_eq_bse_ra | mask_for_rd_eq_wse_ra | mask_for_rd_eq_ra_xor_rb | mask_for_rd_eq_ra_or_rb | mask_for_rd_eq_ra_and_rb | mask_for_rd_eq_ra_plus_rb | mask_for_rd_eq_ra_minus_rb | mask_for_rd_eq_notra_and_rb | mask_for_rd_eq_tiny_rb_plus_field_a | mask_for_rd_eq_value | mask_for_rd_eq_field_e_xor_rb | mask_for_rd_eq_field_e_or_rb | mask_for_rd_eq_field_e_and_rb | mask_for_rd_eq_field_e_plus_rb | mask_for_rd_eq_field_e_minus_rb | mask_for_rd_eq_short_value | mask_for_rd_eq_field_e_xor_ra | mask_for_rd_eq_field_e_or_ra | mask_for_rd_eq_field_e_and_ra | mask_for_rd_eq_field_e_plus_ra | mask_for_rd_eq_field_e_minus_ra | mask_for_rd_eq_mem_raplustiny_ofstimes4 | mask_for_rd_eq_mem8_ra | mask_for_rd_eq_mem16_ra | mask_for_rd_eq_mem32_ra | mask_for_rd_eq_memll32_ra | mask_for_rd_eq_smem8_ra | mask_for_rd_eq_smem16_ra | mask_for_rd_eq_mem8_raplusfield_e | mask_for_rd_eq_mem16_raplusfield_e | mask_for_rd_eq_mem32_raplusfield_e | mask_for_rd_eq_memll32_raplusfield_e | mask_for_rd_eq_smem8_raplusfield_e | mask_for_rd_eq_smem16_raplusfield_e | mask_for_rd_eq_mem8_field_e | mask_for_rd_eq_mem16_field_e | mask_for_rd_eq_mem32_field_e | mask_for_rd_eq_memll32_field_e | mask_for_rd_eq_smem8_field_e | mask_for_rd_eq_smem16_field_e;
	assign group_1_for_rd2_addr = mask_for_woi | mask_for_rd_eq_ra_xor_rb | mask_for_rd_eq_ra_or_rb | mask_for_rd_eq_ra_and_rb | mask_for_rd_eq_ra_plus_rb | mask_for_rd_eq_ra_minus_rb | mask_for_rd_eq_notra_and_rb | mask_for_rd_eq_field_e_xor_rb | mask_for_rd_eq_field_e_or_rb | mask_for_rd_eq_field_e_and_rb | mask_for_rd_eq_field_e_plus_rb | mask_for_rd_eq_field_e_minus_rb;
	assign group_2_for_rd2_addr = mask_for_rd_eq_minus_ra | mask_for_rd_eq_notra | mask_for_rd_eq_bse_ra | mask_for_rd_eq_wse_ra | mask_for_rd_eq_field_e_xor_ra | mask_for_rd_eq_field_e_or_ra | mask_for_rd_eq_field_e_and_ra | mask_for_rd_eq_field_e_plus_ra | mask_for_rd_eq_field_e_minus_ra | mask_for_if_ra_gt_0 | mask_for_if_ra_le_0 | mask_for_if_ra_gt_0_1 | mask_for_if_ra_le_0_1 | mask_for_if_rb_eq_ra | mask_for_if_rb_ne_ra | mask_for_if_signed_rb_lt_ra | mask_for_if_signed_rb_ge_ra | mask_for_if_rb_lt_ra | mask_for_if_rb_ge_ra | mask_for_if_rb_eq_ra_1 | mask_for_if_rb_ne_ra_1 | mask_for_if_signed_rb_lt_ra_1 | mask_for_if_signed_rb_ge_ra_1 | mask_for_if_rb_lt_ra_1 | mask_for_if_rb_ge_ra_1 | mask_for_rd_eq_mem8_ra | mask_for_rd_eq_mem16_ra | mask_for_rd_eq_mem32_ra | mask_for_rd_eq_memll32_ra | mask_for_mem8_ra_eq_rd | mask_for_mem16_ra_eq_rd | mask_for_mem32_ra_eq_rd | mask_for_memsr32_ra_eq_rd | mask_for_rd_eq_smem8_ra | mask_for_rd_eq_smem16_ra | mask_for_rd_eq_mem8_raplusfield_e | mask_for_rd_eq_mem16_raplusfield_e | mask_for_rd_eq_mem32_raplusfield_e | mask_for_rd_eq_memll32_raplusfield_e | mask_for_mem8_raplusfield_e_eq_rd | mask_for_mem16_raplusfield_e_eq_rd | mask_for_mem32_raplusfield_e_eq_rd | mask_for_memsr32_raplusfield_e_eq_rd | mask_for_rd_eq_smem8_raplusfield_e | mask_for_rd_eq_smem16_raplusfield_e;
	assign group_3_for_rd2_addr = mask_for_mem_raplustiny_ofstimes4_eq_rd | mask_for_rd_eq_mem_raplustiny_ofstimes4;
	assign group_1_for_use_reg_a = mask_for_woi | mask_for_pc_eq_rd | mask_for_tpc_eq_rd | mask_for_rd_eq_ra_xor_rb | mask_for_rd_eq_ra_or_rb | mask_for_rd_eq_ra_and_rb | mask_for_rd_eq_ra_plus_rb | mask_for_rd_eq_ra_minus_rb | mask_for_rd_eq_notra_and_rb | mask_for_rd_eq_tiny_rb_plus_field_a | mask_for_if_ra_eq_0 | mask_for_if_ra_ne_0 | mask_for_if_ra_lt_0 | mask_for_if_ra_ge_0 | mask_for_if_ra_eq_0_1 | mask_for_if_ra_ne_0_1 | mask_for_if_ra_lt_0_1 | mask_for_if_ra_ge_0_1 | mask_for_if_rb_eq_ra | mask_for_if_rb_ne_ra | mask_for_if_signed_rb_lt_ra | mask_for_if_signed_rb_ge_ra | mask_for_if_rb_lt_ra | mask_for_if_rb_ge_ra | mask_for_if_rb_eq_ra_1 | mask_for_if_rb_ne_ra_1 | mask_for_if_signed_rb_lt_ra_1 | mask_for_if_signed_rb_ge_ra_1 | mask_for_if_rb_lt_ra_1 | mask_for_if_rb_ge_ra_1 | mask_for_if_ra_bit__eq_1 | mask_for_if_rb_bit__eq_0 | mask_for_mem_raplustiny_ofstimes4_eq_rd | mask_for_mem8_ra_eq_rd | mask_for_mem16_ra_eq_rd | mask_for_mem32_ra_eq_rd | mask_for_memsr32_ra_eq_rd | mask_for_mem8_raplusfield_e_eq_rd | mask_for_mem16_raplusfield_e_eq_rd | mask_for_mem32_raplusfield_e_eq_rd | mask_for_memsr32_raplusfield_e_eq_rd | mask_for_mem8_field_e_eq_rd | mask_for_mem16_field_e_eq_rd | mask_for_mem32_field_e_eq_rd | mask_for_memsr32_field_e_eq_rd;
	assign group_1_for_use_reg_b = mask_for_woi | mask_for_rd_eq_minus_ra | mask_for_rd_eq_notra | mask_for_rd_eq_bse_ra | mask_for_rd_eq_wse_ra | mask_for_rd_eq_ra_xor_rb | mask_for_rd_eq_ra_or_rb | mask_for_rd_eq_ra_and_rb | mask_for_rd_eq_ra_plus_rb | mask_for_rd_eq_ra_minus_rb | mask_for_rd_eq_notra_and_rb | mask_for_rd_eq_field_e_xor_rb | mask_for_rd_eq_field_e_or_rb | mask_for_rd_eq_field_e_and_rb | mask_for_rd_eq_field_e_plus_rb | mask_for_rd_eq_field_e_minus_rb | mask_for_rd_eq_field_e_xor_ra | mask_for_rd_eq_field_e_or_ra | mask_for_rd_eq_field_e_and_ra | mask_for_rd_eq_field_e_plus_ra | mask_for_rd_eq_field_e_minus_ra | mask_for_if_ra_gt_0 | mask_for_if_ra_le_0 | mask_for_if_ra_gt_0_1 | mask_for_if_ra_le_0_1 | mask_for_if_rb_eq_ra | mask_for_if_rb_ne_ra | mask_for_if_signed_rb_lt_ra | mask_for_if_signed_rb_ge_ra | mask_for_if_rb_lt_ra | mask_for_if_rb_ge_ra | mask_for_if_rb_eq_ra_1 | mask_for_if_rb_ne_ra_1 | mask_for_if_signed_rb_lt_ra_1 | mask_for_if_signed_rb_ge_ra_1 | mask_for_if_rb_lt_ra_1 | mask_for_if_rb_ge_ra_1 | mask_for_mem_raplustiny_ofstimes4_eq_rd | mask_for_rd_eq_mem_raplustiny_ofstimes4 | mask_for_rd_eq_mem8_ra | mask_for_rd_eq_mem16_ra | mask_for_rd_eq_mem32_ra | mask_for_rd_eq_memll32_ra | mask_for_mem8_ra_eq_rd | mask_for_mem16_ra_eq_rd | mask_for_mem32_ra_eq_rd | mask_for_memsr32_ra_eq_rd | mask_for_rd_eq_smem8_ra | mask_for_rd_eq_smem16_ra | mask_for_rd_eq_mem8_raplusfield_e | mask_for_rd_eq_mem16_raplusfield_e | mask_for_rd_eq_mem32_raplusfield_e | mask_for_rd_eq_memll32_raplusfield_e | mask_for_mem8_raplusfield_e_eq_rd | mask_for_mem16_raplusfield_e_eq_rd | mask_for_mem32_raplusfield_e_eq_rd | mask_for_memsr32_raplusfield_e_eq_rd | mask_for_rd_eq_smem8_raplusfield_e | mask_for_rd_eq_smem16_raplusfield_e;
	assign group_1_for_op_a = mask_for_rd_eq_ra_lsl_rb | mask_for_rd_eq_ra_lsr_rb | mask_for_rd_eq_ra_asr_rb | mask_for_rd_eq_field_e_lsl_rb | mask_for_rd_eq_field_e_lsr_rb | mask_for_rd_eq_field_e_asr_rb | mask_for_rd_eq_field_e_lsl_ra | mask_for_rd_eq_field_e_lsr_ra | mask_for_rd_eq_field_e_asr_ra | mask_for_rd_eq_ra_times_rb | mask_for_rd_eq_field_e_times_rb | mask_for_rd_eq_field_e_times_ra | mask_for_sii | mask_for_sii_1 | mask_for_sii_2 | mask_for_sii_3 | mask_for_sii_4 | mask_for_sii_5 | mask_for_sii_6 | mask_for_sii_7 | mask_for_sii_8 | mask_for_sii_9 | mask_for_sii_10 | mask_for_sii_11 | mask_for_sii_12 | mask_for_sii_13 | mask_for_sii_14 | mask_for_sii_15 | mask_for_sii_16 | mask_for_sii_17 | mask_for_sii_18 | mask_for_sii_19 | mask_for_sii_20 | mask_for_sii_21 | mask_for_sii_22 | mask_for_sii_23 | mask_for_sii_24 | mask_for_sii_25 | mask_for_sii_26 | mask_for_sii_27 | mask_for_sii_28 | mask_expr;
	assign group_2_for_op_a = mask_for_swi;
	assign group_3_for_op_a = mask_for_rd_eq_tiny_field_a | mask_for_rd_eq_minus_ra | mask_for_rd_eq_bse_ra | mask_for_rd_eq_wse_ra | mask_for_if_ra_gt_0 | mask_for_if_ra_le_0 | mask_for_if_ra_gt_0_1 | mask_for_if_ra_le_0_1;
	assign group_4_for_op_a = mask_for_rd_eq_notra;
	assign group_5_for_op_a = mask_for_rd_eq_value | mask_for_pc_eq_value | mask_for_tpc_eq_value | mask_for_rd_eq_field_e_xor_rb | mask_for_rd_eq_field_e_or_rb | mask_for_rd_eq_field_e_and_rb | mask_for_rd_eq_field_e_plus_rb | mask_for_rd_eq_field_e_minus_rb | mask_for_rd_eq_short_value | mask_for_pc_eq_short_value | mask_for_tpc_eq_short_value | mask_for_rd_eq_field_e_xor_ra | mask_for_rd_eq_field_e_or_ra | mask_for_rd_eq_field_e_and_ra | mask_for_rd_eq_field_e_plus_ra | mask_for_rd_eq_field_e_minus_ra;
	assign field_e = fetch_inst_len == 2'h2 ? ({fetch_inst_2, fetch_inst_1}) : ({fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1[15], fetch_inst_1});
	assign group_1_for_op_b = mask_for_rd_eq_pc | mask_for_rd_eq_value | mask_for_rd_eq_short_value | mask_for_if_ra_eq_0 | mask_for_if_ra_ne_0 | mask_for_if_ra_lt_0 | mask_for_if_ra_ge_0 | mask_for_if_ra_eq_0_1 | mask_for_if_ra_ne_0_1 | mask_for_if_ra_lt_0_1 | mask_for_if_ra_ge_0_1 | mask_for_rd_eq_mem8_field_e | mask_for_rd_eq_mem16_field_e | mask_for_rd_eq_mem32_field_e | mask_for_rd_eq_memll32_field_e | mask_for_mem8_field_e_eq_rd | mask_for_mem16_field_e_eq_rd | mask_for_mem32_field_e_eq_rd | mask_for_memsr32_field_e_eq_rd | mask_for_rd_eq_smem8_field_e | mask_for_rd_eq_smem16_field_e;
	assign group_2_for_op_b = mask_for_rd_eq_tiny_field_a | mask_for_rd_eq_pc_plus_field_atimes2 | mask_for_rd_eq_tiny_rb_plus_field_a;
	assign ones_field_a = u22_output_port[3] ? ({field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one}) : fetch_inst_0[3:0];
	assign group_3_for_op_b = mask_for_if_ra_bit__eq_1 | mask_for_if_rb_bit__eq_0;
	assign group_1_for_op_c = mask_for_woi | mask_for_rd_eq_mem8_ra | mask_for_rd_eq_mem16_ra | mask_for_rd_eq_mem32_ra | mask_for_rd_eq_memll32_ra | mask_for_mem8_ra_eq_rd | mask_for_mem16_ra_eq_rd | mask_for_mem32_ra_eq_rd | mask_for_memsr32_ra_eq_rd | mask_for_rd_eq_smem8_ra | mask_for_rd_eq_smem16_ra;
	assign group_2_for_op_c = mask_for_if_ra_eq_0 | mask_for_if_ra_ne_0 | mask_for_if_ra_lt_0 | mask_for_if_ra_ge_0 | mask_for_if_ra_gt_0 | mask_for_if_ra_le_0 | mask_for_if_ra_eq_0_1 | mask_for_if_ra_ne_0_1 | mask_for_if_ra_lt_0_1 | mask_for_if_ra_ge_0_1 | mask_for_if_ra_gt_0_1 | mask_for_if_ra_le_0_1 | mask_for_if_rb_eq_ra | mask_for_if_rb_ne_ra | mask_for_if_signed_rb_lt_ra | mask_for_if_signed_rb_ge_ra | mask_for_if_rb_lt_ra | mask_for_if_rb_ge_ra | mask_for_if_rb_eq_ra_1 | mask_for_if_rb_ne_ra_1 | mask_for_if_signed_rb_lt_ra_1 | mask_for_if_signed_rb_ge_ra_1 | mask_for_if_rb_lt_ra_1 | mask_for_if_rb_ge_ra_1 | mask_for_if_ra_bit__eq_1 | mask_for_if_rb_bit__eq_0 | mask_for_rd_eq_mem8_raplusfield_e | mask_for_rd_eq_mem16_raplusfield_e | mask_for_rd_eq_mem32_raplusfield_e | mask_for_rd_eq_memll32_raplusfield_e | mask_for_mem8_raplusfield_e_eq_rd | mask_for_mem16_raplusfield_e_eq_rd | mask_for_mem32_raplusfield_e_eq_rd | mask_for_memsr32_raplusfield_e_eq_rd | mask_for_rd_eq_smem8_raplusfield_e | mask_for_rd_eq_smem16_raplusfield_e | mask_for_rd_eq_mem8_field_e | mask_for_rd_eq_mem16_field_e | mask_for_rd_eq_mem32_field_e | mask_for_rd_eq_memll32_field_e | mask_for_mem8_field_e_eq_rd | mask_for_mem16_field_e_eq_rd | mask_for_mem32_field_e_eq_rd | mask_for_memsr32_field_e_eq_rd | mask_for_rd_eq_smem8_field_e | mask_for_rd_eq_smem16_field_e;
	assign group_3_for_op_c = mask_for_mem_raplustiny_ofstimes4_eq_rd | mask_for_rd_eq_mem_raplustiny_ofstimes4;
	assign tiny_ofs = {fetch_inst_0[7:1], 2'h0};
	assign group_1_for_mem_len = mask_for_mem_raplustiny_ofstimes4_eq_rd | mask_for_rd_eq_mem_raplustiny_ofstimes4 | mask_for_rd_eq_mem32_ra | mask_for_rd_eq_memll32_ra | mask_for_mem32_ra_eq_rd | mask_for_memsr32_ra_eq_rd | mask_for_rd_eq_mem32_raplusfield_e | mask_for_rd_eq_memll32_raplusfield_e | mask_for_mem32_raplusfield_e_eq_rd | mask_for_memsr32_raplusfield_e_eq_rd | mask_for_rd_eq_mem32_field_e | mask_for_rd_eq_memll32_field_e | mask_for_mem32_field_e_eq_rd | mask_for_memsr32_field_e_eq_rd;
	assign group_2_for_mem_len = mask_for_rd_eq_mem8_ra | mask_for_mem8_ra_eq_rd | mask_for_rd_eq_smem8_ra | mask_for_rd_eq_mem8_raplusfield_e | mask_for_mem8_raplusfield_e_eq_rd | mask_for_rd_eq_smem8_raplusfield_e | mask_for_rd_eq_mem8_field_e | mask_for_mem8_field_e_eq_rd | mask_for_rd_eq_smem8_field_e;
	assign group_3_for_mem_len = mask_for_rd_eq_mem16_ra | mask_for_mem16_ra_eq_rd | mask_for_rd_eq_smem16_ra | mask_for_rd_eq_mem16_raplusfield_e | mask_for_mem16_raplusfield_e_eq_rd | mask_for_rd_eq_smem16_raplusfield_e | mask_for_rd_eq_mem16_field_e | mask_for_mem16_field_e_eq_rd | mask_for_rd_eq_smem16_field_e;
	assign group_1_for_bse = mask_for_rd_eq_bse_ra | mask_for_rd_eq_smem8_ra | mask_for_rd_eq_smem8_raplusfield_e | mask_for_rd_eq_smem8_field_e;
	assign group_1_for_wse = mask_for_rd_eq_wse_ra | mask_for_rd_eq_smem16_ra | mask_for_rd_eq_smem16_raplusfield_e | mask_for_rd_eq_smem16_field_e;
	assign group_1_for_bze = mask_for_rd_eq_mem8_ra | mask_for_rd_eq_mem8_raplusfield_e | mask_for_rd_eq_mem8_field_e;
	assign group_1_for_wze = mask_for_rd_eq_mem16_ra | mask_for_rd_eq_mem16_raplusfield_e | mask_for_rd_eq_mem16_field_e;
	assign group_1_for_read1_needed = mask_for_woi | mask_for_pc_eq_rd | mask_for_tpc_eq_rd | mask_for_rd_eq_ra_xor_rb | mask_for_rd_eq_ra_or_rb | mask_for_rd_eq_ra_and_rb | mask_for_rd_eq_ra_plus_rb | mask_for_rd_eq_ra_minus_rb | mask_for_rd_eq_notra_and_rb | mask_for_rd_eq_tiny_rb_plus_field_a | mask_for_if_ra_eq_0 | mask_for_if_ra_ne_0 | mask_for_if_ra_lt_0 | mask_for_if_ra_ge_0 | mask_for_if_ra_eq_0_1 | mask_for_if_ra_ne_0_1 | mask_for_if_ra_lt_0_1 | mask_for_if_ra_ge_0_1 | mask_for_if_rb_eq_ra | mask_for_if_rb_ne_ra | mask_for_if_signed_rb_lt_ra | mask_for_if_signed_rb_ge_ra | mask_for_if_rb_lt_ra | mask_for_if_rb_ge_ra | mask_for_if_rb_eq_ra_1 | mask_for_if_rb_ne_ra_1 | mask_for_if_signed_rb_lt_ra_1 | mask_for_if_signed_rb_ge_ra_1 | mask_for_if_rb_lt_ra_1 | mask_for_if_rb_ge_ra_1 | mask_for_if_ra_bit__eq_1 | mask_for_if_rb_bit__eq_0 | mask_for_mem_raplustiny_ofstimes4_eq_rd | mask_for_mem8_ra_eq_rd | mask_for_mem16_ra_eq_rd | mask_for_mem32_ra_eq_rd | mask_for_memsr32_ra_eq_rd | mask_for_mem8_raplusfield_e_eq_rd | mask_for_mem16_raplusfield_e_eq_rd | mask_for_mem32_raplusfield_e_eq_rd | mask_for_memsr32_raplusfield_e_eq_rd | mask_for_mem8_field_e_eq_rd | mask_for_mem16_field_e_eq_rd | mask_for_mem32_field_e_eq_rd | mask_for_memsr32_field_e_eq_rd;
	assign group_1_for_read2_needed = mask_for_woi | mask_for_rd_eq_minus_ra | mask_for_rd_eq_notra | mask_for_rd_eq_bse_ra | mask_for_rd_eq_wse_ra | mask_for_rd_eq_ra_xor_rb | mask_for_rd_eq_ra_or_rb | mask_for_rd_eq_ra_and_rb | mask_for_rd_eq_ra_plus_rb | mask_for_rd_eq_ra_minus_rb | mask_for_rd_eq_notra_and_rb | mask_for_rd_eq_field_e_xor_rb | mask_for_rd_eq_field_e_or_rb | mask_for_rd_eq_field_e_and_rb | mask_for_rd_eq_field_e_plus_rb | mask_for_rd_eq_field_e_minus_rb | mask_for_rd_eq_field_e_xor_ra | mask_for_rd_eq_field_e_or_ra | mask_for_rd_eq_field_e_and_ra | mask_for_rd_eq_field_e_plus_ra | mask_for_rd_eq_field_e_minus_ra | mask_for_if_ra_gt_0 | mask_for_if_ra_le_0 | mask_for_if_ra_gt_0_1 | mask_for_if_ra_le_0_1 | mask_for_if_rb_eq_ra | mask_for_if_rb_ne_ra | mask_for_if_signed_rb_lt_ra | mask_for_if_signed_rb_ge_ra | mask_for_if_rb_lt_ra | mask_for_if_rb_ge_ra | mask_for_if_rb_eq_ra_1 | mask_for_if_rb_ne_ra_1 | mask_for_if_signed_rb_lt_ra_1 | mask_for_if_signed_rb_ge_ra_1 | mask_for_if_rb_lt_ra_1 | mask_for_if_rb_ge_ra_1 | mask_for_mem_raplustiny_ofstimes4_eq_rd | mask_for_rd_eq_mem_raplustiny_ofstimes4 | mask_for_rd_eq_mem8_ra | mask_for_rd_eq_mem16_ra | mask_for_rd_eq_mem32_ra | mask_for_rd_eq_memll32_ra | mask_for_mem8_ra_eq_rd | mask_for_mem16_ra_eq_rd | mask_for_mem32_ra_eq_rd | mask_for_memsr32_ra_eq_rd | mask_for_rd_eq_smem8_ra | mask_for_rd_eq_smem16_ra | mask_for_rd_eq_mem8_raplusfield_e | mask_for_rd_eq_mem16_raplusfield_e | mask_for_rd_eq_mem32_raplusfield_e | mask_for_rd_eq_memll32_raplusfield_e | mask_for_mem8_raplusfield_e_eq_rd | mask_for_mem16_raplusfield_e_eq_rd | mask_for_mem32_raplusfield_e_eq_rd | mask_for_memsr32_raplusfield_e_eq_rd | mask_for_rd_eq_smem8_raplusfield_e | mask_for_rd_eq_smem16_raplusfield_e;
	assign group_1_for_rsv_needed = mask_for_rd_eq_pc | mask_for_rd_eq_tpc | mask_for_rd_eq_tiny_field_a | mask_for_rd_eq_pc_plus_field_atimes2 | mask_for_rd_eq_minus_ra | mask_for_rd_eq_notra | mask_for_rd_eq_bse_ra | mask_for_rd_eq_wse_ra | mask_for_rd_eq_ra_xor_rb | mask_for_rd_eq_ra_or_rb | mask_for_rd_eq_ra_and_rb | mask_for_rd_eq_ra_plus_rb | mask_for_rd_eq_ra_minus_rb | mask_for_rd_eq_notra_and_rb | mask_for_rd_eq_tiny_rb_plus_field_a | mask_for_rd_eq_value | mask_for_rd_eq_field_e_xor_rb | mask_for_rd_eq_field_e_or_rb | mask_for_rd_eq_field_e_and_rb | mask_for_rd_eq_field_e_plus_rb | mask_for_rd_eq_field_e_minus_rb | mask_for_rd_eq_short_value | mask_for_rd_eq_field_e_xor_ra | mask_for_rd_eq_field_e_or_ra | mask_for_rd_eq_field_e_and_ra | mask_for_rd_eq_field_e_plus_ra | mask_for_rd_eq_field_e_minus_ra | mask_for_rd_eq_mem_raplustiny_ofstimes4 | mask_for_rd_eq_mem8_ra | mask_for_rd_eq_mem16_ra | mask_for_rd_eq_mem32_ra | mask_for_rd_eq_memll32_ra | mask_for_rd_eq_smem8_ra | mask_for_rd_eq_smem16_ra | mask_for_rd_eq_mem8_raplusfield_e | mask_for_rd_eq_mem16_raplusfield_e | mask_for_rd_eq_mem32_raplusfield_e | mask_for_rd_eq_memll32_raplusfield_e | mask_for_rd_eq_smem8_raplusfield_e | mask_for_rd_eq_smem16_raplusfield_e | mask_for_rd_eq_mem8_field_e | mask_for_rd_eq_mem16_field_e | mask_for_rd_eq_mem32_field_e | mask_for_rd_eq_memll32_field_e | mask_for_rd_eq_smem8_field_e | mask_for_rd_eq_smem16_field_e;
	assign reg_file_req_valid = fetch_valid &  ~ do_branch;
	assign rd1_addr = (group_1_for_rd1_addr ? fetch_inst_0[3:0] : 4'b0) | (group_2_for_rd1_addr ? fetch_inst_0[15:12] : 4'b0) | (group_3_for_rd1_addr ? fetch_inst_0[7:4] : 4'b0) ;
	assign reg_file_req_read1_addr = rd1_addr;
	assign read1_needed = fetch_av ? 1'h0 : (group_1_for_read1_needed ? 1'h1 : 1'b0) | 1'h0;
	assign rd2_addr = (group_1_for_rd2_addr ? fetch_inst_0[7:4] : 4'b0) | (group_2_for_rd2_addr ? fetch_inst_0[3:0] : 4'b0) | (group_3_for_rd2_addr ? fetch_inst_0[0] : 4'b0) ;
	assign reg_file_req_read2_addr = rd2_addr;
	assign read2_needed = fetch_av ? 1'h0 : (group_1_for_read2_needed ? 1'h1 : 1'b0) | 1'h0;
	assign res_addr = (group_1_for_res_addr ? fetch_inst_0[15:12] : 4'b0) ;
	assign reg_file_req_rsv_addr = res_addr;
	assign rsv_needed = fetch_av ? 1'h0 : (group_1_for_rsv_needed ? 1'h1 : 1'b0) | 1'h0;
	assign exec_unit = (group_1_for_exec_unit ? `op_class__branch : 3'b0) | (group_2_for_exec_unit ? `op_class__alu : 3'b0) | (group_3_for_exec_unit ? `op_class__ld_st : 3'b0) ;
	assign register_outputs = reg_file_req_ready & reg_file_req_valid;
	always_ff @(posedge clk) output_port_exec_unit <= rst ? 3'h0 : register_outputs ? exec_unit : output_port_exec_unit;
	assign alu_op = 
		(group_1_for_alu_op ? `alu_ops__a_minus_b : 3'b0) | 
		(group_2_for_alu_op ? `alu_ops__pc_plus_b : 3'b0) | 
		(group_3_for_alu_op ? `alu_ops__tpc : 3'b0) | 
		(group_4_for_alu_op ? `alu_ops__a_or_b : 3'b0) | 
		(group_5_for_alu_op ? `alu_ops__a_xor_b : 3'b0) | 
		(group_6_for_alu_op ? `alu_ops__a_and_b : 3'b0) | 
		(group_7_for_alu_op ? `alu_ops__a_plus_b : 3'b0) | 
		(group_8_for_alu_op ? `alu_ops__n_b_and_a : 3'b0) ;
	always_ff @(posedge clk) output_port_alu_op <= rst ? 3'h0 : register_outputs ? alu_op : output_port_alu_op;
	assign branch_op = 
		(group_1_for_branch_op ? `branch_ops__swi : 4'b0) | 
		(group_2_for_branch_op ? `branch_ops__stm : 4'b0) | 
		(group_3_for_branch_op ? `branch_ops__cb_eq : 4'b0) | 
		(group_4_for_branch_op ? `branch_ops__pc_w : 4'b0) | 
		(group_5_for_branch_op ? `branch_ops__tpc_w : 4'b0) | 
		(group_6_for_branch_op ? `branch_ops__cb_ne : 4'b0) | 
		(group_7_for_branch_op ? `branch_ops__cb_lts : 4'b0) | 
		(group_8_for_branch_op ? `branch_ops__cb_ges : 4'b0) | 
		(group_9_for_branch_op ? `branch_ops__cb_lt : 4'b0) | 
		(group_10_for_branch_op ? `branch_ops__cb_ge : 4'b0) | 
		(group_11_for_branch_op ? `branch_ops__bb_one : 4'b0) ;
	always_ff @(posedge clk) output_port_branch_op <= rst ? 4'h0 : register_outputs ? branch_op : output_port_branch_op;
	assign ldst_op = (group_1_for_ldst_op ? `ldst_ops__store : 1'b0) | (group_2_for_ldst_op ? `ldst_ops__load : 1'b0) ;
	always_ff @(posedge clk) output_port_ldst_op <= rst ? 1'h0 : register_outputs ? ldst_op : output_port_ldst_op;
	assign use_reg_a = (group_1_for_use_reg_a ? 1'h1 : 1'b0) | 1'h0;
	assign op_a = 
		(group_1_for_op_a ? 3'h7 : 32'b0) | 
		(group_2_for_op_a ? fetch_inst_0[3:0] : 32'b0) | 
		(group_3_for_op_a ? 1'h0 : 32'b0) | 
		(group_4_for_op_a ? 32'hffffffff : 32'b0) | 
		(group_5_for_op_a ? field_e : 32'b0) ;
	always_ff @(posedge clk) u1766_output_port <= rst ? 1'h0 : register_outputs ? use_reg_a : u1766_output_port;
	always_ff @(posedge clk) u1767_output_port <= rst ? 32'h0 : register_outputs ? op_a : u1767_output_port;
	assign output_port_op_a = u1766_output_port ? reg_file_rsp_read1_data : u1767_output_port;
	assign use_reg_b = (group_1_for_use_reg_b ? 1'h1 : 1'b0) | 1'h0;
	assign op_b = (group_1_for_op_b ? 1'h0 : 32'b0) | (group_2_for_op_b ? ones_field_a : 32'b0) | (group_3_for_op_b ? fetch_inst_0[11:8] : 32'b0) ;
	always_ff @(posedge clk) u1769_output_port <= rst ? 1'h0 : register_outputs ? use_reg_b : u1769_output_port;
	always_ff @(posedge clk) u1770_output_port <= rst ? 32'h0 : register_outputs ? op_b : u1770_output_port;
	assign output_port_op_b = u1769_output_port ? reg_file_rsp_read2_data : u1770_output_port;
	assign op_c = (group_1_for_op_c ? 1'h0 : 32'b0) | (group_2_for_op_c ? field_e : 32'b0) | (group_3_for_op_c ? tiny_ofs : 32'b0) ;
	always_ff @(posedge clk) output_port_op_c <= rst ? 32'h0 : register_outputs ? op_c : output_port_op_c;
	assign mem_len = (group_1_for_mem_len ? 2'h3 : 2'b0) | (group_2_for_mem_len ? 1'h1 : 2'b0) | (group_3_for_mem_len ? 2'h2 : 2'b0) ;
	always_ff @(posedge clk) u1773_output_port <= rst ? 2'h0 : register_outputs ? mem_len : u1773_output_port;
	always_ff @(posedge clk) output_port_inst_len <= rst ? 2'h0 : register_outputs ? fetch_inst_len : output_port_inst_len;
	assign bse = (group_1_for_bse ? 1'h1 : 1'b0) | 1'h0;
	always_ff @(posedge clk) output_port_do_bse <= rst ? 1'h0 : register_outputs ? bse : output_port_do_bse;
	assign wse = (group_1_for_wse ? 1'h1 : 1'b0) | 1'h0;
	always_ff @(posedge clk) output_port_do_wse <= rst ? 1'h0 : register_outputs ? wse : output_port_do_wse;
	assign bze = (group_1_for_bze ? 1'h1 : 1'b0) | 1'h0;
	always_ff @(posedge clk) output_port_do_bze <= rst ? 1'h0 : register_outputs ? bze : output_port_do_bze;
	assign wze = (group_1_for_wze ? 1'h1 : 1'b0) | 1'h0;
	always_ff @(posedge clk) output_port_do_wze <= rst ? 1'h0 : register_outputs ? wze : output_port_do_wze;
	always_ff @(posedge clk) output_port_result_reg_addr <= rst ? 4'h0 : register_outputs ? res_addr : output_port_result_reg_addr;
	always_ff @(posedge clk) output_port_result_reg_addr_valid <= rst ? 1'h0 : register_outputs ? rsv_needed : output_port_result_reg_addr_valid;
	always_ff @(posedge clk) output_port_fetch_av <= rst ? 1'h0 : register_outputs ? fetch_av : output_port_fetch_av;
	assign output_port_shifter_op = 2'hx;
	assign output_port_valid = reg_file_rsp_valid;
	assign reg_file_rsp_ready = output_port_ready;
	assign fetch_ready = reg_file_req_ready;
	assign output_port_mem_access_len = u1773_output_port;

	assign u36_output_port = fetch_inst_0[3:0] + 1'h1;
	assign mask_for_sii_29 = mask_expr;
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
	logic [4:0] inst_buf_queue_free_cnt;
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
		.bus_if_response_valid(bus_if_response_valid),

		.queue_av(inst_buf_queue_av),
		.queue_data(inst_buf_queue_data),
		.queue_ready(inst_buf_queue_ready),
		.queue_valid(inst_buf_queue_valid),

		.queue_free_cnt(inst_buf_queue_free_cnt),
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

		.queue_free_cnt(inst_buf_queue_free_cnt),
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

	assign output_port = 
		(bit_code == 1'h0 ? word[0] : 1'b0) | 
		(bit_code == 1'h1 ? word[1] : 1'b0) | 
		(bit_code == 2'h2 ? word[2] : 1'b0) | 
		(bit_code == 2'h3 ? word[3] : 1'b0) | 
		(bit_code == 3'h4 ? word[4] : 1'b0) | 
		(bit_code == 3'h5 ? word[5] : 1'b0) | 
		(bit_code == 3'h6 ? word[6] : 1'b0) | 
		(bit_code == 3'h7 ? word[7] : 1'b0) | 
		(bit_code == 4'h8 ? word[8] : 1'b0) | 
		(bit_code == 4'h9 ? word[9] : 1'b0) | 
		(bit_code == 4'ha ? word[14] : 1'b0) | 
		(bit_code == 4'hb ? word[15] : 1'b0) | 
		(bit_code == 4'hc ? word[16] : 1'b0) | 
		(bit_code == 4'hd ? word[30] : 1'b0) | 
		(bit_code == 4'he ? word[31] : 1'b0) ;

endmodule


////////////////////////////////////////////////////////////////////////////////
// DecoratorModule_6
////////////////////////////////////////////////////////////////////////////////
module DecoratorModule_6 (
	output logic output_port,
	input logic [31:0] word,
	input logic [31:0] bit_code
);

	assign output_port = 
		(bit_code == 1'h0 ? word[0] : 1'b0) | 
		(bit_code == 1'h1 ? word[1] : 1'b0) | 
		(bit_code == 2'h2 ? word[2] : 1'b0) | 
		(bit_code == 2'h3 ? word[3] : 1'b0) | 
		(bit_code == 3'h4 ? word[4] : 1'b0) | 
		(bit_code == 3'h5 ? word[5] : 1'b0) | 
		(bit_code == 3'h6 ? word[6] : 1'b0) | 
		(bit_code == 3'h7 ? word[7] : 1'b0) | 
		(bit_code == 4'h8 ? word[8] : 1'b0) | 
		(bit_code == 4'h9 ? word[9] : 1'b0) | 
		(bit_code == 4'ha ? word[14] : 1'b0) | 
		(bit_code == 4'hb ? word[15] : 1'b0) | 
		(bit_code == 4'hc ? word[16] : 1'b0) | 
		(bit_code == 4'hd ? word[30] : 1'b0) | 
		(bit_code == 4'he ? word[31] : 1'b0) ;

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
	assign next_looped = 
		(push != 1'h1 & pop != 1'h1 ? looped : 1'b0) | 
		(push == 1'h1 & pop != 1'h1 ? push_will_wrap ? 1'h1 : looped : 1'b0) | 
		(push != 1'h1 & pop == 1'h1 ? pop_will_wrap ? 1'h0 : looped : 1'b0) | 
		(push == 1'h1 & pop == 1'h1 ? 
		(push_will_wrap != 1'h1 & pop_will_wrap != 1'h1 ? looped : 1'b0) | 
		(push_will_wrap == 1'h1 & pop_will_wrap != 1'h1 ? 1'h1 : 1'b0) | 
		(push_will_wrap != 1'h1 & pop_will_wrap == 1'h1 ? 1'h0 : 1'b0) | 
		(push_will_wrap == 1'h1 & pop_will_wrap == 1'h1 ? looped : 1'b0)  : 1'b0) ;
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
	logic [32:0] u3_output_port;
	logic [30:0] branch_target;
	logic task_mode_fetch;
	logic [31:0] u8_output_port;
	logic [30:0] fetch_addr;
	logic fetch_page_limit;
	logic fetch_av;
	logic start_new_request;
	logic signed [5:0] u30_output_port;
	logic [4:0] req_len;
	logic req_av;
	logic flushing;
	logic u65_output_port;
	logic u68_output_port;
	logic u70_output_port;
	logic [1:0] state;
	logic [1:0] fsm_next_state;
	logic branch_req;

	assign branch_target = task_mode ? u3_output_port[30:0] : spc;
	always_ff @(posedge clk) task_mode_fetch <= rst ? 1'h0 : do_branch ? task_mode : task_mode_fetch;
	assign advance_request = bus_if_request_valid & bus_if_request_ready;
	always_ff @(posedge clk) fetch_addr <= rst ? 31'h0 : do_branch ? branch_target : u8_output_port[30:0];
	always_ff @(posedge clk) fetch_page_limit <= rst ? 1'h0 :  ~ fetch_addr[7:0] == 1'h0;
	assign start_new_request = (queue_free_cnt >= 4'h8 | do_branch) & state == `InstBufferStates__idle;
	always_ff @(posedge clk) req_len <= rst ? 4'h8 : start_new_request ? do_branch ? 4'h8 : queue_free_cnt : advance_request ? req_len > 1'h0 ? u30_output_port[4:0] : 1'h0 : req_len;
	assign fetch_av = task_mode_fetch & fetch_addr[30:10] > mem_limit;
	always_ff @(posedge clk) req_av <= rst ? 1'h0 : start_new_request ? fetch_av : req_av;
	always_ff @(posedge clk) flushing <= rst ? 1'h0 : (do_branch | state == `InstBufferStates__flush_start) ? 1'h1 : bus_if_response_valid ? flushing : 1'h0;
	assign bus_if_request_valid = state == `InstBufferStates__request &  ~ fetch_page_limit;
	assign bus_if_request_dram_not_ext = fetch_addr[22];
	assign queue_valid = bus_if_response_valid &  ~ flushing;
	assign bus_if_request_byte_en = 2'h3;
	assign bus_if_request_data = 16'hx;
	assign bus_if_request_read_not_write = 1'h1;
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
		.input_request_to_idle(u65_output_port),
		.input_request_to_flush_start(u68_output_port),
		.input_request_to_idle_1(u70_output_port),
		.input_flush_start_to_idle(bus_if_response_valid)
	);

	assign u3_output_port = tpc + (mem_base << 4'ha);
	assign u8_output_port = fetch_addr + advance_request;
	assign u30_output_port = req_len - 1'h1;
	assign queue_av = req_av;
	assign u65_output_port =  ~ do_branch & ( ~ advance_request | req_len == 1'h0);
	assign u68_output_port = do_branch & advance_request &  ~ bus_if_response_valid;
	assign u70_output_port = do_branch & advance_request & bus_if_response_valid;
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
	output logic fetch_response_valid,

	input logic [25:0] mem_request_addr,
	input logic [1:0] mem_request_byte_en,
	input logic [15:0] mem_request_data,
	input logic mem_request_dram_not_ext,
	input logic mem_request_read_not_write,
	output logic mem_request_ready,
	input logic mem_request_valid,

	output logic [15:0] mem_response_data,
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
	logic req_advance;
	logic u28_output_port;
	logic arb_mem_not_fetch;
	logic u31_output_port;
	logic u33_output_port;
	logic u34_output_port;
	logic u35_output_port;
	logic u36_output_port;
	logic u37_output_port;
	logic u38_output_port;
	logic u39_output_port;
	logic u40_output_port;
	logic u41_output_port;
	logic signed [4:0] u44_output_port;
	logic [3:0] wait_states;
	logic [10:0] input_row_addr;
	logic [10:0] row_addr;
	logic [10:0] col_addr;
	logic read_not_write;
	logic [1:0] u65_output_port;
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
	logic [10:0] u170_output_port;
	logic [7:0] data_out_low;
	logic [7:0] data_out_high;
	logic read_active;
	logic [7:0] data_in_low;
	logic [7:0] data_in_high;
	logic [15:0] resp_data;
	logic u194_output_port;
	logic u198_output_port;
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
	always_ff @(posedge clk) u28_output_port <= rst ? 1'h0 : state == `BusIfStates__idle ? mem_request_valid : u28_output_port;
	assign arb_mem_not_fetch = state == `BusIfStates__idle ? mem_request_valid : u28_output_port;
	always_ff @(posedge clk) wait_states <= rst ? 4'h0 : start ? req_addr[25:22] : wait_states == 1'h0 ? 1'h0 : u44_output_port[3:0];
	assign input_row_addr = {req_addr[21], req_addr[19], req_addr[17], req_addr[15:8]};
	always_ff @(posedge clk) row_addr <= rst ? 11'h0 : start ? input_row_addr : row_addr;
	assign req_advance = req_valid & req_ready;
	always_ff @(posedge clk) col_addr <= rst ? 11'h0 : req_advance ? ({req_addr[20], req_addr[18], req_addr[16], req_addr[7:0]}) : col_addr;
	always_ff @(posedge clk) read_not_write <= rst ? 1'h0 : req_advance ? req_read_not_write : read_not_write;
	always_ff @(posedge clk) u65_output_port <= rst ? 2'h0 : req_advance ? req_byte_en : u65_output_port;
	assign byte_en = req_advance ? req_byte_en : u65_output_port;
	always_ff @(posedge clk) data_out <= rst ? 16'h0 : req_advance ? req_data : data_out;
	always_ff @(posedge clk) dram_not_ext <= rst ? 1'h0 : req_advance ? req_dram_not_ext : dram_not_ext;
	always_ff @(posedge clk) DRAM_nRAS <= rst ? 1'h1 : next_state == `BusIfStates__idle | next_state == `BusIfStates__external | next_state == `BusIfStates__non_dram_first | next_state == `BusIfStates__non_dram_wait | next_state == `BusIfStates__pre_external;
	always_ff @(posedge clk) nNREN <= rst ? 1'h1 : next_state != `BusIfStates__non_dram_first & next_state != `BusIfStates__non_dram_wait;
	always_ff @(posedge clk) NR_nCAS_a <= rst ? 1'h1 : state != `BusIfStates__non_dram_first |  ~ byte_en[0];
	always_ff @(posedge clk) NR_nCAS_b <= rst ? 1'h1 : state != `BusIfStates__non_dram_first |  ~ byte_en[1];
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
	always_ff @(negedge clk) u170_output_port <= rst ? 11'h0 : col_addr;
	assign dram_addr = ((state == `BusIfStates__first | state == `BusIfStates__non_dram_first) & clk) ? row_addr : u170_output_port;
	always_ff @(negedge clk) data_out_low <= rst ? 8'h0 : data_out[7:0];
	always_ff @(posedge clk) data_out_high <= rst ? 8'h0 : data_out[15:8];
	assign dram_data_out = clk ? data_out_high : data_out_low;
	assign read_active = state != `BusIfStates__idle & state != `BusIfStates__precharge & state != `BusIfStates__pre_external & state != `BusIfStates__external & read_not_write;
	always_ff @(posedge clk) data_in_low <= rst ? 8'h0 : dram_data_in;
	always_ff @(negedge clk) data_in_high <= rst ? 8'h0 : dram_data_in;
	always_ff @(posedge clk) resp_data <= rst ? 16'h0 : ({data_in_high, data_in_low});
	always_ff @(posedge clk) u194_output_port <= rst ? 1'h0 : read_active & arb_mem_not_fetch;
	always_ff @(posedge clk) mem_response_valid <= rst ? 1'h0 : u194_output_port;
	always_ff @(posedge clk) u198_output_port <= rst ? 1'h0 : read_active &  ~ arb_mem_not_fetch;
	always_ff @(posedge clk) fetch_response_valid <= rst ? 1'h0 : u198_output_port;
	assign fetch_response_data = resp_data;

	FSM fsm (
		.clock_port(clk),
		.reset_port(rst),
		.reset_value(`BusIfStates__idle),
		.state(state),
		.next_state(next_state),
		.default_state(`BusIfStates__idle),
		.input_idle_to_external(u31_output_port),
		.input_idle_to_non_dram_first(u33_output_port),
		.input_idle_to_first(u34_output_port),
		.input_external_to_idle(u35_output_port),
		.input_first_to_precharge(u36_output_port),
		.input_first_to_middle(req_valid),
		.input_middle_to_precharge(u37_output_port),
		.input_precharge_to_idle(u38_output_port),
		.input_precharge_to_pre_external(ext_req),
		.input_pre_external_to_external(ext_req),
		.input_pre_external_to_idle(u39_output_port),
		.input_non_dram_first_to_non_dram_wait(u40_output_port),
		.input_non_dram_wait_to_non_dram_wait(u41_output_port),
		.input_non_dram_wait_to_idle(dram_nWAIT)
	);

	GenericAssertOnClk u73 (
		.clk(clk),
		.rst(rst),
		.input_port(input_row_addr == row_addr | state == `BusIfStates__idle)
	);

	assign ext_grnt = 1'hx;
	assign u31_output_port = ext_req &  ~ req_valid;
	assign u33_output_port = req_valid &  ~ req_dram_not_ext;
	assign u34_output_port = req_valid & req_dram_not_ext;
	assign u35_output_port =  ~ ext_req;
	assign u36_output_port =  ~ req_valid;
	assign u37_output_port =  ~ req_valid;
	assign u38_output_port =  ~ ext_req;
	assign u39_output_port =  ~ ext_req;
	assign u40_output_port = 1'h1;
	assign u41_output_port =  ~ dram_nWAIT;
	assign u44_output_port = wait_states - 1'h1;
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

	assign state_have_0_fragments_selector = 
		(input_have_0_fragments_to_have_all_fragments ? `InstAssembleStates__have_all_fragments : 2'b0) | 
		(input_have_0_fragments_to_need_1_fragments ? `InstAssembleStates__need_1_fragments : 2'b0) | 
		(input_have_0_fragments_to_need_2_fragments ? `InstAssembleStates__need_2_fragments : 2'b0) | 
		(input_have_0_fragments_to_have_0_fragments ? `InstAssembleStates__have_0_fragments : 2'b0) | 
		(input_have_0_fragments_to_have_0_fragments_1 ? `InstAssembleStates__have_0_fragments : 2'b0) | 
		`InstAssembleStates__have_0_fragments;
	assign state_need_1_fragments_selector = (input_need_1_fragments_to_have_all_fragments ? `InstAssembleStates__have_all_fragments : 2'b0) | (input_need_1_fragments_to_need_1_fragments ? `InstAssembleStates__need_1_fragments : 2'b0) | (input_have_0_fragments_to_have_0_fragments_1 ? `InstAssembleStates__have_0_fragments : 2'b0) | `InstAssembleStates__need_1_fragments;
	assign state_need_2_fragments_selector = (input_need_2_fragments_to_need_1_fragments ? `InstAssembleStates__need_1_fragments : 2'b0) | (input_need_2_fragments_to_need_2_fragments ? `InstAssembleStates__need_2_fragments : 2'b0) | (input_have_0_fragments_to_have_0_fragments_1 ? `InstAssembleStates__have_0_fragments : 2'b0) | `InstAssembleStates__need_2_fragments;
	assign state_have_all_fragments_selector = 
		(input_have_all_fragments_to_have_all_fragments ? `InstAssembleStates__have_all_fragments : 2'b0) | 
		(input_have_all_fragments_to_have_all_fragments_1 ? `InstAssembleStates__have_all_fragments : 2'b0) | 
		(input_have_all_fragments_to_need_1_fragments ? `InstAssembleStates__need_1_fragments : 2'b0) | 
		(input_have_all_fragments_to_need_2_fragments ? `InstAssembleStates__need_2_fragments : 2'b0) | 
		(input_have_all_fragments_to_have_0_fragments ? `InstAssembleStates__have_0_fragments : 2'b0) | 
		(input_have_0_fragments_to_have_0_fragments_1 ? `InstAssembleStates__have_0_fragments : 2'b0) | 
		`InstAssembleStates__have_all_fragments;
	assign next_state = 
		(state == `InstAssembleStates__have_0_fragments ? state_have_0_fragments_selector : 2'b0) | 
		(state == `InstAssembleStates__need_1_fragments ? state_need_1_fragments_selector : 2'b0) | 
		(state == `InstAssembleStates__need_2_fragments ? state_need_2_fragments_selector : 2'b0) | 
		(state == `InstAssembleStates__have_all_fragments ? state_have_all_fragments_selector : 2'b0) | 
		default_state;

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

	assign state_idle_selector = (input_idle_to_request ? `InstBufferStates__request : 2'b0) | `InstBufferStates__idle;
	assign state_request_selector = (input_request_to_idle ? `InstBufferStates__idle : 2'b0) | (input_request_to_idle_1 ? `InstBufferStates__idle : 2'b0) | (input_request_to_flush_start ? `InstBufferStates__flush_start : 2'b0) | `InstBufferStates__request;
	assign state_flush_start_selector = (input_flush_start_to_idle ? `InstBufferStates__idle : 2'b0) | `InstBufferStates__flush_start;
	assign next_state = (state == `InstBufferStates__idle ? state_idle_selector : 2'b0) | (state == `InstBufferStates__request ? state_request_selector : 2'b0) | (state == `InstBufferStates__flush_start ? state_flush_start_selector : 2'b0) | default_state;

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

	assign state_idle_selector = (input_idle_to_external ? `BusIfStates__external : 3'b0) | (input_idle_to_non_dram_first ? `BusIfStates__non_dram_first : 3'b0) | (input_idle_to_first ? `BusIfStates__first : 3'b0) | `BusIfStates__idle;
	assign state_external_selector = (input_external_to_idle ? `BusIfStates__idle : 3'b0) | `BusIfStates__external;
	assign state_first_selector = (input_first_to_precharge ? `BusIfStates__precharge : 3'b0) | (input_first_to_middle ? `BusIfStates__middle : 3'b0) | `BusIfStates__first;
	assign state_middle_selector = (input_middle_to_precharge ? `BusIfStates__precharge : 3'b0) | `BusIfStates__middle;
	assign state_precharge_selector = (input_precharge_to_idle ? `BusIfStates__idle : 3'b0) | (input_precharge_to_pre_external ? `BusIfStates__pre_external : 3'b0) | `BusIfStates__precharge;
	assign state_pre_external_selector = (input_precharge_to_pre_external ? `BusIfStates__external : 3'b0) | (input_pre_external_to_idle ? `BusIfStates__idle : 3'b0) | `BusIfStates__pre_external;
	assign state_non_dram_first_selector = (input_non_dram_first_to_non_dram_wait ? `BusIfStates__non_dram_wait : 3'b0) | `BusIfStates__non_dram_first;
	assign state_non_dram_wait_selector = (input_non_dram_wait_to_non_dram_wait ? `BusIfStates__non_dram_wait : 3'b0) | (input_non_dram_wait_to_idle ? `BusIfStates__idle : 3'b0) | `BusIfStates__non_dram_wait;
	assign next_state = 
		(state == `BusIfStates__idle ? state_idle_selector : 3'b0) | 
		(state == `BusIfStates__external ? state_external_selector : 3'b0) | 
		(state == `BusIfStates__first ? state_first_selector : 3'b0) | 
		(state == `BusIfStates__middle ? state_middle_selector : 3'b0) | 
		(state == `BusIfStates__precharge ? state_precharge_selector : 3'b0) | 
		(state == `BusIfStates__pre_external ? state_pre_external_selector : 3'b0) | 
		(state == `BusIfStates__non_dram_first ? state_non_dram_first_selector : 3'b0) | 
		(state == `BusIfStates__non_dram_wait ? state_non_dram_wait_selector : 3'b0) | 
		default_state;

endmodule


