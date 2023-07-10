////////////////////////////////////////////////////////////////////////////////
// ApbGpio
////////////////////////////////////////////////////////////////////////////////
module ApbGpio (
	input logic clk,
	input logic rst,
	input logic [2:0] bus_if_paddr,
	input logic bus_if_penable,
	output logic [7:0] bus_if_prdata,
	output logic bus_if_pready,
	input logic bus_if_psel,
	input logic [7:0] bus_if_pwdata,
	input logic bus_if_pwrite,

	output logic [7:0] output_port,
	output logic output_update,
	input logic [7:0] input_port,
	output logic input_sampled
);

	logic data_reg_wr;
	logic data_reg_rd;

	assign data_reg_rd = bus_if_psel & bus_if_penable &  ~ bus_if_pwrite & bus_if_paddr == 1'h0;
	always_ff @(posedge clk) bus_if_prdata <= rst ? 8'h0 : data_reg_rd ? input_port : bus_if_prdata;
	assign data_reg_wr = bus_if_psel & bus_if_penable & bus_if_pwrite & bus_if_paddr == 1'h0;
	always_ff @(posedge clk) output_port <= rst ? 8'h0 : data_reg_wr ? bus_if_pwdata : output_port;
	always_ff @(posedge clk) output_update <= rst ? 1'h0 : data_reg_wr;
	assign bus_if_pready = 1'h1;

	assign input_sampled = data_reg_rd;
endmodule


