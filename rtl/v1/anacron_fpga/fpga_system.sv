////////////////////////////////////////////////////////////////////////////////
// FpgaSystem
////////////////////////////////////////////////////////////////////////////////
module FpgaSystem (
	input logic clk,
	input logic clk2,
	input logic rst,
	input logic [10:0] brew_if_addr,
	input logic brew_if_bus_en,
	output logic [7:0] brew_if_data_in,
	input logic [7:0] brew_if_data_out,
	input logic brew_if_data_out_en,
	input logic brew_if_n_cas_0,
	input logic brew_if_n_cas_1,
	input logic [3:0] brew_if_n_dack,
	input logic brew_if_n_nren,
	input logic brew_if_n_ras_a,
	input logic brew_if_n_ras_b,
	output logic brew_if_n_wait,
	input logic brew_if_n_we,
	input logic brew_if_tc,

	output logic [15:0] io_apb_if_paddr,
	output logic io_apb_if_penable,
	input logic [7:0] io_apb_if_prdata,
	input logic io_apb_if_pready,
	output logic io_apb_if_psel,
	output logic [7:0] io_apb_if_pwdata,
	output logic io_apb_if_pwrite,

	output logic [7:0] output_pins,
	input logic [7:0] input_pins,
	output logic [7:0] output_pins2,
	input logic [7:0] input_pins2
);

	logic [10:0] u_output_port;
	logic [7:0] ext_if_data_out_r;
	logic [7:0] ext_if_data_out_nr;
	logic [7:0] decode_input_data_out;
	logic [7:0] ext_if_data_out_0;
	logic [7:0] ext_if_data_out_1;
	logic decode_input_n_ras_a;
	logic ext_if_n_ras_a;
	logic decode_input_n_ras_b;
	logic ext_if_n_ras_b;
	logic decode_input_n_cas_0;
	logic ext_if_n_cas_0;
	logic decode_input_n_cas_1;
	logic ext_if_n_cas_1;
	logic [10:0] decode_input_addr;
	logic [10:0] ext_if_addr;
	logic decode_input_n_we;
	logic ext_if_n_we;
	logic decode_input_data_out_en;
	logic ext_if_data_out_en;
	logic decode_input_n_nren;
	logic ext_if_n_nren;
	logic [3:0] decode_input_n_dack;
	logic [3:0] ext_if_n_dack;
	logic decode_input_bus_en;
	logic ext_if_bus_en;
	logic decode_input_tc;
	logic ext_if_tc;
	logic u34_output_port;
	logic u35_output_port;
	logic u36_output_port;
	logic [7:0] dram0_addr;
	logic [7:0] dram1_addr;
	logic [7:0] dram0_data_out;
	logic [7:0] dram1_data_out;
	logic [7:0] decode_gpio_data_in;
	logic [7:0] decode_gpio2_data_in;
	logic [22:0] decode_addr;
	logic decode_rom;
	logic decode_gpio;
	logic decode_gpio2;
	logic decode_io_apb;
	logic [7:0] decode_input_data_in;
	logic decode_input_n_wait;
	logic [7:0] decode_rom_data_in;
	logic [7:0] decode_io_apb_data_in;
	logic decode_io_apb_n_wait;

	always_ff @(posedge clk2) ext_if_data_out_r <= rst ? 8'h0 : brew_if_data_out;
	always_ff @(negedge clk2) ext_if_data_out_nr <= rst ? 8'h0 : brew_if_data_out;
	always_ff @(posedge clk2) ext_if_n_ras_a <= rst ? 1'h1 : brew_if_n_ras_a;
	initial ext_if_n_ras_a <= 1'h1;
	always_ff @(posedge clk2) ext_if_n_cas_0 <= rst ? 1'h1 : brew_if_n_cas_0;
	initial ext_if_n_cas_0 <= 1'h1;
	always_ff @(posedge clk2) ext_if_n_cas_1 <= rst ? 1'h1 : brew_if_n_cas_1;
	initial ext_if_n_cas_1 <= 1'h1;
	assign brew_if_data_in = ( ~ ext_if_n_ras_a &  ~ ext_if_n_cas_0 ? dram0_data_out : 8'b0) | ( ~ ext_if_n_ras_a &  ~ ext_if_n_cas_1 ? dram1_data_out : 8'b0) | ( ~ ext_if_n_ras_a &  ~ ext_if_n_cas_0 |  ~ ext_if_n_ras_a &  ~ ext_if_n_cas_1 ? 8'b0 : decode_input_data_in);
	always_ff @(posedge clk2) ext_if_n_ras_b <= rst ? 1'h1 : brew_if_n_ras_b;
	initial ext_if_n_ras_b <= 1'h1;
	always_ff @(posedge clk2) ext_if_addr <= rst ? 11'h0 : brew_if_addr;
	always_ff @(posedge clk2) ext_if_n_we <= rst ? 1'h1 : brew_if_n_we;
	initial ext_if_n_we <= 1'h1;
	always_ff @(posedge clk2) ext_if_data_out_0 <= rst ? 8'h0 : ext_if_data_out_r;
	always_ff @(posedge clk2) ext_if_data_out_en <= rst ? 1'h0 : brew_if_data_out_en;
	always_ff @(posedge clk2) ext_if_n_nren <= rst ? 1'h1 : brew_if_n_nren;
	initial ext_if_n_nren <= 1'h1;
	always_ff @(posedge clk2) ext_if_n_dack <= rst ? 1'h1 : brew_if_n_dack;
	initial ext_if_n_dack <= 1'h1;
	always_ff @(posedge clk2) ext_if_tc <= rst ? 1'h0 : brew_if_tc;
	always_ff @(posedge clk2) ext_if_bus_en <= rst ? 1'h0 : brew_if_bus_en;
	always_ff @(posedge clk2) ext_if_data_out_1 <= rst ? 8'h0 : ext_if_data_out_nr;
	always_ff @(posedge clk2) u_output_port <= rst ? 11'h0 : brew_if_addr;

	fpga_system_Dram dram0 (
		.clk(clk2),
		.rst(rst),
		.addr(dram0_addr),
		.data_in(ext_if_data_out_0),
		.data_out(dram0_data_out),
		.n_ras(ext_if_n_ras_a),
		.n_cas(ext_if_n_cas_0),
		.n_we(ext_if_n_we)
	);

	fpga_system_Dram_2 dram1 (
		.clk(clk2),
		.rst(rst),
		.addr(dram1_addr),
		.data_in(ext_if_data_out_1),
		.data_out(dram1_data_out),
		.n_ras(ext_if_n_ras_a),
		.n_cas(ext_if_n_cas_1),
		.n_we(ext_if_n_we)
	);

	fpga_system_AddrDecode decode (
		.clk(clk2),
		.brew_if_addr(ext_if_addr),
		.brew_if_bus_en(ext_if_bus_en),
		.brew_if_data_in(decode_input_data_in),
		.brew_if_data_out(ext_if_data_out_0),
		.brew_if_data_out_en(ext_if_data_out_en),
		.brew_if_n_cas_0(ext_if_n_cas_0),
		.brew_if_n_cas_1(ext_if_n_cas_1),
		.brew_if_n_dack(ext_if_n_dack),
		.brew_if_n_nren(ext_if_n_nren),
		.brew_if_n_ras_a(ext_if_n_ras_a),
		.brew_if_n_ras_b(ext_if_n_ras_b),
		.brew_if_n_wait(decode_input_n_wait),
		.brew_if_n_we(ext_if_n_we),
		.brew_if_tc(ext_if_tc),

		.addr(decode_addr),
		.rom(decode_rom),
		.rom_data_in(decode_rom_data_in),
		.rom_n_wait(u34_output_port),
		.gpio(decode_gpio),
		.gpio_data_in(decode_gpio_data_in),
		.gpio_n_wait(u35_output_port),
		.gpio2(decode_gpio2),
		.gpio2_data_in(decode_gpio2_data_in),
		.gpio2_n_wait(u36_output_port),
		.io_apb(decode_io_apb),
		.io_apb_data_in(decode_io_apb_data_in),
		.io_apb_n_wait(decode_io_apb_n_wait)
	);

	fpga_system_Sram rom (
		.clk(clk2),
		.rst(rst),
		.addr(decode_addr[12:0]),
		.data_in(ext_if_data_out_0),
		.data_out(decode_rom_data_in),
		.n_ce(decode_rom),
		.n_we(ext_if_n_we)
	);

	fpga_system_Gpio gpio (
		.clk(clk),
		.rst(rst),
		.data_in(ext_if_data_out_0),
		.data_out(decode_gpio_data_in),
		.n_ce(decode_gpio),
		.n_we(ext_if_n_we),
		.output_pins(output_pins),
		.input_pins(input_pins)
	);

	fpga_system_Gpio gpio2 (
		.clk(clk),
		.rst(rst),
		.data_in(ext_if_data_out_0),
		.data_out(decode_gpio2_data_in),
		.n_ce(decode_gpio2),
		.n_we(ext_if_n_we),
		.output_pins(output_pins2),
		.input_pins(input_pins2)
	);

	fpga_system_ApbBridge apb_bridge (
		.clk(clk2),
		.rst(rst),
		.apb_out_paddr(io_apb_if_paddr),
		.apb_out_penable(io_apb_if_penable),
		.apb_out_prdata(io_apb_if_prdata),
		.apb_out_pready(io_apb_if_pready),
		.apb_out_psel(io_apb_if_psel),
		.apb_out_pwdata(io_apb_if_pwdata),
		.apb_out_pwrite(io_apb_if_pwrite),

		.n_ce(decode_io_apb),
		.n_we(ext_if_n_we),
		.n_wait(decode_io_apb_n_wait),
		.addr(decode_addr[15:0]),
		.data_in(ext_if_data_out_0),
		.data_out(decode_io_apb_data_in)
	);

	assign decode_input_data_out = ext_if_data_out_0;
	assign decode_input_n_ras_a = ext_if_n_ras_a;
	assign decode_input_n_ras_b = ext_if_n_ras_b;
	assign decode_input_n_cas_0 = ext_if_n_cas_0;
	assign decode_input_n_cas_1 = ext_if_n_cas_1;
	assign decode_input_addr = ext_if_addr;
	assign decode_input_n_we = ext_if_n_we;
	assign decode_input_data_out_en = ext_if_data_out_en;
	assign decode_input_n_nren = ext_if_n_nren;
	assign decode_input_n_dack = ext_if_n_dack;
	assign decode_input_bus_en = ext_if_bus_en;
	assign decode_input_tc = ext_if_tc;
	assign u34_output_port = 1'h1;
	assign u35_output_port = 1'h1;
	assign u36_output_port = 1'h1;
	assign dram0_addr = ext_if_addr[7:0];
	assign dram1_addr = ext_if_addr[7:0];
	assign brew_if_n_wait = decode_input_n_wait;
endmodule


////////////////////////////////////////////////////////////////////////////////
// fpga_system_ApbBridge
////////////////////////////////////////////////////////////////////////////////
module fpga_system_ApbBridge (
	input logic clk,
	input logic rst,
	output logic [15:0] apb_out_paddr,
	output logic apb_out_penable,
	input logic [7:0] apb_out_prdata,
	input logic apb_out_pready,
	output logic apb_out_psel,
	output logic [7:0] apb_out_pwdata,
	output logic apb_out_pwrite,

	input logic n_ce,
	input logic n_we,
	output logic n_wait,
	input logic [15:0] addr,
	input logic [7:0] data_in,
	output logic [7:0] data_out
);

	logic apb_done;
	logic served;
	logic [7:0] u23_output_port;

	always_ff @(posedge clk) apb_out_paddr <= rst ? 16'h0 :  ~ n_ce ? addr : apb_out_paddr;
	always_ff @(posedge clk) apb_out_pwdata <= rst ? 8'h0 :  ~ n_ce ? data_in : apb_out_pwdata;
	always_ff @(posedge clk) apb_out_pwrite <= rst ? 1'h0 :  ~ n_ce ?  ~ n_we : apb_out_pwrite;
	assign apb_done = apb_out_penable & apb_out_pready;
	always_ff @(posedge clk) served <= rst ? 1'h0 : n_ce ? 1'h0 : apb_done | served;
	always_ff @(posedge clk) apb_out_psel <= rst ? 1'h0 : apb_out_psel ?  ~ apb_done :  ~ n_ce &  ~ served;
	always_ff @(posedge clk) apb_out_penable <= rst ? 1'h0 : apb_out_penable ?  ~ apb_done : apb_out_psel &  ~ served;
	always_ff @(posedge clk) u23_output_port <= rst ? 8'h0 : apb_done ? apb_out_prdata : u23_output_port;
	assign data_out = apb_done ? apb_out_prdata : u23_output_port;
	assign n_wait = apb_done | n_ce | served;

endmodule


////////////////////////////////////////////////////////////////////////////////
// fpga_system_Gpio
////////////////////////////////////////////////////////////////////////////////
module fpga_system_Gpio (
	input logic clk,
	input logic rst,
	input logic [7:0] data_in,
	output logic [7:0] data_out,
	input logic n_ce,
	input logic n_we,
	output logic [7:0] output_pins,
	input logic [7:0] input_pins
);

	logic [7:0] u4_output_port;

	always_ff @(posedge clk) output_pins <= rst ? 8'h0 :  ~ n_ce &  ~ n_we ? data_in : output_pins;
	always_ff @(posedge clk) u4_output_port <= rst ? 8'h0 : input_pins;
	always_ff @(posedge clk) data_out <= rst ? 8'h0 : u4_output_port;

endmodule


////////////////////////////////////////////////////////////////////////////////
// fpga_system_Sram
////////////////////////////////////////////////////////////////////////////////
module fpga_system_Sram (
	input logic clk,
	input logic rst,
	input logic [12:0] addr,
	input logic [7:0] data_in,
	output logic [7:0] data_out,
	input logic n_ce,
	input logic n_we
);

	logic u2_output_port;

	fpga_system_Memory_3 mem (
		.addr(addr),
		.clk(clk),
		.data_in(data_in),
		.data_out(data_out),
		.write_en(u2_output_port)
	);

	assign u2_output_port =  ~ n_ce &  ~ n_we;
endmodule


////////////////////////////////////////////////////////////////////////////////
// fpga_system_AddrDecode
////////////////////////////////////////////////////////////////////////////////
module fpga_system_AddrDecode (
	input logic clk,
	input logic [10:0] brew_if_addr,
	input logic brew_if_bus_en,
	output logic [7:0] brew_if_data_in,
	input logic [7:0] brew_if_data_out,
	input logic brew_if_data_out_en,
	input logic brew_if_n_cas_0,
	input logic brew_if_n_cas_1,
	input logic [3:0] brew_if_n_dack,
	input logic brew_if_n_nren,
	input logic brew_if_n_ras_a,
	input logic brew_if_n_ras_b,
	output logic brew_if_n_wait,
	input logic brew_if_n_we,
	input logic brew_if_tc,

	output logic [22:0] addr,
	output logic rom,
	input logic [7:0] rom_data_in,
	input logic rom_n_wait,
	output logic gpio,
	input logic [7:0] gpio_data_in,
	input logic gpio_n_wait,
	output logic gpio2,
	input logic [7:0] gpio2_data_in,
	input logic gpio2_n_wait,
	output logic io_apb,
	input logic [7:0] io_apb_data_in,
	input logic io_apb_n_wait
);

	logic prev_n_nren;
	logic [10:0] row_addr;
	logic n_cas;
	logic n_enable;

	always_ff @(posedge clk) prev_n_nren <= brew_if_n_nren;
	always_ff @(posedge clk) row_addr <= ( ~ brew_if_n_nren & prev_n_nren ? brew_if_addr : row_addr);
	assign addr = {row_addr, brew_if_addr, brew_if_n_cas_0};
	assign n_cas = brew_if_n_cas_0 & brew_if_n_cas_1;
	always_ff @(posedge clk) rom <= (addr[22:13] != 1'h0 | n_cas |  ~ brew_if_bus_en | brew_if_n_nren);
	always_ff @(posedge clk) gpio <= (addr[22:12] != 5'h10 | n_cas |  ~ brew_if_bus_en | brew_if_n_nren);
	always_ff @(posedge clk) gpio2 <= (addr[22:12] != 5'h11 | n_cas |  ~ brew_if_bus_en | brew_if_n_nren);
	assign n_enable = addr[22:16] != 2'h2 | n_cas |  ~ brew_if_bus_en | brew_if_n_nren;
	always_ff @(posedge clk) io_apb <= n_enable;
	assign brew_if_data_in = 
		( ~ (addr[22:13] != 1'h0 | n_cas |  ~ brew_if_bus_en | brew_if_n_nren) ? rom_data_in : 8'b0) | 
		( ~ (addr[22:12] != 5'h10 | n_cas |  ~ brew_if_bus_en | brew_if_n_nren) ? gpio_data_in : 8'b0) | 
		( ~ (addr[22:12] != 5'h11 | n_cas |  ~ brew_if_bus_en | brew_if_n_nren) ? gpio2_data_in : 8'b0) | 
		( ~ n_enable ? io_apb_data_in : 8'b0) ;
	assign brew_if_n_wait = 
		( ~ (addr[22:13] != 1'h0 | n_cas |  ~ brew_if_bus_en | brew_if_n_nren) ? rom_n_wait : 1'b0) | 
		( ~ (addr[22:12] != 5'h10 | n_cas |  ~ brew_if_bus_en | brew_if_n_nren) ? gpio_n_wait : 1'b0) | 
		( ~ (addr[22:12] != 5'h11 | n_cas |  ~ brew_if_bus_en | brew_if_n_nren) ? gpio2_n_wait : 1'b0) | 
		( ~ n_enable ? io_apb_n_wait : 1'b0) ;

endmodule


////////////////////////////////////////////////////////////////////////////////
// fpga_system_Dram_2
////////////////////////////////////////////////////////////////////////////////
module fpga_system_Dram_2 (
	input logic clk,
	input logic rst,
	input logic [7:0] addr,
	input logic [7:0] data_in,
	output logic [7:0] data_out,
	input logic n_ras,
	input logic n_cas,
	input logic n_we
);

	logic prev_n_ras;
	logic [7:0] row_addr;
	logic u9_output_port;

	always_ff @(posedge clk) prev_n_ras <= rst ? 1'h0 : n_ras;
	always_ff @(posedge clk) row_addr <= rst ? 8'h0 :  ~ n_ras & prev_n_ras ? addr : row_addr;

	fpga_system_Memory_2 mem (
		.addr({row_addr, addr}),
		.clk(clk),
		.data_in(data_in),
		.data_out(data_out),
		.write_en(u9_output_port)
	);

	assign u9_output_port =  ~ n_cas &  ~ n_we &  ~ n_ras;
endmodule


////////////////////////////////////////////////////////////////////////////////
// fpga_system_Dram
////////////////////////////////////////////////////////////////////////////////
module fpga_system_Dram (
	input logic clk,
	input logic rst,
	input logic [7:0] addr,
	input logic [7:0] data_in,
	output logic [7:0] data_out,
	input logic n_ras,
	input logic n_cas,
	input logic n_we
);

	logic prev_n_ras;
	logic [7:0] row_addr;
	logic ram_clk;
	logic u10_output_port;

	always_ff @(posedge clk) prev_n_ras <= rst ? 1'h0 : n_ras;
	always_ff @(posedge clk) row_addr <= rst ? 8'h0 :  ~ n_ras & prev_n_ras ? addr : row_addr;
	assign ram_clk =  ~ clk;

	fpga_system_Memory mem (
		.addr({row_addr, addr}),
		.clk(ram_clk),
		.data_in(data_in),
		.data_out(data_out),
		.write_en(u10_output_port)
	);

	assign u10_output_port =  ~ n_cas &  ~ n_we &  ~ n_ras;
endmodule


////////////////////////////////////////////////////////////////////////////////
// fpga_system_Memory_3
////////////////////////////////////////////////////////////////////////////////
module fpga_system_Memory_3 (
	input logic [12:0] addr,
	input logic clk,
	input logic [7:0] data_in,
	output logic [7:0] data_out,
	input logic write_en
);

	logic [7:0] mem [0:8191];
	initial begin
		$readmemh("rom.mef", mem);
	end

	always @(posedge clk) begin
		if (write_en) begin
			mem[addr] <= data_in;
		end
		data_out <= mem[addr];
	end

endmodule


////////////////////////////////////////////////////////////////////////////////
// fpga_system_Memory_2
////////////////////////////////////////////////////////////////////////////////
module fpga_system_Memory_2 (
	input logic [15:0] addr,
	input logic clk,
	input logic [7:0] data_in,
	output logic [7:0] data_out,
	input logic write_en
);

	logic [7:0] mem [0:32767];
	initial begin
		$readmemh("dram.1.mef", mem);
	end

	logic [15:0] addr_reg;
	always @(posedge clk) begin
		if (write_en) begin
			mem[addr] <= data_in;
		end
		addr_reg <= addr;
	end
	assign data_out = mem[addr_reg];

endmodule


////////////////////////////////////////////////////////////////////////////////
// fpga_system_Memory
////////////////////////////////////////////////////////////////////////////////
module fpga_system_Memory (
	input logic [15:0] addr,
	input logic clk,
	input logic [7:0] data_in,
	output logic [7:0] data_out,
	input logic write_en
);

	logic [7:0] mem [0:32767];
	initial begin
		$readmemh("dram.0.mef", mem);
	end

	logic [15:0] addr_reg;
	always @(posedge clk) begin
		if (write_en) begin
			mem[addr] <= data_in;
		end
		addr_reg <= addr;
	end
	assign data_out = mem[addr_reg];

endmodule


