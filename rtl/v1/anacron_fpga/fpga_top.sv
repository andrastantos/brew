////////////////////////////////////////////////////////////////////////////////
// FpgaTop
////////////////////////////////////////////////////////////////////////////////
module FpgaTop (
	input logic clk,
	input logic clk2,
	input logic n_rst,

    // GPIO
	output logic [7:0] output_pins,
	input logic [7:0] input_pins,

    // UART
    input logic rxd,
	output logic txd,
	input logic cts,
	output logic rts,
	output logic n_tx_en
);

	logic rst;
	logic [3:0] drq;
	logic n_int;

    logic        ext_bus_tc;
	logic        ext_bus_n_we;
	logic        ext_bus_data_out_en;
	logic        ext_bus_n_ras_a;
	logic        ext_bus_n_ras_b;
	logic        ext_bus_n_nren;
	logic [3:0]  ext_bus_n_dack;
	logic        ext_bus_n_cas_0;
	logic        ext_bus_n_cas_1;
	logic [10:0] ext_bus_addr;
	logic [7:0]  ext_bus_data_out;
	logic        ext_bus_bus_en;
	logic [7:0]  ext_bus_data_in;
	logic        ext_bus_n_wait;

    logic [15:0] system_io_apb_if_paddr;
	logic [7:0]  system_io_apb_if_pwdata;
	logic        system_io_apb_if_pwrite;
	logic        system_io_apb_if_psel;
	logic        system_io_apb_if_penable;
	logic [7:0]  system_io_apb_if_prdata;
	logic        system_io_apb_if_pready;

	assign drq = 1'h0;
	assign n_int = 1'h1;

	assign rst =  ~ n_rst;

	BrewV1Top brew (
		.clk(clk),
		.rst(rst),
		.dram_addr(ext_bus_addr),
		.dram_bus_en(ext_bus_bus_en),
		.dram_data_in(ext_bus_data_in),
		.dram_data_out(ext_bus_data_out),
		.dram_data_out_en(ext_bus_data_out_en),
		.dram_n_cas_0(ext_bus_n_cas_0),
		.dram_n_cas_1(ext_bus_n_cas_1),
		.dram_n_dack(ext_bus_n_dack),
		.dram_n_nren(ext_bus_n_nren),
		.dram_n_ras_a(ext_bus_n_ras_a),
		.dram_n_ras_b(ext_bus_n_ras_b),
		.dram_n_wait(ext_bus_n_wait),
		.dram_n_we(ext_bus_n_we),
		.dram_tc(ext_bus_tc),

		.drq(drq),
		.n_int(n_int)
	);

	FpgaSystem system (
		.clk(clk),
		.clk2(clk2),
		.rst(rst),
		.brew_if_addr(ext_bus_addr),
		.brew_if_bus_en(ext_bus_bus_en),
		.brew_if_data_in(ext_bus_data_in),
		.brew_if_data_out(ext_bus_data_out),
		.brew_if_data_out_en(ext_bus_data_out_en),
		.brew_if_n_cas_0(ext_bus_n_cas_0),
		.brew_if_n_cas_1(ext_bus_n_cas_1),
		.brew_if_n_dack(ext_bus_n_dack),
		.brew_if_n_nren(ext_bus_n_nren),
		.brew_if_n_ras_a(ext_bus_n_ras_a),
		.brew_if_n_ras_b(ext_bus_n_ras_b),
		.brew_if_n_wait(ext_bus_n_wait),
		.brew_if_n_we(ext_bus_n_we),
		.brew_if_tc(ext_bus_tc),

		.io_apb_if_paddr    (system_io_apb_if_paddr),
		.io_apb_if_penable  (system_io_apb_if_penable),
		.io_apb_if_prdata   (system_io_apb_if_prdata),
		.io_apb_if_pready   (system_io_apb_if_pready),
		.io_apb_if_psel     (system_io_apb_if_psel),
		.io_apb_if_pwdata   (system_io_apb_if_pwdata),
		.io_apb_if_pwrite   (system_io_apb_if_pwrite),

		.output_pins(output_pins),
		.input_pins(input_pins)
	);

    logic uart1_psel;
    assign uart1_psel = system_io_apb_if_psel & (system_io_apb_if_paddr[15:8] == 8'h00);
    logic uart_int;

    ApbUart uart1 (
        .clk(clk2),
        .rst(rst),
        .bus_if_paddr    (system_io_apb_if_paddr[2:0]),
        .bus_if_penable  (system_io_apb_if_penable),
        .bus_if_prdata   (system_io_apb_if_prdata),
        .bus_if_pready   (system_io_apb_if_pready),
        .bus_if_psel     (uart1_psel),
        .bus_if_pwdata   (system_io_apb_if_pwdata),
        .bus_if_pwrite   (system_io_apb_if_pwrite),

        .interrupt  (uart_int),
        .rxd        (rxd),
        .txd        (txd),
        .cts        (cts),
        .rts        (rts),
        .n_tx_en    (n_tx_en)
    );
endmodule
