

////////////////////////////////////////////////////////////////////////////////
// Pipeline
////////////////////////////////////////////////////////////////////////////////
module BREW_V1 (
    input  logic        CLK,
    input  logic        RST,
    inout  logic        DRAM_nNREN,
    inout  logic        DRAM_nRAS_A,
    inout  logic        DRAM_nRAS_B,
    inout  logic        DRAM_nCAS_0,
    inout  logic        DRAM_nCAS_1,
    inout  logic [10:0] DRAM_ADDR,
    inout  logic        DRAM_nWE,
    inout  logic [7:0]  DRAM_DATA,
    input  logic        DRAM_nWAIT,
    input  logic [3:0]  DRQ,
    output logic [3:0]  nDACK,
    output logic        TC,
    input  logic        nINT
);
    logic        p_dram_nNREN;
    logic        p_dram_nRAS_A;
    logic        p_dram_nRAS_B;
    logic        p_dram_nCAS_0;
    logic        p_dram_nCAS_1;
    logic [10:0] p_dram_addr;
    logic        p_dram_nWE;
    logic [7:0]  p_dram_data_out;
    logic        p_dram_data_out_en;
    logic        p_bus_en;

    BrewV1Top brew_v1_top(
        .clk                 (CLK),
        .rst                 (RST),

        .dram_nNREN          (p_dram_nNREN),
        .dram_nRAS_A         (p_dram_nRAS_A),
        .dram_nRAS_B         (p_dram_nRAS_B),
        .dram_nCAS_0         (p_dram_nCAS_0),
        .dram_nCAS_1         (p_dram_nCAS_1),
        .dram_addr           (p_dram_addr),
        .dram_nWE            (p_dram_nWE),
        .dram_data_in        (DRAM_DATA),
        .dram_data_out       (p_dram_data_out),
        .dram_data_out_en    (p_dram_data_out_en),
        .dram_nWAIT          (DRAM_nWAIT),
        .dram_nDACK          (nDACK),
        .dram_TC             (TC),
        .dram_bus_en         (p_bus_en),
        .nINT                (nINT),
        .DRQ                 (DRQ)
    );

    assign DRAM_nRAS_A = p_bus_en ? p_dram_nRAS_A : 1'bZ;
    assign DRAM_nRAS_B = p_bus_en ? p_dram_nRAS_B : 1'bZ;
    assign DRAM_nNREN = p_bus_en ? p_dram_nNREN : 1'bZ;
    assign DRAM_nCAS_0 = p_bus_en ? p_dram_nCAS_0 : 1'bZ;
    assign DRAM_nCAS_1 = p_bus_en ? p_dram_nCAS_1 : 1'bZ;
    assign DRAM_ADDR = p_bus_en ? p_dram_addr : 11'bZ;
    assign DRAM_nWE = p_bus_en ? p_dram_nWE : 1'bZ;
    assign DRAM_DATA = (p_bus_en & p_dram_data_out_en) ? p_dram_data_out : 8'bZ;

endmodule
