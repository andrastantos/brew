

////////////////////////////////////////////////////////////////////////////////
// Pipeline
////////////////////////////////////////////////////////////////////////////////
module BREW_V1 (
    input logic clk,
    input logic rst,
    output logic DRAM_nRAS,
    output logic DRAM_nCAS_a,
    output logic DRAM_nCAS_b,
    output logic [10:0] DRAM_ADDR,
    output logic DRAM_nWE,
    inout [7:0] DRAM_DATA,
    input logic DRAM_nWAIT,
    input logic ext_req,
    output logic ext_grnt,
    input logic interrupt
);
    logic [7:0] DRAM_out;

    Pipeline pipeline (
        .clk(clk),
        .rst(rst),

        .dram_nRAS(DRAM_nRAS),
        .dram_nCAS_a(DRAM_nCAS_a),
        .dram_nCAS_b(DRAM_nCAS_b),
        .dram_addr(DRAM_ADDR),
        .dram_nWE(DRAM_nWE),
        .dram_data_in(DRAM_DATA),
        .dram_data_out(DRAM_out),
        .dram_nWAIT(DRAM_nWAIT),
        .ext_req(ext_req),
        .ext_grnt(ext_grnt),
        .interrupt(interrupt)
    );

    assign DRAM_DATA = DRAM_nWE ? 8'bZ : DRAM_out;

endmodule
