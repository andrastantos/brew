

////////////////////////////////////////////////////////////////////////////////
// Pipeline
////////////////////////////////////////////////////////////////////////////////
module BREW_V1 (
    input logic clk,
    input logic rst,
    output logic DRAM_nRASa,
    output logic DRAM_nRASb,
    output logic DRAM_nCASa,
    output logic DRAM_nCASb,
    output logic [11:0] DRAM_ADDR,
    output logic DRAM_nWE,
    inout [7:0] DRAM_DATA_l,
    inout [7:0] DRAM_DATA_h,
    input logic ext_req,
    output logic ext_grnt,
    input logic interrupt
);
    logic [15:0] DRAM_out;

    Pipeline pipeline (
        .clk(clk),
        .rst(rst),

        .DRAM_nRAS(DRAM_nRASa),
        .DRAM_nCAS_l(DRAM_nCASa),
        .DRAM_nCAS_h(DRAM_nCASb),
        .DRAM_ADDR(DRAM_ADDR),
        .DRAM_nWE(DRAM_nWE),
        .DRAM_DATA_rd_l(DRAM_DATA_l),
        .DRAM_DATA_rd_h(DRAM_DATA_h),
        .DRAM_DATA_wr(DRAM_out),
        .ext_req(ext_req),
        .ext_grnt(ext_grnt),
        .interrupt(interrupt)
    );

    assign DRAM_nRASb = DRAM_nRASa;
    assign DRAM_DATA_l = DRAM_nCASa ? 8'bZ : DRAM_out[7:0];
    assign DRAM_DATA_h = DRAM_nCASa ? 8'bZ : DRAM_out[15:8];

endmodule
