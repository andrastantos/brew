`timescale 1ns/1ns

module top();

    logic clk;
    logic clk2;
    logic n_rst;

    // GPIO
    logic [7:0] input_pins;
    logic [7:0] output_pins;

    // UART
    logic rxd;
	logic txd;
	logic cts;
	logic rts;
	logic n_tx_en;

    FpgaTop dut(.*);

    initial begin
        clk = 1;
        clk2 = 1;
    end

    always #10 clk2 = ~clk2;
    always #50 clk = ~clk;

    initial begin
        $display("Reset applied");
        n_rst = 0;
        #500 n_rst = 1;
        $display("Reset removed");
        //#30006 n_rst = 0;
        #28660 n_rst = 0;
        $display("Reset applied");
        #30104 n_rst = 1;
        $display("Reset removed");
    end

    initial begin
    	$dumpfile("anacron_fpga.vcd");
    	$dumpvars(0,top);
        #(1000*1000) $finish;
    end
endmodule
