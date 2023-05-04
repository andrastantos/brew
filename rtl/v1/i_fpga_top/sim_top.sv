module top();

    logic clk;
    logic clk2;
    logic rst;
    logic [7:0] input_pins;
    logic [7:0] output_pins;

    FpgaTop dut(.*);

    initial begin
        clk = 0;
        clk2 = 0;
    end

    always #10 clk2 = ~clk2;
    always #50 clk = ~clk;

    initial begin
        $display("Reset applied");
        rst = 1;
        #500 rst = 0;
        $display("Reset removed");
    end

    initial begin
    	$dumpfile("fpga_top.vcd");
    	$dumpvars(0,top);
        #(1000*1000) $finish;
    end
endmodule
