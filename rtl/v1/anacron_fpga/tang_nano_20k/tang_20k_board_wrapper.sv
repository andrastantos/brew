////////////////////////////////////////////////////////////////////////////////
// Tank Nano 20k Board Wrapper
////////////////////////////////////////////////////////////////////////////////

// Pinout for USB UART board (component side, connector down, left to right):
//   DTR; RXD; TXD; VCC; CTS; GND
//   The VCC (3.3V) output is internally generated, so it's safe to connect
//   to a GPIO.clk
//
// The way we're going to hook this up is this:
//   rts - 
//   rxd - TXD - pin 69
//   txd - RXD - pin 70
//   cts - 
//
// For GPIOs, we will do the following:
//
//  output_pins[0]   -
//  output_pins[1]   -
//  output_pins[2]   - LED[0] - pin 15
//  output_pins[3]   - LED[1] - pin 16
//  output_pins[4]   - LED[2] - pin 17
//  output_pins[5]   - LED[3] - pin 18
//  output_pins[6]   - LED[4] - pin 19
//  output_pins[7]   - LED[5] - pin 20
//
//  output_pins3[0]
//  output_pins3[1]
//  output_pins3[2]
//  output_pins3[3]
//  output_pins3[4]
//  output_pins3[5]
//  output_pins3[6]
//  output_pins3[7]
//
//  output_pins4[0]
//  output_pins4[1]
//  output_pins4[2]
//  output_pins4[3]
//  output_pins4[4]
//  output_pins4[5]
//  output_pins4[6]
//  output_pins4[7]

//  input_pins[0]
//  input_pins[1]
//  input_pins[2]
//  input_pins[3]
//  input_pins[4]
//  input_pins[5]
//  input_pins[6]
//  input_pins[7]
//
//  input_pins3[0]
//  input_pins3[1]
//  input_pins3[2]
//  input_pins3[3]
//  input_pins3[4]
//  input_pins3[5]
//  input_pins3[6]
//  input_pins3[7]
//
//  input_pins4[0]
//  input_pins4[1]
//  input_pins4[2]
//  input_pins4[3]
//  input_pins4[4]
//  input_pins4[5]
//  input_pins4[6]
//  input_pins4[7]
//
//  clk              - 
//  clk2             - 
//
//  n_rst            - MODE0_KEY1 - pin 88

module DecaTop(
    input logic        CLK_IN_27MHZ,

    //////////// KEY //////////
    input logic   MODE0_KEY1,

    //////////// LED //////////
    output logic[5:0]  LED,

    //////////// UART /////////
    input logic rxd,
    output logic txd
);

	//logic [22:0]cnt;
	//logic [22:0]cnt2;
	
	logic clk;
	logic clk2;

    Gowin_rPLL pll(
        .clkout(clk2), //output clkout
        .clkin(CLK_IN_27MHZ) //input clkin
    );
    Gowin_CLKDIV clk_div(
        .clkout(clk),
        .hclkin(clk2),
        .resetn(1'b1)
    );


	//always @(posedge clk) begin
	//	cnt <= cnt + 1'b1;
	//end
	//always @(posedge clk2) begin
	//	cnt2 <= cnt2 + 1'b1;
	//end
	//assign GPIO1_D = {cnt[22:11], cnt2[22:11]};
	
    logic [7:0] output_pins;
	assign LED = output_pins[7:2];
	
    FpgaTop fpga_top(
        .clk(clk),
        .clk2(clk2),
        .n_rst(MODE0_KEY1),

        .output_pins(output_pins),
        //.input_pins  ({KEY[0],      SW[0],       GPIO0_D[ 0], GPIO0_D[ 2], GPIO0_D[ 4], GPIO0_D[ 6], GPIO0_D[ 8], GPIO0_D[10]}),

        //.output_pins3({GPIO0_D[13], GPIO0_D[15], GPIO0_D[17], GPIO0_D[19], GPIO0_D[21], GPIO0_D[23], GPIO0_D[25], GPIO0_D[27]}),
        //.input_pins3 ({GPIO0_D[12], GPIO0_D[14], GPIO0_D[16], GPIO0_D[18], GPIO0_D[20], GPIO0_D[22], GPIO0_D[24], GPIO0_D[26]}),

        //.output_pins4({GPIO0_D[29], GPIO0_D[31], GPIO0_D[33], GPIO0_D[35], GPIO0_D[37], GPIO0_D[39], GPIO0_D[41], GPIO0_D[43]}),
        //.input_pins4 ({GPIO0_D[28], GPIO0_D[30], GPIO0_D[32], GPIO0_D[34], GPIO0_D[36], GPIO0_D[38], GPIO0_D[40], GPIO0_D[42]}),

        .rxd(rxd),
	    .txd(txd)
	    //.cts(GPIO0_D[9]),
	    //.rts(GPIO0_D[1])
    );
endmodule