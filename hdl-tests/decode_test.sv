module decode_test(
	input logic clk,
	input logic rst,
	input logic [15:0] insn,
	output logic insn_16_bit,
	output logic insn_32_bit,
	output logic insn_branch
);
	logic [15:0] insn_r;
	logic insn_16_bit_l;
	logic insn_32_bit_l;
	logic insn_branch_l;
	logic [3:0] FIELD_D;
	logic [3:0] FIELD_C;
	logic [3:0] FIELD_B;
	logic [3:0] FIELD_A;
	
	always_ff @ (posedge clk) begin
		insn_r <= rst ? 16'b0 : insn;
	end

	assign FIELD_D = insn_r[15:12];
	assign FIELD_C = insn_r[11:8];
	assign FIELD_B = insn_r[7:4];
	assign FIELD_A = insn_r[3:0];
	
	assign insn_16_bit_l = ~(
	  FIELD_D == 4'hf ||
	  (FIELD_C == 4'hf && (FIELD_B != 4'hf || FIELD_A == 4'hf)) ||
	  (FIELD_C == 4'he && FIELD_A == 4'hf) ||
	  (FIELD_C < 4'hc && (FIELD_B == 4'hf || FIELD_A == 4'hf))
	);
	
	assign insn_32_bit_l = FIELD_D == 4'hf || FIELD_A != 4'hf;

	assign insn_branch_l = 
	  (FIELD_D == 4'hf         && FIELD_C != 4'hf) ||
	  ((FIELD_D & 4'he) == 4'h2 && (FIELD_C & 4'he) == 4'he && FIELD_B == 4'he) ||
	  ((FIELD_D & 4'he) == 4'h2 && FIELD_C == 4'h0         && (FIELD_B & 4'he) == 4'he && (FIELD_A & 4'he) == 4'he && (FIELD_B & 4'h1 != FIELD_A & 4'h1)) ||
	  (FIELD_C == 4'h0         && FIELD_B == 4'h0         && (FIELD_A & 4'he) == 4'h2) ||
	  ((FIELD_D & 4'h8) == 0   && FIELD_C == 4'h0         && FIELD_B == 4'h0         && FIELD_A == 4'h0) || // <-- SWI insn.
	  (FIELD_D == 4'h8         && FIELD_C == 4'h0         && FIELD_B == 4'h0         && FIELD_A == 4'h0); // <-- STM insn.

	always_ff @ (posedge clk) begin
		insn_16_bit <= insn_16_bit_l;
		insn_32_bit <= insn_32_bit_l;
		insn_branch <= insn_branch_l;
	end
endmodule
