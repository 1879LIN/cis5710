`timescale 1ns / 1ns

// registers are 32 bits in RV32
`define REG_SIZE 31:0

// insns are 32 bits in RV32IM
`define INSN_SIZE 31:0

// RV opcodes are 7 bits
`define OPCODE_SIZE 6:0

// size of the wire to track insn name (6 bits)
`define INSN_NAME_SIZE 5:0

`ifndef RISCV_FORMAL
`include "../hw2b/cla.sv"
`include "../hw3-singlecycle/RvDisassembler.sv"
`include "../hw4-multicycle/divider_unsigned_pipelined.sv"
`endif

module Disasm #(
    byte PREFIX = "D"
) (
    input wire [31:0] insn,
    output wire [(8*32)-1:0] disasm
);
  // synthesis translate_off
  // this code is only for simulation, not synthesis
  string disasm_string;
  always_comb begin
    disasm_string = rv_disasm(insn);
  end
  // HACK: get disasm_string to appear in GtkWave, which can apparently show only wire/logic. Also,
  // string needs to be reversed to render correctly.
  genvar i;
  for (i = 3; i < 32; i = i + 1) begin : gen_disasm
    assign disasm[((i+1-3)*8)-1-:8] = disasm_string[31-i];
  end
  assign disasm[255-:8] = PREFIX;
  assign disasm[247-:8] = ":";
  assign disasm[239-:8] = " ";
  // synthesis translate_on
endmodule

module RegFile (
    input logic [4:0] rd,
    input logic [`REG_SIZE] rd_data,
    input logic [4:0] rs1,
    output logic [`REG_SIZE] rs1_data,
    input logic [4:0] rs2,
    output logic [`REG_SIZE] rs2_data,

    input logic clk,
    input logic we,
    input logic rst
);
  localparam int NumRegs = 32;
  logic [`REG_SIZE] regs[NumRegs];
  // TODO: copy your HW3B code here

    always_ff @(posedge clk) begin
    if (rst) begin
      // Reset all registers to 0
      integer i;
      for (i = 0; i < NumRegs; i = i + 1) begin
        regs[i] <= 0;
      end
    end else if (we) begin
      // Write data to register rd, if rd is not 0 (assuming register 0 is always 0)
      if (rd != 0) regs[rd] <= rd_data;
    end
  end

  // Continuous assignment for read ports
  assign regs[0] = 'b0;
  assign rs1_data = regs[rs1];
  assign rs2_data = regs[rs2];

endmodule

/**
 * This enum is used to classify each cycle as it comes through the Writeback stage, identifying
 * if a valid insn is present or, if it is a stall cycle instead, the reason for the stall. The
 * enum values are mutually exclusive: only one should be set for any given cycle. These values
 * are compared against the trace-*.json files to ensure that the datapath is running with the
 * correct timing.
 *
 * You will need to set these values at various places within your pipeline, and propagate them
 * through the stages until they reach Writeback where they can be checked.
 */
typedef enum {
  /** invalid value, this should never appear after the initial reset sequence completes */
  CYCLE_INVALID = 0,
  /** a stall cycle that arose from the initial reset signal */
  CYCLE_RESET = 1,
  /** not a stall cycle, a valid insn is in Writeback */
  CYCLE_NO_STALL = 2,
  /** a stall cycle that arose from a taken branch/jump */
  CYCLE_TAKEN_BRANCH = 4,

  // the values below are only needed in HW5B

  /** a stall cycle that arose from a load-to-use stall */
  CYCLE_LOAD2USE = 8,
  /** a stall cycle that arose from a div/rem-to-use stall */
  CYCLE_DIV2USE  = 16,
  /** a stall cycle that arose from a fence.i insn */
  CYCLE_FENCEI   = 32
} cycle_status_e;

/** state at the start of Decode stage */
typedef struct packed {
  logic [`REG_SIZE] pc;
  logic [`INSN_SIZE] insn;
  cycle_status_e cycle_status;
} stage_decode_t;

/** execute state **/
typedef struct packed {
  logic [`REG_SIZE] pc;
  //logic [`REG_SIZE]pc_next;
  logic [`INSN_SIZE] insn;
  cycle_status_e cycle_status;

  logic [4:0] rs1;
  logic [4:0] rs2;
  logic [4:0] rd;
  logic [19:0] imm_u;
  logic [11:0] imm_i;
  logic [`REG_SIZE] imm_i_sext;
  logic [`REG_SIZE] imm_b_sext;
  logic [`INSN_NAME_SIZE] insn_exe;
} stage_execute_t;

/** memory state **/
typedef struct packed {
  logic [`REG_SIZE] pc;
  logic [`INSN_SIZE] insn;
  cycle_status_e cycle_status;

  logic [4:0] rd;
  logic [`REG_SIZE] rd_data;

  logic we, halt;
} stage_memory_t;

/** writeback state **/
typedef struct packed {
  logic [`REG_SIZE] pc;
  logic [`INSN_SIZE] insn;
  cycle_status_e cycle_status;

  logic [4:0] rd;
  logic [`REG_SIZE] rd_data;

  logic we,halt;
} stage_writeback_t;


module DatapathPipelined (
    input wire clk,
    input wire rst,
    output logic [`REG_SIZE] pc_to_imem,
    input wire [`INSN_SIZE] insn_from_imem,

    output logic [`REG_SIZE] addr_to_dmem,
    input wire [`REG_SIZE] load_data_from_dmem,
    output logic [`REG_SIZE] store_data_to_dmem,
    output logic [3:0] store_we_to_dmem,

    output logic halt,

    // The PC of the insn currently in Writeback. 0 if not a valid insn.
    output logic [`REG_SIZE] trace_writeback_pc,
    // The bits of the insn currently in Writeback. 0 if not a valid insn.
    output logic [`INSN_SIZE] trace_writeback_insn,
    // The status of the insn (or stall) currently in Writeback. See cycle_status_e enum for valid values.
    output cycle_status_e trace_writeback_cycle_status
);

  /***********/
  /* MODULES */
  /***********/

  RegFile rf (
      .rd(writeback_state.rd),
      .rd_data(w_rd_data),
      .rs1(execute_state.rs1),
      .rs1_data(x_rs1_data),
      .rs2(execute_state.rs2),
      .rs2_data(x_rs2_data),
      .clk(clk),
      .we (write_we),
      .rst(rst)
  );

  
  localparam bit [`OPCODE_SIZE] OpLoad = 7'b00_000_11;
  localparam bit [`OPCODE_SIZE] OpStore = 7'b01_000_11;
  localparam bit [`OPCODE_SIZE] OpBranch = 7'b11_000_11;
  localparam bit [`OPCODE_SIZE] OpJalr = 7'b11_001_11;
  localparam bit [`OPCODE_SIZE] OpMiscMem = 7'b00_011_11;
  localparam bit [`OPCODE_SIZE] OpJal = 7'b11_011_11;

  localparam bit [`OPCODE_SIZE] OpRegImm = 7'b00_100_11;
  localparam bit [`OPCODE_SIZE] OpRegReg = 7'b01_100_11;
  localparam bit [`OPCODE_SIZE] OpEnviron = 7'b11_100_11;

  localparam bit [`OPCODE_SIZE] OpAuipc = 7'b00_101_11;
  localparam bit [`OPCODE_SIZE] OpLui = 7'b01_101_11;


  logic [`REG_SIZE] cycles_current;
  always_ff @(posedge clk) begin
    if (rst) begin
      cycles_current <= 0;
    end else begin
      cycles_current <= cycles_current + 1;
    end
  end

  /***************/
  /* FETCH STAGE */
  /***************/

  logic [`REG_SIZE] f_pc_current;
  wire [`REG_SIZE] f_insn;
  cycle_status_e f_cycle_status;

  // program counter
  always_ff @(posedge clk) begin
    if (rst) begin
      f_pc_current   <= 32'd0;
      // NB: use CYCLE_NO_STALL since this is the value that will persist after the last reset cycle
      f_cycle_status <= CYCLE_NO_STALL;
    end 
    else if (branch_flag) begin
      f_cycle_status <= CYCLE_NO_STALL;
      f_pc_current   <= branch_pc;
      end
    else 
    begin
      f_cycle_status <= CYCLE_NO_STALL;
      f_pc_current   <= f_pc_current + 4;
    end
  end
  // send PC to imem
  assign pc_to_imem = f_pc_current;
  assign f_insn = insn_from_imem;

  
  wire [255:0] f_disasm;
  Disasm #(
      .PREFIX("F")
  ) disasm_0fetch (
      .insn  (f_insn),
      .disasm(f_disasm)
  );

  /****************/
  /* DECODE STAGE */
  /****************/

  // this shows how to package up state in a `struct packed`, and how to pass it between stages
  stage_decode_t decode_state;
  always_ff @(posedge clk) begin
    if (rst) begin
      decode_state <= '{pc: 0,
      insn: 0, 
      cycle_status: CYCLE_RESET};
    end 
    else if (branch_flag) begin
      decode_state <= '{pc: 0, 
      insn: 0, 
      cycle_status: CYCLE_TAKEN_BRANCH};
    end
    else begin
      begin
        decode_state <= '{pc: f_pc_current, 
        insn: f_insn, 
        cycle_status: f_cycle_status};
      end
    end
  end
  wire [255:0] d_disasm;
  Disasm #(
      .PREFIX("D")
  ) disasm_1decode (
      .insn  (decode_state.insn),
      .disasm(d_disasm)
  );

  // components of the instruction
  wire [6:0] d_insn_funct7;
  wire [2:0] d_insn_funct3;
  wire [4:0] d_insn_rs1;
  wire [4:0] d_insn_rs2;
  wire [4:0] d_insn_rd;
  wire [`OPCODE_SIZE] d_insn_opcode;

  // split R-type instruction - see section 2.2 of RiscV spec
  assign {d_insn_funct7, d_insn_rs2, d_insn_rs1, d_insn_funct3, d_insn_rd, d_insn_opcode} = decode_state.insn;

  // setup for I, S, B & J type instructions
  // I - short immediates and loads
  wire [11:0] d_imm_i;
  assign d_imm_i = decode_state.insn[31:20];

  // S - stores
  wire [11:0] d_imm_s;
  assign d_imm_s[11:5] = d_insn_funct7, d_imm_s[4:0] = d_insn_rd;

  // B - conditionals
  wire [12:0] d_imm_b;
  
  assign {d_imm_b[12], d_imm_b[10:5]} = d_insn_funct7,{d_imm_b[4:1], d_imm_b[11]} = d_insn_rd,d_imm_b[0] = 1'b0;

  // J - unconditional jumps
  wire [20:0] d_imm_j;
  assign {d_imm_j[20], d_imm_j[10:1], d_imm_j[11], d_imm_j[19:12], d_imm_j[0]} = {decode_state.insn[31:12], 1'b0};

  // U - setup for U type instructions
  wire [19:0] d_imm_u;
  assign d_imm_u = decode_state.insn[31:12];

  wire [`REG_SIZE] d_imm_i_sext = {{20{d_imm_i[11]}}, d_imm_i[11:0]};
  wire [`REG_SIZE] d_imm_s_sext = {{20{d_imm_s[11]}}, d_imm_s[11:0]};
  wire [`REG_SIZE] d_imm_b_sext = {{19{d_imm_b[12]}}, d_imm_b[12:0]};
  wire [`REG_SIZE] d_imm_j_sext = {{11{d_imm_j[20]}}, d_imm_j[20:0]};

  wire d_insn_lui = d_insn_opcode == OpLui;
  wire d_insn_auipc = d_insn_opcode == OpAuipc;
  wire d_insn_jal = d_insn_opcode == OpJal;
  wire d_insn_jalr = d_insn_opcode == OpJalr;

  wire d_insn_beq = d_insn_opcode == OpBranch && decode_state.insn[14:12] == 3'b000;
  wire d_insn_bne = d_insn_opcode == OpBranch && decode_state.insn[14:12] == 3'b001;
  wire d_insn_blt = d_insn_opcode == OpBranch && decode_state.insn[14:12] == 3'b100;
  wire d_insn_bge = d_insn_opcode == OpBranch && decode_state.insn[14:12] == 3'b101;
  wire d_insn_bltu = d_insn_opcode == OpBranch && decode_state.insn[14:12] == 3'b110;
  wire d_insn_bgeu = d_insn_opcode == OpBranch && decode_state.insn[14:12] == 3'b111;

  wire d_insn_lb = d_insn_opcode == OpLoad && decode_state.insn[14:12] == 3'b000;
  wire d_insn_lh = d_insn_opcode == OpLoad && decode_state.insn[14:12] == 3'b001;
  wire d_insn_lw = d_insn_opcode == OpLoad && decode_state.insn[14:12] == 3'b010;
  wire d_insn_lbu = d_insn_opcode == OpLoad && decode_state.insn[14:12] == 3'b100;
  wire d_insn_lhu = d_insn_opcode == OpLoad && decode_state.insn[14:12] == 3'b101;

  wire d_insn_sb = d_insn_opcode == OpStore && decode_state.insn[14:12] == 3'b000;
  wire d_insn_sh = d_insn_opcode == OpStore && decode_state.insn[14:12] == 3'b001;
  wire d_insn_sw = d_insn_opcode == OpStore && decode_state.insn[14:12] == 3'b010;

  wire d_insn_addi = d_insn_opcode == OpRegImm && decode_state.insn[14:12] == 3'b000;
  wire d_insn_slti = d_insn_opcode == OpRegImm && decode_state.insn[14:12] == 3'b010;
  wire d_insn_sltiu = d_insn_opcode == OpRegImm && decode_state.insn[14:12] == 3'b011;
  wire d_insn_xori = d_insn_opcode == OpRegImm && decode_state.insn[14:12] == 3'b100;
  wire d_insn_ori = d_insn_opcode == OpRegImm && decode_state.insn[14:12] == 3'b110;
  wire d_insn_andi = d_insn_opcode == OpRegImm && decode_state.insn[14:12] == 3'b111;

  wire d_insn_slli = d_insn_opcode == OpRegImm && decode_state.insn[14:12] == 3'b001 && decode_state.insn[31:25] == 7'd0;
  wire d_insn_srli = d_insn_opcode == OpRegImm && decode_state.insn[14:12] == 3'b101 && decode_state.insn[31:25] == 7'd0;
  wire d_insn_srai = d_insn_opcode == OpRegImm && decode_state.insn[14:12] == 3'b101 && decode_state.insn[31:25] == 7'b0100000;

  wire d_insn_add = d_insn_opcode == OpRegReg && decode_state.insn[14:12] == 3'b000 && decode_state.insn[31:25] == 7'd0;
  wire d_insn_sub  = d_insn_opcode == OpRegReg && decode_state.insn[14:12] == 3'b000 && decode_state.insn[31:25] == 7'b0100000;
  wire d_insn_sll = d_insn_opcode == OpRegReg && decode_state.insn[14:12] == 3'b001 && decode_state.insn[31:25] == 7'd0;
  wire d_insn_slt = d_insn_opcode == OpRegReg && decode_state.insn[14:12] == 3'b010 && decode_state.insn[31:25] == 7'd0;
  wire d_insn_sltu = d_insn_opcode == OpRegReg && decode_state.insn[14:12] == 3'b011 && decode_state.insn[31:25] == 7'd0;
  wire d_insn_xor = d_insn_opcode == OpRegReg && decode_state.insn[14:12] == 3'b100 && decode_state.insn[31:25] == 7'd0;
  wire d_insn_srl = d_insn_opcode == OpRegReg && decode_state.insn[14:12] == 3'b101 && decode_state.insn[31:25] == 7'd0;
  wire d_insn_sra  = d_insn_opcode == OpRegReg && decode_state.insn[14:12] == 3'b101 && decode_state.insn[31:25] == 7'b0100000;
  wire d_insn_or = d_insn_opcode == OpRegReg && decode_state.insn[14:12] == 3'b110 && decode_state.insn[31:25] == 7'd0;
  wire d_insn_and = d_insn_opcode == OpRegReg && decode_state.insn[14:12] == 3'b111 && decode_state.insn[31:25] == 7'd0;

  wire d_insn_mul    = d_insn_opcode == OpRegReg && decode_state.insn[31:25] == 7'd1 && decode_state.insn[14:12] == 3'b000;
  wire d_insn_mulh   = d_insn_opcode == OpRegReg && decode_state.insn[31:25] == 7'd1 && decode_state.insn[14:12] == 3'b001;
  wire d_insn_mulhsu = d_insn_opcode == OpRegReg && decode_state.insn[31:25] == 7'd1 && decode_state.insn[14:12] == 3'b010;
  wire d_insn_mulhu  = d_insn_opcode == OpRegReg && decode_state.insn[31:25] == 7'd1 && decode_state.insn[14:12] == 3'b011;
  wire d_insn_div    = d_insn_opcode == OpRegReg && decode_state.insn[31:25] == 7'd1 && decode_state.insn[14:12] == 3'b100;
  wire d_insn_divu   = d_insn_opcode == OpRegReg && decode_state.insn[31:25] == 7'd1 && decode_state.insn[14:12] == 3'b101;
  wire d_insn_rem    = d_insn_opcode == OpRegReg && decode_state.insn[31:25] == 7'd1 && decode_state.insn[14:12] == 3'b110;
  wire d_insn_remu   = d_insn_opcode == OpRegReg && decode_state.insn[31:25] == 7'd1 && decode_state.insn[14:12] == 3'b111;

  wire d_insn_ecall = d_insn_opcode == OpEnviron && decode_state.insn[31:7] == 25'd0;
  wire d_insn_fence = d_insn_opcode == OpMiscMem;

  logic [5:0] d_insn_exe;
  localparam bit [5:0] InsnLui = 6'd1, InsnAuipc = 6'd2, InsnJal = 6'd3, InsnJalr = 6'd4, InsnBeq = 6'd5, 
  InsnBne = 6'd6, InsnBlt = 6'd7,InsnBge = 6'd8, InsnBltu = 6'd9, InsnBgeu = 6'd10, InsnLb = 6'd11, InsnLh = 6'd12, InsnLw = 6'd13,
  InsnLbu = 6'd14, InsnLhu = 6'd15, InsnSb = 6'd16, InsnSh = 6'd17, InsnSw = 6'd18, InsnAddi = 6'd19, InsnSlti = 6'd20,
  InsnSltiu = 6'd21, InsnXori = 6'd22, InsnOri = 6'd23, InsnAndi = 6'd24, InsnSlli = 6'd25, InsnSrli = 6'd26, InsnSrai = 6'd27,
  InsnAdd = 6'd28, InsnSub = 6'd29, InsnSll = 6'd30, InsnSlt = 6'd31, InsnSltu = 6'd32, InsnXor = 6'd33, InsnSrl = 6'd34,
  InsnSra = 6'd35, InsnOr = 6'd36, InsnAnd = 6'd37, InsnMul = 6'd38, InsnMulh = 6'd39, InsnMulhsu = 6'd40, InsnMulhu = 6'd41,
  InsnDiv = 6'd42, InsnDivu = 6'd43, InsnRem = 6'd44, InsnRemu = 6'd45, InsnEcall = 6'd46, InsnFence = 6'd47;

  always @(*) begin
    if (d_insn_lui) begin
        d_insn_exe = InsnLui;
    end else if (d_insn_auipc) begin
        d_insn_exe = InsnAuipc;
    end else if (d_insn_jal) begin
        d_insn_exe = InsnJal;
    end else if (d_insn_jalr) begin
        d_insn_exe = InsnJalr;
    end else if (d_insn_beq) begin
        d_insn_exe = InsnBeq;
    end else if (d_insn_bne) begin
        d_insn_exe = InsnBne;
    end else if (d_insn_blt) begin
        d_insn_exe = InsnBlt;
    end else if (d_insn_bge) begin
        d_insn_exe = InsnBge;
    end else if (d_insn_bltu) begin
        d_insn_exe = InsnBltu;
    end else if (d_insn_bgeu) begin
        d_insn_exe = InsnBgeu;
    end else if (d_insn_lb) begin
        d_insn_exe = InsnLb;
    end else if (d_insn_lh) begin
        d_insn_exe = InsnLh;
    end else if (d_insn_lw) begin
        d_insn_exe = InsnLw;
    end else if (d_insn_lbu) begin
        d_insn_exe = InsnLbu;
    end else if (d_insn_lhu) begin
        d_insn_exe = InsnLhu;
    end else if (d_insn_sb) begin
        d_insn_exe = InsnSb;
    end else if (d_insn_sh) begin
        d_insn_exe = InsnSh;
    end else if (d_insn_sw) begin
        d_insn_exe = InsnSw;
    end else if (d_insn_addi) begin
        d_insn_exe = InsnAddi;
    end else if (d_insn_slti) begin
        d_insn_exe = InsnSlti;
    end else if (d_insn_sltiu) begin
        d_insn_exe = InsnSltiu;
    end else if (d_insn_xori) begin
        d_insn_exe = InsnXori;
    end else if (d_insn_ori) begin
        d_insn_exe = InsnOri;
    end else if (d_insn_andi) begin
        d_insn_exe = InsnAndi;
    end else if (d_insn_slli) begin
        d_insn_exe = InsnSlli;
    end else if (d_insn_srli) begin
        d_insn_exe = InsnSrli;
    end else if (d_insn_srai) begin
        d_insn_exe = InsnSrai;
    end else if (d_insn_add) begin
        d_insn_exe = InsnAdd;
    end else if (d_insn_sub) begin
        d_insn_exe = InsnSub;
    end else if (d_insn_sll) begin
        d_insn_exe = InsnSll;
    end else if (d_insn_slt) begin
        d_insn_exe = InsnSlt;
    end else if (d_insn_sltu) begin
        d_insn_exe = InsnSltu;
    end else if (d_insn_xor) begin
        d_insn_exe = InsnXor;
    end else if (d_insn_srl) begin
        d_insn_exe = InsnSrl;
    end else if (d_insn_sra) begin
        d_insn_exe = InsnSra;
    end else if (d_insn_or) begin
        d_insn_exe = InsnOr;
    end  else if (d_insn_and) begin
        d_insn_exe = InsnAnd;
    end else if (d_insn_mul) begin
        d_insn_exe = InsnMul;
    end else if (d_insn_mulh) begin
        d_insn_exe = InsnMulh;
    end else if (d_insn_mulhsu) begin
        d_insn_exe = InsnMulhsu;
    end else if (d_insn_mulhu) begin
        d_insn_exe = InsnMulhu;
    end else if (d_insn_div) begin
        d_insn_exe = InsnDiv;
    end else if (d_insn_divu) begin
        d_insn_exe = InsnDivu;
    end else if (d_insn_rem) begin
        d_insn_exe = InsnRem;
    end else if (d_insn_remu) begin
        d_insn_exe = InsnRemu;
    end else if (d_insn_ecall) begin
        d_insn_exe = InsnEcall;
    end else if (d_insn_fence) begin
        d_insn_exe = InsnFence;
    end else begin
        d_insn_exe = 6'b0;
    end
   end


  /* EXECUTE STAGE */


  stage_execute_t execute_state;
  always_ff @(posedge clk) begin
    if (rst) begin
      execute_state <= '{
          pc: 0,
          cycle_status: CYCLE_RESET,
          rs1: 0,
          rs2: 0,
          rd: 0,
          insn: 0,
          imm_u: 0,
          imm_i : 0,
          imm_i_sext: 0,
          imm_b_sext:0,
          insn_exe: 0
      };
    end 
    else if (branch_flag) begin
      execute_state <= '{
          rd: 0,
          imm_i : 0,
          imm_u: 0,
          imm_i_sext: 0,
          imm_b_sext:0,
          insn_exe: 0,
          pc: 0,
          insn: 0,
          cycle_status: CYCLE_TAKEN_BRANCH,
          rs1: 0,
          rs2: 0
          
      };
    end
    else begin
        execute_state <= '{
            pc: decode_state.pc,
            insn: decode_state.insn,
            cycle_status: decode_state.cycle_status,

            rs1: d_insn_rs1,
            rs2: d_insn_rs2,
            rd: d_insn_rd,
            
            imm_u: d_imm_u,
            imm_i : d_imm_i,
            imm_i_sext: d_imm_i_sext,
            imm_b_sext: d_imm_b_sext,
            insn_exe: d_insn_exe
        };
    end
  end
  wire [255:0] x_disasm;
  Disasm #(
      .PREFIX("X")
  ) disasm_2execute (
      .insn  (execute_state.insn),
      .disasm(x_disasm)
  );

  // TODO: your code here, though you will also need to modify some of the code above
  // TODO: the testbench requires that your register file instance is named `rf`

  // Module

  logic [4:0] x_rs1, x_rd;
  logic [`REG_SIZE] x_rd_data_inter;
  logic [`REG_SIZE] x_rd_data, x_rs1_data, x_rs2_data;
  logic x_we;
  logic [63:0] mul_1,mul_2,mul_3,mul_4;

  logic [`REG_SIZE] d1,d2;
  logic branch_flag,x_halt;
  logic [`REG_SIZE] branch_pc;



  logic [11:0] imm_i_x;
  // data enable signal
  assign imm_i_x = execute_state.insn[31:20];
  wire x_rs1_en = (execute_state.insn[6:0]==OpRegImm) || (execute_state.insn[6:0]==OpRegReg) || (execute_state.insn[6:0]==OpStore) || (execute_state.insn[6:0]==OpBranch);
  wire x_rs2_en = (execute_state.insn[6:0]==OpRegReg) || (execute_state.insn[6:0]==OpStore) || (execute_state.insn[6:0]==OpBranch);

  // comb logic
  always_comb begin
    x_rd = 0;
    x_rd_data = 0;
    x_we = 0;

    x_halt = 0;
  

    mul_1 = 0;
    mul_2 = 0;
    mul_3 = 0;
    mul_4 = 0;
    x_halt = 0;
    branch_flag = 0;
    branch_pc = 32'b0;

    d1 = x_rs1_data;
    d2 = x_rs2_data;
  // MX, WX Bypass
    if((memory_state.insn[11:7] == execute_state.insn[19:15]) && memory_state.insn[11:7] != 0 && execute_state.insn[19:15]!=0 &&  x_rs1_en && mem_en) begin 
      d1 = memory_state.rd_data;
    end

    if((memory_state.insn[11:7] == execute_state.insn[24:20]) && memory_state.insn[11:7] != 0 && execute_state.insn[24:20]!=0 &&  x_rs2_en && mem_en) begin 
      d2 = memory_state.rd_data;
    end

    if((writeback_state.insn[11:7] == execute_state.insn[19:15]) && writeback_state.insn[11:7] != 0 && execute_state.insn[19:15]!=0 &&  x_rs1_en && write_en) begin 
      d1 = writeback_state.rd_data;
    end

    if((writeback_state.insn[11:7] == execute_state.insn[24:20]) && writeback_state.insn[11:7] != 0 && execute_state.insn[24:20]!=0 &&  x_rs2_en && write_en) begin 
      d2 = writeback_state.rd_data;
    end

    

    case (execute_state.insn_exe)
    //case  (execute_state.insn[6:0])
      
      InsnLui: begin
        x_rd = execute_state.rd;
        x_rd_data = {execute_state.imm_u, 12'b0};
        x_we = 1;
      end

      InsnAddi: begin
        x_rd_data = d1+ execute_state.imm_i_sext;
        x_rd = execute_state.rd;
        x_we = 1;
      end

      InsnSlti: begin
        x_rd = execute_state.rd;
        x_rd_data = $signed(d1) < $signed(execute_state.imm_i_sext) ? 32'h00000001 : 32'h00000000;
        x_we = 1;
      end

      InsnSltiu: begin
        x_rd = execute_state.rd;
        x_rd_data = $unsigned(d1) < $unsigned(execute_state.imm_i_sext) ? 32'h00000001 : 32'h00000000;
        x_we = 1;
      end

      InsnXori: begin
        x_rd = execute_state.rd;
        x_rd_data = d1 ^ execute_state.imm_i_sext;
        x_we = 1;
      end

      InsnOri: begin
        x_rd = execute_state.rd;
        x_rd_data = d1 | execute_state.imm_i_sext;
        x_we = 1;
      end

      InsnAndi: begin
        x_rd = execute_state.rd;
        x_rd_data = d1 & execute_state.imm_i_sext;
        x_we = 1;
      end

      InsnSlli: begin
        x_rd = execute_state.rd;
        x_rd_data = d1 << execute_state.imm_i[4:0];
        x_we = 1;
      end

      InsnSrli: begin
        x_rd = execute_state.rd;
        x_rd_data = d1 >> execute_state.imm_i[4:0];
        x_we = 1;
      end

      InsnSrai: begin
        x_rd = execute_state.rd;
        x_rd_data = $signed(d1) >>> execute_state.imm_i[4:0];
        x_we = 1;
      end

      InsnAdd: begin
        x_rd_data = d1+d2;

        x_rd = execute_state.rd;
        x_we = 1;
      end

      InsnSub: begin
      
        x_rd_data = d1 - d2;
        x_rd = execute_state.rd;

        x_we = 1;
      end

      InsnSll: begin 
        x_rd_data  = d1 << d2[4:0];
        x_rd = execute_state.rd;

      end

      InsnSlt: begin
        x_rd = execute_state.rd;
        x_rd_data = $signed(d1) < $signed(d2) ? 32'h00000001 : 32'h00000000;
        x_we = 1;
      end

      InsnSltu: begin
        x_rd = execute_state.rd;
        x_rd_data = $unsigned(d1) < $unsigned(d2) ? 32'h00000001 : 32'h00000000;
        x_we = 1;
      end

      InsnXor: begin
        x_rd = execute_state.rd;
        x_rd_data = d1 ^ d2;
        x_we = 1;
      end

      InsnSrl: begin

        x_rd_data  = d1  >> d2[4:0];
        x_rd = execute_state.rd;

      end

      InsnSra: begin
        x_rd_data = $signed(d1) >>> d2[4:0];
        x_rd = execute_state.rd;

      end
      

      InsnOr: begin
        x_rd = execute_state.rd;
        x_rd_data = d1 | d2;
        x_we = 1;
      end

      InsnAnd: begin
        x_rd = execute_state.rd;
        x_rd_data = d1 & d2;
        x_we = 1;
      end


      InsnBeq: begin

        if(d1==d2) begin 
          branch_pc = execute_state.pc + execute_state.imm_b_sext;
          branch_flag = 1;
        end
      end

      InsnBne: begin 

        if(d1!=d2) begin 
          branch_pc = execute_state.pc + execute_state.imm_b_sext;
          branch_flag = 1;


        end
      end

      InsnBlt: begin 
 
        if($signed(d1)<$signed(d2)) begin 
          branch_pc = execute_state.pc + execute_state.imm_b_sext;
          branch_flag = 1;
        end
      end

      InsnBge: begin 

        if($signed(d1)>=$signed(d2)) begin 
          branch_pc = execute_state.pc + execute_state.imm_b_sext;
          branch_flag = 1;
        end
      end

      InsnBltu: begin 
  
        if(d1<$unsigned(d2)) begin 
          branch_pc = execute_state.pc + execute_state.imm_b_sext;
          branch_flag = 1;
        end
      end

      InsnBgeu: begin 
        if(d1>=$unsigned(d2)) begin 
          branch_pc = execute_state.pc + execute_state.imm_b_sext;
          branch_flag = 1;
        end
      end



      InsnMul: begin
          x_rd = execute_state.rd;

          mul_1 = (d1 * d2);
          x_rd_data = mul_1[31:0];
          x_we = 1'b1;
      end

      InsnMulh: begin
          x_rd = execute_state.rd;
          mul_2 = {{32{d1[31]}}, d1} * {{32{d2[31]}}, d2};
          x_rd_data = mul_2[63:32];
          x_we = 1'b1;
      end

      InsnMulhsu: begin
          x_rd = execute_state.rd;

          mul_3 = {{32{d1[31]}}, d1} * {32'b0, d2};
          x_rd_data = mul_3[63:32];
              
          x_we = 1'b1;
        end

      InsnMulhu: begin
          x_rd = execute_state.rd;

          mul_4 = ($unsigned(d1) * $unsigned(d2));
          x_rd_data = mul_4[63:32];
          x_we = 1'b1;

        end

      InsnEcall: begin 
        x_halt = 1;
      end

      InsnFence: begin

      end

      default: begin

      end
    endcase
  end

  /* MEMORY STAGE */

  stage_memory_t memory_state;
  always_ff @(posedge clk) begin
    if (rst) begin
      memory_state <= '{pc: 0, 
      insn: 0, 
      cycle_status: CYCLE_RESET, 
      rd: 0, 
      halt : 0,
      rd_data: 0, 
      we: 0};
    end else begin
      begin
        memory_state <= '{
            pc: execute_state.pc,
            insn: execute_state.insn,
            cycle_status: execute_state.cycle_status,

            rd: x_rd,
            rd_data: x_rd_data,
            halt : x_halt,
            we: x_we
        };
      end
    end
  end
  // Memory enable signal for MX bypass
  wire mem_en = (memory_state.insn[6:0]==OpRegImm) || (memory_state.insn[6:0]==OpRegReg) || (memory_state.insn[6:0]==OpLui) || (memory_state.insn[6:0]==OpJal);
  wire [255:0] m_disasm;
  Disasm #(
      .PREFIX("M")
  ) disasm_3memory (
      .insn  (execute_state.insn),
      .disasm(m_disasm)
  );
 
 
 
  /* WRITEBACK STAGE */

  stage_writeback_t writeback_state;
  always_ff @(posedge clk) begin
    if (rst) begin
      writeback_state <= '{
          pc: 0,
          insn: 0,
          cycle_status: CYCLE_RESET,
          rd: 0,
          rd_data: 0,
          we: 0,
          halt : 0
      };
    end else 
    begin
      begin
        writeback_state <= '{
            pc: memory_state.pc,
            insn: memory_state.insn,
            cycle_status: memory_state.cycle_status,
            rd: memory_state.rd,
            rd_data: memory_state.rd_data,
            we: memory_state.we,
            halt: memory_state.halt
        };
      end
    end
  end
  // written enable signal for WX bypassing.
  wire write_en = (writeback_state.insn[6:0]==OpRegImm) || (writeback_state.insn[6:0]==OpRegReg) || (writeback_state.insn[6:0]==OpLui) || (writeback_state.insn[6:0]==OpJal);

  wire [255:0] w_disasm;
  Disasm #(
      .PREFIX("W")
  ) disasm_4writeback (
      .insn  (memory_state.insn),
      .disasm(w_disasm)
  );

  logic write_we;
  logic [`REG_SIZE] w_rd_data;

  always_comb begin
    if (writeback_state.we == 1) begin
      w_rd_data = writeback_state.rd_data;
      write_we = writeback_state.we;
    end else begin
      w_rd_data = 0;
      write_we = 0;
    end
  end
  
  //assign halt = writeback_state.halt;

  assign halt = (memory_state.insn[6:0] == 7'h73) & (memory_state.insn[31:7] == 'b0);

  assign trace_writeback_pc = writeback_state.pc;
  assign trace_writeback_insn = writeback_state.insn;
  assign trace_writeback_cycle_status = writeback_state.cycle_status;

endmodule

module MemorySingleCycle #(
    parameter int NUM_WORDS = 512
) (
    // rst for both imem and dmem
    input wire rst,

    // clock for both imem and dmem. The memory reads/writes on @(negedge clk)
    input wire clk,

    // must always be aligned to a 4B boundary
    input wire [`REG_SIZE] pc_to_imem,

    // the value at memory location pc_to_imem
    output logic [`REG_SIZE] insn_from_imem,

    // must always be aligned to a 4B boundary
    input wire [`REG_SIZE] addr_to_dmem,

    // the value at memory location addr_to_dmem
    output logic [`REG_SIZE] load_data_from_dmem,

    // the value to be written to addr_to_dmem, controlled by store_we_to_dmem
    input wire [`REG_SIZE] store_data_to_dmem,

    // Each bit determines whether to write the corresponding byte of store_data_to_dmem to memory location addr_to_dmem.
    // E.g., 4'b1111 will write 4 bytes. 4'b0001 will write only the least-significant byte.
    input wire [3:0] store_we_to_dmem
);

  // memory is arranged as an array of 4B words
  logic [`REG_SIZE] mem[NUM_WORDS];

  initial begin
    $readmemh("mem_initial_contents.hex", mem, 0);
  end

  always_comb begin
    // memory addresses should always be 4B-aligned
    assert (pc_to_imem[1:0] == 2'b00);
    assert (addr_to_dmem[1:0] == 2'b00);
  end

  localparam int AddrMsb = $clog2(NUM_WORDS) + 1;
  localparam int AddrLsb = 2;

  always @(negedge clk) begin
    if (rst) begin
    end else begin
      insn_from_imem <= mem[{pc_to_imem[AddrMsb:AddrLsb]}];
    end
  end

  always @(negedge clk) begin
    if (rst) begin
    end else begin
      if (store_we_to_dmem[0]) begin
        mem[addr_to_dmem[AddrMsb:AddrLsb]][7:0] <= store_data_to_dmem[7:0];
      end
      if (store_we_to_dmem[1]) begin
        mem[addr_to_dmem[AddrMsb:AddrLsb]][15:8] <= store_data_to_dmem[15:8];
      end
      if (store_we_to_dmem[2]) begin
        mem[addr_to_dmem[AddrMsb:AddrLsb]][23:16] <= store_data_to_dmem[23:16];
      end
      if (store_we_to_dmem[3]) begin
        mem[addr_to_dmem[AddrMsb:AddrLsb]][31:24] <= store_data_to_dmem[31:24];
      end
      // dmem is "read-first": read returns value before the write
      load_data_from_dmem <= mem[{addr_to_dmem[AddrMsb:AddrLsb]}];
    end
  end
endmodule

/* This design has just one clock for both processor and memory. */
module RiscvProcessor (
    input wire clk,
    input wire rst,
    output logic halt,
    output wire [`REG_SIZE] trace_writeback_pc,
    output wire [`INSN_SIZE] trace_writeback_insn,
    output cycle_status_e trace_writeback_cycle_status
);

  wire [`INSN_SIZE] insn_from_imem;
  wire [`REG_SIZE] pc_to_imem, mem_data_addr, mem_data_loaded_value, mem_data_to_write;
  wire [3:0] mem_data_we;

  MemorySingleCycle #(
      .NUM_WORDS(8192)
  ) the_mem (
      .rst                (rst),
      .clk                (clk),
      // imem is read-only
      .pc_to_imem         (pc_to_imem),
      .insn_from_imem     (insn_from_imem),
      // dmem is read-write
      .addr_to_dmem       (mem_data_addr),
      .load_data_from_dmem(mem_data_loaded_value),
      .store_data_to_dmem (mem_data_to_write),
      .store_we_to_dmem   (mem_data_we)
  );

  DatapathPipelined datapath (
      .clk(clk),
      .rst(rst),
      .pc_to_imem(pc_to_imem),
      .insn_from_imem(insn_from_imem),
      .addr_to_dmem(mem_data_addr),
      .store_data_to_dmem(mem_data_to_write),
      .store_we_to_dmem(mem_data_we),
      .load_data_from_dmem(mem_data_loaded_value),
      .halt(halt),
      .trace_writeback_pc(trace_writeback_pc),
      .trace_writeback_insn(trace_writeback_insn),
      .trace_writeback_cycle_status(trace_writeback_cycle_status)
  );

endmodule

