`timescale 1ns / 1ns
//Final version


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

/** NB: ARESETn is active-low, i.e., reset when this input is ZERO */
interface axi_clkrst_if (
    input wire ACLK,
    input wire ARESETn
);
endinterface

interface axi_if #(
      parameter int ADDR_WIDTH = 32
    , parameter int DATA_WIDTH = 32
);
  logic                      ARVALID;
  logic                      ARREADY;
  logic [    ADDR_WIDTH-1:0] ARADDR;
  logic [               2:0] ARPROT;

  logic                      RVALID;
  logic                      RREADY;
  logic [    DATA_WIDTH-1:0] RDATA;
  logic [               1:0] RRESP;

  logic                      AWVALID;
  logic                      AWREADY;
  logic [    ADDR_WIDTH-1:0] AWADDR;
  logic [               2:0] AWPROT;

  logic                      WVALID;
  logic                      WREADY;
  logic [    DATA_WIDTH-1:0] WDATA;
  logic [(DATA_WIDTH/8)-1:0] WSTRB;

  logic                      BVALID;
  logic                      BREADY;
  logic [               1:0] BRESP;

  modport manager(
      input ARREADY, RVALID, RDATA, RRESP, AWREADY, WREADY, BVALID, BRESP,
      output ARVALID, ARADDR, ARPROT, RREADY, AWVALID, AWADDR, AWPROT, WVALID, WDATA, WSTRB, BREADY
  );
  modport subord(
      input ARVALID, ARADDR, ARPROT, RREADY, AWVALID, AWADDR, AWPROT, WVALID, WDATA, WSTRB, BREADY,
      output ARREADY, RVALID, RDATA, RRESP, AWREADY, WREADY, BVALID, BRESP
  );
endinterface


module MemoryAxiLite #(
    parameter int NUM_WORDS  = 32,
    // parameter int ADDR_WIDTH = 32,
    parameter int DATA_WIDTH = 32
) (
    axi_clkrst_if axi,
    axi_if.subord insn,
    axi_if.subord data
);

  // memory is an array of 4B words
  logic [DATA_WIDTH-1:0] mem_array[NUM_WORDS];
  localparam int AddrMsb = $clog2(NUM_WORDS) + 1;
  localparam int AddrLsb = 2;

  // [BR]RESP codes, from Section A 3.4.4 of AXI4 spec
  localparam bit [1:0] ResponseOkay = 2'b00;
  // localparam bit [1:0] ResponseSubordinateError = 2'b10;
  // localparam bit [1:0] ResponseDecodeError = 2'b11;

`ifndef FORMAL
  always_comb begin
    // memory addresses should always be 4B-aligned
    assert (!insn.ARVALID || insn.ARADDR[1:0] == 2'b00);
    assert (!data.ARVALID || data.ARADDR[1:0] == 2'b00);
    assert (!data.AWVALID || data.AWADDR[1:0] == 2'b00);
    // we don't use the protection bits
    assert (insn.ARPROT == 3'd0);
    assert (data.ARPROT == 3'd0);
    assert (data.AWPROT == 3'd0);
  end
`endif

  // TODO: changes will be needed throughout this module

always_ff @(posedge axi.ACLK) begin
    if (!axi.ARESETn) begin
      // start out ready to accept incoming reads
      insn.ARREADY <= 1;
      data.ARREADY <= 1;
      // start out ready to accept an incoming write
      data.AWREADY <= 1;
      insn.AWREADY <= 1;
      data.WREADY <= 1;
      insn.WREADY <=1;
      
      insn.RVALID  <= 1'b0;
      data.RVALID  <= 1'b0;
      insn.BVALID  <= 1'b0;
      data.BVALID  <= 1'b0;
      insn.RRESP   <= ResponseOkay;
      data.RRESP   <= ResponseOkay;
      data.BRESP   <= ResponseOkay;
      insn.BRESP   <= ResponseOkay;
      
      // reset RDATA signal according to ED.
      insn.RDATA <= 0;
      data.RDATA <= 0;

    end else begin
      // Write Data (W) Channel
      if ((data.WVALID && !data.WREADY) | (!data.AWVALID)) begin
          data.WREADY <= 1'b1; // Accept write data
      end

      if ((insn.WVALID && !insn.WREADY) | (!insn.AWVALID)) begin
          insn.WREADY <= 1'b1; // Accept write data
      end 

      // Write Response (B) Channel
      if (data.AWREADY && data.WREADY && data.AWVALID && data.WVALID) begin
          mem_array[data.AWADDR[AddrMsb:AddrLsb]] <= data.WDATA; 
          data.AWREADY <= 1'b1;
          data.WREADY  <= 1'b1;
          data.BVALID  <= 1'b1; // Write is complete
      end else if (data.BREADY && data.BVALID) begin
          data.BVALID <= 1'b0; // Ready for another write after BREADY
      end
      if (insn.AWREADY && insn.WREADY && insn.AWVALID && insn.WVALID) begin
          mem_array[insn.AWADDR[AddrMsb:AddrLsb]] <= insn.WDATA; // Write data to memory
          insn.AWREADY <= 1'b1;
          insn.WREADY  <= 1'b1;
          insn.BVALID  <= 1'b1; // Indicate that write is complete
      end else if (insn.BREADY && insn.BVALID) begin
          insn.BVALID <= 1'b0; // Ready for another write after BREADY
      end

      // Read Address (AR) Channel
      if (!insn.RREADY) begin
          insn.ARREADY <= 1'b1; // Once RREADY goes low, we are no longer ready
      end
      if (!data.RREADY) begin
          data.ARREADY <= 1'b0; // Once RREADY goes low, we are no longer ready
      end

      // Read Data (R) Channel
      if(data.ARREADY && data.ARVALID && data.RREADY) begin
        data.RVALID <= 1'b1; // Indicate that data is valid
        data.RDATA <= mem_array[data.ARADDR[AddrMsb:AddrLsb]];
      end else if (!data.ARVALID) begin
        data.RVALID <= 0;
      end
      if(insn.ARREADY && insn.ARVALID && insn.RREADY) begin
        insn.RVALID <= 1'b1;
        insn.RDATA <= mem_array[insn.ARADDR[AddrMsb:AddrLsb]];
      end else if (!insn.ARVALID) begin
        insn.RVALID <= 0;
      end
 

    end
  end
endmodule

/** This is used for testing MemoryAxiLite in simulation, since Verilator doesn't allow
SV interfaces in top-level modules. We expose all of the AXIL signals here so that tests
can interact with them. */
module MemAxiLiteTester #(
    parameter int NUM_WORDS  = 32,
    parameter int ADDR_WIDTH = 32,
    parameter int DATA_WIDTH = 32
) (
    input wire ACLK,
    input wire ARESETn,

    input  wire                   I_ARVALID,
    output logic                  I_ARREADY,
    input  wire  [ADDR_WIDTH-1:0] I_ARADDR,
    input  wire  [           2:0] I_ARPROT,
    output logic                  I_RVALID,
    input  wire                   I_RREADY,
    output logic [ADDR_WIDTH-1:0] I_RDATA,
    output logic [           1:0] I_RRESP,

    input  wire                       I_AWVALID,
    output logic                      I_AWREADY,
    input  wire  [    ADDR_WIDTH-1:0] I_AWADDR,
    input  wire  [               2:0] I_AWPROT,
    input  wire                       I_WVALID,
    output logic                      I_WREADY,
    input  wire  [    DATA_WIDTH-1:0] I_WDATA,
    input  wire  [(DATA_WIDTH/8)-1:0] I_WSTRB,
    output logic                      I_BVALID,
    input  wire                       I_BREADY,
    output logic [               1:0] I_BRESP,

    input  wire                   D_ARVALID,
    output logic                  D_ARREADY,
    input  wire  [ADDR_WIDTH-1:0] D_ARADDR,
    input  wire  [           2:0] D_ARPROT,
    output logic                  D_RVALID,
    input  wire                   D_RREADY,
    output logic [ADDR_WIDTH-1:0] D_RDATA,
    output logic [           1:0] D_RRESP,

    input  wire                       D_AWVALID,
    output logic                      D_AWREADY,
    input  wire  [    ADDR_WIDTH-1:0] D_AWADDR,
    input  wire  [               2:0] D_AWPROT,
    input  wire                       D_WVALID,
    output logic                      D_WREADY,
    input  wire  [    DATA_WIDTH-1:0] D_WDATA,
    input  wire  [(DATA_WIDTH/8)-1:0] D_WSTRB,
    output logic                      D_BVALID,
    input  wire                       D_BREADY,
    output logic [               1:0] D_BRESP
);

  axi_clkrst_if axi (.*);
  axi_if #(
      .ADDR_WIDTH(ADDR_WIDTH),
      .DATA_WIDTH(DATA_WIDTH)
  ) insn ();
  assign insn.manager.ARVALID = I_ARVALID;
  assign I_ARREADY = insn.manager.ARREADY;
  assign insn.manager.ARADDR = I_ARADDR;
  assign insn.manager.ARPROT = I_ARPROT;
  assign I_RVALID = insn.manager.RVALID;
  assign insn.manager.RREADY = I_RREADY;
  assign I_RRESP = insn.manager.RRESP;
  assign I_RDATA = insn.manager.RDATA;

  axi_if #(
      .ADDR_WIDTH(ADDR_WIDTH),
      .DATA_WIDTH(DATA_WIDTH)
  ) data ();
  assign data.manager.ARVALID = D_ARVALID;
  assign D_ARREADY = data.manager.ARREADY;
  assign data.manager.ARADDR = D_ARADDR;
  assign data.manager.ARPROT = D_ARPROT;
  assign D_RVALID = data.manager.RVALID;
  assign data.manager.RREADY = D_RREADY;
  assign D_RRESP = data.manager.RRESP;
  assign D_RDATA = data.manager.RDATA;

  assign data.manager.AWVALID = D_AWVALID;
  assign D_AWREADY = data.manager.AWREADY;
  assign data.manager.AWADDR = D_AWADDR;
  assign data.manager.AWPROT = D_AWPROT;
  assign data.manager.WVALID = D_WVALID;
  assign D_WREADY = data.manager.WREADY;
  assign data.manager.WDATA = D_WDATA;
  assign data.manager.WSTRB = D_WSTRB;
  assign D_BVALID = data.manager.BVALID;
  assign data.manager.BREADY = D_BREADY;
  assign D_BRESP = data.manager.BRESP;

  MemoryAxiLite #(
      .NUM_WORDS(NUM_WORDS)
  ) mem (
      .axi (axi),
      .insn(insn.subord),
      .data(data.subord)
  );
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
  /** a stall cycle that arose from a load-to-use stall */
  CYCLE_LOAD2USE = 8,
  /** a stall cycle that arose from a div/rem-to-use stall */
  CYCLE_DIV2USE = 16,
  /** a stall cycle that arose from a fence insn */
  CYCLE_FENCE = 32
} cycle_status_e;

typedef struct packed {
  logic [`REG_SIZE] pc;
  // logic [`INSN_SIZE] insn;
  cycle_status_e cycle_status;
} stage_decode_t;

/** execute state **/
typedef struct packed {
  logic [`REG_SIZE] pc;
  logic [`INSN_SIZE] insn;
  cycle_status_e cycle_status;

  logic [4:0] rs1;
  logic [4:0] rs2;
  logic [4:0] rd;
  logic [11:0] imm_i;
  logic [19:0] imm_u;

  logic [`REG_SIZE] imm_i_sext;
  logic [`REG_SIZE] imm_b_sext;
  logic [`REG_SIZE] imm_s_sext;
  logic [`REG_SIZE] imm_j_sext;

  logic [`INSN_NAME_SIZE] insn_exe;
} stage_execute_t;

/** memory state **/
typedef struct packed {
  logic [`REG_SIZE] pc;
  logic [`INSN_SIZE] insn;
  cycle_status_e cycle_status;

  logic [4:0] rd;
  logic [4:0] rs2;

  logic [`REG_SIZE] rd_data;
  logic [`REG_SIZE] rs2_data;
  

  logic [`REG_SIZE] addr_to_dmem;

  logic [5:0] insn_exe;
  logic zero_flag,load_flag,neg_flag;
 

  logic we,halt;
} stage_memory_t;

/** writeback state **/
typedef struct packed {
  logic [`REG_SIZE] pc;
  logic [`INSN_SIZE] insn;
  cycle_status_e cycle_status;

  logic [4:0] rd;
  logic [`REG_SIZE] rd_data;

  logic load_flag;

  logic we;
  logic halt;
} stage_writeback_t;

module DatapathAxilMemory (
    input wire clk,
    input wire rst,

    // Start by replacing this interface to imem...
    // output logic [`REG_SIZE] pc_to_imem,
    // input wire [`INSN_SIZE] insn_from_imem,
    // ...with this AXIL one.
    axi_if.manager imem,

    // Once imem is working, replace this interface to dmem...
    // output logic [`REG_SIZE] addr_to_dmem,
    // input wire [`REG_SIZE] load_data_from_dmem,
    // output logic [`REG_SIZE] store_data_to_dmem,
    // output logic [3:0] store_we_to_dmem,
    // ...with this AXIL one
    axi_if.manager dmem,

    output logic halt,

    // The PC of the insn currently in Writeback. 0 if not a valid insn.
    output logic [`REG_SIZE] trace_writeback_pc,
    // The bits of the insn currently in Writeback. 0 if not a valid insn.
    output logic [`INSN_SIZE] trace_writeback_insn,
    // The status of the insn (or stall) currently in Writeback. See cycle_status_e enum for valid values.
    output cycle_status_e trace_writeback_cycle_status
);

  // TODO: your code here

  RegFile rf (
      .rd(writeback_state.rd),
      .rd_data(w_rd_data),
      .rs1(execute_state.rs1),
      .rs1_data(x_rs1_data),
      .rs2(execute_state.rs2),
      .rs2_data(x_rs2_data),

      .clk(clk),
      .we (w_we),
      .rst(rst)
  );

  // cycle counter, not really part of any stage but useful for orienting within GtkWave
  // do not rename this as the testbench uses this value
  // DON'T TOUCH THIS
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
  logic [`REG_SIZE] f_insn;
  cycle_status_e f_cycle_status;

  // program counter
  always_ff @(posedge clk) begin
    if (rst) begin
      f_pc_current   <= 32'd0;
      // NB: use CYCLE_NO_STALL since this is the value that will persist after the last reset cycle
      f_cycle_status <= CYCLE_NO_STALL;
    end else if (x_branch_flag) begin
      f_pc_current   <= x_branch_pc;
      f_cycle_status <= CYCLE_NO_STALL;
    end else if (x_load_stall || x_divide_stall_flag || d_fence_stall) begin
      f_pc_current   <= f_pc_current;
      f_cycle_status <= CYCLE_NO_STALL;
    end else begin
      f_pc_current   <= f_pc_current + 4;
      f_cycle_status <= CYCLE_NO_STALL;
    end
  end

  always_comb begin
    imem.ARVALID = 1;
    imem.RREADY  = 1;
    imem.ARADDR  = f_pc_current;
  end

 
  /****************/
  /* DECODE STAGE */
  /****************/

  logic [31:0] d_insn1, d_insn0, d_insn;


  always_comb begin
    d_insn1 = (decode_state.cycle_status == CYCLE_TAKEN_BRANCH) ? 32'b0 : imem.RDATA;
end

always_comb begin
    d_insn = (execute_state.cycle_status == CYCLE_LOAD2USE || execute_state.cycle_status == CYCLE_DIV2USE) ? d_insn0 : d_insn1;
end



  // this shows how to package up state in a `struct packed`, and how to pass it between stages
  stage_decode_t decode_state;
  always_ff @(posedge clk) begin
    d_insn0 <= d_insn1;

    if (rst) begin
      decode_state <= '{pc: 0, cycle_status: CYCLE_RESET};
    end else if (x_branch_flag) begin
      decode_state <= '{pc: 0, cycle_status: CYCLE_TAKEN_BRANCH};
    end else if (x_load_stall || 
                x_divide_stall_flag || 
                d_fence_stall) begin
     // d_insn <= imem.RDATA;
      decode_state <= '{pc: decode_state.pc, cycle_status: CYCLE_NO_STALL};
    end else begin
      begin
        decode_state <= '{pc: f_pc_current, cycle_status: f_cycle_status};
      end
    end
  end
  wire [255:0] d_disasm;
  Disasm #(
      .PREFIX("D")
  ) disasm_1decode (
      .insn  (d_insn),
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
  assign {d_insn_funct7, d_insn_rs2, d_insn_rs1, d_insn_funct3, d_insn_rd, d_insn_opcode} = d_insn;


logic [4:0] d_insn_rs1_used,d_insn_rs2_used;

always_comb begin
    // Default assignment
    d_insn_rs1_used = d_insn_rs1;
    d_insn_rs2_used = d_insn_rs2;

    // Handle specific opcode cases where no registers or only rs1 is used
    case (d_insn_opcode)
        OpLui, OpAuipc, OpJal, OpMiscMem, OpEnviron: begin
            d_insn_rs1_used = 5'd0; // Using 5'd0 for explicit 5-bit width
            d_insn_rs2_used = 5'd0;
        end
        OpJalr, OpLoad, OpRegImm: begin
            d_insn_rs1_used = d_insn_rs1;
            d_insn_rs2_used = 5'd0;
        end
        default: ; // Default case already handled by initial assignment
    endcase
end


  // setup for I, S, B & J type instructions
  // I - short immediates and loads
  wire [11:0] d_imm_i;
  assign d_imm_i = d_insn[31:20];
  wire [ 4:0] d_imm_shamt = d_insn[24:20];


  // S - stores
  wire [11:0] d_imm_s;
  assign d_imm_s[11:5] = d_insn_funct7, d_imm_s[4:0] = d_insn_rd;

  // B - conditionals
  wire [12:0] d_imm_b;
  assign {d_imm_b[12], d_imm_b[10:5]} = d_insn_funct7,
      {d_imm_b[4:1], d_imm_b[11]} = d_insn_rd,
      d_imm_b[0] = 1'b0;

  // J - unconditional jumps
  wire [20:0] d_imm_j;
  assign {d_imm_j[20], d_imm_j[10:1], d_imm_j[11], d_imm_j[19:12], d_imm_j[0]} = {
    d_insn[31:12], 1'b0
  };

  // U - setup for U type instructions
  wire [19:0] d_imm_u;
  assign d_imm_u = d_insn[31:12];

  wire [`REG_SIZE] d_imm_i_sext = {{20{d_imm_i[11]}}, d_imm_i[11:0]};
  wire [`REG_SIZE] d_imm_s_sext = {{20{d_imm_s[11]}}, d_imm_s[11:0]};
  wire [`REG_SIZE] d_imm_b_sext = {{19{d_imm_b[12]}}, d_imm_b[12:0]};
  wire [`REG_SIZE] d_imm_j_sext = {{11{d_imm_j[20]}}, d_imm_j[20:0]};

  // opcodes - see section 19 of RiscV spec
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

  wire d_insn_lui = d_insn_opcode == OpLui;
  wire d_insn_auipc = d_insn_opcode == OpAuipc;
  wire d_insn_jal = d_insn_opcode == OpJal;
  wire d_insn_jalr = d_insn_opcode == OpJalr;

  wire d_insn_beq = d_insn_opcode == OpBranch && d_insn[14:12] == 3'b000;
  wire d_insn_bne = d_insn_opcode == OpBranch && d_insn[14:12] == 3'b001;
  wire d_insn_blt = d_insn_opcode == OpBranch && d_insn[14:12] == 3'b100;
  wire d_insn_bge = d_insn_opcode == OpBranch && d_insn[14:12] == 3'b101;
  wire d_insn_bltu = d_insn_opcode == OpBranch && d_insn[14:12] == 3'b110;
  wire d_insn_bgeu = d_insn_opcode == OpBranch && d_insn[14:12] == 3'b111;

  wire d_insn_lb = d_insn_opcode == OpLoad && d_insn[14:12] == 3'b000;
  wire d_insn_lh = d_insn_opcode == OpLoad && d_insn[14:12] == 3'b001;
  wire d_insn_lw = d_insn_opcode == OpLoad && d_insn[14:12] == 3'b010;
  wire d_insn_lbu = d_insn_opcode == OpLoad && d_insn[14:12] == 3'b100;
  wire d_insn_lhu = d_insn_opcode == OpLoad && d_insn[14:12] == 3'b101;

  wire d_insn_sb = d_insn_opcode == OpStore && d_insn[14:12] == 3'b000;
  wire d_insn_sh = d_insn_opcode == OpStore && d_insn[14:12] == 3'b001;
  wire d_insn_sw = d_insn_opcode == OpStore && d_insn[14:12] == 3'b010;

  wire d_insn_addi = d_insn_opcode == OpRegImm && d_insn[14:12] == 3'b000;
  wire d_insn_slti = d_insn_opcode == OpRegImm && d_insn[14:12] == 3'b010;
  wire d_insn_sltiu = d_insn_opcode == OpRegImm && d_insn[14:12] == 3'b011;
  wire d_insn_xori = d_insn_opcode == OpRegImm && d_insn[14:12] == 3'b100;
  wire d_insn_ori = d_insn_opcode == OpRegImm && d_insn[14:12] == 3'b110;
  wire d_insn_andi = d_insn_opcode == OpRegImm && d_insn[14:12] == 3'b111;

  wire d_insn_slli = d_insn_opcode == OpRegImm && d_insn[14:12] == 3'b001 && d_insn[31:25] == 7'd0;
  wire d_insn_srli = d_insn_opcode == OpRegImm && d_insn[14:12] == 3'b101 && d_insn[31:25] == 7'd0;
  wire d_insn_srai = d_insn_opcode == OpRegImm && d_insn[14:12] == 3'b101 && d_insn[31:25] == 7'b0100000;

  wire d_insn_add = d_insn_opcode == OpRegReg && d_insn[14:12] == 3'b000 && d_insn[31:25] == 7'd0;
  wire d_insn_sub  = d_insn_opcode == OpRegReg && d_insn[14:12] == 3'b000 && d_insn[31:25] == 7'b0100000;
  wire d_insn_sll = d_insn_opcode == OpRegReg && d_insn[14:12] == 3'b001 && d_insn[31:25] == 7'd0;
  wire d_insn_slt = d_insn_opcode == OpRegReg && d_insn[14:12] == 3'b010 && d_insn[31:25] == 7'd0;
  wire d_insn_sltu = d_insn_opcode == OpRegReg && d_insn[14:12] == 3'b011 && d_insn[31:25] == 7'd0;
  wire d_insn_xor = d_insn_opcode == OpRegReg && d_insn[14:12] == 3'b100 && d_insn[31:25] == 7'd0;
  wire d_insn_srl = d_insn_opcode == OpRegReg && d_insn[14:12] == 3'b101 && d_insn[31:25] == 7'd0;
  wire d_insn_sra  = d_insn_opcode == OpRegReg && d_insn[14:12] == 3'b101 && d_insn[31:25] == 7'b0100000;
  wire d_insn_or = d_insn_opcode == OpRegReg && d_insn[14:12] == 3'b110 && d_insn[31:25] == 7'd0;
  wire d_insn_and = d_insn_opcode == OpRegReg && d_insn[14:12] == 3'b111 && d_insn[31:25] == 7'd0;

  wire d_insn_mul = d_insn_opcode == OpRegReg && d_insn[31:25] == 7'd1 && d_insn[14:12] == 3'b000;
  wire d_insn_mulh = d_insn_opcode == OpRegReg && d_insn[31:25] == 7'd1 && d_insn[14:12] == 3'b001;
  wire d_insn_mulhsu = d_insn_opcode == OpRegReg && d_insn[31:25] == 7'd1 && d_insn[14:12] == 3'b010;
  wire d_insn_mulhu = d_insn_opcode == OpRegReg && d_insn[31:25] == 7'd1 && d_insn[14:12] == 3'b011;
  wire d_insn_div = d_insn_opcode == OpRegReg && d_insn[31:25] == 7'd1 && d_insn[14:12] == 3'b100;
  wire d_insn_divu = d_insn_opcode == OpRegReg && d_insn[31:25] == 7'd1 && d_insn[14:12] == 3'b101;
  wire d_insn_rem = d_insn_opcode == OpRegReg && d_insn[31:25] == 7'd1 && d_insn[14:12] == 3'b110;
  wire d_insn_remu = d_insn_opcode == OpRegReg && d_insn[31:25] == 7'd1 && d_insn[14:12] == 3'b111;

  wire d_insn_ecall = d_insn_opcode == OpEnviron && d_insn[31:7] == 25'd0;
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

  logic d_save_flag,d_fence_stall;
  always_comb begin
    d_save_flag = (d_insn_exe == InsnSb) ||
                  (d_insn_exe == InsnSh) ||
                  (d_insn_exe == InsnSw);
end



 
  always_comb begin
    if ((d_insn_exe == InsnFence) && (x_save_flag || m_save_flag)) begin
      d_fence_stall = 1;
    end else begin
      d_fence_stall = 0;
    end
  end

  /*****************/
  /* EXECUTE STAGE */
  /*****************/

  stage_execute_t execute_state;
  always_ff @(posedge clk) begin
    if (rst) begin
      execute_state <= '{
          pc: 0,
          insn: 0,
          cycle_status: CYCLE_RESET,

          rs1: 0,
          rs2: 0,
          rd: 0,
          imm_i : 0,
          imm_u: 0,
          imm_i_sext: 0,
          imm_b_sext: 0,
          imm_s_sext: 0,
          imm_j_sext: 0,

          insn_exe: 0
      };
    end else if (x_load_stall) begin
      execute_state <= '{
          pc: 0,
          insn: 0,
          cycle_status: CYCLE_LOAD2USE,

          rs1: 0,
          rs2: 0,
          rd: 0,
          imm_i : d_imm_i,
          imm_u: 0,
          imm_i_sext: 0,
          imm_b_sext: 0,
          imm_s_sext: 0,
          imm_j_sext: 0,

          insn_exe: 0
      };
    end else if (x_divide_stall_flag) begin
      execute_state <= '{
          pc: 0,
          insn: 0,
          cycle_status: CYCLE_DIV2USE,

          rs1: 0,
          rs2: 0,
          rd: 0,

          imm_u: 0,
          imm_i :0,
          imm_i_sext: 0,
          imm_b_sext: 0,
          imm_s_sext: 0,
          imm_j_sext: 0,

          insn_exe: 0
      };
    end else if (x_branch_flag) begin
      execute_state <= '{
          pc: 0,
          insn: 0,
          cycle_status: CYCLE_TAKEN_BRANCH,

          rs1: 0,
          rs2: 0,
          rd: 0,

          imm_u: 0,
          imm_i : 0,
          imm_i_sext: 0,
          imm_b_sext: 0,
          imm_s_sext: 0,
          imm_j_sext: 0,

          insn_exe: 0
      };
    end else begin
      begin
        execute_state <= '{
            pc: decode_state.pc,
            insn: d_insn,
            cycle_status: decode_state.cycle_status,

            rs1: d_insn_rs1,
            rs2: d_insn_rs2,
            rd: d_insn_rd,

            imm_u: d_imm_u,
            imm_i  : d_imm_i,
            imm_i_sext: d_imm_i_sext,
            imm_b_sext: d_imm_b_sext,
            imm_s_sext: d_imm_s_sext,
            imm_j_sext: d_imm_j_sext,

            insn_exe: d_insn_exe
        };
      end
    end
  end
  wire [255:0] x_disasm;
  Disasm #(
      .PREFIX("X")
  ) disasm_2execute (
      .insn  (execute_state.insn),
      .disasm(x_disasm)
  );

  // TODO: the testbench requires that your register file instance is named `rf`

  // // //
  // EXECUTE: Modules Used
  // // //

  logic [4:0] x_rs1, x_rd;
  logic [`REG_SIZE] x_rd_data, x_rs1_data, x_rs2_data;

  logic x_we,x_halt;

  // For branching
  logic [`REG_SIZE] x_branch_pc;
  logic x_branch_flag;


  logic [`REG_SIZE] br_d1, br_d2;
  




  logic [`REG_SIZE] x_div_a, x_div_b;
  logic [`REG_SIZE] m_remainder, m_quotient;
  
  
  logic x_neg_flag,x_zero_flag,x_load_flag,x_save_flag,x_load_stall,x_div_flag,x_divide_stall_flag;

  divider_unsigned_pipelined unsigned_div (
      .clk(clk),
      .rst(rst),
      .i_dividend(x_div_a),
      .i_divisor(x_div_b),
      .o_remainder(m_remainder),
      .o_quotient(m_quotient)
  );

  // always_comb begin 
  //   br_d1 = x_rs1_data;
  //   br_d2 = x_rs2_data;

  //   if((memory_state.insn[11:7] == execute_state.insn[19:15]) && memory_state.insn[11:7] != 0 && execute_state.insn[19:15]!=0 &&  x_rs1_en && mem_en) begin 
  //     br_d1 = memory_state.rd_data;
  //   end

  //   if((memory_state.insn[11:7] == execute_state.insn[24:20]) && memory_state.insn[11:7] != 0 && execute_state.insn[24:20]!=0 &&  x_rs2_en && mem_en) begin 
  //     br_d2 = memory_state.rd_data;
  //   end

  //   if((writeback_state.insn[11:7] == execute_state.insn[19:15]) && writeback_state.insn[11:7] != 0 && execute_state.insn[19:15]!=0 &&  x_rs1_en && write_en) begin 
  //     br_d1 = writeback_state.rd_data;
  //   end

  //   if((writeback_state.insn[11:7] == execute_state.insn[24:20]) && writeback_state.insn[11:7] != 0 && execute_state.insn[24:20]!=0 &&  x_rs2_en && write_en) begin 
  //     br_d2 = writeback_state.rd_data;
  //   end
  // end
  
  always_comb begin
    x_load_flag = (execute_state.insn_exe == InsnLb)  ||
                  (execute_state.insn_exe == InsnLbu) ||
                  (execute_state.insn_exe == InsnLh)  ||
                  (execute_state.insn_exe == InsnLhu) ||
                  (execute_state.insn_exe == InsnLw);
end


  
  always_comb begin
    x_save_flag = (execute_state.insn_exe == InsnSb) ||
                  (execute_state.insn_exe == InsnSh) ||
                  (execute_state.insn_exe == InsnSw);
end




always_comb begin
    // Reset stall to 0 by default
    x_load_stall = 1'b0;

    // Check for conditions that trigger a load stall
    if (x_load_flag && ((execute_state.rd == d_insn_rs1_used && d_insn_rs1_used != 0) ||
                        (execute_state.rd == d_insn_rs2_used && d_insn_rs2_used != 0 && d_save_flag == 0))) begin
        x_load_stall = 1'b1;
    end
end


  
  
  always_comb begin
    x_div_flag = (execute_state.insn_exe == InsnDiv)  ||
                 (execute_state.insn_exe == InsnDivu) ||
                 (execute_state.insn_exe == InsnRem)  ||
                 (execute_state.insn_exe == InsnRemu);
end




always_comb begin
    // Default stall to 0
    x_divide_stall_flag = 1'b0;

    // Check for conditions that trigger a divide use stall
    if (x_div_flag && ((execute_state.rd == d_insn_rs1_used && d_insn_rs1_used != 0) ||
                       (execute_state.rd == d_insn_rs2_used && d_insn_rs2_used != 0))) begin
        x_divide_stall_flag = 1'b1;
    end
end




  // CALCULATING MEMORY ADDRESS

  logic [`REG_SIZE] x_unaligned_addr_to_dmem;
  logic [`REG_SIZE] x_addr_to_dmem;

  // MUL STUFF
  logic [63:0] mul_1,mul_2,mul_3,mul_4;


  logic [`REG_SIZE] x_jalr;
  //wire x_rs1_en = (execute_state.insn[6:0]==OpRegImm) || (execute_state.insn[6:0]==OpRegReg) || (execute_state.insn[6:0]==OpStore) || (execute_state.insn[6:0]==OpBranch);
  //wire x_rs2_en = (execute_state.insn[6:0]==OpRegReg) || (execute_state.insn[6:0]==OpStore) || (execute_state.insn[6:0]==OpBranch);
  //wire mem_en = (memory_state.insn[6:0]==OpRegImm) || (memory_state.insn[6:0]==OpRegReg) || (memory_state.insn[6:0]==OpLui) || (memory_state.insn[6:0]==OpJal);

  // // //
  // EXECUTE: Logic
  // // //

  always_comb begin
    x_rd = 0;
    x_rd_data = 0;
    x_we = 0;
    x_div_a = 0;
    x_div_b = 0;

    mul_1 = 0;
    mul_2 = 0;
    mul_3 = 0;
    mul_4 = 0;

   
    x_zero_flag = 0;
    x_neg_flag= 0;

    x_jalr = 0;

    x_halt = 0;

    // Branching
    x_branch_flag = 0;
    x_branch_pc = execute_state.pc;

    // Loads
    x_unaligned_addr_to_dmem = 0;
    x_addr_to_dmem = 0;

    br_d1 = (execute_state.rs1 != 0) ? (
                              (execute_state.rs1 == memory_state.rd) ? memory_state.rd_data :
                              (execute_state.rs1 == writeback_state.rd) ? writeback_state.rd_data :
                              x_rs1_data
                          ) : x_rs1_data;

    br_d2 = (execute_state.rs2 != 0) ? (
                              (execute_state.rs2 == memory_state.rd) ? memory_state.rd_data :
                              (execute_state.rs2 == writeback_state.rd) ? writeback_state.rd_data :
                              x_rs2_data
                          ) : x_rs2_data;
  

    case (execute_state.insn_exe)

    

      InsnLui: begin
        x_rd = execute_state.rd;
        x_rd_data = {execute_state.imm_u, 12'b0};
        x_we = 1;
      end

      InsnAuipc: begin
        x_rd = execute_state.rd;
        x_rd_data = execute_state.pc + {execute_state.imm_u, 12'b0};
        x_we = 1;
      end

      InsnAddi: begin
        x_rd = execute_state.rd;
        x_rd_data = br_d1+execute_state.imm_i_sext;
        x_we = 1;

        
      end

      InsnSlti: begin
        x_rd = execute_state.rd;
        x_rd_data = $signed(br_d1) < $signed(execute_state.imm_i_sext) ? 32'h00000001 : 32'h00000000;
        x_we = 1;
      end

      InsnSltiu: begin
        x_rd = execute_state.rd;
        x_rd_data = $unsigned(br_d1) < $unsigned(execute_state.imm_i_sext) ? 32'h00000001 : 32'h00000000;
        x_we = 1;
      end

      InsnXori: begin
        x_rd = execute_state.rd;
        x_rd_data = br_d1 ^ execute_state.imm_i_sext;
        x_we = 1;
      end

      InsnOri: begin
        x_rd = execute_state.rd;
        x_rd_data = br_d1 | execute_state.imm_i_sext;
        x_we = 1;
      end

      InsnAndi: begin
        x_rd = execute_state.rd;
        x_rd_data = br_d1 & execute_state.imm_i_sext;
        x_we = 1;
      end

      InsnSlli: begin

        x_rd = execute_state.rd;
        x_rd_data = br_d1 << execute_state.imm_i[4:0];
        x_we = 1;
      end

      InsnSrli: begin
        x_rd = execute_state.rd;
        x_rd_data = br_d1 >> execute_state.imm_i[4:0];
        x_we = 1;
      end

      InsnSrai: begin
        x_rd = execute_state.rd;
        x_rd_data = $signed(br_d1) >>> execute_state.imm_i[4:0];
        x_we = 1;
      end

      InsnSltu: begin
        x_rd = execute_state.rd;
        x_rd_data = $unsigned(br_d1) < $unsigned(br_d2) ? 32'h00000001 : 32'h00000000;
        x_we = 1;
      end


      InsnAdd: begin
      

        x_rd = execute_state.rd;
        x_rd_data = br_d1+br_d2;
        x_we = 1;
      end

      InsnSub: begin
      

        x_rd = execute_state.rd;
        x_rd_data = br_d1-br_d2;
        x_we = 1;
      end

      InsnAnd: begin
        x_rd = execute_state.rd;
        x_rd_data = br_d1 & br_d2;
        x_we = 1;
      end

      InsnOr: begin
        x_rd = execute_state.rd;
        x_rd_data = br_d1 | br_d2;
        x_we = 1;
      end

      InsnXor: begin
        x_rd = execute_state.rd;
        x_rd_data = br_d1 ^ br_d2;
        x_we = 1;
      end

      InsnSlt: begin
        x_rd = execute_state.rd;
        x_rd_data = $signed(br_d1) < $signed(br_d2) ? 32'h00000001 : 32'h00000000;
        x_we = 1;
      end

      InsnSll: begin
        x_rd = execute_state.rd;
        x_rd_data = br_d1 << br_d2[4:0];
        x_we = 1;
      end

      InsnSrl: begin
        x_rd = execute_state.rd;
        x_rd_data = br_d1 >> br_d2[4:0];
        x_we = 1;
      end

      InsnSra: begin
        x_rd = execute_state.rd;
        x_rd_data = $signed(br_d1) >>> br_d2[4:0];
        x_we = 1;
      end

      InsnMul: begin
        x_rd = execute_state.rd;
        mul_1 = (br_d1 * br_d2);
        x_rd_data = mul_1[31:0];
        x_we = 1;
      end

      InsnMulh: begin
        x_rd = execute_state.rd;
        

        mul_2 = {{32{br_d1[31]}}, br_d1} * {{32{br_d2[31]}}, br_d2};
        x_rd_data = mul_2[63:32];
        x_we = 1;
      end

      InsnMulhsu: begin
        x_rd = execute_state.rd;
       

        mul_3 = {{32{br_d1[31]}}, br_d1} * {32'b0, br_d2};
        x_rd_data = mul_3[63:32];

        x_we = 1;
      end

      InsnMulhu: begin
        x_rd = execute_state.rd;
  

        mul_4 = ($unsigned(br_d1) * $unsigned(br_d2));
        x_rd_data = mul_4[63:32];
        x_we = 1;
      end

      // Branch 

      InsnBeq: begin
        x_rd = 0;
        x_rd_data = 0;
        x_we = 0;

        if (br_d1 == br_d2) begin
          x_branch_pc = execute_state.pc + execute_state.imm_b_sext;
          x_branch_flag = 1;
        end
      end

      InsnBne: begin
        x_rd = 0;
        x_rd_data = 0;
        x_we = 0;

        if (br_d1 != br_d2) begin
          x_branch_pc = execute_state.pc + execute_state.imm_b_sext;
          x_branch_flag = 1;
        end
      end

      InsnBlt: begin
        x_rd = 0;
        x_rd_data = 0;
        x_we = 0;

        if ($signed(br_d1) < $signed(br_d2)) begin
          x_branch_pc = execute_state.pc + execute_state.imm_b_sext;
          x_branch_flag = 1;
        end
      end

      InsnBge: begin
        x_rd = 0;
        x_rd_data = 0;
        x_we = 0;
        if ($signed(br_d1) >= $signed(br_d2)) begin
          x_branch_pc = execute_state.pc + execute_state.imm_b_sext;
          x_branch_flag = 1;
        end
      end

      InsnBltu: begin
        x_rd = 0;
        x_rd_data = 0;
        x_we = 0;
        if ($unsigned(br_d1) < $unsigned(br_d2)) begin
          x_branch_pc = execute_state.pc + execute_state.imm_b_sext;
          x_branch_flag = 1;
        end
      end

      InsnBgeu: begin
        x_rd = 0;
        x_rd_data = 0;
        x_we = 0;
        if ($unsigned(br_d1) >= $unsigned(br_d2)) begin
          x_branch_pc = execute_state.pc + execute_state.imm_b_sext;
          x_branch_flag = 1;
        end
      end



      InsnJal: begin
        x_rd = execute_state.rd;
        x_rd_data = execute_state.pc + 32'd4;
        x_we = 1;

        x_branch_pc = execute_state.pc + execute_state.imm_j_sext;
        x_branch_flag = 1;
      end

      InsnJalr: begin
        x_rd = execute_state.rd;
        x_rd_data = execute_state.pc + 32'd4;
        x_we = 1;

        x_jalr = (br_d1 + execute_state.imm_i_sext) & ~(32'd1);
        x_branch_pc = {x_jalr[31:2], 2'b0};
        x_branch_flag = 1;
      end

      /* LOAD INSNS */
      InsnLb: begin
        x_rd = execute_state.rd;
        x_we = 1;

        x_addr_to_dmem = $signed(br_d1) + $signed(execute_state.imm_i_sext);
      end

      InsnLh: begin
        x_rd = execute_state.rd;
        x_we = 1;

        x_addr_to_dmem = $signed(br_d1) + $signed(execute_state.imm_i_sext);
      end

      InsnLw: begin
        x_rd = execute_state.rd;
        x_we = 1;

        x_addr_to_dmem = $signed(br_d1) + $signed(execute_state.imm_i_sext);
      end

      InsnLbu: begin
        x_rd = execute_state.rd;
        x_we = 1;

        x_addr_to_dmem = $signed(br_d1) + $signed(execute_state.imm_i_sext);
      end

      InsnLhu: begin
        x_rd = execute_state.rd;
        x_we = 1;


        x_addr_to_dmem = $signed(br_d1) + $signed(execute_state.imm_i_sext);
      end

      InsnSb: begin
        x_we = 0;
        x_addr_to_dmem = $signed(br_d1) + $signed(execute_state.imm_s_sext);
      end

      InsnSh: begin
        x_we = 0;
        x_addr_to_dmem = $signed(br_d1) + $signed(execute_state.imm_s_sext);
      end

      InsnSw: begin
        x_we = 0;
        x_addr_to_dmem = $signed(br_d1) + $signed(execute_state.imm_s_sext);
      end

      InsnFence: begin
        x_we = 0;
      end

      InsnEcall: begin
        x_we   = 0;
        x_halt = 1;
      end

      InsnDiv: begin
            x_zero_flag = (br_d2 == 0);
            x_we = 1;
            x_rd = execute_state.rd;

            if (br_d2 == 0) begin

                x_rd_data = -1;
            end else begin
                x_div_a = (br_d1[31]) ? ~br_d1 + 1 : br_d1;
                x_div_b = (br_d2[31]) ? ~br_d2 + 1 : br_d2;
                x_neg_flag= (br_d1[31] != br_d2[31]);
            end
        end


      InsnDivu: begin
            x_zero_flag = (br_d2 == 0);
            x_we = 1;
            x_rd = execute_state.rd;

            if (br_d2 == 0) begin
            
                x_rd_data = $unsigned(-1);
            end else begin
                x_div_a = br_d1;
                x_div_b = br_d2;
            end
        end

      InsnRem: begin
            x_zero_flag = (br_d2 == 0);
            x_we = 1;
            x_rd = execute_state.rd;

            if (br_d2 == 0) begin
                x_rd_data = br_d1;
            end else begin
                x_div_a = (br_d1[31]) ? ~br_d1 + 1 : br_d1;
                x_div_b = (br_d2[31]) ? ~br_d2 + 1 : br_d2;
                x_neg_flag= (br_d1[31] && !br_d2[31]) || (!br_d1[31] && br_d2[31]);
            end
        end

      InsnRemu: begin
            x_zero_flag = (br_d2 == 0);
            x_we = 1;
            x_rd = execute_state.rd;

            if (br_d2 == 0) begin
                x_rd_data = br_d1;
            end else begin
                x_div_a = br_d1;
                x_div_b = br_d2;
            end
        end

      default: begin
        x_we = 0;
      end
    endcase
  end

  /****************/
  /* MEMORY STAGE */
  /****************/

  stage_memory_t memory_state;
  always_ff @(posedge clk) begin
    if (rst) begin
      memory_state <= '{
          pc: 0,
          insn: 0,
          cycle_status: CYCLE_RESET,

          rd: 0,
          rs2: 0,
          rd_data: 0,
          rs2_data: 0,

          insn_exe: 0,
          load_flag: 0,
          zero_flag: 0,
          neg_flag: 0,

          addr_to_dmem: 0,

          we: 0,
          halt: 0
      };
    end else begin
      begin
        memory_state <= '{
            pc: execute_state.pc,  
            insn: execute_state.insn,
            cycle_status: execute_state.cycle_status,

            rd: x_rd,
            rs2: execute_state.rs2,
            rd_data: x_rd_data,
            rs2_data: br_d2,

            insn_exe: execute_state.insn_exe,
            load_flag: x_load_flag,
            zero_flag: x_zero_flag,
            neg_flag: x_neg_flag,

            addr_to_dmem: x_addr_to_dmem,

            we: x_we,
            halt: x_halt
        };
      end
    end
  end
  wire [255:0] m_disasm;
  Disasm #(
      .PREFIX("M")
  ) disasm_3memory (
      .insn  (execute_state.insn),
      .disasm(m_disasm)
  );


  logic [`REG_SIZE] m_rd_data,m_data2,m_dmem;

  logic m_save_flag;
  always_comb begin
    m_save_flag = (memory_state.insn_exe == InsnSb) ||
                     (memory_state.insn_exe == InsnSh) ||
                     (memory_state.insn_exe == InsnSw);
end



  assign m_data2 = ((memory_state.rs2 == writeback_state.rd) && writeback_state.load_flag && m_save_flag) ? writeback_state.rd_data : memory_state.rs2_data;

  always_comb begin
    m_dmem = 0;

    dmem.WSTRB = 0;
    dmem.WVALID = 0;
    dmem.AWVALID = 0;
    dmem.BREADY = 0;

    case (memory_state.insn_exe)


      InsnLb: begin
        m_dmem = {memory_state.addr_to_dmem[31:2], 2'b0};

        // Choose which byte of the loaded word to take
        // We also need to sign-extend our result manually
        case (memory_state.addr_to_dmem[1:0])
          2'b00: begin
            m_rd_data = {{24{dmem.RDATA[7]}}, dmem.RDATA[7:0]};
          end
          2'b01: begin
            m_rd_data = {{24{dmem.RDATA[15]}}, dmem.RDATA[15:8]};
          end
          2'b10: begin
            m_rd_data = {{24{dmem.RDATA[23]}}, dmem.RDATA[23:16]};
          end
          2'b11: begin
            m_rd_data = {{24{dmem.RDATA[31]}}, dmem.RDATA[31:24]};
          end
          default: begin
          end
        endcase
      end

      InsnLbu: begin
        m_dmem = {memory_state.addr_to_dmem[31:2], 2'b0};


        case (memory_state.addr_to_dmem[1:0])
          2'b00: begin
            m_rd_data = {24'b0, dmem.RDATA[7:0]};
          end
          2'b01: begin
            m_rd_data = {24'b0, dmem.RDATA[15:8]};
          end
          2'b10: begin
            m_rd_data = {24'b0, dmem.RDATA[23:16]};
          end
          2'b11: begin
            m_rd_data = {24'b0, dmem.RDATA[31:24]};
          end
          default: begin
          end
        endcase
      end

      InsnLh: begin

        if (memory_state.addr_to_dmem[0] == 0) begin
          m_dmem = {memory_state.addr_to_dmem[31:2], 2'b0};

          case (memory_state.addr_to_dmem[1])
            1'b0: begin
              m_rd_data = {{16{dmem.RDATA[15]}}, dmem.RDATA[15:0]};
            end
            1'b1: begin
              m_rd_data = {{16{dmem.RDATA[31]}}, dmem.RDATA[31:16]};
            end
            default: begin
            end
          endcase
        end
      end

      InsnLhu: begin

        if (memory_state.addr_to_dmem[0] == 0) begin
          m_dmem = {memory_state.addr_to_dmem[31:2], 2'b0};

          case (memory_state.addr_to_dmem[1])
            1'b0: begin
              m_rd_data = {16'b0, dmem.RDATA[15:0]};
            end
            1'b1: begin
              m_rd_data = {16'b0, dmem.RDATA[31:16]};
            end
            default: begin
            end
          endcase
        end
      end

      InsnLw: begin
        if (memory_state.addr_to_dmem[1:0] == 2'b0) begin
          m_rd_data = dmem.RDATA;
          m_dmem = memory_state.addr_to_dmem;
        end
      end

      /* SAVE INSNS */
      InsnSb: begin
        dmem.WVALID = 1;
        dmem.AWVALID = 1;
        dmem.BREADY = 1;
        m_dmem = {memory_state.addr_to_dmem[31:2], 2'b0};

        case (memory_state.addr_to_dmem[1:0])
          2'b00: begin
            dmem.WSTRB = 4'b0001;
            dmem.WDATA = {24'b0, m_data2[7:0]};
          end
          2'b01: begin
            dmem.WSTRB = 4'b0010;
            dmem.WDATA = {16'b0, m_data2[7:0], 8'b0};
          end
          2'b10: begin
            dmem.WSTRB = 4'b0100;
            dmem.WDATA = {8'b0, m_data2[7:0], 16'b0};
          end
          2'b11: begin
            dmem.WSTRB = 4'b1000;
            dmem.WDATA = {m_data2[7:0], 24'b0};
          end
          default: begin
          end
        endcase
      end

      InsnSh: begin
        dmem.WVALID  = 1;
        dmem.AWVALID = 1;
        dmem.BREADY  = 1;
        if (memory_state.addr_to_dmem[0] == 0) begin
          m_dmem = {memory_state.addr_to_dmem[31:2], 2'b0};

          case (memory_state.addr_to_dmem[1])
            1'b0: begin
              dmem.WSTRB = 4'b0011;
              dmem.WDATA = {16'b0, m_data2[15:0]};
            end
            1'b1: begin
              dmem.WSTRB = 4'b1100;
              dmem.WDATA = {m_data2[15:0], 16'b0};
            end
            default: begin
            end
          endcase
        end
      end

      InsnSw: begin
        dmem.WVALID  = 1;
        dmem.AWVALID = 1;
        dmem.BREADY  = 1;
        if (memory_state.addr_to_dmem[1:0] == 2'b0) begin
          m_dmem = memory_state.addr_to_dmem;
          dmem.WSTRB = 4'b1111;
          dmem.WDATA = m_data2;
        end
      end

      InsnDiv: begin
        if (memory_state.zero_flag == 1) begin
          m_rd_data = memory_state.rd_data;
        end else if (memory_state.neg_flag == 1) begin
          m_rd_data = ~m_quotient + 1;
        end else begin
          m_rd_data = m_quotient;
        end
      end

      InsnDivu: begin
        if (memory_state.zero_flag == 1) begin
          m_rd_data = memory_state.rd_data;
        end else begin
          m_rd_data = m_quotient;
        end
      end

      InsnRem: begin
        if (memory_state.zero_flag == 1) begin
          m_rd_data = memory_state.rd_data;
        end else if (memory_state.neg_flag == 1) begin
          m_rd_data = ~m_remainder + 1;
        end else begin
          m_rd_data = m_remainder;
        end
      end

      InsnRemu: begin
        if (memory_state.zero_flag == 1) begin
          m_rd_data = memory_state.rd_data;
        end else begin
          m_rd_data = m_remainder;
        end
      end

      default: begin
        m_rd_data = memory_state.rd_data;
      end
    endcase
  end

  // Update addr_to_dmem
  always_comb begin
    dmem.ARVALID = 1;
    dmem.ARADDR  = m_dmem;
    dmem.AWADDR  = m_dmem;
  end


  /*******************/
  /* WRITEBACK STAGE */
  /*******************/

  stage_writeback_t writeback_state;
  always_ff @(posedge clk) begin
    if (rst) begin
      writeback_state <= '{
          pc: 0,
          insn: 0,
          cycle_status: CYCLE_RESET,

          rd: 0,
          rd_data: 0,

          load_flag: 0,

          we: memory_state.we,
          halt: memory_state.halt
      };
    end else begin
      begin
        writeback_state <= '{
            pc: memory_state.pc,
            insn: memory_state.insn,
            cycle_status: memory_state.cycle_status,

            rd: memory_state.rd,
            rd_data: m_rd_data,

            load_flag: memory_state.load_flag,

            we: memory_state.we,
            halt: memory_state.halt
        };
      end
    end
  end
  //wire write_en = (writeback_state.insn[6:0]==OpRegImm) || (writeback_state.insn[6:0]==OpRegReg) || (writeback_state.insn[6:0]==OpLui) || (writeback_state.insn[6:0]==OpJal);

  wire [255:0] w_disasm;
  Disasm #(
      .PREFIX("W")
  ) disasm_4writeback (
      .insn  (memory_state.insn),
      .disasm(w_disasm)
  );

  // Traces
  assign trace_writeback_pc = writeback_state.pc;
  assign trace_writeback_insn = writeback_state.insn;
  assign trace_writeback_cycle_status = writeback_state.cycle_status;

  assign halt = (memory_state.insn[6:0] == 7'h73) & (memory_state.insn[31:7] == 'b0);

  logic w_we;
  logic [`REG_SIZE] w_rd_data;

  // Writing back 
  always_comb begin
    if (writeback_state.we == 1) begin
      w_rd_data = writeback_state.rd_data;
      w_we = writeback_state.we;
    end else begin
      w_rd_data = 0;
      w_we = 0;
    end
    //halt = writeback_state.halt;
  end
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

  // HW5 memory interface
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

  // HW6 memory interface
  axi_clkrst_if axi_cr (.ACLK(clk), .ARESETn(~rst));
  axi_if axi_data ();
  axi_if axi_insn ();
  MemoryAxiLite #(.NUM_WORDS(8192)) mem (
    .axi(axi_cr),
    .insn(axi_insn.subord),
    .data(axi_data.subord)
  );

  DatapathAxilMemory datapath (
      .clk(clk),
      .rst(rst),
      .imem(axi_insn.manager),
      .dmem(axi_data.manager),
      .halt(halt),
      .trace_writeback_pc(trace_writeback_pc),
      .trace_writeback_insn(trace_writeback_insn),
      .trace_writeback_cycle_status(trace_writeback_cycle_status)
  );

endmodule
