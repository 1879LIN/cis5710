/* INSERT NAME AND PENNKEY HERE */

`timescale 1ns / 1ns

// quotient = dividend / divisor

module divider_unsigned_pipelined (
    input wire clk, rst,
    input  wire [31:0] i_dividend,
    input  wire [31:0] i_divisor,
    output wire [31:0] o_remainder,
    output wire [31:0] o_quotient
);

    // TODO: your code here
    wire [31:0] dividend[33];
    wire [31:0] remainder[33];
    wire [31:0] quotient[33];
     //register:
    logic [31:0] reg_stage_2_dividend;
    logic [31:0] reg_stage_2_divisor;
    logic [31:0] reg_stage_2_remainder;
    logic [31:0] reg_stage_2_quotient;

    //stage 1:
    assign dividend[0] = i_dividend;
    assign remainder[0] = 32'b0;
    assign quotient[0] = 32'b0;


    genvar i;
    genvar j;
    
    for (i = 0; i < 16; i = i + 1) begin  
        divu_1iter u_divu_1iter(
            .i_dividend  ( dividend[i]),
            .i_divisor   ( i_divisor),
            .i_remainder ( remainder[i]),
            .i_quotient  ( quotient[i]),
            .o_dividend  ( dividend[i+1]),
            .o_remainder ( remainder[i+1]),
            .o_quotient  ( quotient[i+1])
        );
    end


   

    always_ff @(posedge clk) 
    if (rst) begin
        reg_stage_2_dividend <= 32'b0;
        reg_stage_2_divisor <= 32'b0;
        reg_stage_2_remainder <= 32'b0;
        reg_stage_2_quotient <= 32'b0;
    end else begin
        reg_stage_2_dividend <= dividend[16];
        reg_stage_2_divisor <= i_divisor;
        reg_stage_2_remainder <= remainder[16];
        reg_stage_2_quotient <= quotient[16];
    end

    //stage 2:
    assign dividend[16] = reg_stage_2_dividend;
    assign remainder[16] = reg_stage_2_remainder;
    assign quotient[16] = reg_stage_2_quotient;


    

    for (j = 16; j < 32; j = j + 1) begin 
        divu_1iter u_divu_1iter(
            .i_dividend  ( dividend[j]),
            .i_divisor   ( reg_stage_2_divisor),
            .i_remainder ( remainder[j]),
            .i_quotient  ( quotient[j]),
            .o_dividend  ( dividend[j+1]),
            .o_remainder ( remainder[j+1]),
            .o_quotient  ( quotient[j+1])
        );
    end


    assign o_remainder = remainder[32];
    assign o_quotient = quotient[32];


endmodule

//hello 
module divu_1iter (
    input  wire [31:0] i_dividend,
    input  wire [31:0] i_divisor,
    input  wire [31:0] i_remainder,
    input  wire [31:0] i_quotient,
    output wire [31:0] o_dividend,
    output wire [31:0] o_remainder,
    output wire [31:0] o_quotient
);


    wire [31:0] tem_remainder;
    assign tem_remainder = (i_remainder<<1) | ((i_dividend >>31)& 32'h1);
    assign o_quotient = (tem_remainder < i_divisor) ? (i_quotient<<1) : (i_quotient<<1 | 32'h1);
    assign o_remainder = (tem_remainder < i_divisor) ? tem_remainder : (tem_remainder - i_divisor);
    assign o_dividend = i_dividend << 1 ;
    
endmodule
