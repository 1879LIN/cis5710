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
