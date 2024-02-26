/* TODO: INSERT NAME AND PENNKEY HERE */

`timescale 1ns / 1ps
`default_nettype none

/**
 * @param a first 1-bit input
 * @param b second 1-bit input
 * @param g whether a and b generate a carry
 * @param p whether a and b would propagate an incoming carry
 */
module gp1(input wire a, b,
           output wire g, p);
   assign g = a & b;
   assign p = a | b;
endmodule

/**
 * Computes aggregate generate/propagate signals over a 4-bit window.
 * @param gin incoming generate signals 
 * @param pin incoming propagate signals
 * @param cin the incoming carry
 * @param gout whether these 4 bits collectively generate a carry (ignoring cin)
 * @param pout whether these 4 bits collectively would propagate an incoming carry (ignoring cin)
 * @param cout the carry outs for the low-order 3 bits
 */
 
module gp4(input wire [3:0] gin, pin,
           input wire cin,
           output wire gout, pout,
           output wire [2:0] cout);

           assign cout[0] = gin[0]|(pin[0]&cin);
           assign cout[1] = (gin[1]|pin[1]&gin[0])|((pin[1]&pin[0])&cout[0]);
           assign cout[2] = gin[2]|(pin[2]&cout[1]);
           assign pout = pin[0]&pin[1]&pin[2]&pin[3];
           assign gout = (gin[3]|pin[3]&gin[2])|((pin[3]&pin[2])&(gin[1]|(pin[1]&gin[0])));
             
endmodule

/**
 * 16-bit Carry-Lookahead Adder
 * @param a first input
 * @param b second input
 * @param cin carry in
 * @param sum sum of a + b + carry-in
 */
module cla16
  (input wire [15:0]  a, b,
   input wire         cin,
   output wire [15:0] sum);

   wire [15:0] g_tmp;
   wire [15:0] p_tmp;
   wire [4:0] gout_tmp;
   wire [4:0] pout_tmp;
   wire [14:0] cout_tmp;
   wire [14:0] cout1;
   wire [2:0] gg;
   wire [2:0] pp;
   gp1 gtmp_0 (a[0],b[0],g_tmp[0],p_tmp[0]);
   gp1 gtmp_1 (a[1],b[1],g_tmp[1],p_tmp[1]);
   gp1 gtmp_2 (a[2],b[2],g_tmp[2],p_tmp[2]);
   gp1 gtmp_3 (a[3],b[3],g_tmp[3],p_tmp[3]);
   gp1 gtmp_4 (a[4],b[4],g_tmp[4],p_tmp[4]);
   gp1 gtmp_5 (a[5],b[5],g_tmp[5],p_tmp[5]);
   gp1 gtmp_6 (a[6],b[6],g_tmp[6],p_tmp[6]);
   gp1 gtmp_7 (a[7],b[7],g_tmp[7],p_tmp[7]);
   gp1 gtmp_8 (a[8],b[8],g_tmp[8],p_tmp[8]);
   gp1 gtmp_9 (a[9],b[9],g_tmp[9],p_tmp[9]);
   gp1 gtmp_10 (a[10],b[10],g_tmp[10],p_tmp[10]);
   gp1 gtmp_11 (a[11],b[11],g_tmp[11],p_tmp[11]);
   gp1 gtmp_12 (a[12],b[12],g_tmp[12],p_tmp[12]);
   gp1 gtmp_13 (a[13],b[13],g_tmp[13],p_tmp[13]);
   gp1 gtmp_14 (a[14],b[14],g_tmp[14],p_tmp[14]);
   gp1 gtmp_15 (a[15],b[15],g_tmp[15],p_tmp[15]);
   gp4 gtemp_fourbit_0 (g_tmp[3:0],p_tmp[3:0], cin, gout_tmp[0],pout_tmp[0],cout1[2:0]);
   gp4 gtemp_fourbit_1 (g_tmp[7:4],p_tmp[7:4], cin, gout_tmp[1],pout_tmp[1],cout_tmp[5:3]);
   gp4 gtemp_fourbit_2 (g_tmp[11:8],p_tmp[11:8], cin, gout_tmp[2],pout_tmp[2],cout_tmp[8:6]);
   gp4 gtemp_fourbit_3 (g_tmp[15:12],p_tmp[15:12], cin, gout_tmp[3],pout_tmp[3],cout_tmp[11:9]);
   gp4 gtemp_fourbit_4 (gout_tmp[3:0],pout_tmp[3:0], cin, gout_tmp[4],pout_tmp[4],cout_tmp[14:12]);

   gp4 gtemp_fourbit_5 (g_tmp[7:4],p_tmp[7:4], cout_tmp[12], gg[0],pp[0],cout1[6:4]);
   gp4 gtemp_fourbit_6 (g_tmp[11:8],p_tmp[11:8], cout_tmp[13], gg[1],pp[1],cout1[10:8]);
   gp4 gtemp_fourbit_7 (g_tmp[15:12],p_tmp[15:12], cout_tmp[14], gg[2],pp[2],cout1[14:12]);
   assign cout1[3] = cout_tmp[12];
   assign cout1[7] = cout_tmp[13];
   assign cout1[11] = cout_tmp[14];
   assign sum[0] = a[0]^b[0]^cin;
   assign sum[1] = a[1]^b[1]^cout1[0];
   assign sum[15:2] = a[15:2]^b[15:2]^cout1[14:1];
endmodule



/** Lab 2 Extra Credit, see details at
  https://github.com/upenn-acg/cis501/blob/master/lab2-alu/lab2-cla.md#extra-credit
 If you are not doing the extra credit, you should leave this module empty.
 */
module gpn
  #(parameter N = 4)
  (input wire [N-1:0] gin, pin,
   input wire  cin,
   output wire gout, pout,
   output wire [N-2:0] cout);
 
endmodule
