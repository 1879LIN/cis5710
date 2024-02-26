`timescale 1ns / 1ps

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
 * @param gout whether these 4 bits internally would generate a carry-out (independent of cin)
 * @param pout whether these 4 bits internally would propagate an incoming carry from cin
 * @param cout the carry outs for the low-order 3 bits
 */
module gp4(input wire [3:0] gin, pin,
           input wire cin,
           output wire gout, pout,
           output wire [2:0] cout);

   // TODO: your code here
           assign cout[0] = gin[0] | (pin[0] & cin);
           assign cout[1] = gin[1] | (pin[1] & (gin[0] | (pin[0] & cin)));
           assign cout[2] = gin[2] | (pin[2] & (gin[1] | (pin[1] & (gin[0] | (pin[0] & cin)))));
           assign pout = pin[0]&pin[1]&pin[2]&pin[3];
           assign gout = (gin[3]|pin[3]&gin[2])|((pin[3]&pin[2])&(gin[1]|(pin[1]&gin[0])));
endmodule

/** Same as gp4 but for an 8-bit window instead */
module gp8(input wire [7:0] gin, pin,
           input wire cin,
           output wire gout, pout,
           output wire [6:0] cout);

   // TODO: your code here
           assign cout[0] = gin[0] | (pin[0] & cin);
           assign cout[1] = gin[1] | (pin[1] & (gin[0] | (pin[0] & cin)));
           assign cout[2] = gin[2] | (pin[2] & (gin[1] | (pin[1] & (gin[0] | (pin[0] & cin)))));
           assign cout[3] = gin[3] | (pin[3] & (gin[2] | (pin[2] & (gin[1] | (pin[1] & (gin[0] | (pin[0] & cin)))))));
           assign cout[4] = gin[4] | (pin[4] & (gin[3] | (pin[3] & (gin[2] | (pin[2] & (gin[1] | (pin[1] & (gin[0] | (pin[0] & cin)))))))));
           assign cout[5] = gin[5] | (pin[5] & (gin[4] | (pin[4] & (gin[3] | (pin[3] & (gin[2] | (pin[2] & (gin[1] | (pin[1] & (gin[0] | (pin[0] & cin)))))))))));
           assign cout[6] = gin[6] | (pin[6] & (gin[5] | (pin[5] & (gin[4] | (pin[4] & (gin[3] | (pin[3] & (gin[2] | (pin[2] & (gin[1] | (pin[1] & (gin[0] | (pin[0] & cin)))))))))))));
// Compute overall block generate and propagate
           assign pout = &pin; // AND reduction of pin signals
           assign gout = gin[7] | (pin[7] & cout[6]);
endmodule

module cla
  (input wire [31:0]  a, b,
   input wire         cin,
   output wire [31:0] sum);

   // TODO: your code here
// Intermediate signals for gp1 outputs
    wire [31:0] g, p;
    // gp4 outputs
    wire [7:0] g4, p4;
    wire [30:0] cout;
    wire gout,pout;
    wire [30:0] coutt;
    // gp8 outputs
    wire [3:0] g8, p8;
    wire [6:0] cout8,g4t,p4t;

    // Manually instantiated gp1 modules for each bit
    gp1 gp1_0(a[0], b[0], g[0], p[0]); gp1 gp1_1(a[1], b[1], g[1], p[1]);
    gp1 gp1_2(a[2], b[2], g[2], p[2]); gp1 gp1_3(a[3], b[3], g[3], p[3]);
    gp1 gp1_4(a[4], b[4], g[4], p[4]); gp1 gp1_5(a[5], b[5], g[5], p[5]);
    gp1 gp1_6(a[6], b[6], g[6], p[6]); gp1 gp1_7(a[7], b[7], g[7], p[7]);
    gp1 gp1_8(a[8], b[8], g[8], p[8]); gp1 gp1_9(a[9], b[9], g[9], p[9]);
    gp1 gp1_10(a[10], b[10], g[10],p[10]); gp1 gp1_11(a[11], b[11], g[11], p[11]);
    gp1 gp1_12(a[12], b[12], g[12], p[12]); gp1 gp1_13(a[13], b[13], g[13], p[13]);
    gp1 gp1_14(a[14], b[14], g[14], p[14]); gp1 gp1_15(a[15], b[15], g[15], p[15]);
    gp1 gp1_16(a[16], b[16], g[16], p[16]); gp1 gp1_17(a[17], b[17], g[17], p[17]);
    gp1 gp1_18(a[18], b[18], g[18], p[18]); gp1 gp1_19(a[19], b[19], g[19], p[19]);
    gp1 gp1_20(a[20], b[20], g[20], p[20]); gp1 gp1_21(a[21], b[21], g[21], p[21]);
    gp1 gp1_22(a[22], b[22], g[22], p[22]); gp1 gp1_23(a[23], b[23], g[23], p[23]);
    gp1 gp1_24(a[24], b[24], g[24], p[24]); gp1 gp1_25(a[25], b[25], g[25], p[25]);
    gp1 gp1_26(a[26], b[26], g[26], p[26]); gp1 gp1_27(a[27], b[27], g[27], p[27]);
    gp1 gp1_28(a[28], b[28], g[28], p[28]); gp1 gp1_29(a[29], b[29], g[29], p[29]);
    gp1 gp1_30(a[30], b[30], g[30], p[30]); gp1 gp1_31(a[31], b[31], g[31], p[31]);
      
    gp4 gp4_0_temp(g[3:0], p[3:0], cin, g4[0], p4[0], cout[2:0]);
    gp4 gp4_1_temp(g[7:4], p[7:4], cout8[0], g4[1], p4[1], cout[6:4]);
    gp4 gp4_2_temp(g[11:8], p[11:8], cout8[1], g4[2], p4[2], cout[10:8]);
    gp4 gp4_3_temp(g[15:12], p[15:12], cout8[2], g4[3], p4[3], cout[14:12]);
    gp4 gp4_4_temp(g[19:16], p[19:16], cout8[3], g4[4], p4[4], cout[18:16]);
    gp4 gp4_5_temp(g[23:20], p[23:20], cout8[4], g4[5], p4[5], cout[22:20]);
    gp4 gp4_6_temp(g[27:24], p[27:24], cout8[5], g4[6], p4[6], cout[26:24]);
    gp4 gp4_7_temp(g[31:28], p[31:28], cout8[6], g4[7], p4[7], cout[30:28]);

    gp8 gp8_0(g4[7:0], p4[7:0], cin, gout, pout, cout8[6:0]);
    
    
    assign cout[3] = cout8[0];
    assign cout[7] = cout8[1];
    assign cout[11] = cout8[2];
    assign cout[15] = cout8[3];
    assign cout[19] = cout8[4];
    assign cout[23] = cout8[5];
    assign cout[27] = cout8[6];
    
    // Manually calculate sum for each bit
    assign sum[0] = a[0] ^ b[0] ^ cin;
    assign sum[31:1] = a[31:1] ^ b[31:1] ^ cout[30:0];
endmodule
