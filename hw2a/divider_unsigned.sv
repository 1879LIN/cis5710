/* INSERT NAME AND PENNKEY HERE */

`timescale 1ns / 1ns

// quotient = dividend / divisor

module divider_unsigned (
    input  wire [31:0] i_dividend,
    input  wire [31:0] i_divisor,
    output wire [31:0] o_remainder,
    output wire [31:0] o_quotient
);

    // TODO: your code here
      wire [31:0] o_dividend_tem0;
      wire [31:0] o_remainder_tem0;
      wire [31:0] o_quotient_tem0;
      
      wire [31:0] o_dividend_tem1;
      wire [31:0] o_remainder_tem1;
      wire [31:0] o_quotient_tem1;
      
      wire [31:0] o_dividend_tem2;
      wire [31:0] o_remainder_tem2;
      wire [31:0] o_quotient_tem2;
      
      wire [31:0] o_dividend_tem3;
      wire [31:0] o_remainder_tem3;
      wire [31:0] o_quotient_tem3;
      
      wire [31:0] o_dividend_tem4;
      wire [31:0] o_remainder_tem4;
      wire [31:0] o_quotient_tem4;
      
      wire [31:0] o_dividend_tem5;
      wire [31:0] o_remainder_tem5;
      wire [31:0] o_quotient_tem5;
      
      wire [31:0] o_dividend_tem6;
      wire [31:0] o_remainder_tem6;
      wire [31:0] o_quotient_tem6;
      
      wire [31:0] o_dividend_tem7;
      wire [31:0] o_remainder_tem7;
      wire [31:0] o_quotient_tem7;
      
      wire [31:0] o_dividend_tem8;
      wire [31:0] o_remainder_tem8;
      wire [31:0] o_quotient_tem8;
      
      wire [31:0] o_dividend_tem9;
      wire [31:0] o_remainder_tem9;
      wire [31:0] o_quotient_tem9;
      
      wire [31:0] o_dividend_tem10;
      wire [31:0] o_remainder_tem10;
      wire [31:0] o_quotient_tem10;
      
      wire [31:0] o_dividend_tem11;
      wire [31:0] o_remainder_tem11;
      wire [31:0] o_quotient_tem11;
      
      wire [31:0] o_dividend_tem12;
      wire [31:0] o_remainder_tem12;
      wire [31:0] o_quotient_tem12;
      
      wire [31:0] o_dividend_tem13;
      wire [31:0] o_remainder_tem13;
      wire [31:0] o_quotient_tem13;
      
      wire [31:0] o_dividend_tem14;
      wire [31:0] o_remainder_tem14;
      wire [31:0] o_quotient_tem14;
      
      wire [31:0] o_dividend_tem15;
      wire [31:0] o_remainder_tem15;
      wire [31:0] o_quotient_tem15;
      
      wire [31:0] o_dividend_tem16;
      wire [31:0] o_remainder_tem16;
      wire [31:0] o_quotient_tem16;
      
      wire [31:0] o_dividend_tem17;
      wire [31:0] o_remainder_tem17;
      wire [31:0] o_quotient_tem17;
      
      wire [31:0] o_dividend_tem18;
      wire [31:0] o_remainder_tem18;
      wire [31:0] o_quotient_tem18;
      
      wire [31:0] o_dividend_tem19;
      wire [31:0] o_remainder_tem19;
      wire [31:0] o_quotient_tem19;
      
      wire [31:0] o_dividend_tem20;
      wire [31:0] o_remainder_tem20;
      wire [31:0] o_quotient_tem20;
     
      wire [31:0] o_dividend_tem21;
      wire [31:0] o_remainder_tem21;
      wire [31:0] o_quotient_tem21;

      wire [31:0] o_dividend_tem22;
      wire [31:0] o_remainder_tem22;
      wire [31:0] o_quotient_tem22;

      wire [31:0] o_dividend_tem23;
      wire [31:0] o_remainder_tem23;
      wire [31:0] o_quotient_tem23;

      wire [31:0] o_dividend_tem24;
      wire [31:0] o_remainder_tem24;
      wire [31:0] o_quotient_tem24;

      wire [31:0] o_dividend_tem25;
      wire [31:0] o_remainder_tem25;
      wire [31:0] o_quotient_tem25;

      wire [31:0] o_dividend_tem26;
      wire [31:0] o_remainder_tem26;
      wire [31:0] o_quotient_tem26;

      wire [31:0] o_dividend_tem27;
      wire [31:0] o_remainder_tem27;
      wire [31:0] o_quotient_tem27;

      wire [31:0] o_dividend_tem28;
      wire [31:0] o_remainder_tem28;
      wire [31:0] o_quotient_tem28;

      wire [31:0] o_dividend_tem29;
      wire [31:0] o_remainder_tem29;
      wire [31:0] o_quotient_tem29;

      wire [31:0] o_dividend_tem30;
      wire [31:0] o_remainder_tem30;
      wire [31:0] o_quotient_tem30;

      wire [31:0] o_dividend_tem31;
      wire [31:0] o_remainder_tem31;
      wire [31:0] o_quotient_tem31;

      divu_1iter it0(i_dividend, i_divisor, 32'b0, 32'b0, o_dividend_tem0, o_remainder_tem0, o_quotient_tem0);
      divu_1iter it1(o_dividend_tem0, i_divisor, o_remainder_tem0, o_quotient_tem0, o_dividend_tem1, o_remainder_tem1, o_quotient_tem1);
      divu_1iter it2(o_dividend_tem1, i_divisor, o_remainder_tem1, o_quotient_tem1, o_dividend_tem2, o_remainder_tem2, o_quotient_tem2);
      divu_1iter it3(o_dividend_tem2, i_divisor, o_remainder_tem2, o_quotient_tem2, o_dividend_tem3, o_remainder_tem3, o_quotient_tem3);
      divu_1iter it4(o_dividend_tem3, i_divisor, o_remainder_tem3, o_quotient_tem3, o_dividend_tem4, o_remainder_tem4, o_quotient_tem4);
      divu_1iter it5(o_dividend_tem4, i_divisor, o_remainder_tem4, o_quotient_tem4, o_dividend_tem5, o_remainder_tem5, o_quotient_tem5);
      divu_1iter it6(o_dividend_tem5, i_divisor, o_remainder_tem5, o_quotient_tem5, o_dividend_tem6, o_remainder_tem6, o_quotient_tem6);
      divu_1iter it7(o_dividend_tem6, i_divisor, o_remainder_tem6, o_quotient_tem6, o_dividend_tem7, o_remainder_tem7, o_quotient_tem7);
      divu_1iter it8(o_dividend_tem7, i_divisor, o_remainder_tem7, o_quotient_tem7, o_dividend_tem8, o_remainder_tem8, o_quotient_tem8);
      divu_1iter it9(o_dividend_tem8, i_divisor, o_remainder_tem8, o_quotient_tem8, o_dividend_tem9, o_remainder_tem9, o_quotient_tem9);
      divu_1iter it10(o_dividend_tem9, i_divisor, o_remainder_tem9, o_quotient_tem9, o_dividend_tem10, o_remainder_tem10, o_quotient_tem10);
      divu_1iter it11(o_dividend_tem10, i_divisor, o_remainder_tem10, o_quotient_tem10, o_dividend_tem11, o_remainder_tem11, o_quotient_tem11);
      divu_1iter it12(o_dividend_tem11, i_divisor, o_remainder_tem11, o_quotient_tem11, o_dividend_tem12, o_remainder_tem12, o_quotient_tem12);
      divu_1iter it13(o_dividend_tem12, i_divisor, o_remainder_tem12, o_quotient_tem12, o_dividend_tem13, o_remainder_tem13, o_quotient_tem13);
      divu_1iter it14(o_dividend_tem13, i_divisor, o_remainder_tem13, o_quotient_tem13, o_dividend_tem14, o_remainder_tem14, o_quotient_tem14);
      divu_1iter it15(o_dividend_tem14, i_divisor, o_remainder_tem14, o_quotient_tem14, o_dividend_tem15, o_remainder_tem15, o_quotient_tem15);
      
      divu_1iter it16(o_dividend_tem15, i_divisor, o_remainder_tem15, o_quotient_tem15, o_dividend_tem16, o_remainder_tem16, o_quotient_tem16);
      divu_1iter it17(o_dividend_tem16, i_divisor, o_remainder_tem16, o_quotient_tem16, o_dividend_tem17, o_remainder_tem17, o_quotient_tem17);
      divu_1iter it18(o_dividend_tem17, i_divisor, o_remainder_tem17, o_quotient_tem17, o_dividend_tem18, o_remainder_tem18, o_quotient_tem18);
      divu_1iter it19(o_dividend_tem18, i_divisor, o_remainder_tem18, o_quotient_tem18, o_dividend_tem19, o_remainder_tem19, o_quotient_tem19);
      divu_1iter it20(o_dividend_tem19, i_divisor, o_remainder_tem19, o_quotient_tem19, o_dividend_tem20, o_remainder_tem20, o_quotient_tem20);
      divu_1iter it21(o_dividend_tem20, i_divisor, o_remainder_tem20, o_quotient_tem20, o_dividend_tem21, o_remainder_tem21, o_quotient_tem21);
      divu_1iter it22(o_dividend_tem21, i_divisor, o_remainder_tem21, o_quotient_tem21, o_dividend_tem22, o_remainder_tem22, o_quotient_tem22);
      divu_1iter it23(o_dividend_tem22, i_divisor, o_remainder_tem22, o_quotient_tem22, o_dividend_tem23, o_remainder_tem23, o_quotient_tem23);
      divu_1iter it24(o_dividend_tem23, i_divisor, o_remainder_tem23, o_quotient_tem23, o_dividend_tem24, o_remainder_tem24, o_quotient_tem24);
      divu_1iter it25(o_dividend_tem24, i_divisor, o_remainder_tem24, o_quotient_tem24, o_dividend_tem25, o_remainder_tem25, o_quotient_tem25);
      divu_1iter it26(o_dividend_tem25, i_divisor, o_remainder_tem25, o_quotient_tem25, o_dividend_tem26, o_remainder_tem26, o_quotient_tem26);
      divu_1iter it27(o_dividend_tem26, i_divisor, o_remainder_tem26, o_quotient_tem26, o_dividend_tem27, o_remainder_tem27, o_quotient_tem27);
      divu_1iter it28(o_dividend_tem27, i_divisor, o_remainder_tem27, o_quotient_tem27, o_dividend_tem28, o_remainder_tem28, o_quotient_tem28);
      divu_1iter it29(o_dividend_tem28, i_divisor, o_remainder_tem28, o_quotient_tem28, o_dividend_tem29, o_remainder_tem29, o_quotient_tem29);
      divu_1iter it30(o_dividend_tem29, i_divisor, o_remainder_tem29, o_quotient_tem29, o_dividend_tem30, o_remainder_tem30, o_quotient_tem30);
      divu_1iter it31(o_dividend_tem30, i_divisor, o_remainder_tem30, o_quotient_tem30, o_dividend_tem31, o_remainder, o_quotient);


endmodule


module divu_1iter (
    input  wire [31:0] i_dividend,
    input  wire [31:0] i_divisor,
    input  wire [31:0] i_remainder,
    input  wire [31:0] i_quotient,
    output wire [31:0] o_dividend,
    output wire [31:0] o_remainder,
    output wire [31:0] o_quotient
);
  /*
    for (int i = 0; i < 32; i++) {
        remainder = (remainder << 1) | ((dividend >> 31) & 0x1);
        if (remainder < divisor) {
            quotient = (quotient << 1);
        } else {
            quotient = (quotient << 1) | 0x1;
            remainder = remainder - divisor;
        }
        dividend = dividend << 1;
    }
    */

    // TODO: your code here
      wire [31:0] tem_remainder;
      assign tem_remainder = (i_remainder<<1) | ((i_dividend >>31)& 32'h1);
      assign o_quotient = (tem_remainder < i_divisor) ? (i_quotient<<1) : (i_quotient<<1 | 32'h1);
      assign o_remainder = (tem_remainder < i_divisor) ? tem_remainder : (tem_remainder - i_divisor);
      assign o_dividend = i_dividend << 1 ;
endmodule
