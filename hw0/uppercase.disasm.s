
uppercase.bin:     file format elf32-littleriscv


Disassembly of section .text:

00010074 <_start>:
   10074:	ffff2517          	auipc	a0,0xffff2
   10078:	f8c50513          	addi	a0,a0,-116 # 2000 <__DATA_BEGIN__>

0001007c <uppercase_loop>:
   1007c:	00050583          	lb	a1,0(a0)
   10080:	02058463          	beqz	a1,100a8 <end_program>
   10084:	06100613          	li	a2,97
   10088:	00c5cc63          	blt	a1,a2,100a0 <not_lowercase>
   1008c:	07a00613          	li	a2,122
   10090:	00b64863          	blt	a2,a1,100a0 <not_lowercase>
   10094:	02000693          	li	a3,32
   10098:	40d585b3          	sub	a1,a1,a3
   1009c:	00b50023          	sb	a1,0(a0)

000100a0 <not_lowercase>:
   100a0:	00150513          	addi	a0,a0,1
   100a4:	fd9ff06f          	j	1007c <uppercase_loop>

000100a8 <end_program>:
   100a8:	0000006f          	j	100a8 <end_program>
