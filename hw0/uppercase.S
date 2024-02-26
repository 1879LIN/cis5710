        .section .data
input_string:   .asciz "Hello world!"

        # tohost/fromhost are data locations used by Spike
        .align 8
tohost: .byte 0
        .align 8
fromhost: .byte 0

        
        .section .text
        .globl _start
_start:
        # Load the address of the input string into a0
        la a0, input_string

        # Loop over each character of the string
uppercase_loop:
    # Load the current character from the string into a1
    lb a1, 0(a0)

    # Check for null terminator to end the loop
    beqz a1, end_program

    # Check if the character is a lowercase letter
    li a2, 'a'
    blt a1, a2, not_lowercase
    li a2, 'z'
    bgt a1, a2, not_lowercase

    # Convert the character to uppercase by subtracting 32
    li a3, 32
    sub a1, a1, a3

    # Store the uppercase character back into the string
    sb a1, 0(a0)

not_lowercase:
    # Move to the next character
    addi a0, a0, 1

    # Jump back to the start of the loop
    j uppercase_loop

        
end_program:
        # jump here when you're done
        j end_program

