# utility procedures for use in ARM assembly language programs
#   strlen: get the length of a null-terminated string
#   strcmp: compare two strings lexicographically ("dictionary" order)
#   print: write a null-terminated string to a given file descriptor
#   println: print a null-terminated string followed by a newline
#   newline: write a newline to a given file descriptor
#   atoi: convert a null-terminated string of ASCII digits to a 32-bit number
#   itoa: convert a 32-bit number into a null-terminated string of ASCII digits
#   udiv10: unsigned division by 10, giving a quotient and remainder
#   qr: generic division, giving a quotient and remainder

  .text
  .global strlen
  .global strcmp
  .global print
  .global println
  .global newline
  .global atoi
  .global itoa
  .global udiv10
  .global qr


  .equ WRITE, 4

# procedure print - write a null-terminated string
# parameters:
#   r0 - file descriptor to write to
#   r1 - the address of the string to write
# returns:
#   r0 - the original address in parameter r1
print:
  push {r1, r5, r6, r7, lr}
  mov r5, r0              @ save the file descriptor
  mov r6, r1              @ save the string address
  mov r0, r1              @ prepare to call strlen
  bl strlen               @ get string length in r0
  mov r2, r0              @ number of characters to write in r2
  mov r0, r5              @ file descriptor
  mov r1, r6              @ string address
  mov r7, #WRITE          @ make a WRITE system call
  svc 0
  pop {r0, r5, r6, r7, pc}

# procedure strlen - get the length of a null-terminated byte string
# parameters:
#   r0 - address of string
# returns:
#   r0 - number of characters in string (not including the null byte)
strlen:
  mov r1, r0              @ address of string
  mov r0, #0              @ initialize length
0:
  ldrb r2, [r1], #1       @ get next character
  cmp r2, #0              @ is it null?
  moveq pc, lr            @ if so, exit with count in r0
  add r0, #1              @ otherwise increment length ...
  bal 0b                  @ ... and go back for more

# procedure newline - print a newline to the given file descriptor
# parameters:
#   r0 - file descriptor to write to
# returns nothing
newline:
  push {r7, lr}
  adr r1, NL              @ address of newline string
  mov r2, #1              @ one character to write
  mov r7, #WRITE          @ make a WRITE system call
  svc 0
  pop {r7, pc}

NL:  .ascii "\n"
  .align 2

# procedure println - print a string followed by a newline to a given file descriptor
# parameters
#   r0 - file descriptor to write to
#   r1 - string to write
# returns
#   r0 - address of original string in r1
println:
  push {r1, r4, lr}
  mov r4, r0              @ save file descriptor
  bl print                @ print the string
  mov r0, r4              @ retrieve file descriptor
  bl newline
  pop {r0, r4, pc}

# procedure atoi
# parameters:
#   r0: the address of a null-terminated string of ASCII decimal digits,
#       optionally beginning with an initial minus '-' sign
# returns:
#   r0: the 32-bit signed integer representation of the input string
atoi:
  mov r2, r0              @ save string address
  mov r3, #0              @ clear +/- flag
  mov r0, #0              @ running sum
  ldrb r1, [r2], #1       @ check first character for '-'
  cmp r1, #'-
  moveq r3, #1            @ set +/- flag
  bne 2f                  @ not a '-', so check for r1 being a digit
# if we get here, the first character is a '-', so get the next char
1:
  ldrb r1, [r2], #1       @ get next char and advance
2:
  subs r1, r1, #'0        @ check for decimal digit
  blt 9f                  @ not a decimal digit (possibly null), so quit
  cmp r1, #10
  bge 9f                  @ not a decimal digit, so quit
  mov r0, r0, LSL #1      @ multiply running sum ...
  add r0, r0, r0, LSL #2  @ ... by 10
  add r0, r0, r1          @ and add the digit to running sum
  bal 1b                  @ go back for more
9:
  cmp r3, #0              @ check the +/- flag
  rsbne r0, r0, #0        @ change sign if '-' flag is set
  mov pc, lr

# itoa -- convert a 32-bit signed integer integer to an ASCII
# string of decimal digits, possibly preceded by a minus '-' sign
# parameters
#   r0 -- integer to convert (n)
#   r1 -- address of string to write to (s)
# returns
#   r0 -- address of string
itoa:
  push {r1, r4, lr}
  mov r4, r1              @ save buffer address in r4 ...
  mov r2, #0              @ push null terminator on stack
  push {r2}
  cmp r0, #0              @ is n<0?
  movlt r2, #'-           @ if so, plunk a minus sign ...
  strltb r2, [r4], #1     @ ... in buffer, advance to next char, and ...
  rsblt r0, r0, #0        @ ... negate r0 for further processing
# get the successive remainders of the dividend (in r0) upon division by 10,
# and push them on the stack until the dividend becomes zero
8:
  bl udiv10               @ returns with r0=quotient,
                          @ r1=remainder
  add r1, r1, #'0         @ convert remainder to ASCII ...
  push {r1}               @ ... and push on stack
  cmp r0, #0              @ is dividend zero?
  bne 8b                  @ no, go back for more
# at this point, all of the characters of the string are on the stack,
# so unstack them (including the null sentinel) and put them into the
# buffer pointed to by r4
9:
  pop {r1}                @ next digit (or null) to deposit
  strb r1, [r4], #1       @ put in buffer and advance buffer pointer
  cmp r1, #0              @ is it null?
  bne 9b                  @ go back for more digits if no null
  pop {r0, r4, pc}

# udiv10 -- unsigned divide by 10
# parameters:
#   r0 - dividend
# returns
#   r0 - quotient
#   r1 - remainder
udiv10:
  mov r3, r0              @ save dividend (n)
  ldr r1, =0x1999999a     @ 2^32/10
  sub r0, r0, r0, lsr #30 @ fix for large values
  umull r2, r0, r1, r0    @ quotient in r0 (q)
  ldr r2, =10
  mul r1, r0, r2
  sub r1, r3, r1          @ remainder in r1 (r1 = r3 - 10*r0)
  mov pc, lr

# procedure qr - get quotient and remainder
# parameters:
#   r0 - signed or unsigned 32-bit dividend
#   r1 - signed or unsigned 32-bit divisor
# returns:
#   r0 - quotient
#   r1 - remainder
# If applied to signed 2's complement signed numbers,
# the divisor must be positive and the dividend must be nonnegative.
# If applied to unsigned numbers, the divisor must be nonzero.
qr:
  cmp r1, #0              @ check for zero divisor
  moveq r0, #0
  mvneq r1, #0            @ -1
  moveq pc, lr            @ return (0, -1) if divisor is zero
  clz ip, r1              @ shift count for divisor
  mov r3, r1              @ save divisor
  mov r1, r0              @ initialize remainder to dividend
  mov r0, #0              @ initialze quotient to zero
1:
  subs r2, r1, r3, LSL ip @ try to subtract shifted divisor ...
                          @ ... from current remainder
  adc r0, r0, r0          @ shift quotient ; if subtract OK ...
                          @ (carry is 1), set bit 1 of quotient ...
  movcs r1, r2            @ ... and update remainder
  subs ip, ip, #1         @ decrement shift count
  bge 1b                  @ loop if any shifts (including 0) are left
  mov pc, lr              @ otherwise return

# procedure strcmp - compare two null-terminated character strings
# parameters:
#   r0 - first string address
#   r1 - second string address
# returns:
#   r0 - <0 (first<second), 0 (first=second), >0 (first>second)
strcmp:
1:
  ldrb r2, [r0], #1       @ first char
  ldrb r3, [r1], #1       @ second char
  subs ip, r2, r3
  bne 2f
  cmp r2, #0
  bne 1b                  @ continue if equal but not at end-of-string
2:
  mov r0, ip
  mov pc, lr

# procedure strcpy - copy a string from source to destination
# parameters:
#   r0 - destination string address
#   r1 - source string address
# returns nothing
strcpy:
  ldrb ip, [r1], #1       @ get char from source
  strb ip, [r0], #1       @ put char to destination
  cmp ip, #0              @ are we finished?
  bne strcpy
  mov pc, lr
