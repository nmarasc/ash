# This is not going to end well

  .text
  .global _start
  .equ EXIT, 1

_start:
  ldr r0, [sp]
  add r1,sp,#4
  add r2,r1,r0,LSL #2
  add r2,r2,#4
  bl main
  mov r7, #EXIT
  svc 0

# prcedure main - do a shell thing
# parameters:
#   r0 - int argc, number of command line parameters
#   r1 - char *argv[], pointer to array of command line strings
#   r2 - char *envp[], pointer to array of environment strings
# returns:
#   r0 - exit code (should be zero)
main:
  push {r4-r6,lr}
  mov r4,r0
  mov r5,r1
  mov r6,r2
  mov r0,#1
  mov r1,r6
  bl print_array
  mov r0,#0
  pop {r4-r6,pc}

# procedure print_array - prints the content of and array
# parameters:
#   r0 - file descriptor
#   r1 - string array pointer
# returns nothing
print_array:
  push {r4,r5,lr}
  mov r4,r0
  mov r5,r1
0:
  ldr r1,[r5],#4
  cmp r1,#0
  popeq {r4,r5,pc}
  mov r0,r4
  bl println
  bal 0b
