# This is not going to end well

  .text
  .global _start
  .equ EXIT, 1
  .equ STDOUT, 1

_start:
  ldr r0, [sp]            @ grab argc
  add r1,sp,#4            @ get pointer to argv
  add r2,r1,r0,LSL #2     @ skip to end of argv
  add r2,r2,#4            @ get pointer to envp
  bl main                 @ call main
  mov r7, #EXIT           @ make exit system call
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
  mov r4,r0               @ save argc
  mov r5,r1               @ save argv
  mov r6,r2               @ save envp
  mov r0,#STDOUT          @ arg1: stdout
  mov r1,r6               @ arg2: envp
  bl print_array          @ call print_array
  mov r0,#0               @ success exit code
  pop {r4-r6,pc}

# procedure print_array - prints the content of and array
# parameters:
#   r0 - file descriptor
#   r1 - string array pointer
# returns nothing
print_array:
  push {r4,r5,lr}
  mov r4,r0               @ save fd
  mov r5,r1               @ save array pointer
0:
  ldr r1,[r5],#4          @ load element and increment
  cmp r1,#0               @ check for end of array
  popeq {r4,r5,pc}        @ return if at end
  mov r0,r4               @ arg1: fd
  bl println              @ call println
  bal 0b                  @ repeat
