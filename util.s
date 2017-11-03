# utility procedures for use in ARM assembly language programs
#   strlen:    get the length of a null-terminated string
#   strcmp:    compare two strings lexicographically ("dictionary" order)
#   print:     write a null-terminated string to a given file descriptor
#   println:   print a null-terminated string followed by a newline
#   newline:   write a newline to a given file descriptor
#   atoi:      convert a null-terminated string of ASCII digits to a 32-bit number
#   itoa:      convert a 32-bit number into a null-terminated string of ASCII digits
#   udiv10:    unsigned division by 10, giving a quotient and remainder
#   qr:        generic division, giving a quotient and remainder
#	  heap_init: initialize the free memory list in the heap
#	  mem_alloc: allocate a block of memory of a given size
#	  mem_free:  add an allocated block back to the free list

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
	.global mem_alloc
	.global mem_free
	.global heap_init
	.global heapbase
	.global heapsize

	.equ MEMFREE, 1024*1024*4	@ number of free bytes in heap
	.equ HMASK, 0x07	@ mask for header alignment
	.equ HSIZE, HMASK+1	@ header size

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

# procedure heap_init -- initialize the free memory list in the heap
# parameters: none
# returns nothing
heap_init:
	ldr r3, =heapsize
	ldr ip, [r3]
	cmp ip, #0		@ is the heap already initialized?
	movne pc, lr		@ if so, do nothing
	ldr ip, =MEMFREE
	bic ip, ip, #HMASK	@ round down to multiple of 8
	str ip, [r3]
	mov r3, ip		@ heap size in bytes
	ldr r0, =heapbase
	ldr r0, [r0]		@ address of left sentinel
# set up header sentinel
	add r1, r0, #HSIZE	@ address of first "real" block
	str r1, [r0]		@ put ptr to block in left sentinel
	mov ip, #0
	str ip, [r0, #4]	@ avail size in left sentinel (always zero)
	sub r3, r3, #2*HSIZE	@ distance from block to right sentinel
	add r2, r1, r3		@ address of right sentinel
	sub r3, r3, #HSIZE	@ avail size in block
	str r2, [r1]		@ ptr to right sentinel in block header
	str r3, [r1, #4]	@ block size in block header
	mov ip, #0
	str ip, [r2]
	str ip, [r2, #4]	@ put right sentinal pointer (null) ...
				@ ... and size (zero) in sentinel header
	mov pc, lr		@ return

# procedure mem_alloc -- allocate a block of memory of a given size
# parameters:
#   r0 - the (unsigned) size of the block requested -- must be > 0
# returns:
#   r0 - the 32-bit address of the free space in the allocated memory block
#        or null (0) if the allocation failed
# Each block header consists of two words: the first is a pointer (next)
# to the next block header, and the second is an integer (avail) of
# free bytes in the block, not including the header.  The header is
# stored as the first two words of the block.  All allocations are made
# as multiples of HSIZE.
# If the heap is uninitialized upon entry, heap_init will be called before
# any allocations are made.
mem_alloc:
	cmp r0, #0
	moveq pc, lr		@ return if zero bytes requested
	ldr ip, =heapsize	@ address of heap size
	ldr ip, [ip]		@ value of heap size
	cmp ip, #0		@ if zero, must initialize
	bne 0f
	push {r0, lr}
	bl heap_init		@ initialize the heap
	pop {r0, lr}
0:
	add r0, r0, #HMASK	@ round up request to next header alignment
	bic r0, r0, #HMASK
	ldr r1, =heapbase	@ address of ptr to heap base
	ldr r1, [r1]		@ prev ptr (initially the LH sentinel)
	ldr r2, [r1]		@ curr ptr (the block to the right of prev)
1:
	ldr r3, [r2, #4]	@ curr avail size
	cmp r3, #0		@ are we at the end of the free list?
	moveq r0, #0		@ if so, return null (no blocks avail)
	moveq pc, lr
	subs r3, r3, r0		@ difference between request and avail
	movlo r1, r2		@ if too small, prev gets curr, and ...
	ldrlo r2, [r2]		@ ... curr gets next block in list ...
	blo 1b			@ ... and continue the search
# At this point, curr (r2) is  of sufficient size to meet the request.
# If what's left (r3) is less than or equal to the header size, we just use
# the current block
	cmp r3, #HSIZE		@ if all we have left is a header ...
	ldrls ip, [r2]		@ ... move curr next ptr to ...
	strls ip, [r1]		@ ... prev next ptr ...
	bls 9f			@ ... and clean up
# If we get here, the block is not an exact match, so we carve up
# the curr block appropriately -- no need to fix the prev header (r1)
#   r0 - request size (rounded up to nearest allocation alignment)
#   r2 - curr ptr
#   r3 - size of block left over after allocation (including the header)
	mov r1, r2		@ save ptr to curr block
	add r2, r2, r3		@ ptr to beginning of new allocation block
	sub ip, r3, #HSIZE	@ size of what's left in the curr block
	str ip, [r1, #4]	@ store new avail size in curr
	str r0, [r2, #4]	@ put allocation size in new block header
# At this point, r2 is the address of the header of the block to allocate.
# The size of the block has already been set, so we make the next field
# of the block header point to itself and return the address
# of the data in r0
9:
	str r2, [r2]		@ make block header point to itself!
	add r0, r2, #HSIZE	@ move past the header
	mov pc, lr		@ and return

# procedure mem_free - add an allocated block back to the free list
# parameters:
#   r0 - address of allocated memory returned by previous call to mem_alloc
# returns:
#   r0 - 0 if successful, -1 if not
mem_free:
	push {r4, r5, lr}
	sub r1, r0, #HSIZE	@ addr. of block to be freed, including hdr
	ands ip, r1, #HMASK	@ lower three bits must be zero
	bne 9f			@ error exit
# Set r0 to the LH sentinel block and r2 to the RH sentinel block
	ldr r0, =heapbase
	ldr r0, [r0]		@ r0 points to the LH sentinel block
	ldr ip, =heapsize
	ldr ip, [ip]		@ the size of the heap
	sub ip, ip, #HSIZE	@ adjust for the RH sentinel block header
	add r2, r0, ip		@ r2 points to the RH sentinel block
# First see if r1 is strictly within the heap area
	cmp r0, r1		@ LH sentinel address must be lower than r1
	bhs 9f
	cmp r1, r2		@ r1 must be lower than RH sentinel address
	bhs 9f			@ error exit
# Now do some sanity checks on the r1 block
	ldr ip, [r1]		@ ip must be r1
	cmp ip, r1
	bne 9f			@ error exit
	ldr r4, [r1, #4]	@ get the size of the block to free
	ands ip, r4, #HMASK	@ the size must be a multiple of 8
	bne 9f			@ error exit if not
	cmp r4, #0		@ and must be nonzero
	beq 9f			@ error exit otherwise
# At this point, r1 points to a block whose address is between the
# LH sentinel and the RH sentinel, so we chase down the free list
# starting at the LH sentinel to see where this block should be placed.
# Note that r0 initially points to the LH sentinel block (prev) and
# that r0 is less than r1.
# We stop our search when r2, the address of the 'curr' block,
# is greater than r1.  Since r1 is known to be less than the address
# of the RH sentinel block, which is at the end of the linked list,
# this search is guaranteed to succeed.
	ldr r2, [r0]		@ r2 = the next unallocated block (curr)
1:	cmp r1, r2		@ is r1 less than curr?
	movhs r0, r2		@ if not, ...
	ldrhs r2, [r0]		@ ... move to next prev/curr pair ...
	bhs 1b			@ ... and repeat the search
# At this point, r2 (curr) is the first block whose address is greater than r1.
# Now verify that r0 (prev) is less than r1.
	cmp r0, r1
	bhs 9f			@ error if not
# Now r1 is pinned between r0 (prev) and r2 (curr).
	ldr r3, [r0, #4]	@ the number of free bytes in block r0
	ldr r5, [r2, #4]	@ the number of free bytes in block r2
# Note that the number of about-to-be-free bytes in block r1 is
# already in r4.
# For a block rx already on the free list, we use the notation rx.free
# to be the number of free bytes in the block.  This value is in [rx, #4].
# Similarly, we use the notation rx.next to be the pointer to the block
# following rx in the free list.  This value is in [rx].
# So far, we have
#   r3 contains r0.free
#   r4 contains r1.free
#   r5 contains r2.free
#   r0.next is r2
#
# In order for the r1 block to be between blocks r0 and r2,
# the following inequalities must hold -- otherwise there would be an overlap
#    (1) r0 + 8 + r0.free <= r1
#    (2) r1 + 8 + r1.free <= r2
# If inequality (2) is an equality, it may be possible to coalesce r1 and r2.
# If inequality (1) is an equality, it may be possible to coalesce r0 and r1.
# We check inequality (2) first:
	add ip, r4, #8		@ 8 + r1.free
	add ip, ip, r1		@ r1 + 8 + r1.free
	cmp ip, r2		@ should be <=
	bhi 9f			@ if not, return error
# Don't coalesce r1 and r2 if (2) is strict or if r2 is
# the RH sentinel block.
	bne 2f			@ don't coalesce r1 and r2 if (2) is strict
	cmp r5, #0		@ is r2 the RH sentinel?
	beq 2f			@ if so, don't coalesce
# Coalesce blocks r1 and r2
	add r4, r4, r5
	add r4, r4, #HSIZE	@ the new free size for r1
	str r4, [r1, #4]	@ update r1.free
	ldr r2, [r2]		@ r2 = r2.next (original r2 is swallowed up)
# No coalesce, so we simply link r1.next to r2
2:
	str r2, [r1]		@ r1.next = r2
# We check inequality (1) next:
3:
	add ip, r3, #8		@ 8 + r0.free
	add ip, ip, r0		@ r0 + 8 + r0.free
	cmp ip, r1		@ should be <=
	bhi 9f			@ if not, return error
# Don't coalesce r0 and r1 if (1) is strict or if r0 is
# the LH sentinel block.
	bne 4f			@ don't coalesce r0 and r1 if (1) is strict
	cmp r3, #0		@ is r0 the LH sentinel?
	beq 4f			@ if so, don't coalesce
# Coalesce blocks r0 and r1
	add r3, r3, r4
	add r3, r3, #HSIZE	@ the new free size for r0
	str r3, [r0, #4]	@ update r0.free
	ldr r1, [r1]		@ r1 = r1.next (original r1 is swallowed up)
# No coalesce, so we simply link r0.next to r1
4:
	str r1, [r0]		@ r0.next = r1
# Finish up
5:
	mov r0, #0		@ success
	pop {r4, r5, pc}

# Error exit -- returns -1
9:	mvn r0, #0
	pop {r4, r5, pc}


	.data

heapsize: .word 0		@ size of the heap (0 if uninitialized)
heapbase: .word base		@ location of free memory in bss

	.bss

	.align 3		@ align on a multiple of 8
	.lcomm base, MEMFREE	@ heap storage
