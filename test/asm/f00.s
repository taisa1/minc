.file	"f.c"
.text
.p2align 4,,15
.globl f
.type f, @function
f:
LFB0:
.cfi_startproc
movq $123, %rax
ret
.cfi_endproc
.LFE0:
.size f, .-f
.ident "GCC: (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0"
.section	.note.GNU-stack,"",@progbits
