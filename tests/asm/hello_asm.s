  .set STDIN_FILENO,  0
  .set STDOUT_FILENO, 1
  .set STDERR_FILENO, 2

  .set SYS_exit,      1
  .set SYS_open,      2
  .set SYS_close,     3
  .set SYS_read,      4
  .set SYS_write,     5
  .set SYS_lseek,     6
  .set SYS_unlink,    7
  .set SYS_getpid,    8
  .set SYS_kill,      9
  .set SYS_fstat,     10

  .section .rodata
  .p2align 2
.Lgreeting:
  .string "Hello world\n"
.Lgreeting_end:
  .text
  .p2align 1
  .global _start
  .type _start, @function
_start:
  $r4 <- STDOUT_FILENO
  $r5 <- .Lgreeting
  $r6 <- .Lgreeting_end
  $r6 <- $r6 - $r5
  syscall
  .hword SYS_write
  $r4 <- 0
  syscall
  .hword SYS_exit
