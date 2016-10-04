DEFAULT REL
section .text
global collect

collect:
  ;TODO: generational garbage collection
  mov rax, 0x2000001      ; System call number for exit = 1
  mov rdi, 1              ; Exit failure = 1
  syscall                 ; Invoke the kernel