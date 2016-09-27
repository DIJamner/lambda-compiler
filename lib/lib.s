section .data
non_func_err     db      "Error: tried to call a non-function", 0x0a

section .text
global print
global not_a_func
  
not_a_func:
  add rsp, -8             ; re-align the stack pointer
  mov rax, 0x2000004      ; System call write = 4
  mov rdi, 1              ; Write to standard out = 1
  mov rsi, non_func_err   ; the string is in the 3rd arg; move to the second arg
  mov rdx, 14             ; The size to write
  syscall                 ; Invoke the kernel
  mov rax, 0x2000001      ; System call number for exit = 1
  mov rdi, 1              ; Exit failure = 0
  syscall                 ; Invoke the kernel

print:
  add rsp, -8             ; re-align the stack pointer
  mov rax, 0x2000004      ; System call write = 4
  mov rdi, 1              ; Write to standard out = 1
  mov rsi, rdx            ; the string is in the 3rd arg; move to the second arg
  mov rdx, 14             ; The size to write
  syscall                 ; Invoke the kernel
  mov rax, not_a_func     ; return not_a_func
  add rsp, 8              ; re-align the stack pointer
  ret                     ; return