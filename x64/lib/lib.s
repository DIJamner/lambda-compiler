DEFAULT REL
section .data
non_func_err: db 10, "Error: tried to call a non-function", 10 ;NOTE: not null-terminated!
non_func_err_len equ $ - non_func_err

section .text
extern _malloc
global print
global not_a_func
global new_env
;; from Rust libgc
extern _get_next
  
not_a_func: ;TODO: accept an argument listing the type that was used in function position?
  add rsp, -8               ; re-align the stack pointer
  mov rax, 0x2000004        ; System call write = 4
  mov rdi, 2                ; Write to standard err = 2
  mov rsi, non_func_err     ; the string is in the 3rd arg; move to the second arg
  mov rdx, non_func_err_len ; The size to write
  syscall                   ; Invoke the kernel
  mov rax, 0x2000001        ; System call number for exit = 1
  mov rdi, 1                ; Exit failure = 1
  syscall                   ; Invoke the kernel

print:
  add rsp, -8             ; re-align the stack pointer
  ; calculate the length of the string
  mov rax, 0
  mov rdi, r8
  mov ecx, 0xfffff   ; maximum scan length
  cld
  repne scasb
  sub   ecx, 0xfffff  ; sub by the scan length
  neg   ecx
  dec   ecx          ; account for the null char
  mov rdx, rcx            ; the size of the string
  ; call write
  mov rax, 0x2000004      ; System call write = 4
  mov rdi, 1              ; Write to standard out = 1
  mov rsi, r8             ; the string is in the 3rd arg; move to the second arg
  syscall                 ; Invoke the kernel
  mov rax, not_a_func     ; return not_a_func
  add rsp, 8              ; re-align the stack pointer
  ret                     ; return
;  mov rdi, r8
;  jmp _printf

%define ENV_SIZE 3 * 8  ; the number of bytes in an environment
%define outerEnv(reg) [reg]
%define outerArgCode(reg) [reg + 8]
%define outerArgEnv(reg) [reg + 16]

; new_env(env, arg-code, arg-env)
; invariant: preserves the contents of rdx, rsi, r8 across call
new_env:
  add rsp, -8                 ; re-align the stack pointer (TODO: Why is this correct?)
  ;; store the arguments on the stack
  push rdx ; env
  push rsi ;; codeptr
  push r8 ;; envptr
retry_new_env:                ; skip prologue on retry
  mov r8, ENV_SIZE           ; allocate a heap entry for one environment             
  call _get_next 
  ;; retrieve arguments
  pop r8
  pop rsi
  pop rdx
  mov outerEnv(rax), rdx      ; store the environment in the first enviroment slot
  mov outerArgCode(rax), rsi  ; store the argument code pointer in the second enviroment slot
  mov outerArgEnv(rax), r8   ; store the argument env pointer in the third enviroment slot
  add rsp, 8
  ret                         ; return the new env
