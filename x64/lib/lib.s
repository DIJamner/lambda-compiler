section .data
non_func_err: db "Error: tried to call a non-function", 0

section .text
extern _malloc
extern _printf
global print
global not_a_func
global new_env
;; from gc.s
extern collect
  
not_a_func:
  add rsp, -8             ; re-align the stack pointer
  mov rax, 0x2000004      ; System call write = 4
  mov rdi, 1              ; Write to standard out = 1
  mov rsi, non_func_err   ; the string is in the 3rd arg; move to the second arg
  mov rdx, 14             ; The size to write TODO: replace w/ correct size, write to stderr
  syscall                 ; Invoke the kernel
  mov rax, 0x2000001      ; System call number for exit = 1
  mov rdi, 1              ; Exit failure = 1
  syscall                 ; Invoke the kernel

print: ; TODO: replace w/ printf?
;  add rsp, -8             ; re-align the stack pointer
;  mov rax, 0x2000004      ; System call write = 4
;  mov rdi, 1              ; Write to standard out = 1
;  mov rsi, rdx            ; the string is in the 3rd arg; move to the second arg
;  mov rdx, 14             ; The size to write
;  syscall                 ; Invoke the kernel
;  mov rax, not_a_func     ; return not_a_func
;  add rsp, 8              ; re-align the stack pointer
;  ret                     ; return
jmp _printf

%define ENV_SIZE 3 * 8  ; the number of bytes in an environment
%define outerEnv(reg) [reg]
%define outerArgCode(reg) [reg + 8]
%define outerArgEnv(reg) [reg + 16]

; new_env(env, arg-code, arg-env)
; invariant: preserves the contents of rdi, rsi, rdx across call TODO: change, calling convention changed
new_env:
  ;; store the arguments on the stack (realigns stack)
  push rdx ; envptr
  push rsi ;; codeptr
  push rdi ;; env
retry_new_env:                ; skip prologue on retry
  mov rdi, ENV_SIZE           ; allocate a heap entry for one environment             
  call _malloc
  cmp rax, 0                  ; check to see that a result was actually allocated
  je gc_new_env               ; if no memory could be allocated, garbage collect then try again
  ;; retrieve arguments
  pop rdi
  pop rsi
  pop rdx
  mov outerEnv(rax), rdi      ; store the environment in the first enviroment slot
  mov outerArgCode(rax), rsi  ; store the argument code pointer in the second enviroment slot
  mov outerArgEnv(rax), rdx   ; store the argument env pointer in the third enviroment slot
  ret                         ; return the new env
gc_new_env:
  call collect                ; there is not enough memory; perform garbage collection
  jmp retry_new_env           ; try to allocate again
