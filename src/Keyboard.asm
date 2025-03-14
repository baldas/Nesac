;
; Assembly wrapper routine for keyboard
;
; command line: nasm -fcoff -okey.o keyboard.asm
;

;=============================================================================
; EXTERNAL REFERENCES
;=============================================================================
extern ___djgpp_ds_alias			; this is from gcc compiler
extern _KeyboardHandler				; C/C++ function (void KeyboardHandler(void))
									; with code locked (handler function)
extern _keyboard_stack				; a keyboard stack data (locked) to be
									; used by the handler (it's declared in
									; the C/C++ module)

;=============================================================================
; INTERFACE FUNCTIONS  (C style)
;=============================================================================
global _KeyboardWrapper				; void KeyboardWrapper(void)
global _END_KeyboardWrapper			; void END_KeyboardWrapper(void)


[bits 32]

[section .text]
;
; OBS: This handler is not reentrante (the stack is not dinamic at all)
;
; void KeyboardWrapper(void)
_KeyboardWrapper:
   push  ds                      ; save registers
   push  es
   push  fs
   push  gs
   pushad

;;;   xor   eax, eax    ; ***

   mov   ax, cs   ;
   mov   ds, ax   ; cs override  (0x2e)

   mov   ax, word [___djgpp_ds_alias]         ; set selectors correctly
   mov   ds, ax
   mov   es, ax
   mov   fs, ax
   mov   gs, ax

;   cli            ; not necessary as this is yet a hardware handler

   cld                                 ; positive increments

   mov   ecx, esp                      ; save ESP in ECX and SS in DX

;;;   xor   edx, edx    ; ***

   mov   dx, ss

   mov   ss, ax                        ; set SS
   mov   ebx, _keyboard_stack+1024     ; set ESP (point to the top of the
   mov   esp, ebx                      ;          stack)

   push  edx                           ; save ECX and DX (ESP and SS)
   push  ecx

   call  _KeyboardHandler              ; call our handler

   pop   eax                           ; EAX = ESP
   pop   ebx                           ;  BX = SS
   mov   ss, bx                        ; restore stack values
   mov   esp, eax

   popad                               ; restore registers
   pop   gs
   pop   fs
   pop   es
   pop   ds

   sti                                 ; end of the wrapper
iret
; void END_KeyboardWrapper(void)
_END_KeyboardWrapper:                  ; necessary because we must lock
                                       ; this wrapper in memory
