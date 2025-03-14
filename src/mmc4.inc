

;****************************************************
;* iNES MMC #4 (Nintendo MMC3)                      *
;****************************************************

[section .text]

%macro Exit_MMC3 0
   pop   ebp
   ret
%endmacro


_Mapper_04:
   push  ebp

   mov   ebx, [esp+8]               ; ebx = address (register)
   mov   eax, [esp+12]              ; eax = data

   and   ebx, 0xE001                
   rol   bx, 0x04
   jmp   [MMC3_Regs_Tb+ebx*4]             ; jump to selected register


;***************************************
Reg_0x8000:
   mov   dword [_MMC3_Command], eax       ; save the information to use in
                                          ; Reg_0x8001

   mov   ebx, dword [XOR_Table+eax*4]     ; save the value that will be XORed
   mov   dword [_MMC3_XOR_Val], ebx       ; with VROM base (0x0000/0x0010)

   and   eax, 0x40
   cmp   dword [Base_Toggle], eax
   jne   TOGGLE_BASE
      Exit_MMC3

TOGGLE_BASE:
   mov   dword [Base_Toggle], eax      ; save toggle information

   mov   ebx, dword [_PRGPages]

   test  eax, eax                   ; is it from 0x00->0x40 or 0x40->0x00
   jz    _0x40_to_0x00
      dec   ebx
      mov   eax, [_PRGBank+ebx*4]
      mov   [_cBank0x8000], eax
      mov   eax, [Sec_Bank_Tb+ebx*4]
      mov   ebx, [_PRGBank+eax*4]
      mov   [_cBank0xC000], ebx

      Exit_MMC3

_0x40_to_0x00:
   dec   ebx
   mov   eax, [_PRGBank+ebx*4]
   mov   [_cBank0xC000], eax
   mov   eax, dword [Sec_Bank_Tb+ebx*4]
   mov   ebx, [_PRGBank+eax*4]
   mov   [_cBank0x8000], ebx

   Exit_MMC3
;***************************************


;***************************************
Reg_0x8001:
   mov   ebx, dword [_MMC3_Command]
   and   ebx, 0x07                  ; ebx = command number
   cmp   ebx, 0x05
   jg    PRGROM_SWAP          ; is it PRGROM or VROM swapping ?

;********************
VROM_SWAP:
   and   eax, dword [_VROMPages]             ; take the page that will be
                                             ; swapped

;   cmp   ebx, 0x01
;   jg    dsds
;      and   eax, 0xFE

;dsds:

   mov   ebx, dword [Command_Table+ebx*4]    ; take the base for swapping
   xor   ebx, dword [_MMC3_XOR_Val]          ; and do an adequate XOR

; So, till here we have: 'eax' = page that will be swapped into base 'ebx'

   lea   ebx, [_VROM+ebx]              ; take FISICAL base address
   mov   ebp, [_VROMBank+eax*4]        ; take FISICAL bank address
   mov   [ebx], ebp                    ; base address = bank address

   test  dword [_MMC3_Command], 0x06      ; is there another 1k to swap ?
   jz    SWAP_ANOTHER_1K
      Exit_MMC3

SWAP_ANOTHER_1K:
   mov   ebp, [_VROMBank+eax*4+4]
   mov   [ebx+4], ebp

   Exit_MMC3
;********************

;********************
PRGROM_SWAP:
   and   eax, dword [_PRGPages]     ; take the page to be swapped

   and   ebx, 0x01                  ; ebx = 0 -> 0x8000/0xA000 (1st bank)
                                    ;       1 -> 0xA000/0xC000 (2nd bank)

   test  dword [_MMC3_Command], 0x40   ; in what place the swap will occur ?
   jnz    SWAP_AT_0xA000_0xC000

SWAP_AT_0x8000_0xA000:
      mov   ebp, [_PRGBank+eax*4]
      mov   dword [_cBank0x8000+ebx*4], ebp
      Exit_MMC3

SWAP_AT_0xA000_0xC000:
   xor   ebx, 0x01
   mov   ebp, [_PRGBank+eax*4]
   mov   dword [_cBank0xA000+ebx*4], ebp
   
   Exit_MMC3
;***************************************

;***************************************
Reg_0xA000:
   test  eax, 0x01                  ; what mirror is this ?
   jz    .HORIZ_MIRROR
      mov   ebx, [_pBank0x2000]     ; _VRAMBase0x2400 = _VRAMBase0x2000
      mov   [_pBank0x2400], ebx
      mov   ebx, [_pBank0x2C00]     ; _VRAMBase0x2800 = _VRAMBase0x2C00
      mov   [_pBank0x2800], ebx

      Exit_MMC3

.HORIZ_MIRROR:
   mov   ebx, [_pBank0x2000]        ; _VRAMBase0x2800 = _VRAMBase0x2000
   mov   [_pBank0x2800], ebx
   mov   ebx, [_pBank0x2C00]        ; _VRAMBase0x2400 = _VRAMBase0x2C00
   mov   [_pBank0x2400], ebx

   Exit_MMC3
;***************************************

;***************************************
Reg_0xA001:
; not implemented yet
   Exit_MMC3
;***************************************

;***************************************
Reg_0xC000:
   mov   dword [_MMC3_IRQ_CTR], eax
   Exit_MMC3
;***************************************

;***************************************
Reg_0xC001:
   mov   dword [_MMC3_Latch], eax     
   Exit_MMC3
;***************************************

;***************************************
Reg_0xE000:
   mov   eax, [_MMC3_Latch]         ; scanline counter = latch value
   mov   [_MMC3_IRQ_CTR], eax

   mov   dword [_MMC3_IRQ_EBL], 0x00

   Exit_MMC3
;***************************************

;***************************************
Reg_0xE001:
   mov   dword [_MMC3_IRQ_EBL], 0x01
   Exit_MMC3
;***************************************

END_MMC3:
   Exit_MMC3
;***************************************


[section .data]

;*********************************************************
; Used to find the XOR value in MMC3 implementation.
;

align 4, db 0
XOR_Table:
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00

         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00

         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00

         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00

         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10
         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10
         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10
         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10

         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10
         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10
         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10
         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10

         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10
         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10
         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10
         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10

         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10
         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10
         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10
         dd    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10
;*********************************************************


;*********************************************************
; Selects a MMC3 register handler routine.
;
align 4, db 0
MMC3_Regs_Tb:
         dd    0x0000000        ; 00
         dd    0x0000000        ; 01
         dd    0x0000000        ; 02
         dd    0x0000000        ; 03
         dd    0x0000000        ; 04
         dd    0x0000000        ; 05
         dd    0x0000000        ; 06
         dd    0x0000000        ; 07
         dd    Reg_0x8000       ; 08
         dd    0x0000000        ; 09
         dd    Reg_0xA000       ; 0A
         dd    0x0000000        ; 0B
         dd    Reg_0xC000       ; 0C
         dd    0x0000000        ; 0D
         dd    Reg_0xE000       ; 0E
         dd    0x0000000        ; 0F
         dd    0x0000000        ; 10
         dd    0x0000000        ; 11
         dd    0x0000000        ; 12
         dd    0x0000000        ; 13
         dd    0x0000000        ; 14
         dd    0x0000000        ; 15
         dd    0x0000000        ; 16
         dd    0x0000000        ; 17
         dd    Reg_0x8001       ; 18
         dd    0x0000000        ; 19
         dd    Reg_0xA001       ; 1A
         dd    0x0000000        ; 1B
         dd    Reg_0xC001       ; 1C
         dd    0x0000000        ; 1D
         dd    Reg_0xE001       ; 1E
         dd    0x0000000        ; 1F
;*********************************************************

align 4, db 0
_MMC3:
_MMC3_Command:    dd 0x00
_MMC3_Latch:      dd 0x00
_MMC3_XOR_Val:    dd 0x00
_MMC3_IRQ_EBL:    dd 0x00
_MMC3_IRQ_CTR:    dd 0x00


;*********************************************************
; Returns the PT base according to command (stored in
; register 0x8000)
;

align 4, db 0
Command_Table:   ; OBS: IT'S ALREADY SHIFTED: >> 8
         dd    0x0000, 0x0008, 0x0010, 0x0014, 0x0018, 0x001C
;*********************************************************

Base_Toggle:      dd    0x00        ; used in MMC3 implementation


;*********************************************************
; In case of base toggle, find the right bank for the other
; bank (not the hardwired)
;

align 4, db 0
Sec_Bank_Tb:
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
         dd    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
;*********************************************************

