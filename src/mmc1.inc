
;****************************************************
;* iNES MMC #1 (Nintendo MMC1)                      *
;****************************************************

[section .text]

%macro Exit_Mapper_01 0
   pop   ebp
   ret
%endmacro

_Mapper_01:
   push  ebp

   mov   ebx, [esp+8]               ; ebx = address
   mov   eax, [esp+12]              ; eax = data

; First we select the right MMC1 register to work to
   and   ebx, 0x6000          ; ebx =        0x0 -> 0x8000-0x9FFF  (MMC1_Reg1)
   shr   ebx, 11              ;              0x4 -> 0xA000-0xBFFF  (MMC1_Reg2)
                              ;              0x8 -> 0xC000-0xDFFF  (MMC1_Reg3)
                              ;              0xC -> 0xE000-0xFFFF  (MMC1_Reg4)

   test  eax, 0x80            ; must we reset this register ?
   jnz   near Reset_MMC1_Reg
      shr   eax, 0x01                        ; read the 5-bit serially
      rcr   dword [MMC1_Regs+ebx], 0x01     

      dec   dword [_MMC1_SP_Reg]             ; have the 5-bit already come ?
      jz    PROCESS_REGISTER
         Exit_Mapper_01

PROCESS_REGISTER:
      mov   dword [_MMC1_SP_Reg], 0x05    ; clear counter and ...    
      shr   dword [MMC1_Regs+ebx], 0x1B   ; ajust our 5-bit word

      shr   ebx, 0x03      ; set flags
      ja    near Register2          ; C = 0  &  Z = 0
      jnc   Register0               ; C = 0
      jz    near Register1          ; Z = 1

;
; Don't move the order of the register, 'cause this is FULL optimizaded
;

;***************************************
Register3:
   mov   eax, dword [_MMC1_Reg4]           ; eax = 5-bit word

; bit 0/1/2/3  - Select PRG ROM bank

   and   eax, dword [_PRGPages]        ; eax = 16k bank select

   test  dword [_MMC1_Reg1], 0x08      ; is this a 32k swapping ?
   jz    MMC1_SWAP_32
      mov   ebx, [_PRGBank+eax*4]         ; PRG 16k swap
      mov   eax, [PRG_Switch_Area]
      mov   [eax], ebx
      add   ebx, 0x2000
      mov   [eax+4], ebx

      Exit_Mapper_01

MMC1_SWAP_32:       
;   shr   eax, 0x01
   mov   ebx, [_PRGBank+eax*4]
   mov   [_cBank0x8000], ebx
   add   ebx, 0x2000
   mov   [_cBank0xA000], ebx

   mov   ebx, [_PRGBank+eax*4+4]
   mov   [_cBank0xC000], ebx
   add   ebx, 0x2000
   mov   [_cBank0xE000], ebx

   Exit_Mapper_01
;***************************************


;***************************************
Register0:
   mov   eax, dword [_MMC1_Reg1]           ; eax = 5-bit word

   test  eax, 0x01                  ; what mirror is this ?
   jz    .HORIZ_MIRROR
;;;      mov   ebx, [_pBase0x2000]     ; _VRAMBase0x2400 = _VRAMBase0x2000
;;;      mov   [_pBase0x2400], ebx

		mov	ebx, [_VRAMBank]
		mov	[_pBank0x2000], ebx
		mov	[_pBank0x2400], ebx

;;;      mov   ebx, [_PPUBase]            ; _VRAMBase0x2800 = _VRAMBase0x2C00
;;;      add   ebx, 0x2C00

		mov	ebx, [_VRAMBank+3*4]

      mov   [_pBank0x2C00], ebx     ; because we may have 'one-screem m.'
      mov   [_pBank0x2800], ebx
      jmp   CONTINUE_Reg0

.HORIZ_MIRROR:
;   mov   ebx, [_VRAMBase0x2000]        ; _VRAMBase0x2800 = _VRAMBase0x2000
;   mov   [_VRAMBase0x2800], ebx
;   mov   ebx, [_PPUBase]               ; _VRAMBase0x2400 = _VRAMBase0x2C00
;   add   ebx, 0x2C00
;   mov   [_VRAMBase0x2C00], ebx        ; because we may have 'one-screem m.'
;   mov   [_VRAMBase0x2400], ebx

	mov	ebx, [_VRAMBank]
	mov	[_pBank0x2000], ebx
	mov	[_pBank0x2800], ebx

	mov	ebx, [_VRAMBank+4*3]
	mov	[_pBank0x2C00], ebx
	mov	[_pBank0x2400], ebx



CONTINUE_Reg0:


; bit 1     - One-Screen Mirroring        
   test  eax, 0x02
   jnz   NORMAL_MIRRORING
      mov   ebx, [_VRAMBank]        
      mov   [_pBank0x2400], ebx
      mov   [_pBank0x2800], ebx
      mov   [_pBank0x2C00], ebx

NORMAL_MIRRORING:


; bit 2     - PRG Switching Area
   lea   ebx, [_cBank0xC000]               ; area to use for switching
   and   eax, 0x04                           
   add   eax, eax
   sub   ebx, eax
   mov   dword [PRG_Switch_Area], ebx

; bit 3     - PRG Switching Size          
; see Register3 to see how this is used.

; bit 4     - VROM Switching Size        
; see Register1/Register2 to see how this is used.

   Exit_Mapper_01
;***************************************

;***************************************
Register1:
   mov   eax, dword [_MMC1_Reg2]           ; eax = 5-bit word

; bit 0/1/2/3 - Select VROM bank at $0000

   and   eax, dword [_VROMPages]          ; select the page to switch to
                                          ; in banks of 4k

   test  dword [_MMC1_Reg1], 0x10            ; Which swapping size is this:
   jne   SWAP_4K                             ; 4k or 8k?
      cmp   dword [_VROMPages], 0xFFFFFFFF
      je    near END_MAPPER1              ; swap 8k if only the cart has
         mov   ebx, [_VROMBank+eax*4]     ; VROM (I don't know if this is
         mov   [_pBank0x0000], ebx			; true - Mega Man 2 seems not to
         add   ebx, 0x0400                ; worry about)
         mov   [_pBank0x0400], ebx
         add   ebx, 0x0400
         mov   [_pBank0x0800], ebx
         add   ebx, 0x0400
         mov   [_pBank0x0C00], ebx

         mov   ebx, [_VROMBank+eax*4+4]
         mov   [_pBank0x1000], ebx
         add   ebx, 0x0400
         mov   [_pBank0x1400], ebx
         add   ebx, 0x0400
         mov   [_pBank0x1800], ebx
         add   ebx, 0x0400
         mov   [_pBank0x1C00], ebx

         Exit_Mapper_01

SWAP_4K:                               ; swap only a 4k bank at PPU 0x0000
   mov   ebx, [_VROMBank+eax*4]
   mov   [_pBank0x0000], ebx
   add   ebx, 0x0400
   mov   [_pBank0x0400], ebx
   add   ebx, 0x0400
   mov   [_pBank0x0800], ebx
   add   ebx, 0x0400
   mov   [_pBank0x0C00], ebx

; bit 4     - 256K ROM Selection Register 0
; not implemented yet!

   Exit_Mapper_01
;***************************************

;***************************************
Register2:
   mov   eax, dword [_MMC1_Reg3]          ; eax = 5-bit word

; bit 0/1/2/3  - Select VROM bank at $1000

   test  dword [_MMC1_Reg1], 0x10         ; is this a 4k swapping ?
   jz    near END_MAPPER1
      and   eax, dword [_VROMPages]          ; swapp 4k at PPU 0x1000
      mov   ebx, [_VROMBank+eax*4]
      mov   [_pBank0x1000], ebx
      add   ebx, 0x0400
      mov   [_pBank0x1400], ebx
      add   ebx, 0x0400
      mov   [_pBank0x1800], ebx
      add   ebx, 0x0400
      mov   [_pBank0x1C00], ebx

; bit 4     - 256K ROM Selection Register 1
; not implemented yet

   Exit_Mapper_01
;***************************************


;***************************************
Reset_MMC1_Reg:
   mov   dword [_MMC1_SP_Reg], 0x5           ; reset counter
   test  ebx, ebx                            ; is this Reg1 ?
   jz    THIS_IS_REG1
      mov   dword [MMC1_Regs+ebx], 0x00      ; RESET register 2/3/4 value

THIS_IS_REG1:
   or    dword [_MMC1_Reg1], 0x0C   ; see Metroid: at reset bit 4 = 0
                                    ; bit 4 do not change at reset (Artelius)
   mov   ebx, _cBank0x8000
   mov   dword [PRG_Switch_Area], ebx

   Exit_Mapper_01
;***************************************

END_MAPPER1:
   Exit_Mapper_01
;***************************************

[section .data]

align 4, db 0
_MMC1:
MMC1_Regs: 
_MMC1_Reg1:     dd    0x0C
_MMC1_Reg2:     dd    0x00
_MMC1_Reg3:     dd    0x00
_MMC1_Reg4:     dd    0x00
_MMC1_SP_Reg:   dd    0x05

PRG_Switch_Area:  dd    0x00        ; used in MMC1 implementation
