;*****************************************************************************
;                          *** 6502 ASM interface ***
;
;
;*****************************************************************************

%ifndef _6502_ASM_
%define _6502_ASM_

;*==========================================================================*
;*     INCLUDES                                                             *
;*==========================================================================*

;*==========================================================================*
;*     INTERFACE DEFINITIONS/MACROS
;*==========================================================================*

;*==========================================================================*
;*     INTERFACE DATA				                                           *
;*==========================================================================*
; All data are 32-bit in size   
	extern _a6502						; base address (struct access)		
   extern _a6502_PC					; Program Counter    (16-bit)
   extern _a6502_rA					; Accumulator        (8-bit)
   extern _a6502_rX					; X Index Reg.       (8-bit)
   extern _a6502_rY					; Y Index Reg.       (8-bit)
   extern _a6502_rS					; Stack Pointer      (8-bit)
   extern _a6502_rF					; Processor Status   (8-bit)
   extern _a6502_Cycles				; number of cycles to be executed
	extern _TrapBadOpCodes			; TRUE/FALSE for trapping bad opcodes

; these are pointers to each one of the eight 6502 8K memory banks
   extern _PrgRamBase
	extern _cBank0x2000
	extern _cBank0x4000
   extern _cBank0x6000
   extern _cBank0x8000
   extern _cBank0xA000
   extern _cBank0xC000
   extern _cBank0xE000


;*==========================================================================*
;*     INTERFACE FUNCTIONS							                               *
;*==========================================================================*
   extern _RST_6502        ; void RST_6502(void)
   extern _NMI_6502        ; void NMI_6502(void)
   extern _IRQ_6502        ; void IRQ_6502(void)
   extern _RUN_6502        ; u32  RUN_6502(void)
				;	Return value (C-like -> in eax):
				;	0x00				= OK!
				;	0x0000XX00		= Bad Opcode trapped! Opcode is returned at XX.
				;	This will be returned ONLY IF '_TrapBadOpCodes' == TRUE


%endif  ; _6502_ASM_
