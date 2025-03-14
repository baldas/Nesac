;=============================================================================
;|
;|
;|
;|
;|
;|----------------------------------- // -----------------------------------
;|
;|  General Description
;|
;|
;|
;|----------------------------------- // -----------------------------------
;|
;|  Doubts/To be done
;|
;|
;|
;|
;|----------------------------------- // -----------------------------------
;|
;|  History:
;|
;|	08/02/2000:
;|		. partially implemented iNES mapper #3.
;|
;|	09/02/2000:
;|		. completed the implementation of iNES mapper #3 (at least I think :)
;|		. implemented iNES mapper #2.
;|
;| 12/02/2000:
;|		. implemented iNES mapper #1 (for games <= 256k). There still are some
;|     bugs I'm trying to get rid of.
;|
;| 14/02/2000:
;|		. found and fixed MANY bugs in mapper #1 implementation. Still I have
;|    not tasted 32kb PRG ROM switching, it's problably wrong.
;|
;| 15/02/2000:
;|		. implemented mapper #4 partially. I still have to implement IRQ stuff.
;|
;|	17/02/2000:
;|		. mapper #4 IRQ stuff implemented! Many games work now!!! Still, I
;|    have to add some things related to $2006 writes durings refresh!
;|
;|	21/02/2000:
;|		. 32k PRGROM switch (MMC1) works fine now. Still i have not implemented 
;|		when changed occur from/to low/high banks or 16k/32k, as I have not seem
;|		ANY game that uses or have crashed because of it yet!
;|
;|	12/03/2000:
;|		. fixed some bugs in the MMC3 implementation. Now games like 'Ninja 
;|		Gaiden III', 'G.I. Joe II' work good.
;|		. implemented 'One-Screen mirroring' in the MMC1 routine.
;|
;| 04/02/2001:
;|		. Just changed some variables and improved the documentation.
;|
;|
;=============================================================================
[BITS 32]

;=============================================================================
; GLOBAL SYMBOLS
;=============================================================================
;------------
; Functions
;------------
   Global   _No_Mapper     ; void No_Mapper(void);
   Global   _Mapper_01     ; void Mapper_01(int Data, int Addr);
   Global   _Mapper_02     ; void Mapper_02(int Data, int Addr);
   Global   _Mapper_03     ; void Mapper_03(int Data, int Addr);
   Global   _Mapper_04     ; void Mapper_04(int Data, int Addr);

;------------
; Data
;------------
   Global   _MMC3
   Global   _MMC3_Command, _MMC3_Latch, _MMC3_XOR_Val
   Global   _MMC3_IRQ_EBL, _MMC3_IRQ_CTR

   Global   _MMC1
   Global   _MMC1_Reg1, _MMC1_Reg2, _MMC1_Reg3, _MMC1_Reg4, _MMC1_SP_Reg

	Global	_PRGPages
	Global	_PRGBank
	Global	_MMC_Handler

	Global	_VROMPages
	Global	_VROMBank
	Global	_VRAMBank

;==================== END OF GLOBAL SYMBOLS DEFINITION =======================


;=============================================================================
; INTERFACE DATA
;=============================================================================
[section .data]

align 4
_PRGPages:        dd 0x00        ; PRG pages of the cart loaded
_PRGBank: times 128 dd 0x00    ; Maximum number of banks
; maximum = 64 x 16k  ->  minimum bank size = 8k (at the moment)
; so, _PRGBank = 64 x (16/8) = 128 (worst case)

_MMC_Handler:  dd 0x00000000

align 4
_VROMPages:       dd 0x00000000     ; number of VROM pages used minus 1 (to
                                    ; use in the MMC routines)
_VROMBank: times 512 dd 0x00000000  ; array holding the max VROM pages
_VRAMBank: times 4   dd 0x00
;----------------------------------- // -----------------------------------



;=============================================================================
; INCLUDES/REQUIRED EXTERNAL REFERENCES
;=============================================================================
%include "..\include\6502.ha"
;%include "..\include\io.ha"
%include "..\include\ppu.ha"

;----------------------------------- // -----------------------------------



;=============================================================================
; LOCAL DEFINITIONS/MACROS
;=============================================================================
;----------------------------------- // -----------------------------------





;=============================================================================
; INTERFACE CODE
;=============================================================================
[section .text]

;****************************************************
;* No Mapper                                        *
;****************************************************
_No_Mapper:
   ret


%include "mmc1.inc"
%include "mmc2.inc"
%include "mmc3.inc"
%include "mmc4.inc"


;----------------------------------- // -----------------------------------



;=============================================================================
; PRIVATE CODE
;=============================================================================
[section .text]
;----------------------------------- // -----------------------------------



;=============================================================================
; PRIVATE DATA
;=============================================================================
[section .data]
;----------------------------------- // -----------------------------------
