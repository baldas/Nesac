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
;|     This module (IO module) is divided in several sections, each one 
;|  relating to an aspect of NES I/O devices.
;|  
;|     Following is a brief description of each one:
;|
;|        . Section 1:   _ReadIO
;|
;|                       addresses      0x2000 -> 0x3FFF     (read)
;|                                      0x4000 -> 0x4017     (read)
;|                                      0x6000 -> 0x7FFF     (read)
;|          
;|        . Section 2:   _WriteIO
;|
;|                       addresses      0x2000 -> 0x3FFF     (write)
;|                                      0x4000 -> 0x4017     (write)
;|                                      0x6000 -> 0x7FFF     (write)
;|
;|
;|
;|     For more information regarding each section see the respectives ones.
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
;|	19/01/2000:
;|		. implemented the basics of PPU registers read\write.
;|
;|	24/01/2000:
;|		. implemented reading from PPU 0x2007 register (now BATTLE CITY
;|		works!).
;|
;|	01/02/2000:
;|		. now 2007 writes (PPU memory address) are not allowed if it is
;|		VROM (0-1FFF). With this and 6502 stack security, Zippy Race works!
;|    (I don't know why but it was writting to VROM!!?? (bad dump?))
;|
;|	12/02/2000:
;|		. added the 'Expansion memory' (_NOT_ SAVE RAM) as I found some games
;|	   that uses this (actually, only METROID)
;|
;|	21/02/2000:
;|		. implemented: "Read from 2002 makes scroll register to be cleared".
;|    With this, carts like 'Friday-the 13th' scrolls OK!
;|
;| 22/02/2000:
;|		. fixed a bug in the palette, when values above 0x3F (63) was written.
;|    Now I'm ANDering the value written with 0x3F, this makes games that
;|    seemed to be broken back to light (find it with 'Adv. of Dino Riki').
;|    . now reads from joysticks return 0x40/0x41 for depressed/pressed
;|    button respectivily (I don't know why!!!). Find it analising the game
;|    'Last StarFight' with Nesten. At any other emulator the joystick does
;|    not work.
;|		. reads/writes from/to PPU memory 0x3000-0x3F00 are mirrored into
;|    0x2000-0x2F00, as I've found a game that need that (Seikima 2).
;|
;|	12/03/2000:
;|		. I've made a simple implementation of '2005/2006 magic'. Most games 
;|		with midHblank writes work now! Because of the start of school I must 
;|		not be doing changes for some long time.
;|
;| 04/02/2001:
;|		. made some clean-up stuff. Removed things not related to this module.
;|		. implemented loopy's 2005/2006 magic.
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
   Global   _ReadIO        ; u8 ReadIO(u32 address)
   Global   _WriteIO       ; void WriteIO(u32 address, u8 data)


;------------
; Data
;------------
   Global  _Joy1_Data, _Mapper



;==================== END OF GLOBAL SYMBOLS DEFINITION =======================


;=============================================================================
; INTERFACE DATA
;=============================================================================
[section .data]

align 4
_Joy1_Data:    dd 0x00000000  ; PC's keyboard's -> NES information
                              ; 1 = A, 2 = B, 4 = Select, 8 = Start, ...
_Mapper:       dd 0x00000000


;----------------------------------- // -----------------------------------



;=============================================================================
; INCLUDES/REQUIRED EXTERNAL REFERENCES
;=============================================================================
%include "..\include\6502.ha"
%include "..\include\ppu.ha"
%include "..\include\mmc.ha"

;----------------------------------- // -----------------------------------



;=============================================================================
; LOCAL DEFINITIONS/MACROS
;=============================================================================
%define DEFAULT_RET 0x00            ; value returned by default

%macro Exit_ReadTrap 0              ; our exit macro
   ret
%endmacro

%macro Exit_WriteTrap 0
   pop   ebp
   ret
%endmacro

;----------------------------------- // -----------------------------------





;=============================================================================
; INTERFACE CODE
;=============================================================================
[section .text]


;###########################################################################
;#		void ReadIO(void)                         
;#
;#
;#
;# Parameters:
;#              Input :   Address to be read (32-bit in stack).
;#              Output:   Value read (a byte in register AL).
;#
;#  OBS:
;#     . Code is ended in this section by calling 'Exit_ReadTrap' macro.
;#      
;#
;#     Valid address range:       0x2000 -> 0x3FFF
;#                                0x4000 -> 0x4017
;#                                0x6000 -> 0x7FFF
;#
;#        If none of these register is sent, a default value (DEFAULT_RET) is
;#     returned.
;#
;# *** _ONLY_ USE "EAX/EBX" REGISTERS IN THIS SECTION. IF OTHERS REGISTERS ***
;#           *** MUST BE USED, THEN "PUSH-POP" THEM WHEN NEEDED ***
;###########################################################################
align 4
_ReadIO:
   mov   ebx, [esp+4]            ; ebx (bx) = Address (see the valid address
                                 ;            range above).

   mov   eax, ebx                      ; low nibble of eax holds the index
   shr   eax, 0x0C                     ; (0-F) to table of registers handlers
   jmp   [Main_RTrap_Sel_Tb+eax*4]

;*************************************************    *** 0x2000<->0x3FFF
RANGE_2000_READ:
   and   ebx, 0x0007                ; I'm using area 0x2008->0x3FFF as a
                                    ; mirror of 0x2000->0x2007 every 8 bytes,

   jmp   [Sub_R2000_Sel_Tb+ebx*4]   ; selects the right register(0->7)


R2000:      
;***************************************  * PPU Control Register #1 *
   mov   eax, [_Ctrl_Reg_1]
   Exit_ReadTrap
;***************************************


R2001:      
;***************************************  * PPU Control Register #2 *
   mov   eax, [_Ctrl_Reg_2]
   Exit_ReadTrap
;***************************************


R2002:      
;***************************************  * PPU Status Register *
   mov   eax, [_Status_Reg]
   and   dword [_Status_Reg], 01111111b      ; conform Nesticle/Nesten

;;; IF YOU CLEAR 'HIT FLAG' here, games like Bomberman will not work!!!

; reset scroll register
   mov   dword [What_Byte_2005], 0x01     ; THIS MAKES 'FRIDAY-THE 13TH' WORKS!

   Exit_ReadTrap
;***************************************


R2003:      
;***************************************  * Sprite RAM Address Register *
   mov   al, byte [_Spr_RAM_Ad]
   Exit_ReadTrap
;***************************************


R2004:      
;***************************************  * Sprite RAM Data Register *
   mov   ebx, [_Spr_RAM_Ad]            ; ebx = offset from _SPR_RAM
   mov   al, byte [_SPR_RAM+ebx]       ; return value

   add   bl, byte [Inc_Value]          ; increment the register and already
   mov   [_Spr_RAM_Ad], ebx            ; take care of wrap around 0xFF->0x00

   Exit_ReadTrap
;***************************************


R2007:      
;***************************************  * VRAM Data Register *
	mov	ebx, dword [loop_v]
	and	ebx, 0x3FFF

   cmp   ebx, 0x3F00             ; is is a palett read ?
   jl    NOT_PALETT_READ
		and	ebx, 0x001F
		add	ebx, _Palette
      jmp   READ_2007_VALUE

NOT_PALETT_READ:
   cmp   ebx, 0x3000             ; is it a 0x3000-0x3EFF write (mirror) ?
   jl    NOT_0x3000_MIRROR
      and   ebx, 0x2FFF          ; mirrors 0x3000 into 0x2000

NOT_0x3000_MIRROR:
   mov   eax, ebx
   shr   eax, 0x0A
   and   ebx, 0x03FF
   add   ebx, [_VROM+eax*4]            ; adds to logical the fisical address

READ_2007_VALUE:
   mov   al, byte [ebx]                   ; return value

   mov   ebx, dword [Inc_Value]           
   and   ebx, dword [Is_2007_1st_Read]    ; increment address if not 1st read

	add	dword [loop_v], ebx
	and	dword [loop_v], 0x3FFF

   mov   byte [Is_2007_1st_Read], 0xFF    ; not in first read anymore

   Exit_ReadTrap
;***************************************

;*************************************************    *** 0x4000<->0x4017
RANGE_4000_READ:
   and   ebx, 0x0017                   ; selects one of the 17 registers
   jmp   [Sub_R4000_Sel_Tb+ebx*4]      ; handle it


R4016:
;***************************************     * Joystick #1 read *
   mov   eax, [_Joy1_Data]    ; PC's keyboard's -> NES information
   mov   ebx, [Joy1_Read]     ; index of the button to be read

   and   eax, ebx                            ; if the key is set we return ...

;* HERE WE HAVE AN 'AGI' optimization
      shl   bl, 1                ; shift it for next key (already take care of
      mov   [Joy1_Read], ebx     ; byte boundary)

   mov   eax, dword [Joy_Set_Table+eax*4]    ; ... 0x41, otherwise 0x40
                         
   Exit_ReadTrap
;***************************************


;*************************************************    *** 0x6000<->0x7FFF
EXP_MEM_READ:
	sub	ebx, 0x6000
   add   ebx, [_cBank0x6000]
   mov   al, byte [ebx]
   Exit_ReadTrap
;***************************************


END_READ:
   mov   eax, DEFAULT_RET
   Exit_ReadTrap
;----------------------------------- // -----------------------------------



;###########################################################################
;#		void WriteIO(void)
;#
;#
;# Parameters:
;#              Inputs :   - Address to be read (32-bit in stack).
;#                         - Data to be written (8-bit (32-bit) in stack). 
;#              Output:   None
;#
;#  OBS:
;#     . Code is ended in this section by calling 'Exit_WriteTrap' macro.
;#      
;#
;#     Valid address range:       0x2000 -> 0x3FFF
;#                                0x4000 -> 0x4017
;#                                0x6000 -> 0x7FFF
;#
;#        If none of these register is sent, no significant code is executed.
;#
;#   *** _ONLY_ USE "EAX/EBX/EBP" REGISTERS IN THIS SECTION. IF OTHERS ***
;#        *** ONES MUST BE USED, THEN "PUSH-POP" THEM WHEN NEEDED ***
;###########################################################################
align 4
_WriteIO:
   push  ebp

   mov   ebx, [esp+8]            ; ebx = Address
   mov   eax, [esp+12]           ; eax (al) = Data

   mov   ebp, ebx
   shr   ebp, 0x0C
   jmp   [ds:Main_WTrap_Sel_Tb+ebp*4]     ; selects the right range


;*************************************************    *** 0x2000<->0x3FFF
RANGE_2000_WRITE:
   and   ebx, 0x0007
   jmp   [Sub_W2000_Sel_Tb+ebx*4]      ; selects the right register


W2000:      
;***************************************  * PPU Control Register #1 *
   mov   [_Ctrl_Reg_1], eax            ; update register

; set some variables to speed things

   mov   ebx, [Incr_Table+eax*4]       ; increment value (1/32)
   mov   dword [Inc_Value], ebx

   mov   bh, al                        ; Bg PT logical address
   and   ebx, 0x1000
   mov   dword [_PTBgBase], ebx

   mov   ebx, [Spr_PT_Table+eax*4]     ; Sprite PT encoding variable
   mov   dword [_PTSpBase], ebx

   mov   ebx, [Sprite_Size+eax*4]      ; Set 'Sprite Size' variable
   mov   dword [_SPR_Size], ebx

;2000 write:
;        t:0000 1100 0000 0000=d:0000 0011
	and	eax, 0x03
	shl	eax, 0x0A
	and	dword [loop_t], 0xF3FF
	or		dword [loop_t], eax


   Exit_WriteTrap
;***************************************


W2001:      
;***************************************  * PPU Control Register #2 *
   mov   [_Ctrl_Reg_2], eax
   Exit_WriteTrap
;***************************************


W2002:      
;***************************************  * PPU Status Register *
;   mov   [_Status_Reg], eax              ; I THINK this can't be performed
   Exit_WriteTrap
;***************************************


W2003:      
;***************************************  * Sprite RAM Address Register *
   mov   [_Spr_RAM_Ad], eax
   Exit_WriteTrap
;***************************************


W2004:      
;***************************************  * Sprite RAM Data Register *
   mov   ebx, dword [_Spr_RAM_Ad]
   mov   byte [_SPR_RAM+ebx], al

   add   bl, byte [Inc_Value]           ; increment address/wrap around
   mov   [_Spr_RAM_Ad], ebx

   Exit_WriteTrap
;***************************************

       
W2005:
;***************************************  * Scroll Register *
   mov   ebx, dword [What_Byte_2005]

	cmp	ebx, 0x00
	je		SECOND_WRITE
		;2005 first write:
		;        x=d:0000 0111
		mov	dword [loop_x], eax
		and	dword [loop_x], 0x07

		;        t:0000 0000 0001 1111=d:1111 1000
		shr	eax, 0x03
		and	dword [loop_t], 0xFFE0
		or		dword [loop_t], eax

		jmp END_2005_WRITE


SECOND_WRITE:
;2005 second write:
;       t:0000 0011 1110 0000=d:1111 1000
	and	dword	[loop_t], 0xFC1F
	mov	ebx, eax
	and	ebx, 0xF8
	shl	ebx, 0x02
	or		dword [loop_t], ebx

;       t:0111 0000 0000 0000=d:0000 0111
	and	dword [loop_t], 0x8FFF
	and	eax, 0x07
	shl	eax, 12
	or		dword [loop_t], eax



END_2005_WRITE:
   mov   ebx, dword [What_Byte_2005]
   xor   ebx, 0x01                     ; toggle byte order (0/1)
   mov   dword [What_Byte_2005], ebx

   Exit_WriteTrap
;***************************************



W2006:
;***************************************  * VRAM Address Register *
   mov   ebx, dword [What_Byte_2005]

	cmp	ebx, 0x00
   je   _2nd_WRITE
   
		;	2006 first write:
      ;		  t:0011 1111 0000 0000=d:0011 1111
      ;		  t:1100 0000 0000 0000=0
	
		and	dword [loop_t], 0x00FF
		and	eax, 0x3F
		shl	eax, 0x08
		or		dword [loop_t], eax	
		jmp	GO_OUT_2007
	
	
_2nd_WRITE:	
;2006 second write:
;        t:0000 0000 1111 1111=d:1111 1111

	and	dword [loop_t], 0xFF00
	or		dword [loop_t], eax

;        v=t
	mov	eax, [loop_t]
	mov	[loop_v], eax


GO_OUT_2007:
   mov   ebx, dword [What_Byte_2005]
   xor   ebx, 0x01                        ; toggle byte order (0/1)
   mov   dword [What_Byte_2005], ebx
   mov   dword [Is_2007_1st_Read], 0x00   ; first read ON

   Exit_WriteTrap
;***************************************



W2007:      
;***************************************  * VRAM Data Register *
	mov	ebx, [loop_v]
	and	ebx, 0x3FFF			

   cmp   ebx, 0x3F00
   jge   PALETT_WRITE               ; is it a write to palett ?
      cmp   ebx, 0x2000
      jl    near VROM_WRITE         ; is the value to be written in VROM ?
         and   ebx, 0x0FFF       ; mirrors 0x3000 into 0x2000 (Seikima 2 uses
         mov   ebp, ebx                                     ;  it)
         shr   ebp, 0x0A
         and   ebx, 0x03FF
         add   ebx, [ds:_VRAM+ebp*4]
         mov   [ebx], al

         jmp   END_2007_WRITE

PALETT_WRITE:
	and	ebx, 0x001F						; mirror 0x20 = 0x00 / 0x21 = 0x01 / ...
   cmp   al, byte [_Palette+ebx]    ; is the value we're gonna write the same?
   je    near SAME_PALETT_VALUE     
      and   al, 0x3F             ; values > 0x40 are not allowed
      test   ebx, 0xE3
      je    PALETT_MIRROR           ; is it a mirror ?
         mov   [_Palette+ebx], al                 ; write to non-mirror palett
         jmp   END_2007_WRITE
         
PALETT_MIRROR:
   test  ebx, 0x0C
   jne   NOT_THE_RIGHT_MIRROR       ; is the mirror valid (00 or 10) ?
      mov   [_Palette+0x00], al        ; Write to palett mirror
      mov   [_Palette+0x04], al
      mov   [_Palette+0x08], al
      mov   [_Palette+0x0C], al
      mov   [_Palette+0x10], al
      mov   [_Palette+0x14], al
      mov   [_Palette+0x18], al
      mov   [_Palette+0x1C], al
      jmp   END_2007_WRITE

VROM_WRITE:                               
      cmp   dword [_VROMPages], 0xFFFFFFFF   ; does the game has VROM ?
      jne   GAME_WITH_VROM
         mov   ebp, ebx                      ; No, so we CAN write
         shr   ebp, 0x0A
         and   ebx, 0x03FF
         add   ebx, [ds:_VROM+ebp*4]
         mov   [ebx], al                     

SAME_PALETT_VALUE:                  ; Don't touch the palett
NOT_THE_RIGHT_MIRROR:               ; It's unnecessary to change the palett
GAME_WITH_VROM:
END_2007_WRITE:
	mov	ebx, [loop_v]
	add	ebx, dword [Inc_Value]
	and	ebx, 0x3FFF
	mov	[loop_v], ebx


   Exit_WriteTrap
;***************************************


;*************************************************    *** 0x4000<->0x4017
RANGE_4000_WRITE:
   and   ebx, 0x0017
   jmp   [Sub_W4000_Sel_Tb+ebx*4]         ; selects the right register


W4014:      
;***************************************  * PPU DMA transfer *
   shl   eax, 8               ; data * 100
   mov   ebx, dword [_PrgRamBase]
   add   ebx, eax             ; BASE + data*100

   mov   eax, 63
DMA_LOOP:
   mov   ebp, [ebx+eax*4]
   mov   [_SPR_RAM+eax*4], ebp
   dec   eax
 jns   DMA_LOOP

;   sub   dword [_a6502_Cycles], 510       ; don't know!!!

   Exit_WriteTrap
;***************************************  


W4016:      
;***************************************     * Joystick #1 write *
   and   eax, 0x01                  ; mask bit #0
   cmp   eax, dword [Joy1_Strobe]   ; has something changed ?
   je    NO_CHANGE
      mov   [Joy1_Strobe], eax      ; save newest write
      xor   eax, 0x01               ; change must be from 1 to 0 (1 -> 0)
      mov  dword [Joy1_Read], eax   ; acknoledge or not (acording to change)

NO_CHANGE:
   Exit_WriteTrap
;***************************************


;*************************************************    *** 0x6000<->0x7FFF
EXP_MEM_WRITE:
	sub	ebx, 0x6000
   add   ebx, [_cBank0x6000]
   mov   byte [ebx], al
   Exit_WriteTrap
;***************************************  


END_WRITE:
   Exit_WriteTrap
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


align 4
Is_2007_1st_Read: dd    0x00        ; 0xFF = No;  0x00 = Yes
What_Byte_2005:   dd    0x01        ; (0 = Vertical / 1 = Horizontal)
What_Byte_2006:   dd    0x01        ; first byte = upper 8-bit

Joy1_Strobe:      dd    0x00        ; store last joystick write information
Joy1_Read:        dd    0x01        ; index of the button to be read
                                    ; (1 = A, 2 = B, 4 = Select, ...)
Inc_Value:        dd    0x01        ; increment value (1/32)
;*****************************************************************************



;*********************************************************
; The index ranges from 0x0 to 0xF, and indicates what
; range of 0x1000 memory we'll be handled:
;     0x0 = 0x0000->0x0FFF, 0x1 = 0x1000->0x1FFF, ...
;

align 4, db 0
Main_RTrap_Sel_Tb:
   dd    END_READ                ; (0x0000<->0x0FFF area) ; should never be
   dd    END_READ                ; (0x1000<->0x1FFF area) ; reached
   dd    RANGE_2000_READ         ; (0x2000<->0x2FFF area)
   dd    RANGE_2000_READ         ; (0x3000<->0x3FFF area)
   dd    RANGE_4000_READ         ; (0x4000<->0x4FFF area)
   dd    END_READ                ; (0x5000<->0x5FFF area)
   dd    EXP_MEM_READ            ; (0x6000<->0x6FFF area)
   dd    EXP_MEM_READ            ; (0x7000<->0x7FFF area)
   dd    END_READ                ; (0x8000<->0x8FFF area) ; should never be
   dd    END_READ                ; (0x9000<->0x9FFF area) ; reached
   dd    END_READ                ; (0xA000<->0xAFFF area) ;
   dd    END_READ                ; (0xB000<->0xBFFF area) ;
   dd    END_READ                ; (0xC000<->0xCFFF area) ;
   dd    END_READ                ; (0xD000<->0xDFFF area) ;
   dd    END_READ                ; (0xE000<->0xEFFF area) ;
   dd    END_READ                ; (0xF000<->0xFFFF area) ;
;*********************************************************


;*********************************************************
; The index ranges from 0 to 7, holding the register low
; significant byte that selects the right register handler.
;

align 4, db 0
Sub_R2000_Sel_Tb:
            dd    R2000            ; PPU  (0x2000)
            dd    R2001            ; PPU  (0x2001)
            dd    R2002            ; PPU  (0x2002)
            dd    R2003            ; PPU  (0x2003)
            dd    R2004            ; PPU  (0x2004)
            dd    END_READ         ; PPU  (0x2005)
            dd    END_READ         ; PPU  (0x2006)
            dd    R2007            ; PPU  (0x2007)
;*********************************************************


;*********************************************************
; Same as 'Sub_R2000_Sel_Tb', but the indexes are relative
; from 0x4000 address range.
;

align 4, db 0
Sub_R4000_Sel_Tb:
            dd    END_READ         ; pAPU   (0x4000)
            dd    END_READ         ; pAPU   (0x4001)
            dd    END_READ         ; pAPU   (0x4002)
            dd    END_READ         ; pAPU   (0x4003)
            dd    END_READ         ; pAPU   (0x4004)
            dd    END_READ         ; pAPU   (0x4005)
            dd    END_READ         ; pAPU   (0x4006)
            dd    END_READ         ; pAPU   (0x4007)
            dd    END_READ         ; pAPU   (0x4008)
            dd    END_READ         ; pAPU   (0x4009)
            dd    END_READ         ; pAPU   (0x400A)
            dd    END_READ         ; pAPU   (0x400B)
            dd    END_READ         ; pAPU   (0x400C)
            dd    END_READ         ; pAPU   (0x400D)
            dd    END_READ         ; pAPU   (0x400E)
            dd    END_READ         ; pAPU   (0x400F)
            dd    END_READ         ; pAPU   (0x4010)
            dd    END_READ         ; pAPU   (0x4011)
            dd    END_READ         ; pAPU   (0x4012)
            dd    END_READ         ; pAPU   (0x4013)
            dd    END_READ         ; PPU    (0x4014)
            dd    END_READ         ; pAPU   (0x4015)
            dd    R4016            ; Joypad (0x4016)
            dd    END_READ         ; Joypad (0x4017)
;*********************************************************


;*********************************************************
; These are the 'write' versions of the above tables.
;

align 4, db 0
Main_WTrap_Sel_Tb:
   dd    END_WRITE               ; (0x0000<->0x0FFF area)
   dd    END_WRITE               ; (0x1000<->0x1FFF area)
   dd    RANGE_2000_WRITE        ; (0x2000<->0x2FFF area)
   dd    RANGE_2000_WRITE        ; (0x3000<->0x3FFF area)
   dd    RANGE_4000_WRITE        ; (0x4000<->0x4FFF area)
   dd    END_WRITE               ; (0x5000<->0x5FFF area)
   dd    EXP_MEM_WRITE           ; (0x6000<->0x6FFF area)
   dd    EXP_MEM_WRITE           ; (0x7000<->0x7FFF area)
   dd    END_WRITE               ; (0x8000<->0x8FFF area)
   dd    END_WRITE               ; (0x9000<->0x9FFF area)
   dd    END_WRITE               ; (0xA000<->0xAFFF area)
   dd    END_WRITE               ; (0xB000<->0xBFFF area)
   dd    END_WRITE               ; (0xC000<->0xCFFF area)
   dd    END_WRITE               ; (0xD000<->0xDFFF area)
   dd    END_WRITE               ; (0xE000<->0xEFFF area)
   dd    END_WRITE               ; (0xF000<->0xFFFF area)

align 4, db 0
Sub_W2000_Sel_Tb
            dd    W2000            ; PPU  (0x2000)
            dd    W2001            ; PPU  (0x2001)
            dd    W2002            ; PPU  (0x2002)
            dd    W2003            ; PPU  (0x2003)
            dd    W2004            ; PPU  (0x2004)
            dd    W2005            ; PPU  (0x2005)
            dd    W2006            ; PPU  (0x2006)
            dd    W2007            ; PPU  (0x2007)

align 4, db 0
Sub_W4000_Sel_Tb:
            dd    END_WRITE        ; pAPU   (0x4000)
            dd    END_WRITE        ; pAPU   (0x4001)
            dd    END_WRITE        ; pAPU   (0x4002)
            dd    END_WRITE        ; pAPU   (0x4003)
            dd    END_WRITE        ; pAPU   (0x4004)
            dd    END_WRITE        ; pAPU   (0x4005)
            dd    END_WRITE        ; pAPU   (0x4006)
            dd    END_WRITE        ; pAPU   (0x4007)
            dd    END_WRITE        ; pAPU   (0x4008)
            dd    END_WRITE        ; pAPU   (0x4009)
            dd    END_WRITE        ; pAPU   (0x400A)
            dd    END_WRITE        ; pAPU   (0x400B)
            dd    END_WRITE        ; pAPU   (0x400C)
            dd    END_WRITE        ; pAPU   (0x400D)
            dd    END_WRITE        ; pAPU   (0x400E)
            dd    END_WRITE        ; pAPU   (0x400F)
            dd    END_WRITE        ; pAPU   (0x4010)
            dd    END_WRITE        ; pAPU   (0x4011)
            dd    END_WRITE        ; pAPU   (0x4012)
            dd    END_WRITE        ; pAPU   (0x4013)
            dd    W4014            ; PPU    (0x4014)
            dd    END_WRITE        ; pAPU   (0x4015)
            dd    W4016            ; Joypad (0x4016)
            dd    END_WRITE        ; Joypad (0x4017)
;*********************************************************



;*********************************************************
; Returns the size of the sprite (0x7 -> 8x8, 0xF -> 8x16)
; given the Ctrl_Reg_1.
;

align 4, db 0
Sprite_Size:
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
            dd 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F 
;*********************************************************


;*********************************************************
; Returns the value used for increment 0x2006 given the
; Ctrl_Reg_1.
;

align 4, db 0
Incr_Table:
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
            dd 0x01, 0x01, 0x01, 0x01, 0x20, 0x20, 0x20, 0x20
;*********************************************************




;*********************************************************
; Returns the Sprite PT encoded value given the Ctrl_Reg_1.
;

align 4, db 0
Spr_PT_Table:
         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB
         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB

         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB
         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB

         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB
         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB

         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB
         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB

         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB
         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB

         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB
         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB

         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB
         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB

         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB
         dd 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF
         dd 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB, 0xFFFFFFFB
;*********************************************************


;*********************************************************
;  Returns 0x40 if the index is 0x00. Otherwise returns
; always 0x41.
;

align 4, db 0
Joy_Set_Table:
         dd    0x40, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41
         times 248 dd 0x41
;*********************************************************



;----------------------------------- // -----------------------------------
