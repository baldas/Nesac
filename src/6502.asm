;=============================================================================
;|
;|  NES 6502 custom-made emulator (c) 2000 - by _aLe()
;|
;|
;|  Assemble under NASM: nasm -f coff 6502.asm
;|  Link with DJGPP (or a compiler supporting COFF object format)
;|----------------------------------- // -----------------------------------
;|
;|  General description
;|
;|		This emulator was made in order to provide a specific 6502 NES cpu AS
;| FAST AS it can. So it's macro-based. It uses a JMP table to decode each
;| opcode. For more detailed information, give a look in this source file.
;|		To interface with this module the following functions are provided:
;|
;|###########################################################################
;|#		void RST_6502(void)                         
;|#
;|#	Emulates 6502 RST pin.
;|###########################################################################
;|###########################################################################
;|#		void NMI_6502(void)
;|#
;|# Emulates 6502 NMI pin.
;|###########################################################################
;|###########################################################################
;|#		void IRQ_6502(void)
;|#
;|# Emulates 6502 IRQ pin.
;|###########################################################################
;|###########################################################################
;|#		u32 RUN_6502(void)
;|#
;|# This does the main job of a real microprocessor (instruction execution):
;|#
;|#		1- Fetch byte (opcode) from memory (pointed by PC)
;|#		2- Update PC
;|#		3- Decode opcode to see what it does
;|#		4- If required, fetch an operand (and update PC again if needed)
;|#		5- Do what the instruction should do (move, logical operations ...)
;|#		6- Back to step 1
;|#
;|#	The number of cycles that will be executed is stored at _a6502_Cycles.
;|#
;|#	Return value (C-like -> in eax):
;|#
;|#	 0x00				= OK!
;|#	 0x0000XX00		= Bad Opcode trapped! Opcode is returned at XX.
;|#						  This will be returned ONLY IF '_TrapBadOpCodes' == TRUE
;|#
;|###########################################################################
;|
;|		You can also access some 6502 related data, as definied in the section
;| "GLOBAL SYMBOLS".
;|		To C/C++ interface see the relative header file (should be 6502.h).
;|		There's also an assembly header file (6502.ha) to be included in modules
;|	with uses this one.
;|----------------------------------- // -----------------------------------
;|
;|  Doubts/To be done
;|
;|		. "Add 1 if page boundary is crossed" - not implemented;
;|		. "Add 2 if branch occurs to different page" - not implemented;
;|		. Instructions ADC/SBC doubt:
;|		  got from "64doc.txt":
;|			"Flags after decimal operation:	NMOS: Invalid N, V and Z flags"
;|			Actually, this is not implemented. It's changed as a normal ADC/SBC.
;|		. Not implemented wrap from 0xFFFF to 0x0000 of 6502 PC register;
;|		. In functions/opcodes: NMI_6502 / IRQ_6502 / BRK
;|				"Set "break flag" before saving to stack ???"
;|				By now it's been saved.
;|----------------------------------- // -----------------------------------
;|
;|  History:
;|
;|   . v 1.0 - first version (end of debug: 01/14/2000)
;|      Implemented:
;|                    - All 6502 NMOS instructions.
;|                    - Pins RST, IRQ, NMI emulation.
;|
;| 01/30/2000
;|  . Found a BUG in my documents, which stated that 'PLA' doesn't set the
;| flags. After lost a entire night debugging SMB1 cart, I found that it DOES
;| need the PLA to set the flags.
;|
;| 02/01/2000
;|  . Some instructions were optimizaded (XOR EAX, EAX before main JMP). Debug
;| especial code was removed!
;|  . Registers "EAX, EBX, ECX and EDX" are no more PUSHed/POPed in the
;| 'functions'.
;|  . Made macro 'Safe_Stack' to preserve the correct working of the stack 
;| (wrap around).
;|  . 6502 related variables are now stored in the session '.data' instead of
;| '.bss'.
;|
;|
;| 02/09/2000
;|  . added code to tell apart 2 banks of code: 0x8000 and 0xC000.
;| OBS:
;|  1. It's not checked if executing code at the end of the 1st bank (e.g.
;| 0xBF..) and PC is being incremented normally, to enter the other bank
;| correctly.
;|  2. It's not either checked to change the bank throught 'branch' instr.
;|  3. It's not possible to RUN code on RAM.
;|
;|
;| 02/10/2000
;|  . As 'Dick Tracy' really DOES executes code in RAM, I HAD to implement this.
;| Now code CAN be executed in RAM!!!
;|
;| 02/12/2000
;|  . 'Kid Icarus' executes code in 'expansion RAM' (0x6000-0x7FFF) so I up-
;| grade this to allow code to be executed in expanded RAM.
;|
;| 02/16/2000
;|  . Implemented one-by-one read/write byte from/to memory! No more problems
;| with banks!!! Though, the speed, of course, has decresead :(
;|
;| 02/18/2000
;|  . Added some documentation in some parts of this module.
;|
;| 02/20/2000
;|  . Now all 6502 variables are 4-byte long (this seems to be faster, 'cause
;| alignment is in 4-byte page)
;|
;| 01/19/2001
;|  . Made a new organization of assembly files. Now there's a header for
;| assembly modules too (file extension = 'ha');
;|  . Created an assembly header file: '6502.ha';
;|  . file 'defs.inc' removed;
;|  . All data/code are 4-byte aligned;
;|	 . All functions preserve all the registers now (security);
;|
;| 01/21/2001
;|  . changed macro 'Get_Fis_Addr' to 'GET_80x86_ADDR'. It's clearer now!
;|  . remove some global data that was not used in this module (garbage 
;| colection :)
;|  . now all the 64k memory is divided into 8K banks
;|	 . change the working of macros 'Read6502Mem' and 'Write6502Mem': can 
;| call C-like extern functions now: ReadIO, WriteIO, MMC_Handler
;|	 . changed the opcode macro "CMx": 
;|			old -> "movzx eax, al" to new -> "and	eax, 0x00FF"
;|	 . changed order the "inc PC" appears in case of a branch in the "Branch"
;| macro.
;|	 . new data created: '_TrapBadOpCodes'. Setting this to TRUE will make 
;| function RUN_6502 return a value != 0x00 if a bad opcode is trapped.
;|
;|
;=============================================================================

%define alignment align 4 				; default alignment


;=============================================================================
; GLOBAL SYMBOLS
;=============================================================================
;------------
; Functions
;------------
   Global _RST_6502        ; void RST_6502(void)
   Global _NMI_6502        ; void NMI_6502(void)
   Global _IRQ_6502        ; void IRQ_6502(void)
   Global _RUN_6502        ; u32  RUN_6502(void)

;------------
; Data
;------------
; All data are 32-bit in size

   Global _a6502				; for more information about these, refer
   Global _a6502_PC			; to the section 'INTERFACE DATA' below
   Global _a6502_rA
   Global _a6502_rX
   Global _a6502_rY
   Global _a6502_rS
   Global _a6502_rF
   Global _a6502_Cycles
	Global _TrapBadOpCodes
   Global _PrgRamBase
	Global _cBank0x2000
	Global _cBank0x4000
   Global _cBank0x6000
   Global _cBank0x8000
   Global _cBank0xA000
   Global _cBank0xC000
   Global _cBank0xE000

;==================== END OF GLOBAL SYMBOLS DEFINITION =======================



;=============================================================================
; INTERFACE DATA
;=============================================================================
[section .data]

;	OBS:
;	_DO_NOT_ change the orders of these data (they're used in a structure in the
; C/C++ interface.

;	6502 registers are organized as follows (they are all 32-bit for speed reasons):
alignment
_a6502:
_a6502_PC:		dd 0x00			; Program Counter    (16-bit)
_a6502_rA:     dd 0x00        ; Accumulator        (8-bit)
_a6502_rX:     dd 0x00        ; X Index Reg.       (8-bit)
_a6502_rY:     dd 0x00        ; Y Index Reg.       (8-bit)
_a6502_rS:     dd 0xFF        ; Stack Pointer      (8-bit)
_a6502_rF:     dd 0x20        ; Processor Status   (8-bit)

;	Others important data:

_a6502_Cycles: dd 0x00			; number of cycles to be executed.
						; This is treated as a signed 32-bit value because this way 
						; the emulation is more accurate (suppose you set this variable 
						; with value 1 and call RUN_6502. Suppose the instruction takes
						; 3 cycles to execute. This way, after leaving RUN_6502, 
						; _a6502_Cycles will contain -2, that is, we  should  discount
						; 2 cycles from the total of cycles to be executed in the next
						; call to RUN_6502).

_TrapBadOpCodes: dd 0x00		; 0x00 -> FALSE / !0x00 -> TRUE
						; if it's desired to 'trap' bad opcodes set this to TRUE

; pointers to the memory banks (these MUST be consecutives in memory)
_PrgRamBase:   dd 0x00				; pointer to NES RAM memory
_cBank0x2000:	dd 0x00				;  
_cBank0x4000:	dd 0x00				; 
_cBank0x6000:  dd 0x00				; pointer to cartridge RAM (may be battery-backed)
_cBank0x8000:  dd 0x00				; pointer to PRG-ROM bank 0x8000
_cBank0xA000:  dd 0x00				; pointer to PRG-ROM bank 0xA000
_cBank0xC000:  dd 0x00				; pointer to PRG-ROM bank 0xC000
_cBank0xE000:  dd 0x00				; pointer to PRG-ROM bank 0xE000

;   The memory is divided into banks because of the NES mappers. It's easier to just
; change a pointer than moving a multiple of 8k chunck of memory.
;----------------------------------- // -----------------------------------


;=============================================================================
; INCLUDES/REQUIRED EXTERNAL REFERENCES
;=============================================================================
;
; The external references to the functions _ReadIO, _WriteIO, _MMC_Handler
;
%include "..\include\io.ha"
%include "..\include\mmc.ha"
;----------------------------------- // -----------------------------------


;=============================================================================
; LOCAL DEFINITIONS/MACROS
;=============================================================================
%define  FALSE    0
%define  TRUE     ~FALSE

; to speed things up, during execution we keep the 6502 registers within the
; 80x86 as stated below:
%define  PC    edi               ; 6502 <-> 80x86 used correspondences
%define  rA    cl
%define  rX    dh
%define  rY    dl
%define  rS    ebp
%define  rF    ch

%define  Base  esi					; _PrgRamBase


;  STATUS REGISTER (FLAGS)
;
;     Bit No.       7   6   5   4   3   2   1   0
;                   S   V   1   B   D   I   Z   C
;
Carry_Flag     equ 00000001b        ; C - Carry flag
Zero_Flag      equ 00000010b        ; Z - Zero flag
Interrupt_Flag equ 00000100b        ; I - Interrupt flag
Decimal_Flag   equ 00001000b        ; D - Decimal flag
Break_Flag     equ 00010000b        ; B - Break flag
Unused_Flag    equ 00100000b        ; 1 - Unused bit
Overflow_Flag  equ 01000000b        ; V - Overflow flag
Sign_Flag      equ 10000000b        ; S - Sign flag (or Negative flag)

; SET/RESET flags macros
;
%define Set_Carry_Flag        or    rF, Carry_Flag
%define Set_Zero_Flag         or    rF, Zero_Flag
%define Set_Interrupt_Flag    or    rF, Interrupt_Flag
%define Set_Decimal_Flag      or    rF, Decimal_Flag
%define Set_Break_Flag        or    rF, Break_Flag
%define Set_Unused_Flag       or    rF, Unused_Flag
%define Set_Overflow_Flag     or    rF, Overflow_Flag
%define Set_Sign_Flag         or    rF, Sign_Flag

%define Reset_Carry_Flag      and   rF, ~Carry_Flag
%define Reset_Zero_Flag       and   rF, ~Zero_Flag
%define Reset_Interrupt_Flag  and   rF, ~Interrupt_Flag
%define Reset_Decimal_Flag    and   rF, ~Decimal_Flag
%define Reset_Break_Flag      and   rF, ~Break_Flag
%define Reset_Unused_Flag     and   rF, ~Unused_Flag
%define Reset_Overflow_Flag   and   rF, ~Overflow_Flag
%define Reset_Sign_Flag       and   rF, ~Sign_Flag



;###########################################################################
;# Takes 6502 variables from memory to 80x86 registers (speed-up)
;###########################################################################
%macro GET_6502_FROM_MEMORY 0
   mov   PC, dword [_a6502_PC]      
   mov   rA, byte [_a6502_rA]       
   mov   rX, byte [_a6502_rX]
   mov   rY, byte [_a6502_rY]
   mov   rS, dword [_a6502_rS]
   mov   rF, byte [_a6502_rF]
   mov   Base, dword [_PrgRamBase]
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;# Move the 6502 registers back to their respective memory locations
;###########################################################################
%macro MOVE_6502_TO_MEMORY 0
   mov   dword [_a6502_PC], PC      
   mov   byte [_a6502_rA], rA
   mov   byte [_a6502_rX], rX
   mov   byte [_a6502_rY], rY
   mov   dword [_a6502_rS], rS
   mov   byte [_a6502_rF], rF
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;# Save the registers needed in a C-like function calling
;###########################################################################
%macro SAVE_TO_CALL 0
	push	ebp
	push	ecx
	push	edx
	push	esi
	push	edi
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;# Restore the registers needed in a C-like function calling (SAVE_TO_CALL)
;###########################################################################
%macro RESTORE_CALL 0
	pop	edi
	pop	esi
	pop	edx
	pop	ecx
	pop	ebp
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;# Calculates 80x86 address from 6502 address.
;#
;# Parameters:
;#     %1 -> 6502 logic address (0x0000-0xFFFF) (32-bit)
;#     %2 -> 32-bit register that will contain the 80x86 address
;#
;# Registers affected:
;#		NONE (only the one returned with the 80x86 address)
;#
;# OBS:
;#    . The registers %1, %2 _MUST_NOT_ be the same.
;#
;###########################################################################
%macro GET_80x86_ADDR 2
push  %1												; must preserve first parameter
	mov	%2, %1
	and	%2, 0x1FFF								; bank mask
	shr   %1, 13
   add	%2, dword [_PrgRamBase+%1*4]		; create the 80x86 address
pop   %1												; restore first parameter
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;# Reads a byte from 6502 memory.
;#
;# Parameters:
;#     eax  -> 6502 address
;#     %1   -> 8-bit register that will hold the byte read from memory
;#
;# Registers affected:
;#                       eax, ebx and %1(of course)
;#
;# Read Memory Map:
;#
;#		0x0000-0x1FFF		-> read from _PrgRamBase base memory
;#										memory above 0x0800 is mirror (8k RAM)
;#
;#		0x2000-0x7FFF		-> call an EXTERN procedure: u8 ReadIO(u32 address)
;#										the return data MUST come in the EAX register.
;#										the order of parameter passing is a C-like one.
;#
;#		0x8000-0xFFFF		-> read from the correct PRG-ROM bank
;#
;###########################################################################
%macro Read6502Mem 1
	cmp	eax, 0x2000
	jae	%%NOT_RAM						; RAM read (0x0000-0x1FFF)
		and	eax, 0x07FF					; NES mirror RAM
		mov	%1, byte [Base+eax]
		jmp	%%END_6502READ

alignment
%%NOT_RAM:
	cmp	eax, 0x8000						; I/O read (0x2000-0x7FFF)
	jae	%%ROM_READ
		SAVE_TO_CALL
		push	eax							; C-like parameter passing
	   call  _ReadIO						; I/O read handler
	   add   esp, 4
		RESTORE_CALL
		mov   %1, al						; expected return in eax (C-like)
		jmp	%%END_6502READ

alignment
%%ROM_READ:									; read from ROM (0x8000-0xFFFF)
	GET_80x86_ADDR eax, ebx		
   mov   %1, byte [ebx]

alignment
%%END_6502READ:
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;# Writes a byte to 6502 memory.
;#
;# Parameters:
;#     eax -> 6502 address
;#     %1  -> 8-bit register, which data will be written in memory
;#              
;# Registers affected:
;#                       eax, ebx
;#
;# Write Memory Map:
;#
;#		0x0000-0x1FFF		-> write to _PrgRamBase base memory
;#										memory above 0x0800 is mirror (8k RAM)
;#
;#		0x2000-0x7FFF		-> call an EXTERN procedure: void WriteIO(u32 address, u8 data)
;#										the order of parameter passing is a C-like one.
;#
;#		0x8000-0xFFFF		-> call an EXTERN procedure pointed by _MMC_Handler:
;#										void Mapper_Handler(u32 address, u8 data)
;#										the order of parameter passing is a C-like one.
;#
;###########################################################################
%macro Write6502Mem 1
	cmp	eax, 0x2000
	jae	%%NOT_RAM						; RAM write (0x0000-0x1FFF)
		and	eax, 0x07FF					; NES mirror RAM
      mov   byte [Base+eax], %1
      jmp   %%END_6502WRITE

alignment
%%NOT_RAM:
	cmp	eax, 0x8000						; I/O write (0x2000-0x7FFF)
	jae	%%ROM_WRITE
		SAVE_TO_CALL
		movzx ebx, %1        
	   push  ebx            			; C-like parameter passing (1st parameter
		push  eax							; is the last to be pushed)
		call  _WriteIO						; I/O write handler
	   add   esp, 8
		RESTORE_CALL
		jmp	%%END_6502WRITE

alignment
%%ROM_WRITE:								; write to ROM (0x8000-0xFFFF) (mapper stuff)
	SAVE_TO_CALL
	movzx ebx, %1              
   push  ebx								; call handler pointed by '_MMC_Handler'
   push  eax
   call  [_MMC_Handler]
   add   esp, 8
	RESTORE_CALL

alignment
%%END_6502WRITE:
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;# Emulates a fetch of a byte from 6502 memory (0x0000-0xFFFF)
;#
;# Parameters:
;#     %1 -> 8-bit register that will hold the byte fetched (cannot be EBX)
;#
;# Registers affected:
;#                       ebx and %1(of course)
;#
;# OBS:
;#        . As the name says, this macro is _ONLY_ for reads that uses PC as
;# a pointer into memory (like a real uP when fetching opcode/operand).
;#        . It already increment PC register.
;#
;###########################################################################
%macro FetchByte 1
   GET_80x86_ADDR PC, ebx
   mov   %1, byte [ebx]
   inc   PC								; doesn't check for wrap 0xFFFF -> 0x0000
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;# Emulates a fetch of a word from 6502 memory (0x0000-0xFFFF)
;#
;# Parameters:
;#     eax -> register that will hold the word fetched
;#
;# Registers affected:
;#                       ebx and eax(of course)
;#
;# OBS:
;#		This is only for simplicity, as a real 6502, being an 8-bit uP, it can't 
;# fetch a word in a single step. So, this fetchs two consecutives bytes making
;# a word stored in 80x86 register 'ax'.
;#
;###########################################################################
%macro FetchWord 0
   FetchByte al						; no problem if we pass a bank limit
   FetchByte ah
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;#		These macros are to CREATE THE OPERAND of an instruction given the 
;# 6502 addressing mode. The operand is returned in the eax 80x86 register.
;# No others registers are changed!
;#
;# Parameters:
;#		eax (output) -> 6502 operand (relative to addressing mode choosen)
;#
;# OBS:
;#		Register eax SHOULD be empty (zeroed) before 'calling' these macros.
;###########################################################################
%macro MakeImm 0              ; Immediate
%endmacro

%macro MakeZP 0               ; Zero-page Absolute
   FetchByte al                
%endmacro

%macro MakeZPX 0              ; Zero-page Indexed - X
   FetchByte al
   add   al, rX
%endmacro

%macro MakeZPY 0              ; Zero-page Indexed - Y
   FetchByte al
   add   al, rY
%endmacro

%macro MakeAbs 0              ; Absolute
   FetchWord
%endmacro

%macro MakeAbsX 0             ; Absolute Indexed - X
   FetchWord
   add   al, rX
   adc   ah, 0x00
%endmacro

%macro MakeAbsY 0             ; Absolute Indexed - Y
   FetchWord
   add   al, rY
   adc   ah, 0x00
%endmacro

%macro MakePreInd 0           ; Pre-indexed indirect
   FetchByte al
   add   al, rX
   mov   ax, word [Base+eax]
%endmacro

%macro MakePosInd 0           ; Post-indexed indirect
   FetchByte al
   mov   ax, [Base+eax]
   add   al, rY
   adc   ah, 0x00
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;#		These macros are used to READ a byte from 6502 memory relative to each
;# of the 6502 addressing modes.
;#
;# parameters:
;#
;#		eax -> 6502 address of the data that will be read (except for 'ReadImm')
;#		%1  -> 8-bit register to where the data will be written
;#
;# Registers affected:
;#								ebx, eax and %1(of course)
;#
;# OBS: 
;#		The ZP modes DO NOT use 'Read6502Mem' to read the memory. It uses a
;# 'move' directly for speed reasons (as NES don't use this range of memory 
;# for nothing special).
;###########################################################################
%macro ReadImm 1								; immediate operands use PC
   FetchByte %1
%endmacro

%macro ReadZP 1								; Zero-page Absolute
   mov   %1, byte [Base+eax]
%endmacro

%macro ReadZPX 1								; Zero-page Indexed - X
   mov   %1, byte [Base+eax]
%endmacro

%macro ReadZPY 1								; Zero-page Indexed - Y
   mov   %1, byte [Base+eax]
%endmacro

%macro ReadAbs 1								; Absolute
   Read6502Mem %1
%endmacro

%macro ReadAbsX 1								; Absolute Indexed - X
   Read6502Mem %1
%endmacro

%macro ReadAbsY 1								; Absolute Indexed - Y
   Read6502Mem %1
%endmacro

%macro ReadPreInd 1							; Pre-indexed indirect
   Read6502Mem %1
%endmacro

%macro ReadPosInd 1							; Post-indexed indirect
   Read6502Mem %1
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;#		These macros are used to WRITE a byte to 6502 memory relative to each
;# of the 6502 addressing modes.
;#
;# parameters:
;#
;#		eax -> 6502 address of the data that will be write (except for 'ReadImm')
;#		%1  -> 8-bit register with the data to be written
;#
;# Registers affected:
;#								eax, ebx
;#
;# OBS: 
;#		The ZP modes DO NOT use 'Write6502Mem' to write to memory. It uses a
;# 'move' directly for speed reasons (as NES don't use this range of memory 
;# for nothing special).
;###########################################################################
%macro WriteZP 1							; Zero-page Absolute
   mov   [Base+eax], %1
%endmacro

%macro WriteZPX 1							; Zero-page Indexed - X
   mov   [Base+eax], %1
%endmacro

%macro WriteZPY 1							; Zero-page Indexed - Y
   mov   [Base+eax], %1
%endmacro

%macro WriteAbs 1							; Absolute
   Write6502Mem %1
%endmacro

%macro WriteAbsX 1						; Absolute Indexed - X
   Write6502Mem %1
%endmacro

%macro WriteAbsY 1						; Absolute Indexed - Y
   Write6502Mem %1
%endmacro

%macro WritePreInd 1						; Pre-indexed indirect
   Write6502Mem %1
%endmacro

%macro WritePosInd 1						; Post-indexed indirect
   Write6502Mem %1
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;# Returns code to the main 'fetch-opcode' section (in RUN_6502).
;# Parameter %1 is the number of cycles to subtract from the total of cycles
;# to execute.
;###########################################################################
%macro Goto_Decode_OpCode 1
   mov   eax, %1
   jmp   Fetch_OpCode			; at RUN_6502
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;# Just clears the high order 24-bit from rS (6502 stack) register.
;###########################################################################
%macro Safe_Stack 0
   and   rS, 0x000000FF			; takes care of wrapping 0xFF -> 0x00
%endmacro
;----------------------------------- // -----------------------------------


;
; OPCODES MACROS
;
;	For a detailed description of a step-by-step instruction execution, check 
; out the '64doc.txt'
;
;
; EAX = 0 at start of any of these macros
;

;###########################################################################
;#*****************************
;# LDx - Operation:  M -> x                   Flags  
;#*****************************
;#  x = A, X ou Y                          N Z C I D V
;#                                         / / _ _ _ _
;#
;# Parameters:
;#		1  -> a possible addressing mode: Imm, ZP, ZPX, ZPY, Abs, AbsX, AbsY, 
;#				PreInd, PosInd.
;#		2  -> number of cycles that the instruction takes to execute.
;#		3  -> register that will hold the data read: rA, rX or rY.
;###########################################################################
%macro LDx 3
   Make%1                        ; eax = operand
   Read%1 %3                     ; %3 <- data

   movzx eax, %3						; adjust flags
   and   rF, 01111101b
   or    rF, [Flag_Table+eax]    
   Goto_Decode_OpCode %2
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# STx - Operation:  M <- x                   Flags  
;#*****************************
;#  x = A, X ou Y                          N Z C I D V
;#                                         _ _ _ _ _ _
;#
;# Parameters:
;#		1  -> a possible addressing mode: Imm, ZP, ZPX, ZPY, Abs, AbsX, AbsY, 
;#				PreInd, PosInd.
;#		2  -> number of cycles that the instruction takes to execute.
;#		3  -> register that will hold the data read: rA, rX ou rY.
;###########################################################################
%macro STx 3
   Make%1								; eax = operand
   Write%1 %3							; write at eax, data in %3
   Goto_Decode_OpCode %2      
%endmacro                     
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# INC/DEC - Operation:  M(+/-)1 -> M         Flags  
;#*****************************
;#                                         N Z C I D V
;#                                         / / _ _ _ _
;#
;# Parameters
;#		1  -> a possible addressing mode: Imm, ZP, ZPX, ZPY, Abs, AbsX, AbsY, 
;#				PreInd, PosInd.
;#		2  -> number of cycles that the instruction takes to execute.
;#		3  -> operation to be done: 'inc' or 'dec.
;###########################################################################
%macro Inc_Dec_Mem 3
   Make%1								; eax = operand
push  eax				; save operand (Read%1 may destroy eax!!!)
   Read%1 bl                     ; bl <- data at eax
   %3   bl                       ; inc/dec it
pop   eax				; restore operand
push  ebx				; save data
   Write%1 bl                    ; [eax] = bl
pop   ebx				; restore data
   movzx eax, bl						; adjust flags
   and   rF, 01111101b
   or    rF, [Flag_Table+eax]
   Goto_Decode_OpCode %2
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# INx/DEx - Operation:  x(+/-)1 -> x         Flags  
;#*****************************
;# x = X ou Y                              N Z C I D V
;#                                         / / _ _ _ _
;#
;# Parameters
;#		1  -> one of the 6502 register to be inc/dec: A, X or Y.
;#		2  -> number of cycles that the instruction takes to execute.
;#		3  -> operation to be done: 'inc' or 'dec.
;###########################################################################
%macro Inc_Dec_Reg 3
   %3    %1								; register version
   mov   al, %1
   and   rF, 01111101b
   or    rF, [Flag_Table+eax]
   Goto_Decode_OpCode %2
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# AND/ORA/EOR - Operation: A&M -> A          Flags  
;#*****************************         
;#                                         N Z C I D V
;#                                         / / _ _ _ _
;#
;# Parameters:
;#		1  -> a possible addressing mode: Imm, ZP, ZPX, ZPY, Abs, AbsX, AbsY, 
;#				PreInd, PosInd.
;#		2  -> number of cycles that the instruction takes to execute.
;#		3  -> operation to be done: 'and', 'or' or 'xor'.
;###########################################################################
%macro Logic 3
   Make%1
   Read%1 bl
   %3    rA, bl
   movzx eax, rA
   and   rF, 01111101b          
   or    rF, [Flag_Table+eax]
   Goto_Decode_OpCode %2
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;# Instructions: CLC, CLD, CLI, CLV, SEC, SED, SEI
;#
;# Parameters:
;#		1	-> operation to be done with a flag (set ou reset).
;###########################################################################
%macro Do_Flag 1
   %1
   Goto_Decode_OpCode 2
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# CMP/CPX/CPY - Operation:  (A/X/Y)-M        Flags  
;#*****************************
;#                                         N Z C I D V
;#                                         / / / _ _ _
;#
;# Flags values are given by table below:
;#          +-------------------------+---------------------+
;#          |                         |  N       Z       C  |
;#          +-------------------------+---------------------+
;#          | A, X, or Y  <  Memory   |  1       0       0  |
;#          | A, X, or Y  =  Memory   |  0       1       1  |
;#          | A, X, or Y  >  Memory   |  0       0       1  |
;#          +-----------------------------------------------+
;#
;# Parameters:
;#		1  -> a possible addressing mode: Imm, ZP, ZPX, ZPY, Abs, AbsX, AbsY, 
;#				PreInd, PosInd.
;#		2  -> number of cycles that the instruction takes to execute.
;#		3  -> one of the registers: A, X or Y
;###########################################################################
%macro CMx 3
   Make%1
   Read%1 bl
   mov   al, %3							; al = x (A, X or Y)
   sub   al, bl
   cmc                              ; 6502 flag is the opposite of the 80x86
   setc  bl
	and	eax, 0x00FF
   and   rF, 01111100b    
   or    rF, [Flag_Table+eax]
   or    rF, bl                     ; set carry
   Goto_Decode_OpCode %2
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# BIT - Operation:  A&M, M7 -> N, M6 -> V    Flags  
;#*****************************
;#                                         N Z C I D V
;#                                        M7 / _ _ _ M6
;#
;# Parameters:
;#		1  -> a possible addressing mode: Imm, ZP, ZPX, ZPY, Abs, AbsX, AbsY, 
;#				PreInd, PosInd.
;#		2  -> number of cycles that the instruction takes to execute.
;###########################################################################
%macro BIT 2
   Make%1
   Read%1 al                     ; al <- data
   test  rA, al                  ; rA & data
   setz  bl                      ; if (ZF == 1) -> bl = 1, else bl = 0
   and   al, 11000000b           ; isolate flags: Sign, Overflow
   shl   bl, 1                   ; adjust 6502 Zero flag bit position
   or    al, bl                  ; make an only byte
   and   rF, 00111101b           ; clear the flags that may be changed
   or    rF, al                  ; set flags
   Goto_Decode_OpCode %2
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************           +-+-+-+-+-+-+-+-+
;# ASL - Operation:                  C <- |7|6|5|4|3|2|1|0| <- 0
;#*****************************           +-+-+-+-+-+-+-+-+    
;#*****************************           +-+-+-+-+-+-+-+-+
;# LSR - Operation:                  0 -> |7|6|5|4|3|2|1|0| -> C
;#*****************************           +-+-+-+-+-+-+-+-+    
;#*****************************   +------------------------------+   
;# ROL - Operation:               |   +-+-+-+-+-+-+-+-+    +-+   |
;#*****************************   +-< |7|6|5|4|3|2|1|0| <- |C| <-+
;#                                   +-+-+-+-+-+-+-+-+    +-+
;#*****************************   +------------------------------+   
;# ROR - Operation:               |   +-+    +-+-+-+-+-+-+-+-+   |
;#*****************************   +-> |C| -> |7|6|5|4|3|2|1|0| >-+
;#                                    +-+    +-+-+-+-+-+-+-+-+
;#
;#                                            Flags
;#                                         N Z C I D V
;#                                         / / / _ _ _
;#
;# Parameters:
;#		1  -> a possible addressing mode: Imm, ZP, ZPX, ZPY, Abs, AbsX, AbsY, 
;#				PreInd, PosInd.
;#		2  -> number of cycles that the instruction takes to execute.
;#		3  -> operation to be done: 'shl'(asl), 'shr'(lsr), 'rcl'(rol) or 'rcr'(ror).
;#		4  -> FALSE (to 'shl' and 'shr') or TRUE (to 'rcl' and 'rcr')
;###########################################################################
%macro Rot_Mem 4        ; memory
   Make%1
push  eax					; save operand
   Read%1 bl                  ; bl <- data
   %if %4                     ; additional processing for 'rcl' e 'rcr'
      mov   al, rF
      shr   al, 0x01          ; carry flag 80x86 = carry flag 6502
   %endif
   %3    bl, 1
   setc  bh							; bh = 80x86 carry flag
pop   eax					; restore operand
push  ebx					; save carry flag information
   Write%1 bl                 ; [eax] <- bl (back to memory)
pop   ebx
   movzx eax, bl
   and   rF, 01111100b  
   or    rF, [Flag_Table+eax]
   or    rF, bh               ; set carry flag
   Goto_Decode_OpCode %2
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;# Instructions: ASL, LSR, ROL, ROR (accumulator version)
;#
;# Parameters:
;#		1  -> operation to be done: 'shl'(asl), 'shr'(lsr), 'rcl'(rol) or 'rcr'(ror).
;#		2  -> FALSE (to 'shl' and 'shr') or TRUE (to 'rcl' and 'rcr')
;###########################################################################
%macro Rot_Reg 2        ; accumulator version
   %if %2               
      mov   al, rF
      shr   al, 0x01    
   %endif
   %1    rA, 1
   setc  bh
   movzx eax, rA
   and   rF, 01111100b          
   or    rF, [Flag_Table+eax]
   or    rF, bh   
   Goto_Decode_OpCode 2
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# Txy - Operation:  x -> y   (TAX, TXA, TAY, TYA)
;#*****************************
;#					                               Flags
;#					                            N Z C I D V
;#                                         / / _ _ _ _
;#
;# Parameters:
;#		1	-> target register (rA, rX or rY)
;#		2  -> source register (rA, rX, or rY)
;###########################################################################
%macro Transf 2
   mov   %1, %2
   mov   al, %1
   and   rF, 01111101b   
   or    rF, [Flag_Table+eax]
   Goto_Decode_OpCode 2
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# PHx - Operation:  x to S     
;#*****************************
;# x = A or P                                 Flags  
;#                                         N Z C I D V
;#                                         _ _ _ _ _ _
;#
;# Parameters:
;#		1  -> register that will be saved in the stack (rA ou rF).
;###########################################################################
%macro PHx 1
   mov   [Base+rS+0x100], %1			; stack it
   dec   rS
   Safe_Stack
   Goto_Decode_OpCode 3
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# ADC - Operation:  A+M+C -> A               Flags  
;#*****************************
;#                                         N Z C I D V
;#                                         / / / _ _ /
;#
;# Parameters:
;#		1  -> a possible addressing mode: Imm, ZP, ZPX, ZPY, Abs, AbsX, AbsY, 
;#				PreInd, PosInd.
;#		2  -> number of cycles that the instruction takes to execute.
;###########################################################################
%macro ADC6502 2
   Make%1
   Read%1 bl                     ; bl <- data

   test   rF, Decimal_Flag		; decimal operation ?
   jne    short %%DECIMAL
      mov   al, rF					; set 80x86 "carry" flag ...
      shr   al, 0x01             ; ... based on the 6502 one
      adc   rA, bl               ; rA = rA + data + carry
      jmp   short %%CONTINUA		; 'jmp' doesn't change flags (no task switch here)

alignment
%%DECIMAL:							; decimal adding
   mov   al, rF                  ; set 80x86 "carry" flag ...
   shr   al, 0x01                ; ... based on the 6502 one
   adc   rA, bl						; rA = rA + data + carry
   mov   al, rA                  ; "daa" acts in 'al'
   daa                   
   mov   rA, al

alignment
%%CONTINUA:
   seto  bl                      ; bl = 1 if OV == 1
   setc  al                      ; al = 1 if CY == 1
   shl   bl, 6                   ; adjust OV to 6502
   or    bl, al                  ; only 1 byte
   and   rF, 00111100b           ; clear flags that may be changed
   movzx eax, rA
   or    rF, [Flag_Table+eax]    ; set flags
   or    rF, bl                  ; carry and overflow

   Goto_Decode_OpCode %2
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# SBC - Operation:  A-M-C' -> A              Flags  
;#*****************************
;#                                         N Z C I D V
;#                                         / / / _ _ /
;#
;# Parameters:
;#		1  -> a possible addressing mode: Imm, ZP, ZPX, ZPY, Abs, AbsX, AbsY, 
;#				PreInd, PosInd.
;#		2  -> number of cycles that the instruction takes to execute.
;###########################################################################
%macro SBC 2
   Make%1
   Read%1 bl							; bl <- data

   test   rF, Decimal_Flag		; decimal operation ?
   jne    short %%DECIMAL
										;"normal" subtraction
		mov   al, rF               ; sets 80x86 carry flag based on the 6502 one
		not   al                   ; As in 6502 we subtract 1 if CF = 0, we ...
		shr   al, 0x01             ; ... invert the carry flag
		sbb   rA, bl               ; rA = rA - bl - ~C
		jmp   short %%CONTINUA		; 'jmp' doesn't change flags (no task switch here)

alignment
%%DECIMAL:							; decimal operation
   mov   al, rF						; sets 80x86 carry flag based on the 6502 one
   not   al								; As in 6502 we subtract 1 if CF = 0, we ...
   shr   al, 0x01						; ... invert the carry flag
   sbb   rA, bl						; rA = rA - bl - ~C
   mov   al, rA						; "das" acts in al
   das                        
   mov   rA, al

alignment
%%CONTINUA:
   cmc               ; complements carry flag; subtraction on 6502 is opposite
							; relative to the 80x86
   seto  bl								; bl = 1 if OV == 1
   setc  al								; al = 1 se CY == 1
   shl   bl, 6							; adjust OV to 6502
   or    bl, al						; only one byte
   and   rF, 00111100b				; clear flags that may be changed
   movzx eax, rA
   or    rF, [Flag_Table+eax]		; set flags
   or    rF, bl						; carry and overflow

   Goto_Decode_OpCode %2
%endmacro
;----------------------------------- // -----------------------------------


;###########################################################################
;# Instructions: BCS, BEQ, BMI, BVS, BCC, BNE, BPL, BVC
;#
;# The branchs instructions don't change any of the 6502 flags.
;#
;# Parameters:
;#		1	-> condition to jump (caso dos flags setados)
;#		2	-> TRUE - jump if flag set / FALSE - jump if flag REset
;###########################################################################
%macro Branch 2
   %if %2							; jump if flag set
      test  rF, %1						; flag set ?
      je    short %%NO_BRANCH			; if not, go out
   %else								; jump if flag reset
      test  rF, %1						; flag REset ?      
      jne   short %%NO_BRANCH			; if not, go out
   %endif
										; flags set/reset
   GET_80x86_ADDR PC, ebx
   mov   al, [ebx]						; al <- jump offset
   inc   PC									; jump is relative to next address
   
   test  al, 10000000b        ; negative offset ?
   jne   short %%Negative     
      add   PC, eax					   ; normal addition
												; no 0xFFFF->0x0000 verification
      jmp   short %%Continua

alignment
%%Negative:							; 2's complement addition  
   neg   al                
   sub   PC, eax					
												; no 0xFFFF->0x0000 verification
alignment
%%Continua:							; branch was taken
   Goto_Decode_OpCode 3   
		                        
alignment
%%NO_BRANCH:						; no branch, only 2 cycles discount
   inc   PC									; go to next address
   Goto_Decode_OpCode 2
%endmacro
;----------------------------------- // -----------------------------------




;=============================================================================
; INTERFACE CODE
;=============================================================================
[section .text]


;###########################################################################
;#		void RST_6502(void)                         
;#
;#	Emulates 6502 RST pin.
;###########################################################################
alignment
_RST_6502:
   push	eax
	push	ebx

   xor   eax, eax
   mov   ebx, [_cBank0xE000]			; load PC with 0xFFFC/0xFFFD vector
   mov   ax, [ebx + 01FFCh]
   mov   [_a6502_PC], eax

   sub   dword [_a6502_Cycles], 6	; discount cycles

	pop	ebx
	pop	eax
ret
;----------------------------------- // -----------------------------------


;###########################################################################
;#		void NMI_6502(void)
;#
;# Emulates 6502 NMI pin.
;###########################################################################
alignment
_NMI_6502:
   pushad

   mov   rS, dword [_a6502_rS]
   mov   Base, dword [_PrgRamBase]

   mov   ebx, dword [_a6502_PC]			; save PC in stack
   mov   [Base+rS+100h], bh
   dec   rS
   Safe_Stack
   mov   [Base+rS+100h], bl
   dec   rS
   Safe_Stack

   Reset_Break_Flag          ; reset "break flag" before saving it in the stack ?

   mov   rF, byte [_a6502_rF]				; save flags in stack
   mov   [Base+rS+100h], rF      
   dec   rS                      
   Safe_Stack

   xor   eax, eax
   mov   ebx, [_cBank0xE000]				; load PC with 0xFFFA/0xFFFB vector
   mov   ax, [ebx + 01FFAh]
   mov   dword [_a6502_PC], eax

   sub   dword [_a6502_Cycles], 7		; discount cycles

   mov   dword [_a6502_rS], rS			; back to memory

   popad
ret
;----------------------------------- // -----------------------------------


;###########################################################################
;#		void IRQ_6502(void)
;#
;# Emulates 6502 IRQ pin.
;###########################################################################
alignment
_IRQ_6502:
   pushad

   test  byte [_a6502_rF], Interrupt_Flag      ; Interrupt flag enabled ?
   jne   near SAI_IRQ

	   mov   rS, dword [_a6502_rS]
	   mov   Base, dword [_PrgRamBase]

	   mov   ebx, dword [_a6502_PC]     ; save PC in stack
	   mov   [Base+rS+100h], bh
		dec   rS
		Safe_Stack
		mov   [Base+rS+100h], bl
		dec   rS
		Safe_Stack

		Reset_Break_Flag          ; reset "break flag" before saving it in the stack ?

	   mov   rF, byte [_a6502_rF]			; save flags in stack
	   mov   [Base+rS+100h], rF      
	   dec   rS
	   Safe_Stack

	   Set_Interrupt_Flag

		xor   eax, eax
		mov   ebx, [_cBank0xE000]			; load PC with 0xFFFE/0xFFFF vector
		mov   ax, [ebx + 01FFEh]
		mov   dword [_a6502_PC], eax

	   sub   dword [_a6502_Cycles], 7	; discount cycles

	   mov   dword [_a6502_rS], rS		; back to memory

SAI_IRQ:

   popad
ret
;----------------------------------- // -----------------------------------


;###########################################################################
;#		u32 RUN_6502(void)
;#
;# This does the main job of a real microprocessor (instruction execution):
;#
;#				1- Fetch byte (opcode) from memory (pointed by PC)
;#				2- Update PC
;#				3- Decode opcode to see what it does
;#				4- If required, fetch an operand (and update PC again if needed)
;#				5- Do what the instruction should do (move, logical operations ...)
;#				6- Back to step 1
;#
;#	The number of cycles that will be executed is stored at _a6502_Cycles.
;#
;#	Return value (C-like -> in eax):
;#
;#	 0x00				= OK!
;#	 0x0000XX00		= Bad Opcode trapped! Opcode is returned at XX.
;#						  This will be returned ONLY IF '_TrapBadOpCodes' == TRUE
;#
;###########################################################################
alignment
_RUN_6502:
   pushad									; save all registers
                  
	GET_6502_FROM_MEMORY

   xor   eax, eax                   ; first subtract 0x00 from cycles

;***
; It's the main loop (fetch/decode opcode)
;***
alignment
Fetch_OpCode:                     
   sub   [_a6502_Cycles], eax       ; update cycles
   jle   short EXIT_6502_NORMAL     ; are there more cycles ?
      xor   ebx, ebx                

      GET_80x86_ADDR PC, eax			; fetch opcode
      mov   bl, byte [eax]				
      inc   PC                      ; PC points to operand now

      xor   eax, eax                ; clear eax to use in the opcodes handlers
      jmp   [Op_Table+ebx*4]        ; decode opcode

;
; ATTENTION!!! ALL OPCODES ROUTINES _ALREADY_ BEGIN WITH "EAX = 0"
;

alignment
EXIT_6502_NORMAL:							; normal exit code

	MOVE_6502_TO_MEMORY

   popad										; restore the registers
	xor	eax, eax
ret

alignment
EXIT_6502_BAD_OPCODE:					; exit in case of a bad opcode
	mov	eax, ebx
	shl	eax, 0x04		; return value : 0x0000XX00 -> XX = opcode

	dec	PC					; makes PC point to the address of the bad opcode

	MOVE_6502_TO_MEMORY
	
	popad
ret
;----------------------------------- // -----------------------------------



;=============================================================================
; PRIVATE CODE
;=============================================================================
[section .text]

;
; These are the opcodes that will be called by RUN_6502.
;

; Special TRAP BAD OPCODES

;###########################################################################
;# Called when a 'BAD OPCODE' tries to execute
;#
;# It checks '_TrapBadOpCodes' to see if must or not trap.
;###########################################################################
alignment
TRAP:
   cmp   byte [_TrapBadOpCodes], 00h			; Trap bad opcodes ?
   jne    EXIT_6502_BAD_OPCODE
		Goto_Decode_OpCode 2							; do not trap opcodes, just
															; discount default 2 cycles
;----------------------------------- // -----------------------------------


;
; Below are the macro-based opcodes:
;

; LDA
alignment
OP_A9:   LDx Imm,    2, rA
alignment
OP_A5:   LDx ZP,     3, rA
alignment
OP_B5:   LDx ZPX,    4, rA
alignment
OP_AD:   LDx Abs,    4, rA
alignment
OP_BD:   LDx AbsX,   4, rA
alignment
OP_B9:   LDx AbsY,   4, rA
alignment
OP_A1:   LDx PreInd, 6, rA
alignment
OP_B1:   LDx PosInd, 5, rA
;----------------------------------- // -----------------------------------
; LDX
alignment
OP_A2:   LDx Imm,    2, rX
alignment
OP_A6:   LDx ZP,     3, rX
alignment
OP_B6:   LDx ZPY,    4, rX
alignment
OP_AE:   LDx Abs,    4, rX
alignment
OP_BE:   LDx AbsY,   4, rX
;----------------------------------- // -----------------------------------
; LDY
alignment
OP_A0:   LDx Imm,    2, rY
alignment
OP_A4:   LDx ZP,     3, rY
alignment
OP_B4:   LDx ZPX,    4, rY
alignment
OP_AC:   LDx Abs,    4, rY
alignment
OP_BC:   LDx AbsX,   4, rY
;----------------------------------- // -----------------------------------
; STA
alignment
OP_85:   STx ZP,     3, rA
alignment
OP_95:   STx ZPX,    4, rA
alignment
OP_8D:   STx Abs,    4, rA
alignment
OP_9D:   STx AbsX,   5, rA
alignment
OP_99:   STx AbsY,   5, rA
alignment
OP_81:   STx PreInd, 6, rA
alignment
OP_91:   STx PosInd, 6, rA
;----------------------------------- // -----------------------------------
; STX
alignment
OP_86:   STx ZP,  3, rX
alignment
OP_96:   STx ZPY, 4, rX
alignment
OP_8E:   STx Abs, 4, rX
;----------------------------------- // -----------------------------------
; STY
alignment
OP_84:   STx ZP,  3, rY
alignment
OP_94:   STx ZPX, 4, rY
alignment
OP_8C:   STx Abs, 4, rY
;----------------------------------- // -----------------------------------
; INC
alignment
OP_E6:   Inc_Dec_Mem ZP,   5, inc
alignment
OP_F6:   Inc_Dec_Mem ZPX,  6, inc
alignment
OP_EE:   Inc_Dec_Mem Abs,  6, inc
alignment
OP_FE:   Inc_Dec_Mem AbsX, 7, inc
;----------------------------------- // -----------------------------------
; DEC
alignment
OP_C6:   Inc_Dec_Mem ZP,   5, dec
alignment
OP_D6:   Inc_Dec_Mem ZPX,  6, dec
alignment
OP_CE:   Inc_Dec_Mem Abs,  6, dec
alignment
OP_DE:   Inc_Dec_Mem AbsX, 7, dec
;----------------------------------- // -----------------------------------
; INX
alignment
OP_E8:   Inc_Dec_Reg rX, 2, inc
;----------------------------------- // -----------------------------------
; INY
alignment
OP_C8:   Inc_Dec_Reg rY, 2, inc
;----------------------------------- // -----------------------------------
; DEX
alignment
OP_CA:   Inc_Dec_Reg rX, 2, dec
;----------------------------------- // -----------------------------------
; DEY
alignment
OP_88:   Inc_Dec_Reg rY, 2, dec
;----------------------------------- // -----------------------------------
; AND
alignment
OP_29:   Logic Imm,    2, and
alignment
OP_25:   Logic ZP,     3, and
alignment
OP_35:   Logic ZPX,    4, and
alignment
OP_2D:   Logic Abs,    4, and
alignment
OP_3D:   Logic AbsX,   4, and
alignment
OP_39:   Logic AbsY,   4, and
alignment
OP_21:   Logic PreInd, 6, and
alignment
OP_31:   Logic PosInd, 5, and
;----------------------------------- // -----------------------------------
; ORA
alignment
OP_09:   Logic Imm,    2, or
alignment
OP_05:   Logic ZP,     3, or
alignment
OP_15:   Logic ZPX,    4, or
alignment
OP_0D:   Logic Abs,    4, or
alignment
OP_1D:   Logic AbsX,   4, or
alignment
OP_19:   Logic AbsY,   4, or
alignment
OP_01:   Logic PreInd, 6, or
alignment
OP_11:   Logic PosInd, 5, or
;----------------------------------- // -----------------------------------
; EOR
alignment
OP_49:   Logic Imm,    2, xor
alignment
OP_45:   Logic ZP,     3, xor
alignment
OP_55:   Logic ZPX,    4, xor
alignment
OP_4D:   Logic Abs,    4, xor
alignment
OP_5D:   Logic AbsX,   4, xor
alignment
OP_59:   Logic AbsY,   4, xor
alignment
OP_41:   Logic PreInd, 6, xor
alignment
OP_51:   Logic PosInd, 5, xor
;----------------------------------- // -----------------------------------
; CLC   
alignment
OP_18:   Do_Flag {Reset_Carry_Flag}
;----------------------------------- // -----------------------------------
; CLD   
alignment
OP_D8:   Do_Flag {Reset_Decimal_Flag}
;----------------------------------- // -----------------------------------
; CLI   
alignment
OP_58:   Do_Flag {Reset_Interrupt_Flag}
;----------------------------------- // -----------------------------------
; CLV
alignment
OP_B8:   Do_Flag {Reset_Overflow_Flag}
;----------------------------------- // -----------------------------------
; SEC    
alignment
OP_38:   Do_Flag {Set_Carry_Flag}
;----------------------------------- // -----------------------------------
; SED  
alignment
OP_F8:   Do_Flag {Set_Decimal_Flag}
;----------------------------------- // -----------------------------------
; SEI 
alignment
OP_78:   Do_Flag {Set_Interrupt_Flag}
;----------------------------------- // -----------------------------------
; CMP
alignment
OP_C9:   CMx   Imm,    2, rA
alignment
OP_C5:   CMx   ZP,     3, rA
alignment
OP_D5:   CMx   ZPX,    4, rA
alignment
OP_CD:   CMx   Abs,    4, rA
alignment
OP_DD:   CMx   AbsX,   4, rA
alignment
OP_D9:   CMx   AbsY,   4, rA
alignment
OP_C1:   CMx   PreInd, 6, rA
alignment
OP_D1:   CMx   PosInd, 5, rA
;----------------------------------- // -----------------------------------
; CPX
alignment
OP_E0:   CMx   Imm,  2, rX
alignment
OP_E4:   CMx   ZP,   3, rX
alignment
OP_EC:   CMx   Abs,  4, rX
;----------------------------------- // -----------------------------------
; CPY
alignment
OP_C0:   CMx   Imm,  2, rY
alignment
OP_C4:   CMx   ZP,   3, rY
alignment
OP_CC:   CMx   Abs,  4, rY
;----------------------------------- // -----------------------------------
; BIT
alignment
OP_24:   BIT ZP,  3
alignment
OP_2C:   BIT Abs, 4
;----------------------------------- // -----------------------------------
; ASL - memory
alignment
OP_06:   Rot_Mem ZP,   5, shl, FALSE
alignment
OP_16:   Rot_Mem ZPX,  6, shl, FALSE
alignment
OP_0E:   Rot_Mem Abs,  6, shl, FALSE
alignment
OP_1E:   Rot_Mem AbsX, 7, shl, FALSE
;----------------------------------- // -----------------------------------
; LSR - memory
alignment
OP_46:   Rot_Mem ZP,   5, shr, FALSE
alignment
OP_56:   Rot_Mem ZPX,  6, shr, FALSE
alignment
OP_4E:   Rot_Mem Abs,  6, shr, FALSE
alignment
OP_5E:   Rot_Mem AbsX, 7, shr, FALSE
;----------------------------------- // -----------------------------------
; ROL - memory
alignment
OP_26:   Rot_Mem ZP,   5, rcl, TRUE
alignment
OP_36:   Rot_Mem ZPX,  6, rcl, TRUE
alignment
OP_2E:   Rot_Mem Abs,  6, rcl, TRUE
alignment
OP_3E:   Rot_Mem AbsX, 7, rcl, TRUE
;----------------------------------- // -----------------------------------
; ROR - memory
alignment
OP_66:   Rot_Mem ZP,   5, rcr, TRUE
alignment
OP_76:   Rot_Mem ZPX,  6, rcr, TRUE
alignment
OP_6E:   Rot_Mem Abs,  6, rcr, TRUE
alignment
OP_7E:   Rot_Mem AbsX, 7, rcr, TRUE
;----------------------------------- // -----------------------------------
; ASL - register
alignment
OP_0A:   Rot_Reg  shl, FALSE
;----------------------------------- // -----------------------------------
; LSR - register
alignment
OP_4A:   Rot_Reg  shr, FALSE
;----------------------------------- // -----------------------------------
; ROL - register
alignment
OP_2A:   Rot_Reg  rcl, TRUE
;----------------------------------- // -----------------------------------
; ROR - register
alignment
OP_6A:   Rot_Reg  rcr, TRUE
;----------------------------------- // -----------------------------------
; TAX
alignment
OP_AA:   Transf rX, rA
;----------------------------------- // -----------------------------------
; TXA
alignment
OP_8A:   Transf rA, rX
;----------------------------------- // -----------------------------------
; TAY
alignment
OP_A8:   Transf rY, rA
;----------------------------------- // -----------------------------------
; TYA
alignment
OP_98:   Transf rA, rY
;----------------------------------- // -----------------------------------
; PHA 
alignment
OP_48:   PHx rA
;----------------------------------- // -----------------------------------
; PHP
alignment
OP_08:   PHx rF
;----------------------------------- // -----------------------------------
; ADC
alignment
OP_69:   ADC6502  Imm,    2
alignment
OP_65:   ADC6502  ZP,     3
alignment
OP_75:   ADC6502  ZPX,    4
alignment
OP_6D:   ADC6502  Abs,    4
alignment
OP_7D:   ADC6502  AbsX,   4
alignment
OP_79:   ADC6502  AbsY,   4
alignment
OP_61:   ADC6502  PreInd, 6
alignment
OP_71:   ADC6502  PosInd, 5
;----------------------------------- // -----------------------------------
; SBC
alignment
OP_E9:   SBC   Imm,    2
alignment
OP_E5:   SBC   ZP,     3
alignment
OP_F5:   SBC   ZPX,    4
alignment
OP_ED:   SBC   Abs,    4
alignment
OP_FD:   SBC   AbsX,   4
alignment
OP_F9:   SBC   AbsY,   4
alignment
OP_E1:   SBC   PreInd, 6
alignment
OP_F1:   SBC   PosInd, 5
;----------------------------------- // -----------------------------------
; BCS 
alignment
OP_B0:   Branch Carry_Flag, TRUE
;----------------------------------- // -----------------------------------
; BEQ 
alignment
OP_F0:   Branch Zero_Flag, TRUE
;----------------------------------- // -----------------------------------
; BMI 
alignment
OP_30:   Branch Sign_Flag, TRUE
;----------------------------------- // -----------------------------------
; BVS 
alignment
OP_70:   Branch Overflow_Flag, TRUE
;----------------------------------- // -----------------------------------
; BCC
alignment
OP_90:   Branch Carry_Flag, FALSE
;----------------------------------- // -----------------------------------
; BNE 
alignment
OP_D0:   Branch Zero_Flag, FALSE
;----------------------------------- // -----------------------------------
; BPL 
alignment
OP_10:   Branch Sign_Flag, FALSE
;----------------------------------- // -----------------------------------
; BVC 
alignment
OP_50:   Branch Overflow_Flag, FALSE
;----------------------------------- // -----------------------------------


;
; Now, the other opcodes (not macro-based at all):
;


;###########################################################################
;#*****************************
;# TXS - Operation:  X -> S        Flags are not changed here
;#*****************************
;#*****************************
;# TSX - Operation:  S -> X      
;#*****************************
;#                                            Flags  
;#                                         N Z C I D V
;#                                         / / _ _ _ _
;#
;###########################################################################
; TXS
alignment
OP_9A:
   movzx rS, rX
   Goto_Decode_OpCode 2

; TSX                                        
alignment
OP_BA:
   mov   eax, rS
   mov   rX, al
   and   rF, 01111101b          
   or    rF, [Flag_Table+eax]
   Goto_Decode_OpCode 2
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# PLx - Operation:  x from S                 Flags  
;#*****************************
;#                                         N Z C I D V
;#                                         / / _ _ _ _/  From Stack
;#                                  ; Corrigido flags (SMB1) 01/30/2000
;#
;###########################################################################
; PLA
alignment
OP_68:
   inc   rS								; rS points to an empty place
   Safe_Stack
   mov   rA, [Base+rS+100h]
   mov   al, rA
   and   rF, 01111101b           
   or    rF, [Flag_Table+eax]
   Goto_Decode_OpCode 4

; PLP
alignment
OP_28:
   inc   rS
   Safe_Stack
   mov   rF, [Base+rS+100h]
   Goto_Decode_OpCode 4
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# JMP - Operation:  PC <- Addr               Flags  
;#*****************************
;#                                         N Z C I D V
;#                                         _ _ _ _ _ _
;#
;###########################################################################
; Absolute
alignment
OP_4C:
   MakeAbs
   mov   PC, eax
   Goto_Decode_OpCode 3

; Indirect   
alignment
OP_6C:
   MakeAbs									; eax = word operand
   mov   PC, eax
   GET_80x86_ADDR PC, ebx				; get byte pointed
   mov   al, [ebx]
   inc   PC									; next byte
   GET_80x86_ADDR PC, ebx				; get the other byte
   mov   ah, [ebx]

   mov   PC, eax							; those bytes made a 6502 address to jump
   Goto_Decode_OpCode 5
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# NOP - Operation:  loose 2 cycles           Flags  
;#*****************************
;#                                         N Z C I D V
;#                                         _ _ _ _ _ _
;#
;###########################################################################
alignment
OP_EA:
   Goto_Decode_OpCode 2
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# JSR - Operation:  PC+2 to S                Flags  
;#*****************************
;#                                         N Z C I D V
;#                                         _ _ _ _ _ _
;#
;###########################################################################
; Absolute 
alignment
OP_20:
   lea   ebx, [PC + 1]        ; bx <- 6502 address 1 byte before next instruction

   mov [Base+rS+100h], bh     ; save 1st byte
   dec   rS
   Safe_Stack
   mov [Base+rS+100h], bl     ; save 2nd byte
   dec   rS
   Safe_Stack

   GET_80x86_ADDR PC, ebx		; get the 6502 address to jump to
   mov   al, [ebx]
   inc   PC
   GET_80x86_ADDR PC, ebx
   mov   ah, [ebx]

   mov   PC, eax					; jump
   Goto_Decode_OpCode 6
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# RTS - Operation:  PC from S, PC++          Flags  
;#*****************************
;#                                         N Z C I D V
;#                                         _ _ _ _ _ _
;#
;###########################################################################
; Implied  
alignment
OP_60:
   inc   rS
   Safe_Stack
   mov   al, [Base+rS+100h]      ; restore low byte address
   inc   rS
   Safe_Stack
   mov   ah, [Base+rS+100h]      ; restore high byte address
   mov   PC, eax

   inc   PC                      ; must point to next byte
   Goto_Decode_OpCode 6
;----------------------------------- // -----------------------------------


;###########################################################################
;#*****************************
;# RTI - Operation:  F from S, PC from S      Flags  
;#*****************************
;#                                         N Z C I D V
;#                                          From Stack
;#
;###########################################################################
alignment
OP_40:
   inc   rS                   ; increment stack pointer
   Safe_Stack

   mov   rF, [Base+rS+100h]   ; restore flags first
   inc   rS
   Safe_Stack

   mov   al, [Base+rS+100h]	; restore callee address
   inc   rS                   
   Safe_Stack
   mov   ah, [Base+rS+100h]
   
	mov   PC, eax					; go back
   Goto_Decode_OpCode 6
;----------------------------------- // -----------------------------------


;###########################################################################
;*****************************
; BRK - Operation:  PC+2 to S, P to S        Flags  
;*****************************
;                                         N Z C I D V
;                                         _ _ _ 1 _ _
;
;###########################################################################
; Implied 
alignment
OP_00:
   lea   ebx, [PC+1]             ; ebx <- 6502 return address

   mov   [Base+rS+100h], bh      ; push in stack
   dec   rS
   Safe_Stack
   mov   [Base+rS+100h], bl
   dec   rS
   Safe_Stack

   Set_Break_Flag          ; set "break flag" before saving to stack ???

   mov   [Base+rS+100h], rF      ; push flags
   dec   rS                      
   Safe_Stack

   Set_Interrup_Flag             ; this is in "6502.txt" (true ?)

   xor   eax, eax
   mov   ebx, [_cBank0xE000]		; load PC with 0xFFFE/0xFFFF vector
   mov   ax, [ebx + 01FFEh]
   mov   PC, eax

   Goto_Decode_OpCode 7
;----------------------------------- // -----------------------------------




;=============================================================================
; PRIVATE DATA
;=============================================================================
[section .data]

;
; Tables
;

;###########################################################################
;# OpCode JMP table
;#
;# The index is a 6502 opcode got from memory. 
;###########################################################################
alignment
Op_Table:
         dd OP_00       ; 00 - BRK                
         dd OP_01       ; 01 - ORA - (Indirect,X) 
         dd TRAP        ; 02 - Future Expansion   
         dd TRAP        ; 03 - Future Expansion   
         dd TRAP        ; 04 - Future Expansion   
         dd OP_05       ; 05 - ORA - Zero Page    
         dd OP_06       ; 06 - ASL - Zero Page    
         dd TRAP        ; 07 - Future Expansion   
         dd OP_08       ; 08 - PHP                
         dd OP_09       ; 09 - ORA - Immediate    
         dd OP_0A       ; 0A - ASL - Accumulator  
         dd TRAP        ; 0B - Future Expansion   
         dd TRAP        ; 0C - Future Expansion   
         dd OP_0D       ; 0D - ORA - Absolute     
         dd OP_0E       ; 0E - ASL - Absolute     
         dd TRAP        ; 0F - Future Expansion   
         dd OP_10       ; 10 - BPL                
         dd OP_11       ; 11 - ORA - (Indirect),Y 
         dd TRAP        ; 12 - Future Expansion   
         dd TRAP        ; 13 - Future Expansion            
         dd TRAP        ; 14 - Future Expansion            
         dd OP_15       ; 15 - ORA - Zero Page,X           
         dd OP_16       ; 16 - ASL - Zero Page,X           
         dd TRAP        ; 17 - Future Expansion            
         dd OP_18       ; 18 - CLC                         
         dd OP_19       ; 19 - ORA - Absolute,Y            
         dd TRAP        ; 1A - Future Expansion            
         dd TRAP        ; 1B - Future Expansion            
         dd TRAP        ; 1C - Future Expansion            
         dd OP_1D       ; 1D - ORA - Absolute,X            
         dd OP_1E       ; 1E - ASL - Absolute,X            
         dd TRAP        ; 1F - Future Expansion            
         dd OP_20       ; 20 - JSR                         
         dd OP_21       ; 21 - AND - (Indirect,X)          
         dd TRAP        ; 22 - Future Expansion            
         dd TRAP        ; 23 - Future Expansion            
         dd OP_24       ; 24 - BIT - Zero Page             
         dd OP_25       ; 25 - AND - Zero Page             
         dd OP_26       ; 26 - ROL - Zero Page             
         dd TRAP        ; 27 - Future Expansion            
         dd OP_28       ; 28 - PLP                         
         dd OP_29       ; 29 - AND - Immediate             
         dd OP_2A       ; 2A - ROL - Accumulator           
         dd TRAP        ; 2B - Future Expansion            
         dd OP_2C       ; 2C - BIT - Absolute              
         dd OP_2D       ; 2D - AND - Absolute              
         dd OP_2E       ; 2E - ROL - Absolute              
         dd TRAP        ; 2F - Future Expansion            
         dd OP_30       ; 30 - BMI                         
         dd OP_31       ; 31 - AND - (Indirect),Y          
         dd TRAP        ; 32 - Future Expansion            
         dd TRAP        ; 33 - Future Expansion            
         dd TRAP        ; 34 - Future Expansion            
         dd OP_35       ; 35 - AND - Zero Page,X           
         dd OP_36       ; 36 - ROL - Zero Page,X           
         dd TRAP        ; 37 - Future Expansion            
         dd OP_38       ; 38 - SEC                         
         dd OP_39       ; 39 - AND - Absolute,Y            
         dd TRAP        ; 3A - Future Expansion            
         dd TRAP        ; 3B - Future Expansion            
         dd TRAP        ; 3C - Future Expansion            
         dd OP_3D       ; 3D - AND - Absolute,X            
         dd OP_3E       ; 3E - ROL - Absolute,X            
         dd TRAP        ; 3F - Future Expansion            
         dd OP_40       ; 40 - RTI                         
         dd OP_41       ; 41 - EOR - (Indirect,X)          
         dd TRAP        ; 42 - Future Expansion            
         dd TRAP        ; 43 - Future Expansion            
         dd TRAP        ; 44 - Future Expansion            
         dd OP_45       ; 45 - EOR - Zero Page             
         dd OP_46       ; 46 - LSR - Zero Page             
         dd TRAP        ; 47 - Future Expansion            
         dd OP_48       ; 48 - PHA                         
         dd OP_49       ; 49 - EOR - Immediate             
         dd OP_4A       ; 4A - LSR - Accumulator           
         dd TRAP        ; 4B - Future Expansion            
         dd OP_4C       ; 4C - JMP - Absolute              
         dd OP_4D       ; 4D - EOR - Absolute              
         dd OP_4E       ; 4E - LSR - Absolute              
         dd TRAP        ; 4F - Future Expansion            
         dd OP_50       ; 50 - BVC                         
         dd OP_51       ; 51 - EOR - (Indirect),Y          
         dd TRAP        ; 52 - Future Expansion            
         dd TRAP        ; 53 - Future Expansion            
         dd TRAP        ; 54 - Future Expansion            
         dd OP_55       ; 55 - EOR - Zero Page,X           
         dd OP_56       ; 56 - LSR - Zero Page,X           
         dd TRAP        ; 57 - Future Expansion            
         dd OP_58       ; 58 - CLI                         
         dd OP_59       ; 59 - EOR - Absolute,Y            
         dd TRAP        ; 5A - Future Expansion            
         dd TRAP        ; 5B - Future Expansion            
         dd TRAP        ; 5C - Future Expansion            
         dd OP_5D       ; 5D - EOR - Absolute,X            
         dd OP_5E       ; 5E - LSR - Absolute,X            
         dd TRAP        ; 5F - Future Expansion            
         dd OP_60       ; 60 - RTS                         
         dd OP_61       ; 61 - ADC - (Indirect,X)          
         dd TRAP        ; 62 - Future Expansion            
         dd TRAP        ; 63 - Future Expansion            
         dd TRAP        ; 64 - Future Expansion            
         dd OP_65       ; 65 - ADC - Zero Page             
         dd OP_66       ; 66 - ROR - Zero Page             
         dd TRAP        ; 67 - Future Expansion            
         dd OP_68       ; 68 - PLA                         
         dd OP_69       ; 69 - ADC - Immediate             
         dd OP_6A       ; 6A - ROR - Accumulator           
         dd TRAP        ; 6B - Future Expansion            
         dd OP_6C       ; 6C - JMP - Indirect              
         dd OP_6D       ; 6D - ADC - Absolute              
         dd OP_6E       ; 6E - ROR - Absolute              
         dd TRAP        ; 6F - Future Expansion            
         dd OP_70       ; 70 - BVS                         
         dd OP_71       ; 71 - ADC - (Indirect),Y          
         dd TRAP        ; 72 - Future Expansion            
         dd TRAP        ; 73 - Future Expansion            
         dd TRAP        ; 74 - Future Expansion            
         dd OP_75       ; 75 - ADC - Zero Page,X           
         dd OP_76       ; 76 - ROR - Zero Page,X           
         dd TRAP        ; 77 - Future Expansion            
         dd OP_78       ; 78 - SEI                         
         dd OP_79       ; 79 - ADC - Absolute,Y            
         dd TRAP        ; 7A - Future Expansion            
         dd TRAP        ; 7B - Future Expansion            
         dd TRAP        ; 7C - Future Expansion            
         dd OP_7D       ; 7D - ADC - Absolute,X            
         dd OP_7E       ; 7E - ROR - Absolute,X            
         dd TRAP        ; 7F - Future Expansion            
         dd TRAP        ; 80 - Future Expansion            
         dd OP_81       ; 81 - STA - (Indirect,X)          
         dd TRAP        ; 82 - Future Expansion            
         dd TRAP        ; 83 - Future Expansion            
         dd OP_84       ; 84 - STY - Zero Page             
         dd OP_85       ; 85 - STA - Zero Page             
         dd OP_86       ; 86 - STX - Zero Page             
         dd TRAP        ; 87 - Future Expansion            
         dd OP_88       ; 88 - DEY                         
         dd TRAP        ; 89 - Future Expansion            
         dd OP_8A       ; 8A - TXA                         
         dd TRAP        ; 8B - Future Expansion            
         dd OP_8C       ; 8C - STY - Absolute              
         dd OP_8D       ; 8D - STA - Absolute              
         dd OP_8E       ; 8E - STX - Absolute              
         dd TRAP        ; 8F - Future Expansion            
         dd OP_90       ; 90 - BCC                         
         dd OP_91       ; 91 - STA - (Indirect),Y          
         dd TRAP        ; 92 - Future Expansion            
         dd TRAP        ; 93 - Future Expansion            
         dd OP_94       ; 94 - STY - Zero Page,X           
         dd OP_95       ; 95 - STA - Zero Page,X           
         dd OP_96       ; 96 - STX - Zero Page,Y           
         dd TRAP        ; 97 - Future Expansion            
         dd OP_98       ; 98 - TYA                         
         dd OP_99       ; 99 - STA - Absolute,Y            
         dd OP_9A       ; 9A - TXS                         
         dd TRAP        ; 9B - Future Expansion            
         dd TRAP        ; 9C - Future Expansion            
         dd OP_9D       ; 9D - STA - Absolute,X            
         dd TRAP        ; 9E - Future Expansion            
         dd TRAP        ; 9F - Future Expansion            
         dd OP_A0       ; A0 - LDY - Immediate             
         dd OP_A1       ; A1 - LDA - (Indirect,X)          
         dd OP_A2       ; A2 - LDX - Immediate             
         dd TRAP        ; A3 - Future Expansion            
         dd OP_A4       ; A4 - LDY - Zero Page             
         dd OP_A5       ; A5 - LDA - Zero Page             
         dd OP_A6       ; A6 - LDX - Zero Page             
         dd TRAP        ; A7 - Future Expansion            
         dd OP_A8       ; A8 - TAY                         
         dd OP_A9       ; A9 - LDA - Immediate             
         dd OP_AA       ; AA - TAX                         
         dd TRAP        ; AB - Future Expansion            
         dd OP_AC       ; AC - LDY - Absolute              
         dd OP_AD       ; AD - LDA - Absolute              
         dd OP_AE       ; AE - LDX - Absolute              
         dd TRAP        ; AF - Future Expansion            
         dd OP_B0       ; B0 - BCS                         
         dd OP_B1       ; B1 - LDA - (Indirect),Y          
         dd TRAP        ; B2 - Future Expansion            
         dd TRAP        ; B3 - Future Expansion            
         dd OP_B4       ; B4 - LDY - Zero Page,X           
         dd OP_B5       ; B5 - LDA - Zero Page,X           
         dd OP_B6       ; B6 - LDX - Zero Page,Y           
         dd TRAP        ; B7 - Future Expansion            
         dd OP_B8       ; B8 - CLV                         
         dd OP_B9       ; B9 - LDA - Absolute,Y            
         dd OP_BA       ; BA - TSX                         
         dd TRAP        ; BB - Future Expansion            
         dd OP_BC       ; BC - LDY - Absolute,X            
         dd OP_BD       ; BD - LDA - Absolute,X            
         dd OP_BE       ; BE - LDX - Absolute,Y            
         dd TRAP        ; BF - Future Expansion            
         dd OP_C0       ; C0 - CPY - Immediate             
         dd OP_C1       ; C1 - CMP - (Indirect,X)          
         dd TRAP        ; C2 - Future Expansion            
         dd TRAP        ; C3 - Future Expansion            
         dd OP_C4       ; C4 - CPY - Zero Page    
         dd OP_C5       ; C5 - CMP - Zero Page    
         dd OP_C6       ; C6 - DEC - Zero Page             
         dd TRAP        ; C7 - Future Expansion            
         dd OP_C8       ; C8 - INY                         
         dd OP_C9       ; C9 - CMP - Immediate             
         dd OP_CA       ; CA - DEX                         
         dd TRAP        ; CB - Future Expansion            
         dd OP_CC       ; CC - CPY - Absolute              
         dd OP_CD       ; CD - CMP - Absolute              
         dd OP_CE       ; CE - DEC - Absolute              
         dd TRAP        ; CF - Future Expansion            
         dd OP_D0       ; D0 - BNE                         
         dd OP_D1       ; D1 - CMP (Indirect),Y            
         dd TRAP        ; D2 - Future Expansion            
         dd TRAP        ; D3 - Future Expansion            
         dd TRAP        ; D4 - Future Expansion            
         dd OP_D5       ; D5 - CMP - Zero Page,X           
         dd OP_D6       ; D6 - DEC - Zero Page,X           
         dd TRAP        ; D7 - Future Expansion            
         dd OP_D8       ; D8 - CLD                         
         dd OP_D9       ; D9 - CMP - Absolute,Y            
         dd TRAP        ; DA - Future Expansion            
         dd TRAP        ; DB - Future Expansion            
         dd TRAP        ; DC - Future Expansion            
         dd OP_DD       ; DD - CMP - Absolute,X            
         dd OP_DE       ; DE - DEC - Absolute,X            
         dd TRAP        ; DF - Future Expansion            
         dd OP_E0       ; E0 - CPX - Immediate             
         dd OP_E1       ; E1 - SBC - (Indirect,X)          
         dd TRAP        ; E2 - Future Expansion            
         dd TRAP        ; E3 - Future Expansion            
         dd OP_E4       ; E4 - CPX - Zero Page    
         dd OP_E5       ; E5 - SBC - Zero Page             
         dd OP_E6       ; E6 - INC - Zero Page             
         dd TRAP        ; E7 - Future Expansion            
         dd OP_E8       ; E8 - INX                         
         dd OP_E9       ; E9 - SBC - Immediate             
         dd OP_EA       ; EA - NOP                         
         dd TRAP        ; EB - Future Expansion            
         dd OP_EC       ; EC - CPX - Absolute              
         dd OP_ED       ; ED - SBC - Absolute              
         dd OP_EE       ; EE - INC - Absolute              
         dd TRAP        ; EF - Future Expansion            
         dd OP_F0       ; F0 - BEQ                         
         dd OP_F1       ; F1 - SBC - (Indirect),Y          
         dd TRAP        ; F2 - Future Expansion            
         dd TRAP        ; F3 - Future Expansion            
         dd TRAP        ; F4 - Future Expansion            
         dd OP_F5       ; F5 - SBC - Zero Page,X           
         dd OP_F6       ; F6 - INC - Zero Page,X           
         dd TRAP        ; F7 - Future Expansion            
         dd OP_F8       ; F8 - SED                         
         dd OP_F9       ; F9 - SBC - Absolute,Y            
         dd TRAP        ; FA - Future Expansion            
         dd TRAP        ; FB - Future Expansion            
         dd TRAP        ; FC - Future Expansion            
         dd OP_FD       ; FD - SBC - Absolute,X            
         dd OP_FE       ; FE - INC - Absolute,X            
         dd TRAP        ; FF - Future Expansion            
;----------------------------------- // -----------------------------------


;###########################################################################
;# Zero/Sign Table
;#
;# This is a speed table to correctly set the 6502 flags
;###########################################################################
alignment
Flag_Table:
            db 02h, 00h, 00h, 00h, 00h, 00h, 00h, 00h    ; 00h - 0Fh
            db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h
            db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h    ; 10h - 1Fh
            db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h
            db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h    ; 20h - 2Fh
            db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h
            db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h    ; 30h - 3Fh
            db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h
            db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h    ; 40h - 4Fh
            db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h
            db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h    ; 50h - 5Fh
            db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h
            db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h    ; 60h - 6Fh
            db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h
            db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h    ; 70h - 7Fh
            db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h    ; 80h - 8Fh
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h    ; 90h - 9Fh
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h    ; A0h - AFh
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h    ; B0h - BFh
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h    ; C0h - CFh
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h    ; D0h - DFh
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h    ; E0h - EFh
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h    ; F0h - FFh
            db 80h, 80h, 80h, 80h, 80h, 80h, 80h, 80h
;----------------------------------- // -----------------------------------

; EOF