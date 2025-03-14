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
;|	20/01/2000:
;|		. implemented video functions:
;|			- Set video mode (using bios' function 0x10);
;|       - 256x256 ModeQ (linear)
;|
;|
;| 04/02/2001:
;|		. 'DrawScanline' now belongs to this module;
;|		. text are now displayed through the 'frame_buffer' pointer buffer;
;|
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

	Global	_Go_256x256
   Global	_SetVideo
   Global   _ChangePal
   Global   _SetNESPal
   Global   _ToBuffer
   Global	_WaitVRetrace
	Global	_WaitHRetrace
	Global	_DrawScanline

;------------
; Data
;------------

	Global _NesPal


;==================== END OF GLOBAL SYMBOLS DEFINITION =======================



;=============================================================================
; INTERFACE DATA
;=============================================================================
[section .data]

align 4
_NesPal:
         db 0x75 >> 2, 0x75 >> 2, 0x75 >> 2, 00       ; 0
         db 0x27 >> 2, 0x1B >> 2, 0x8F >> 2, 00       ; 1
         db 0x00 >> 2, 0x00 >> 2, 0xAB >> 2, 00       ; 2
         db 0x47 >> 2, 0x00 >> 2, 0x9F >> 2, 00       ; 3
         db 0x8F >> 2, 0x00 >> 2, 0x77 >> 2, 00       ; 4
         db 0xAB >> 2, 0x00 >> 2, 0x13 >> 2, 00       ; 5
         db 0xA7 >> 2, 0x00 >> 2, 0x00 >> 2, 00       ; 6
         db 0x7F >> 2, 0x0B >> 2, 0x00 >> 2, 00       ; 7
         db 0x43 >> 2, 0x2F >> 2, 0x00 >> 2, 00       ; 8
         db 0x00 >> 2, 0x78 >> 2, 0x00 >> 2, 00       ; 9
         db 0x00 >> 2, 0x6B >> 2, 0x00 >> 2, 00       ; A ;
         db 0x00 >> 2, 0x3F >> 2, 0x17 >> 2, 00       ; B
         db 0x1B >> 2, 0x3F >> 2, 0x5F >> 2, 00       ; C
         db 0x00 >> 2, 0x00 >> 2, 0x00 >> 2, 00       ; D
         db 0x00 >> 2, 0x00 >> 2, 0x00 >> 2, 00       ; E
         db 0x00 >> 2, 0x00 >> 2, 0x00 >> 2, 00       ; F
         db 0xBC >> 2, 0xBC >> 2, 0xBC >> 2, 00       ; 10
         db 0x00 >> 2, 0x73 >> 2, 0xEF >> 2, 00       ; 11
         db 0x23 >> 2, 0x3B >> 2, 0xEF >> 2, 00       ; 12
         db 0x83 >> 2, 0x00 >> 2, 0xF3 >> 2, 00       ; 13
         db 0xBF >> 2, 0x00 >> 2, 0xBF >> 2, 00       ; 14
         db 0xE7 >> 2, 0x00 >> 2, 0x5B >> 2, 00       ; 15
         db 0xDB >> 2, 0x2B >> 2, 0x00 >> 2, 00       ; 16
         db 0xCB >> 2, 0x4F >> 2, 0x0F >> 2, 00       ; 17
         db 0x8B >> 2, 0x73 >> 2, 0x00 >> 2, 00       ; 18
         db 0x00 >> 2, 0x97 >> 2, 0x00 >> 2, 00       ; 19
         db 0x00 >> 2, 0xAB >> 2, 0x00 >> 2, 00       ; 1A
         db 0x00 >> 2, 0x93 >> 2, 0x3B >> 2, 00       ; 1B
         db 0x00 >> 2, 0x83 >> 2, 0x8B >> 2, 00       ; 1C
         db 0x00 >> 2, 0x00 >> 2, 0x00 >> 2, 00       ; 1D
         db 0x00 >> 2, 0x00 >> 2, 0x00 >> 2, 00       ; 1E
         db 0x00 >> 2, 0x00 >> 2, 0x00 >> 2, 00       ; 1F
         db 0xFF >> 2, 0xFF >> 2, 0xFF >> 2, 00       ; 20
         db 0x3F >> 2, 0xBF >> 2, 0xFF >> 2, 00       ; 21
         db 0x5F >> 2, 0x97 >> 2, 0xFF >> 2, 00       ; 22
         db 0xA7 >> 2, 0x8B >> 2, 0xFD >> 2, 00       ; 23
         db 0xF7 >> 2, 0x7B >> 2, 0xFF >> 2, 00       ; 24
         db 0xFF >> 2, 0x77 >> 2, 0xB7 >> 2, 00       ; 25
         db 0xFF >> 2, 0x77 >> 2, 0x63 >> 2, 00       ; 26
         db 0xFF >> 2, 0x9B >> 2, 0x3B >> 2, 00       ; 27
         db 0xF3 >> 2, 0xBF >> 2, 0x3F >> 2, 00       ; 28
         db 0x83 >> 2, 0xD3 >> 2, 0x13 >> 2, 00       ; 29
         db 0x4F >> 2, 0xDF >> 2, 0x4B >> 2, 00       ; 2A
         db 0x58 >> 2, 0xF8 >> 2, 0x98 >> 2, 00       ; 2B
         db 0x00 >> 2, 0xEB >> 2, 0xDB >> 2, 00       ; 2C
         db 0x78 >> 2, 0x78 >> 2, 0x78 >> 2, 00       ; 2D     ;;
         db 0x00 >> 2, 0x00 >> 2, 0x00 >> 2, 00       ; 2E
         db 0x00 >> 2, 0x00 >> 2, 0x00 >> 2, 00       ; 2F
         db 0xFF >> 2, 0xFF >> 2, 0xFF >> 2, 00       ; 30
         db 0xAB >> 2, 0xEF >> 2, 0xFF >> 2, 00       ; 31
         db 0xC7 >> 2, 0xD7 >> 2, 0xFF >> 2, 00       ; 32
         db 0xEF >> 2, 0xCB >> 2, 0xFF >> 2, 00       ; 33
         db 0xFF >> 2, 0xC7 >> 2, 0xFF >> 2, 00       ; 34
         db 0xFF >> 2, 0xC7 >> 2, 0xDB >> 2, 00       ; 35
         db 0xFF >> 2, 0xBF >> 2, 0xB3 >> 2, 00       ; 36
         db 0xFF >> 2, 0xDB >> 2, 0xAB >> 2, 00       ; 37
         db 0xFF >> 2, 0xE7 >> 2, 0xA3 >> 2, 00       ; 38
         db 0xE3 >> 2, 0xFF >> 2, 0xA3 >> 2, 00       ; 39
         db 0xAB >> 2, 0xF3 >> 2, 0xBF >> 2, 00       ; 3A
         db 0xB3 >> 2, 0xFF >> 2, 0xCF >> 2, 00       ; 3B
         db 0x9F >> 2, 0xFF >> 2, 0xF3 >> 2, 00       ; 3C
         db 0xF8 >> 2, 0xD8 >> 2, 0xF8 >> 2, 00       ; 3D
         db 0x00 >> 2, 0x00 >> 2, 0x00 >> 2, 00       ; 3E
         db 0x00 >> 2, 0x00 >> 2, 0x00 >> 2, 00       ; 3F
;----------------------------------- // -----------------------------------



;=============================================================================
; INCLUDES/REQUIRED EXTERNAL REFERENCES
;=============================================================================
%include "..\include\ppu.ha"
	extern	_frame_buffer
	extern	_video_base
	extern	_G_Buf
	extern	_VideoSel     ; word  - selector used to access video memory at 0xA0000

;----------------------------------- // -----------------------------------



;=============================================================================
; LOCAL DEFINITIONS/MACROS
;=============================================================================
; Some VGA registers definitions
%define ATTRCON_ADDR    0x3C0
%define MISC_ADDR       0x3C2
%define VGAENABLE_ADDR  0x3C3
%define SEQ_ADDR        0x3C4
%define GRACON_ADDR     0x3CE
%define CRTC_ADDR       0x3D4
%define STATUS_ADDR     0x3DA

;----------------------------------- // -----------------------------------





;=============================================================================
; INTERFACE CODE
;=============================================================================
[section .text]

;###########################################################################
;#		void Go_256x256(void)                         
;#
;#
;###########################################################################
align 4
_Go_256x256:

   mov   ax, 0013h
   int   10h

;***
   mov   edx, CRTC_ADDR
   mov   al, 0x11
   out   dx, al

   mov   edx, 0x3D5
   in    al, dx

   mov   ah, al
   and   ah, 0x7F

   mov   edx, CRTC_ADDR
   mov   al, 0x11
   out   dx, ax
;****   

   mov   edx, MISC_ADDR
   mov   al, 0xE3
   out   dx, al

   mov   edx, CRTC_ADDR
   mov   al, 0x00
   mov   ah, 0x5F
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x01
   mov   ah, 0x3F
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x02
   mov   ah, 0x40
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x03
   mov   ah, 0x82
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x04
   mov   ah, 0x4A
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x05
   mov   ah, 0x9A
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x06
   mov   ah, 0x23
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x07
   mov   ah, 0xB2
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x08
   mov   ah, 0x00
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x09
   mov   ah, 0x61
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x10
   mov   ah, 0x0A
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x11
   mov   ah, 0xAC
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x12
   mov   ah, 0xFF
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x13
   mov   ah, 0x20
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x14
   mov   ah, 0x40
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x15
   mov   ah, 0x07
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x16
   mov   ah, 0x1A
   out   dx, ax

   mov   edx, CRTC_ADDR
   mov   al, 0x17
   mov   ah, 0xA3
   out   dx, ax

   mov   edx, SEQ_ADDR
   mov   al, 0x01
   mov   ah, 0x01
   out   dx, ax

   mov   edx, SEQ_ADDR
   mov   al, 0x04
   mov   ah, 0x0E
   out   dx, ax

   mov   edx, SEQ_ADDR
   mov   al, 0x05
   mov   ah, 0x40
   out   dx, ax

   mov   edx, SEQ_ADDR
   mov   al, 0x06
   mov   ah, 0x05
   out   dx, ax

   mov   edx, STATUS_ADDR     ; reset read/write flip-flop
   in    al, dx
   mov   edx, ATTRCON_ADDR
   mov   al, 0x10 | 0x20      ; ensure VGA output is enabled
   out   dx, al
   mov   edx, ATTRCON_ADDR    
   mov   al, 0x41
   out   dx, al

   mov   edx, STATUS_ADDR     ; reset read/write flip-flop
   in    al, dx
   mov   edx, ATTRCON_ADDR
   mov   al, 0x13 | 0x20      ; ensure VGA output is enabled
   out   dx, al
   mov   edx, ATTRCON_ADDR    
   mov   al, 0x00
   out   dx, al

ret
;----------------------------------- // -----------------------------------


;###########################################################################
;#		void SetVideo(u8 mode)                         
;#
;#
;###########################################################################
align 4
_SetVideo:
   push  ebp
   mov   ebp, esp

   xor   eax, eax
   mov   al, byte [ebp+8]
   int   10h

   pop ebp

ret
;----------------------------------- // -----------------------------------



;###########################################################################
;#		void ChangePal(u8 entry, u8 red, u8 green, u8 blue)
;#
;#
;###########################################################################
align 4
_ChangePal:

   mov   eax, [esp+4]      ; eax = CLUT entry

   mov   edx, 0x3C8        ; Seleciona indice no CLUT
   out   dx, al 

   mov   edx, 0x3C9        ; Endereco para dados

   mov   eax, [esp+8]         ; Write "RED"
   out   dx, al 

   mov   eax, [esp+12]       ; Write "GREEN"
   out   dx, al 

   mov   eax, [esp+16]       ; Write "BLUE"
   out   dx, al 

ret
;----------------------------------- // -----------------------------------




;###########################################################################
;#		void SetNESPal(void)
;#
;#
;###########################################################################
align 4
_SetNESPal:
   pushad

   mov   edx, 0x3C8        ; select a CLUT index
   mov   eax, 0x40          ; first one (border reasons!)
   out   dx, al            ; go!

   mov   edx, 0x3C9        ; data address
	mov	ebx, 0x00
PAL_LOOP:
   mov   al, byte [_NesPal+ebx*4]          ; RED
   out   dx, al 
   mov   al, byte [_NesPal+ebx*4+1]        ; GREEN
   out   dx, al 
   mov   al, byte [_NesPal+ebx*4+2]        ; BLUE
   out   dx, al

   inc	ebx
	cmp	ebx, 0x40
 jne PAL_LOOP

   popad

ret
;----------------------------------- // -----------------------------------


;###########################################################################
;#		void ToBuffer(u32 X, u32 Y, u32 Index)
;#
;#
;###########################################################################
align 4
_ToBuffer:
	push	ecx
	push	ebx

   mov   eax, [esp+20]        ; eax = index for the table
   shl   eax, 6               ; eax = eax * 64

   mov   ebx, [esp+16]        ; ebx = Y
   shl   ebx, 8					; *256
   add   ebx, [esp+12]        ; (Y*256) + X
   add   ebx, _frame_buffer	; buffer offset


   mov   ecx, [Char_Table+eax]
   mov   [ebx], ecx
   mov   ecx, [Char_Table+eax+4]
   mov   [ebx+4], ecx
   mov   ecx, [Char_Table+eax+8]
   mov   [ebx+0x100], ecx
   mov   ecx, [Char_Table+eax+12]
   mov   [ebx+0x104], ecx
   mov   ecx, [Char_Table+eax+16]
   mov   [ebx+0x200], ecx
   mov   ecx, [Char_Table+eax+20]
   mov   [ebx+0x204], ecx
   mov   ecx, [Char_Table+eax+24]
   mov   [ebx+0x300], ecx
   mov   ecx, [Char_Table+eax+28]
   mov   [ebx+0x304], ecx
   mov   ecx, [Char_Table+eax+32]
   mov   [ebx+0x400], ecx
   mov   ecx, [Char_Table+eax+36]
   mov   [ebx+0x404], ecx
   mov   ecx, [Char_Table+eax+40]
   mov   [ebx+0x500], ecx
   mov   ecx, [Char_Table+eax+44]
   mov   [ebx+0x504], ecx
   mov   ecx, [Char_Table+eax+48]
   mov   [ebx+0x600], ecx
   mov   ecx, [Char_Table+eax+52]
   mov   [ebx+0x604], ecx
   mov   ecx, [Char_Table+eax+56]
   mov   [ebx+0x700], ecx
   mov   ecx, [Char_Table+eax+60]
   mov   [ebx+0x704], ecx

	pop	ebx
	pop	ecx
ret
;----------------------------------- // -----------------------------------



;###########################################################################
;#		void WaitVRetrace(void);
;#
;#
;###########################################################################
align 4
_WaitVRetrace:

   mov   edx, STATUS_ADDR

wait_end_of_started_vblank:
   
	in    al, dx
   test  al, 08h
   jnz	wait_end_of_started_vblank

wait_start_of_vblank:

   in    al, dx
   test  al, 08h
   jz   wait_start_of_vblank

ret
;----------------------------------- // -----------------------------------



;###########################################################################
;#		void WaitHRetrace(void);
;#
;#
;###########################################################################
align 4
_WaitHRetrace:

   mov   edx, STATUS_ADDR

wait_end_of_started_hblank:
   
	in    al, dx
   test  al, 01h
   jnz	wait_end_of_started_hblank

wait_start_of_hblank:

   in    al, dx
   test  al, 01h
   jz   wait_start_of_hblank

ret
;----------------------------------- // -----------------------------------



;###########################################################################
;#		void DrawScanline(void)
;#
;#	   This is the scanline based drawing routine. It'll display on the screen
;# data in the buffer pointed by '_G_Buf'. This buffer has PPU palette indexes,
;# so we need to convert to PC 8-bit mode colors. If others things must be
;# displayed on the screen, put that in the '_frame_buffer' buffer, where 0x00
;# means transparency.
;###########################################################################
align 4
_DrawScanline:
	pushad

   mov   fs, word [_VideoSel]          ; screen address selector

	mov	esi, [_video_base]
	mov	eax, [_current_scanline]
	shl	eax, 8								; esi = pointer to video buffer
	add	esi, eax

   mov   ebx, [_G_Buf]						; ebx = pointer to NES display buffer

	mov	ebp, _frame_buffer				; ebp = pointer to frame buffer 
	add	ebp, eax

	xor	edi, edi
GO_VIDEO:
      mov   eax, [ebx+edi*4]				; plot in chuncks of 4-byte (pixels)

		xor	ecx, ecx						

;*************************************
		mov	edx, eax							; transform to 80x86 8-bit color values
		shr	edx, 16

		mov	cl, dh
		mov	ch, [_Palette+ecx]

		and	edx, 0xFF
		mov	cl, [_Palette+edx]

		shl	ecx, 16

		mov	dl, ah
		mov	ch, [_Palette+edx]

		mov	dl, al
		mov	cl, [_Palette+edx]

		or		ecx, 0x40404040				; first palette entry is at 0x40
;*************************************

		mov	eax, [ds:ebp+edi*4]			; check frame_buffer
		cmp	eax, 0x00						; 0x00 means transparency
		je		SEND_TO_VIDEO
			cmp	al, 0x00
			je		NOT_THIS_PIXEL_1
				mov	cl, al
	
	NOT_THIS_PIXEL_1:
			cmp	ah, 0x00
			je		NOT_THIS_PIXEL_2
				mov	ch, ah

	NOT_THIS_PIXEL_2:
			shr	eax, 16
			ror	ecx, 16
			cmp	al, 0x00
			je		NOT_THIS_PIXEL_3
				mov	cl, al

	NOT_THIS_PIXEL_3:
			cmp	ah, 0x00
			je		NOT_THIS_PIXEL_4
				mov	ch, ah

	NOT_THIS_PIXEL_4:
			ror	ecx, 16


SEND_TO_VIDEO:
      mov   [fs:esi+edi*4], ecx

		inc	edi
		cmp	edi, 64
   jne   short GO_VIDEO

   popad
ret             
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


;***** Font Table (8x8)

align 4, db 0
Char_Table:
Number_0:
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,0,0,1,1,0,0
               db    1,1,0,0,1,1,0,0
               db    1,1,0,0,1,1,0,0
               db    1,1,0,0,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0

Number_1:
               db    0,0,0,0,1,1,0,0
               db    0,0,0,1,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    0,0,0,0,1,1,0,0

Number_2:
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,0,0,0,0,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0

Number_3:
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0

Number_4:
               db    1,1,0,0,1,1,0,0
               db    1,1,0,0,1,1,0,0
               db    1,1,0,0,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    0,0,0,0,1,1,0,0

Number_5:
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,0,0,0,0,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0

Number_6:
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,0,0,0,0,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,0,0,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0

Number_7:
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    0,0,0,0,1,1,0,0

Number_8:
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,0,0,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,0,0,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0

Number_9:
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,0,0,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    1,1,1,1,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    0,0,0,0,1,1,0,0
               db    0,0,0,0,1,1,0,0
;----------------------------------- // -----------------------------------
