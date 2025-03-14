;=============================================================================
;|
;|	 NES PPU module  (c) 2001  - by _aLe()
;|
;|
;|	 Assemble under NASM: nasm -f coff ppu.asm
;|
;|----------------------------------- // -----------------------------------
;|
;|  General Description
;|
;|		Provides emulation functions for the NES PPU.
;|
;|
;|----------------------------------- // -----------------------------------
;|
;|  Doubts/To be done
;|
;|
;|		to be done :)
;|
;|
;|----------------------------------- // -----------------------------------
;|
;|  History:
;|
;|
;|	22/01/2000:
;|		. implemented background render\display routine. The background was about
;|	125 FPS in games such POPEYE, MAPPY, CHACK'N POP, DK,... (in a Pentium 100Mhz)
;|
;|	23/01/2000:
;|		. implemented sprite render routine with:
;|			- 8x8 and 8x16 sprites support;
;|       - Vertical/horizontal (for both 8x8\8x16) flippings;
;|       - Background priority;
;|       (the FPS was about 95 FPS in the same games for background only)
;|
;|	26/01/2000:
;|		. implemented vertical\horizontal background scrolling (pixel-by-pixel method!)
;|
;| 27/01/2000:
;|		. made simultaneous vertical\horizontal scrolling and put all the background 
;|		rendering engine in one (I had two, one special for horizontal only scrolling)
;|
;| 28/01/2000:
;|		. added "Hit Flag". Now 'Brush Roller's title screen is correct.
;|
;| 29/01/2000:
;|		. I have fixed a bug in the "Hit Flag" implementation (I forgot that background 
;|		priority doesn't bother) and now ... Excite Bike runs perfectly!
;|
;| 01/02/2000:
;|		. fixed a bug in the simultaneous hor/vert scrolling (I forgot to check for 
;|		F0-FF range for vertical update).
;|
;| 03/02/2000:
;|		. added (fixed) the way a write to vertical scroll register in non-Vblank period 
;|		works. Now it is ignored for the rest of the screen refresh (but i think it must 
;|		be stored and used in the next non-VBl. period (if no write in the VBlank was 
;|		performed)). With this feature, scrolling in Sonson cart works perfectly.
;|
;|	04/02/2000:
;|		. fixed a bug in sprite priority (when two sprites are in the same position on 
;|		the screen).
;|		. added to sprite priority: 'if a sprite has the Sprite Priority bit set and has 
;|		a higher priority than a sprite at the same location, then the background will be 
;|		in front of both sprites.' With this, the sprite priority is 100% implemented!!!
;|
;| 21/02/2000:
;|		. now 'sprite #0 hit' is checking for bg transparency (what makes 'Renegade' status 
;|		bar be perfect!!!).
;|
;|	04/02/2001:
;|    . background and sprites are different procedures now.
;|		. changed the scrolling stuff to incorporate loopy's implementation of 2005/2006
;| magic.
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
	Global	_RenderBg
	Global	_RenderSprite

	Global	_FrameStart
   Global   _ScanlineStart
	Global	_ScanlineEnd
	Global   _ClearBg
	Global	_ClearSpr

;------------
; Data
;------------
   Global	_current_scanline

   Global   _PPU
   Global   _Ctrl_Reg_1, _Ctrl_Reg_2, _Status_Reg, _Spr_RAM_Ad
   Global   _Hor_Scr_Reg, _Vt_Scr_Reg, _VRAM_Ad_Reg, _SPR_RAM

	Global   _VROM
   Global   _pBank0x0000, _pBank0x0400, _pBank0x0800, _pBank0x0C00
   Global   _pBank0x1000, _pBank0x1400, _pBank0x1800, _pBank0x1C00
	Global	_VRAM
   Global   _pBank0x2000, _pBank0x2400, _pBank0x2800, _pBank0x2C00
	Global	_Palette
	
	Global	_PTBgBase, _PTSpBase, _SPR_Size, _Mirror_Type;

	Global	loop_v
	Global	loop_t
	Global	loop_x


;==================== END OF GLOBAL SYMBOLS DEFINITION =======================




;=============================================================================
; INTERFACE DATA
;=============================================================================
[section .data]

align 4
_PPU:
_Ctrl_Reg_1:   dd 0x00              ; 0
_Ctrl_Reg_2:   dd 0x00              ; 4
_Status_Reg:   dd 0x00              ; 8
_Spr_RAM_Ad:   dd 0x00              ; 12
_Hor_Scr_Reg:  dd 0x00              ; 16
_Vt_Scr_Reg:   dd 0x00              ; 20
_VRAM_Ad_Reg:  dd 0x00              ; 24
_SPR_RAM:      times 256 db 0       ; 28
_VROM:										; _VROM and _VRAM MUST be consecutives in memory
_pBank0x0000:	dd 0x00
_pBank0x0400:	dd 0x00
_pBank0x0800:	dd 0x00
_pBank0x0C00:	dd 0x00
_pBank0x1000:	dd 0x00
_pBank0x1400:	dd 0x00
_pBank0x1800:	dd 0x00
_pBank0x1C00:	dd 0x00
_VRAM:
_pBank0x2000:	dd 0x00
_pBank0x2400:	dd 0x00
_pBank0x2800:	dd 0x00
_pBank0x2C00:	dd 0x00
_Palette:		times 0x40 db 0x00
; END OF PPU STRUCTURE


_PTBgBase:     dd 0x00000000  ; logical address
_PTSpBase:     dd 0xFFFFFFFF  ; for sprites: bit 2: 0 = PT 1; 1 = PT 0
                              ; see module "GRAPH" to see how this is decoded
_SPR_Size:     dd 0x00000007  ; holds the sprite's size (07 or 15)
_Mirror_Type:  dd 0x00000000  ; for Hor = 0x01; Vert = 0x02

loop_v:		dd 0x00
loop_t:		dd 0x00
loop_x:		dd 0x00

_current_scanline:	dd 0x00


;----------------------------------- // -----------------------------------



;=============================================================================
; INCLUDES/REQUIRED EXTERNAL REFERENCES
;=============================================================================
%include "..\include\io.ha"

extern	_G_Buf

;----------------------------------- // -----------------------------------



;=============================================================================
; LOCAL DEFINITIONS/MACROS
;=============================================================================

;###########################################################################
;# Stores in EBP the appropriate Name Table address (80x86)
;###########################################################################
%macro Get_80x86_Address 0
	mov	ebx, [loop_v]
	mov	eax, ebx						; make a copy

	and	ebx, 0x0CFF					; get 0x0XX or 0x4XX or 0x8XX or 0xCXX

	shr	ebx, 8   					; 0 or 4 or 8 or C
	mov	ebp, [_VRAM+ebx]

	and	eax, 0x03FF
	mov	[NT_to_Use], ebp
	add	ebp, eax
	mov	[Tile_Num], eax

%endmacro
;----------------------------------- // -----------------------------------





;=============================================================================
; INTERFACE CODE
;=============================================================================
[section .text]

;###########################################################################
;#		void RenderBg(void)                         
;#
;#	Render NES background image into memory pointed by _G_Buf.
;#
;#
;# Note:
;#       The buffer has the NES palette indexes as its values, i.e, one value
;# between 0x00 -> 0x1F. It's converted to real 80x86 RGB colors in the
;# drawing routine 'DrawScanline'.
;###########################################################################
align 4
_RenderBg:
   pushad

	mov   esi, [_PTBgBase]			; get PPU Background Pattern Table Address 
											; (0x0000 or 0x1000)
	mov	eax, [loop_v]
	shr	eax, 12
   add   esi, eax						; add offset to the Pattern Table
											; (according with vertical position)

;
; ESI -> this won't change till the end of this function anymore!
;			holds PPU background Pattern Table address with its vertical offset
;

   mov   ebx, [_G_Buf]				; N_G_Buf -> pointer to buffer
	sub	ebx, [loop_x]				; note that the first and last 8-bytes are for garbage
   mov   [N_G_Buf], ebx				; stuff (used when it's necessary horizontal scrolling)

	mov	dword [how_many], 32		; number of tiles to render
	cmp	dword [loop_x], 0x00		
	je	NO_SCROLL						; if there is horizontal scrolling ...
		inc	dword [how_many]		; ... it'll be 33 tiles instead
		
NO_SCROLL:


; BACKGROUND MAIN LOOP
;*********************************************************
BG_RENDER:
   xor   edi, edi						; edi = loop counter: 0 - 31/32
	
	Get_80x86_Address

BG_LOOP:
;  First, we get the attribute byte:
   xor   eax, eax
   mov   ebx, dword [NT_to_Use]
   mov   edx, dword [Tile_Num]

   mov   al, byte [Attr_Table+edx+edi]		; al = Attribute Table index
   mov   al, byte [ebx+0x3C0+eax]			; al = attribute byte
   and   al, byte [Order_Table+edx+edi]	; al = attribute byte for the Tile ...
                                          ; ... that's being rendered

   mov   edx, [Expand_Table_3+eax*4]		; expand it in 8-byte into EDX/ECX
	mov	ecx, edx

   xor   ebx, ebx
   mov   bl, byte [ds:ebp+edi]
   shl   ebx, 4									; ebx = PT tile index

	add	ebx, esi									; add it to the saved PT address and select
	mov	eax, ebx									; the correct 80x86 address bank
   shr   eax, 0x0A
   and   ebx, 0x03FF
   add   ebx, [_VROM+eax*4]					; now ebx points to real PT data

; Takes the information from PT calculed above, and decodifies it.
   mov   al, byte [ebx]							; al = byte in PT (1st LAYER)
   or    edx, [Expand_Table_1+eax*8]		; we expand it in 8-byte (64 bits) ...
   or    ecx, [Expand_Table_1+4+eax*8]		; ... using EDX-ECX
   mov   al, byte [ebx+8]						; al = byte in PT (2nd LAYER)
   or    edx, [Expand_Table_2+eax*8]		; expand it too, and make one single
   or    ecx, [Expand_Table_2+4+eax*8]		; 8-byte value 

; Now, edx-ecx have the 8 bytes that are the tile's 8 pixels. Let's name these
; 8 bytes so:
;              P0 - P1 - P2 - P3 - P4 - P5 - P6 - P7
;
;  This is how they must be print in video. In the registers they're looking
; like this:
;                 EDX -> P3 - P2 - P1 - P0
;                 ECX -> P7 - P6 - P5 - P4 
;                        
;  They are inverted because the 80x86 use little-endian format of storage, so
; when we move them to memory (in chunks of 4-byte) they will be inverted!

;	So let's move them to the buffer...
   mov   ebx, [N_G_Buf]
   mov   [ebx+edi*8], edx
   mov   [ebx+edi*8+4], ecx


; check for Name Table horizontal crossing (scrolling)
	mov	ebx, [loop_v]
	and	ebx, 0x001F
	cmp	ebx, 0x001F
	jnz	DONT_WRAP							; if there's a crossing, made some adjusts

		and	dword [loop_v], 0xFFE0
		xor	dword [loop_v], 0x0400		; next Name Table	

		Get_80x86_Address						; recalculates 80x86 addresses

		sub	ebp, edi							; offsets adjusts
		sub	[Tile_Num], edi
		dec	ebp
		dec	dword [Tile_Num]

		jmp	GO_ON

DONT_WRAP:
	inc	dword [loop_v]						; increment loopy's register

GO_ON:
   inc   edi									; did we render all the scanline ?
	cmp	edi, [how_many]
 jnz   near BG_LOOP
;*********************************************************

; Just check for background clipping
   test  dword [_Ctrl_Reg_2], 0x02
	jnz END_BGRENDER
		mov	ebx, [_G_Buf]
      mov   eax, 0x20202020               ; left side bg clipping
      mov   dword [ebx], eax
		mov   dword [ebx+4], eax

END_BGRENDER:
	popad
ret
;----------------------------------- // -----------------------------------



;###########################################################################
;#		void RenderSprite(void)
;#
;#	Render NES sprites into memory pointed by _G_Buf.
;#
;#
;# Note:
;#       The buffer has the NES palette indexes as its values, i.e, one value
;# between 0x00 -> 0x1F. It's converted to real 80x86 RGB colors in the
;# drawing routine 'DrawScanline'.
;#			A sprite buffer is used to keep track of priority stuff.
;###########################################################################
align 4
_RenderSprite:
	pushad

   xor   edi, edi									; loop counter: 0 -> 63
SPR_LOOP:

   mov   ebx, dword [_SPR_Size]           ; ebx = 7 or 15 (sprite size)
   mov   ecx, dword [_current_scanline]	; eax = scanline  (0-240)
   dec   ecx
                  ; Background 'N'th line <=> Sprite 'N-1'th line

; For checking if we'll render the sprite, we use:
;     if (Lin_Atual-Lin_Sprite <= Spr_Size) then Imprime_Sprite
;        
; OBS: Lin_Atual-Lin_Sprite is an unsigned value. This means that if we
; subtract 2 of 1 (1 - 2), it will result in 0xFFFFFFFF (not -1)
; The registers values are as follows:
;  ecx = Lin_Atual;              (0 -> 239 if not displaying the 1st line)
;  eax = Lin_Sprite;             (holded in Sprite RAM)
;  ebx = Spr_Size                (07 if 8x8, 15 if 8x16)

   xor   eax, eax
   mov   al, byte [_SPR_RAM+edi*4]
   sub   ecx, eax                      ; now ecx = offset (0-7/0-15)
   cmp   ecx, ebx
   ja    near END_SPRITE               ; go take another sprite

;
;  Here we have:  ecx = Offset,  eax = Lin_Sprite,  ebx = Spr_Size
;
;  OK, we'll render the sprite. First we calculate the PT address for this
; sprite
;
;  Because there are different PT address for different sprite size, it was
; created a table for holding this.
;  The entry (index) for this table (Spr_PT) is as follows:
;
;  eax = index (8 bits, total of 256 differents entrys on the table)
;
;  al = 8 bits    :        bit 0:      0 = sprite's NT index is even
;                                      1 = sprite's NT index is odd
;
;                          bit 2:      0 = Pattern Table is 0x1000
;                                      1 = Pattern Table is 0x0000
;
;                          bit 3:      0 = sprites are 8x8
;                                      1 = sprites are 8x16
;
;                       bits 7-4:      MUST BE ALL 0's (else the entry on
;                                      the table will be corrupted!!)
;  The others bits are irrelevant.
;
;  Sprites 8x16 don't use the address stated in Ctr_Reg_1 address. To find out
; the base in this case we need to know if the Tile index of the 8x16 sprite
; is even or odd. If even, PT = 0x0000; if odd, PT = 0x0FF0 (0x1000-0x0010).
;  So, bit 0 is irrelevant if we'll render a 8x8 sprite, and bit 2 is
; irrelevant if we'll render a 8x16 sprite!
;
; _PTSpBase = 0xFFFFFFFF if PT is #0, or 0xFFFFFFFB if PT is #1. This is
; this way because the encoding method below.
;
	mov   al, [_SPR_RAM+1+edi*4]     ; take sprite's Tile index
   or    al, 00001100b              ; just set bits 2 and 3
   and   eax, ebx                   ; now we set correctly bit 3
                                    ; this will clear bits 7-4
   and   eax, dword [_PTSpBase]     ; now time for bit 2
   mov   esi, [Spr_PT+eax*4]        ; esi = logic address


;  Now we need to know if we have VERTICAL flipping.
;
;  Again, we'll use a table to avoid much calculus or jumps instructions.
; This table is "Adj_V_Flip", and it has what it says: Adjusts values for
; vertical flip.
;
;  eax (al) contains the index for the table:
;
;  al = 8 bits    :           bit 7:   1 = vertical flip
;                                      0 = no vertical flip
;
;                          bits 0-4:   a base inside the table (7/15)
;                                    +
;                                      a offset inside the base (0-7/0-15)
;
;  The "offset inside the base" depends upon the "base inside the table". If
; it is 7, then offset = 0-7 (sprites 8x8); else if it is 15, then offset
; is 0-15 (which indicate us the sprite is 8x16)
;
	mov   al, byte [_SPR_RAM+2+edi*4]   ; take sprite's attribute
   and   eax, 10000000b                ; mask bit 7 (indicate the flip)
   or    eax, ebx                      ; base inside the table
   add   eax, ecx                      ; offset inside the base
   add   esi, [Adj_V_Flip+eax*4]       ; PPU PT address + offset found!

; Now let's take the bytes from PT selected to render it!
   xor   ebx, ebx
   mov   bl, byte [_SPR_RAM+1+edi*4]      ; bl = Tile (indice)
   shl   ebx, 4                           

   add   esi, ebx                         ; find 80x86 PT address to get the data
   mov   eax, esi
   shr   eax, 0x0A
   and   esi, 0x03FF
   add   esi, [_VROM+eax*4]               

   mov   al, byte [esi]                   ; PT layer #0
   mov   edx, [Expand_Table_1+eax*8]      ; expand it the same way we did
   mov   ecx, [Expand_Table_1+eax*8+4]    ; in background rendering
   mov   al, byte [esi+8]                 ; PT layer #1
   or    edx, [Expand_Table_2+eax*8]      ; expand it too
   or    ecx, [Expand_Table_2+eax*8+4]


; Till here we got our 8-byte pixels in EDX-ECX this way:
;                 EDX -> P3 - P2 - P1 - P0 
;                 ECX -> P7 - P6 - P5 - P4
; Though we won't save them in chuncks of 8-byte, they are still inverted
; because I didn't want to spend more 4096 bytes in such expands tables!
;

; Now we check for Horizontal Flipping
   mov   al, byte [_SPR_RAM+2+edi*4]      ; al = Sprite's attribute
   test  al, 01000000b							; does it have horiz. flipping ?
   jz    near DO_NOT_FLIP_HOR
      bswap edx                  ; 486+ instruction used !!!   
      bswap ecx                  ; we invert the pixels (bytes) stored in
      xchg  ecx, edx             ; EDX-ECX registers

DO_NOT_FLIP_HOR:
   or    ecx, [Expand_Table_4+eax*4]      ; get the 2 upper bits for
   or    edx, [Expand_Table_4+eax*4]      ; palett index

   mov   al, [_SPR_RAM+3+edi*4]           ; take X coordenate

	mov	ebx, SprBuf								; pointer to sprite buffer
	add	ebx, eax

	mov	esi, [_G_Buf]							; pointer to bg buffer
	add	esi, eax


; now we move the bytes to the bg buffer verifying certain conditions:
;  1) Sprite clipping
;  2) Transparent pixels are not rendered
;  3) Background priority (the higher sprite priority is for the first sprite
;     starting from position 0 in Sprite RAM)

;
; DOES CLIPPING BOTHER FOR "HIT FLAG" IMPLEMENTATION ???
;

; till here:
;                 eax = sprite's X coordenate
;                 ebx = sprite buffer (priority stuff)
;                 esi = display buffer

; 1) Sprite clipping
;*****
	test  dword [_Ctrl_Reg_2], 0x04        ; does we have sprite clipping ?
   jnz   NO_SPR_CLIP
		and   edx, [Spr_Clip_Table+eax*8]
      and   ecx, [Spr_Clip_Table+eax*8+4]

NO_SPR_CLIP:
; Set some variables we need to check
	mov   al, byte [_SPR_RAM+2+edi*4]      ; al = sprite attribute byte
   mov   ah, [Hit_Flag_Table+edi]         ; ah = 'hit flag' information


; Now we check 2) and 3) for each pixel
;******************** Pixel #0
	test  edx, 0xE3					; 2) Transparent pixels are not rendered
   jz    DONT_RENDER_PIXEL_0

   test  byte [esi], 0x03           ; background is transparent ?
   jz    RENDER_PIXEL_0             ; if so, we always draw the sprite

   test  byte [ebx], 0x03           ; If we already have a sprite draw(ed)
   jnz   DONT_RENDER_PIXEL_0        ; (even if it has bg priority set), we
                                       ; don't touch it.

   or    [_Status_Reg], ah          ; check for 'Hit Flag'
                                       ; (See that it checks bg transparency!)

   test  eax, 0x20               ; 3) Background priority
   jnz   SPR_NOT_TRANSP_0           ; Bg is non-transparent and has prior.

RENDER_PIXEL_0:
   mov   byte [esi], dl
;*******************
SPR_NOT_TRANSP_0:                      ; the pixel is _NOT_ transparent, so
   mov   byte [ebx], dl             ; we save the information


DONT_RENDER_PIXEL_0:
;******************* Pixel #1
   test  edx, 0xE300
   jz    DONT_RENDER_PIXEL_1
   test  dword [esi], 0x0300
   jz    RENDER_PIXEL_1
   test  dword [ebx], 0x0300
   jnz   DONT_RENDER_PIXEL_1
   or    [_Status_Reg], ah
   test  eax, 0x20
   jnz   SPR_NOT_TRANSP_1

RENDER_PIXEL_1:
   mov   byte [esi+1], dh
;*******************

SPR_NOT_TRANSP_1:
   mov   byte [ebx+1], dh

DONT_RENDER_PIXEL_1:
;******************* Pixel #2
	shr   edx, 16
   test  edx, 0xE3
   jz    DONT_RENDER_PIXEL_2
   test  dword [esi], 0x030000
   jz    RENDER_PIXEL_2
   test  dword [ebx], 0x030000
   jnz   DONT_RENDER_PIXEL_2
   or    [_Status_Reg], ah
   test  eax, 0x20
   jnz   SPR_NOT_TRANSP_2

RENDER_PIXEL_2:
   mov   byte [esi+2], dl
;*******************

SPR_NOT_TRANSP_2:
   mov   byte [ebx+2], dl

DONT_RENDER_PIXEL_2:
;******************* Pixel #3
   test  edx, 0xE300
   jz    DONT_RENDER_PIXEL_3
   test  dword [esi], 0x03000000
   jz    RENDER_PIXEL_3
   test  dword [ebx], 0x03000000
   jnz   DONT_RENDER_PIXEL_3
   or    [_Status_Reg], ah
   test  eax, 0x20
   jnz   SPR_NOT_TRANSP_3

RENDER_PIXEL_3:
   mov   byte [esi+3], dh
;*******************

SPR_NOT_TRANSP_3:
   mov   byte [ebx+3], dh

DONT_RENDER_PIXEL_3:
;******************* Pixel #4
   test  ecx, 0xE3
   jz    DONT_RENDER_PIXEL_4
   test  byte [esi+4], 0x03
   jz    RENDER_PIXEL_4
   test  byte [ebx+4], 0x03
   jnz   DONT_RENDER_PIXEL_4
   or    [_Status_Reg], ah
   test  eax, 0x20
   jnz   SPR_NOT_TRANSP_4

RENDER_PIXEL_4:
   mov   byte [esi+4], cl
;*******************

SPR_NOT_TRANSP_4:
   mov   byte [ebx+4], cl

DONT_RENDER_PIXEL_4:
;******************* Pixel #5
   test  ecx, 0xE300
   jz    DONT_RENDER_PIXEL_5
   test  byte [esi+5], 0x03
   jz    RENDER_PIXEL_5
   test  byte [ebx+5], 0x03
   jnz   DONT_RENDER_PIXEL_5
   or    [_Status_Reg], ah
   test  eax, 0x20
   jnz   SPR_NOT_TRANSP_5

RENDER_PIXEL_5:
   mov   byte [esi+5], ch
;*******************

SPR_NOT_TRANSP_5:
   mov   byte [ebx+5], ch

DONT_RENDER_PIXEL_5:
;******************* Pixel #6
   shr   ecx, 16
   test  ecx, 0xE3
   jz    DONT_RENDER_PIXEL_6
   test  byte [esi+6], 0x03
   jz    RENDER_PIXEL_6
   test  byte [ebx+6], 0x03
   jnz   DONT_RENDER_PIXEL_6
   or    [_Status_Reg], ah
   test  eax, 0x20
   jnz   SPR_NOT_TRANSP_6

RENDER_PIXEL_6:
   mov   byte [esi+6], cl
;*******************

SPR_NOT_TRANSP_6
   mov   byte [ebx+6], cl

DONT_RENDER_PIXEL_6:
;******************* Pixel #7
   test  ecx, 0xE300
   jz    DONT_RENDER_PIXEL_7
   test  byte [esi+7], 0x03
   jz    RENDER_PIXEL_7
   test  byte [ebx+7], 0x03
   jnz   DONT_RENDER_PIXEL_7
   or    [_Status_Reg], ah
   test  eax, 0x20
   jnz   SPR_NOT_TRANSP_7

RENDER_PIXEL_7:
   mov   byte [esi+7], ch
;*******************

SPR_NOT_TRANSP_7:
   mov   byte [ebx+7], ch

DONT_RENDER_PIXEL_7:


END_SPRITE:
   inc   edi
   cmp   edi, 64
 jnz   near SPR_LOOP
;*********************************************************

	popad
ret
;----------------------------------- // -----------------------------------






;###########################################################################
;#		void ClearBg(void)
;#
;#   Clears background buffer.
;###########################################################################
align 4
_ClearBg:
	push	edi
	push	ecx
	push	ebx

	mov	ebx, [_G_Buf]
   mov   edi, 31
	mov	ecx, 0x20202020		; color index 0x20 is the 80x86 color transparency

align 4
.CLEAR_BG_BUFFER:
		mov   [ebx+edi*8], ecx
		mov   [ebx+edi*8+4], ecx
		dec   edi
   jns   short .CLEAR_BG_BUFFER

	pop	ebx
	pop	ecx
	pop	edi
ret
;----------------------------------- // -----------------------------------



;###########################################################################
;#		void ClearSpr(void)
;#
;#   Clears sprite buffer.
;###########################################################################
_ClearSpr:
	push	ecx
	push	ebx

   mov   ebx, 31
   xor   ecx, ecx

align 4
.CLEAR_SPR_BUFFER:
     mov   [SprBuf+ebx*8], ecx
     mov   [SprBuf+ebx*8+4], ecx
     dec   ebx
   jns   short .CLEAR_SPR_BUFFER

	pop	ebx
	pop	ecx
ret
;----------------------------------- // -----------------------------------



;###########################################################################
;#		void FrameStart(void)
;#
;#   Loopy's frame start assigment.
;###########################################################################
align 4
;frame start (line 0) (if background and sprites are enabled):
;        v=t
_FrameStart:
	push	eax

	mov	eax, [loop_t]
	mov	[loop_v], eax

	pop	eax
ret
;----------------------------------- // -----------------------------------



;###########################################################################
;#		void ScanlineStart(void)
;#
;#   Loopy's scanline start assigment.
;###########################################################################
align 4
;;;scanline start (if background and sprites are enabled):
;;;        v:0000 0100 0001 1111=t:0000 0100 0001 1111
_ScanlineStart:
	push	eax

	and	dword [loop_v], 0xFBE0
	mov	eax, [loop_t]
	and	eax, 0x041F
	or		dword [loop_v], eax

	pop	eax
ret
;----------------------------------- // -----------------------------------



;###########################################################################
;#		void ScanlineEnd(void)
;#
;#   Scanline adjusts before start another one (vertical adjusts).
;###########################################################################
align 4
;   You can think of bits 5,6,7,8,9 as the "y scroll"(*8). this functions
; slightly different from the X.  it wraps to 0 and bit 11 is switched when
; it's incremented from _29_ instead of 31.  there are some odd side effects
; from this.. if you manually set the value above 29 (from either 2005 or
; 2006), the wrapping from 29 obviously won't happen, and attrib data will be
; used as name table data.  the "y scroll" still wraps to 0 from 31, but
; without switching bit 11.  this explains why writing 240+ to 'Y' in 2005
; appeared as a negative scroll value.
_ScanlineEnd:
	pushad

	mov	eax, [loop_v]
	mov	ebx, eax

	and	eax, 0x7000
	cmp	eax, 0x7000
	jne	DONT_WRAP_Y_OFFSET

		and	dword [loop_v], 0x8FFF
		and	ebx, 0x03E0
		cmp	ebx, 0x03A0
		jne	DONT_WRAP_29
			xor	dword [loop_v], 0x0800
			and	dword [loop_v], 0xFC1F
			jmp	END_THIS
			
DONT_WRAP_29:
		cmp	ebx, 0x03E0
		jne	DONT_WRAP_31
			and	dword [loop_v], 0xFC1F
			jmp	END_THIS


DONT_WRAP_31:
		add	dword [loop_v], 0x0020
		jmp	END_THIS


DONT_WRAP_Y_OFFSET:
	add	dword [loop_v], 0x1000


END_THIS:	

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

align 4
SprBuf:		times 256+8		db 0x00			; sprite buffer


align 4
Tile_Num:                  ; holds the INITIAL number of the Tile inside
            dd 0x00			; the Name Table (for the scanline that's is
                           ; being rendered)

NT_to_Use:                 ; holds the fisical NT base WITHOUT the
            dd 0x00        ; offset (eg. FISICAL+0x2000/0x2400...)

N_G_Buf:                   ; pointers to video buffer (this was created
            dd 0x00        ; because when we do horizontal scrolling
                           ; pixel-by-pixel, may exist some pixels that
                           ; must not be printed. So we move them to
                           ; buffer's places where the display routine
                           ; do not print them.)

how_many:	dd 0x00			; number of tiles to be rendered in the scanline (32 or 33)



;*********************************************************
; The index for this table is a 8-bit value took from the
; PT selected (1st layer). Each entry on this table is
; expanded into 32-bit value. The even entries are for high
; nibble of the index, and the subsequent is for a low
; nibble entry.
; OBS: This is stored in inverted order (because little-endian)
;

align 4, db 0        ; 64 x 8 dwords      (2048 bytes)
Expand_Table_1:
            dd 0x00000000, 0x00000000, 0x00000000, 0x01000000, 0x00000000, 0x00010000, 0x00000000, 0x01010000
            dd 0x00000000, 0x00000100, 0x00000000, 0x01000100, 0x00000000, 0x00010100, 0x00000000, 0x01010100 
            dd 0x00000000, 0x00000001, 0x00000000, 0x01000001, 0x00000000, 0x00010001, 0x00000000, 0x01010001 
            dd 0x00000000, 0x00000101, 0x00000000, 0x01000101, 0x00000000, 0x00010101, 0x00000000, 0x01010101 
            dd 0x01000000, 0x00000000, 0x01000000, 0x01000000, 0x01000000, 0x00010000, 0x01000000, 0x01010000 
            dd 0x01000000, 0x00000100, 0x01000000, 0x01000100, 0x01000000, 0x00010100, 0x01000000, 0x01010100 
            dd 0x01000000, 0x00000001, 0x01000000, 0x01000001, 0x01000000, 0x00010001, 0x01000000, 0x01010001 
            dd 0x01000000, 0x00000101, 0x01000000, 0x01000101, 0x01000000, 0x00010101, 0x01000000, 0x01010101 
            dd 0x00010000, 0x00000000, 0x00010000, 0x01000000, 0x00010000, 0x00010000, 0x00010000, 0x01010000 
            dd 0x00010000, 0x00000100, 0x00010000, 0x01000100, 0x00010000, 0x00010100, 0x00010000, 0x01010100 
            dd 0x00010000, 0x00000001, 0x00010000, 0x01000001, 0x00010000, 0x00010001, 0x00010000, 0x01010001 
            dd 0x00010000, 0x00000101, 0x00010000, 0x01000101, 0x00010000, 0x00010101, 0x00010000, 0x01010101 
            dd 0x01010000, 0x00000000, 0x01010000, 0x01000000, 0x01010000, 0x00010000, 0x01010000, 0x01010000 
            dd 0x01010000, 0x00000100, 0x01010000, 0x01000100, 0x01010000, 0x00010100, 0x01010000, 0x01010100 
            dd 0x01010000, 0x00000001, 0x01010000, 0x01000001, 0x01010000, 0x00010001, 0x01010000, 0x01010001 
            dd 0x01010000, 0x00000101, 0x01010000, 0x01000101, 0x01010000, 0x00010101, 0x01010000, 0x01010101 
            dd 0x00000100, 0x00000000, 0x00000100, 0x01000000, 0x00000100, 0x00010000, 0x00000100, 0x01010000 
            dd 0x00000100, 0x00000100, 0x00000100, 0x01000100, 0x00000100, 0x00010100, 0x00000100, 0x01010100 
            dd 0x00000100, 0x00000001, 0x00000100, 0x01000001, 0x00000100, 0x00010001, 0x00000100, 0x01010001 
            dd 0x00000100, 0x00000101, 0x00000100, 0x01000101, 0x00000100, 0x00010101, 0x00000100, 0x01010101 
            dd 0x01000100, 0x00000000, 0x01000100, 0x01000000, 0x01000100, 0x00010000, 0x01000100, 0x01010000 
            dd 0x01000100, 0x00000100, 0x01000100, 0x01000100, 0x01000100, 0x00010100, 0x01000100, 0x01010100 
            dd 0x01000100, 0x00000001, 0x01000100, 0x01000001, 0x01000100, 0x00010001, 0x01000100, 0x01010001 
            dd 0x01000100, 0x00000101, 0x01000100, 0x01000101, 0x01000100, 0x00010101, 0x01000100, 0x01010101 
            dd 0x00010100, 0x00000000, 0x00010100, 0x01000000, 0x00010100, 0x00010000, 0x00010100, 0x01010000 
            dd 0x00010100, 0x00000100, 0x00010100, 0x01000100, 0x00010100, 0x00010100, 0x00010100, 0x01010100 
            dd 0x00010100, 0x00000001, 0x00010100, 0x01000001, 0x00010100, 0x00010001, 0x00010100, 0x01010001 
            dd 0x00010100, 0x00000101, 0x00010100, 0x01000101, 0x00010100, 0x00010101, 0x00010100, 0x01010101 
            dd 0x01010100, 0x00000000, 0x01010100, 0x01000000, 0x01010100, 0x00010000, 0x01010100, 0x01010000 
            dd 0x01010100, 0x00000100, 0x01010100, 0x01000100, 0x01010100, 0x00010100, 0x01010100, 0x01010100 
            dd 0x01010100, 0x00000001, 0x01010100, 0x01000001, 0x01010100, 0x00010001, 0x01010100, 0x01010001 
            dd 0x01010100, 0x00000101, 0x01010100, 0x01000101, 0x01010100, 0x00010101, 0x01010100, 0x01010101 
            dd 0x00000001, 0x00000000, 0x00000001, 0x01000000, 0x00000001, 0x00010000, 0x00000001, 0x01010000 
            dd 0x00000001, 0x00000100, 0x00000001, 0x01000100, 0x00000001, 0x00010100, 0x00000001, 0x01010100 
            dd 0x00000001, 0x00000001, 0x00000001, 0x01000001, 0x00000001, 0x00010001, 0x00000001, 0x01010001 
            dd 0x00000001, 0x00000101, 0x00000001, 0x01000101, 0x00000001, 0x00010101, 0x00000001, 0x01010101 
            dd 0x01000001, 0x00000000, 0x01000001, 0x01000000, 0x01000001, 0x00010000, 0x01000001, 0x01010000 
            dd 0x01000001, 0x00000100, 0x01000001, 0x01000100, 0x01000001, 0x00010100, 0x01000001, 0x01010100 
            dd 0x01000001, 0x00000001, 0x01000001, 0x01000001, 0x01000001, 0x00010001, 0x01000001, 0x01010001 
            dd 0x01000001, 0x00000101, 0x01000001, 0x01000101, 0x01000001, 0x00010101, 0x01000001, 0x01010101 
            dd 0x00010001, 0x00000000, 0x00010001, 0x01000000, 0x00010001, 0x00010000, 0x00010001, 0x01010000 
            dd 0x00010001, 0x00000100, 0x00010001, 0x01000100, 0x00010001, 0x00010100, 0x00010001, 0x01010100 
            dd 0x00010001, 0x00000001, 0x00010001, 0x01000001, 0x00010001, 0x00010001, 0x00010001, 0x01010001 
            dd 0x00010001, 0x00000101, 0x00010001, 0x01000101, 0x00010001, 0x00010101, 0x00010001, 0x01010101 
            dd 0x01010001, 0x00000000, 0x01010001, 0x01000000, 0x01010001, 0x00010000, 0x01010001, 0x01010000 
            dd 0x01010001, 0x00000100, 0x01010001, 0x01000100, 0x01010001, 0x00010100, 0x01010001, 0x01010100 
            dd 0x01010001, 0x00000001, 0x01010001, 0x01000001, 0x01010001, 0x00010001, 0x01010001, 0x01010001 
            dd 0x01010001, 0x00000101, 0x01010001, 0x01000101, 0x01010001, 0x00010101, 0x01010001, 0x01010101 
            dd 0x00000101, 0x00000000, 0x00000101, 0x01000000, 0x00000101, 0x00010000, 0x00000101, 0x01010000 
            dd 0x00000101, 0x00000100, 0x00000101, 0x01000100, 0x00000101, 0x00010100, 0x00000101, 0x01010100 
            dd 0x00000101, 0x00000001, 0x00000101, 0x01000001, 0x00000101, 0x00010001, 0x00000101, 0x01010001 
            dd 0x00000101, 0x00000101, 0x00000101, 0x01000101, 0x00000101, 0x00010101, 0x00000101, 0x01010101 
            dd 0x01000101, 0x00000000, 0x01000101, 0x01000000, 0x01000101, 0x00010000, 0x01000101, 0x01010000 
            dd 0x01000101, 0x00000100, 0x01000101, 0x01000100, 0x01000101, 0x00010100, 0x01000101, 0x01010100 
            dd 0x01000101, 0x00000001, 0x01000101, 0x01000001, 0x01000101, 0x00010001, 0x01000101, 0x01010001 
            dd 0x01000101, 0x00000101, 0x01000101, 0x01000101, 0x01000101, 0x00010101, 0x01000101, 0x01010101 
            dd 0x00010101, 0x00000000, 0x00010101, 0x01000000, 0x00010101, 0x00010000, 0x00010101, 0x01010000 
            dd 0x00010101, 0x00000100, 0x00010101, 0x01000100, 0x00010101, 0x00010100, 0x00010101, 0x01010100 
            dd 0x00010101, 0x00000001, 0x00010101, 0x01000001, 0x00010101, 0x00010001, 0x00010101, 0x01010001 
            dd 0x00010101, 0x00000101, 0x00010101, 0x01000101, 0x00010101, 0x00010101, 0x00010101, 0x01010101 
            dd 0x01010101, 0x00000000, 0x01010101, 0x01000000, 0x01010101, 0x00010000, 0x01010101, 0x01010000 
            dd 0x01010101, 0x00000100, 0x01010101, 0x01000100, 0x01010101, 0x00010100, 0x01010101, 0x01010100 
            dd 0x01010101, 0x00000001, 0x01010101, 0x01000001, 0x01010101, 0x00010001, 0x01010101, 0x01010001 
            dd 0x01010101, 0x00000101, 0x01010101, 0x01000101, 0x01010101, 0x00010101, 0x01010101, 0x01010101 
;*********************************************************


;*********************************************************
; The index for this table is a 8-bit value took from the
; PT selected (2st layer). Each entry on this table is
; expanded into 32-bit value. The even entries are for high
; nibble of the index, and the subsequent is for a low
; nibble entry.
; OBS: This is stored in inverted order (because little-endian)
;

align 4, db 0        ; 64 x 8 dwords      (2048 bytes)
Expand_Table_2:      
            dd 0x00000000, 0x00000000, 0x00000000, 0x02000000, 0x00000000, 0x00020000, 0x00000000, 0x02020000
            dd 0x00000000, 0x00000200, 0x00000000, 0x02000200, 0x00000000, 0x00020200, 0x00000000, 0x02020200 
            dd 0x00000000, 0x00000002, 0x00000000, 0x02000002, 0x00000000, 0x00020002, 0x00000000, 0x02020002 
            dd 0x00000000, 0x00000202, 0x00000000, 0x02000202, 0x00000000, 0x00020202, 0x00000000, 0x02020202 
            dd 0x02000000, 0x00000000, 0x02000000, 0x02000000, 0x02000000, 0x00020000, 0x02000000, 0x02020000 
            dd 0x02000000, 0x00000200, 0x02000000, 0x02000200, 0x02000000, 0x00020200, 0x02000000, 0x02020200 
            dd 0x02000000, 0x00000002, 0x02000000, 0x02000002, 0x02000000, 0x00020002, 0x02000000, 0x02020002 
            dd 0x02000000, 0x00000202, 0x02000000, 0x02000202, 0x02000000, 0x00020202, 0x02000000, 0x02020202 
            dd 0x00020000, 0x00000000, 0x00020000, 0x02000000, 0x00020000, 0x00020000, 0x00020000, 0x02020000 
            dd 0x00020000, 0x00000200, 0x00020000, 0x02000200, 0x00020000, 0x00020200, 0x00020000, 0x02020200 
            dd 0x00020000, 0x00000002, 0x00020000, 0x02000002, 0x00020000, 0x00020002, 0x00020000, 0x02020002 
            dd 0x00020000, 0x00000202, 0x00020000, 0x02000202, 0x00020000, 0x00020202, 0x00020000, 0x02020202 
            dd 0x02020000, 0x00000000, 0x02020000, 0x02000000, 0x02020000, 0x00020000, 0x02020000, 0x02020000 
            dd 0x02020000, 0x00000200, 0x02020000, 0x02000200, 0x02020000, 0x00020200, 0x02020000, 0x02020200 
            dd 0x02020000, 0x00000002, 0x02020000, 0x02000002, 0x02020000, 0x00020002, 0x02020000, 0x02020002 
            dd 0x02020000, 0x00000202, 0x02020000, 0x02000202, 0x02020000, 0x00020202, 0x02020000, 0x02020202 
            dd 0x00000200, 0x00000000, 0x00000200, 0x02000000, 0x00000200, 0x00020000, 0x00000200, 0x02020000 
            dd 0x00000200, 0x00000200, 0x00000200, 0x02000200, 0x00000200, 0x00020200, 0x00000200, 0x02020200 
            dd 0x00000200, 0x00000002, 0x00000200, 0x02000002, 0x00000200, 0x00020002, 0x00000200, 0x02020002 
            dd 0x00000200, 0x00000202, 0x00000200, 0x02000202, 0x00000200, 0x00020202, 0x00000200, 0x02020202 
            dd 0x02000200, 0x00000000, 0x02000200, 0x02000000, 0x02000200, 0x00020000, 0x02000200, 0x02020000 
            dd 0x02000200, 0x00000200, 0x02000200, 0x02000200, 0x02000200, 0x00020200, 0x02000200, 0x02020200 
            dd 0x02000200, 0x00000002, 0x02000200, 0x02000002, 0x02000200, 0x00020002, 0x02000200, 0x02020002 
            dd 0x02000200, 0x00000202, 0x02000200, 0x02000202, 0x02000200, 0x00020202, 0x02000200, 0x02020202 
            dd 0x00020200, 0x00000000, 0x00020200, 0x02000000, 0x00020200, 0x00020000, 0x00020200, 0x02020000 
            dd 0x00020200, 0x00000200, 0x00020200, 0x02000200, 0x00020200, 0x00020200, 0x00020200, 0x02020200 
            dd 0x00020200, 0x00000002, 0x00020200, 0x02000002, 0x00020200, 0x00020002, 0x00020200, 0x02020002 
            dd 0x00020200, 0x00000202, 0x00020200, 0x02000202, 0x00020200, 0x00020202, 0x00020200, 0x02020202 
            dd 0x02020200, 0x00000000, 0x02020200, 0x02000000, 0x02020200, 0x00020000, 0x02020200, 0x02020000 
            dd 0x02020200, 0x00000200, 0x02020200, 0x02000200, 0x02020200, 0x00020200, 0x02020200, 0x02020200
            dd 0x02020200, 0x00000002, 0x02020200, 0x02000002, 0x02020200, 0x00020002, 0x02020200, 0x02020002 
            dd 0x02020200, 0x00000202, 0x02020200, 0x02000202, 0x02020200, 0x00020202, 0x02020200, 0x02020202 
            dd 0x00000002, 0x00000000, 0x00000002, 0x02000000, 0x00000002, 0x00020000, 0x00000002, 0x02020000 
            dd 0x00000002, 0x00000200, 0x00000002, 0x02000200, 0x00000002, 0x00020200, 0x00000002, 0x02020200 
            dd 0x00000002, 0x00000002, 0x00000002, 0x02000002, 0x00000002, 0x00020002, 0x00000002, 0x02020002 
            dd 0x00000002, 0x00000202, 0x00000002, 0x02000202, 0x00000002, 0x00020202, 0x00000002, 0x02020202 
            dd 0x02000002, 0x00000000, 0x02000002, 0x02000000, 0x02000002, 0x00020000, 0x02000002, 0x02020000 
            dd 0x02000002, 0x00000200, 0x02000002, 0x02000200, 0x02000002, 0x00020200, 0x02000002, 0x02020200 
            dd 0x02000002, 0x00000002, 0x02000002, 0x02000002, 0x02000002, 0x00020002, 0x02000002, 0x02020002 
            dd 0x02000002, 0x00000202, 0x02000002, 0x02000202, 0x02000002, 0x00020202, 0x02000002, 0x02020202 
            dd 0x00020002, 0x00000000, 0x00020002, 0x02000000, 0x00020002, 0x00020000, 0x00020002, 0x02020000 
            dd 0x00020002, 0x00000200, 0x00020002, 0x02000200, 0x00020002, 0x00020200, 0x00020002, 0x02020200 
            dd 0x00020002, 0x00000002, 0x00020002, 0x02000002, 0x00020002, 0x00020002, 0x00020002, 0x02020002 
            dd 0x00020002, 0x00000202, 0x00020002, 0x02000202, 0x00020002, 0x00020202, 0x00020002, 0x02020202 
            dd 0x02020002, 0x00000000, 0x02020002, 0x02000000, 0x02020002, 0x00020000, 0x02020002, 0x02020000 
            dd 0x02020002, 0x00000200, 0x02020002, 0x02000200, 0x02020002, 0x00020200, 0x02020002, 0x02020200 
            dd 0x02020002, 0x00000002, 0x02020002, 0x02000002, 0x02020002, 0x00020002, 0x02020002, 0x02020002 
            dd 0x02020002, 0x00000202, 0x02020002, 0x02000202, 0x02020002, 0x00020202, 0x02020002, 0x02020202 
            dd 0x00000202, 0x00000000, 0x00000202, 0x02000000, 0x00000202, 0x00020000, 0x00000202, 0x02020000 
            dd 0x00000202, 0x00000200, 0x00000202, 0x02000200, 0x00000202, 0x00020200, 0x00000202, 0x02020200 
            dd 0x00000202, 0x00000002, 0x00000202, 0x02000002, 0x00000202, 0x00020002, 0x00000202, 0x02020002 
            dd 0x00000202, 0x00000202, 0x00000202, 0x02000202, 0x00000202, 0x00020202, 0x00000202, 0x02020202 
            dd 0x02000202, 0x00000000, 0x02000202, 0x02000000, 0x02000202, 0x00020000, 0x02000202, 0x02020000 
            dd 0x02000202, 0x00000200, 0x02000202, 0x02000200, 0x02000202, 0x00020200, 0x02000202, 0x02020200 
            dd 0x02000202, 0x00000002, 0x02000202, 0x02000002, 0x02000202, 0x00020002, 0x02000202, 0x02020002 
            dd 0x02000202, 0x00000202, 0x02000202, 0x02000202, 0x02000202, 0x00020202, 0x02000202, 0x02020202 
            dd 0x00020202, 0x00000000, 0x00020202, 0x02000000, 0x00020202, 0x00020000, 0x00020202, 0x02020000 
            dd 0x00020202, 0x00000200, 0x00020202, 0x02000200, 0x00020202, 0x00020200, 0x00020202, 0x02020200 
            dd 0x00020202, 0x00000002, 0x00020202, 0x02000002, 0x00020202, 0x00020002, 0x00020202, 0x02020002 
            dd 0x00020202, 0x00000202, 0x00020202, 0x02000202, 0x00020202, 0x00020202, 0x00020202, 0x02020202 
            dd 0x02020202, 0x00000000, 0x02020202, 0x02000000, 0x02020202, 0x00020000, 0x02020202, 0x02020000 
            dd 0x02020202, 0x00000200, 0x02020202, 0x02000200, 0x02020202, 0x00020200, 0x02020202, 0x02020200 
            dd 0x02020202, 0x00000002, 0x02020202, 0x02000002, 0x02020202, 0x00020002, 0x02020202, 0x02020002 
            dd 0x02020202, 0x00000202, 0x02020202, 0x02000202, 0x02020202, 0x00020202, 0x02020202, 0x02020202
;*********************************************************


;*********************************************************
; This is for the 2 high bits for the Tiles palett entry.
; The entry for this table has to have the one of the
; following format:
;                    8-bit value:     1) xx 00 00 00
;                                     2) 00 xx 00 00
;                                     3) 00 00 xx 00
;                                     4) 00 00 00 xx
; If these are not the case, a NOT DESIRED value will be
; returned; else the value returned will be a 4 8-bit value
; with the 2 high bits of each low nibble of a byte with xx
; value ("ORed" with 0x20 - border reasons)

align 4, db 0        ; 32 x 8 dwords      (1024 bytes)
Expand_Table_3:
            dd 0x00000000, 0x04040404, 0x08080808, 0x0C0C0C0C, 0x04040404, 0x00000000, 0x00000000, 0x00000000 
            dd 0x08080808, 0x00000000, 0x00000000, 0x00000000, 0x0C0C0C0C, 0x00000000, 0x00000000, 0x00000000 
            dd 0x04040404, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 
            dd 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 
            dd 0x08080808, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 
            dd 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 
            dd 0x0C0C0C0C, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 
            dd 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 
            dd 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404 
            dd 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404 
            dd 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404 
            dd 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404 
            dd 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404 
            dd 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404 
            dd 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404 
            dd 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404, 0x04040404 
            dd 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808 
            dd 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808 
            dd 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808 
            dd 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808 
            dd 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808 
            dd 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808 
            dd 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808 
            dd 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808, 0x08080808 
            dd 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C 
            dd 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C 
            dd 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C 
            dd 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C 
            dd 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C 
            dd 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C 
            dd 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C 
            dd 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C, 0x0C0C0C0C 
;*********************************************************


;*********************************************************
; This is for the 2 high bits for the Sprites palett entry.
; The index for this table is a 8-bit value took from the
; Sprite's attribute byte.
; The value returned will be a 4 8-bit value with the 2
; high bits of each low nibble of a byte set with the low
; two bits of the index, + 1 in the high nibble (because
; this is for Sprites)

align 4, db 0        ; 32 x 8 dwords      (1024 bytes)
Expand_Table_4:
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
            dd 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 0x10101010, 0x14141414, 0x18181818, 0x1C1C1C1C, 
;*********************************************************



;*********************************************************
; The index is a 8-bit value with the Tile position in the
; NT (range: 0-959). It will return the offset inside the
; PPU Attribute Table that contain the attribute byte
; desired

align 4, db 0     ; 32 x 32 bytes       (1024 bytes)
Attr_Table:
            db 00,00,00,00, 01,01,01,01, 02,02,02,02, 03,03,03,03, 04,04,04,04, 05,05,05,05, 06,06,06,06, 07,07,07,07
            db 00,00,00,00, 01,01,01,01, 02,02,02,02, 03,03,03,03, 04,04,04,04, 05,05,05,05, 06,06,06,06, 07,07,07,07
            db 00,00,00,00, 01,01,01,01, 02,02,02,02, 03,03,03,03, 04,04,04,04, 05,05,05,05, 06,06,06,06, 07,07,07,07
            db 00,00,00,00, 01,01,01,01, 02,02,02,02, 03,03,03,03, 04,04,04,04, 05,05,05,05, 06,06,06,06, 07,07,07,07

            db 08,08,08,08, 09,09,09,09, 10,10,10,10, 11,11,11,11, 12,12,12,12, 13,13,13,13, 14,14,14,14, 15,15,15,15 
            db 08,08,08,08, 09,09,09,09, 10,10,10,10, 11,11,11,11, 12,12,12,12, 13,13,13,13, 14,14,14,14, 15,15,15,15
            db 08,08,08,08, 09,09,09,09, 10,10,10,10, 11,11,11,11, 12,12,12,12, 13,13,13,13, 14,14,14,14, 15,15,15,15 
            db 08,08,08,08, 09,09,09,09, 10,10,10,10, 11,11,11,11, 12,12,12,12, 13,13,13,13, 14,14,14,14, 15,15,15,15 

            db 16,16,16,16, 17,17,17,17, 18,18,18,18, 19,19,19,19, 20,20,20,20, 21,21,21,21, 22,22,22,22, 23,23,23,23
            db 16,16,16,16, 17,17,17,17, 18,18,18,18, 19,19,19,19, 20,20,20,20, 21,21,21,21, 22,22,22,22, 23,23,23,23
            db 16,16,16,16, 17,17,17,17, 18,18,18,18, 19,19,19,19, 20,20,20,20, 21,21,21,21, 22,22,22,22, 23,23,23,23
            db 16,16,16,16, 17,17,17,17, 18,18,18,18, 19,19,19,19, 20,20,20,20, 21,21,21,21, 22,22,22,22, 23,23,23,23

            db 24,24,24,24, 25,25,25,25, 26,26,26,26, 27,27,27,27, 28,28,28,28, 29,29,29,29, 30,30,30,30, 31,31,31,31
            db 24,24,24,24, 25,25,25,25, 26,26,26,26, 27,27,27,27, 28,28,28,28, 29,29,29,29, 30,30,30,30, 31,31,31,31
            db 24,24,24,24, 25,25,25,25, 26,26,26,26, 27,27,27,27, 28,28,28,28, 29,29,29,29, 30,30,30,30, 31,31,31,31
            db 24,24,24,24, 25,25,25,25, 26,26,26,26, 27,27,27,27, 28,28,28,28, 29,29,29,29, 30,30,30,30, 31,31,31,31

            db 32,32,32,32, 33,33,33,33, 34,34,34,34, 35,35,35,35, 36,36,36,36, 37,37,37,37, 38,38,38,38, 39,39,39,39
            db 32,32,32,32, 33,33,33,33, 34,34,34,34, 35,35,35,35, 36,36,36,36, 37,37,37,37, 38,38,38,38, 39,39,39,39
            db 32,32,32,32, 33,33,33,33, 34,34,34,34, 35,35,35,35, 36,36,36,36, 37,37,37,37, 38,38,38,38, 39,39,39,39
            db 32,32,32,32, 33,33,33,33, 34,34,34,34, 35,35,35,35, 36,36,36,36, 37,37,37,37, 38,38,38,38, 39,39,39,39

            db 40,40,40,40, 41,41,41,41, 42,42,42,42, 43,43,43,43, 44,44,44,44, 45,45,45,45, 46,46,46,46, 47,47,47,47
            db 40,40,40,40, 41,41,41,41, 42,42,42,42, 43,43,43,43, 44,44,44,44, 45,45,45,45, 46,46,46,46, 47,47,47,47
            db 40,40,40,40, 41,41,41,41, 42,42,42,42, 43,43,43,43, 44,44,44,44, 45,45,45,45, 46,46,46,46, 47,47,47,47
            db 40,40,40,40, 41,41,41,41, 42,42,42,42, 43,43,43,43, 44,44,44,44, 45,45,45,45, 46,46,46,46, 47,47,47,47

            db 48,48,48,48, 49,49,49,49, 50,50,50,50, 51,51,51,51, 52,52,52,52, 53,53,53,53, 54,54,54,54, 55,55,55,55 
            db 48,48,48,48, 49,49,49,49, 50,50,50,50, 51,51,51,51, 52,52,52,52, 53,53,53,53, 54,54,54,54, 55,55,55,55 
            db 48,48,48,48, 49,49,49,49, 50,50,50,50, 51,51,51,51, 52,52,52,52, 53,53,53,53, 54,54,54,54, 55,55,55,55 
            db 48,48,48,48, 49,49,49,49, 50,50,50,50, 51,51,51,51, 52,52,52,52, 53,53,53,53, 54,54,54,54, 55,55,55,55 

            db 56,56,56,56, 57,57,57,57, 58,58,58,58, 59,59,59,59, 60,60,60,60, 61,61,61,61, 62,62,62,62, 63,63,63,63
            db 56,56,56,56, 57,57,57,57, 58,58,58,58, 59,59,59,59, 60,60,60,60, 61,61,61,61, 62,62,62,62, 63,63,63,63
            db 56,56,56,56, 57,57,57,57, 58,58,58,58, 59,59,59,59, 60,60,60,60, 61,61,61,61, 62,62,62,62, 63,63,63,63
            db 56,56,56,56, 57,57,57,57, 58,58,58,58, 59,59,59,59, 60,60,60,60, 61,61,61,61, 62,62,62,62, 63,63,63,63
;*********************************************************


;*********************************************************
; The index is a 8-bit value with the Tile position in the
; NT (range: 0-959). We MUST 'ANDed' the returned value
; with the PPU Attribute Table byte to obtain a byte
; holding the 2 high bits of the palett entry. The byte
; 'ANDed' will be in one of the following ways:
;                                     1) xx 00 00 00
;                                     2) 00 xx 00 00
;                                     3) 00 00 xx 00
;                                     4) 00 00 00 xx
;  Then we must call Expand_Table_3 to put the 2 bits in
; their correct places.

align 4, db 0     ; 32 x 32 bytes      (1024 bytes)
Order_Table:
            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0

            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0

            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0

            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0

            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0

            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0

            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0

            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C, 0x03,0x03,0x0C,0x0C
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0
            db 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0, 0x30,0x30,0xC0,0xC0
;*********************************************************


;*********************************************************
; The index is a codified byte (see Sprite rendering
; routine) to select the Sprite PT logical value.
;

align 4, db 0
Spr_PT:
      dd 0x1000, 0x1000, 0x1000, 0x1000, 0x0000, 0x0000, 0x0000, 0x0000
      dd 0x0000, 0x0FF0, 0x0000, 0x0FF0, 0x0000, 0x0FF0, 0x0000, 0x0FF0
;*********************************************************


;*********************************************************
; Adjust for vertical flipping (if there's one).
; See the sprite's rendering to know how the index is made.
; This return a correct value indicating the line inside
; the actual Sprite that's being rendering.

align 4, db 0        ; [(20 x 8) - 1] dwords  (636 bytes)
Adj_V_Flip:
            dd 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff        ; unused
            dd 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07  ; no vert.
            dd 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07  ; flipping
            dd 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17 
            dd 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff  ; unused
            dd 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff  ; unused
            dd 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff  ; unused
            dd 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff  ; unused
            dd 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff  ; unused
            dd 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff  ; unused
            dd 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff  ; unused
            dd 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff  ; unused
            dd 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff  ; unused
            dd 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff  ; unused
            dd 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff  ; unused
            dd 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff  ; unused
            dd 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff  ; unused
            dd 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x00  ; now we have
            dd 0x17, 0x16, 0x15, 0x14, 0x13, 0x12, 0x11, 0x10  ; vertical
            dd 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x00  ; flipping
;*********************************************************



;*********************************************************
; The index is the sprite X-coordenate, and we got a 64-bit
; value (8 bytes - pixels) with '0xFF' for no clipping, and
; '0x00' for clipping (hence, this must be ANDed with the
; 8 bytes of the sprites that are being rendered)
;

align 4, db 0     ; (2048 bytes)
Spr_Clip_Table:
            dd 0x00000000, 0x00000000, 0x00000000, 0xFF000000, 0x00000000, 0xFFFF0000, 0x00000000, 0xFFFFFF00
            dd 0x00000000, 0xFFFFFFFF, 0xFF000000, 0xFFFFFFFF, 0xFFFF0000, 0xFFFFFFFF, 0xFFFFFF00, 0xFFFFFFFF
            times 496 dd 0xFFFFFFFF
;*********************************************************


;*********************************************************
; If the index for this table is 0 then a byte with bit
; 6 set will be return for "ORing" with Status register

align 4, db 0     ; 8 x 8 bytes     (64 bytes)
Hit_Flag_Table:
            db 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
            db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
            db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
            db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
            db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
            db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
            db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
            db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
;*********************************************************



;----------------------------------- // -----------------------------------
