

   extern   _FrameStart
   extern   _ScanlineStart
	extern	_ScanlineEnd
	extern	_RenderBg
	extern	_RenderSprite
	extern	_ClearBg
	extern	_ClearSpr

;------------
; Data
;------------
   extern _current_scanline

   extern   _PPU
   extern   _Ctrl_Reg_1, _Ctrl_Reg_2, _Status_Reg, _Spr_RAM_Ad
   extern   _Hor_Scr_Reg, _Vt_Scr_Reg, _VRAM_Ad_Reg, _SPR_RAM
	extern   _VROM, _VRAM
   extern   _pBank0x0000, _pBank0x0400, _pBank0x0800, _pBank0x0C00
   extern   _pBank0x1000, _pBank0x1400, _pBank0x1800, _pBank0x1C00
   extern   _pBank0x2000, _pBank0x2400, _pBank0x2800, _pBank0x2C00
	extern	_Palette
	
	extern	_PTBgBase, _PTSpBase, _SPR_Size, _Mirror_Type



; NAO ESTÄ NO .H

	extern loop_v,	loop_t, loop_x
