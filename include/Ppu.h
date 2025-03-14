
#ifndef _GRAPH_H_
#define _GRAPH_H_

#include "types.h"

// Estrutura da PPU (registradores e memoria)
volatile extern struct {
   u32 Ctrl_Reg_1;       // PPU Control Register #1    0x2000
   u32 Ctrl_Reg_2;       // PPU Control Register #2    0x2001
   u32 Status_Reg;       // PPU Status Register        0x2002
   u32 Spr_RAM_Ad;       // SPR-RAM Address Register   0x2003
   u32 Hor_Scr_Reg;      // Hor. Scroll Register  (at 0x2005)
   u32 Vt_Scr_Reg;       // Vert. Scroll Register (at 0x2005)
   u32 VRAM_Ad_Reg;      // VRAM Address Register      0x2006
   u8  SPR_RAM[256];     // Sprite RAM
	u8	*Bank0x0000;
	u8	*Bank0x0400;
	u8	*Bank0x0800;
	u8 *Bank0x0C00;
	u8 *Bank0x1000;
	u8 *Bank0x1400;
	u8 *Bank0x1800;
	u8 *Bank0x1C00;
	u8 *Bank0x2000;
	u8 *Bank0x2400;
	u8 *Bank0x2800;
	u8 *Bank0x2C00;
	u8	Palette[0x40];
} PPU;


extern u32 current_scanline;
extern u32 PTBgBase, PTSpBase, SPR_Size, Mirror_Type;
//extern u32 G_Buf, Spr_Buf;

//extern void DoLine(void);
extern void RenderBg(void);
extern void RenderSprite(void);
extern void FrameStart(void);
extern void ScanlineEnd(void);
extern void ScanlineStart(void);
extern void ClearBg(void);
extern void ClearSpr(void);

#endif /* _GRAPH_H_ */
