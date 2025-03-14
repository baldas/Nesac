/*****************************************************************************
                        *** 6502 C/C++ interface ***


*****************************************************************************/

#ifndef _6502_H
#define _6502_H

/*==========================================================================*/
/*     SYSTEM HEADERS                                                       */
/*==========================================================================*/

/*==========================================================================*/
/*     LOCAL HEADERS                                                        */
/*==========================================================================*/
#include "types.h"


/*==========================================================================*/
/*     INTERFACE DEFINITIONS / ENUMERATIONS / TYPEDEFS                      */
/*==========================================================================*/

/*==========================================================================*/
/*     INTERFACE DATA (PROTOTYPE)                                           */
/*==========================================================================*/
/* _DO_NOT_ change the order of the struct members */
extern volatile struct     
{
/* 6502 registers (they are implemented as 32-bit) */
   u32 PC;						/* Program Counter    (16-bit) */
   u32 rA;						/* Accumulator        (8-bit)	 */
   u32 rX;						/* X Index Reg.       (8-bit)  */
   u32 rY;						/* Y Index Reg.       (8-bit)  */
   u32 rS;						/* Stack Pointer      (8-bit)  */
   u32 rF;						/* Processor Status   (8-bit)  */

   s32 Cycles;					/* number of cycles to be executed */
									/* this is SIGNED (more accurate)  */

	u32 TrapBadOpCodes;		/* set this to 'TRUE' if you want to trap bad  */
									/* opcodes. RUN_6502 will return a value !0x00 */
									/* (see function prototype bellow */

/* pointers to the memory banks */
   u8  *PrgRamBase;			/* pointer to NES RAM memory				*/
	u8	 *Bank0x2000;
	u8  *Bank0x4000;
   u8  *Bank0x6000;			/* pointer to cartridge RAM (may be battery-backed)*/
   u8  *Bank0x8000;			/* pointer to PRG-ROM bank 0x8000		*/
   u8  *Bank0xA000;			/* pointer to PRG-ROM bank 0xA000		*/
   u8  *Bank0xC000;			/* pointer to PRG-ROM bank 0xC000		*/
   u8  *Bank0xE000;			/* pointer to PRG-ROM bank 0xE000		*/
} a6502;


/*==========================================================================*/
/*     INTERFACE CLASS / FUNCTIONS PROTOTYPES                               */
/*==========================================================================*/

#ifdef __cplusplus
extern "C" {
#endif
/* These functions follow the standard C calling conventions */

	u32  RUN_6502(void);	
/* return value:	
		0x00			= OK!
		0x0000XX00	= Bad Opcode trapped! Opcode is returned at XX.
						  This will be returned ONLY IF '_TrapBadOpCodes' == TRUE */
	void NMI_6502(void);
	void IRQ_6502(void);
	void RST_6502(void);

#ifdef __cplusplus
}
#endif


#endif  /* _6502_H */