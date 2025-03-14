
#ifndef _MMC_H
#define _MMC_H



volatile extern struct {
   u32 Reg1;
   u32 Reg2;
   u32 Reg3;
   u32 Reg4;
   u32 SP_Reg;
} MMC1;


volatile extern struct {
   u32 Command;
   u32 Latch;
   u32 XOR_Val;
   u32 IRQ_ENABLE;
   u32 IRQ_COUNTER;
} MMC3;


extern u32 PRGPages;
extern u8  *PRGBank[128];
extern void (*MMC_Handler)(u32 address, u8 data);  // pointer to a MMC handler

extern s32 VROMPages;
extern u8  *VROMBank[512];
extern u8  *VRAMBank[4];


extern   void No_Mapper(u32 address, u8 data);
extern   void Mapper_01(u32 address, u8 data);
extern   void Mapper_02(u32 address, u8 data);
extern   void Mapper_03(u32 address, u8 data);
extern   void Mapper_04(u32 address, u8 data);

#endif /* _MMC_H */
