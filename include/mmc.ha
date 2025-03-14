;------------
; Functions
;------------
   extern   _No_Mapper     ; void No_Mapper(void);
   extern   _Mapper_01     ; void Mapper_01(int Data, int Addr);
   extern   _Mapper_02     ; void Mapper_02(int Data, int Addr);
   extern   _Mapper_03     ; void Mapper_03(int Data, int Addr);
   extern   _Mapper_04     ; void Mapper_04(int Data, int Addr);

;------------
; Data
;------------
   extern   _MMC3
   extern   _MMC3_Command, _MMC3_Latch, _MMC3_XOR_Val
   extern   _MMC3_IRQ_EBL, _MMC3_IRQ_CTR

   extern   _MMC1
   extern   _MMC1_Reg1, _MMC1_Reg2, _MMC1_Reg3, _MMC1_Reg4, _MMC1_SP_Reg

	extern	_PRGPages
	extern	_PRGBank
	extern	_MMC_Handler

	extern	_VROMPages 
	extern	_VROMBank
	extern	_VRAMBank