/*

*/
#include <stdio.h>

#include "..\include\6502.h"
#include "..\include\io.h"
#include "..\include\types.h"
#include "..\include\mmc.h"
#include "..\include\ppu.h"
#include "..\include\video.h"


u32 SetPRGROM(u32 bank_size, FILE *rom_file);
u32 SetVROM(u32 bank_size, FILE *rom_file);

/*
 iNES file format
---------------------------------------------------------------------------
0-3      String "NES^Z" used to recognize .NES files.
4        Number of 16kB ROM banks.
5        Number of 8kB VROM banks.
6        bit 0     1 for vertical mirroring, 0 for horizontal mirroring
         bit 1     1 for battery-backed RAM at $6000-$7FFF
         bit 2     1 for a 512-byte trainer at $7000-$71FF
         bit 3     1 for a four-screen VRAM layout 
         bit 4-7   Four lower bits of ROM Mapper Type.
7        bit 0-3   Reserved, must be zeroes!
         bit 4-7   Four higher bits of ROM Mapper Type.
8-15     Reserved, must be zeroes!
16-...   ROM banks, in ascending order. If a trainer is present, its
         512 bytes precede the ROM bank contents.
...-EOF  VROM banks, in ascending order.
---------------------------------------------------------------------------
*/
struct
{
	u8	ID[4];
	u8 prg_rom_16k_banks;
	u8 chr_rom_8k_banks;
	u8 byte_6;
	u8 byte_7;
	u8 reserved[8];
} iNES_header;


int LoadROM(char *rom_name)
{
	FILE *rom_file;		// ponteiro para "input stream" (stdio)
   long rom_size;
   short Map;           // mapper number
   int  ret_val;
   u16 i;              // counter

// Abre arquivo desejado (arquivo binario so' para leitura)
   if ((rom_file = fopen(rom_name, "rb")) == NULL) return 1;

// Pega informacao no cabecalho
   fread(&iNES_header, 0x01, 16, rom_file);

// Check for signature
   if ((iNES_header.ID[0] != 0x4E)||(iNES_header.ID[1] != 0x45)||
		(iNES_header.ID[2] != 0x53)||(iNES_header.ID[3] != 0x1A))
   { fclose(rom_file); return 2; }

// Check PRG pages validity (1-64)
   if ((iNES_header.prg_rom_16k_banks <= 0) || 
		(iNES_header.prg_rom_16k_banks >= 65))
   { fclose(rom_file); return 3; }

// Check CHR pages validity (0-64)
   if (iNES_header.chr_rom_8k_banks >= 65)
   { fclose(rom_file); return 4; }

// This is for use in IO module (to be "XORed" for NT mirroring)
// Vertical => 0x02 | Horizontal => 0x01
   Mirror_Type = (iNES_header.byte_6 & 0x01) ? 0x02:0x01;

// Get Mapper
   Map = ((iNES_header.byte_6 & 0xF0) >> 4) | iNES_header.byte_7;
   Mapper = (u32) Map;

   printf("iNES file header found!\n");
   printf("PRG: %d x 16kb page(s) | CHR: %d x 8kb page(s)\n", iNES_header.prg_rom_16k_banks, iNES_header.chr_rom_8k_banks);
   printf("Mirror Type: %s", (iNES_header.byte_6 & 0x01) ? "Vertical":"Horizontal");
   printf(" | Battery RAM: %s", (iNES_header.byte_6 & 0x02) ? "YES":"NO");
   printf(" | Trainer: %s", (iNES_header.byte_6 & 0x04) ? "YES":"NO");
   printf(" | 4 Screen VRAM: %s", (iNES_header.byte_6 & 0x08) ? "YES\n":"NO\n");
   printf("Mapper #%d\n", Map);

	printf("Allocating 6502 RAM memory... ");
	if ((a6502.PrgRamBase = (u8 *) calloc(0x0800, 1)) == NULL) return 6;
   printf("OK!\n");


	printf("Allocating SRAM memory... ");
	if ((a6502.Bank0x6000 = (u8 *) calloc(0x2000, 1)) == NULL) return 6;
	printf("OK!\n");


// by now VRAM are 4 statics banks 
	printf("Allocating VRAM memory... ");
	for (i=0; i<4; i++)
		if ((VRAMBank[i] = (u8 *) calloc(0x0400, 1)) == NULL) return 7;
	printf("OK!\n");


// VRAM bank settings - this is 4-screen mirroring
	PPU.Bank0x2000 = VRAMBank[0];
	PPU.Bank0x2400 = VRAMBank[1];
	PPU.Bank0x2800 = VRAMBank[2];
	PPU.Bank0x2C00 = VRAMBank[3];


   switch (Mapper)
   {
/******************** Mapper #0 start-up configuration *********************/

	case 0x00:   

		if (iNES_header.prg_rom_16k_banks == 1)			// only one 16k bank
		{
			SetPRGROM(16, rom_file);

		// PRG-ROM bank settings
			a6502.Bank0x8000 = PRGBank[0];
			a6502.Bank0xA000 = a6502.Bank0x8000+0x2000;
			a6502.Bank0xC000 = a6502.Bank0x8000;	// 16k carts must mirror into 0xC000
			a6502.Bank0xE000 = a6502.Bank0xC000+0x2000;
		}
		else	// prg_rom_16k_banks == 2		// one single 32k bank
		{
			SetPRGROM(32, rom_file);
		// PRG-ROM bank settings
			a6502.Bank0x8000 = PRGBank[0];

		// assign adjacents banks
			a6502.Bank0xA000 = a6502.Bank0x8000+0x2000;
			a6502.Bank0xC000 = a6502.Bank0x8000+0x4000;
			a6502.Bank0xE000 = a6502.Bank0x8000+0x6000;
		}


		SetVROM(8, rom_file);

		// this MAPPER must have at maximum 1 8k bank
		PPU.Bank0x0000 = VROMBank[0];
		PPU.Bank0x0400 = PPU.Bank0x0000+0x0400;
		PPU.Bank0x0800	= PPU.Bank0x0000+0x0800;
		PPU.Bank0x0C00	= PPU.Bank0x0000+0x0C00;
		PPU.Bank0x1000	= PPU.Bank0x0000+0x1000;
		PPU.Bank0x1400	= PPU.Bank0x0000+0x1400;
		PPU.Bank0x1800	= PPU.Bank0x0000+0x1800;
		PPU.Bank0x1C00	= PPU.Bank0x0000+0x1C00;

	//***************************

	   MMC_Handler = No_Mapper;   // no mapper is needed

   break;


/******************** Mapper #1 start-up configuration *********************/

   case 0x01:
	//***************************

	// set banks to minimum size: 16k
		SetPRGROM(16, rom_file);

	// PRG-ROM bank settings
		a6502.Bank0x8000 = PRGBank[0];
		a6502.Bank0xC000 = PRGBank[PRGPages];		// last bank

	// assign adjacents banks
		a6502.Bank0xA000 = a6502.Bank0x8000+0x2000;
		a6502.Bank0xE000 = a6502.Bank0xC000+0x2000;


		SetVROM(4, rom_file);		// minimum of 4k banks

		PPU.Bank0x0000 = VROMBank[0];
		PPU.Bank0x0400 = PPU.Bank0x0000+0x0400;
		PPU.Bank0x0800	= PPU.Bank0x0000+0x0800;
		PPU.Bank0x0C00	= PPU.Bank0x0000+0x0C00;

		PPU.Bank0x1000	= VROMBank[1];
		PPU.Bank0x1400	= PPU.Bank0x1000+0x0400;
		PPU.Bank0x1800	= PPU.Bank0x1000+0x0800;
		PPU.Bank0x1C00	= PPU.Bank0x1000+0x0C00;

      if (VROMPages < 0)  // no VROM, allocate 8k for a fake VROM
      {
			if ((VROMBank[0] = (u8 *) calloc(0x2000, 1)) == NULL) return 7;

         PPU.Bank0x0000 = VROMBank[0];
         PPU.Bank0x0400 = PPU.Bank0x0000+0x0400;
         PPU.Bank0x0800 = PPU.Bank0x0000+0x0800;
         PPU.Bank0x0C00 = PPU.Bank0x0000+0x0C00;
         PPU.Bank0x1000 = PPU.Bank0x0000+0x1000;
         PPU.Bank0x1400 = PPU.Bank0x0000+0x1400;
         PPU.Bank0x1800 = PPU.Bank0x0000+0x1800;
         PPU.Bank0x1C00 = PPU.Bank0x0000+0x1C00;
      }

      MMC_Handler = Mapper_01;


	break;


/******************** Mapper #2 start-up configuration *********************/

   case 0x02:     // multi 16k PRG pages

		SetPRGROM(16, rom_file);

      a6502.Bank0x8000 = PRGBank[0];            // 1st
      a6502.Bank0xA000 = a6502.Bank0x8000+0x2000;
      a6502.Bank0xC000 = PRGBank[PRGPages];     // last
      a6502.Bank0xE000 = a6502.Bank0xC000+0x2000;

		
		// this mapper shouldn't have VROM
      SetVROM(8, rom_file);         // maximum = 1 8k VROM bank

		if (VROMPages < 0)  // no VROM, allocate 8k for a fake VROM
		{
			VROMPages = -1;
			if ((VROMBank[0] = (u8 *) calloc(0x2000, 1)) == NULL) return 7;
		}

		PPU.Bank0x0000 = VROMBank[0];
		PPU.Bank0x0400 = PPU.Bank0x0000+0x0400;
		PPU.Bank0x0800	= PPU.Bank0x0000+0x0800;
		PPU.Bank0x0C00	= PPU.Bank0x0000+0x0C00;
		PPU.Bank0x1000	= PPU.Bank0x0000+0x1000;
		PPU.Bank0x1400	= PPU.Bank0x0000+0x1400;
		PPU.Bank0x1800	= PPU.Bank0x0000+0x1800;
		PPU.Bank0x1C00	= PPU.Bank0x0000+0x1C00;

      MMC_Handler = Mapper_02;

	break;


/******************** Mapper #3 start-up configuration *********************/

   case 0x03:     

		SetPRGROM(32, rom_file);		// Only 01 32k bank

	// Bank organization
		a6502.Bank0x8000 = PRGBank[0];
      a6502.Bank0xA000 = a6502.Bank0x8000+0x2000;
      a6502.Bank0xC000 = a6502.Bank0x8000+0x4000;
      a6502.Bank0xE000 = a6502.Bank0x8000+0x6000;


	// Multi 8k banks
		SetVROM(8, rom_file);
      
   // Bank organization
		PPU.Bank0x0000 = VROMBank[0];
		PPU.Bank0x0400 = PPU.Bank0x0000+0x0400;
		PPU.Bank0x0800	= PPU.Bank0x0000+0x0800;
		PPU.Bank0x0C00	= PPU.Bank0x0000+0x0C00;
		PPU.Bank0x1000	= PPU.Bank0x0000+0x1000;
		PPU.Bank0x1400	= PPU.Bank0x0000+0x1400;
		PPU.Bank0x1800	= PPU.Bank0x0000+0x1800;
		PPU.Bank0x1C00	= PPU.Bank0x0000+0x1C00;

      MMC_Handler = Mapper_03;      // Mapper function to use

	break;


/******************** Mapper #4 start-up configuration *********************/

	case 0x04:
		
		// 8k PRGROM banks
		SetPRGROM(8, rom_file);
		
		a6502.Bank0x8000 = PRGBank[0];
      a6502.Bank0xA000 = PRGBank[1];
      a6502.Bank0xC000 = PRGBank[PRGPages-1];
      a6502.Bank0xE000 = PRGBank[PRGPages];


		SetVROM(1, rom_file);			// 1k banks

/********************* CHANGED HERE FOR FINAL_FANTASY 3 *********************/
// just added theses lines:

      if (VROMPages < 0)  // some carts (FF3) doesn't have VROM, but
      {                   // they swap banks (RAM) at PPU 0x0000-0x1FFF!!!
			VROMPages = -1;

      // allocate 8x1K RAM bank's
         for (i=0; i<8; i++)
            if ((VROMBank[i] = (u8 *) calloc(0x0400, 1)) == NULL)
               return 7;
      }
/****************************************************************************/

		PPU.Bank0x0000 = VROMBank[0];
		PPU.Bank0x0400 = VROMBank[1];
		PPU.Bank0x0800 = VROMBank[2];
		PPU.Bank0x0C00 = VROMBank[3];
		PPU.Bank0x1000 = VROMBank[4];
		PPU.Bank0x1400 = VROMBank[5];
		PPU.Bank0x1800 = VROMBank[6];
		PPU.Bank0x1C00 = VROMBank[7];


		MMC_Handler = Mapper_04;

	break;

   default:
		fclose(rom_file); return 5;

   }


	PPU.Palette[0x20] = 0x80;
	PPU.Palette[0x21] = 0x81;

   if (Mirror_Type == 0x01)
   {
		PPU.Bank0x2400 = PPU.Bank0x2000;
      PPU.Bank0x2800 = PPU.Bank0x2C00;
   }
   else
   {
		PPU.Bank0x2800 = PPU.Bank0x2000;
		PPU.Bank0x2400 = PPU.Bank0x2C00;
   }


   fclose(rom_file);
   return 0;
}







/*

	bank_size:		32k, 16k or 8k
*/
u32 SetPRGROM(u32 bank_size, FILE *rom_file)
{
	u32 memory_to_allocate;
	long byte_read;		// numero de bytes lidos (ao carregar arquivo)
   s32 i;

	fseek(rom_file,0x10L,SEEK_SET);

	if (bank_size == 32)
	{
		memory_to_allocate = 0x8000;
		PRGPages = (iNES_header.prg_rom_16k_banks/2)-1;			// 0 means 1, 1 -> 2, ... -1 == no bank
	}
	else if (bank_size == 16)
	{
		memory_to_allocate = 0x4000;
		PRGPages = iNES_header.prg_rom_16k_banks-1;
	}
	else	// bank_size = 8
	{
		memory_to_allocate = 0x2000;
		PRGPages = (iNES_header.prg_rom_16k_banks*2)-1;
	}

	for (i=0; i<=PRGPages; i++)
	{
		if ((PRGBank[i] = (u8 *) calloc(memory_to_allocate, 1)) == NULL) return 6;

	   byte_read = fread(PRGBank[i], 1, memory_to_allocate, rom_file);
      if (byte_read != memory_to_allocate) { fclose(rom_file); return 8; }
	}
}


/*

	bank_size:		8k, 4k, 2k or 1k

*/
u32 SetVROM(u32 bank_size, FILE *rom_file)
{
	u32 memory_to_allocate;
	long byte_read;		// numero de bytes lidos (ao carregar arquivo)
   s32 i;

	if (bank_size == 8)
	{
		memory_to_allocate = 0x2000;
		VROMPages = (s32)iNES_header.chr_rom_8k_banks-1;					// 0 means 1, 1 -> 2, ... -1 == no bank
	}
	else if (bank_size == 4)
	{
		memory_to_allocate = 0x1000;
		VROMPages = (s32)(iNES_header.chr_rom_8k_banks*2)-1;
	}
	else if (bank_size == 2)
	{
		memory_to_allocate = 0x0800;
		VROMPages = (s32)(iNES_header.chr_rom_8k_banks*4)-1;
	}
	else	// bank_size == 1
	{
		memory_to_allocate = 0x0400;
		VROMPages = (s32)(iNES_header.chr_rom_8k_banks*8)-1;
	}

	for (i=0; i<=VROMPages; i++)
	{
		if ((VROMBank[i] = (u8 *) calloc(memory_to_allocate, 1)) == NULL) return 7;

		byte_read = fread(VROMBank[i], 1, memory_to_allocate, rom_file);
      if (byte_read != memory_to_allocate) { fclose(rom_file); return 9; }
	}
}
