/*****************************************************************************
                              *** TITLE ***

   A brief description.


What's New:

   version x.xx:
    . . . . . . . . . 

   22/01:   (GRAPH\NES modules)
            - Implemented a basic 'emulation main loop' with a FPS measure.

   28/01:
            - Implemented a iNES header decodifier. Now we can figure out what
         games the emulator can run (LoadROM module).
            - Implemented NT mirroring (easier than I thought!).
   21/02:
         - Now the emulator checks for a palett file at start-up. If it was
      not found, then a built-in is loaded.

  15/09:
         - It was a long time, isn't it? I've found some time and made some
      little changes. This time I've put my own keyboard handler (till here
      I was using Allegro one). This handler is the same I've been working
      for a game to be released by Sigma Interactive (c) in the future.


   version x.xx:
    . . . . . . . . .

    .
    .
    .

                                            Sigma Interactive (c) - Month/Year
                                                                  version x.xx
*****************************************************************************/

/*==========================================================================*/
/*     FEATURE TEST SWITCHES                                                */
/*==========================================================================*/

/*==========================================================================*/
/*     SYSTEM HEADERS                                                       */
/*==========================================================================*/
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <go32.h>



/*==========================================================================*/
/*     LOCAL HEADERS                                                        */
/*==========================================================================*/
#include "..\include\6502.h"
#include "..\include\IO.h"
#include "..\include\keyboard.h"
#include "..\include\types.h"
#include "..\include\mmc.h"
#include "..\include\ppu.h"
#include "..\include\video.h"


/*==========================================================================*/
/*     INTERFACE DATA                                                       */
/*==========================================================================*/
volatile short VideoSel;
volatile u32	video_base;
u32	lines_to_show;


/*==========================================================================*/
/*     REQUIRED EXTERNAL REFERENCES (AVOID)                                 */
/*==========================================================================*/
extern int LoadROM(char *);


/*==========================================================================*/
/*     PRIVATE DEFINITIONS / ENUMERATIONS / TYPEDEFS                        */
/*==========================================================================*/
#define SCREEN_X	256
#define SCREEN_Y	256


/*==========================================================================*/
/*     PRIVATE STRUCTURES / UNIONS                                          */
/*==========================================================================*/

/*==========================================================================*/
/*     PRIVATE DATA                                                         */
/*==========================================================================*/
static int ScLnPCyc = 114;
static u16 HexToDec[] =
{
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x10, 0x11, 0x12,
   0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25,
	0x26, 0x27, 0x28, 0x29, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38,
	0x39, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x50, 0x51,
	0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x60, 0x61, 0x62, 0x63, 0x64,
	0x65, 0x66, 0x67, 0x68, 0x69, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
	0x78, 0x79, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x90,
	0x91, 0x92, 0x93, 0x94, 0x95, 0x96
};

u8 DummyBuf[8+256+8];
u8 NES_line[8+256+8];
u8 *G_Buf;

u8 frame_buffer[256*240];


/*==========================================================================*/
/*     PRIVATE CLASS / FUNCTIONS PROTOTYPES                                 */
/*==========================================================================*/
int Get_User_Enter(void);
int LoadPalette(void);
void InitNES(void);
void RunNES(void);
void EndNES(void);


/*==========================================================================*/
/*     PRIVATE CLASS / FUNCTIONS IMPLEMENTATION                             */
/*==========================================================================*/
int Get_User_Enter(void)
{
	u32 i;

   if (key[KEY_ESC]) return -1;

// B button
   if (key[KEY_X]) Joy1_Data |= 0x01;
   else Joy1_Data &= 0xfe;

// A button
   if (key[KEY_Z]) Joy1_Data |= 0x02;
   else Joy1_Data &= 0xfd;

// SELECT
   if (key[KEY_A]) Joy1_Data |= 0x04;
   else Joy1_Data &= 0xfb;

// START
   if (key[KEY_S]) Joy1_Data |= 0x08;
   else Joy1_Data &= 0xf7;
    
   if (key[KEY_UP]) Joy1_Data |= 0x10;
   else Joy1_Data &= 0xef;
    
   if (key[KEY_DOWN]) Joy1_Data |= 0x20;
   else Joy1_Data &= 0xdf;
       
   if (key[KEY_LEFT]) Joy1_Data |= 0x40;
   else Joy1_Data &= 0xbf;
    
   if (key[KEY_RIGHT]) Joy1_Data |= 0x80;
   else Joy1_Data &= 0x7f;

   if (key[KEY_P]) ScLnPCyc++;
   if (key[KEY_O]) ScLnPCyc--;

   return 0;
}


void InitNES(void)
{
   if (!InitKeyboard()) exit(0);

   VideoSel = _dos_ds;

   Go_256x256();        // inicializa modo grafico

	ChangePal(0xC0, 0, 0, 0);
   ChangePal(1,0xff,0xff,0xff);	// texto


	video_base = 0xA0000+((SCREEN_Y-240)/2*SCREEN_X);

	memset(frame_buffer, 0x00, 256*240);

	memset(frame_buffer, 0xC0, 256*8);
	memset(frame_buffer+256*(224+8), 0xC0, 256*8);


	SetNESPal();
   RST_6502();

	a6502.TrapBadOpCodes = nTrue;
}


void EndNES(void)
{
   SetVideo(0x03);                     // Volta para modo texto
   RemoveKeyboard();
}


int LoadPalette(void)
{
   FILE *pal_file;
   long byte_read;      
   char  *pal_name;
   u8  pal_buf[64*3];
   int  i, j;

   pal_name = "nespal.pal";

   if ((pal_file = fopen(pal_name, "rb")) == NULL) return 0;

   byte_read = fread(pal_buf, 0x01, 64*3, pal_file);
   if (byte_read != 64*3) { fclose(pal_file); return 0; }

   for (i=0; i<64; i++)
   {
      for (j=0; j<3; j++)
      {
         NesPal[i*4+j] = (pal_buf[i*3+j] >> 2);

      }
      NesPal[i*4+3] = 0x00;
   }

   fclose(pal_file);
   return 1;
}


void RunNES(void)
{
	int   i;
   int   irq_occur = 0;
	u32  bad_opcode = 0;
	u32  frame_skip = 0;
	u32  cont = 0;


//	G_Buf = NES_line+8;


   for (;;)
   {

		WaitVRetrace();
		if ((PPU.Ctrl_Reg_2 & 0x08) || (PPU.Ctrl_Reg_2 & 0x10)) FrameStart();
      for (i = 0; i < 240; i++)
      {

         if (Mapper == 0x04)
			{
	         if ((PPU.Ctrl_Reg_2 & 0x10) || (PPU.Ctrl_Reg_2 & 0x08))
                  MMC3.IRQ_COUNTER--;

            if (MMC3.IRQ_COUNTER == 0)
            {
	            if (MMC3.IRQ_ENABLE) irq_occur = 0x01;
            }
			}

         a6502.Cycles += ScLnPCyc;
         if (bad_opcode = RUN_6502())
			{
				return;
			// fazer o tratamento de bad opcode
			}
         if (Mapper == 0x04)
			{
	         if (irq_occur)
            {
		         IRQ_6502();
               irq_occur = 0x00;
            }
			}

         current_scanline = i;

			// must be OR (sprite disabled - Ninja Gaiden 3)
			if ((PPU.Ctrl_Reg_2 & 0x08) || (PPU.Ctrl_Reg_2 & 0x10)) ScanlineStart();


			if (cont == frame_skip)
			{
				G_Buf = NES_line+8;

				if (PPU.Ctrl_Reg_2 & 0x08) RenderBg(); else ClearBg();
				if (PPU.Ctrl_Reg_2 & 0x10) 
				{
					ClearSpr();
					RenderSprite();
				}

//				if ((i>7) && (i<232))
//				{
//					WaitHRetrace();
					DrawScanline();
//				}
			}
			else
			if (!(PPU.Status_Reg & 0x40) && ((current_scanline >= PPU.SPR_RAM[0]+1) && 
														 (current_scanline<(PPU.SPR_RAM[0]+1+SPR_Size))) )
			{
				G_Buf = DummyBuf+8;

				if (PPU.Ctrl_Reg_2 & 0x08) RenderBg();
				else ClearBg();
				if (PPU.Ctrl_Reg_2 & 0x10)
				{
					ClearSpr();
					RenderSprite(); 
				}
			}


			if ((PPU.Ctrl_Reg_2 & 0x08) || (PPU.Ctrl_Reg_2 & 0x10)) ScanlineEnd();

		}


		for (i=240; i<262; i++)
		{

         if (i == 241)
         {
            PPU.Status_Reg |= 0x80;           // in Vblank
				continue;
         }
         else if (i == 244)
         {
            if (PPU.Ctrl_Reg_1 & 0x80) NMI_6502();
         }
         else if (i == 261)
         {
            PPU.Status_Reg = 0x00;           // not in Vblank
         }

         
			a6502.Cycles += ScLnPCyc;
			if (bad_opcode = RUN_6502())
			{
				return;
			// fazer o tratamento de bad opcode
			}

      }

      if (Get_User_Enter()) break;

		cont++;
		if (cont > frame_skip) cont = 0;

   }
}


/*==========================================================================*/
/*     INTERFACE FUNCTIONS                                                  */
/*==========================================================================*/

/*==========================================================================*/
/*     INTERFACE CLASS BODIES                                               */
/*==========================================================================*/

/*==========================================================================*/
/*     MAIN                                                                 */
/*==========================================================================*/
int main(int argc, char *argv[])
{
	int v;

   char *Start_Errors[ ] =
   {  "                 ",
      "File not found!!!",
      "iNES header not found!!!",
      "An invalid PRG page number found!!!",
      "An invalid CHR page number found!!!",
      "Sorry, this mapper is not supported!!!",
      "6502 memory alocation error!!!",
      "PPU memory alocation error!!!",
      "PRG ROM loading error!!!",
      "CHR ROM loading error!!!"
   };


   printf("NESac v1.0  (c) 2000 -  Programmed by _aLe() -  NASM & GCC rock!!!\n");
   printf("\n");

   if (LoadPalette() == 0)
   {
      printf("Palette file not found... - A built-in was loaded!\n");
      printf ("\n");
   }

   if (argc != 2)
   {
      printf("No file specified!!! -  Use: NESac <ROMFILE>\n");
      return -1;
   }

   if ((v = LoadROM(argv[1])) != 0)
   {
      puts(Start_Errors[v]); printf("Aborting the emulation...\n");
      return -1;
   }
   else
   {
      printf("\n");
      printf("Press any key to start the emulation...\n");
      getch();
   }



	InitNES();
   RunNES();
	EndNES();

	return 0;
}


/*==========================================================================*/
/*     END OF MODULE                                                        */
/*==========================================================================*/
