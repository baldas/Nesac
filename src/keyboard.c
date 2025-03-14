/*****************************************************************************
                              *** KEYBOARD ***


What's New:

   version 0.01:
    Very simple:
      . wrote a wrapper routine to load the handler (in assembly);
      . wrote install and remove handler routines;
      . extended keys are partially supported, ie, it'll work if you check
        Key[KEY_UP] but you cannot say if it's from the numkeypad or not
        (because it's equal to key[KEY_8_PAD]);
      . missing:
         - control the two states keys: CapsLock and NumLock
         - light/turn off the LED's
         - support completly the extended keys (0xE0 prefix) (right CTRL and
           ALT keys)
         - support the keys: PAUSE, PRINTSCREEN
         - make a buffer and routines like C ones (getch() for example)
         - other little things...

   version 0.05:
      . implemented a circular queue keyboard buffer and a clean function;
      . implemented a function 'KeyPressed()' to check if there's a key on the
        buffer;
      . implemented a function ('ReadKey()') to get the keys from the buffer.
      . extended codes are supported now;
      . led's are correctly controlled (i think so);

	version 0.06:
		. made some 'type' stuff to adapt it to the C++ compiler;
		. for interface information see this header file (keyboard.h);

	version 0.06n:
		. adapted for use with NESac emulator;

																						 30/01/2001
                                                                version 0.06n
*****************************************************************************/

/*==========================================================================*/
/*     FEATURE TEST SWITCHES                                                */
/*==========================================================================*/


/*==========================================================================*/
/*     SYSTEM HEADERS                                                       */
/*==========================================================================*/
#include <dpmi.h>					// _go32_dpmi_lock stuff
#include <pc.h>               // outportX()/inportX() stuff
#include <sys/segments.h>     // _my_xx() stuff */
#include <sys/farptr.h>			// _farpeek()/_farpoke()
#include <go32.h>					// _dos_ds


/*==========================================================================*/
/*     LOCAL HEADERS                                                        */
/*==========================================================================*/
#include "..\include\keyboard.h"			// header file


/*==========================================================================*/
/*     INTERFACE DATA                                                       */
/*==========================================================================*/
volatile bool key[128];						// see header file
volatile sU8 status_key;


/*==========================================================================*/
/*     REQUIRED EXTERNAL REFERENCES (AVOID)                                 */
/*==========================================================================*/
#ifdef __cplusplus			
extern "C" {					// it's a C interface with assembly
#endif

// These routines are in the keyboard ASM file
extern void KeyboardWrapper(void);        // keyboard wrapper function
extern void END_KeyboardWrapper(void);    // mark the end of function

#ifdef __cplusplus
}
#endif


/*==========================================================================*/
/*     PRIVATE DEFINITIONS / ENUMERATIONS / TYPEDEFS                        */
/*==========================================================================*/
// don't let data/code be swapped to the disk
#define LOCK_DATA(x)   if (_go32_dpmi_lock_data((void *)&x, sizeof(x))) return false
#define LOCK_CODE(s,e) if (_go32_dpmi_lock_code((void *)s, (sU32)e)) return false

#define KEYBOARD_INT   9      // keyboard interrupt vector number (IRQ 1)
#define KEYBOARD_BUFFER 128	// keyboard buffer size


/*==========================================================================*/
/*     PRIVATE STRUCTURES / UNIONS                                          */
/*==========================================================================*/


/*==========================================================================*/
/*     PRIVATE DATA                                                         */
/*==========================================================================*/
static __dpmi_paddr old_keyboard_handler;    // BIOS keyboard handler

// 1k stack for keyboard handler. It should be static, but we use it in the 
// assembly wrapper
sU32 keyboard_stack[1024];                   

// The keyboard buffer was implemented as a circular queue. The buffer and
// the variables for queue management are declared bellow:
static volatile sU16 key_buffer[KEYBOARD_BUFFER];
static volatile sU8 key_buffer_tail;
static volatile sU8 key_buffer_head;

static volatile sU8 last_scancode;		// last scancode read from keyboard


/*
   The next 4 tables (normal, shift, caps, num) are used by the handler 
for converting the keyboard scancodes to the correspondent ascii code,
according to the states of the keys:

               SHIFT     pressed  -> table 'key_shift_ascii' used
               CAPSLOCK  set      -> table 'key_caps_ascii' used
               NUMLOCK   set      -> table 'key_num_ascii' used  
               None of the above  -> table 'key_normal_ascii' used
*/
static volatile sChar key_normal_ascii[128] =
{
/*         0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  A  |  B  |  C  |  D  |  E  |  F  | */
/* 0 */  0x00, 0x00, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x30, 0x2D, 0x3D, 0x00, 0x00,
/* 1 */  0x71, 0x77, 0x65, 0x72, 0x74, 0x79, 0x75, 0x69, 0x6F, 0x70, 0x5B, 0x5D, 0x00, 0x00, 0x61, 0x73,
/* 2 */  0x64, 0x66, 0x67, 0x68, 0x6A, 0x6B, 0x6C, 0x3B, 0x27, 0x60, 0x00, 0x5C, 0x7A, 0x78, 0x63, 0x76,
/* 3 */  0x62, 0x6E, 0x6D, 0x2C, 0x2E, 0x2F, 0x00, 0x2A, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
/* 4 */  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x2D, 0x00, 0x00, 0x00, 0x2B, 0x00,
/* 5 */  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
/* 6 */  0x00, 0x00, 0x2F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
/* 7 */  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};
      
static volatile sChar key_shift_ascii[128] =
{
/*         0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  A  |  B  |  C  |  D  |  E  |  F  | */
/* 0 */  0x00, 0x00, 0x21, 0x40, 0x23, 0x24, 0x25, 0x5E, 0x26, 0x2A, 0x28, 0x29, 0x5F, 0x2B, 0x00, 0x00,
/* 1 */  0x51, 0x57, 0x45, 0x52, 0x54, 0x59, 0x55, 0x49, 0x4F, 0x50, 0x7B, 0x7D, 0x00, 0x00, 0x41, 0x53,
/* 2 */  0x44, 0x46, 0x47, 0x48, 0x4A, 0x4B, 0x4C, 0x3A, 0x22, 0x7E, 0x00, 0x7C, 0x5A, 0x58, 0x43, 0x56,
/* 3 */  0x42, 0x4E, 0x4D, 0x3C, 0x3E, 0x3F, 0x00, 0x2A, 0x00, 0x20, 0x2A, 0x00, 0x00, 0x00, 0x00, 0x00,
/* 4 */  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x2D, 0x00, 0x00, 0x00, 0x2B, 0x00,
/* 5 */  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
/* 6 */  0x00, 0x00, 0x2F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
/* 7 */  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

static volatile sChar key_caps_ascii[128] =
{
/*         0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  A  |  B  |  C  |  D  |  E  |  F  | */
/* 0 */  0x00, 0x00, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x30, 0x2D, 0x3D, 0x00, 0x00,
/* 1 */  0x51, 0x57, 0x45, 0x52, 0x54, 0x59, 0x55, 0x49, 0x4F, 0x50, 0x5B, 0x5D, 0x00, 0x00, 0x41, 0x53,
/* 2 */  0x44, 0x46, 0x47, 0x48, 0x4A, 0x4B, 0x4C, 0x3B, 0x27, 0x60, 0x00, 0x5C, 0x5A, 0x58, 0x43, 0x56,
/* 3 */  0x42, 0x4E, 0x4D, 0x2C, 0x2E, 0x2F, 0x00, 0x2A, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
/* 4 */  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x2D, 0x00, 0x00, 0x00, 0x2B, 0x00,
/* 5 */  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
/* 6 */  0x00, 0x00, 0x2F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
/* 7 */  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

static volatile sChar key_num_ascii[128] =
{
/*         0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  A  |  B  |  C  |  D  |  E  |  F  | */
/* 0 */  0x00, 0x00, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x30, 0x2D, 0x3D, 0x00, 0x00,
/* 1 */  0x71, 0x77, 0x65, 0x72, 0x74, 0x79, 0x75, 0x69, 0x6F, 0x70, 0x5B, 0x5D, 0x00, 0x00, 0x61, 0x73,
/* 2 */  0x64, 0x66, 0x67, 0x68, 0x6A, 0x6B, 0x6C, 0x3B, 0x27, 0x60, 0x00, 0x5C, 0x7A, 0x78, 0x63, 0x76,
/* 3 */  0x62, 0x6E, 0x6D, 0x2C, 0x2E, 0x2F, 0x00, 0x2A, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
/* 4 */  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x37, 0x38, 0x39, 0x2D, 0x34, 0x35, 0x36, 0x2B, 0x31,
/* 5 */  0x32, 0x33, 0x30, 0x2E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
/* 6 */  0x00, 0x00, 0x2F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
/* 7 */  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};


/*
   This table is used by our handler to 'create' new scancodes for the
extended ones. A extended scancode is prefixed with the scancode '0xE0' (in
a key press or in a key release).
   Our handler identifies the extended scancodes and then uses this table to
'create' a new one, beggining with the scancode 0x60 (because the last one
definied by this module is 0x58 (see it in the header file). Byte 0x59 is used 
for all the extended keys that are not known (not in the list below).

    E0 1C = Enter
    E0 1D = Right Control
    E0 35 = / (on the numeric keypad)
    E0 38 = Right Alt
    E0 47 = Home
    E0 48 = Up
    E0 49 = Page-Up
    E0 4B = Left
    E0 4D = Right
    E0 4F = End
    E0 50 = Down
    E0 51 = Page-Down
    E0 52 = Insert
    E0 53 = Delete
*/
static volatile sU8 key_extended[128] =
{
/*         0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  A  |  B  |  C  |  D  |  E  |  F  | */
/* 0 */  0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59,
/* 1 */  0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x60, 0x61, 0x59, 0x59,
/* 2 */  0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59,
/* 3 */  0x59, 0x59, 0x59, 0x59, 0x59, 0x62, 0x59, 0x59, 0x63, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59,
/* 4 */  0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x64, 0x65, 0x66, 0x59, 0x67, 0x59, 0x68, 0x59, 0x69,
/* 5 */  0x6A, 0x6B, 0x6C, 0x6D, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59,
/* 6 */  0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59,
/* 7 */  0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59 
};


/*==========================================================================*/
/*     PRIVATE CLASS / FUNCTIONS PROTOTYPES                                 */
/*==========================================================================*/
#ifdef __cplusplus
extern "C" {					// it's a C interface with assembly
#endif

// This is the main handler (it should be static, be we need it in the ASM
// file, because it's from there that it will be called.
void KeyboardHandler(void);

#ifdef __cplusplus
}
#endif


static void InstallKeyboardHandler(void);		// install the handler
static void UpdateLeds(void);						// play with the keyboard leds


/*==========================================================================*/
/*     PRIVATE CLASS / FUNCTIONS IMPLEMENTATION                             */
/*==========================================================================*/
/*
	HOW THE HANDLER WORKS:

	A hardware handler is called whenever the hardware tells the PIC he wants
it to be called. When this happens, the PIC 'chooses' an address in the PC
memory, based on the IRQ triggered and in the vector table. The control is then
passed to the handler and now it's responsible for managing correctly the 
hardware.
	In case of the keyboard, whenever a user press/release a key, the keyboard 
triggers the IRQ1 on the PIC's, accessing the address in the vector table at 
position 9 (known as INT 9). In our case this routine is the one in the ASM
file (KeyboardWrapper()), because we cannot do such one in a normal C file. This
wrapper saves the microprocessor registers needed, set a stack to the interrupt
and then pass the control to this C/C++ routine.
	What it does, then, is take from the keyboard the scancode, treat it in some
way, and when finished, signals an 'END-OF-INTERRUPT' (EOI) to the PIC by 
writing 0x20 at port 0x20.
	For more information see the doc "kbguide.txt" in the docs directory.
*/
void KeyboardHandler(void)
{
   sU8 scan_code;						// keyboard key scan code to be read and ...
   sChar ascii_code;					// it's correspondent ascii code

   scan_code = inportb(0x60);		// take the key scancode from keyboard

// take care here: 0xD8 is the highest scancode used by this module (definied
// in the header file) excluding the extended keys, of course. If it's
// incremented in future, 0xD9 must be changed bellow!!!
   if (scan_code < 0xD9)			// process only known keys
   {
	// when a key is pressed, bit 7 is reset (0). If released, bit 7 is set (1).
      if (!(scan_code & 0x80))	// treat a key pressed
      {
         /********** 1st section - treat the scancode ************/
         switch (scan_code)	// switch to treat the leds keys
         {
         case KEY_CAPSLOCK:
            if (last_scancode != KEY_CAPSLOCK)  // avoid typematic rate
            {
               status_key ^= CAPSLOCK;       // switch
               UpdateLeds();
            }
            break;

         case KEY_NUMLOCK:
            if (last_scancode != KEY_NUMLOCK)
            {
               status_key ^= NUMLOCK;
               UpdateLeds();
            }
            break;

         case KEY_SCRLOCK:
            if (last_scancode != KEY_SCRLOCK)
            {
               status_key ^= SCRLOCK;
               UpdateLeds();
            }
            break;
         }

         if (last_scancode == 0xE0)				// use extended keys if it's the case
            scan_code = key_extended[scan_code];

         key[scan_code] = true;					// flag keypressed


         /********** 2nd section - treat the ascii code ************/
         if (key[KEY_LSHIFT] || key[KEY_RSHIFT])		// 1st priority: SHIFT keys
         {
            ascii_code = key_shift_ascii[scan_code];
         }
         else if (status_key & NUMLOCK)					// 2nd priority: NUMLOCK
         {
            ascii_code = key_num_ascii[scan_code];
         }
         else if (status_key & CAPSLOCK)					// 3rd priority: CAPSLOCK
         {
            ascii_code = key_caps_ascii[scan_code];
         }
         else ascii_code = key_normal_ascii[scan_code];		// do it in the normal way

         /****** 3rd section - save ascii and scancode in buffer ******/
		// scancode stays in the high byte of the word, while ascii stays at low byte
         key_buffer[key_buffer_head++] = (scan_code << 8) | ascii_code;

		// the buffer is a circular queue
         key_buffer_head %= KEYBOARD_BUFFER;    // if (key_buffer_head >= KEYBOARD_BUFFER)
                                                //    key_buffer_head = 0;
         if (key_buffer_head == key_buffer_tail)
         {
            key_buffer_tail++;
            key_buffer_tail %= KEYBOARD_BUFFER;  // if (key_buffer_tail >= KEYBOARD_BUFFER)
         }                                       //    key_buffer_tail = 0;
      }
      else  // key released
      {
//         scan_code &= 0x7F;       <--- DON'T DO IT!!!!!!!!!

         if (last_scancode == 0xE0)
            key[key_extended[scan_code&0x7F]] = false;
         else
            key[scan_code&0x7F] = false;		// flag key released
      }
   }
/*
   I really don't know if we need to ACK the keyboard controller that the
scancode was read (by reading/writing from/to port 0x61. Some handlers don't
do it (allegro and that for watcom C++) and others do.
*/
   last_scancode = scan_code;						// save last scancode
   outportb(0x20, 0x20);							// Ack to the PIT
}
void END_KeyboardHandler(void) { }			// mark the end of function to lock it



/* 
	This routine is responsible for initialising handler variables, saving the previous
handler, and starting up our new one.
 */
static void InstallKeyboardHandler(void)
{
   __dpmi_paddr pm_addr;			// our protected mode address structure

   pm_addr.selector = _my_cs();						// fill it with CS value and ...
   pm_addr.offset32 = (sU32)KeyboardWrapper;		// ... address of our handler

// clear BIOS keyboard leds variables (turn them off)
   _farpokeb(_dos_ds, 0x417, _farpeekb(_dos_ds, 0x417) & 0x8F);
   _farpokeb(_dos_ds, 0x418, _farpeekb(_dos_ds, 0x418) & 0x8F);

// clear our keyboard buffer
   ClearKeyboardBuffer();

// save the BIOS keyboard handler address and ...
   __dpmi_get_protected_mode_interrupt_vector(KEYBOARD_INT,
                                              &old_keyboard_handler);

// ... set our own keyboard handler
   __dpmi_set_protected_mode_interrupt_vector(KEYBOARD_INT, &pm_addr);

   status_key = 0;
   UpdateLeds();				// update leds (in this case, turn them off)
}


// Should update the leds, based on the variable 'status_key'
static void UpdateLeds(void)
{
// I'm not sure if this is the correct way of handling the leds. I've seen in other files
// (Allegro per example) that they don't stand in the loop bellow indefiniedly. But, in
// other files (see the 'kbguide.txt' in the docs directory) they really do this way.

	while (inportb(0x64) & 0x02);
   outportb(0x60, 0xED);					// 1st byte -> tell keyboard to change leds

	while (inportb(0x64) & 0x02);
   outportb(0x60, status_key);			// 2nd byte -> tell keyboard which one to change
/*
	the format of the status_key is as follows:

   bit 0 = Scroll Lock			(0=Off 1=On)
		 1 = Num Lock
		 2 = Caps Lock
    rest = 0
*/

/* wait for keyboard read signal */
//   while (inportb(0x64) & 0x01);			// this is ACK sent by the keyboard and it's
//   if (inportb(0x60) != 0xFA) return;	// not handled by now
}
void END_UpdateLeds(void) { }


/*==========================================================================*/
/*     INTERFACE FUNCTIONS                                                  */
/*==========================================================================*/
/*
	This is the function you MUST call before any other keyboard manipulation.
	What it does is lock all the data/code used by the handler. This is done
because, in other case, the dpmi host could swap it to disk (virtual memory)
what should create a disaster!!!
*/
bool InitKeyboard(void)
{
// first we lock all code/data/stack that the handler could touch
   LOCK_DATA(key);
   LOCK_DATA(status_key);
   LOCK_DATA(keyboard_stack);
   LOCK_DATA(key_buffer);
   LOCK_DATA(key_buffer_tail);
   LOCK_DATA(key_buffer_head);
   LOCK_DATA(last_scancode);
   LOCK_DATA(key_normal_ascii);
   LOCK_DATA(key_shift_ascii);
   LOCK_DATA(key_caps_ascii);
   LOCK_DATA(key_num_ascii);
   LOCK_DATA(key_extended);
	LOCK_CODE(KeyboardHandler, (sU32)END_KeyboardHandler-(sU32)KeyboardHandler);
	LOCK_CODE(KeyboardWrapper, (sU32)END_KeyboardWrapper-(sU32)KeyboardWrapper);
   LOCK_CODE(UpdateLeds, (sU32)END_UpdateLeds-(sU32)UpdateLeds);

// and then install the handler
   InstallKeyboardHandler();
   return true;
}


// This will clear the keyboard buffer
void ClearKeyboardBuffer(void)
{
	sU8 i;
   asm volatile ("cli");

   last_scancode = 0;
   key_buffer_tail = 0;
   key_buffer_head = 0;

   for (i=0; i<128; i++)
      key[i] = false;

   asm volatile ("sti");
}


// Returns true if there's a key in the buffer
bool KeyPressed(void)
{
   return (key_buffer_tail != key_buffer_head);
}


/*
	This will take a key from buffer. If there's no key in the buffer, then
it will wait for one. This is like Allegro one: The high byte of the word
returned contain the keyboard scancode, and the low byte contain the ascii
code. 
BEWARE: the ascii code is valid ONLY if the key is one that can be printed
on the screen (ie. '0'..'9', 'a'..'z','A'..'Z', symbolsl like !#$... you know...).
This way is better for doing user input management.
*/
sU16 ReadKey(void)
{
   sU16 key;

   while (key_buffer_tail == key_buffer_head);	// wait for a key

   asm volatile ("cli");
   key = key_buffer[key_buffer_tail++];

   key_buffer_tail %= KEYBOARD_BUFFER;  // if (key_buffer_tail >= KEYBOARD_BUFFER)
                                        //    key_buffer_tail = 0;
   asm volatile ("sti");

   return key;
}


// This MUST be called in order to restore the previous handler
void RemoveKeyboard(void)
{
   status_key = 0;
   UpdateLeds();							// turn off the lights

// just restore the BIOS handler
   __dpmi_set_protected_mode_interrupt_vector(KEYBOARD_INT,
                                              &old_keyboard_handler);
}


/*==========================================================================*/
/*     INTERFACE CLASS BODIES                                               */
/*==========================================================================*/

/*==========================================================================*/
/*     END OF MODULE                                                        */
/*==========================================================================*/
