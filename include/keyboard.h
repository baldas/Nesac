/*****************************************************************************
                        *** KEYBOARD HEADER FILE ***

	This module provides some keyboard interface routines (see interface
functions prototypes at the end of this file).
	It's MUCH better than BIOS one, and provides mechanism to detect simultaneous
key presses. It's based on the Allegro one, but has several differences and it's
more compact too. It's NOT just a copy of that one, as you can realize if you
surf through the source.

*****************************************************************************/

#ifndef _KEYBOARD_H
#define _KEYBOARD_H

/*==========================================================================*/
/*     SYSTEM HEADERS                                                       */
/*==========================================================================*/

/*==========================================================================*/
/*     LOCAL HEADERS                                                        */
/*==========================================================================*/

/*==========================================================================*/
/*     INTERFACE DEFINITIONS / ENUMERATIONS / TYPEDEFS                      */
/*==========================================================================*/

// keyboard module special definitions
typedef unsigned char  sU8;
typedef unsigned short sU16;
typedef unsigned long  sU32;
typedef signed   char  sS8;
typedef char			  sChar;
typedef signed   short sS16;
typedef signed   long  sS32;

#ifndef __cplusplus
typedef enum{false=0, true=1} bool;
#endif


// keyboard scancodes definition
// A = Alphanumeric keys / F = Functions keys / N = Numeric keys
#define KEY_ESC               0x01                             /* F */
#define KEY_1                 0x02           /* or  !  */      /* A */
#define KEY_2                 0x03           /* or  @  */      /* A */
#define KEY_3                 0x04           /* or  #  */      /* A */
#define KEY_4                 0x05           /* or  $  */      /* A */
#define KEY_5                 0x06           /* or  %  */      /* A */
#define KEY_6                 0x07           /* or  ^  */      /* A */
#define KEY_7                 0x08           /* or  &  */      /* A */
#define KEY_8                 0x09           /* or  *  */      /* A */
#define KEY_9                 0x0A           /* or  (  */      /* A */
#define KEY_0                 0x0B           /* or  )  */      /* A */
#define KEY_MINUS             0x0C           /* or  _  */      /* A */
#define KEY_EQUAL             0x0D           /* or  +  */      /* A */
#define KEY_BACKSPACE         0x0E                             /* A */
#define KEY_TAB               0x0F                             /* A */ 
#define KEY_Q                 0x10                             /* A */
#define KEY_W                 0x11                             /* A */
#define KEY_E                 0x12                             /* A */
#define KEY_R                 0x13                             /* A */
#define KEY_T                 0x14                             /* A */
#define KEY_Y                 0x15                             /* A */
#define KEY_U                 0x16                             /* A */
#define KEY_I                 0x17                             /* A */
#define KEY_O                 0x18                             /* A */
#define KEY_P                 0x19                             /* A */
#define KEY_OPENBRACE         0x1A           /* or  {  */      /* A */
#define KEY_CLOSEBRACE        0x1B           /* or  }  */      /* A */
#define KEY_ENTER             0x1C                             /* A */
#define KEY_LCTRL             0x1D                             /* A */
#define KEY_A                 0x1E                             /* A */
#define KEY_S                 0x1F                             /* A */
#define KEY_D                 0x20                             /* A */
#define KEY_F                 0x21                             /* A */
#define KEY_G                 0x22                             /* A */
#define KEY_H                 0x23                             /* A */
#define KEY_J                 0x24                             /* A */
#define KEY_K                 0x25                             /* A */
#define KEY_L                 0x26                             /* A */
#define KEY_COLON             0x27           /* or  :  */      /* A */
#define KEY_QUOTE             0x28           /* or  "  */      /* A */
#define KEY_TILDE             0x29           /* or  ~  */      /* A */
#define KEY_LSHIFT            0x2A                             /* A */   
#define KEY_BACKSLASH         0x2B           /* or  |  */      /* A */
#define KEY_Z                 0x2C                             /* A */
#define KEY_X                 0x2D                             /* A */
#define KEY_C                 0x2E                             /* A */
#define KEY_V                 0x2F                             /* A */
#define KEY_B                 0x30                             /* A */
#define KEY_N                 0x31                             /* A */
#define KEY_M                 0x32                             /* A */
#define KEY_COMMA             0x33           /* or  <  */      /* A */
#define KEY_STOP              0x34           /* or  >  */      /* A */
#define KEY_SLASH             0x35           /* or  ?  */      /* A */
#define KEY_RSHIFT            0x36                             /* A */
#define KEY_ASTERISK          0x37                             /* N */
#define KEY_LALT              0x38                             /* A */
#define KEY_SPACE             0x39                             /* A */
#define KEY_CAPSLOCK          0x3A                             /* A */
#define KEY_F1                0x3B                             /* F */
#define KEY_F2                0x3C                             /* F */
#define KEY_F3                0x3D                             /* F */
#define KEY_F4                0x3E                             /* F */
#define KEY_F5                0x3F                             /* F */
#define KEY_F6                0x40                             /* F */
#define KEY_F7                0x41                             /* F */
#define KEY_F8                0x42                             /* F */
#define KEY_F9                0x43                             /* F */
#define KEY_F10               0x44                             /* F */
#define KEY_NUMLOCK           0x45                             /* N */
#define KEY_SCRLOCK           0x46                             /* F */
#define KEY_7_PAD             0x47           /* or  7  */      /* N */
#define KEY_8_PAD             0x48                             /* N */
#define KEY_9_PAD             0x49           /* or  9  */      /* N */
#define KEY_MINUS_PAD         0x4A                             /* N */
#define KEY_4_PAD             0x4B                             /* N */
#define KEY_5_PAD             0x4C                             /* N */
#define KEY_6_PAD             0x4D                             /* N */
#define KEY_PLUS_PAD          0x4E                             /* N */
#define KEY_1_PAD             0x4F           /* or  1  */      /* N */
#define KEY_2_PAD             0x50                             /* N */
#define KEY_3_PAD             0x51           /* or  3  */      /* N */
#define KEY_0_PAD             0x52           /* or  0  */      /* N */
#define KEY_DEL_PAD           0x53           /* or  .  */      /* N */                  
#define KEY_F11               0x57                             /* F */
#define KEY_F12               0x58                             /* F */
// extended keys
#define KEY_NENTER            0x60           // numeric ENTER
#define KEY_RCTRL             0x61
#define KEY_NSLASH            0x62           // numeric SLASH
#define KEY_RALT              0x63
#define KEY_HOME              0x64
#define KEY_UP                0x65
#define KEY_PGUP              0x66
#define KEY_LEFT              0x67
#define KEY_RIGHT             0x68
#define KEY_END               0x69
#define KEY_DOWN              0x6A
#define KEY_PGDN              0x6B
#define KEY_INSERT            0x6C
#define KEY_DEL               0x6D

// status key definition
#define SCRLOCK               0x01
#define NUMLOCK               0x02
#define CAPSLOCK              0x04

/* 
	Use these macros to verify if the states keys are turned on or off.

Ex:
	if (IS_SCRLOCK) printf("The ScrollLock key is turned on!!!");
*/
#define IS_SCRLOCK  (status_key & SCRLOCK)
#define IS_NUMLOCK  (status_key & NUMLOCK)
#define IS_CAPSLOCK (status_key & CAPSLOCK)



/*==========================================================================*/
/*     INTERFACE DATA (PROTOTYPE)                                           */
/*==========================================================================*/
/*
	Keys status ordered by scan codes indexes. Use the above KEY_xxx definitions
to access a key:		
									false => key released;
									true  => key pressed;
 
	Ex:  key[KEY_A] (if true the A key is pressed)

 BEWARE: NEVER (I said NEVER!!!) use this variable in assigment (like 
 key[KEY_A] = false, as this is used in the keyboard handler.
*/
extern volatile bool key[128];

// don't play directly with this. See the macros IS_xxx above!!!
extern volatile sU8 status_key;



#ifdef __cplusplus			// it's a C interface
extern "C" {
#endif

/*==========================================================================*/
/*     INTERFACE CLASS / FUNCTIONS PROTOTYPES                               */
/*==========================================================================*/
// This is the function you MUST call before any other keyboard manipulation.
extern bool InitKeyboard(void);

// This will clear the keyboard buffer
extern void ClearKeyboardBuffer(void);

// Returns true if there's a key in the buffer
extern bool KeyPressed(void);

/*
	This will take a key from buffer. If there's no key in the buffer, then
it will wait for one. This is like Allegro one: The high byte of the word
returned contain the keyboard scancode, and the low byte contain the ascii
code. 
BEWARE: the ascii code is valid ONLY if the key is one that can be printed
on the screen (ie. '0'..'9', 'a'..'z','A'..'Z', symbolsl like !#$... you know...).
This way is better for doing user input management.
*/
extern sU16 ReadKey(void);

// This MUST be called in order to restore the previous handler
extern void RemoveKeyboard(void);


#ifdef __cplusplus
}
#endif

#endif  /* _KEYBOARD_H */
