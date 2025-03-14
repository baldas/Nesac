#ifndef _IO_H
#define _IO_H

#include "..\include\types.h"



extern u32 DoIsetPal, Joy1_Data, Mapper;



extern u8 ReadIO(u32 address);
extern void WriteIO(u32 address, u8 data);

#endif  /* _IO_H */

