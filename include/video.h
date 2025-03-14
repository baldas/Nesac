
#ifndef _VIDEO_H_
#define _VIDEO_H_


extern   u8 NesPal[];

extern volatile void SetNESPal(void);
extern volatile void WaitVRetrace(void);
extern volatile void WaitHRetrace(void);

extern volatile void PrintLetter(short X, short Y, int Index);
extern void Go_256x256(void);
extern void SetVideo(u8 mode);

extern void ChangePal(u8 entry, u8 red, u8 green, u8 blue);
extern void ToBuffer(u32 X, u32 Y, u32 Index);

extern void DrawScanline(void);

#endif /* _VIDEO_H_ */