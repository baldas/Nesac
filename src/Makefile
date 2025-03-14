


home = c:/nesac/
src_dir = $(home)src/
inc_dir = $(home)include/


OBJS = 6502.o ppu.o io.o lrom.o video.o keyboard.o key.o mmc.o

$(home)nesac.exe : nesac.exe 
	copy nesac.exe $(subst /,\,$(home)nesac.exe)

nesac.exe : nesac.c $(OBJS) $(inc_dir)6502.h $(inc_dir)IO.h $(inc_dir)keyboard.h $(inc_dir)ppu.h $(inc_dir)video.h $(inc_dir)types.h
	gcc -onesac.exe nesac.c $(OBJS)

6502.o : 6502.asm $(inc_dir)io.ha $(inc_dir)mmc.ha
	nasmw -fcoff 6502.asm

ppu.o : ppu.asm $(inc_dir)ppu.h $(inc_dir)io.ha
	nasmw -fcoff ppu.asm

io.o : io.asm $(inc_dir)6502.ha $(inc_dir)ppu.ha
	nasmw -fcoff io.asm

mmc.o : mmc.asm $(inc_dir)6502.ha $(inc_dir)ppu.ha
	nasmw -fcoff mmc.asm

video.o : video.asm
	nasmw -fcoff video.asm

lrom.o : lrom.c $(inc_dir)6502.h $(inc_dir)io.h $(inc_dir)types.h
	gcc -c lrom.c

key.o : keyboard.asm
	nasmw -fcoff keyboard.asm -okey.o

keyboard.o : keyboard.c $(inc_dir)keyboard.h $(inc_dir)types.h
	gcc -c keyboard.c

