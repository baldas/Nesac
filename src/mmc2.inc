


;****************************************************
;* iNES MMC #2 (UNROM)                              *
;****************************************************

[section .text]

_Mapper_02:

   mov   eax, [esp+8]

   and   eax, dword [_PRGPages]           ; swapp a 16k bank at 0x8000
   mov   ebx, [_PRGBank+eax*4]            
   mov   dword [_cBank0x8000], ebx
   add   ebx, 0x2000
   mov   dword [_cBank0xA000], ebx

ret
