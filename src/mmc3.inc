

;****************************************************
;* iNES MMC #3 (CNROM)                              *
;****************************************************

[section .text]

_Mapper_03:

   mov   eax, [esp+8]

;;;   or    eax, dword [_a6502_Base+ebx]       ; NESDEV (add it???)

   and   eax, dword [_VROMPages]       ; page to switch

   mov   ebx, [_VROMBank+eax*4]        ; 8k page
   mov   [_pBank0x0000], ebx
   add   ebx, 0x0400
   mov   [_pBank0x0400], ebx
   add   ebx, 0x0400
   mov   [_pBank0x0800], ebx
   add   ebx, 0x0400
   mov   [_pBank0x0C00], ebx
   add   ebx, 0x0400
   mov   [_pBank0x1000], ebx
   add   ebx, 0x0400
   mov   [_pBank0x1400], ebx
   add   ebx, 0x0400
   mov   [_pBank0x1800], ebx
   add   ebx, 0x0400
   mov   [_pBank0x1C00], ebx

ret


