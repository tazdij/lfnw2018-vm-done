; Should the source fix endianess?
; xFEED = ED FE 00 00 ; in memory

MOV     @x00010000  xFEED0000
MOV     @x04010000  @x00010000  x04000000
HALT
