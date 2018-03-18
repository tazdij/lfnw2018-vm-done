; Should the source fix endianess?
; FEED = ED FE 00 00 ; in memory

MOV     @x00000100  x0000FEED
MOV     @x00000104  @x00000100  x00000004
MOV     R1  x0000FEED   ; In memory 00 00 ED FE
ADDI    R1  x00000002   ; Result of adding R1 and literal 2 stored in R1


; JUMP ops only update the PC Register with a new address
; CALL sets up a Stack Frame and a Return semantic

main_loop:
  MOV       R1  x000000FF
  MOV       R2  x00000001
  ADDI      R2  R1

my_label:
  POP       R2
  POP       R3
  
  ADDI      R2  R3
  
  PUSH      R2
  
  CMP       R2  11
  JRE       main_loop


HALT
