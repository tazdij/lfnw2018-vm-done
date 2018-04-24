unit lfnwVM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PByteArray = ^ByteArray;
  ByteArray = Array of Byte;

  EVMType = (EVMInt, EVMSingle, EVMDouble, EVMBool, EVMByte, EVMWord, EVMString);

  PVMState = ^VMState;

  VMOpFunc = procedure(state : PVMState); (* Procedures might be a little faster *)

  VMState = record
    SM : ByteArray; (* Stack Memory *)
    PM : ByteArray; (* Program Memory *)
    HM : ByteArray; (* Heap Memory *)
    RM : Array[0..255] of Byte; (* Register Memory *)

    SP : LongInt; (* Stack Pointer *)
    FP : LongInt; (* Frame Pointer *)
    PC : LongInt; (* Program Counter *)

    (* Handlers for all OpCodes in our VM *)
    OpCodeHandlers : Array[0..255] of VMOpFunc;
  end;



function VM_NewState(StackSize : Cardinal; HeapSize : Cardinal; CodeFile : AnsiString) : PVMState;
procedure VM_FreeState(state : PVMState);

procedure VM_Run(state : PVMState);


procedure VM_RegisterOpHandler(state : PVMState; code : Byte; func : VMOpFunc);

implementation

(* Private Op Handlers *)
(*
  Naming convention for handlers
    I - LongInteger (Little Endian)
    B - Byte
    C - Char* (C String)
    R - Register
    H - Heap Address (Little Endian)
    A - Program Memory Address (Little Endian)
    S - Stack Operation
    O - Output

    Modifiers
    i - indirection
    l - literal
    x - array, multiple of type

*)

(* HALT *)
procedure VM_OpHALT(state : PVMState);
begin
  state^.PC := Length(state^.PM);
end;

(* Move a constant integer into a Register *)
(* MOV   R1  xFEED0101 *)
procedure VM_OpMOV_RIl(state : PVMState);
var reg : Byte;
begin
  Move(state^.PM[state^.PC + 1], reg, 1);
  Move(state^.PM[state^.PC + 2], state^.RM[reg], 4);
  state^.PC := state^.PC + 7;
end;

(* Move a value in a Register into another Register *)
(* MOV   R1  R2 *)
procedure VM_OpMOV_RR(state : PVMState);
begin

end;

(* Move a constant integer into a Heap Memory Address *)
(* MOV   @x00010000  x00000001 *)
procedure VM_OpMOV_HIl(state : PVMState);
var addr : LongInt;
    //tmp : LongInt;
begin
  Move(state^.PM[state^.PC + 1], addr, 4);
  Move(state^.PM[state^.PC + 5], state^.HM[addr], 4);
  //Move(state^.PM[state^.PC + 5], tmp, 4);
  //WriteLn(addr, ' = ', tmp);
  state^.PC := state^.PC + 9;
end;

(* Copy from Heap Memory into Heap Memory for x num bytes *)
(* MOV   @x00010000  @x04010000  x04000000 *)
procedure VM_OpMOV_HHBx(state : PVMState);
var addrDest : LongInt;
    addrSrc : LongInt;
    count : LongInt;
begin
  Move(state^.PM[state^.PC + 1], addrDest, 4);
  Move(state^.PM[state^.PC + 5], addrSrc, 4);
  Move(state^.PM[state^.PC + 9], count, 4);

  Move(state^.HM[addrSrc], state^.HM[addrDest], count);

  state^.PC := state^.PC + 13;
end;

(* Output from Heap Memory to Console Integer *)
(* PRINTI @x00010000 *)
procedure VM_OpPRINT_HOI(state : PVMState);
var addr : LongInt;
    val : LongInt;
begin
  Move(state^.PM[state^.PC + 1], addr, 4);
  Move(state^.HM[addr], val, 4);

  WriteLn(val);

  state^.PC := state^.PC + 5;
end;

(* Output from Heap Memory to Console Char *)
(* PRINTC @x00010000  *)
procedure VM_OpPRINT_HOC(state : PVMState);
var addr : LongInt = 0;
    val : Char = #0;
begin
  Move(state^.PM[state^.PC + 1], addr, 4);
  Move(state^.HM[addr], val, 1);

  Write(val);

  state^.PC := state^.PC + 5;
end;

(* Push a 32bit value onto the stack *)
(* PUSHI x41 *)
procedure VM_OpPUSH_Il(state : PVMState);
begin
  Move(state^.PM[state^.PC + 1], state^.SM[state^.SP], 4);
  state^.SP := state^.SP + 4;

  state^.PC := state^.PC + 5;
end;

(* Push 32bit value in the heap location *)
(* PUSHI @x00010000 *)
procedure VM_OpPUSH_HI(state : PVMState);
begin

end;

(* Push a 32bit value from a register into the stack *)
(* PUSH R2 *)
procedure VM_OpPUSH_RI(state : PVMState);
begin

end;

procedure VM_OpPOP_HI(state : PVMState);
begin

end;

procedure VM_OpPOP_RI(state : PVMState);
begin

end;

procedure VM_OpPOP_SI(state : PVMState);
begin

end;

(* CALL label_to_goto numargs *)
(* CALL x0F000000 2 *)
procedure VM_OpCALL_A(state : PVMState);
var destAddr : LongInt = 0;
    retAddr : LongInt = 0;
    prevFP : LongInt = 0;

begin
  // All arguments should have been pushed by caller already

  // Return address is next instruction
  retAddr := state^.PC + 5;

  Move(state^.PM[state^.PC + 1], state^.PC, 4); // Move the address to call into PC

  // Put the return address on the Stack
  Move(retAddr, state^.SM[state^.SP], 4);
  state^.SP := state^.SP + 4;

  // Put the previous Frame Pointer on the Stack
  Move(state^.FP, state^.SM[state^.SP], 4);
  state^.FP := state^.SP;
  state^.SP := state^.SP + 4;

end;

(* RET *)
procedure VM_OpRET(state : PVMState);
var tmpSP : LongInt;
begin
  (*
    Stack Layout
    -1 - Initial Stack Frame Pointer
    ---------------------------
    00 - arg1
    04 - arg2
    08 - return address (PC)
    0C - frame index (-1)       <--- FP = 0C
    ---------------------------
    10 - arg1
    14 - arg2
    18 - return address (PC)
    1C - frame index (0C)       <--- FP = 1C
    ---------------------------
    20 - arg1
    24 - arg2
    28 - return address (PC)
    2C - frame index (1C)       <--- FP = 2C
    30 - ______________         <--- SP = 30
    ...

    FP register contains address of the start of the current Stack Frame
    SP register contains address of top element + 1 on the stack (next available)
  *)

  (* get SP of the new top of the stack *)
  tmpSP := state^.FP;


  (* set FP to the previous frame from the value stored in current stack frame *)
  state^.FP := state^.SM[state^.SP - 4];

  (* Update program counter - according to stack *)
  state^.PC := state^.SM[state^.SP - 8];

  (* set SP to new location *)
  state^.SP := state^.FP + 4;  (* Points to Frame Pointer + 4 bytes *)

end;

(* RET x01000000 *)
procedure VM_OpRET_I(state : PVMState);
var tmpSP : LongInt;
    oriSP : LongInt;
    numRet : Byte;
    i : Byte;
begin
  Move(state^.PM[state^.PC + 1], numRet, 1);
  oriSP := state^.SP;
  tmpSP := state^.FP;

  state^.FP := state^.SM[state^.SP - (1 + numRet)]; // Return to previous StackFrame
  state^.PC := state^.SM[state^.SP - (2 + numRet)]; // Return to previous Program Code Address + 1
  state^.SP := tmpSP;

  // TODO: move results to top of stack
  //    place SP at the new TOP
  for i := 0 to numRet do
  begin

  end;


end;

(* ADD *)
procedure VM_OpADD_SI(state : PVMState);
var valA, valB : LongInt;
begin
  Move(state^.SM[state^.SP - 4], valA, 4);
  Move(state^.SM[state^.SP - 8], valB, 4);
  valA := valA + valB;
  Move(valA, state^.SM[state^.SP - 8], 4);
  state^.SP := state^.SP - 4;
  state^.PC := state^.PC + 1;
end;

procedure VM_OpADD_HSI(state : PVMState);
begin

end;

procedure VM_OpADD_HHI(state : PVMState);
begin

end;

procedure VM_OpADD_HRI(state : PVMState);
begin

end;

procedure VM_OpADD_RSI(state : PVMState);
begin

end;

function VM_NewState(StackSize : Cardinal; HeapSize : Cardinal; CodeFile : AnsiString) : PVMState;
var state : PVMState;
    f : file;
    buf : Byte = 0;
    i : Integer;
    codeLength : Int64;
begin
  Result := nil;
  New(state);

  if FileExists(CodeFile) then
  begin
    (* Open and read all file bytes into ProgramMemory *)
    AssignFile(f, CodeFile);
    Reset(f, 1);
    codeLength := FileSize(f);
    SetLength(state^.PM, codeLength);
    i := 0;
    while not EOF(f) do
    begin
      BlockRead(f, buf, 1);
      state^.PM[i] := buf;
      Inc(i);
    end;

    CloseFile(f);
  end
  else
  begin
    (* Unable to open binary file *)
    WriteLn('Unable to locate file: ', CodeFile);
    Dispose(state);
    Exit();
  end;

  SetLength(state^.HM, HeapSize);
  SetLength(state^.SM, StackSize);

  (* Initialize Registers *)
  state^.PC := 0;
  state^.SP := 0;
  state^.FP := -1;

  (* Register our handlers *)
  VM_RegisterOpHandler(state, 0, @VM_OpHALT);
  VM_RegisterOpHandler(state, 1, @VM_OpMOV_HIl);
  VM_RegisterOpHandler(state, 2, @VM_OpMOV_HHBx);
  VM_RegisterOpHandler(state, 3, @VM_OpMOV_RIl);
  VM_RegisterOpHandler(state, 4, @VM_OpPRINT_HOI);
  VM_RegisterOpHandler(state, 5, @VM_OpPRINT_HOC); // Special functionality, stops at first NULL (0) Byte

  VM_RegisterOpHandler(state, 10, @VM_OpPUSH_Il);


  (* Stack Function Calls *)
  VM_RegisterOpHandler(state, 100, @VM_OpCALL_A);
  VM_RegisterOpHandler(state, 101, @VM_OpRET);
  VM_RegisterOpHandler(state, 102, @VM_OpRET_I);

  Result := state;
end;

procedure VM_FreeState(state : PVMState);
begin
  if not Assigned(state) then
     Exit();

  SetLength(state^.SM, 0);
  SetLength(state^.HM, 0);
  SetLength(state^.PM, 0);

  Dispose(state);
end;

procedure VM_Run(state : PVMState);
var IsEnd : Boolean = False;
    CurOpCode : Byte;
    i, j : Integer;
begin

  (* Check that the state is ready *)

  (* Start looping over bytes calling OpCode handlers *)
  while not IsEnd do
  begin
    CurOpCode := state^.PM[state^.PC];
    state^.OpCodeHandlers[CurOpCode](state);

    if state^.PC = Length(state^.PM) then
       IsEnd := True;
  end;

  (* Dump Heap memory to console *)
  WriteLn('');
  WriteLn('Heap Memory:');
  j := 0;
  for i := 0 to Length(state^.HM) - 1 do
  begin
    Write(HexStr(state^.HM[i], 2), ' ');

    if (i + 1) mod 8 = 0 then
    begin
      WriteLn();

      Inc(j);
      if j = 8 then
      begin
        WriteLn();
        WriteLn(i+1);
        j := 0;
      end;

    end;
  end;

  WriteLn('');
  WriteLn('Stack Memory:');
  j := 0;
  for i := 0 to Length(state^.SM) - 1 do
  begin
    Write(HexStr(state^.SM[i], 2), ' ');

    if (i + 1) mod 8 = 0 then
    begin
      WriteLn();

      Inc(j);
      if j = 8 then
      begin
        WriteLn();
        WriteLn(i+1);
        j := 0;
      end;

    end;
  end;

end;


procedure VM_RegisterOpHandler(state : PVMState; code : Byte; func : VMOpFunc);
begin
  state^.OpCodeHandlers[code] := func;
end;

end.

