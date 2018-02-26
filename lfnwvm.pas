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

    SP : Cardinal; (* Stack Pointer *)
    PC : Cardinal; (* Program Counter *)

    (* Handlers for all OpCodes in our VM *)
    OpCodeHandlers : Array[0..255] of VMOpFunc;
  end;



function VM_NewState(StackSize : Cardinal; HeapSize : Cardinal; CodeFile : AnsiString) : PVMState;
procedure VM_FreeState(state : PVMState);

procedure VM_Run(state : PVMState);


(* Stack handling functions *)
function VM_PopInteger(state : PVMState; index : Integer = -1) : Integer;
function VM_PopSingle(state : PVMState; index : Integer = -1) : Single;
function VM_PopDouble(state : PVMState; index: Integer = -1) : Double;
function VM_PopBool(state : PVMState; index : Integer = -1) : Boolean;
function VM_PopByte(state : PVMState; index : Integer = -1) : Byte;
function VM_PopWord(state : PVMState; index : Integer = -1) : Word;
function VM_PopString(state : PVMState; index : Integer = -1) : AnsiString;
function VM_GetStackType(state : PVMState; index : Integer = -1) : EVMType;

function VM_PushInteger(state : PVMState; val : Integer) : Integer;
function VM_PushSingle(state : PVMState; val : Single) : Integer;
function VM_PushDouble(state : PVMState; val : Double) : Integer;
function VM_PushBool(state : PVMState; val : Boolean) : Integer;
function VM_PushByte(state : PVMState; val : Byte) : Integer;
function VM_PushWord(state : PVMState; val : Word) : Integer;
function VM_PushString(state : PVMState; val : AnsiString) : Integer;


procedure VM_RegisterOpHandler(state : PVMState; code : Byte; func : VMOpFunc);

implementation

(* Private Op Handlers *)
(*
  Naming convention for handlers
    I - LongInteger (Little Endian)
    B - Byte
    R - Register
    H - Heap Address (Little Endian)
    S - Stack Operation

    Modifiers
    i - indirection
    l - literal

*)

(* HALT *)
procedure VM_OpHALT(state : PVMState);
begin
  state^.PC := Length(state^.PM);
end;

(* Move a constant integer into a Register *)
(* MOV   R1  xFEED0101 *)
procedure VM_OpMOV_RIl(state : PVMState);
begin

end;

(* Move a constant integer into a Heap Memory Address *)
(* MOV   @x00010000  x00000001 *)
procedure VM_OpMOV_HIl(state : PVMState);
var addr : LongInt;
begin
  Move(state^.PM[state^.PC + 1], addr, 4);
  Move(state^.PM[state^.PC + 5], state^.HM[addr], 4);
  state^.PC := state^.PC + 9;
end;

(* Copy from Heap Memory into Heap Memory for x num bytes *)
(* MOV   @x00010000  @x04010000  x04000000 *)
procedure VM_OpMOV_HHbi(state : PVMState);
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

  (* Register our handlers *)
  VM_RegisterOpHandler(state, 0, @VM_OpHALT);
  VM_RegisterOpHandler(state, 1, @VM_OpMOV_HIl);
  VM_RegisterOpHandler(state, 2, @VM_OpMOV_HHbi);

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

end;

function VM_PopInteger(state : PVMState; index : Integer = -1) : Integer;
begin

end;

function VM_PopSingle(state : PVMState; index : Integer = -1) : Single;
begin

end;

function VM_PopDouble(state : PVMState; index: Integer = -1) : Double;
begin

end;

function VM_PopBool(state : PVMState; index : Integer = -1) : Boolean;
begin

end;

function VM_PopByte(state : PVMState; index : Integer = -1) : Byte;
begin

end;

function VM_PopWord(state : PVMState; index : Integer = -1) : Word;
begin

end;

function VM_PopString(state : PVMState; index: Integer = -1) : AnsiString;
begin

end;

function VM_GetStackType(state : PVMState; index : Integer = -1) : EVMType;
begin


end;

function VM_PushInteger(state : PVMState; val : Integer) : Integer;
begin

end;

function VM_PushSingle(state : PVMState; val : Single) : Integer;
begin

end;

function VM_PushDouble(state : PVMState; val : Double) : Integer;
begin

end;

function VM_PushBool(state : PVMState; val : Boolean) : Integer;
begin

end;

function VM_PushByte(state : PVMState; val : Byte) : Integer;
begin

end;

function VM_PushWord(state : PVMState; val : Word) : Integer;
begin

end;

function VM_PushString(state : PVMState; val : AnsiString) : Integer;
begin

end;

procedure VM_RegisterOpHandler(state : PVMState; code : Byte; func : VMOpFunc);
begin
  state^.OpCodeHandlers[code] := func;
end;

end.

