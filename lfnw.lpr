program lfnw;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, lfnwVM
  { you can add units after this };
var vm : PVMState;







begin
  WriteLn('Hello World');

  vm := VM_NewState(65536, 1024, './temp.bin');

  VM_Run(vm);

  VM_FreeState(vm);
end.

