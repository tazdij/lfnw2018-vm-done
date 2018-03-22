program lfnw;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  lfnwVM;
var vm : PVMState;

begin
  vm := VM_NewState(512, 512, ParamStr(1));

  VM_Run(vm);

  VM_FreeState(vm);
end.

