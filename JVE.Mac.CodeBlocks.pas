{*******************************************************}
{                                                       }
{     Implementation of Objective-C Code Blocks         }
{                                                       }
{       Copyright(c) 2017 TamoSoft Limited              }
{                                                       }
{*******************************************************}

{
LICENSE:

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

You may not use the Software in any projects published under viral licenses,
including, but not limited to, GNU GPL.

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE
}
//USAGE EXAMPLE
//
//    FSaveFile :=TNSSavePanel.Wrap(TNSSavePanel.OCClass.savePanel);
//    NSWin := WindowHandleToPlatform(Screen.ActiveForm.Handle).Wnd;
//    objc_msgSendP2(
//                   (FSaveFile as ILocalObject).GetObjectID,
//                   sel_getUid(PAnsiChar('beginSheetModalForWindow:completionHandler:')),
//                   (NSWin as ILocalObject).GetObjectID,
//                   TObjCBlock.CreateBlockWithProcedure(
//                          procedure (p1: NSInteger)
//                          begin
//                            if p1 = 0
//                              then ShowMessage ('Cancel')
//                              else ShowMessage ('OK');
//                           end)
//                          );

unit JVE.Mac.CodeBlocks;

interface

{$IF Defined(MACOS)}

uses System.SysUtils, Macapi.ObjectiveC, Macapi.Helpers, Macapi.ObjCRuntime,
  {$IF Defined(IOS)} iOSapi.Foundation, iOSapi.CocoaTypes;
  {$ELSE} Macapi.Foundation, Macapi.CocoaTypes; {$ENDIF}

type
  TProc1 = TProc;
  TProc2 = TProc<pointer>;
  TProc3 = TProc<pointer, pointer>;
  TProc4 = TProc<pointer, pointer, pointer>;
  TProc5 = TProc<pointer, pointer, pointer, pointer>;
  TProc6 = TProc<NSInteger>;
  TProc7 = TFunc<NSRect, boolean>;

  TProcType = (ptNone, pt1, pt2, pt3, pt4, pt5, pt6, pt7);

  TObjCBlock = record
   private
     class function CreateBlockWithCFunc(const aTProc: TProc; const aType: TProcType): pointer; static;
   public
     class function CreateBlockWithProcedure(const aProc: TProc1): pointer; overload; static;
     class function CreateBlockWithProcedure(const aProc: TProc2): pointer; overload; static;
     class function CreateBlockWithProcedure(const aProc: TProc3): pointer; overload; static;
     class function CreateBlockWithProcedure(const aProc: TProc4): pointer; overload; static;
     class function CreateBlockWithProcedure(const aProc: TProc5): pointer; overload; static;
     class function CreateBlockWithProcedure(const aProc: TProc6): pointer; overload; static;
     class function CreateBlockWithProcedure(const aProc: TProc7): pointer; overload; static;
  end;

implementation

type
  Block_Descriptor = packed record
    Reserved: NativeUint;
    Size: NativeUint;
    copy_helper: pointer;
    dispose_helper: pointer;
  end;
  PBlock_Descriptor = ^Block_Descriptor;

  Block_Literal = packed record
    Isa: pointer;
    Flags: integer;
    Reserved: integer;
    Invoke: pointer;
    Descriptor: PBlock_Descriptor;
  end;
  PBlock_Literal = ^Block_Literal;

  TBlockInfo = packed record
     BlockStructure: PBlock_Literal;
     LocProc: TProc;
     ProcType: TProcType;
  end;
  PBlockInfo = ^TBlockInfo;

  TObjCBlockList = class (TObject)
  private
    FBlockList: TArray<TBlockInfo>;
    procedure ClearAllBlocks;
  public
    constructor Create;
    destructor Destroy; override;
    function AddNewBlock(const aTProc: TProc; const aType: TProcType): pointer;
    function FindMatchingBlock(const aCurrBlock: pointer): integer;
    procedure ClearBlock(const idx: integer);
    property BlockList: TArray<TBlockInfo> read FBlockList ;
  end;

var
  BlockObj: TObjCBlockList;

function InvokeCallback(aNSBlock, p1, p2, p3, p4: pointer): pointer; cdecl;
var
  i: integer;
  aRect: NSRect;
begin
  result:= nil;
  if Assigned(BlockObj) then
  begin
    TMonitor.Enter(BlockObj);
    try
      i:= BlockObj.FindMatchingBlock(aNSBlock);
      if i >= 0 then
      begin
        case  BlockObj.BlockList[i].ProcType of
          TProcType.pt1: TProc1(BlockObj.BlockList[i].LocProc)();
          TProcType.pt2: TProc2(BlockObj.BlockList[i].LocProc)(p1);
          TProcType.pt3: TProc3(BlockObj.BlockList[i].LocProc)(p1, p2);
          TProcType.pt4: TProc4(BlockObj.BlockList[i].LocProc)(p1, p2, p3);
          TProcType.pt5: TProc5(BlockObj.BlockList[i].LocProc)(p1, p2, p3, p4);
          TProcType.pt6: TProc6(BlockObj.BlockList[i].LocProc)(NSinteger(p1));
          TProcType.pt7:
          begin
            aRect.origin.x   := CGFloat(p1);
            aRect.origin.y   := CGFloat(p2);
            aRect.size.width := CGFloat(p3);
            aRect.size.height:= CGFloat(p4);
            result:= pointer(TProc7(BlockObj.BlockList[i].LocProc)(aRect));
          end;
        end;
      end;
    finally
      TMonitor.Exit(BlockObj);
    end;
  end;
end;

procedure DisposeCallback(aNSBlock: pointer) cdecl;
var
  i: integer;
begin
  if Assigned(BlockObj) then
  begin
    TMonitor.Enter(BlockObj);
    try
      i:= BlockObj.FindMatchingBlock(aNSBlock);
      if i >= 0
        then BlockObj.ClearBlock(i);
    finally
      TMonitor.Exit(BlockObj);
    end;
  end;
  TNSObject.Wrap(aNSBlock).release;
end;

procedure CopyCallback(scr, dst: pointer) cdecl;
begin
end;

class function TObjCBlock.CreateBlockWithProcedure(const aProc: TProc1): pointer;
begin
  result:= CreateBlockWithCFunc(TProc(aProc), TProcType.pt1);
end;

class function TObjCBlock.CreateBlockWithProcedure(const aProc: TProc2): pointer;
begin
  result:= CreateBlockWithCFunc(TProc(aProc), TProcType.pt2);
end;

class function TObjCBlock.CreateBlockWithProcedure(const aProc: TProc3): pointer;
begin
  result:= CreateBlockWithCFunc(TProc(aProc), TProcType.pt3);
end;

class function TObjCBlock.CreateBlockWithProcedure(const aProc: TProc4): pointer;
begin
  result:= CreateBlockWithCFunc(TProc(aProc), TProcType.pt4);
end;

class function TObjCBlock.CreateBlockWithProcedure(const aProc: TProc5): pointer;
begin
  result:= CreateBlockWithCFunc(TProc(aProc), TProcType.pt5);
end;

class function TObjCBlock.CreateBlockWithProcedure(const aProc: TProc6): pointer;
begin
  result:= CreateBlockWithCFunc(TProc(aProc), TProcType.pt6);
end;

class function TObjCBlock.CreateBlockWithProcedure(const aProc: TProc7): pointer;
begin
  result:= CreateBlockWithCFunc(TProc(aProc), TProcType.pt7);
end;

class function TObjCBlock.CreateBlockWithCFunc(const aTProc: TProc; const aType: TProcType): pointer;
begin
  result:= nil;
  if Assigned(BlockObj) then
  begin
    TMonitor.Enter(BlockObj);
    try
      result:= BlockObj.AddNewBlock(aTProc, aType);
    finally
      TMonitor.Exit(BlockObj);
    end;
  end;
end;

{TObjCBlockList}

constructor TObjCBlockList.Create;
begin
  inherited;
end;

destructor TObjCBlockList.Destroy;
begin
  TMonitor.Enter(Self);
  try
    ClearAllBlocks;
  finally
    TMonitor.Exit(Self);
  end;
  inherited Destroy;
end;

procedure TObjCBlockList.ClearBlock(const idx: integer);
begin
  Dispose(FBlockList[idx].BlockStructure.Descriptor);
  Dispose(FBlockList[idx].BlockStructure);
  FBlockList[idx].LocProc:= nil;
  Delete(FBlockList, idx, 1);
end;

function TObjCBlockList.AddNewBlock(const aTProc: TProc; const aType: TProcType): pointer;
var
  aDesc:  PBlock_Descriptor;
const
  BLOCK_HAS_COPY_DISPOSE = 1 shl 25;
begin
  SetLength(FBlockList, Length(FBlockList) + 1);
  FillChar(FBlockList[High(FBlockList)], SizeOf(TBlockInfo), 0);

  New(FBlockList[High(FBlockList)].BlockStructure);
  FBlockList[High(FBlockList)].BlockStructure.Isa    := NSClassFromString ((StrToNSStr('NSBlock') as ILocalobject).GetObjectID);
  FBlockList[High(FBlockList)].BlockStructure.Invoke := @InvokeCallback;
  FBlockList[High(FBlockList)].BlockStructure.Flags  := BLOCK_HAS_COPY_DISPOSE;
  FBlockList[High(FBlockList)].ProcType              := aType;
  FBlockList[High(FBlockList)].LocProc               := aTProc;

  New(aDesc);
  aDesc.Reserved       := 0;
  aDesc.Size           := SizeOf(Block_Literal);
  aDesc.copy_helper    := @CopyCallback;
  aDesc.dispose_helper := @DisposeCallback;
  FBlockList[High(FBlockList)].BlockStructure.Descriptor := aDesc;

  result:= FBlockList[High(FBlockList)].BlockStructure;
end;

procedure TObjCBlockList.ClearAllBlocks();
var
  i: integer;
begin
  for i := High(FBlockList) downto Low(FBlockList) do
     ClearBlock(i);
end;

function TObjCBlockList.FindMatchingBlock(const aCurrBlock: pointer): integer;
var
  i: integer;
begin
  result:= -1;
  if aCurrBlock <> nil then
  begin
    for i:= Low(FBlockList) to High(FBlockList) do
    begin
      if FBlockList[i].BlockStructure.Descriptor = PBlock_Literal(aCurrBlock).Descriptor
        then Exit(i);
    end;
  end;
end;

initialization
  BlockObj:=TObjCBlockList.Create;

finalization
  FreeAndNil(BlockObj);

{$ELSE}
implementation
{$ENDIF}
end.