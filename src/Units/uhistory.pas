{
  注意：这里的代码只针对 Windows 和 GTK2，其它平台未测试。

  编辑框内容的变化无非就是增加内容和减少内容，既不增加也不减少那就算不上变化。
  即使是选中了部分文本后再执行粘贴操作，也是分两步变化的，先删除被选中的内容，
  再加入剪贴板中的内容（Windows 是这么干的），或者先加入剪贴板中的内容，再删除
  被选中的内容（GTK2 是这么干的）

  所以只要把这些变化记录下来，就可以随时撤销和恢复了。最佳的记录时机就是在编辑
  框的内容改变前后进行记录，但是编辑框只提供了 OnChange 事件，也就是内容改变之
  后才触发的事件，所以我需要把改变之前的内容提前保存下来，以便在内容改变之后进
  行对比。

  正常情况下（GTK2 属于非正常情况）：
  1、增加内容之后，光标应该在新增加的内容之后，而且新文本内容比旧文本内容长，
  所以很容易算出增加了什么内容，从哪里开始增加的。
  2、删除内容之后，光标应该在被删除的内容之前，而且新文本内容比旧文本内容短，
  所以也容易算出减少了什么内容容，从哪里开始减少的。

  这就是本单元实现历史记录的基本思路，所需要的条件有三个：
  1、变化前的文本内容
  2、变化后的文本内容
  3、在哪里发生了改变（即正确的 SelStart）

  满足下面两个条件的 SelStart 就是正确的 SelStart：
  1、向编辑框中添加文本后，SelStart 应该在新增加的文本之后。
  2、从编辑框中删除文本后，SelStart 应该在被删除的文本之前。

  但是 GTK2 的这种“先加入剪贴板中的内容，再删除选中的内容”的做法给计算历史记录
  带来了麻烦，因为无法在 OnChange 事件中得到正确的 SelStart，在 OnChange 事件
  中，GTK2 的 SelStart 要用来记录被选中的内容，所以无法提供新增加的内容的具体
  位置，这就是一个盲点，需要比较变化前后文本内容才能知道增内容的正确位置。这种
  情况的一个特点就是编辑框内容变化之后，SelLength 不为 0（只有 GTK2 会出现这种
  情况，这可以作为一个着手点）。

  GTK2 还有一个致命的问题，就是拖放文本时无法提供正确的放置位置，也就是拖放文
  本前和拖放文本后的 SelStart 和 SelLength 是相同的，你不知道用户把文本拖放到
  了什么位置，这种情况简直就是一片漆黑，你只知道文本框的内容变化了，其它什么都
  不知道。更要命的是，拖放操作也是先把内容增加的目标位置，然后再删除之前选择的
  内容，这和前面说过的情况相同，因此你无法区分用户是执行了拖放操作还是执行了粘
  贴操作。

  最后，GTK2 的拖放操作还有一个 BUG，如果你快速反复的选择文本并拖放文本，你会
  发现你拖放的文本会莫名其妙的消失，也就是丢失数据。

  我很想禁用 GTK2 的编辑框文本拖放功能，但是我不知道怎么实现，如果有谁知道，恳
  请告诉我，非常感谢！

  本代码最后很艰难的实现了 Undo Redo 功能，支持任何拖拽操作。

  E-Mail: tomitomy@163.com
}

(*
// 用法演示
// 此演示需要 Form1, Edit1, Memo1, PopupMenu1, ActionList1 等控件
unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, ActnList, Menus, Clipbrd, uhistory;

type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1         : TEdit;
    Memo1         : TMemo;

    ActionList1   : TActionList;
    actnCut       : TAction;
    actnCopy      : TAction;
    actnPaste     : TAction;
    actnSelectAll : TAction;
    actnDelete    : TAction;
    actnUndo      : TAction;
    actnRedo      : TAction;

    PopupMenu1    : TPopupMenu;
    MenuItem1     : TMenuItem;
    MenuItem2     : TMenuItem;
    MenuItem3     : TMenuItem;
    MenuItem4     : TMenuItem;
    MenuItem5     : TMenuItem;
    MenuItem6     : TMenuItem;
    MenuItem7     : TMenuItem;
    MenuItem8     : TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure ActionExecute(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);

    procedure EditEnter(Sender: TObject);
  private
    FEditHistory   : THistoryManager;
    FMemoHistory   : THistoryManager;

    FActiveEdit    : TCustomEdit;
    FActiveHistory : THistoryManager;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // 初始化控件状态
  Edit1.OnEnter           := @EditEnter;
  Memo1.OnEnter           := @EditEnter;

  MenuItem1.Action        := actnCut;
  MenuItem2.Action        := actnCopy;
  MenuItem3.Action        := actnPaste;
  MenuItem4.Action        := actnSelectAll;
  MenuItem5.Action        := actnDelete;
  MenuItem6.Caption       := '-';
  MenuItem7.Action        := actnUndo;
  MenuItem8.Action        := actnRedo;

  actnCut.OnExecute       := @ActionExecute;
  actnCopy.OnExecute      := @ActionExecute;
  actnPaste.OnExecute     := @ActionExecute;
  actnSelectAll.OnExecute := @ActionExecute;
  actnDelete.OnExecute    := @ActionExecute;
  actnUndo.OnExecute      := @ActionExecute;
  actnRedo.OnExecute      := @ActionExecute;

  actnUndo.OnUpdate       := @ActionUpdate;

  Edit1.PopupMenu         := PopupMenu1;
  Memo1.PopupMenu         := PopupMenu1;

  // 分别为 TEdit 和 TMemo 创建历史记录管理器
  // 默认历史记录最大尺寸为 32K，默认历史记录最小数量为 10
  FEditHistory := THistoryManager.Create(Edit1);
  FEditHistory.CreateHistory(True);

  FMemoHistory := THistoryManager.Create(Memo1);
  FMemoHistory.CreateHistory(True);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // 注意释放顺序要与创建时相反，这关系到 Application.OnIdle 的复位。
  FMemoHistory.Free;
  FEditHistory.Free;
end;

procedure TForm1.ActionExecute(Sender: TObject);
begin
  case (Sender as TAction).Name of
    'actnCut'      : FActiveHistory.Cut;
    'actnCopy'     : FActiveEdit.CopyToClipboard;
    'actnPaste'    : FActiveHistory.Paste;
    'actnSelectAll': FActiveEdit.SelectAll;
    'actnDelete'   : FActiveHistory.Delete;
    'actnUndo'     : FActiveHistory.Undo;
    'actnRedo'     : FActiveHistory.Redo;
  end;
end;

procedure TForm1.ActionUpdate(Sender: TObject);
begin
  actnUndo  .Enabled := (FActiveHistory <> nil) and (FActiveHistory.CanUndo);
  actnRedo  .Enabled := (FActiveHistory <> nil) and (FActiveHistory.CanRedo);
  actnCut   .Enabled := FActiveEdit.SelLength > 0;
  actnCopy  .Enabled := actnCut.Enabled;
  actnPaste .Enabled := ClipBoard.HasFormat(CF_TEXT);
  actnDelete.Enabled := actnCut.Enabled;
end;

procedure TForm1.EditEnter(Sender: TObject);
begin
  FActiveEdit := Sender as TCustomEdit;

  if FActiveEdit = Edit1 then
    FActiveHistory := FEditHistory
  else
    FActiveHistory := FMemoHistory;
end;

end.
*)

unit uhistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Dialogs, Forms, Clipbrd;

type

  THalfRecord = (hrFull, hrFirstHalf, hrSecondHalf);

  PHistoryRecord = ^THistoryRecord;

  { THistoryRecord 用来存放单个操作的历史记录 }

  THistoryRecord = record
    PrevSelStart  : integer;      // OnChange 之前的 SelStart
    PrevSelLength : integer;      // OnChange 之前的 SelLength
    PrevSelText   : string;       // OnChange 之前的 SelText
    SelStart      : integer;      // OnChange 之后的 SelStart
    SelLength     : integer;      // OnChange 之后的 SelLength
    SelText       : string;       // OnChange 之后的 SelText
    HalfRecord    : THalfRecord;  // 标记当前记录是否只是半个过程，用于拖放操作
  end;

  { THistory 用来存取多个操作的历史记录 }

  THistory = class
  private
    FList  : TList;        // 历史记录列表
    FIndex : integer;      // 当前历史记录索引
    FSize  : integer;      // 历史记录总大小
    // 清空并复位历史记录数据
    procedure Reset;
    // 限制历史记录总大小
    procedure Limit(MaxSize, MinCount: integer);
    function  GetCount: Integer;
    function  GetRecordSize(Idx: integer): integer;
    function  Get(Idx: Integer): PHistoryRecord;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRecord(PHR: PHistoryRecord);
    procedure AddRecord(
      APrevSelStart  : integer;
      APrevSelLength : integer;
      APrevSelText   : string;
      ASelStart      : integer;
      ASelLength     : integer;
      ASelText       : string); inline;
    property  Index : Integer read FIndex;
    property  Size  : integer read FSize;
    property  Count : integer read GetCount;
    property  List  : TList   read FList;
    property  Items[Idx: Integer]: PHistoryRecord read Get; default;
  end;

  { THistoryManager }

  THistoryManager = class
  private
    FEdit               : TCustomEdit;   // 用来实现历史操作的编辑框控件
    FHistory            : THistory;      // 历史记录数据
    FMaxSize            : integer;       // 历史记录最大容量限制
    FMinCount           : integer;       // 历史记录最小数量保护
    FTotalSize          : integer;       // 用于多重历史记录（多个 THistory）
    FEditing            : boolean;       // 标记编辑框是否处于编辑状态
    FEnabled            : boolean;       // 启用或禁用对编辑框的监视
    //FIgnoreOnce         : boolean;       // 用来避免 PasteFromClipboard 引发的 OnChange 事件
    FHalfRecord         : boolean;       // 用于处理拖拽操作的标记
    FPrevText           : string;        // OnChagne 之前的 Text
    FPrevSelStart       : integer;       // OnChagne 之前的 SelStart
    FOldApplicationIdle : TIdleEvent;    // 修改 OnApplicationIdle 用来获取 OnChagne 之前的 SelStart
    FOldEditChange      : TNotifyEvent;  // 修改 OnChange 用来监视编辑框内容的变化
    FOnHistoryChanged   : TNotifyEvent;  // 提供 OnHistoryChanged 给外部代码用于监视历史记录的变化

    FFixSelTextBug      : Boolean;       // 修补 GTK2 中 SelText:=Something 不触发 OnChange 事件的 BUG

    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure EditChange(Sender: TObject);
    // 仅使用 PrevText、Text、SelStart 来计算历史记录数据（可以在 Windows 中正常工作）
    procedure CalcRecord(PHR: PHistoryRecord);
    // 针对 GTK2 的特殊处理
    procedure CalcRecordGTK2Memo(PHR: PHistoryRecord);
    // 仅使用 PrevText、Text 来计算历史记录数据（速度最慢）
    procedure CalcRecordHard(PHR: PHistoryRecord);

    function  GetIndex     : integer;
    function  GetCount     : integer;
    function  GetSize      : integer;
    function  GetTotalSize : integer;
  public
    constructor Create(AEdit: TCustomEdit; AMaxSize: Integer = 32 * 1024; AMinCount: integer = 10);
    destructor Destroy; override;
    // 创建历史记录数据存取对象
    procedure CreateHistory(AEnabled: Boolean);
    // 销毁历史记录数据存取对象
    procedure DestroyHistory(AHistory: THistory = nil);
    // 切换历史记录数据存取对象
    procedure SwitchHistory(AHistory: THistory);

    // 向当前历史记录数据中添加记录
    procedure AddRecord(PHR: PHistoryRecord); inline;
    // 向当前历史记录数据中添加记录
    procedure AddRecord(
      APrevSelStart  : integer;
      APrevSelLength : integer;
      APrevSelText   : string;
      ASelStart      : integer;
      ASelLength     : integer;
      ASelText       : string); inline;
    // 向当前历史记录数据中添加记录，对所有内容进行整体替换
    procedure AddRecordSimply(APrevText: string); inline;
    // 清空并复位历史记录数据
    procedure Reset;

    function  CanUndo: boolean;
    function  CanRedo: boolean;
    procedure Undo;
    procedure Redo;

    procedure Cut;
    procedure Paste;
    procedure Delete;

    property  Edit      : TCustomEdit read FEdit;
    property  History   : THistory read FHistory;
    property  Enabled   : Boolean read FEnabled write FEnabled;

    property  Index     : integer read GetIndex;
    property  Size      : integer read GetSize;
    property  Count     : integer read GetCount;

    property  TotalSize : integer read GetTotalSize;
    property  MaxSize   : integer read FMaxSize write FMaxSize;
    property  MinCount  : integer read FMinCount write FMinCount;
    property  OnHistoryChanged: TNotifyEvent read FOnHistoryChanged write FOnHistoryChanged;
  end;

function UTF8DiffBytePos(S1, S2: string; var Start1, Start2: SizeInt; Reverse: boolean = False): boolean;

function UTF8LengthFast(PStr: PChar; const ASize: SizeInt): SizeInt;
function UTF8LengthFast(const AStr: String): SizeInt;  inline;

function UTF8PosToBytePos(const PStr: PChar; const ASize: SizeInt; UPos: SizeInt): SizeInt;
function UTF8PosToBytePos(const AStr: String; const UPos: SizeInt): SizeInt;  inline;

function UTF8CodePointLength(const PStr: PChar): SizeInt;
function UTF8CodePointLength(const AStr: string): SizeInt;  inline;

implementation

{ THistory }

constructor THistory.Create;
begin
  inherited Create;
  FList := TList.Create;
  Reset;
end;

destructor THistory.Destroy;
begin
  Reset;
  FList.Free;
  inherited Destroy;
end;

// 清空并复位历史记录
procedure THistory.Reset;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    dispose(Items[i]);
  FList.Clear;
  FIndex := -1;
  FSize := 0;
end;

// 添加一条历史记录
procedure THistory.AddRecord(PHR: PHistoryRecord);
var
  i: integer;
begin
  // 删除当前索引之后的历史记录
  for i := FList.Count - 1 downto FIndex + 1 do begin
    Dec(FSize, GetRecordSize(i));
    dispose(Items[i]);
    FList.Delete(i);
  end;
  FList.Add(PHR);
  Inc(FIndex);
  Inc(FSize, GetRecordSize(FIndex));
end;

// 添加一条历史记录
procedure THistory.AddRecord(
  APrevSelStart  : integer;
  APrevSelLength : integer;
  APrevSelText   : string;
  ASelStart      : integer;
  ASelLength     : integer;
  ASelText       : string); inline;
var
  PHR: PHistoryRecord;
begin
  PHR := new(PHistoryRecord);
  PHR^.PrevSelStart  := APrevSelStart;
  PHR^.PrevSelLength := APrevSelLength;
  PHR^.PrevSelText   := APrevSelText;
  PHR^.SelStart      := ASelStart;
  PHR^.SelLength     := ASelLength;
  PHR^.SelText       := ASelText;
  AddRecord(PHR);
end;

// 获取指定的历史记录的大小
function THistory.GetRecordSize(Idx: integer): integer;
begin
  Result :=
    Length(Items[Idx]^.PrevSelText) +
    Length(Items[Idx]^.SelText) +
    Sizeof(THistoryRecord);
end;

// 限制历史记录总大小，但保证历史记录数量不低于 MinCount
procedure THistory.Limit(MaxSize, MinCount: integer);
begin
  while (MaxSize > 0) and (FSize > MaxSize) and (Count > MinCount) do begin
    Dec(FSize, GetRecordSize(0));
    dispose(Items[0]);
    FList.Delete(0);
    Dec(FIndex);
  end;
end;

// 获取历史记录总个数
function THistory.GetCount: Integer;
begin
  Result := FList.Count;
end;

// 获取指定的历史记录
function THistory.Get(Idx: Integer): PHistoryRecord;
begin
  Result := PHistoryRecord(FList[Idx]);
end;

{ THistoryManager }

constructor THistoryManager.Create(AEdit: TCustomEdit; AMaxSize: Integer = 32 * 1024; AMinCount: integer = 10);
begin
  inherited Create;

  FMaxSize  := AMaxSize;
  FMinCount := AMinCount;

  FEdit := AEdit;

  FOldEditChange := FEdit.OnChange;
  FEdit.OnChange := @EditChange;

  FOldApplicationIdle := Application.OnIdle;
  Application.OnIdle := @ApplicationIdle;

  // 历史记录数据由外部代码在需要的时候创建
  FHistory    := nil;
  FPrevText   := '';
  FTotalSize  := 0;
  // 创建了历史记录数据之后才能开始监视
  FEnabled    := False;
  FEditing    := True;
  //FIgnoreOnce := False;
  FHalfRecord := False;
  FFixSelTextBug := False;
end;

destructor THistoryManager.Destroy;
begin
  FEdit.OnChange := FOldEditChange;
  Application.OnIdle := FOldApplicationIdle;

  DestroyHistory;

  inherited Destroy;
end;

procedure THistoryManager.Reset;
begin
  if FHistory <> nil then begin
    FHistory.Reset;
    FPrevText := FEdit.Text;
    if Assigned(FOnHistoryChanged) then
      FOnHistoryChanged(self);
  end else
    FPrevText := '';
end;

procedure THistoryManager.CreateHistory(AEnabled: boolean);
begin
  // 旧的 FHistory 由外部代码保管，这里不做处理，只保留其大小
  if FHistory <> nil then
    Inc(FTotalSize, FHistory.Size);

  FHistory := THistory.Create;
  FPrevText := FEdit.Text;

  FEnabled := AEnabled;
end;

// 请通过此方法释放历史记录，避免历史记录大小计算错误
procedure THistoryManager.DestroyHistory(AHistory: THistory = nil);
begin
  if (AHistory <> FHistory) and (AHistory <> nil) then
    Dec(FTotalSize, AHistory.Size)
  else begin
    AHistory := FHistory;
    FHistory := nil;
    FPrevText := '';
    FEnabled := False;
  end;

  if AHistory <> nil then FreeAndNil(AHistory);
end;

procedure THistoryManager.SwitchHistory(AHistory: THistory);
begin
  // 旧的 FHistory 由外部代码保管，这里不做处理，只保留其大小
  if FHistory <> nil then
    Inc(FTotalSize, FHistory.Size);

  FHistory := AHistory;
  // 没有数据可供读写时，要禁用编辑框监视，否则会出现异常
  if FHistory = nil then FEnabled := False;

  // 去掉之前保存过的大小（FTotalSize 不包括当前历史记录的大小）
  if FHistory <> nil then begin
    Dec(FTotalSize, FHistory.Size);
    // 历史记录与 PrevText 是相关联的，必须保持一致
    FPrevText := FEdit.Text;
  end else
    FPrevText := '';
end;

// 请通过此方法添加历史记录，因为此方法会更新 FPrevText
procedure THistoryManager.AddRecord(PHR: PHistoryRecord); inline;
begin
  FHistory.AddRecord(PHR);
  FPrevText := FEdit.Text;
end;

// 请通过此方法添加历史记录，因为此方法会更新 FPrevText
procedure THistoryManager.AddRecord(
  APrevSelStart: integer;
  APrevSelLength: integer;
  APrevSelText: string;
  ASelStart: integer;
  ASelLength: integer;
  ASelText: string); inline;
begin
  FHistory.AddRecord(APrevSelStart, APrevSelLength, APrevSelText,
    ASelStart, ASelLength, ASelText);
  FPrevText := FEdit.Text;
end;

// 请通过此方法添加历史记录，因为此方法会更新 FPrevText
procedure THistoryManager.AddRecordSimply(APrevText: string); inline;
begin
  FHistory.AddRecord(
    0, UTF8LengthFast(APrevText), APrevText,
    0, UTF8LengthFast(FEdit.Text), FEdit.Text);
  FPrevText := FEdit.Text;
end;

// 记录 OnChange 之前的 SelStart
procedure THistoryManager.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  FPrevSelStart := FEdit.SelStart;

  if Assigned(FOldApplicationIdle) then
    FOldApplicationIdle(Sender, Done);
end;

// 分析并存储历史记录
procedure THistoryManager.EditChange(Sender: TObject);
var
  PHR: PHistoryRecord;
begin
  if FFixSelTextBug then
    FFixSelTextBug := False;

  // 为了监控 OnChange 事件是否被触发，要在这里处理所有 OnChange 事件，即使历史记录被禁用
  if FEnabled then begin
    // 忽略错误的 OnChange 事件（即内容没有发生改变）
    if FEditing and (Length(FPrevText) <> Length(FEdit.Text)) then begin
      // 忽略由 PasteFromClipboard 延迟触发的 OnChange 事件
      //if FIgnoreOnce then
      //  FIgnoreOnce := False
      //else begin
        PHR := new(PHistoryRecord);
        PHR^.HalfRecord := hrFull;

        // Pos 的结果是从索引 1 开始计数的
        if Pos('Gtk2', FEdit.WidgetSetClass.ClassName) > 0 then begin
          if FEdit.ClassType = TEdit then
            CalcRecordHard(PHR)
          else
            CalcRecordGTK2Memo(PHR);
        end else
          CalcRecord(PHR);

        // AddRecord 会自动设置 FPrevText := FEdit.Text;
        AddRecord(PHR);
        FHistory.Limit(FMaxSize, FMinCount);
      //end;
    end;

    if Assigned(FOnHistoryChanged) then
      FOnHistoryChanged(self);
  end;

  if Assigned(FOldEditChange) then
    FOldEditChange(Sender);
end;

// 这个函数可以在 Windows 中正确处理所有变化情况
// 用于 TMemo 这种内容比较多的控件
procedure THistoryManager.CalcRecord(PHR: PHistoryRecord);
var
  PrevLen, Len, BSelStart: integer;
  SelText : string;
  SelLength: integer;
begin
  PrevLen := Length(FPrevText);
  Len     := Length(FEdit.Text);
  // 转换为字节索引，因为字节索引的操作速度比字符索引快很多。
  // UTF8PosToBytePos 的参数和结果是从索引 1 开始计数的。
  BSelStart := UTF8PosToBytePos(PChar(FEdit.Text), Len, FEdit.SelStart + 1);
  // Len > PrevLen 表示编辑框中增加了内容，此时 SelStart 应该在新增加的内容之后。
  // Len < PrevLen 表示编辑框中删除了内容，此时 SelStart 应该在被删除的内容之前。
  // Len = PrevLen 不可能存在
  if Len > PrevLen then begin
    // 计算出增加了什么内容（Copy 的参数是从索引 1 开始计数的）
    SelText := Copy(FEdit.Text, BSelStart - (Len - PrevLen), (Len - PrevLen));
    // 计算出增加了多少字符
    SelLength := UTF8LengthFast(SelText);

    PHR^.PrevSelStart := FEdit.SelStart - SelLength;
    PHR^.PrevSelLength := 0;
    PHR^.PrevSelText := '';

    PHR^.SelStart := PHR^.PrevSelStart;
    PHR^.SelLength := SelLength;
    PHR^.SelText := SelText;
  end else if Len < PrevLen then begin
    // 计算出删除了什么内容（Copy 的参数是从索引 1 开始计数的）
    SelText := Copy(FPrevText, BSelStart, (PrevLen - Len));
    // 计算出删除了多少字符
    SelLength := UTF8LengthFast(SelText);

    PHR^.PrevSelStart := FEdit.SelStart;
    PHR^.PrevSelLength := SelLength;
    PHR^.PrevSelText := SelText;

    PHR^.SelStart := PHR^.PrevSelStart;
    PHR^.SelLength := 0;
    PHR^.SelText := '';
  end else
    ShowMessage('CalcRecord: History Record Error !');

  if FHalfRecord then begin
    PHR^.HalfRecord := hrSecondHalf;
    FHalfRecord := False;
  end;
end;

// 用于 TMemo 这种内容比较多的控件
procedure THistoryManager.CalcRecordGTK2Memo(PHR: PHistoryRecord);
var
  BPrevSelStart, BSelLength: integer;
  BSelStartFix, BSelStartFix2: SizeInt;
  SelStart: integer;
  PrevSelText, SelText: string;
begin
  // 只有 GTK2 会出现“编辑框内容改变之后，SelLength 不为 0”的情况。
  // 这里不考虑“选中文本后再粘贴”的情况，因为粘贴操作可以在 OnChange 之前进行拦
  // 截并处理，以便减轻这里的负担。这里只考虑拖拽操作，拖拽前后 SelStart 始终是
  // 拖拽前的值，但如果是向头拖拽（朝文本开始的方向拖拽），则当文本内容增加后，
  // SelStart 也会相应进行偏移，如果是向尾拖拽，则增加的内容在 SelStart 之后，
  // SelStart 不会进偏移。
  // 通过这个特点，我可以知道文本是向头拖拽还是向尾拖拽，从而从 SelStart 处开始
  // 定向搜索变化前后的不同之处。拖拽操作一般不会拖太远，所以计算起来也比较快。
  // 这个操作依赖于 FPrevSelStart，这样才能知道 SelStart 有没有进行偏移。
  if FEdit.SelLength > 0 then begin
    // 拖拽操作的前半部分（增加内容）
    PHR^.HalfRecord := hrFirstHalf;
    FHalfRecord := True;

    if FEdit.SelStart = FPrevSelStart then
    // SelStart 没有进行偏移，执行的是向尾拖拽，接下来要从 SelStart 处开始向
    // 后比较文本的不同之处，从而获取新增加的内容。
    begin
      // 获取 PrevSelStart 的字节索引。
      // UTF8PosToBytePos 的参数和结果是从索引 1 开始计数的。
      BPrevSelStart := UTF8PosToBytePos(PChar(FPrevText), Length(FPrevText), FPrevSelStart + 1);
      // 获取新增内容的字节长度
      BSelLength := Length(FEdit.Text) - Length(FPrevText);
      // Copy 的参数是从索引 1 开始计数的
      PrevSelText := Copy(FPrevText, BPrevSelStart, BSelLength);
      // 准备查找的起始位置（索引从 1 开始，跳过已选择的部分）。
      BSelStartFix := BPrevSelStart + BSelLength;
      BSelStartFix2 := BSelStartFix;
      // 向尾查找不同之处（即新内容的插入位置，返回值索引从 1 开始）。
      // 如果没有不同之处，则说明新内容被插入到文本的尾部。
      UTF8DiffBytePos(FEdit.Text, FPrevText, BSelStartFix, BSelStartFix2);
      // 获取新增加的内容（新增加的内容不一定就是之前选择的内容，比如 a++b++c++，
      // 当把 +b+ 拖到 c+ 之后，就会出现这种情况）。
      // Copy 的参数是从索引 1 开始计数的。
      SelText := Copy(FEdit.Text, BSelStartFix, BSelLength);
      while PrevSelText <> SelText do begin
        Dec(BSelStartFix);
        SelText := Copy(FEdit.Text, BSelStartFix, BSelLength);
      end;
      // 计算正确的 SelStart。
      SelStart := FPrevSelStart + UTF8LengthFast(PChar(FPrevText) + BPrevSelStart - 1, BSelStartFix - BPrevSelStart);
    end
    else if FEdit.SelStart > FPrevSelStart then
    // SelStart 发生了偏移，执行的是向头拖拽，接下来要从 SelStart 处开始向头比
    // 较文本的不同之处，从而获取新增加的内容。
    begin
      // 获取 PrevSelStart 的字节索引。
      // UTF8PosToBytePos 的参数和结果是从索引 1 开始计数的。
      BPrevSelStart := UTF8PosToBytePos(PChar(FPrevText), Length(FPrevText), FPrevSelStart + 1);
      // 获取新增内容的字节长度
      BSelLength := Length(FEdit.Text) - Length(FPrevText);
      // Copy 的参数是从索引 1 开始计数的
      PrevSelText := Copy(FPrevText, BPrevSelStart, BSelLength);
      // 准备查找的起始位置（索引从 1 开始，跳过已选择的部分）。
      BSelStartFix := BPrevSelStart + BSelLength; // FEdit.Text 偏移后的 SelStart
      BSelStartFix2 := BPrevSelStart;             // FPrevSelStart
      // 向头查找不同之处（即新内容的插入位置，返回值索引从 1 开始）。
      // 如果没有不同之处，则说明新内容被插入到文本的头部。
      UTF8DiffBytePos(FEdit.Text, FPrevText, BSelStartFix, BSelStartFix2, True);
      // 计算正确的 SelStart
      if BSelStartFix2 = 0 then begin // 新内容被插入到文本的头部
        SelStart := 0;
        SelText := PrevSelText;
      end
      else begin
        // 跳过找到的不同的字符
        BSelStartFix := BSelStartFix + UTF8CodePointLength(PChar(FEdit.Text) + BSelStartFix - 1);
        // 获取新增加的内容（新增加的内容不一定就是之前选择的内容，比如 a++b++c++，
        // 当把 +c+ 拖到 b+ 之前，就会出现这种情况）。
        if BSelStartFix <= BSelLength then BSelStartFix := BSelLength + 1;
        // 将 BSelStartFix 定位到新添加文本的开头位置
        Dec(BSelStartFix, BSelLength);
        // 获取新添加的文本
        // Copy 的参数是从索引 1 开始计数的。
        SelText := Copy(FEdit.Text, BSelStartFix, BSelLength);
        // 修补错误的结果
        while (SelText <> '') and (PrevSelText <> SelText) do begin
          Inc(BSelStartFix);
          SelText := Copy(FEdit.Text, BSelStartFix, BSelLength);
        end;
        // 计算正确的 SelStart
        SelStart := FPrevSelStart - UTF8LengthFast(PChar(FPrevText) + BSelStartFix - 1, BPrevSelStart - BSelStartFix);
      end;
    end else begin
      ShowMessage('CalcRecordGTK2Memo: History Record Error !');
      Exit;
    end;

    PHR^.PrevSelStart  := SelStart;
    PHR^.PrevSelLength := 0;
    PHR^.PrevSelText   := '';

    PHR^.SelStart      := PHR^.PrevSelStart;
    PHR^.SelText       := SelText;
    PHR^.SelLength     := UTF8LengthFast(SelText);
  end else if (FEdit.SelStart = FPrevSelStart) and (Length(FEdit.Text) > Length(FPrevText)) then // 从其它控件中拖放文本进来到光标之后
    CalcRecordHard(PHR)
  else if FEdit.SelStart - FPrevSelStart > 1 then // 从其它控件中拖放文本进来到光标之前（粘贴的情况已经在之前被拦截了）
     CalcRecordHard(PHR)
  else // 普通的输入、删除操作
    CalcRecord(PHR);
end;

// 用于 TEdit 这种内容比较少的控件（从头到尾比较文本的变化）
procedure THistoryManager.CalcRecordHard(PHR: PHistoryRecord);
var
  PrevLen, Len: integer;
  SelStart, PrevSelStart, SelLength: SizeInt;
  SelText: string;
begin
  Len     := Length(FEdit.Text);
  PrevLen := Length(FPrevText);

  // 向尾查找不同之处（参数和返回值的索引从 1 开始）。
  SelStart     := 1;
  PrevSelStart := 1;
  UTF8DiffBytePos(FEdit.Text, FPrevText, SelStart, PrevSelStart);

  if Len > PrevLen then begin   // 向 FEdit 中添加内容
    Dec(SelStart);
    SelText := Copy(FEdit.Text, SelStart + 1, (Len - PrevLen));
    SelLength := UTF8LengthFast(SelText);
    SelStart := UTF8LengthFast(PChar(FEdit.Text), SelStart);

    PHR^.PrevSelStart := SelStart;
    PHR^.PrevSelLength := 0;
    PHR^.PrevSelText := '';

    PHR^.SelStart := PHR^.PrevSelStart;
    PHR^.SelLength := SelLength;
    PHR^.SelText := SelText;
  end else if Len < PrevLen then begin   // 从 FEdit 中删除内容
    Dec(PrevSelStart);
    SelText := Copy(FPrevText, PrevSelStart + 1, (PrevLen - Len));
    SelLength := UTF8LengthFast(SelText);
    SelStart := UTF8LengthFast(PChar(FPrevText), PrevSelStart);

    PHR^.PrevSelStart := SelStart;
    PHR^.PrevSelLength := SelLength;
    PHR^.PrevSelText := SelText;

    PHR^.SelStart := PHR^.PrevSelStart;
    PHR^.SelLength := 0;
    PHR^.SelText := '';
  end else begin
    ShowMessage('CalcRecordHard: History Record Error !');
    Exit;
  end;

  if (FEdit.SelLength > 0) and (Length(FEdit.Text) > Length(FPrevText)) then begin // 拖拽操作或粘贴操作的前半步（增加内容）
    PHR^.HalfRecord := hrFirstHalf;
    FHalfRecord := True;
  end else if FHalfRecord then begin // 拖拽操作或粘贴操作的后半步（删除内容）
    PHR^.HalfRecord := hrSecondHalf;
    FHalfRecord := False;
  end;
end;

function THistoryManager.CanUndo: boolean;
begin
  Result := (FHistory <> nil) and (FHistory.FIndex >= 0);
end;

function THistoryManager.CanRedo: boolean;
begin
  Result := (FHistory <> nil) and (FHistory.FIndex < FHistory.Count - 1);
end;

procedure THistoryManager.Undo;
var
  Half: THalfRecord;
begin
  if FHistory.FIndex < 0 then Exit;

  FEditing := False;
  // 防止 FEdit 的内容改变时发生内存复制操作，因为 FPrevText 和 FEdit.Text 是
  // 相同的，共用内存，改写 FEdit.Text 将引发内存复制操作
  FPrevText := '';
  // 在“编辑框无内容”或“编辑框内容全选”的情况下，执行 SelText:=something 不会
  // 触发 OnChange 事件，所以需要用 FFixSelTextBug 修补
  FFixSelTextBug := True;
  with FHistory[FHistory.FIndex]^ do begin
    FEdit.SelStart  := SelStart;
    FEdit.SelLength := SelLength;
    FEdit.SelText   := PrevSelText;
    Half            := HalfRecord;
  end;
  FPrevText := FEdit.Text;
  if FFixSelTextBug then EditChange(FEdit);
  FEditing := True;

  Dec(FHistory.FIndex);

  if Half = hrSecondHalf then Undo;

  if Assigned(FOnHistoryChanged) and (Half <> hrFirstHalf) then
    FOnHistoryChanged(self);
end;

procedure THistoryManager.Redo;
var
  Half: THalfRecord;
begin
  if FHistory.FIndex >= FHistory.Count - 1 then Exit;

  Inc(FHistory.FIndex);

  FEditing := False;
  // 防止 FEdit 的内容改变时发生内存复制操作，因为 FPrevText 和 FEdit.Text 是
  // 相同的，共用内存，改写 FEdit.Text 将引发内存复制操作
  FPrevText := '';
  // 在“编辑框无内容”或“编辑框内容全选”的情况下，执行 SelText:=something 不会
  // 触发 OnChange 事件，所以需要用 FFixSelTextBug 修补
  FFixSelTextBug := True;
  with FHistory[FHistory.FIndex]^ do begin
    FEdit.SelStart  := PrevSelStart;
    FEdit.SelLength := PrevSelLength;
    FEdit.SelText   := SelText;
    Half            := HalfRecord;
  end;
  FPrevText := FEdit.Text;
  if FFixSelTextBug then EditChange(FEdit);
  FEditing := True;

  if Half = hrFirstHalf then Redo;

  if Assigned(FOnHistoryChanged) and (Half <> hrSecondHalf) then
    FOnHistoryChanged(self);
end;

// 使用此处的剪切操作，可以免去对比文本所花费的时间
procedure THistoryManager.Cut;
var
  SelStart, SelLength: integer;
  SelText: string;
begin
  if FEdit.SelLength = 0 then Exit;
  SelStart  := FEdit.SelStart;
  SelLength := FEdit.SelLength;
  SelText   := FEdit.SelText;

  FEnabled := False;
  // CutToClipboard 会正常触发 OnChange 事件，不过在这里历史记录监视被取消了
  FEdit.CutToClipboard;
  // AddRecord 会自动设置 FPrevText := FEdit.Text;
  AddRecord(SelStart, SelLength, SelText, SelStart, 0, '');
  FEnabled := True;

  if Assigned(FOnHistoryChanged) then
    FOnHistoryChanged(self);
end;

// 必须用此处的方法执行粘贴操作，因为需要拦截“粘贴”操作并进行相应的处理
procedure THistoryManager.Paste;
var
  SelStart, SelLength: integer;
  SelText: string;
  ClipBoardText: string;
begin
  if not Clipboard.HasFormat(CF_TEXT) then Exit;
  SelStart  := FEdit.SelStart;
  SelLength := FEdit.SelLength;
  SelText   := FEdit.SelText;
  ClipBoardText := ClipBoard.AsText;

  FEnabled := False;
  // 在“编辑框无内容”或“编辑框内容全选”的情况下，执行 SelText:=something 不会
  // 触发 OnChange 事件，所以需要用 FFixSelTextBug 修补
  FFixSelTextBug := True;

  // PasteFromClipboard 会延迟触发 FEdit.OnChange 事件，无法通过上面的 Enabled := False 来控制
  // GTK2 TMemo 这混乱的逻辑简直让人抓狂，整个单元所有复杂的代码都是在修补 GTK2 TMemo 的 Bug
  //FEdit.PasteFromClipboard;
  // 弄个标记，用来忽略延迟的 OnChange 触发
  //FIgnoreOnce := True;

  // 设置 SelText 会导致 GTK2 TMemo 的内容自动滚动
  FEdit.SelText := ClipBoardText;

  // 在没有执行过剪切或复制操作之前，执行 PasteFromClipboard 将无法及时获取粘贴
  // 后的结果，导致 FPrevText := FEdit.Text 获取错误的结果

  // AddRecord 会自动设置 FPrevText := FEdit.Text;
  // 必须在这里提前处理“粘贴”操作，如果交给 EditChange 处理则会和拖拽操作相互冲突，导致历史记录出错
  AddRecord(SelStart, SelLength, SelText, SelStart, UTF8LengthFast(ClipBoardText), ClipBoardText);

  if FFixSelTextBug then EditChange(FEdit);
  FEnabled := True;

  if Assigned(FOnHistoryChanged) then
    FOnHistoryChanged(self);
end;

// 必须使用此方法执行删除操作，此方法会修补 ClearSelection 的 BUG
procedure THistoryManager.Delete;
var
  SelStart, SelLength: integer;
  SelText: string;
begin
  if FEdit.SelLength = 0 then Exit;
  SelStart  := FEdit.SelStart;
  SelLength := FEdit.SelLength;
  SelText   := FEdit.SelText;

  FEnabled := False;
  // 在“编辑框无内容”或“编辑框内容全选”的情况下，执行 SelText:=something 不会
  // 触发 OnChange 事件，所以需要用 FFixSelTextBug 修补
  FFixSelTextBug := True;

  // 设置 SelText 会导致 GTK2 TMemo 的内容自动滚动，ClearSelection 就是通过设置 SelText 实现的
  // 在 Windows 中，执行 ClearSelection 无法在 OnChange 事件中获得正确的 SelStart
  FEdit.ClearSelection;

  // AddRecord 会自动设置 FPrevText := FEdit.Text;
  AddRecord(SelStart, SelLength, SelText, SelStart, 0, '');

  if FFixSelTextBug then EditChange(FEdit);
  FEnabled := True;

  if Assigned(FOnHistoryChanged) then
    FOnHistoryChanged(self);
end;

function THistoryManager.GetIndex: integer;
begin
  if FHistory <> nil then
    Result := FHistory.FIndex + 1
  else
    Result := -1;
end;

function THistoryManager.GetCount: integer;
begin
  if FHistory <> nil then
    Result := FHistory.Count
  else
    Result := 0;
end;

function THistoryManager.GetSize: integer;
begin
  if FHistory <> nil then
    Result := FHistory.FSize
  else
    Result := 0;
end;

function THistoryManager.GetTotalSize: integer;
begin
  Result := FTotalSize + GetSize;
end;

// 查找 S1 和 S2 的不同之处，参数和返回值的索引都是从 1 开始，到 Length(S) 结束。
// Start1 表示 S1 的起始查找位置，查找结果也通过该参数返回。
// Start2 表示 S2 的起始查找位置，查找结果也通过该参数返回。
// Reverse 为 True 表示向头查找，False 表示向尾查找。
// 返回值：True 表示找到，False 表示没找到。
// 如果没找到，则向头查找时，某一 Start 返回 0，向尾查找时某一 Start 返回 UTF8Length(S)+1。
function UTF8DiffBytePos(S1, S2: string; var Start1, Start2: SizeInt; Reverse: boolean = False): boolean;

  // 回到 S1 中字符码点的起始位置
  procedure GoToCpStartS1;
  begin
    while Start1 > 0 do // 如果 UTF8 编码不正确，则 Start1 可能小于等于 0
      case S1[Start1] of
        #0..#127, #192..#247: break;
        else Dec(Start1);
      end;
  end;

  // 回到 S2 中字符码点的起始位置
  procedure GoToCpStartS2;
  begin
    while Start2 > 0 do // 如果 UTF8 编码不正确，则 Start2 可能小于等于 0
      case S2[Start2] of
        #0..#127, #192..#247: break;
        else Dec(Start2);
      end;
  end;

begin
  Result := False;
  if (Start1 <= 0) or (Start2 <= 0) or (Start1 > Length(S1)) or (Start2 > Length(S2)) then Exit;

  if Reverse then begin
    while (Start1 >= 1) and (Start2 >= 1) and (S1[Start1] = S2[Start2]) do begin
      Dec(Start1);
      Dec(Start2);
    end;
    if Start1 > 1 then GoToCpStartS1;
    if Start2 > 1 then GoToCpStartS2;
    Result := (Start1 > 0) and (Start2 > 0);
  end else begin
    while (Start1 <= Length(S1)) and (Start2 <= Length(S2)) and (S1[Start1] = S2[Start2]) do begin
      Inc(Start1);
      Inc(Start2);
    end;
    if Start1 <= Length(S1) then GoToCpStartS1;
    if Start2 <= Length(S2) then GoToCpStartS2;
    Result := (Start1 <= Length(S1)) and (Start2 <= Length(S2));
  end;
end;

//// 获取 PStr 所指向的字符串中 UTF8 字符的个数，SSize 限定字符串长度
//function UTF8LengthFast(PStr: PChar; const SSize: SizeInt): SizeInt;
//var
//  Pos: Integer;
//begin
//  Result := 0;
//  Pos    := 0;
//  while Pos < SSize do begin
//    case PStr[Pos] of
//      // #0..#127: Inc(Pos);
//      #192..#223: Inc(Pos, 2);
//      #224..#239: Inc(Pos, 3);
//      #240..#247: Inc(Pos, 4);
//      else Inc(Pos);
//    end;
//    Inc(Result);
//  end;
//end;
//
//// 获取字符串 AStr 中 UTF8 字符的个数
//function UTF8LengthFast(const AStr: String): SizeInt; inline;
//begin
//  Result := UTF8LengthFast(PChar(AStr), Length(AStr));
//end;

// 获取 PStr 所指向的字符串中 UTF8 字符的个数，SSize 限定字符串长度
// 由 Lazarus 论坛的 engkin 编写，速度非常快
function UTF8LengthFast(PStr: PChar; const ASize: PtrInt): PtrInt;
const
{$ifdef CPU32}
  ONEMASK   =$01010101;
  EIGHTYMASK=$80808080;
{$endif}
{$ifdef CPU64}
  ONEMASK   =$0101010101010101;
  EIGHTYMASK=$8080808080808080;
{$endif}
var
  pnx:PPtrInt absolute PStr;  { To get the contents of the text in PtrInt blocks. x refers to 32 or 64 bits }
  pn8:pint8 absolute pnx;  { To read text as Int8 in the initial and final loops }
  ix:PtrInt absolute pnx;  { To read text as PtrInt in the block loop }
  nx:PtrInt;               { values processed in block loop }
  i,cnt,e:PtrInt;
begin
  Result := 0;

  e := ix+ASize; { End marker }

  { Handle any initial misaligned bytes. }
  cnt := (not (ix-1)) and (sizeof(PtrInt)-1);
  if cnt>ASize then
    cnt := ASize;
  for i := 1 to cnt do
  begin
    { Is this byte NOT the first byte of a character? }
    Result += (pn8^ shr 7) and ((not pn8^) shr 6);
    inc(pn8);
  end;

  { Handle complete blocks }
  for i := 1 to (ASize-cnt) div sizeof(PtrInt) do
  begin
    { Count bytes which are NOT the first byte of a character. }
    nx := ((pnx^ and EIGHTYMASK) shr 7) and ((not pnx^) shr 6);

    {$push}{$overflowchecks off}
    Result += (nx * ONEMASK) >> ((sizeof(PtrInt) - 1) * 8);
    {$pop}
    inc(pnx);
  end;

  { Take care of any left-over bytes. }
  while ix<e do
  begin
    { Is this byte NOT the first byte of a character? }
    Result += (pn8^ shr 7) and ((not pn8^) shr 6);
    inc(pn8);
  end;

  Result := (ASize - Result);
end;

// 获取字符串 AStr 中 UTF8 字符的个数
function UTF8LengthFast(const AStr: string): PtrInt;
begin
  Result := UTF8LengthFast(PChar(AStr), Length(AStr));
  //Result := UTF8Len_SIMD(PChar(AStr), Length(AStr));
end;

// UTF8 索引转换为字节索引
// PStr  ：要在其中进行搜索的源字符串指针
// SSize ：要搜索的最大字节长度
// UPos  ：要搜索的 UTF8 索引位置，从 1 开始
// 返回值：UTF8 索引对应的字节索引，从 1 开始，0 表示 UPos 不合法，大于 Length(AStr) 表示 UPos 超出范围
function UTF8PosToBytePos(const PStr: PChar; const ASize: SizeInt; UPos: SizeInt): SizeInt;
begin
  if UPos < 1 then Result := -1 else Result := 0;

  while (UPos > 1) and (Result < ASize) do begin
    case PStr[Result] of
      // #0..#127: Inc(Pos);
      #192..#223: Inc(Result, 2);
      #224..#239: Inc(Result, 3);
      #240..#247: Inc(Result, 4);
      else Inc(Result);
    end;
    Dec(UPos);
  end;

  Inc(Result)
end;

// UTF8 索引转换为字节索引
// AStr  ：要在其中进行搜索的源字符串
// UPos  ：要搜索的 UTF8 索引位置，从 1 开始
// 返回值：UTF8 索引对应的字节索引，从 1 开始，0 表示 UPos 不合法，大于 Length(AStr) 表示 UPos 超出范围
function UTF8PosToBytePos(const AStr: String; const UPos: SizeInt): SizeInt; inline;
begin
  Result := UTF8PosToBytePos(PChar(AStr), Length(AStr), UPos);
end;

// UTF8 字符串第一个字符的长度
// PStr  ：要计算的字符串的指针
// 返回值：第一个字符的长度
function UTF8CodePointLength(const PStr: PChar): SizeInt;
begin
  case PStr[0] of
    // #0..#127: Inc(Pos);
    #192..#223: Result := 2;
    #224..#239: Result := 3;
    #240..#247: Result := 4;
    else Result := 1;
  end;
end;

// UTF8 字符串第一个字符的长度
// AStr  ：要计算的字符串
// 返回值：第一个字符的长度
function UTF8CodePointLength(const AStr: string): SizeInt; inline;
begin
  Result := UTF8CodePointLength(PChar(AStr));
end;

end.

