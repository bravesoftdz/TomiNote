unit ftextutils;

{$mode objfpc}{$H+}
{$codepage UTF8}   // 字符串常量和字符串字面量需要这个选项

interface

uses
  Classes, SysUtils, Forms, StdCtrls, ComCtrls, ExtCtrls, LCLType,
  LCLTranslator, Controls, Menus, ActnList, Clipbrd, uhistory;

type

  { TformTextUtils }

  TformTextUtils = class(TForm)

    pgctMain              : TPageControl;
    tabsScript            : TTabSheet;
    pnlScript             : TPanel;
    lstbScriptList        : TListBox;
    spltScript            : TSplitter;
    memoScript            : TMemo;
    editScriptName        : TEdit;
    bttnAddScript         : TButton;
    bttnDeleteScript      : TButton;
    bttnModifyScript      : TButton;
    chkbSearchInSelection : TCheckBox;

    actlScript            : TActionList;
    actnCut               : TAction;
    actnCopy              : TAction;
    actnPaste             : TAction;
    actnSelectAll         : TAction;
    actnDelete            : TAction;
    actnUndo              : TAction;
    actnRedo              : TAction;

    menuScript            : TPopupMenu;
    pmiCut                : TMenuItem;
    pmiCopy               : TMenuItem;
    pmiPaste              : TMenuItem;
    pmiSelectAll          : TMenuItem;
    pmiDelete             : TMenuItem;
    pmiSplitter           : TMenuItem;
    pmiUndo               : TMenuItem;
    pmiRedo               : TMenuItem;

    bttnOK                : TButton;
    bttnCancel            : TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure bttnOKClick(Sender: TObject);
    procedure bttnCancelClick(Sender: TObject);

    procedure lstbScriptListSelectionChange(Sender: TObject; User: boolean);
    procedure editScriptNameKeyPress(Sender: TObject; var Key: char);
    procedure bttnAddScriptClick(Sender: TObject);
    procedure bttnDeleteScriptClick(Sender: TObject);
    procedure bttnModifyScriptClick(Sender: TObject);

    procedure EditEnter(Sender: TObject);
    procedure ActionExecute(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure pgctMainChange(Sender: TObject);
  private
    FLastScriptName : string;
    FScripts        : TStringList;

    FEditHistory    : THistoryManager;
    FMemoHistory    : THistoryManager;

    FScriptHistory  : array of THistory;

    FActiveEdit     : TCustomEdit;
    FActiveHistory  : THistoryManager;

    procedure OKEvent;
    procedure CancelEvent;

    procedure LoadScrips;
    procedure SaveScripts;
    procedure RenameScript(OldName, NewName: string);

    procedure CreateScriptHistory(Index: integer);
    procedure DestroyScriptHistory(Index: integer);
    procedure ClearScriptHistory;
  public

  end;

var
  formTextUtils: TformTextUtils;

resourcestring
    Res_NoSelection = 'No text is currently selected. Do you want the Replace operation performed on the entire document?';

implementation

uses
  fmain, uconfig, ucommon;

{$R *.lfm}

{ TformTextUtils }

procedure TformTextUtils.FormCreate(Sender: TObject);
begin
  FScripts := TStringList.Create;

  // 初始化窗口状态
  if Config.KeepTextUtilsFormRect then begin
    BoundsRect := Config.TextUtilsFormRect;
    AutoSize   := False;
  end;

  if Screen.Fonts.IndexOf(Config.WindowFontName) <> -1 then
    Font.Name := Config.WindowFontName;
  Font.Size := Config.WindowFontSize;

  // 这些资源需要手动载入
  tabsScript.Caption       := Res_TextScript;
  bttnAddScript.Caption    := Res_AddScript;
  bttnDeleteScript.Caption := Res_DeleteScript;
  bttnModifyScript.Caption := Res_ModifyScript;

  // 初始化控件状态
  pgctMain.ActivePageIndex := 0;

  // 初始化控件状态（历史记录相关）
  editScriptName.OnEnter   := @EditEnter;
  memoScript.OnEnter       := @EditEnter;

  pmiCut.Action            := actnCut;
  pmiCopy.Action           := actnCopy;
  pmiPaste.Action          := actnPaste;
  pmiSelectAll.Action      := actnSelectAll;
  pmiDelete.Action         := actnDelete;
  pmiSplitter.Caption      := '-';
  pmiUndo.Action           := actnUndo;
  pmiRedo.Action           := actnRedo;

  actnCut.OnExecute        := @ActionExecute;
  actnCopy.OnExecute       := @ActionExecute;
  actnPaste.OnExecute      := @ActionExecute;
  actnSelectAll.OnExecute  := @ActionExecute;
  actnDelete.OnExecute     := @ActionExecute;
  actnUndo.OnExecute       := @ActionExecute;
  actnRedo.OnExecute       := @ActionExecute;

  actnCut.Caption          := formMain.actnCut.Caption;
  actnCopy.Caption         := formMain.actnCopy.Caption;
  actnPaste.Caption        := formMain.actnPaste.Caption;
  actnSelectAll.Caption    := formMain.actnSelectAll.Caption;
  actnDelete.Caption       := formMain.actnDelete.Caption;
  actnUndo.Caption         := formMain.actnUndo.Caption;
  actnRedo.Caption         := formMain.actnRedo.Caption;

  actnUndo.OnUpdate        := @ActionUpdate;

  editScriptName.PopupMenu := menuScript;
  memoScript.PopupMenu     := menuScript;

  // 分别为 TEdit 和 TMemo 创建历史记录管理器
  // 默认历史记录最大尺寸为 32K，默认历史记录最小数量为 10
  FEditHistory := THistoryManager.Create(editScriptName);
  FEditHistory.CreateHistory(True);

  FMemoHistory := THistoryManager.Create(memoScript);

  // 载入脚本
  LoadScrips;

  chkbSearchInSelection.Checked := Config.SearchInSelection;
  lstbScriptList.Width          := Config.ScriptListBarWidth;
  spltScript.Left               := pnlScript.Width;

  if Config.ActiveTheme = BrightThemeID then begin
    lstbScriptList.Font.Color   := Config.BrightFontColor;
    lstbScriptList.Color        := Config.BrightBackColor;
    memoScript.Font.Color       := Config.BrightFontColor;
    memoScript.Color            := Config.BrightBackColor;
  end else begin
    lstbScriptList.Font.Color   := Config.DarkFontColor;
    lstbScriptList.Color        := Config.DarkBackColor;
    memoScript.Font.Color       := Config.DarkFontColor;
    memoScript.Color            := Config.DarkBackColor;
  end;

  lstbScriptList.Font.Name      := Config.TreeBarFontName;
  //lstbScriptList.Font.Size      := Config.TreeBarFontSize;

  memoScript.Font.Name          := Config.NoteBarFontName;
  //memoScript.Font.Size          := Config.NoteBarFontSize;

  if Config.SwapOKCancel then begin
    bttnOK.Caption          := Res_CaptionCancel;
    bttnOK.Cancel           := True;

    bttnCancel.Caption      := Res_CaptionExecute;
    // bttnCancel.Default      := True;
  end else begin
    bttnOK.Caption          := Res_CaptionExecute;
    // bttnOK.Default          := True;

    bttnCancel.Caption      := Res_CaptionCancel;
    bttnCancel.Cancel       := True;
  end;

  ActionUpdate(nil);
end;

procedure TformTextUtils.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // 保存窗口状态
  Config.TextUtilsFormRect  := BoundsRect;

  // 保存控件状态
  Config.SearchInSelection  := chkbSearchInSelection.Checked;
  Config.ScriptListBarWidth := lstbScriptList.Width;

  // 保存脚本（必须在销毁历史记录之前）
  SaveScripts;

  ClearScriptHistory;

  // 注意释放顺序要与创建时相反，这关系到 Application.OnIdle 的复位
  // FNodeUtils 中也有历史记录管理器，所以这两个窗体不能同时存在
  // 否则可能导致 Application.OnIdle 无法正确复位
  FreeAndNil(FMemoHistory);
  FreeAndNil(FEditHistory);

  FreeAndNil(FScripts);

  CloseAction := caFree;
  formTextUtils := nil;
end;

procedure TformTextUtils.bttnOKClick(Sender: TObject);
begin
  if Config.SwapOKCancel then CancelEvent else OKEvent;
end;

procedure TformTextUtils.bttnCancelClick(Sender: TObject);
begin
  if Config.SwapOKCancel then OKEvent else CancelEvent;
end;

procedure TformTextUtils.OKEvent;
begin
  if chkbSearchInSelection.Checked and (formMain.memoNote.SelLength = 0) and
    (Application.MessageBox(PChar(Res_NoSelection), PChar(AppTitle), MB_YESNO + MB_ICONQUESTION) <> ID_YES) then
      Exit;

  Hide;

  with formMain do begin
    SubmitNote;

    case pgctMain.PageIndex of
      0: ScriptReplace(memoScript.Text, chkbSearchInSelection.Checked);
    end;
  end;

  Close;
end;

procedure TformTextUtils.CancelEvent;
begin
  Close;
end;

procedure TformTextUtils.bttnAddScriptClick(Sender: TObject);
begin
  if editScriptName.Text = '' then Exit;

  editScriptName.Text := StringReplace(editScriptName.Text, #10, '\n');
  editScriptName.Text := StringReplace(editScriptName.Text, #13, '\r');
  editScriptName.Text := StringReplace(editScriptName.Text, '=', '_');

  if lstbScriptList.Items.IndexOf(editScriptName.Text) = -1 then begin
    lstbScriptList.Items.Add(editScriptName.Text);
    FScripts.Add(editScriptName.Text + '=');
    SetLength(FScriptHistory, FScripts.Count);
  end;
  lstbScriptList.ItemIndex := lstbScriptList.Items.IndexOf(editScriptName.Text);
  memoScript.ReadOnly := lstbScriptList.ItemIndex = -1;
end;

procedure TformTextUtils.bttnDeleteScriptClick(Sender: TObject);
var
  Index: integer;
begin
  Index := lstbScriptList.ItemIndex;

  if Index = -1 then Exit;

  // 按下 Shift 键可以忽略警告
  if (not IsKeyDown(VK_SHIFT)) and (Application.MessageBox(PChar(Res_DelItemWarning), PChar(Caption), MB_YESNO + MB_ICONWARNING) <> ID_YES) then Exit;

  FScripts.Delete(FScripts.IndexOfName(lstbScriptList.Items[Index]));

  if FLastScriptName = lstbScriptList.Items[Index] then
    FLastScriptName := '';

  lstbScriptList.Items.Delete(Index);
  DestroyScriptHistory(Index);

  if lstbScriptList.Count - 1 >= Index then
    lstbScriptList.ItemIndex := Index
  else if lstbScriptList.Count - 1 >= Index - 1 then
    lstbScriptList.ItemIndex := Index - 1;

  if lstbScriptList.ItemIndex = -1 then begin
    editScriptName.Text := '';
    memoScript.ReadOnly := True;
    memoScript.Text := '';
  end;
end;

procedure TformTextUtils.bttnModifyScriptClick(Sender: TObject);
var
  Index: integer;
  OldName: string;
begin
  Index := lstbScriptList.ItemIndex;
  if (editScriptName.Text = '') or (Index = -1) then Exit;

  OldName := lstbScriptList.Items[Index];
  lstbScriptList.Items[Index] := editScriptName.Text;
  RenameScript(OldName, editScriptName.Text);
  FLastScriptName := editScriptName.Text;
end;

procedure TformTextUtils.editScriptNameKeyPress(Sender: TObject; var Key: char);
begin
  if Key = Chr(VK_RETURN) then
    bttnModifyScriptClick(Sender);
end;

procedure TformTextUtils.LoadScrips;
var
  ConfigFile: string;
  Strs: TStringList;
  i: Integer;
  Line: string;
  Title, Content: string;
begin
  Line := '';
  Title := '';
  Content := '';
  ConfigFile := ConcatPaths([formMain.ConfigDir, ScriptFile]);

  Strs := TStringList.Create;
  try
    if FileExists(ConfigFile) then Strs.LoadFromFile(ConfigFile);

    for i := 0 to Strs.Count - 1 do begin
      Line := Strs[i];
      if (Line <> '') and (Line[1] = '[') then begin
        if Title <> '' then begin
          FScripts.Add(Title + '=' + Content);
          lstbScriptList.Items.Add(Title);
          Content := '';
        end;
        Title := Copy(Line, 2, Length(Line) - 2);
      end else if Content = '' then
        Content := Line
      else
        Content := Content + #10 + Line;
    end;

    if Title <> '' then begin
      FScripts.Add(Title + '=' + Content);
      lstbScriptList.Items.Add(Title);
    end;
  finally
    Strs.Free;
  end;
  SetLength(FScriptHistory, FScripts.Count);

  if lstbScriptList.Count > Config.LastScriptID then
    lstbScriptList.ItemIndex := Config.LastScriptID
  else if lstbScriptList.Count > 0 then
    lstbScriptList.ItemIndex := 0;

  memoScript.ReadOnly := lstbScriptList.ItemIndex = -1;
end;

procedure TformTextUtils.SaveScripts;
var
  ConfigFile: string;
  Strs: TStringList;
  i: integer;
begin
  ConfigFile := ConcatPaths([formMain.ConfigDir, ScriptFile]);

  if FScripts.Count = 0 then begin
    if FileExists(ConfigFile) then DeleteFile(ConfigFile);
    Exit;
  end;

  lstbScriptListSelectionChange(lstbScriptList, False);

  Strs := TStringList.Create;
  try
    for i := 0 to FScripts.Count - 1 do begin
      Strs.Add('[' + FScripts.Names[i] + ']');
      Strs.Add(FScripts.ValueFromIndex[i]);
    end;
    Strs.SaveToFile(ConfigFile);
  finally
    Strs.Free;
  end;

  Config.LastScriptID := lstbScriptList.ItemIndex;
end;

procedure TformTextUtils.RenameScript(OldName, NewName: string);
var
  Index: Integer;
begin
  Index := FScripts.IndexOfName(OldName);
  if Index = -1 then Exit;
  FScripts[Index] := NewName + '=' + FScripts.ValueFromIndex[Index];
end;

procedure TformTextUtils.lstbScriptListSelectionChange(Sender: TObject; User: boolean);
var
  Index: Integer;
begin
  if FLastScriptName <> '' then
    FScripts.Values[FLastScriptName] := memoScript.Text;

  Index := lstbScriptList.ItemIndex;
  memoScript.ReadOnly := Index = -1;

  if lstbScriptList.ItemIndex = -1 then
    FLastScriptName := ''
  else
    FLastScriptName := lstbScriptList.Items[Index];

  if FLastScriptName <> '' then begin
    FMemoHistory.SwitchHistory(nil);
    memoScript.Text := FScripts.Values[FLastScriptName];
    if FScriptHistory[Index] = nil then
      FScriptHistory[Index] := THistory.Create;
    FMemoHistory.SwitchHistory(FScriptHistory[Index]);
    FMemoHistory.Enabled := True;
  end;

  FEditHistory.Enabled := False;
  editScriptName.Text := FLastScriptName;
  FEditHistory.Reset;
  FEditHistory.Enabled := True;
end;

procedure TformTextUtils.EditEnter(Sender: TObject);
begin
  FActiveEdit := Sender as TCustomEdit;

  if FActiveEdit = editScriptName then
    FActiveHistory := FEditHistory
  else
    FActiveHistory := FMemoHistory;
end;

procedure TformTextUtils.ActionExecute(Sender: TObject);
begin
  case (Sender as TAction).Name of
  'actnCut'      : FActiveHistory.Cut;
  'actnCopy'     : FActiveEdit.CopyToClipboard;
  'actnPaste'    : FActiveHistory.Paste;
  'actnSelectAll': FActiveEdit.SelectAll;
  'actnDelet'    : FActiveHistory.Delete;
  'actnUndo'     : FActiveHistory.Undo;
  'actnRedo'     : FActiveHistory.Redo;
  end;
end;

procedure TformTextUtils.ActionUpdate(Sender: TObject);
begin
  if pgctMain.ActivePage = tabsScript then begin
    actnUndo     .Enabled := (FActiveHistory <> nil) and (FActiveHistory.CanUndo);
    actnRedo     .Enabled := (FActiveHistory <> nil) and (FActiveHistory.CanRedo);
    actnCut      .Enabled := (FActiveEdit <> nil) and (FActiveEdit.SelLength > 0);
    actnCopy     .Enabled := actnCut.Enabled;
    actnPaste    .Enabled := ClipBoard.HasFormat(CF_TEXT);
    actnSelectAll.Enabled := actnCut.Enabled;
    actnDelete   .Enabled := actnCut.Enabled;
  end else begin
    actnUndo     .Enabled := False;
    actnRedo     .Enabled := False;
    actnCut      .Enabled := False;
    actnCopy     .Enabled := False;
    actnPaste    .Enabled := False;
    actnSelectAll.Enabled := False;
    actnDelete   .Enabled := False;
  end;
end;

procedure TformTextUtils.pgctMainChange(Sender: TObject);
begin
  ActionUpdate(nil);
end;

procedure TformTextUtils.CreateScriptHistory(Index: integer);
var
  i: integer;
begin
  if (Index < 0) or (Index > FScripts.Count) then Exit;

  SetLength(FScriptHistory, Length(FScriptHistory) + 1);

  for i := High(FScriptHistory) downto Index + 1 do
    FScriptHistory[i] := FScriptHistory[i - 1];

  FScriptHistory[Index] := THistory.Create;
end;

procedure TformTextUtils.DestroyScriptHistory(Index: integer);
var
  i: integer;
begin
  if (Index < 0) or (Index >= FScripts.Count) then Exit;

  if FMemoHistory.History = FScriptHistory[Index] then
    FMemoHistory.SwitchHistory(nil);
  FScriptHistory[Index].Free;

  for i := Index to High(FScriptHistory) - 1 do
    FScriptHistory[i] := FScriptHistory[i + 1];

  SetLength(FScriptHistory, Length(FScriptHistory) - 1);
end;

procedure TformTextUtils.ClearScriptHistory;
var
  i: integer;
begin
  for i := 0 to High(FScriptHistory) do
    FScriptHistory[i].Free;
  FMemoHistory.SwitchHistory(nil);
  SetLength(FScriptHistory, 0);
end;

end.

