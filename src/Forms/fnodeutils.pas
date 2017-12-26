unit fnodeutils;

{$mode objfpc}{$H+}
{$codepage UTF8}   // 字符串常量和字符串字面量需要这个选项

interface

uses
  Classes, SysUtils, Forms, ComCtrls, StdCtrls, ExtCtrls, LCLTranslator, Types,
  LCLType, Controls, Menus, ActnList, Clipbrd, uhistory;

type

  { TformNodeUtils }

  TformNodeUtils = class(TForm)
    chkbNonGreedy        : TCheckBox;
    chkbMultiLine        : TCheckBox;
    chkbIgnoreCase       : TCheckBox;

    pgctMain             : TPageControl;

    tabsSort             : TTabSheet;
    radgSortDirection    : TRadioGroup;
    radgSortOf           : TRadioGroup;

    tabsSplit            : TTabSheet;
    lablSeparator        : TLabel;
    lablTitle            : TLabel;
    combSeparator        : TComboBox;
    combTitle            : TComboBox;
    chkbIncludeSeparator : TCheckBox;
    chkbAddPreNum        : TCheckBox;
    chkbAddSufNum        : TCheckBox;
    editPreNumLen        : TEdit;
    editSufNumLen        : TEdit;

    tabsRename           : TTabSheet;
    lablSearch           : TLabel;
    lablReplace          : TLabel;
    combSearch           : TComboBox;
    combReplace          : TComboBox;
    chkbIgnoreCase2      : TCheckBox;
    chkbMultiLine2       : TCheckBox;
    chkbNonGreedy2       : TCheckBox;
    radgSearchIn         : TRadioGroup;
    chkbSpecifyDepth     : TCheckBox;
    editDepth            : TEdit;

    tabsScript           : TTabSheet;
    pnlScript            : TPanel;
    lstbScriptList       : TListBox;
    spltScript           : TSplitter;
    memoScript           : TMemo;
    editScriptName       : TEdit;
    bttnAddScript        : TButton;
    bttnDeleteScript     : TButton;
    bttnModifyScript     : TButton;
    chkbSpecifyDepth2    : TCheckBox;
    editDepth2           : TEdit;

    actlScript           : TActionList;
    actnCut              : TAction;
    actnCopy             : TAction;
    actnPaste            : TAction;
    actnSelectAll        : TAction;
    actnDelete           : TAction;
    actnUndo             : TAction;
    actnRedo             : TAction;

    menuScript           : TPopupMenu;
    pmiCut               : TMenuItem;
    pmiCopy              : TMenuItem;
    pmiPaste             : TMenuItem;
    pmiSelectAll         : TMenuItem;
    pmiDelete            : TMenuItem;
    pmiSplitter          : TMenuItem;
    pmiUndo              : TMenuItem;
    pmiRedo              : TMenuItem;

    bttnOK               : TButton;
    bttnCancel           : TButton;

    procedure bttnAddScriptClick(Sender: TObject);
    procedure bttnCancelClick(Sender: TObject);
    procedure bttnDeleteScriptClick(Sender: TObject);
    procedure bttnModifyScriptClick(Sender: TObject);
    procedure bttnOKClick(Sender: TObject);
    procedure chkbAddPreNumChange(Sender: TObject);
    procedure chkbAddSufNumChange(Sender: TObject);
    procedure chkbSpecifyDepth2Change(Sender: TObject);
    procedure chkbSpecifyDepthChange(Sender: TObject);
    procedure combReplaceCloseUp(Sender: TObject);
    procedure combSearchCloseUp(Sender: TObject);
    procedure combSeparatorCloseUp(Sender: TObject);
    procedure combTitleCloseUp(Sender: TObject);
    procedure editMouseWheel1(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure editScriptNameKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure lstbScriptListSelectionChange(Sender: TObject; User: boolean);

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

    procedure PerformSort;
    procedure PerformSplit;
    procedure PerformRename;
    procedure PerformScript;

    procedure CreateScriptHistory(Index: integer);
    procedure DestroyScriptHistory(Index: integer);
    procedure ClearScriptHistory;
  public

  end;

var
  formNodeUtils: TformNodeUtils;

resourcestring
  Res_SortDirection = 'Ascending'#10'Descending';
  Res_SortOf        = 'Sibling'#10'Children';
  Res_SearchIn      = 'Node Name'#10'Note';
  Res_RenameWarning = 'Rename multiple nodes, this operation can''t be undo, do you want to continue?';
  Res_ScriptWarning = 'Replace multiple notes, this operation can''t be undo, do you want to continue?';

implementation

uses
  fmain, uconfig, ucommon;

{$R *.lfm}

{ TformNodeUtils }

procedure TformNodeUtils.FormCreate(Sender: TObject);
begin
  FScripts := TStringList.Create;

  // 初始化窗口状态
  if Config.KeepNodeUtilsFormRect then begin
    BoundsRect := Config.NodeUtilsFormRect;
    AutoSize := False;
  end;

  if Screen.Fonts.IndexOf(Config.WindowFontName) <> -1 then
    Font.Name := Config.WindowFontName;
  Font.Size := Config.WindowFontSize;

  // 这些资源需要手动载入
  radgSortDirection.Items.Text  := Res_SortDirection;
  radgSortOf.Items.Text         := Res_SortOf;

  chkbIgnoreCase.Caption        := Res_IgnoreCase;
  chkbMultiLine.Caption         := Res_MultiLine;
  chkbNonGreedy.Caption         := Res_NonGreedy;

  chkbIgnoreCase2.Caption       := Res_IgnoreCase;
  chkbMultiLine2.Caption        := Res_MultiLine;
  chkbNonGreedy2.Caption        := Res_NonGreedy;

  lablSearch.Caption            := Res_TextSearch;
  lablReplace.Caption           := Res_TextReplace;

  radgSearchIn.Items.Text       := Res_SearchIn;
  chkbSpecifyDepth.Caption      := Res_ActsDepth;
  chkbSpecifyDepth2.Caption     := Res_ActsDepth;

  tabsScript.Caption            := Res_TextScript;
  bttnAddScript.Caption         := Res_AddScript;
  bttnDeleteScript.Caption      := Res_DeleteScript;
  bttnModifyScript.Caption      := Res_ModifyScript;

  // 初始化控件状态
  if (Config.NodeUtilsTabIndex < 0) or (Config.NodeUtilsTabIndex >= pgctMain.PageCount) then
    Config.NodeUtilsTabIndex := 0;

  pgctMain.TabIndex             := Config.NodeUtilsTabIndex;

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

  radgSortDirection.ItemIndex   := Config.SortDirection;
  radgSortOf.ItemIndex          := Config.SortOf;

  combSeparator.Text            := Config.SeparatorText;
  combTitle.Text                := Config.TitleText;

  combSeparator.Items           := Config.RecentSeparator;
  combTitle.Items               := Config.RecentTitle;

  chkbIgnoreCase.Checked        := Config.SplitIgnoreCase;
  chkbMultiLine.Checked         := Config.SplitMultiLine;
  chkbNonGreedy.Checked         := Config.SplitNonGreedy;

  chkbIncludeSeparator.Checked  := Config.IncludeSeparator;

  chkbAddPreNum.Checked         := True;
  chkbAddPreNum.Checked         := Config.AddPrefixNumber;

  chkbAddSufNum.Checked         := True;
  chkbAddSufNum.Checked         := Config.AddSuffixNumber;

  editPreNumLen.Text            := IntToStr(Config.PrefixNumberLength);
  editSufNumLen.Text            := IntToStr(Config.SuffixNumberLength);

  combSearch.Text               := Config.RenameSearchText;
  combReplace.Text              := Config.RenameReplaceText;

  combSearch.Items              := Config.RenameRecentSearch;
  combReplace.Items             := Config.RenameRecentReplace;

  chkbIgnoreCase2.Checked       := Config.RenameIgnoreCase;
  chkbMultiLine2.Checked        := Config.RenameMultiLine;
  chkbNonGreedy2.Checked        := Config.RenameNonGreedy;

  radgSearchIn.ItemIndex        := Config.RenameSearchIn;

  chkbSpecifyDepth.Checked      := True;
  chkbSpecifyDepth.Checked      := Config.RenameSpecifyDepth;
  editDepth.Text                := IntToStr(Config.RenameDepth);

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

  chkbSpecifyDepth2.Checked     := True;
  chkbSpecifyDepth2.Checked     := Config.ScriptSpecifyDepth;
  editDepth2.Text               := IntToStr(Config.ScriptDepth);

  if Config.SwapOKCancel then begin
    bttnOK.Caption              := Res_CaptionCancel;
    bttnOK.Cancel               := True;

    bttnCancel.Caption          := Res_CaptionExecute;
    // bttnCancel.Default       := True;
  end else begin
    bttnOK.Caption              := Res_CaptionExecute;
    // bttnOK.Default           := True;

    bttnCancel.Caption          := Res_CaptionCancel;
    bttnCancel.Cancel           := True;
  end;

  ActionUpdate(nil);
end;

procedure TformNodeUtils.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // 保存窗口状态
  Config.NodeUtilsFormRect      := BoundsRect;
  Config.NodeUtilsTabIndex      := pgctMain.TabIndex;

  combSeparator.Text            := Escape(combSeparator.Text);
  combTitle.Text                := Escape(combTitle.Text);

  // 保存控件状态
  Config.SortDirection          := radgSortDirection.ItemIndex;
  Config.SortOf                 := radgSortOf.ItemIndex;

  Config.SeparatorText          := combSeparator.Text;
  Config.TitleText              := combTitle.Text;

  Config.SplitIgnoreCase        := chkbIgnoreCase.Checked;
  Config.SplitMultiLine         := chkbMultiLine.Checked;
  Config.SplitNonGreedy         := chkbNonGreedy.Checked;

  Config.IncludeSeparator       := chkbIncludeSeparator.Checked;

  Config.AddPrefixNumber        := chkbAddPreNum.Checked;
  Config.AddSuffixNumber        := chkbAddSufNum.Checked;

  Config.PrefixNumberLength     := StrToIntDef(editPreNumLen.Text, 3);
  Config.SuffixNumberLength     := StrToIntDef(editSufNumLen.Text, 3);

  Config.RenameSearchText       := combSearch.Text;
  Config.RenameReplaceText      := combReplace.Text;

  Config.RenameIgnoreCase       := chkbIgnoreCase2.Checked;
  Config.RenameMultiLine        := chkbMultiLine2.Checked;
  Config.RenameNonGreedy        := chkbNonGreedy2.Checked;

  Config.RenameSearchIn         := radgSearchIn.ItemIndex;

  Config.RenameSpecifyDepth     := chkbSpecifyDepth.Checked;
  Config.RenameDepth            := StrToIntDef(editDepth.Text, 1);

  Config.ScriptSpecifyDepth     := chkbSpecifyDepth2.Checked;
  Config.ScriptDepth            := StrToIntDef(editDepth2.Text, 1);

  // 保存脚本（必须在销毁历史记录之前）
  SaveScripts;

  ClearScriptHistory;

  // 注意释放顺序要与创建时相反，这关系到 Application.OnIdle 的复位
  // FTextUtils 中也有历史记录管理器，所以这两个窗体不能同时存在
  // 否则可能导致 Application.OnIdle 无法正确复位
  FreeAndNil(FMemoHistory);
  FreeAndNil(FEditHistory);

  FreeAndNil(FScripts);

  CloseAction                   := caFree;
  formNodeUtils                 := nil;
end;

procedure TformNodeUtils.bttnOKClick(Sender: TObject);
begin
  if Config.SwapOKCancel then CancelEvent else OKEvent;
end;

procedure TformNodeUtils.bttnCancelClick(Sender: TObject);
begin
  if Config.SwapOKCancel then OKEvent else CancelEvent;
end;

procedure TformNodeUtils.OKEvent;
var
  Index: integer;
begin
  case pgctMain.ActivePageIndex of
    2: if (combSearch.Text = '') or (not chkbSpecifyDepth.Checked or (StrToIntDef(editDepth.Text, 1) <> 1)) and
      (Application.MessageBox(PChar(Res_RenameWarning), PChar(AppTitle), MB_YESNO) <> ID_YES) then
        Exit;
    3: if (memoScript.Text = '') or (not chkbSpecifyDepth2.Checked or (StrToIntDef(editDepth2.Text, 1) <> 1)) and
      (Application.MessageBox(PChar(Res_ScriptWarning), PChar(AppTitle), MB_YESNO) <> ID_YES) then
        Exit;
  end;

  Hide;

  formMain.SubmitNote;

  // 保存分隔符文本
  if combSeparator.Text <> '' then begin
    combSeparator.Text := Escape(combSeparator.Text);
    Index := Config.RecentSeparator.IndexOf(combSeparator.Text);
    if Index >= 0 then
      Config.RecentSeparator.Delete(Index)
    else if Config.RecentSeparator.Count >= Config.RecentCountLimit then
      Config.RecentSeparator.Delete(Config.RecentSeparator.Count - 1);

    Config.RecentSeparator.Insert(0, combSeparator.Text);
  end;

  // 保存标题文本
  if combTitle.Text <> '' then begin
    combTitle.Text := Escape(combTitle.Text);
    Index := Config.RecentTitle.IndexOf(combTitle.Text);
    if Index >= 0 then
      Config.RecentTitle.Delete(Index)
    else if Config.RecentTitle.Count >= Config.RecentCountLimit then
      Config.RecentTitle.Delete(Config.RecentTitle.Count - 1);

    Config.RecentTitle.Insert(0, combTitle.Text);
  end;

  // 保存分隔符文本
  if combSearch.Text <> '' then begin
    combSearch.Text := Escape(combSearch.Text);
    Index := Config.RenameRecentSearch.IndexOf(combSearch.Text);
    if Index >= 0 then
      Config.RenameRecentSearch.Delete(Index)
    else if Config.RenameRecentSearch.Count >= Config.RecentCountLimit then
      Config.RenameRecentSearch.Delete(Config.RenameRecentSearch.Count - 1);

    Config.RenameRecentSearch.Insert(0, combSearch.Text);
  end;

  // 保存标题文本
  if combReplace.Text <> '' then begin
    combReplace.Text := Escape(combReplace.Text);
    Index := Config.RenameRecentReplace.IndexOf(combReplace.Text);
    if Index >= 0 then
      Config.RenameRecentReplace.Delete(Index)
    else if Config.RenameRecentReplace.Count >= Config.RecentCountLimit then
      Config.RenameRecentReplace.Delete(Config.RenameRecentReplace.Count - 1);

    Config.RenameRecentReplace.Insert(0, combReplace.Text);
  end;

  case pgctMain.ActivePageIndex of
    0: PerformSort;
    1: PerformSplit;
    2: PerformRename;
    3: PerformScript;
  end;

  Close;
end;

procedure TformNodeUtils.CancelEvent;
begin
  Close;
end;

procedure TformNodeUtils.PerformSort;
begin
  formMain.SortNode(radgSortOf.ItemIndex = 0, radgSortDirection.ItemIndex = 1);
end;

procedure TformNodeUtils.PerformSplit;
var
  HeadStr, SeparatorPattern, TitlePattern: string;
  IncludeSeparator: boolean;
  PreNumLen, SufNumLen: integer;
begin
  HeadStr := '';
  if chkbIgnoreCase.Checked    then HeadStr := HeadStr + '(?i)'   else HeadStr := HeadStr + '(?-i)';
  if chkbMultiLine.Checked     then HeadStr := HeadStr + '(?m-s)' else HeadStr := HeadStr + '(?s-m)';
  if chkbNonGreedy.Checked     then HeadStr := HeadStr + '(?-g)'  else HeadStr := HeadStr + '(?g)';

  SeparatorPattern := combSeparator.Text;
  if SeparatorPattern <> '' then SeparatorPattern := HeadStr + SeparatorPattern;

  TitlePattern     := combTitle.Text;
  if TitlePattern <> '' then TitlePattern := HeadStr + TitlePattern;

  IncludeSeparator := chkbIncludeSeparator.Checked;

  if chkbAddPreNum.Checked then
    PreNumLen := StrToIntDef(editPreNumLen.Text, 0)
  else
    PreNumLen := 0;

  if chkbAddSufNum.Checked then
    SufNumLen := StrToIntDef(editSufNumLen.Text, 0)
  else
    SufNumLen := 0;

  formMain.SplitNote(SeparatorPattern, TitlePattern, IncludeSeparator, PreNumLen, SufNumLen);
end;

procedure TformNodeUtils.PerformRename;
var
  HeadStr, SearchText, NameText: string;
  SearchInNote: boolean;
  Depth: Integer;
begin
  HeadStr := '';
  if chkbIgnoreCase2.Checked    then HeadStr := HeadStr + '(?i)'   else HeadStr := HeadStr + '(?-i)';
  if chkbMultiLine2.Checked     then HeadStr := HeadStr + '(?m-s)' else HeadStr := HeadStr + '(?s-m)';
  if chkbNonGreedy2.Checked     then HeadStr := HeadStr + '(?-g)'  else HeadStr := HeadStr + '(?g)';

  SearchText := combSearch.Text;
  if SearchText <> '' then SearchText := HeadStr + SearchText;

  NameText     := combReplace.Text;
  if NameText = '' then Exit;

  SearchInNote := radgSearchIn.ItemIndex = 1;

  if chkbSpecifyDepth.Checked then
    Depth := StrToIntDef(editDepth.Text, 1)
  else
    Depth := AllDepth;

  formMain.RenameNodes(SearchText, NameText, SearchInNote, Depth);
end;

procedure TformNodeUtils.PerformScript;
var
  Depth: integer;
begin
  if chkbSpecifyDepth2.Checked then
    Depth := StrToIntDef(editDepth2.Text, 1)
  else
    Depth := AllDepth;
  formMain.ScriptReplace(memoScript.Text, Depth);
end;

procedure TformNodeUtils.editMouseWheel1(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Num: Integer;
begin
  Num := StrToIntDef((Sender as TEdit).Text, 1);
  if WheelDelta > 0 then Inc(Num) else Dec(Num);
  if Num < 1 then Num := 1;
  (Sender as TEdit).Text := IntToStr(Num);
end;

procedure TformNodeUtils.combSeparatorCloseUp(Sender: TObject);
var
  Index: Integer;
begin
  Index := combSeparator.ItemIndex;
  if IsKeyDown(VK_SHIFT) and (Index >= 0) then begin
    Config.RecentSeparator.Delete(Index);
    combSeparator.Items.Delete(Index);
  end;
end;

procedure TformNodeUtils.combTitleCloseUp(Sender: TObject);
var
  Index: Integer;
begin
  Index := combTitle.ItemIndex;
  if IsKeyDown(VK_SHIFT) and (Index >= 0) then begin
    Config.RecentTitle.Delete(Index);
    combTitle.Items.Delete(Index);
  end;
end;

procedure TformNodeUtils.combSearchCloseUp(Sender: TObject);
var
  Index: Integer;
begin
  Index := combSearch.ItemIndex;
  if IsKeyDown(VK_SHIFT) and (Index >= 0) then begin
    Config.RenameRecentSearch.Delete(Index);
    combSearch.Items.Delete(Index);
  end;
end;

procedure TformNodeUtils.combReplaceCloseUp(Sender: TObject);
var
  Index: Integer;
begin
  Index := combReplace.ItemIndex;
  if IsKeyDown(VK_SHIFT) and (Index >= 0) then begin
    Config.RenameRecentReplace.Delete(Index);
    combReplace.Items.Delete(Index);
  end;
end;

procedure TformNodeUtils.bttnAddScriptClick(Sender: TObject);
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

procedure TformNodeUtils.bttnDeleteScriptClick(Sender: TObject);
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

procedure TformNodeUtils.bttnModifyScriptClick(Sender: TObject);
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

procedure TformNodeUtils.editScriptNameKeyPress(Sender: TObject; var Key: char);
begin
  if Key = Chr(VK_RETURN) then
    bttnModifyScriptClick(Sender);
end;

procedure TformNodeUtils.LoadScrips;
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

procedure TformNodeUtils.SaveScripts;
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

procedure TformNodeUtils.RenameScript(OldName, NewName: string);
var
  Index: Integer;
begin
  Index := FScripts.IndexOfName(OldName);
  if Index = -1 then Exit;
  FScripts[Index] := NewName + '=' + FScripts.ValueFromIndex[Index];
end;

procedure TformNodeUtils.lstbScriptListSelectionChange(Sender: TObject; User: boolean);
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

procedure TformNodeUtils.chkbAddPreNumChange(Sender: TObject);
begin
  editPreNumLen.Enabled := chkbAddPreNum.Checked;
end;

procedure TformNodeUtils.chkbAddSufNumChange(Sender: TObject);
begin
  editSufNumLen.Enabled := chkbAddSufNum.Checked;
end;

procedure TformNodeUtils.chkbSpecifyDepthChange(Sender: TObject);
begin
  editDepth.Enabled := chkbSpecifyDepth.Checked;
end;

procedure TformNodeUtils.chkbSpecifyDepth2Change(Sender: TObject);
begin
  editDepth2.Enabled := chkbSpecifyDepth2.Checked;
end;

procedure TformNodeUtils.EditEnter(Sender: TObject);
begin
  FActiveEdit := Sender as TCustomEdit;

  if FActiveEdit = editScriptName then
    FActiveHistory := FEditHistory
  else
    FActiveHistory := FMemoHistory;
end;

procedure TformNodeUtils.ActionExecute(Sender: TObject);
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

procedure TformNodeUtils.ActionUpdate(Sender: TObject);
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

procedure TformNodeUtils.pgctMainChange(Sender: TObject);
begin
  ActionUpdate(nil);
end;

procedure TformNodeUtils.CreateScriptHistory(Index: integer);
var
  i: integer;
begin
  if (Index < 0) or (Index > FScripts.Count) then Exit;

  SetLength(FScriptHistory, Length(FScriptHistory) + 1);

  for i := High(FScriptHistory) downto Index + 1 do
    FScriptHistory[i] := FScriptHistory[i - 1];

  FScriptHistory[Index] := THistory.Create;
end;

procedure TformNodeUtils.DestroyScriptHistory(Index: integer);
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

procedure TformNodeUtils.ClearScriptHistory;
var
  i: integer;
begin
  for i := 0 to High(FScriptHistory) do
    FScriptHistory[i].Free;
  FMemoHistory.SwitchHistory(nil);
  SetLength(FScriptHistory, 0);
end;

end.

