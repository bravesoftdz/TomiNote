unit utreedb;

{$mode objfpc}{$H+}
{$codepage UTF8}   // 字符串常量和字符串字面量需要这个选项

interface

uses
  Classes, SysUtils, sqlite3dyn, sqlite3conn, sqldb, LazUTF8, RegExpr, IniFiles, ucommon, uscript;

type

  // 从 ComCtrls 中复制过来的，元素顺序必须与 ComCtrls 中保持一直
  TAttachMode = (
    naAdd,
    naAddFirst,
    naAddChild,
    naAddChildFirst,
    naInsert,
    naInsertBehind
    );

  { TBackupThread }

  TBackupThread = class(TThread)
    FDatabaseName: string;
    FBackupFile: string;
  protected
    procedure Execute; override;
  public
    constructor Create(FromFile, ToFile: string);
  end;

  { TDBConfig }

  TDBConfig = class(TObject)
    DatabaseVersion: string;
    LastNodeID: Integer;
    AutoBackupRemaining: integer;
    ChangedAfterBackup: boolean;
    DataBaseIsOpened: boolean;
    public
      procedure Load(AData: string);
      function  DataString: string;
  end;

  { TTreeDB }

  TTreeDB = class(TObject)
  private
    FSQLConn: TSQLite3Connection;
    FSQLQuery: TSQLQuery;
    FSQLTran: TSQLTransaction;
    // 有时需要以只读的方式打开数据库，比如多线程导出数据库的时候，此时可以
    // 设置 FAutoOpenTran 为 False，避免在关闭数据库的时候对数据库进行写入操作。
    FAutoOpenTran: boolean;

    FDatabaseName: string;
    FActive: boolean;
    FChanged: boolean;
    FActiveChanged: TNotifyEvent;
    FDBConfig: TDBConfig;

    procedure CreateTable;
    function  TableExists(TableName: string): boolean;
    procedure InitTable;
    // 从储备库(rowid=3)中取出一个节点重新使用
    function  ReuseNode: Integer;
    // 向数据库中添加一个节点
    function  CreateNewNode(Name, Note: string): Integer;
    // 将一个节点从树上脱离
    procedure DetachNode(ID: Integer);
    // 将一个节点停靠在树上
    procedure AttachNode(ID: Integer; ToID: Integer; mode: TAttachMode);
    procedure SetParent(ID: Integer; ParentID: Integer);

    procedure DataChanged(AChanged: boolean);
    procedure ActiveChanged(AActive: boolean);

    procedure SaveConfig;
  public
    constructor Create;
    destructor Destroy; override;

    // 数据库文件
    function  OpenDB(FileName: string): boolean;
    procedure SaveDB;
    function  CloseDB(Save: boolean): boolean;
    function  BackupDB(BackupDir: string; Count: integer): boolean;

    // 基本读写
    function  AddNode(Name, Note: string; ToID: Integer; mode: TAttachMode): Integer;
    procedure DelNode(ID: Integer);
    procedure RecycleNode(ID: Integer);
    procedure EmptyRecycler;
    procedure MoveNode(FromID, ToID: Integer; Mode: TAttachMode);
    function  UpDownNode(ID: Integer; Up: boolean): boolean;
    function  MoveLeft(ID: Integer): boolean;
    function  MoveRight(ID: Integer): boolean;
    function  CopyNode(FromID, ToID: Integer; Mode: TAttachMode): Integer;
    function  GetParent(ID: Integer): Integer;
    function  GetChildren(ID: Integer): TBoundArray;
    procedure SetChildren(ID: Integer; Children: TBoundArray);
    function  GetName(ID: Integer): string;
    procedure SetName(ID: Integer; Name: string);
    function  GetNote(ID: Integer): string;
    procedure SetNote(ID: Integer; Note: string);

    // 导入和导出
    function  ImportFile(FileName: string; IncludeExt: boolean; ToID: Integer;
      mode: TAttachMode; StripTailSpace: boolean): Integer;
    function  ImportDir(FromDir: string; IncludeRoot: boolean; IncludeExt: boolean;
      ToID: Integer; mode: TAttachMode; StripTailSpace: boolean): Integer;
    function  ImportDB(FromDBFile: string; FromID, ToID: Integer; mode: TAttachMode): Integer;

    procedure ExportToDir(ID: Integer; ToDir: string; Ext: string; Depth: integer;
      StripTailSpace: boolean);
    procedure ExportToFile(ID: Integer; ToFile, Splitter: string; Depth: integer;
      StripTailSpace: boolean);
    function  ExportToDB(ID, ToID: Integer; ToDBFile: string; Depth: integer): boolean;

    // 搜索和替换
    procedure Search(ID: Integer; ASearchText: string; IncludeName, IncludeNote: boolean;
      Depth: integer; IgnoreCase: boolean; Count: Integer; var Rst: TSearchResult);
    procedure Replace(ID: Integer; ASearchText, AReplaceText: string; IncludeName, IncludeNote: boolean;
      Depth: integer; IgnoreCase: boolean; Count: Integer; var Rst: TSearchResult);
    procedure RegSearch(ID: Integer; ASearchText: string; IncludeName, IncludeNote: boolean;
      Depth: integer; Count: Integer; var Rst: TSearchResult);
    procedure RegReplace(ID: Integer; ASearchText, AReplaceText: string; IncludeName, IncludeNote: boolean;
      Depth: integer; Count: Integer; var Rst: TSearchResult);

    procedure RenameNodes(ID: integer; ASearchText, AReplaceText: string; SearchNote: boolean;
      Depth: integer; var Rst: TSearchResult);

    procedure ScriptReplace(ID: Integer; ScriptText: string; Depth: integer; Rst: TSearchResult);

    // 属性
    property  AutoOpenTran: boolean read FAutoOpenTran write FAutoOpenTran;
    property  DatabaseName: string read FDatabaseName;
    property  DBConfig: TDBConfig read FDBConfig write FDBConfig;
    property  Active: boolean read FActive;
    property  Changed: boolean read FChanged;
    property  OnActiveChanged: TNotifyEvent read FActiveChanged write FActiveChanged;
  end;

const
  // 数据库版本
  Version       = '1.0';

  // 数据表名称和字段名称
  NoteTable     = 'tnote';
  ParentField   = 'parent';
  ChildrenField = 'children';
  NameField     = 'name';
  NoteField     = 'note';

  AllDepth      = 0;

const
  // 固定 ID
  FreeID      = 0;
  RootID      = 1;
  RecyclerID  = 2;
  ReserveID   = 3;
  ConfigID    = 4;
  SpareID     = 5;

  // 在导出节点时，如果一个节点含有子节点，则将其内容保存在指定名称的文件中
  DirNode = '$DirectoryNode$';

implementation

var
  // SQLite3 的动态链接库文件
  LibFile: string;

{ TBackupThread }

constructor TBackupThread.Create(FromFile, ToFile: string);
begin
  FDatabaseName := FromFile;
  FBackupFile := ToFile;
  inherited Create(False);
end;

// 多线程备份数据库
procedure TBackupThread.Execute;
var
  ATreeDB: TTreeDB;
begin
  Self.FreeOnTerminate := True;
  ATreeDB := TTreeDB.Create;
  ATreeDB.AutoOpenTran := False;
  try
    ATreeDB.OpenDB(FDatabaseName);
    ATreeDB.ExportToDB(RootID, RootID, FBackupFile, AllDepth);
    ATreeDB.ExportToDB(RecyclerID, RecyclerID, FBackupFile, AllDepth);
    ATreeDB.CloseDB(False);
  finally
    ATreeDB.Free;
  end;
end;

{ TDBConfig }

// 解析数据库配置信息
procedure TDBConfig.Load(AData: string);
var
  AStream: TStringStream;
  IniFile: TIniFile;
begin
  AStream := TStringStream.Create(AData);
  try
    IniFile := TIniFile.Create(TStream(AStream));
    try
      DatabaseVersion     := IniFile.ReadString ('Config', 'Version', Version);
      LastNodeID          := IniFile.ReadInteger('Config', 'LastNodeID', 0);
      AutoBackupRemaining := IniFile.ReadInteger('Config', 'AutoBackupRemaining', -1);
      ChangedAfterBackup  := IniFile.ReadBool   ('Config', 'ChangedAfterBackup', False);
      DataBaseIsOpened    := IniFile.ReadBool   ('Config', 'DataBaseIsOpened', False);
    finally
      IniFile.Free;
    end;
  finally
    AStream.Free;
  end;
end;

// 序列化数据库配置信息
function TDBConfig.DataString: string;
var
  AStream: TStringStream;
  IniFile: TIniFile;
begin
  AStream := TStringStream.Create('');
  try
    IniFile := TIniFile.Create(TStream(AStream));
    try
      IniFile.WriteString ('Config', 'Version', DatabaseVersion);
      IniFile.WriteInteger('Config', 'LastNodeID', LastNodeID);
      IniFile.WriteInteger('Config', 'AutoBackupRemaining', AutoBackupRemaining);
      IniFile.WriteBool   ('Config', 'ChangedAfterBackup', ChangedAfterBackup);
      IniFile.WriteBool   ('Config', 'DataBaseIsOpened', DataBaseIsOpened);
    finally
      IniFile.Free;
    end;
    Result := AStream.DataString;
  finally
    AStream.Free;
  end;
end;

{ TTreeDB }

constructor TTreeDB.Create;
begin
  inherited Create;

  FDBConfig := TDBConfig.Create;

  FDatabaseName := '';
  FActive := False;
  FChanged := False;
  FAutoOpenTran := True;

  FSQLConn := TSQLite3Connection.Create(nil);
  FSQLTran := TSQLTransaction.Create(nil);
  FSQLQuery := TSQLQuery.Create(nil);

  FSQLConn.Transaction := FSQLTran;
  FSQLTran.DataBase := FSQLConn;
  FSQLQuery.DataBase := FSQLConn;
end;

destructor TTreeDB.Destroy;
begin
  FreeAndNil(FSQLQuery);
  FreeAndNil(FSQLTran);
  FreeAndNil(FSQLConn);

  FreeAndNil(FDBConfig);

  inherited Destroy;
end;

procedure TTreeDB.DataChanged(AChanged: boolean);
begin
  FChanged := AChanged;
end;

procedure TTreeDB.ActiveChanged(AActive: boolean);
begin
  FActive := AActive;

  if Assigned(FActiveChanged) then
    FActiveChanged(Self);
end;

procedure TTreeDB.SaveConfig;
begin
  FSQLTran.Active := True;
  SetNote(ConfigID, FDBConfig.DataString);
  FSQLTran.Commit;
  if FAutoOpenTran then
    FSQLTran.Active := True;
  FChanged := False;
end;

{ 数据库文件 }

function TTreeDB.OpenDB(FileName: string): boolean;
begin
  Result := False;

  // 执行打开操作之前必须先关闭数据库
  if FActive or (FileName = '') then Exit;

  FDatabaseName := FileName;
  FSQLConn.DatabaseName := FDatabaseName;
  FSQLConn.Open;

  // 打开失败
  if not FSQLConn.Connected then begin
    FDataBaseName := '';
    Exit;
  end;

  // 创建默认数据表并初始化固定字段
  CreateTable;

  // 读取数据库配置信息，标记“数据库文件已经被打开”
  FDBConfig.Load(GetNote(ConfigID));
  if not FDBConfig.DataBaseIsOpened then begin
    FDBConfig.DataBaseIsOpened := True;
    SaveConfig;
    FDBConfig.DataBaseIsOpened := False;
  end;

  // 根据需要自动开启事务
  if FAutoOpenTran then FSQLTran.Active := True;

  ActiveChanged(FSQLConn.Connected);

  Result := True;
end;

procedure TTreeDB.SaveDB;
begin
  if FSQLTran.Active and FChanged then begin
    // 配置信息会在关闭数据库的时候保存。
    DBConfig.ChangedAfterBackup := True;
    // 对于强行打开的数据库，只有内容被改动并保存的时候，才会复位 DataBaseIsOpened 选项
    // 的状态，这样，在退出时，才会保存配置信息
    DBConfig.DataBaseIsOpened := False;
    // 如果多个程序同时打开同一个数据库文件，这里可能会出错
    FSQLTran.Commit;
  end;

  // 根据需要自动开启事务
  if FAutoOpenTran then FSQLTran.Active := True;

  DataChanged(False);
end;

function TTreeDB.CloseDB(Save: boolean): boolean;
begin
  Result := False;

  if FSQLTran.Active then
    if Save then SaveDB else FSQLTran.Rollback;

  // 保存配置信息，强行打开的数据库不会执行此操作
  if not FDBConfig.DataBaseIsOpened then SaveConfig;

  FSQLConn.Close;
  if FSQLConn.Connected then Exit;

  FDataBaseName := '';

  ActiveChanged(False);

  Result := True;
end;

function TTreeDB.BackupDB(BackupDir: string; Count: integer): boolean;
var
  i: integer;
  BackupName: string;
  BackupFile, BackupFile2: string;
  MaxCount: integer;
begin
  Result := False;

  if not FileExists(FDatabaseName) then Exit;

  BackupName := ExtractFileName(FDatabaseName) + '.bak';
  ForceDirectories(BackupDir);

  // 删除达到容量限制的最后一个备份文件
  // 如果设置 MaxCount=99，则可以删除序号大于 Count 的备份文件
  MaxCount := Count;
  for i := MaxCount downto Count do
  begin
    BackupFile := ChangeFileExt(BackupName, '.bak_' + Format('%.2d', [i]));
    BackupFile := ConcatPaths([BackupDir, BackupFile]);

    if FileExists(BackupFile) and not DeleteFile(BackupFile) then
      Exit;
  end;

  BackupFile := '';

  // 整体后移备份文件，空出第一个备份位置
  for i := Count - 1 downto 1 do
  begin
    BackupFile := ChangeFileExt(BackupName, '.bak_' + Format('%.2d', [i]));
    BackupFile := ConcatPaths([BackupDir, BackupFile]);

    BackupFile2 := ChangeFileExt(BackupName, '.bak_' + Format('%.2d', [i + 1]));
    BackupFile2 := ConcatPaths([BackupDir, BackupFile2]);

    if FileExists(BackupFile) and not RenameFile(BackupFile, BackupFile2) then
      Exit;
  end;

  // 开始备份
  TBackupThread.Create(FDatabaseName, BackupFile);

  // 标记“自上次备份以来，数据库未曾更改”
  DBConfig.ChangedAfterBackup := False;

  Result := True;
end;

procedure TTreeDB.CreateTable;
var
  OldChanged: Boolean;
begin
  if not TableExists(NoteTable) then
  begin
    OldChanged := FChanged;
    FSQLTran.Active := True;

    FSQLConn.ExecuteDirect(Format('Create Table "%s"(' +
      ' "%s" Integer Not Null,' +
      ' "%s" Blob,' +
      ' "%s" Text,' +
      ' "%s" Text);',
      [NoteTable, ParentField, ChildrenField, NameField, NoteField]));
    InitTable;

    FSQLTran.Commit;
    DataChanged(OldChanged);
  end;
end;

function TTreeDB.TableExists(TableName: string): boolean;
var
  TableNames: TStringList;
begin
  Result := False;

  TableNames := TStringList.Create;
  try
    FSQLConn.GetTableNames(TableNames);
    Result := TableNames.IndexOf(TableName) >= 0;
  finally
    TableNames.Free;
  end;
end;

procedure TTreeDB.InitTable;
begin
  CreateNewNode('Root', '');
  CreateNewNode('Recycler', '');
  CreateNewNode('Reserve', '');
  CreateNewNode('Config', '');
  CreateNewNode('Spare', '');
end;

{ 基本读写 }

function TTreeDB.AddNode(Name, Note: string; ToID: Integer; mode: TAttachMode): Integer;
begin
  // 从储备库中取出一条记录重新使用
  Result := ReuseNode;
  if Result = -1 then
    // 如果储备库为空，则创建一条新记录
    Result := CreateNewNode(Name, Note)
  else
  begin
    // 如果重用记录成功，则填写记录信息
    SetName(Result, Name);
    SetNote(Result, Note);
  end;

  AttachNode(Result, ToID, Mode);
end;

function TTreeDB.ReuseNode: Integer;
var
  Children: TBoundArray;
begin
  Children := GetChildren(ReserveID);

  if Assigned(Children) then
  begin
    // 从储备库中取出最后一个节点重新使用
    Result := Children[High(Children)];
    // 复位节点信息
    SetParent(Result, FreeID);
    SetChildren(Result, nil);
    SetName(Result, '');
    SetNote(Result, '');
    // 将剩下的节点写入数据库
    SetLength(Children, High(Children));
    SetChildren(ReserveID, Children);
    SetLength(Children, 0);
  end
  else
    // 储备库中没有记录
    Result := -1;
end;

function TTreeDB.CreateNewNode(Name, Note: string): Integer;
begin
  // 插入一条新记录
  FSQLQuery.SQL.Text :=
    Format('Insert Into %s (%s, %s, %s) Values (:parent, :name, :note)',
      [NoteTable, ParentField, NameField, NoteField]);
  FSQLQuery.Params.ParamByName('parent').AsInteger := FreeID;
  FSQLQuery.Params.ParamByName('name').AsString := Name;
  FSQLQuery.Params.ParamByName('note').AsString := Note;
  FSQLQuery.ExecSQL;

  // 获取最近插入的记录的 ID
  FSQLQuery.SQL.Text := Format('Select last_insert_rowid() From %s', [NoteTable]);
  FSQLQuery.Open;
  Result := FSQLQuery.Fields[0].AsInteger;
  FSQLQuery.Close;

  DataChanged(True);
end;

procedure TTreeDB.DelNode(ID: Integer);
var
  Buf, OldChildren: TBoundArray;
  Count: integer;

  // 用于递归的子函数
  procedure DoDelNode(ID: Integer);
  var
    Child: Integer;
  begin
    if Count > High(Buf) then
      SetLength(Buf, Length(Buf) * 2);
    Buf[Count] := ID;
    Inc(Count);
    for Child in GetChildren(ID) do
      DoDelNode(Child);
  end;

begin
  DetachNode(ID);

  // 收集将要被删除的节点的 ID
  SetLength(Buf, 8);
  Count := 0;
  DoDelNode(ID);

  // 获取储备库中的节点，然后与要删除的节点合并
  OldChildren := GetChildren(ReserveID);
  if Length(OldChildren) > 0 then
  begin
    // 新删除的节点放在子节点列表的前面，这样可以保证最新删除的节点最后被重用，
    // 即最新删除的节点尽可能长时间的留在数据中，便于需要的时候进行数据恢复
    SetLength(Buf, Count + Length(OldChildren));
    Move(OldChildren[0], Buf[Count], Length(OldChildren) * Sizeof(Integer));
  end
  else
    SetLength(Buf, Count);

  SetChildren(ReserveID, Buf);

  SetLength(OldChildren, 0);
  SetLength(Buf, 0);
end;

procedure TTreeDB.RecycleNode(ID: Integer);
begin
  MoveNode(ID, RecyclerID, naAddChild);
end;

procedure TTreeDB.EmptyRecycler;
var
  Child: integer;
begin
  for Child in GetChildren(RecyclerID) do
    DelNode(Child);
end;

procedure TTreeDB.MoveNode(FromID, ToID: Integer; Mode: TAttachMode);
begin
  DetachNode(FromID);
  AttachNode(FromID, ToID, Mode);
end;

// 向整数数组中插入一个元素
function InsertIntElem(IntArr: TBoundArray; Index: integer; Data: integer; Mode: TAttachMode): TBoundArray;
var
  i: integer;
begin
  Result := nil;

  if Assigned(IntArr) and ((Index < Low(IntArr)) or (Index > High(IntArr))) then
    Exit;

  SetLength(IntArr, Length(IntArr) + 1);

  case mode of
    naInsert:
    begin
      for i := High(IntArr) downto index + 1 do
        IntArr[i] := IntArr[i - 1];
      IntArr[index] := Data;
    end;

    naInsertBehind:
    begin
      for i := High(IntArr) downto index + 2 do
        IntArr[i] := IntArr[i - 1];
      IntArr[index + 1] := Data;
    end;

    naAddFirst, naAddChildFirst:
    begin
      for i := High(IntArr) downto 1 do
        IntArr[i] := IntArr[i - 1];
      IntArr[0] := Data;
    end;

    naAdd, naAddChild:
      IntArr[High(IntArr)] := Data;
  end;

  Result := IntArr;
end;

// 从整数数组中删除一个元素
function RemoveIntElem(IntArr: TBoundArray; Data: integer): TBoundArray;
var
  Index, i: integer;
begin
  Result := nil;

  if not Assigned(IntArr) then
    Exit;

  for Index := 0 to High(IntArr) do
  begin
    if IntArr[Index] = Data then
    begin
      for i := Index to High(IntArr) - 1 do
        IntArr[i] := IntArr[i + 1];
      SetLength(IntArr, Length(IntArr) - 1);

      Break;
    end;
  end;

  Result := IntArr;
end;

procedure TTreeDB.DetachNode(ID: Integer);
var
  ParentID: integer;
  Children: TBoundArray;
begin
  // 从父节点中删除自身
  ParentID := GetParent(ID);
  Children := GetChildren(ParentID);
  Children := RemoveIntElem(Children, ID);
  SetChildren(ParentID, Children);
  SetLength(Children, 0);

  // 设置自身为自由节点
  SetParent(ID, FreeID);
end;

procedure TTreeDB.AttachNode(ID: Integer; ToID: Integer; mode: TAttachMode);
var
  Index: integer;
  ParentID: integer;
  Children: TBoundArray;
begin
  Index := 0;
  ParentID := 0;
  Children := nil;

  // 不能停靠在根节点的前后
  if ToID in [RootID, RecyclerID, ReserveID] then
  begin
    // 如果停靠目标是根节点，则停靠模式将自动转换为子节点模式
    case mode of
      naAddFirst, naInsert: mode := naAddChildFirst;
      naAdd, naInsertBehind: mode := naAddChild;
    end;
  end;

  case mode of
    naInsert, naInsertBehind:
    begin
      ParentID := GetParent(ToID);
      Children := GetChildren(ParentID);
      // 获取目标节点的序号
      for Index := 0 to High(Children) do
        if Children[Index] = ToID then
          Break;
    end;

    naAdd, naAddFirst:
    begin
      ParentID := GetParent(ToID);
      Children := GetChildren(ParentID);
    end;

    naAddChild, naAddChildFirst:
    begin
      ParentID := ToID;
      Children := GetChildren(ParentID);
    end;
  end;

  // 为自由节点设置父节点
  SetParent(ID, ParentID);

  // 将自由节点添加到父节点中
  Children := InsertIntElem(Children, Index, ID, mode);
  SetChildren(ParentID, Children);
  SetLength(Children, 0);
end;

function TTreeDB.UpDownNode(ID: Integer; Up: boolean): boolean;
var
  ParentID: integer;
  Siblings: TBoundArray;
  Index: integer;
  i: integer;
begin
  Result := False;
  ParentID := GetParent(ID);
  Siblings := GetChildren(ParentID);

  if Length(Siblings) = 1 then Exit;

  for Index := 0 to High(Siblings) do
    if Siblings[Index] = ID then
      Break;

  if Up then begin
    if Index = 0 then begin
      // 首节点前移，将其移动到最后
      for i := 0 to High(Siblings) - 1 do
        Siblings[i] := Siblings[i + 1];
      Siblings[High(Siblings)] := ID;
    end else begin
      // 非首节点前移，与前一个节点交换位置
      Siblings[Index] := Siblings[Index - 1];
      Siblings[Index - 1] := ID;
    end;
  end else begin
    if Index = High(Siblings) then begin
      // 尾节点后移，将其移动到最前
      for i := High(Siblings) downto 1 do
        Siblings[i] := Siblings[i - 1];
      Siblings[0] := ID;
    end else begin
      // 非尾节点后移，与后一个节点交换位置
      Siblings[Index] := Siblings[Index + 1];
      Siblings[Index + 1] := ID;
    end;
  end;

  SetChildren(ParentID, Siblings);
  SetLength(Siblings, 0);
  Result := True;
end;

// 将整数数组 Arr2 追加到整数数组 Arr1 的尾部
// StartIndex：Arr2 的起始读取位置
// EndIndex  ：Arr2 的结束读取位置的下一个位置，0 表示一直读取到最后一个元素
procedure AppendArray(var Arr1, Arr2: TBoundArray; StartIndex: Integer; EndIndex: Integer = 0);
var
  Arr1Len: Integer;
begin
  Arr1Len := Length(Arr1);
  if EndIndex = 0 then EndIndex := Length(Arr2);
  SetLength(Arr1, Arr1Len + EndIndex - StartIndex);
  Move(Arr2[StartIndex], Arr1[Arr1Len], (EndIndex - StartIndex) * Sizeof(Integer));
end;

// 左移节点，将 ID 之后的同级节点移动到 ID 的子节点尾部，将 ID 移动到父节点之后
function TTreeDB.MoveLeft(ID: Integer): boolean;
var
  ParentID: Integer;
  ParentChildren: TBoundArray;
  Children: TBoundArray;
  Index: Integer;
  StartIndex: Integer;
begin
  Result := False;
  ParentID := GetParent(ID);
  if ParentID = RootID then Exit;

  ParentChildren := GetChildren(ParentID);
  for StartIndex := 0 to High(ParentChildren) do
    if ParentChildren[StartIndex] = ID then Break;

  // 将自身之后的节点移到自身子节点尾部
  for Index := StartIndex + 1 to High(ParentChildren) do
    SetParent(ParentChildren[Index], ID);

  Children := GetChildren(ID);
  AppendArray(Children, ParentChildren, StartIndex + 1);
  SetChildren(ID, Children);

  // 将自身移到父节点之后
  AttachNode(ID, ParentID, naInsertBehind);

  SetLength(ParentChildren, StartIndex);
  SetChildren(ParentID, ParentChildren);
  SetLength(ParentChildren, 0);
  SetLength(Children, 0);
  Result := True;
end;

// 右移节点，将 ID 移动到前一节点的子节点尾部，将 ID 的子节点移动到 ID 之后
function TTreeDB.MoveRight(ID: Integer): boolean;
var
  ParentID: Integer;
  PrevID: Integer;
  ParentChildren: TBoundArray;
  Children: TBoundArray;
  Index: Integer;
  StartIndex: Integer;
begin
  Result := False;
  ParentID := GetParent(ID);
  ParentChildren := GetChildren(ParentID);
  for StartIndex := 0 to High(ParentChildren) do
    if ParentChildren[StartIndex] = ID then Break;

  if StartIndex = 0 then Exit;

  PrevID := ParentChildren[StartIndex - 1];

  // 将自身移到前一节点的子节点尾部
  MoveNode(ID, PrevID, naAddChild);
  ParentChildren := GetChildren(PrevID);

  // 将自身的子节点移到自身之后
  Children := GetChildren(ID);
  for Index := 0 to High(Children) do
    SetParent(Children[Index], PrevID);

  AppendArray(ParentChildren, Children, 0);
  SetChildren(PrevID, ParentChildren);

  SetLength(Children, 0);
  SetChildren(ID, Children);
  SetLength(ParentChildren, 0);
  SetLength(Children, 0);
  Result := True;
end;

function TTreeDB.CopyNode(FromID, ToID: Integer; Mode: TAttachMode): Integer;
var
  Child: integer;
begin
  Result := AddNode(GetName(FromID), GetNote(FromID), ToID, Mode);

  for Child in GetChildren(FromID) do
    CopyNode(Child, Result, naAddChild);
end;

function TTreeDB.GetParent(ID: Integer): Integer;
begin
  FSQLQuery.SQL.Text := Format('Select %s From %s where rowid=:rowid', [ParentField, NoteTable]);
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.Open;
  Result := FSQLQuery.Fields[0].AsInteger;
  FSQLQuery.Close;
end;

procedure TTreeDB.SetParent(ID: Integer; ParentID: Integer);
begin
  FSQLQuery.SQL.Text := Format('Update %s Set %s=:parent Where rowid=:rowid', [NoteTable, ParentField]);
  FSQLQuery.Params.ParamByName('parent').AsInteger := ParentID;
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.ExecSQL;

  DataChanged(True);
end;

// 获取数组的长度字段（隐藏字段）的地址
function GetArrHigh(P: Pointer): PDynArrayIndex; inline;
begin
  Result := P - SizeOf(IntPtr);  // See TDynArray in dynarr.inc
end;

// 将整数数组转换为字节数组
function IntsToBytes(const Ints: TBoundArray): TBytes;
begin
  if Assigned(Ints) then
    GetArrHigh(@Ints[0])^ := Length(Ints) * SizeOf(Ints[0]) - 1;
  Result := TBytes(Ints);
end;

// 将字节数组转换为整数数组
function BytesToInts(const Bytes: TBytes): TBoundArray;
begin
  if Assigned(Bytes) then
    GetArrHigh(@Bytes[0])^ := Length(Bytes) div SizeOf(TBoundArray[0]) - 1;
  Result := TBoundArray(Bytes);
end;

function TTreeDB.GetChildren(ID: Integer): TBoundArray;
begin
  FSQLQuery.SQL.Text := Format('Select %s From %s where rowid=:rowid', [ChildrenField, NoteTable]);
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.Open;
  Result := BytesToInts(FSQLQuery.Fields[0].AsBytes);
  FSQLQuery.Close;
end;

procedure TTreeDB.SetChildren(ID: Integer; Children: TBoundArray);
var
  BChildren: TBytes;
begin
  BChildren := IntsToBytes(Children);
  FSQLQuery.SQL.Text := Format('Update %s Set %s=:children Where rowid=:rowid', [NoteTable, ChildrenField]);
  FSQLQuery.Params.ParamByName('children').AsBytes := BChildren;
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.ExecSQL;
  // 恢复传入时的数组长度信息
  BytesToInts(BChildren);
  DataChanged(True);
end;

function TTreeDB.GetName(ID: Integer): string;
begin
  FSQLQuery.SQL.Text := Format('Select %s From %s where rowid=:rowid', [NameField, NoteTable]);
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.Open;
  Result := FSQLQuery.Fields[0].AsString;
  FSQLQuery.Close;
end;

procedure TTreeDB.SetName(ID: Integer; Name: string);
begin
  FSQLQuery.SQL.Text := Format('Update %s Set %s=:name Where rowid=:rowid', [NoteTable, NameField]);
  FSQLQuery.Params.ParamByName('name').AsString := Name;
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.ExecSQL;

  DataChanged(True);
end;

function TTreeDB.GetNote(ID: Integer): string;
begin
  FSQLQuery.SQL.Text := Format('Select %s From %s where rowid=:rowid', [NoteField, NoteTable]);
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.Open;
  Result := FSQLQuery.Fields[0].AsString;
  FSQLQuery.Close;
end;

procedure TTreeDB.SetNote(ID: Integer; Note: string);
begin
  FSQLQuery.SQL.Text := Format('Update %s Set %s=:note Where rowid=:rowid', [NoteTable, NoteField]);
  FSQLQuery.Params.ParamByName('note').AsString := Note;
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.ExecSQL;

  DataChanged(True);
end;

{ 导入和导出 }

// FileName  ：要导入的文件
// IncludeExt：导入时是否包含文件的扩展名
// ToID      ：导入的目标节点
// Mode      ：导入后的停靠模式
function TTreeDB.ImportFile(FileName: string; IncludeExt: boolean;
  ToID: Integer; mode: TAttachMode; StripTailSpace: boolean): Integer;
var
  Name, Note: string;
begin
  Result := ToID;

  if not FileExists(FileName) then
    Exit;

  Name := ExtractFileName(FileName);
  if not IncludeExt then
    Name := ChangeFileExt(Name, '');

  Note := ReadFile(FileName, StripTailSpace);

  Result := AddNode(Name, Note, ToID, Mode);
end;

// DirName    ：要导入的目录名（入口目录）
// IncludeRoot：是否将入口目录也作为一个节点导入
// IncludeExt ：导入时是否包含文件的扩展名
// ToID       ：导入的目标节点
// Mode       ：导入后的停靠模式
function TTreeDB.ImportDir(FromDir: string; IncludeRoot: boolean;IncludeExt: boolean;
  ToID: Integer; mode: TAttachMode; StripTailSpace: boolean): Integer;

  // 用于递归的子函数
  procedure DoImportDir(FromDir: string; ToID: integer; mode: TAttachMode);
  var
    i: integer;
    FullPath: string;
    Name, Note: string;
    NewID: integer;
    FindRst: TStringList;
  begin
    FindRst := TStringList.Create;
    try
      // 查找文件和目录，并排序
      FindRst.Clear;
      FindFiles(FromDir, FindRst);
      FindRst.Sort;

      // 将找到的文件或目录添加到节点树中
      for i := 0 to FindRst.Count - 1 do begin
        Name := FindRst[i];
        FullPath := ConcatPaths([FromDir, Name]);

        if DirectoryExists(FullPath) then begin
          // 添加目录节点并递归处理子目录
          NewID := AddNode(Name, '', ToID, Mode);
          DoImportDir(FullPath, NewID, naAddChild);
        end else begin
          if not IncludeExt then
            Name := ChangeFileExt(Name, '');
          Note := ReadFile(FullPath, StripTailSpace);

          if (FindRst[i] = DirNode) and not (ToID in [RootID, RecyclerID, ReserveID]) then
            // 该文件中存放的是目录节点的内容，将其写入目录节点中
            SetNote(ToID, Note)
          else
            // 该文件是普通文本文件，将其添加到新的节点中
            NewID := AddNode(Name, Note, ToID, Mode);
        end;

        // 第一个节点使用参数中的停靠模式，其它节点则停靠在前一个节点的后面
        if Mode in [naAddFirst, naAddChildFirst, naInsertBehind] then begin
          ToID := NewID;
          Mode := naInsertBehind;
        end;
      end;
    finally
      FindRst.Free;
    end;
  end;

begin
  Result := ToID;

  if not DirectoryExists(FromDir) then
    Exit;

  if IncludeRoot then
  begin
    // 如果包含入口目录，则将入口目录作为一个节点添加到树中
    FromDir := ExcludeTrailingPathDelimiter(FromDir);
    Result := AddNode(ExtractFileName(FromDir), '', ToID, Mode);

    Mode := naAddChild;
  end
  else
    Result := ToID;

  DoImportDir(FromDir, Result, Mode);
end;

// FromDBFile：要导入的数据库文件
// FromID    ：要导入的入口节点（将导入该节点及其所有子节点）
// ToID      ：要导入的目标节点
// mode      ：导入后的停靠模式
function TTreeDB.ImportDB(FromDBFile: string; FromID, ToID: Integer; mode: TAttachMode): Integer;
var
  Child: integer;
  TreeDB: TTreeDB;

  // 用于递归的子函数
  procedure DoImportDB(FromID, ToID: integer; mode: TAttachMode);
  var
    Child, NewID: integer;
  begin
    // 将数据从源数据库中拷贝到新数据库中
    NewID := AddNode(TreeDB.GetName(FromID), TreeDB.GetNote(FromID), ToID, Mode);

    for Child in TreeDB.GetChildren(FromID) do
      DoImportDB(Child, NewID, naAddChild);

    // 第一个节点使用参数中的停靠模式，其它节点则停靠在前一个节点的后面
    if Mode in [naAddFirst, naAddChildFirst, naInsertBehind] then
    begin
      ToID := NewID;
      Mode := naInsertBehind;
    end;
  end;

begin
  TreeDB := TTreeDB.Create;
  try
    TreeDB.OpenDB(FromDBFile);
    if not TreeDB.Active then Exit;

    Result := ToID;

    if FromID in [RootID, RecyclerID] then
      // 如果入口节点是根节点，则将其所有子节点导入
      for Child in TreeDB.GetChildren(FromID) do
        DoImportDB(Child, ToID, Mode)
    else
    begin
      // 如果入口节点不是根节点，则将该节点导入
      Result := AddNode(TreeDB.GetName(FromID), TreeDB.GetNote(FromID), ToID, Mode);
      for Child in TreeDB.GetChildren(FromID) do
        DoImportDB(Child, Result, naAddChild);
    end;

    TreeDB.CloseDB(True);
  finally
    TreeDB.Free;
  end;
end;

// ID   ：要导出的节点
// ToDir：存放导出结果的目录
// Ext  ：导出到文本文件将要添加的扩展名（可以为空）
// Depth： 要导出的深度，1 表示只导出当前节点，AllDepth 表示导出所有节点，其它值表示指定深度
procedure TTreeDB.ExportToDir(ID: Integer; ToDir: string; Ext: string; Depth: integer;
  StripTailSpace: boolean);
var
  Child: integer;

  // 用于递归的子函数
  procedure DoExportTree(ID: integer; ToDir: string);
  var
    Children: TBoundArray;
    Name, Note: string;
    i: integer;
  begin
    Dec(Depth);

    Name := FixFileName(GetName(ID));
    Note := GetNote(ID);
    Children := GetChildren(ID);

    // 最后一层深度的节点将被导出为文件，无论其是否含有子节点
    if Assigned(Children) and (Depth <> 0) then
    begin
      // 将节点导出为目录
      ToDir := GetNonExistsPath(ToDir, Name, '', 3);
      ForceDirectories(ToDir);

      // 保存节点的内容到文本文件
      if Note <> '' then
        WriteFile(ConcatPaths([ToDir, DirNode + Ext]), Note, StripTailSpace);

      for i := 0 to High(Children) do
        DoExportTree(Children[i], ToDir);

      SetLength(Children, 0);
    end else
      // 将节点导出为文本文件
      WriteFile(GetNonExistsPath(ToDir, Name, Ext, 3), Note, StripTailSpace);

    Inc(Depth);
  end;

begin
  if ID in [RootID, RecyclerID] then
    for Child in GetChildren(ID) do
      DoExportTree(Child, ToDir)
  else
    DoExportTree(ID, ToDir);
end;

// ID    : 要导出的节点
// ToFile: 存放导出结果的文本文件
// Depth : 要导出的深度，1 表示只导出当前节点，AllDepth 表示导出所有节点，其它值表示指定深度
procedure TTreeDB.ExportToFile(ID: Integer; ToFile, Splitter: string;
  Depth: integer; StripTailSpace: boolean);
var
  Buf: TStringStream;
  Child: integer;

  // 用于递归的子函数
  procedure DoExportTreeToFile(ID: integer);
  var
    Name, Note: string;
    Child: integer;
  begin
    Dec(Depth);

    Name := GetName(ID);
    Note := GetNote(ID);

    // 将节点名称写入缓存中
    Buf.WriteString(Name + LineEnding + LineEnding);

    // 将节点内容写入缓存中
    if Note <> '' then
      Buf.WriteString(Note.Trim + LineEnding + LineEnding);

    // 将分隔符写入缓存中
    if Splitter <> '' then
      Buf.WriteString(Splitter + LineEnding + LineEnding);

    if Depth <> 0 then
      for Child in GetChildren(ID) do
        DoExportTreeToFile(Child);

    Inc(Depth);
  end;

begin
  Buf := TStringStream.Create('');
  try

    if ID in [RootID, RecyclerID] then
      for Child in GetChildren(ID) do
        DoExportTreeToFile(Child)
    else
      DoExportTreeToFile(ID);

    WriteFile(ToFile, Buf.DataString, StripTailSpace);

  finally
    Buf.Free;
  end;
end;

// ID      ：要导出的节点
// ToID    ：目标数据库中的目标节点
// ToDBFile：要导出的目标数据库（如果文件不存在会自动创建，如果文件已经存在，
//           则不会覆盖原文件的数据，新的数据将追加到目标数据库的尾部）
// Depth   ：要导出的深度，1 表示只导出当前节点，AllDepth 表示导出所有节点，其它值表示指定深度
function TTreeDB.ExportToDB(ID, ToID: Integer; ToDBFile: string; Depth: integer): boolean;
var
  Child: integer;
  TreeDB: TTreeDB;

  // 用于递归的子函数
  procedure DoExportTreeToDB(ID, ToID: integer);
  var
    Child, TargetNewID: integer;
  begin
    Dec(Depth);

    TargetNewID := TreeDB.AddNode(GetName(ID), GetNote(ID), ToID, naAddChild);

    if Depth <> 0 then
      for Child in GetChildren(ID) do
        DoExportTreeToDB(Child, TargetNewID);

    Inc(Depth);
  end;

begin
  TreeDB := TTreeDB.Create;
  try
    Result := TreeDB.OpenDB(ToDBFile);

    if not Result then Exit;

    if ID in [RootID, RecyclerID] then
      for Child in GetChildren(ID) do
        DoExportTreeToDB(Child, ToID)
    else
      DoExportTreeToDB(ID, ToID);

    TreeDB.FChanged := False;
    TreeDB.CloseDB(True);
  finally
    TreeDB.Free;
  end;
end;

// 普通搜索
// ID         ：要在其中搜索内容的节点
// ASearchText：要搜索的字符串
// IncludeName：是否在节点名称中搜索
// IncludeNote：是否在节点内容中搜索
// Depth      ：要搜索的深度，1 表示只搜索当前节点，AllDepth 表示搜索所有节点，其它值表示指定深度
// Rst        ：用来存放搜索结果的变量
procedure TTreeDB.Search(ID: Integer; ASearchText: string; IncludeName,
  IncludeNote: boolean; Depth: integer; IgnoreCase: boolean; Count: Integer;
  var Rst: TSearchResult);
var
  Child: integer;

  // 用于递归的子函数
  procedure DoSearch(ID: integer);
  var
    Child: integer;
    SearchCount: integer;
  begin
    Dec(Depth);

    // 在节点名称中搜索
    if IncludeName and ((Count <= 0) or (Rst.Count < Count)) and
    (UTF8StringPos(GetName(ID), ASearchText, 1, IgnoreCase) > 0) then
      Rst.Add(ID, 0, 0);

    // 在节点内容中搜索
    if IncludeNote and ((Count <= 0) or (Rst.Count < Count)) then begin
      if Count > 0 then
        SearchCount := Count - Rst.Count
      else
        SearchCount := 0;
      UTF8SearchSpecial(ID, GetNote(ID), ASearchText, IgnoreCase, SearchCount, Rst);
    end;

    // 达到指定深度或指定搜索数量则不再继续深入
    if (Depth <> 0) and ((Count <= 0) or (Rst.Count < Count)) then
      for Child in GetChildren(ID) do
        DoSearch(Child);

    Inc(Depth);
  end;

begin
  if ASearchText = '' then Exit;

  ASearchText := UnEscape(ASearchText);

  if ID in [RootID, RecyclerID] then
    for Child in GetChildren(ID) do
      DoSearch(Child)
  else
    DoSearch(ID);
end;

// 普通替换
// ID          ：要在其中搜索内容的节点
// ASearchText ：要搜索的字符串
// AReplaceText：用来替换的字符串
// IncludeName ：是否在节点名称中搜索
// IncludeNote ：是否在节点内容中搜索
// Depth       ：要搜索的深度，1 表示只搜索当前节点，AllDepth 表示搜索所有节点，其它值表示指定深度
// Rst         ：用来存放替换结果的变量
procedure TTreeDB.Replace(ID: Integer; ASearchText, AReplaceText: string;
  IncludeName, IncludeNote: boolean; Depth: integer; IgnoreCase: boolean;
  Count: Integer; var Rst: TSearchResult);
var
  Child: integer;

  // 用于递归的子函数
  procedure DoReplace(ID: integer);
  var
    Child: integer;
    NewName, NewNote: string;
    SearchCount: Integer;
  begin
    Dec(Depth);

    // 在节点名称中替换
    if IncludeName and ((Count <= 0) or (Rst.Count < Count)) then begin
      if Count > 0 then
        SearchCount := Count - Rst.Count
      else
        SearchCount := 0;
      NewName := UTF8ReplaceSpecial(ID, GetName(ID), ASearchText, AReplaceText, IgnoreCase, SearchCount);
      if SearchCount > 0 then begin
        SetName(ID, NewName);
        Rst.Add(ID, 0, 0);
      end;
    end;

    // 在节点内容中替换
    if IncludeNote and ((Count <= 0) or (Rst.Count < Count)) then begin
      if Count > 0 then
        SearchCount := Count - Rst.Count
      else
        SearchCount := 0;
      NewNote := UTF8ReplaceSpecial(ID, GetNote(ID), ASearchText, AReplaceText, IgnoreCase, SearchCount, Rst);
      if SearchCount > 0 then
        SetNote(ID, NewNote);
    end;

    // 达到指定深度或指定搜索数量则不再继续深入
    if (Depth <> 0) and ((Count <= 0) or (Rst.Count < Count)) then
      for Child in GetChildren(ID) do
        DoReplace(Child);

    Inc(Depth);
  end;

begin
  if ASearchText = '' then Exit;

  ASearchText := UnEscape(ASearchText);
  AReplaceText := UnEscape(AReplaceText);

  if ID in [RootID, RecyclerID] then
    for Child in GetChildren(ID) do
      DoReplace(Child)
  else
    DoReplace(ID);
end;

// 正则表达式搜索
// ID         ：要在其中搜索内容的节点
// ASearchText：要搜索的字符串
// IncludeName：是否在节点名称中搜索
// IncludeNote：是否在节点内容中搜索
// Depth      ：要搜索的深度，1 表示只搜索当前节点，AllDepth 表示搜索所有节点，其它值表示指定深度
// Rst        ：用来存放搜索结果的变量
procedure TTreeDB.RegSearch(ID: Integer; ASearchText: string; IncludeName,
  IncludeNote: boolean; Depth: integer; Count: Integer; var Rst: TSearchResult);
var
  Child: integer;
  Expr: TRegExpr;

  // 用于递归的子函数
  procedure DoSearch(ID: integer);
  var
    Child: integer;
    Name, Note: string;
    Found: boolean;
  begin
    Dec(Depth);

    // 在节点名称中搜索
    if IncludeName then begin
      Name := GetName(ID);
      if Expr.Exec(Name) and ((Count <= 0) or (Rst.Count < Count)) then
        Rst.Add(ID, 0, 0);
    end;

    // 在节点内容中搜索
    if IncludeNote then begin
      Note := GetNote(ID);
      Found := Expr.Exec(Note);  // 需要 use lazUTF8 或 {$CODEPAGE UTF8} 以支持 Unicode 字符
      while Found and ((Count <= 0) or (Rst.Count < Count)) do begin
        Rst.Add(ID, Expr.MatchPos[0], Expr.MatchLen[0]);
        Found := Expr.ExecNext;  // 需要 use lazUTF8 或 {$CODEPAGE UTF8} 以支持 Unicode 字符
      end;
    end;

    // 达到指定深度或指定搜索数量则不再继续深入
    if (Depth <> 0) and ((Count <= 0) or (Rst.Count < Count)) then
      for Child in GetChildren(ID) do
        DoSearch(Child);

    Inc(Depth);
  end;

begin
  if ASearchText = '' then Exit;

  // 正则表达式自会会处理 \\，所以需要专用的转义函数进行转义处理
  ASearchText := RegUnEscape(ASearchText);

  Expr := TRegExpr.Create(ASearchText);

  if ID in [RootID, RecyclerID] then
    for Child in GetChildren(ID) do
      DoSearch(Child)
  else
    DoSearch(ID);

  Expr.Free;
end;

// 正则表达式替换
// ID          ：要在其中搜索内容的节点
// ASearchText ：要搜索的字符串
// AReplaceText：用来替换的字符串
// IncludeName ：是否在节点名称中搜索
// IncludeNote ：是否在节点内容中搜索
// Depth       ：要搜索的深度，1 表示只搜索当前节点，AllDepth 表示搜索所有节点，其它值表示指定深度
// Rst         ：用来存放替换结果的变量
procedure TTreeDB.RegReplace(ID: Integer; ASearchText, AReplaceText: string;
  IncludeName, IncludeNote: boolean; Depth: integer; Count: Integer;
  var Rst: TSearchResult);
var
  Child: integer;
  Expr: TRegExpr;
  Replaced: boolean;

  function OneReplace(ID: integer; AText: string): string;
  var
    Found: boolean;
    Start, ReplacedStart, ReplacedLength: integer;
    ReplacedOne, MiddleString: string;
  begin
    Result := '';

    Start := 1;

    ReplacedStart := 1;
    ReplacedLength := 0;

    // 第一次查找
    Found := Expr.Exec(AText);  // 需要 use lazUTF8 或 {$CODEPAGE UTF8} 以支持 Unicode 字符

    if not Found then
    begin
      Replaced := False;
      Exit;
    end;

    while Found and ((Count <= 0) or (Rst.Count < Count)) do
    begin
      // 获取不匹配的子串
      MiddleString := UTF8Copy(AText, Start, Expr.MatchPos[0] - Start);

      // 获取单个替换结果
      ReplacedOne := ReplaceRegExpr(ASearchText, Expr.Match[0], AReplaceText, True);

      // 登记替换位置和长度
      ReplacedStart := ReplacedStart + UTF8LengthFast(MiddleString);
      ReplacedLength := UTF8LengthFast(ReplacedOne);
      Rst.Add(ID, ReplacedStart, ReplacedLength);

      // 替换当前搜索到的内容
      Result := Result + MiddleString + ReplacedOne;

      // 更新查找和替换的起始位置
      Start := Expr.MatchPos[0] + Expr.MatchLen[0];
      ReplacedStart := ReplacedStart + ReplacedLength;

      // 继续查找
      Found := Expr.ExecNext;
    end;
    // 将最后一个查找结果之后的内容写入 Result 中
    MiddleString := UTF8Copy(AText, Start, MaxInt);
    Result := Result + MiddleString;

    Replaced := True;
  end;

  // 用于递归的子函数
  procedure DoReplace(ID: integer);
  var
    Child: integer;
    Name, Note: string;
  begin
    Dec(Depth);

    // 替换节点名称
    if IncludeName then
    begin
      Name := GetName(ID);
      if Expr.Exec(Name) and ((Count <= 0) or (Rst.Count < Count)) then
      begin
        SetName(ID, Expr.Replace(Name, AReplaceText, True));
        Rst.Add(ID, 0, 0);
      end;
    end;

    // 替换节点内容
    if IncludeNote and ((Count <= 0) or (Rst.Count < Count)) then begin
      Note := OneReplace(ID, GetNote(ID));
      if Replaced then
        SetNote(ID, Note);
    end;

    // 达到指定深度则不再继续深入
    if Depth <> 0 then
      for Child in GetChildren(ID) do
        DoReplace(Child);

    Inc(Depth);
  end;

begin
  if ASearchText = '' then Exit;

  // 正则表达式自会会处理 \\，所以需要专用的转义函数进行转义处理
  ASearchText := RegUnEscape(ASearchText);
  AReplaceText := RegUnEscape(AReplaceText);

  Expr := TRegExpr.Create(ASearchText);

  if ID in [RootID, RecyclerID] then
    for Child in GetChildren(ID) do
      DoReplace(Child)
  else
    DoReplace(ID);

  Expr.Free;
end;

// 批量重命名，从节点名称或节点内容中搜索字符串，并将搜索出来的字符串经过替换
// 操作后作为节点名称使用，替换内容中可以使用${Num=3,005+5}，用于在替换结果中
// 插入序号，其中3表示从第三个查找结果开始重命名，005表示序号的起始值和序号宽
// 度，+5表示序号的增长步长为5，可以简写为${Num=3,}或${Num=005+5}或${Num=005}
// 如果搜索结果为空，则会跳过该节点，不进行重命名
// ID          ：要在其中搜索内容的节点
// ASearchText ：要搜索的字符串
// AReplaceText：用来替换的字符串
// SearchNote  ：是否在节点内容中搜索，是则搜索节点内容，否则搜索节点名称
// Depth       ：要执行重命名的节点所在的深度，其它深度的节点不会被重命名，
//               1 表示当前节点或根节点，AllDepth 表示重命名所有节点
// Rst         ：用来存放重命名结果的变量
procedure TTreeDB.RenameNodes(ID: integer; ASearchText, AReplaceText: string; SearchNote: boolean;
  Depth: integer; var Rst: TSearchResult);
var
  Child: integer;
  Expr: TRegExpr;
  StartIndex, StartNum, NumStep, NumLen: integer;
  AReplaceText1, AReplaceText2: string;

  procedure ParseOrderNumber;
  var
    BPos, StartBPos, SubBPos: integer;
    SubPattern: string;
  begin
    StartIndex := 1;
    // StartNum = 0 表示没有找到序号标记
    StartNum := 0;
    // 标记是两个字节开头，所以从第二个字节开始查找
    BPos := 2;
    StartBPos := 0;
    while BPos <= Length(AReplaceText) do begin
      // 查找序号标记
      case AReplaceText[BPos] of
        '{': if AReplaceText[BPos-1] = '$' then StartBPos := BPos + 1;
        '}': if StartBPos > 0 then
        begin
          // 发现序号标记（也可能是正则表达式的分组标记）
          SubPattern := StringCopy(AReplaceText, StartBPos, BPos);
          if StartsWithCaseInSensitive(SubPattern, 'Num=') then begin
            // 序号之前的部分
            AReplaceText1 := StringCopy(AReplaceText, 1, StartBPos - 2);
            // 将序号之前的 $n 替换为 ${n} 否则会和序号冲突
            SubBPos := Length(AReplaceText1);
            while (SubBPos > 0) and (AReplaceText1[SubBPos] in ['0'..'9']) do
              Dec(SubBPos);
            if (SubBPos > 1) and (AReplaceText1[SubBPos] = '$') then
              AReplaceText1 := StringCopy(AReplaceText1, 1, SubBPos + 1) +
                '{' + StringCopy(AReplaceText1, SubBPos + 1) + '}';
            // 序号之后的部分
            AReplaceText2 := StringCopy(AReplaceText, BPos + 1);
            // 序号参数
            SubPattern := StringCopy(SubPattern, 5);
            // StartIndex 跳过指定的节点数
            if StringPos(SubPattern, ',') > 0 then begin
              StartIndex := StrToIntDef(StringBefore(SubPattern, ','), 1);
              SubPattern := StringBehind(SubPattern, ',');
            end;
            // NumStep 序号的步长
            if StringPos(SubPattern, '+') > 0 then begin
              NumStep := StrToIntDef(StringBehind(SubPattern, '+'), 1);
              SubPattern := StringBefore(SubPattern, '+');
            end else
              NumStep := 1;
            // NumLen 序号的宽度，StartNum 序号的起始值
            NumLen := Length(TrimSpace(SubPattern));
            StartNum := StrToIntDef(SubPattern, 1);
            break;
          end;
        end;
      end;
      Inc(BPos);
    end;
  end;

  // 用于递归的子函数
  procedure DoRename(ID: integer);
  var
    Child: integer;
    AText: string;
  begin
    Dec(Depth);

    // 如果指定了深度，则只处理指定深度的节点
    // 如果没有指定深度，则 Depth 永远小于 0
    if (Depth <= 0) then begin
      if StartIndex = 1 then begin
        // 获取搜索对象
        if SearchNote then AText := GetNote(ID) else AText := GetName(ID);

        // 判断是否有必要执行替换操作
        if Expr.Exec(AText) then
        begin
          // 获取搜索内容
          AText := Expr.Match[0];
          // 根据需要插入序号
          if StartNum > 0 then begin
            AReplaceText := Format('%s%.' + IntToStr(NumLen) + 'd%s', [AReplaceText1, StartNum, AReplaceText2]);
            Inc(StartNum, NumStep);
          end;
          // 执行替换操作，并将替换结果写入节点名称中
          SetName(ID, Expr.Replace(AText, AReplaceText, True));
          Rst.Add(ID, 0, 0);
        end;
      end else
        // 跳过指定数量的节点
        Dec(StartIndex);
    end;
    // 达到指定深度，则不再深入
    if Depth <> 0 then
      for Child in GetChildren(ID) do
        DoRename(Child);

    Inc(Depth);
  end;

begin
  if ASearchText = '' then Exit;

  ASearchText := RegUnEscape(ASearchText);
  AReplaceText := RegUnEscape(AReplaceText);

  ParseOrderNumber;

  Expr := TRegExpr.Create(ASearchText);

  if ID in [RootID, RecyclerID] then
    for Child in GetChildren(ID) do
      DoRename(Child)
  else
    DoRename(ID);

  Expr.Free;
end;

// 通过脚本对节点内容进行多次替换操作
procedure TTreeDB.ScriptReplace(ID: Integer; ScriptText: string; Depth: integer; Rst: TSearchResult);
var
  Script: TScript;
  AText: string;

  // 递归处理指定节点及其子节点
  procedure DoReplace(ID: Integer);
  var
    Child: integer;
  begin
    Dec(Depth);

    // 如果指定了深度，则只处理指定深度的节点
    // 如果没有指定深度，则 Depth 永远小于 0
    if (Depth <= 0) then begin
      AText := Script.ExecScript(GetNote(ID));
      if Script.TextChanged then begin
        SetNote(ID, AText);
        Rst.Add(ID, 0, 0);
      end;
    end;

    // 达到指定深度，则不再深入处理
    if Depth <> 0 then
      for Child in GetChildren(ID) do
        DoReplace(Child);

    Inc(Depth);
  end;

var
  Child: Integer;
begin
  Script := TScript.Create;
  try
    Script.ParseScript(ScriptText);

    if ID in [RootID, RecyclerID] then
      for Child in GetChildren(ID) do
        DoReplace(Child)
    else
      DoReplace(ID);
  finally
    Script.Free;
  end;
end;

initialization

  // 请将 SQLite3 的动态链接库文件放到 lib 目录中
{$ifdef MSWINDOWS}
  LibFile := ConcatPaths([ExtractFileDir(ParamStrUTF8(0)), 'lib', 'sqlite3.dll']);
{$else}
  LibFile := ConcatPaths([ExtractFileDir(ParamStrUTF8(0)), 'lib', 'libsqlite3.so']);
{$endif}
  // 为 SQLite3 控件指定一个动态链接库文件，如果未指定，则使用系统默认的动态链接库文件
  if FileExists(LibFile) then
    SQLiteDefaultLibrary := LibFile;

end.

