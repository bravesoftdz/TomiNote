unit ucommon;

{$mode objfpc}{$H+}
{$codepage UTF8}   // 字符串常量和字符串字面量需要这个选项

interface

uses
  Classes, SysUtils, LCLIntf, RegExpr;

type

  // 用来存储搜索或替换结果的数组
  SizeIntArray = array of SizeInt;

  // 用于存储 TreeDB 单个搜索结果的记录体
  PSearchRecord = ^TSearchRecord;

  TSearchRecord = record
    ID      : Integer;  // Node ID
    UStart  : SizeInt;  // UTF8 Start
    ULength : SizeInt;  // UTF8 Length
  end;

  // 用于存储 TreeDB 搜索结果的列表

  { TSearchResult }

  TSearchResult = class
  private
    FList: TList;
    function  Get(Index: integer): PSearchRecord;
    procedure Put(Index: integer; Item: PSearchRecord);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(PSR: PSearchRecord);
    procedure Add(ID: integer; UStart, ULength: SizeInt);
    procedure Del(Index: integer);
    procedure Clear;
    function  Count: integer;
    property  Items[Index: integer]: PSearchRecord read Get write Put; default;
  end;

function StartsWith(const PStr, PSubStr: PChar;
  const ASize: SizeInt; SubSize: SizeInt): boolean;

function StartsWith(const AStr, ASubStr: string):boolean; inline;

function StartsWithCaseInSensitive(const PStr, PSubStr: PChar;
  const ASize: SizeInt; SubSize: SizeInt): boolean;

function StartsWithCaseInSensitive(const AStr, ASubStr: string):boolean; inline;

function EndsWith(PStr: PChar; const PSubStr: PChar;
  const ASize: SizeInt; SubSize: SizeInt):boolean;

function EndsWith(const AStr, ASubStr: string):boolean; inline;

function EndsWithCaseInSensitive(PStr: PChar; const PSubStr: PChar;
  const ASize: SizeInt; SubSize: SizeInt):boolean;

function EndsWithCaseInSensitive(const AStr, ASubStr: string):boolean; inline;

function TrimLeft(const AStr: string; SubStrs: array of string): string;

function TrimRight(const AStr: string; SubStrs: array of string): string;

function Trim(const AStr: string; SubStrs: array of string): string;

function TrimSpace(const AStr: string): string;

function StripTailSpace(const AText: string): string; inline;

function InvertCase(const C: Char): Char;
function InvertCase(const PStr: PChar; const ASize: SizeInt): string;
function InvertCase(const AStr: String): string;

function UTF8LengthFast(const PStr: PChar; const ASize: SizeInt): SizeInt;
function UTF8LengthFast(const AStr: String): SizeInt; inline;

function UTF8PosToBytePos(const PStr: PChar; const ASize: SizeInt; UPos: SizeInt): SizeInt;
function UTF8PosToBytePos(const AStr: String; const UPos: SizeInt): SizeInt; inline;

function UTF8CodePointLength(const PStr: PChar): SizeInt;
function UTF8CodePointLength(const AStr: string): SizeInt; inline;

procedure ByteMatchesToUTF8Matches(const AStr: string; var AMatches: SizeIntArray);

function FindMatchesChar(const PText: PChar; const ASearchChar: Char;
  const ASize: SizeInt;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt;

function FindMatchesChar(const AText: String; const ASearchChar: Char;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt; inline;

function FindMatchesString(const PText, PSearchText: PChar;
  const ASize, ASearchSize: SizeInt;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt;

function FindMatchesString(const AText, ASearchText: string;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt; inline;

function FindMatchesBoyerMoore(const PText: PChar; PSearchText: PChar;
  const ASize, ASearchSize: SizeInt;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: Boolean = False): SizeInt;

function FindMatchesBoyerMoore(const AText, ASearchText: string;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: Boolean = False): SizeInt; inline;

function FindMatches(const AText, ASearchText: string;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt;

function UTF8FindMatches(const AText, ASearchText: string;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt;

function ReplaceMatches(const AText, ASearchText, AReplaceText: string;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): string;

function UTF8ReplaceMatches(const AText, ASearchText, AReplaceText: string;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): string;

function StringPos(const AText, ASearchText: String;
  const MatchCount: SizeInt = 1; const IgnoreCase: Boolean = False): SizeInt;

function UTF8StringPos(const AText, ASearchText: String;
  const MatchCount: SizeInt = 1; const IgnoreCase: Boolean = False): SizeInt;

function StringReplace(const AText, ASearchText, AReplaceText: String;
  const MatchCount: SizeInt = 0; const IgnoreCase: Boolean = False): string;

function StringCopy(const AStr: string; StartBPos: SizeInt; EndBPos: SizeInt = 0): string;
function UTF8StringCopy(const AStr: string; StartUPos: SizeInt; EndUPos: SizeInt = 0): string;

function StringBefore(const AStr, ASubStr: string; IgnoreCase: Boolean = False): string;
function StringBehind(const AStr, ASubStr: string; IgnoreCase: Boolean = False): string;

function UTF8SearchSpecial(ID: integer; const AText, ASearchText: string;
  IgnoreCase: boolean; Count: Integer; Rst: TSearchResult = nil): SizeInt;
function UTF8ReplaceSpecial(ID: integer; const AText, ASearchText, AReplaceText: string;
  IgnoreCase: boolean; var Count: Integer; Rst: TSearchResult = nil): string;

function ToLF(S: string): string;
function ToLineEnding(S: string): string;

// function Escape_Abandoned(S: string): string;
function Escape(S: string): string;
function UnEscape(S: string): string;
function RegUnEscape(S: string): string;

function ReadFile(FileName: string; StripSpace: boolean): string;
procedure WriteFile(FileName: string; AText: string; StripSpace: boolean);

function FixFileName(FileName: string): string;
function GetNonExistsPath(ToDir, FileName, FileExt: string; SuffixLen: integer): string;
procedure FindFiles(APath: string; Rst: TStringList);

function IsKeyDown(const AKey: integer): boolean; inline;

const
  // 大写转小写映射表，用于快速将源字符串中的字符转换成小写
  LowerCaseArray: array [0..255] of char = (
  #0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,
  #11,#12,#13,#14,#15,#16,#17,#18,#19,#20,
  #21,#22,#23,#24,#25,#26,#27,#28,#29,#30,
  #31,#32,#33,#34,#35,#36,#37,#38,#39,#40,
  #41,#42,#43,#44,#45,#46,#47,#48,#49,#50,
  #51,#52,#53,#54,#55,#56,#57,#58,#59,#60,
  #61,#62,#63,#64,
  'a','b','c','d','e','f','g','h','i','j','k','l','m',
  'n','o','p','q','r','s','t','u','v','w','x','y','z',
  #91,#92,#93,#94,#95,#96,
  'a','b','c','d','e','f','g','h','i','j','k','l','m',
  'n','o','p','q','r','s','t','u','v','w','x','y','z',
  #123,#124,#125,#126,#127,#128,#129,#130,
  #131,#132,#133,#134,#135,#136,#137,#138,#139,#140,
  #141,#142,#143,#144,#145,#146,#147,#148,#149,#150,
  #151,#152,#153,#154,#155,#156,#157,#158,#159,#160,
  #161,#162,#163,#164,#165,#166,#167,#168,#169,#170,
  #171,#172,#173,#174,#175,#176,#177,#178,#179,#180,
  #181,#182,#183,#184,#185,#186,#187,#188,#189,#190,
  #191,#192,#193,#194,#195,#196,#197,#198,#199,#200,
  #201,#202,#203,#204,#205,#206,#207,#208,#209,#210,
  #211,#212,#213,#214,#215,#216,#217,#218,#219,#220,
  #221,#222,#223,#224,#225,#226,#227,#228,#229,#230,
  #231,#232,#233,#234,#235,#236,#237,#238,#239,#240,
  #241,#242,#243,#244,#245,#246,#247,#248,#249,#250,
  #251,#252,#253,#254,#255);

implementation

{ TSearchResult }

constructor TSearchResult.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TSearchResult.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TSearchResult.Get(Index: integer): PSearchRecord;
begin
  Result := PSearchRecord(FList.Items[Index]);
end;

procedure TSearchResult.Put(Index: integer; Item: PSearchRecord);
begin
  FList.Items[Index] := Item;
end;

procedure TSearchResult.Add(PSR: PSearchRecord);
begin
  FList.Add(PSR);
end;

procedure TSearchResult.Add(ID: integer; UStart, ULength: SizeInt);
var
  PSR: PSearchRecord;
begin
  PSR := new(PSearchRecord);
  PSR^.ID      := ID;
  PSR^.UStart  := UStart;
  PSR^.ULength := ULength;

  FList.Add(PSR);
end;

procedure TSearchResult.Del(Index: integer);
begin
  dispose(PSearchRecord(FList.Items[Index]));
  FList.Delete(Index);
end;

function TSearchResult.Count: integer;
begin
  Result := FList.Count;
end;

procedure TSearchResult.Clear;
var
  i: integer;
begin
  for i := FList.Count - 1 downto 0 do
    dispose(Items[i]);
  FList.Clear;
end;

{ 自定义函数 }

// 判断 PStr 是否以 PSubStr 开头
function StartsWith(const PStr, PSubStr: PChar;
  const ASize: SizeInt; SubSize: SizeInt): boolean;
begin
  Result := False;
  if SubSize > ASize then Exit;

  for SubSize := SubSize - 1 downto 0 do
    if PStr[SubSize] <> PSubStr[SubSize] then
      Exit;

  Result := True;
end;

// 判断 PStr 是否以 PSubStr 开头
function StartsWithCaseInSensitive(const PStr, PSubStr: PChar; const ASize: SizeInt; SubSize: SizeInt): boolean;
begin
  Result := False;
  if SubSize > ASize then Exit;

  for SubSize := SubSize - 1 downto 0 do
    if LowerCaseArray[Ord(PStr[SubSize])] <> LowerCaseArray[Ord(PSubStr[SubSize])] then
      Exit;

  Result := True;
end;

// 判断 AStr 是否以 ASubStr 开头
function StartsWith(const AStr, ASubStr: string):boolean; inline;
begin
  Result := StartsWith(PChar(AStr), PChar(ASubStr), Length(AStr), Length(ASubStr));
end;

// 判断 AStr 是否以 ASubStr 开头
function StartsWithCaseInSensitive(const AStr, ASubStr: string):boolean; inline;
begin
  Result := StartsWithCaseInSensitive(PChar(AStr), PChar(ASubStr), Length(AStr), Length(ASubStr));
end;

// 判断 PStr 是否以 PSubStr 结尾
function EndsWith(PStr: PChar; const PSubStr: PChar; const ASize: SizeInt; SubSize: SizeInt):boolean;
begin
  Result := False;
  if SubSize > ASize then Exit;

  Inc(PStr, ASize - SubSize);
  for SubSize := SubSize - 1 downto 0 do
    if PStr[SubSize] <> PSubStr[SubSize] then
      Exit;

  Result := True;
end;

// 判断 AStr 是否以 ASubStr 结尾
function EndsWith(const AStr, ASubStr: string):boolean; inline;
begin
  Result := EndsWith(PChar(AStr), PChar(ASubStr), Length(AStr), Length(ASubStr));
end;

// 判断 PStr 是否以 PSubStr 结尾，区分大小写
function EndsWithCaseInSensitive(PStr: PChar; const PSubStr: PChar;
  const ASize: SizeInt; SubSize: SizeInt): boolean;
begin
  Result := False;
  if SubSize > ASize then Exit;

  Inc(PStr, ASize - SubSize);
  for SubSize := SubSize - 1 downto 0 do
    if LowerCaseArray[Ord(PStr[SubSize])] <> LowerCaseArray[Ord(PSubStr[SubSize])] then
      Exit;

  Result := True;
end;

// 判断 AStr 是否以 ASubStr 结尾，区分大小写
function EndsWithCaseInSensitive(const AStr, ASubStr: string): boolean; inline;
begin
  Result := EndsWithCaseInSensitive(PChar(AStr), PChar(ASubStr), Length(AStr), Length(ASubStr));
end;

// 删除 UTF8 字符串 AStr 左边的多个子字符串
function TrimLeft(const AStr: string; SubStrs: array of string): string;
var
  i, Count: Integer;
  PStr: PChar;
  BLen: SizeInt;
begin
  Count := Length(SubStrs);
  PStr := PChar(AStr);
  BLen := Length(AStr);
  i := 0;
  while i < Count do begin
    if StartsWith(PStr, PChar(SubStrs[i]), BLen, Length(SubStrs[i])) then begin
      Inc(PStr, Length(SubStrs[i]));
      Dec(BLen, Length(SubStrs[i]));
      i := 0;
    end else
      Inc(i);
  end;

  Result := Copy(AStr, Length(AStr) - BLen + 1, BLen);
end;

// 删除 UTF8 字符串 AStr 右边的多个子字符串
function TrimRight(const AStr: string; SubStrs: array of string): string;
var
  i, Count: Integer;
  BLen: SizeInt;
begin
  Count := Length(SubStrs);
  BLen := Length(AStr);
  i := 0;
  while i < Count do begin
    if EndsWith(PChar(AStr), PChar(SubStrs[i]), BLen, Length(SubStrs[i])) then begin
      Dec(BLen, Length(SubStrs[i]));
      i := 0;
    end else
      Inc(i);
  end;

  Result := Copy(AStr, 1, BLen);
end;

// 删除 UTF8 字符串 AStr 首尾的多个子字符串
function Trim(const AStr: string; SubStrs: array of string): string;
begin
  Result := TrimRight(TrimLeft(AStr, SubStrs), SubStrs);
end;

// 删除字符串 AStr 首尾的空白字符
function TrimSpace(const AStr: string): string;
var
  StartBPos, EndBPos: integer;
begin
  StartBPos := 1;
  EndBPos := Length(AStr);

  while (StartBPos <= EndBPos) and (AStr[StartBPos] in [' ', #0..#31, #127]) do
    Inc(StartBPos);

  while (StartBPos <= EndBPos) and (AStr[EndBPos] in [' ', #0..#31, #127]) do
    Dec(EndBPos);

  Result := Copy(AStr, StartBPos, EndBPos - StartBPos + 1);
end;

function StripTailSpace(const AText: string): string;
begin
  Result := RegExpr.ReplaceRegExpr('(?m)[ 　\t]+?$', AText, '', False);
end;

// 反转 C 的大小写状态
function InvertCase(const C: Char): Char;
begin
  case C of
    'a'..'z': Result := Chr(Ord(C) - 32);
    'A'..'Z': Result := Chr(Ord(C) + 32);
    else Result := C;
  end;
end;

// 反转 PStr 所指向的字符串的大小写状态，ASize 指定字符串长度
// 如果 PStr 中不包含字母，则返回空字符串
function InvertCase(const PStr: PChar; const ASize: SizeInt): string;
var
  i: Integer;
  NotChanged: Boolean;
begin
  NotChanged := True;
  SetLength(Result, ASize);
  for i := 0 to Pred(ASize) do
    case PStr[i] of
      'a'..'z': begin Result[i+1] := Chr(Ord(PStr[i]) - 32); NotChanged := False; end;
      'A'..'Z': begin Result[i+1] := Chr(Ord(PStr[i]) + 32); NotChanged := False; end;
      else Result[i+1] := PStr[i];
    end;
  if NotChanged then Result := '';
end;

// 反转 AStr 的大小写状态
// 如果 AStr 中不包含字母，则返回空字符串
function InvertCase(const AStr: String): string; inline;
begin
  Result := InvertCase(PChar(AStr), Length(AStr));
end;

// 获取 PStr 所指向的字符串中 UTF8 字符的个数，ASize 限定字符串长度
// 等 lazUTF8.UTF8LengthFast 稳定后，可以使用 lazUTF8.UTF8LengthFast
// lazUTF8.UTF8LengthFast 的速度非常快，但目前好像还不稳定
function UTF8LengthFast(const PStr: PChar; const ASize: SizeInt): SizeInt;
var
  Pos: Integer;
begin
  Result := 0;
  Pos    := 0;
  while Pos < ASize do begin
    case PStr[Pos] of
        // #0  ..#127: Inc(Pos);
        #192..#223: Inc(Pos, 2);
        #224..#239: Inc(Pos, 3);
        #240..#247: Inc(Pos, 4);
        else Inc(Pos);
    end;
    Inc(Result);
  end;
end;

// 获取字符串 AStr 中 UTF8 字符的个数
function UTF8LengthFast(const AStr: String): SizeInt; inline;
begin
  Result := UTF8LengthFast(PChar(AStr), Length(AStr));
end;

// UTF8 索引转换为字节索引
// PStr  ：要在其中进行搜索的源字符串指针
// ASize ：要搜索的最大字节长度
// UPos  ：要搜索的 UTF8 索引位置，从 1 开始
// 返回值：UTF8 索引对应的字节索引，从 1 开始，0 表示 UPos 不合法，大于 Length(AStr) 表示 UPos 超出范围
function UTF8PosToBytePos(const PStr: PChar; const ASize: SizeInt; UPos: SizeInt): SizeInt;
begin
  if UPos < 1 then Result := -1 else Result := 0;

  while (UPos > 1) and (Result < ASize) do begin
    case PStr[Result] of
      // #0  ..#127: Inc(Pos);
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
    // #0  ..#127: Inc(Pos);
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

// 将 AMatches 中的字节索引转换为 UTF8 字符索引，索引都是从 1 开始
procedure ByteMatchesToUTF8Matches(const AStr: string; var AMatches: SizeIntArray);
var
  i: SizeInt;
  BPos: SizeInt;
  UPos: SizeInt;
begin
  BPos:=1;
  UPos:=1;

  for i := 0 to High(AMatches) do begin
    Inc(UPos, UTF8LengthFast(@AStr[BPos], AMatches[i] - BPos));
    BPos := AMatches[i];
    AMatches[i] := UPos;
  end;
end;

// 查找字符
// PText      ：要在其中进行搜索的源字符串指针
// ASearchChar：要搜索的字符
// ASize      ：要在其中进行搜索的源字符串长度
// AMatches   ：存放搜索结果的数组，数组索引从 0 开始，存放的字符串索引从 1 开始
// MatchCount ：匹配次数，匹配指定次数后则停止搜索，0 表示无限制
// IgnoreCase ：忽略大小写
// 返回值     ：成功匹配的次数
function FindMatchesChar(const PText: PChar; const ASearchChar: Char;
  const ASize: SizeInt;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt;
const
  MATCHESCOUNTRESIZER = 100;
var
  // 缓冲区空间当前容量，这里初始化为 1，只匹配一个结果的情况比较多见
  MatchesAllocatedLimit: SizeInt = 1;

  // 动态分配 Matches 的内存
  procedure ResizeAllocatedMatches;
  begin
    MatchesAllocatedLimit:=FindMatchesChar+MATCHESCOUNTRESIZER;
    SetLength(AMatches,MatchesAllocatedLimit);
  end;

  // 添加一个查找结果
  procedure AddMatch(const APosition: SizeInt); inline;
  begin
    // 根据需要动态分配 AMatches 的内存
    if FindMatchesChar = MatchesAllocatedLimit then ResizeAllocatedMatches;
    // 添加查找结果
    AMatches[FindMatchesChar]:=APosition;
    // 返回已匹配的数量，同时用于与参数 MatchCount 进行比较
    inc(FindMatchesChar);
  end;

var
  BPos: SizeInt;
  ASearchChar2: Char;

begin
  Result := 0;
  // 初始化缓冲区空间
  SetLength(AMatches, MatchesAllocatedLimit);

  // 定义一个大小写相反的字符用来进行“不区分大小写”的比较
  if IgnoreCase then begin
    ASearchChar2 := InvertCase(ASearchChar);
    // 如果两个字符相同，则没必要区分大小写
    IgnoreCase := ASearchChar <> ASearchChar2;
  end;

  BPos := 0;
  if IgnoreCase then begin
    while (BPos < ASize) do begin
      if (PText[BPos] = ASearchChar) or (PText[BPos] = ASearchChar2) then begin
        AddMatch(BPos + 1);
        if FindMatchesChar = MatchCount then break;
      end;
      Inc(BPos);
    end;
  end else begin
    while (BPos < ASize) do begin
      if PText[BPos] = ASearchChar then begin
        AddMatch(BPos + 1);
        if FindMatchesChar = MatchCount then break;
      end;
      Inc(BPos);
    end;
  end;

  // 去掉多余的缓冲区空间
  SetLength(AMatches, Result);
end;

// 查找字符
// AText      ：要在其中进行搜索的源字符串
// ASearchChar：要搜索的字符
// AMatches   ：存放搜索结果的数组，数组索引从 0 开始，存放的字符串索引从 1 开始
// MatchCount ：匹配次数，匹配指定次数后则停止搜索，0 表示无限制
// IgnoreCase ：忽略大小写
// 返回值     ：成功匹配的次数
function FindMatchesChar(const AText: String; const ASearchChar: Char;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt; inline;
begin
  Result := FindMatchesChar(PChar(AText), ASearchChar, Length(AText), AMatches, MatchCount, IgnoreCase);
end;

// 查找字符串（原始算法）
// PText      ：要在其中进行搜索的源字符串指针
// PSearchText：要搜索的字符串指针
// ASize      ：要在其中进行搜索的源字符串长度
// ASearchSize：要搜索的字符串长度
// AMatches   ：存放搜索结果的数组，数组索引从 0 开始，存放的字符串索引从 1 开始
// MatchCount ：匹配次数，匹配指定次数后则停止搜索，0 表示无限制
// IgnoreCase ：忽略大小写
// 返回值     ：成功匹配的次数
function FindMatchesString(const PText, PSearchText: PChar;
  const ASize, ASearchSize: SizeInt;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt;
const
  MATCHESCOUNTRESIZER = 100;
var
  // 缓冲区空间当前容量值
  MatchesAllocatedLimit: SizeInt = 0;

  // 动态分配 Matches 的内存
  procedure ResizeAllocatedMatches;
  begin
    MatchesAllocatedLimit := FindMatchesString + MATCHESCOUNTRESIZER;
    SetLength(AMatches, MatchesAllocatedLimit);
  end;

  // 添加一个查找结果
  procedure AddMatch(const APosition: SizeInt); inline;
  begin
    // 根据需要动态分配 Matches 的内存
    if FindMatchesString = MatchesAllocatedLimit then
      ResizeAllocatedMatches;
    // 添加查找结果
    AMatches[FindMatchesString]:=APosition;
    // 返回已匹配的数量，同时用于与参数 MatchCount 进行比较
    inc(FindMatchesString);
  end;

var
  BPos, SubBPos: SizeInt;

  // 返回 False 表示继续查找，返回 True 表示查找完毕
  function CompareOnce: boolean; inline;
  begin
    if SubBPos < 0 then begin
      AddMatch(BPos + 2);
      Result := FindMatchesString = MatchCount;
      Inc(BPos, ASearchSize);
      Inc(BPos, ASearchSize);
    end else begin
      Dec(BPos, SubBPos);
      Inc(BPos, ASearchSize);
      Result := False
    end;
  end;

var
  ASearchText2: String;
  PSearchText2: PChar;

begin
  Result := 0;
  // 初始化缓冲区空间（清空 AMatches 中已经存在的内容）
  SetLength(AMatches, MatchesAllocatedLimit);
  if ASearchSize=0 then Exit;

  // 定义一个大小写相反的字符串用来进行“不区分大小写”的比较
  if IgnoreCase then begin
    ASearchText2 := InvertCase(PSearchText, ASearchSize);
    // 如果两个字符串相同，则没必要区分大小写
    IgnoreCase := ASearchText2 <> '';
    if IgnoreCase then PSearchText2 := @ASearchText2[1];
  end;

  // 遍历查找
  BPos := ASearchSize - 1;
  if IgnoreCase then begin
    while (BPos < ASize) do begin
      SubBPos := ASearchSize - 1;
      while (SubBPos >= 0) and ((PText[BPos] = PSearchText[SubBPos]) or (PText[BPos] = PSearchText2[SubBPos])) do begin
        dec(BPos);
        dec(SubBPos);
      end;
      if CompareOnce then Break;
    end;
  end else begin
    while (BPos < ASize) do begin
      SubBPos := ASearchSize - 1;
      while (SubBPos >= 0) and (PText[BPos] = PSearchText[SubBPos]) do begin
        dec(BPos);
        dec(SubBPos);
      end;
      if CompareOnce then Break;
    end;
  end;

  // 去掉多余的缓冲区空间
  SetLength(AMatches, Result);
end;

// 查找字符串（原始算法）
// AText      ：要在其中进行搜索的源字符串
// ASearchText：要搜索的字符串
// AMatches   ：存放搜索结果的数组，数组索引从 0 开始，存放的字符串索引从 1 开始
// MatchCount ：匹配次数，匹配指定次数后则停止搜索，0 表示无限制
// IgnoreCase ：忽略大小写
// 返回值     ：成功匹配的次数
function FindMatchesString(const AText, ASearchText: string;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt; inline;
begin
  Result := FindMatchesString(PChar(AText), PChar(ASearchText), Length(AText), Length(ASearchText), AMatches, MatchCount, IgnoreCase);
end;

// 查找字符串（Boyer Moore 算法），从 StrUtils 中复制而来，进行了简单的修改
// PText         ：要在其中进行搜索的源字符串指针
// PSearchText   ：要搜索的字符串指针
// ASize         ：要在其中进行搜索的源字符串长度
// ASearchSize   ：要搜索的字符串长度
// AMatches      ：存放搜索结果的数组，数组索引从 0 开始，存放的字符串索引从 1 开始
// MatchCount    ：匹配次数，匹配指定次数后则停止搜索，0 表示无限制
// IgnoreCase    ：是否忽略大小写
// 返回值        ：成功匹配的次数
function FindMatchesBoyerMoore(const PText: PChar; PSearchText: PChar;
  const ASize, ASearchSize: SizeInt; out AMatches: SizeIntArray;
  const MatchCount: SizeInt = 0; IgnoreCase: Boolean = False): SizeInt;
const
  ALPHABET_LENGHT=256;
  MATCHESCOUNTRESIZER=100;
var
  // 缓冲区空间当前容量值
  MatchesAllocatedLimit: SizeInt = 0;
type
  AlphabetArray=array [0..ALPHABET_LENGHT-1] of SizeInt;

  function Max(const a1,a2: SizeInt): SizeInt; inline;
  begin
    if a1>a2 then Result:=a1 else Result:=a2;
  end;

  // 计算“坏字符”跳转表
  // “坏字符”是Pattern与源字符串进行比较时，不匹配的那个字符
  procedure MakeDeltaJumpTable1(out DeltaJumpTable1: AlphabetArray; const aPattern: PChar; const aPatternSize: SizeInt);
  var
    i: SizeInt;
  begin
    // 如果遇到Pattern中不存在的字符，则跳转到Pattern长度之后继续比较
    for i := 0 to ALPHABET_LENGHT-1 do
      DeltaJumpTable1[i]:=aPatternSize;
    // 如果遇到Pattern中存在的字符，则使该字符与Pattern中相应的字符对齐后，重新
    // 比较，最后一个字符无需处理因为BoyerMoore算法是从Pattern尾部向前进行比较
    // 的，无论如何不需要将当前比较的源字符再次与最后一个字符对齐
    for i := 0 to aPatternSize - 1 - 1 do
      DeltaJumpTable1[Ord(aPattern[i])]:=aPatternSize - 1 - i;
  end;

  // 判断以aPos结尾的“子串”是否在Pattern的尾部，并返回“子串”的长度
  function SuffixLength(const aPattern: PChar; const aPatternSize, aPos: SizeInt): SizeInt; inline;
  var
    i: SizeInt;
  begin
    i:=0;
    while (aPattern[aPos-i] = aPattern[aPatternSize-1-i]) and (i <= aPos) do
      inc(i);
    Result:=i;
  end;

  // 计算“好后缀”跳转表
  // “好后缀”是Pattern与源字符串进行比较时，相匹配的那些字符
  procedure MakeDeltaJumpTable2(var DeltaJumpTable2: SizeIntArray; const aPattern: PChar; const aPatternSize: SizeInt);
  var
    Position: SizeInt;
    SuffixLengthValue: SizeInt;
  begin
    FillByte(DeltaJumpTable2[0], Length(DeltaJumpTable2) * Sizeof(SizeInt), 0);

    // 最后一个字符不做处理，保持为 0，这样当与“坏字符”规则进行比较时，始终选择“坏字符”规则
    for Position := 0 to aPatternSize - 2 do begin
      // 如果当前字符未设置，则先设置为普通跳转值
      if DeltaJumpTable2[Position] = 0 then
        DeltaJumpTable2[Position] := aPatternSize + (aPatternSize - 1 - Position);
      // 判断以当前字符结尾的“子串”是否与“好后缀”匹配，并获取匹配的“子串”长度
      SuffixLengthValue:=SuffixLength(aPattern,aPatternSize,Position);
      // 如果存在这样的“子串”，则修改“好后缀”前一个字符的跳转位置为“子串”的首字符位置
      if SuffixLengthValue > 0 then
        DeltaJumpTable2[aPatternSize - 1 - SuffixLengthValue] := aPatternSize - 1 - Position + SuffixLengthValue;
    end;
  end;

  // 动态分配 Matches 的内存
  procedure ResizeAllocatedMatches;
  begin
    MatchesAllocatedLimit:=FindMatchesBoyerMoore+MATCHESCOUNTRESIZER;
    SetLength(AMatches,MatchesAllocatedLimit);
  end;

  // 添加一个查找结果
  procedure AddMatch(const aPosition: SizeInt); inline;
  begin
    // 根据需要动态分配 Matches 的内存
    if FindMatchesBoyerMoore = MatchesAllocatedLimit then
      ResizeAllocatedMatches;
    // 添加查找结果
    AMatches[FindMatchesBoyerMoore]:=aPosition;
    // 返回已匹配的数量，同时用于与参数 MatchCount 进行比较
    inc(FindMatchesBoyerMoore);
  end;

var
  BPos,SubBPos: SizeInt;
  DeltaJumpTable1: array [0..ALPHABET_LENGHT-1] of SizeInt;
  DeltaJumpTable2: SizeIntArray;

  // 返回 False 表示继续查找，返回 True 表示查找完毕
  function CompareOnce: boolean; inline;
  begin
    // 没有坏字符，完全匹配
    if (SubBPos<0) then begin
      AddMatch(BPos+2);
      Result := FindMatchesBoyerMoore = MatchCount;
      inc(BPos,ASearchSize);
      inc(BPos,ASearchSize);
    end else begin
      // 比较“坏字符”规则的跳转长度和“好后缀”规则的跳转长度，取跳转更长的值
      BPos:=BPos + Max(DeltaJumpTable1[Ord(PText[BPos])],DeltaJumpTable2[SubBPos]);
      Result := False;
    end;
  end;

var
  LowerPattern: string;

begin
  Result:=0;
  // 初始化缓冲区空间（清空 AMatches 中已经存在的内容）
  SetLength(AMatches, MatchesAllocatedLimit);
  if ASearchSize=0 then Exit;

  if IgnoreCase then begin
    // 创建一个小写的 Pattern，用来进行“不区分大小写”的比较
    SetLength(LowerPattern, ASearchSize);
    for BPos:= 0 to Pred(ASearchSize) do
      LowerPattern[BPos+1] := LowerCaseArray[Ord(PSearchText[BPos])];
    PSearchText := @LowerPattern[1];
  end;

  // 创建“坏字符”跳转表
  MakeDeltaJumpTable1(DeltaJumpTable1,PSearchText,ASearchSize);
  // 将“坏字符”跳转表中大写字符的跳转值改成与小写字符相同，这样就可以忽略大小写了
  if IgnoreCase then
    for BPos := Ord('A') to Ord('Z') do
      DeltaJumpTable1[BPos] := DeltaJumpTable1[BPos + 32];

  // 创建“好后缀”跳转表
  SetLength(DeltaJumpTable2,ASearchSize);
  MakeDeltaJumpTable2(DeltaJumpTable2,PSearchText,ASearchSize);

  // 从 Pattern 的尾部向前进行比较
  BPos := ASearchSize - 1;
  if IgnoreCase then begin
    while (BPos < ASize) do begin
      SubBPos:=ASearchSize-1;
      // 查找坏字符
      while (SubBPos>=0) and (LowerCaseArray[Ord(PText[BPos])] = PSearchText[SubBPos]) do begin
        dec(BPos);
        dec(SubBPos);
      end;
      if CompareOnce then break;
    end;
  end else begin
    while (BPos < ASize) do begin
      SubBPos:=ASearchSize-1;
      // 查找坏字符
      while (SubBPos>=0) and (PText[BPos] = PSearchText[SubBPos]) do begin
        dec(BPos);
        dec(SubBPos);
      end;
      if CompareOnce then break;
    end;
  end;

  // 去掉多余的缓冲区空间
  SetLength(AMatches,Result);
end;

// 查找字符串（Boyer Moore 算法），从 StrUtils 中复制而来，进行了简单的修改
// AText         ：要在其中进行搜索的源字符串
// ASearchText   ：要搜索的字符串
// AMatches      ：存放搜索结果的数组，数组索引从 0 开始，存放的字符串索引从 1 开始
// MatchCount    ：匹配次数，匹配指定次数后则停止搜索，0 表示无限制
// IgnoreCase    ：是否忽略大小写
// 返回值        ：成功匹配的次数
function FindMatchesBoyerMoore(const AText, ASearchText: string; out AMatches: SizeIntArray;
  const MatchCount: SizeInt = 0; IgnoreCase: Boolean = False): SizeInt;
begin
  Result := FindMatchesBoyerMoore(PChar(AText), PChar(ASearchText), Length(AText), Length(ASearchText), AMatches, MatchCount, IgnoreCase);
end;

// 搜索字符串，根据 ASearchText 的长度自动选择高效算法
// AText      ：要在其中进行搜索的源字符串
// ASearchText：要搜索的字符串
// AMatches   ：存放搜索结果的数组，数组索引从 0 开始，存放的字符串索引从 1 开始
// MatchCount ：匹配次数，匹配指定次数后则停止搜索，0 表示无限制
// IgnoreCase ：忽略大小写
// 返回值     ：成功匹配的次数
function FindMatches(const AText, ASearchText: string;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt; inline;
begin
  if Length(ASearchText) = 1 then
    Result := FindMatchesChar(PChar(AText), ASearchText[1], Length(AText), AMatches, MatchCount, IgnoreCase)
  else
    Result := FindMatchesBoyerMoore(AText, ASearchText, AMatches, MatchCount, IgnoreCase);
end;

// 搜索UTF8字符串
// AText      ：要在其中进行搜索的源字符串
// ASearchText：要搜索的字符串
// AMatches   ：存放搜索结果的数组，数组索引从 0 开始，存放的字符串索引从 1 开始
// MatchCount ：匹配次数，匹配指定次数后则停止搜索，0 表示无限制
// IgnoreCase ：忽略大小写
// 返回值     ：成功匹配的次数
function UTF8FindMatches(const AText, ASearchText: string;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt; inline;
begin
  if Length(ASearchText) = 1 then
    Result := FindMatchesChar(PChar(AText), ASearchText[1], Length(AText), AMatches, MatchCount, IgnoreCase)
  else
    Result := FindMatchesBoyerMoore(AText, ASearchText, AMatches, MatchCount, IgnoreCase);
  ByteMatchesToUTF8Matches(AText, AMatches);
end;

// 替换字符串
// AText       ：要在其中进行搜索的源字符串
// ASearchText ：要搜索的字符串
// AReplaceText：要替换的字符串
// AMatches    ：存放搜索结果的数组，数组索引从 0 开始，存放的字符串索引从 1 开始
// MatchCount  ：匹配次数，匹配指定次数后则停止搜索，0 表示无限制
// IgnoreCase  ：忽略大小写
// 返回值      ：成功匹配的次数
function ReplaceMatches(const AText, ASearchText, AReplaceText: string;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): string;
var
  ASearchSize: SizeInt;
  AReplaceSize: SizeInt;
  MatchesCount: SizeInt;
  MatchIndex: SizeInt;
  MatchTarget: SizeInt;
  MatchInternal: SizeInt;
  AdvanceIndex: SizeInt;
begin
  MatchesCount := FindMatches(AText, ASearchText, AMatches, MatchCount, IgnoreCase);

  if MatchesCount = 0 then begin
    Result := AText;
    Exit;
  end;

  ASearchSize := Length(ASearchText);
  AReplaceSize := Length(AReplaceText);

  SetLength(Result, Length(AText)+(AReplaceSize-ASearchSize)*MatchesCount);

  MatchIndex:=1;
  MatchTarget:=1;

  for MatchInternal := 0 to Pred(MatchesCount) do begin
    // 上次搜索结果和本次搜索结果之间的字节数，不包括搜索结果本身
    AdvanceIndex:=AMatches[MatchInternal]-MatchIndex;
    // 将搜索结果之间的内容写入 Result 中
    move(AText[MatchIndex],Result[MatchTarget],AdvanceIndex);
    // 修改索引值为替换后的位置
    AMatches[MatchInternal] := MatchTarget;
    // 本次搜索结果之后的位置
    inc(MatchIndex,AdvanceIndex + ASearchSize);
    // 本次替换结果的起始位置
    inc(MatchTarget,AdvanceIndex);
    // 如果替换内容不为空，则将替换内容写入 Result 中
    if AReplaceSize>0 then begin
      move(AReplaceText[1],Result[MatchTarget],AReplaceSize);
      // 本次替换结果之后的位置
      inc(MatchTarget,AReplaceSize);
    end;
  end;
  // 将最后一个搜索结果之后的内容写入 Result 中
  if MatchTarget<=Length(Result) then
    move(AText[MatchIndex],Result[MatchTarget],Length(Result)-MatchTarget+1);
end;

// 替换UTF8字符串
// AText       ：要在其中进行搜索的源字符串
// ASearchText ：要搜索的字符串
// AReplaceText：要替换的字符串
// AMatches    ：存放搜索结果的数组，数组索引从 0 开始，存放的字符串索引从 1 开始
// MatchCount  ：匹配次数，匹配指定次数后则停止搜索，0 表示无限制
// IgnoreCase  ：忽略大小写
// 返回值      ：成功匹配的次数
function UTF8ReplaceMatches(const AText, ASearchText, AReplaceText: string;
  out AMatches: SizeIntArray; const MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): string;
var
  ASearchSize: SizeInt;
  AReplaceSize: SizeInt;
  MatchesCount: SizeInt;
  MatchIndex: SizeInt;
  MatchTarget: SizeInt;
  MatchInternal: SizeInt;
  AdvanceIndex: SizeInt;
  MarchTargetU: SizeInt;
  AReplaceULen: SizeInt;
begin
  MatchesCount := FindMatches(AText, ASearchText, AMatches, MatchCount, IgnoreCase);

  if MatchesCount = 0 then begin
    Result := AText;
    Exit;
  end;

  ASearchSize:=Length(ASearchText);
  AReplaceSize:=Length(AReplaceText);

  SetLength(Result,Length(AText)+(AReplaceSize-ASearchSize)*MatchesCount);

  MatchIndex:=1;
  MatchTarget:=1;
  MarchTargetU:=1;
  AReplaceULen:=UTF8LengthFast(PChar(AReplaceText), Length(AReplaceText));

  for MatchInternal := 0 to Pred(MatchesCount) do begin
    // 上次搜索结果和本次搜索结果之间的字节数，不包括搜索结果本身
    AdvanceIndex:=AMatches[MatchInternal]-MatchIndex;
    // 将搜索结果之间的内容写入 Result 中
    move(AText[MatchIndex],Result[MatchTarget],AdvanceIndex);
    // 将搜索结果的字节索引转换为字符索引
    Inc(MarchTargetU, UTF8LengthFast(@AText[MatchIndex], AdvanceIndex));
    AMatches[MatchInternal] := MarchTargetU;
    Inc(MarchTargetU, AReplaceULen);
    // 本次搜索结果之后的位置
    inc(MatchIndex,AdvanceIndex + ASearchSize);
    // 本次替换结果的起始位置
    inc(MatchTarget,AdvanceIndex);
    // 如果替换内容不为空，则将替换内容写入 Result 中
    if AReplaceSize>0 then begin
      move(AReplaceText[1],Result[MatchTarget],AReplaceSize);
      // 本次替换结果之后的位置
      inc(MatchTarget,AReplaceSize);
    end;
  end;
  // 将最后一个搜索结果之后的内容写入 Result 中
  if MatchTarget<=Length(Result) then
    move(AText[MatchIndex],Result[MatchTarget],Length(Result)-MatchTarget+1);
end;

// 搜索字符串
// AText      ：要在其中进行搜索的源字符串
// ASearchText：要搜索的字符串
// MatchCount ：匹配次数，匹配指定次数后则停止搜索，0 表示无限制
// IgnoreCase ：忽略大小写
// 返回值     ：最后一次匹配的字节位置
function StringPos(const AText, ASearchText: String;
  const MatchCount: SizeInt = 1; const IgnoreCase: Boolean = False): SizeInt;
var
  Matches: SizeIntArray;
begin
  if FindMatches(AText, ASearchText, Matches, MatchCount, IgnoreCase) > 0 then
    Result := Matches[High(Matches)]
  else
    Result := 0;
end;

// 搜索字符串
// AText      ：要在其中进行搜索的源字符串
// ASearchText：要搜索的字符串
// MatchCount ：匹配次数，匹配指定次数后则停止搜索，0 表示无限制
// IgnoreCase ：忽略大小写
// 返回值     ：最后一次匹配的字符位置
function UTF8StringPos(const AText, ASearchText: String;
  const MatchCount: SizeInt = 1; const IgnoreCase: Boolean = False): SizeInt;
var
  Matches: SizeIntArray;
begin
  if UTF8FindMatches(AText, ASearchText, Matches, MatchCount, IgnoreCase) > 0 then
    Result := Matches[High(Matches)]
  else
    Result := 0;
end;

// 替换字符串
// AText       ：要在其中进行搜索的源字符串
// ASearchText ：要搜索的字符串
// AReplaceText：要替换的字符串
// MatchCount  ：匹配次数，匹配指定次数后则停止搜索，0 表示无限制
// IgnoreCase  ：忽略大小写
// 返回值      ：替换之后的字符串
function StringReplace(const AText, ASearchText, AReplaceText: String;
  const MatchCount: SizeInt = 0; const IgnoreCase: Boolean = False): string;
var
  Matches: SizeIntArray;
begin
  Result := ReplaceMatches(AText, ASearchText, AReplaceText, Matches, MatchCount, IgnoreCase);
end;

// 获取 AStr 的子串，索引从 1 开始计数
// StartBPos：子串起始索引，字节索引
// EndBPos  ：子串结束位置的下一个字节的索引，字节索引
function StringCopy(const AStr: string; StartBPos: SizeInt; EndBPos: SizeInt = 0): string;
begin
  if EndBPos <= 0 then EndBPos := Length(AStr) + 1;
  Result := Copy(AStr, StartBPos, EndBPos - StartBPos);
end;

// 获取 AStr 的子串，索引从 1 开始计数
// StartUPos：子串起始索引，字符索引
// EndUPos  ：子串结束位置的下一个字符的索引，字符索引
function UTF8StringCopy(const AStr: string; StartUPos: SizeInt; EndUPos: SizeInt = 0): string;
begin
  StartUPos := UTF8PosToBytePos(AStr, StartUPos);
  if EndUPos <= 0 then
    EndUPos := Length(AStr) + 1
  else
    EndUPos := UTF8PosToBytePos(AStr, EndUPos);
  Result := Copy(AStr, StartUPos, EndUPos - StartUPos);
end;

// 在 AStr 中查找 ASubStr，找到后，返回 ASubStr 之前的子串，不包括 ASubStr 本身
// IgnoreCase：查找时是否忽略大小写
function StringBefore(const AStr, ASubStr: string; IgnoreCase: Boolean = False): string;
var
  EndBPos: SizeInt;
begin
  EndBPos := StringPos(AStr, ASubStr, 1, IgnoreCase);
  if EndBPos = 0 then
    Result := AStr
  else
    Result := StringCopy(AStr, 1, EndBPos);
end;

// 在 AStr 中查找 ASubStr，找到后，返回 ASubStr 之后的子串，不包括 ASubStr 本身
// IgnoreCase：查找时是否忽略大小写
function StringBehind(const AStr, ASubStr: string; IgnoreCase: Boolean = False): string;
var
  StartBPos: SizeInt;
begin
  StartBPos := StringPos(AStr, ASubStr, 1, IgnoreCase);
  if StartBPos = 0 then
    Result := AStr
  else
    Result :=StringCopy(AStr, StartBPos + Length(ASubStr));
end;

// 用于 TreeDB 的字符串搜索函数
// ID         ：要在其中进行搜索的节点 ID
// AText      ：要在其中进行搜索的源字符串
// ASearchText：要搜索的字符串
// IgnoreCase ：忽略大小写
// Count      ：要搜索的数量限制，实际搜索的数量也通过这个参数返回
// Rst        ：存放搜索结果的列表，列表索引从 0 开始，存放的字符串索引从 1 开始
// 返回值     ：成功匹配的次数
function UTF8SearchSpecial(ID: integer; const AText, ASearchText: string;
  IgnoreCase: boolean; Count: Integer; Rst: TSearchResult = nil): SizeInt;
var
  i: Integer;
  Matches: SizeIntArray;
  SearchTextLen: Integer;
begin
  Result := UTF8FindMatches(AText, ASearchText, Matches, Count, IgnoreCase);

  if (Rst <> nil) and (Result > 0) then begin
    SearchTextLen := UTF8LengthFast(ASearchText);
    for i := 0 to Result - 1 do
      Rst.Add(ID, Matches[i], SearchTextLen);
  end;
end;

// 用于 TreeDB 的字符串替换函数
// ID          ：要在其中进行搜索的节点 ID
// AText       ：要在其中进行搜索的源字符串
// ASearchText ：要搜索的字符串
// AReplaceText：要替换的字符串
// IgnoreCase  ：忽略大小写
// Count       ：要搜索的数量限制，实际搜索的数量也通过这个参数返回
// Rst         ：存放搜索结果的列表，列表索引从 0 开始，存放的字符串索引从 1 开始
// 返回值      ：成功匹配的次数
function UTF8ReplaceSpecial(ID: integer; const AText, ASearchText, AReplaceText: string;
  IgnoreCase: boolean; var Count: Integer; Rst: TSearchResult = nil): string;
var
  i: Integer;
  Matches: SizeIntArray;
  ReplaceTextLen: Integer;
begin
  Result := UTF8ReplaceMatches(AText, ASearchText, AReplaceText, Matches, Count, IgnoreCase);

  Count := Length(Matches);
  if (Rst <> nil) and (Count > 0) then begin
    ReplaceTextLen := UTF8LengthFast(AReplaceText);
    for i := 0 to High(Matches) do
      Rst.Add(ID, Matches[i], ReplaceTextLen);
  end;

end;

// 将 S 中的 \r\n 或 \r 转换为 \n
function ToLF(S: string): string;
var
  StartBPos  : integer;
  BPos, BLen : integer;
  ResultBPos : integer;
begin
  if S = '' then begin
    Result := '';
    Exit;
  end;

  StartBPos  := 1;
  BPos       := 1;
  BLen       := Length(S);
  ResultBPos := 1;
  SetLength(Result, BLen);

  while BPos < BLen do begin
    if S[BPos] = #13 then begin
      // 将两个 #13 之间的内容写入 Result 中
      move(S[StartBPos], Result[ResultBPos], BPos - StartBPos);
      // 更新 Result 的写入位置
      Inc(ResultBPos, BPos - StartBPos);
      // 继续搜索下一个字符
      Inc(BPos);
      // 搜索结果的下一个位置
      StartBPos := BPos;
      // 对于 #13#10，直接忽略掉 #13 即可
      if S[BPos] = #10 then continue;
      // 对于单独的 #13，将其转换为 #10 写入 Result 中
      Result[ResultBPos] := #10;
      // 更新 Result 的写入位置
      Inc(ResultBPos);
    end else
      // 其它字符直接跳过
      Inc(BPos);
  end;
  // 将最后一个 #13（除了尾部的 #13）之后的内容写入 Result
  move(S[StartBPos], Result[ResultBPos], BLen - StartBPos);
  // 更新 Result 的写入位置
  Inc(ResultBPos, BLen - StartBPos);
  // 处理尾部的 #13
  if S[BLen] = #13 then
    Result[ResultBPos] := #10
  else
    Result[ResultBPos] := S[BLen];
  // 去掉多余的缓冲区空间
  SetLength(Result, ResultBPos);
end;

// 将 S 中的 \n 转换为系统默认的换行符
function ToLineEnding(S: string): string;
begin
  Result := StringReplace(S, #10, LineEnding);
end;

{
// 将 S 中的特殊字符（#13 #10 #9 \）转换为转义字符(\r \n \t \\)
// 字符串中原有的 \r \n \t \\ 不会进行处理
// 这个转换函数会影响正则表达式字符串，所以弃用
function Escape_Abandoned(S: string): string;
var
  StartPos  : SizeInt;
  i         : SizeInt;
  c         : Char;
  MeetSlash : boolean;
begin
  StartPos  := 1;
  Result    := '';
  MeetSlash := False;

  for i := 1 to Length(S) do begin
    c := S[i];
    if MeetSlash then begin
      MeetSlash := False;
      case c of
        '\', 'r', 'n', 't': continue;
      end;
    end else begin
      case c of
        '\': begin MeetSlash := True; continue; end;
        #13: c := 'r';
        #10: c := 'n';
        #9 : c := 't';
        else continue;
      end;
    end;
    Result := Result + Copy(S, StartPos, i - StartPos) + '\' + c;
    StartPos := i + 1;
  end;

  if StartPos <= Length(S) then
    Result := Result + Copy(S, StartPos, Length(S) - StartPos + 1);
end;
}

// 将 S 中的特殊字符（#13 #10 #9）转换为转义字符(\r \n \t)
// 这个转换函数不会处理 \ 字符，所以可以用于正则表达式字符串
// 只有特殊的 \ 字符会被处理，比如 '\'#10 会被转换成 '\\\n'
function Escape(S: string): string;
var
  StartPos  : SizeInt;
  i         : SizeInt;
  c         : Char;
  SS        : string;
  MeetSlash : Boolean;
begin
  StartPos  := 1;
  Result    := '';
  SS        := '';
  MeetSlash := False;

  for i := 1 to Length(S) do begin
    c := S[i];
    if MeetSlash then begin
      MeetSlash := False;
      case c of
        #13: SS := '\\r';
        #10: SS := '\\n';
        #9 : SS := '\\t';
        else continue;
      end;
    end else begin
      case c of
        '\': begin MeetSlash := True; continue; end;
        #13: SS := '\r';
        #10: SS := '\n';
        #9 : SS := '\t';
        else continue;
      end;
    end;
    Result := Result + Copy(S, StartPos, i - StartPos) + SS;
    StartPos := i + 1;
  end;

  if StartPos <= Length(S) then
    Result := Result + Copy(S, StartPos, Length(S) - StartPos + 1);
end;

// 将 S 中的转义字符（\r \n \t \\ \xFF）转换为特殊字符（#13 #10 #9 \ Char）
// 其它地方单独的 \ 字符会被保留
// 与 Escape 函数配合使用
function UnEscape(S: string): string;
var
  StartBPos : SizeInt;
  BPos      : SizeInt;
  BLen      : SizeInt;
  MeetSlash : boolean;
begin
  StartBPos := 1;
  BPos      := 1;
  BLen      := Length(S);

  Result    := '';
  MeetSlash := False;

  while BPos <= BLen do begin
    if MeetSlash then begin
      MeetSlash := False;
      case S[BPos] of
        '\': Result := Result + '\';
        'r': Result := Result + #13;
        'n': Result := Result + #10;
        't': Result := Result + #9;
        'x': begin  // 为了与正则表达式兼容，x 必须用小写
          if (BPos <= BLen - 2) and
             (S[BPos+1] in ['0'..'9', 'A'..'F', 'a'..'z']) and
             (S[BPos+2] in ['0'..'9', 'A'..'F', 'a'..'z']) then
          begin
            Result := Result + Chr(StrToInt('$' + Copy(S, BPos+1, 2)));
            Inc(BPos, 2);
            StartBPos := BPos + 1;
          end else
            Result := Result + '\' + S[BPos];
        end;
        else Result := Result + '\' + S[BPos];
      end;
    end else if S[BPos] = '\' then begin
      MeetSlash := True;
      Result := Result + Copy(S, StartBPos, BPos - StartBPos);
      StartBPos := BPos + 2;
    end;
    Inc(BPos);
  end;

  if StartBPos <= BLen then
    Result := Result + Copy(S, StartBPos, Length(S) - StartBPos + 1);
end;

// 将 S 中的转义字符（\r \n \t \xFF）转换为特殊字符（#13 #10 #9 Char）
// 其它地方单独的 \ 字符会被保留
// 与 Escape 函数配合使用
// 专用于正则表达式的替换内容，因为正则表达式自己会处理 \\
function RegUnEscape(S: string): string;
var
  StartBPos : SizeInt;
  BPos      : SizeInt;
  BLen      : SizeInt;
  MeetSlash : boolean;
begin
  StartBPos := 1;
  BPos      := 1;
  BLen      := Length(S);

  Result    := '';
  MeetSlash := False;

  while BPos <= BLen do begin
    if MeetSlash then begin
      MeetSlash := False;
      case S[BPos] of
        'r': Result := Result + #13;
        'n': Result := Result + #10;
        't': Result := Result + #9;
        'x': begin  // 为了与正则表达式兼容，x 必须用小写
          if (BPos <= BLen - 2) and
             (S[BPos+1] in ['0'..'9', 'A'..'F', 'a'..'z']) and
             (S[BPos+2] in ['0'..'9', 'A'..'F', 'a'..'z']) then
          begin
            Result := Result + Chr(StrToInt('$' + Copy(S, BPos+1, 2)));
            Inc(BPos, 2);
            StartBPos := BPos + 1;
          end else
            Result := Result + '\' + S[BPos];
        end;
        else Result := Result + '\' + S[BPos];
      end;
    end else if S[BPos] = '\' then begin
      MeetSlash := True;
      Result := Result + Copy(S, StartBPos, BPos - StartBPos);
      StartBPos := BPos + 2;
    end;
    Inc(BPos);
  end;

  if StartBPos <= BLen then
    Result := Result + Copy(S, StartBPos, Length(S) - StartBPos + 1);
end;

// 读取文本文件内容，同时将换行符统一为 #10
function ReadFile(FileName: string; StripSpace: Boolean): string;
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(FileName, fmOpenRead);
  try
    // 忽略 UTF8-BOM
    if (AStream.Size >= 3) and (AStream.ReadByte = $EF) and (AStream.ReadByte = $BB) and (AStream.ReadByte = $BF) then begin
      SetLength(Result, AStream.Size - 3);
      AStream.ReadBuffer(Result[1], AStream.Size - 3);
    end else begin
      AStream.Seek(0, SoFromBeginning);
      SetLength(Result, AStream.Size);
      AStream.ReadBuffer(Result[1], AStream.Size);
    end;
    Result := ToLF(Result);
    if StripSpace then
      Result := StripTailSpace(Result);
  finally
    AStream.Free;
  end;
end;

// 将字符串写入文件，同时将换行符转换为当前系统默认的换行符
procedure WriteFile(FileName: string; AText: string; StripSpace: boolean);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(FileName, fmCreate);
  try
    if StripSpace then
      AText := StripTailSpace(AText);
    AText := ToLineEnding(AText);
    AStream.WriteBuffer(AText[1], Length(AText));
  finally
    AStream.Free;
  end;
end;

// 将文件名中的非法字符转换为下划线
function FixFileName(FileName: string): string;
var
  i: integer;
begin
  for i := 0 to High(FileName) do
    if FileName[i] in ['/', '\', ':', '<', '>', '*', '?', '|', '"', #9, #10, #13] then
      FileName[i] := '_';
  Result := FileName;
end;

// 获取一个不存在的文件名（避免与现有文件重名）
// ToDir    ：文件所在路径
// FileName ：文件名，不包含扩展名
// FileExt  ：文件扩展名
// SuffixLen：给文件名添加数字序号时的序号长度
function GetNonExistsPath(ToDir, FileName, FileExt: string; SuffixLen: integer): string;
var
  i: integer;
  Suffix: string;
begin
  Result := ConcatPaths([ToDir, FileName + FileExt]);
  i := 1;
  while FileExists(Result) or DirectoryExists(Result) do begin
    Suffix := Format('%.' + IntToStr(SuffixLen) + 'd', [i]);
    Result := ConcatPaths([ToDir, FileName + ' (' + Suffix + ')' + FileExt]);
    Inc(i);
  end;
end;

// 搜索 APath 中的所有文件和子目录，结果存入 Rst 返回
procedure FindFiles(APath: string; Rst: TStringList);
var
  SearchRec: TRawbyteSearchRec;
begin
  if FindFirst(ConcatPaths([APath, '*']), faAnyFile or faDirectory, SearchRec) = 0 then
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        continue;
      Rst.Add(SearchRec.Name);
    until FindNext(SearchRec) <> 0;
  FindClose(SearchRec);
end;

// 判断某个按键是否按下
function IsKeyDown(const AKey: integer): boolean; inline;
begin
  Result := GetKeyState(AKey) < 0;
end;

end.
