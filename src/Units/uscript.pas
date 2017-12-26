unit uscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, ucommon;

type
  // 操作类型：普通替换，正则表达式替换，一对一普通替换
  TReplaceMode = (rmUnknow, rmNormal, rmRegExpr, rmOneOnOne);

  // 脚本中的单个替换操作
  TReplaceInfo = record
    Mode: TReplaceMode;
    Loop: Integer;
    Srch: string;
    Repl: string;
    IgnoreCase: boolean;
  end;

  // 存放脚本中的所有替换操作
  TReplaceList = array of TReplaceInfo;

  { TScript }

  TScript = class
  private
    FReplaceList  : TReplaceList;
    FExpr         : TRegExpr;
    FTextChanged  : boolean;
    procedure AddReplaceInfo(Mode: TReplaceMode; Loop: integer; Srch, Repl: string; IgnoreCase: boolean);
    procedure Reset;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseScript(ScriptText: string);
    function  ExecScript(AText: string): string;
    property  TextChanged: boolean read FTextChanged;
  end;

implementation

{ TScript }

constructor TScript.Create;
begin
  inherited Create;
  FExpr := TRegExpr.Create;
end;

destructor TScript.Destroy;
begin
  FExpr.Free;
  Reset;
  inherited Destroy;
end;

// 添加一个替换操作
procedure TScript.AddReplaceInfo(Mode: TReplaceMode; Loop: integer; Srch, Repl: string; IgnoreCase: boolean);
begin
  SetLength(FReplaceList, Length(FReplaceList) + 1);
  FReplaceList[High(FReplaceList)].Mode := Mode;
  FReplaceList[High(FReplaceList)].Loop := Loop;
  FReplaceList[High(FReplaceList)].Srch := Srch;
  FReplaceList[High(FReplaceList)].Repl := Repl;
  FReplaceList[High(FReplaceList)].IgnoreCase := IgnoreCase;
end;

procedure TScript.Reset;
var
  i: integer;
begin
  for i := 0 to High(FReplaceList) do begin
    FReplaceList[i].Srch := '';
    FReplaceList[i].Repl := '';
  end;
  SetLength(FReplaceList, 0);
end;

// 解析脚本，将文本格式的脚本转换为数组形式，便于反复读取
procedure TScript.ParseScript(ScriptText: string);
var
  Strs       : TStringList;
  LineIndex  : integer;
  Line       : string;

  Mode       : TReplaceMode;
  IgnoreCase : boolean;
  Loop       : integer;
  Srch       : string;
  Repl       : string;
begin
  Strs := TStringList.Create;
  try
    // 初始化为默认值
    Mode := rmNormal;
    Loop := 0;
    IgnoreCase := False;

    // 开始解析脚本
    Strs.Text := ScriptText;
    for LineIndex := 0 to Strs.Count - 1 do begin
      Line := Strs[LineIndex];
      if Length(Line) >= 5 then begin
        case LowerCase(Copy(Line, 1, 5)) of
          // 替换模式
          'mode=':
          case LowerCase(TrimSpace(StringCopy(Line, 6))) of
            'regexpr'  : Mode := rmRegExpr;
            'oneonone' : Mode := rmOneOnOne;
            else         Mode := rmNormal;
          end;
          // 区分大小写
          'case=': IgnoreCase := LowerCase(TrimSpace(StringCopy(Line, 6))) = 'false';
          // 循环次数（Loop > 0 表示遇到循环标记，将执行循环操作，否则继续向后执行）
          'loop=': Loop := StrToIntDef(StringCopy(Line, 6), 1);
          // 搜索内容
          'srch=': Srch := StringCopy(Line, 6);
          // 替换内容
          'repl=': if Srch <> '' then begin
            Repl := StringCopy(Line, 6);
            AddReplaceInfo(Mode, Loop, Srch, Repl, IgnoreCase);
            // 复位
            Srch := '';
            Repl := '';
            Loop := 0;
          end;
        end;
      end;
    end;
    // 无论结尾有没有 Loop=，依然处理前一个 Loop 信息
    AddReplaceInfo(rmUnknow, 1, '', '', False);
  finally
    Strs.Free;
  end;
end;

// 执行一次脚本
// ID    ：要替执行替换操作的节点
// 返回值：是否有内容被替换
function TScript.ExecScript(AText: string): string;
var
  ReplaceIndex : integer;
  OneIndex     : integer;
  LoopStart    : integer;
  LoopCount    : integer;
  ASearchText  : string;
  AReplaceText : String;
  Ones1        : TStringList;
  Ones2        : TStringList;
  LoopFound    : boolean;
begin
  FTextChanged := False;
  ReplaceIndex := 0;
  LoopStart    := -1;
  LoopCount    := 1;
  LoopFound    := False;
  // 循环执行脚本中的所有操作
  while ReplaceIndex <= High(FReplaceList) do begin
    // 遇到 Loop 标记，而且不是循环首
    if (FReplaceList[ReplaceIndex].Loop > 0) and (LoopStart <> ReplaceIndex) then begin
      if LoopFound and (LoopCount > 1) then begin // 循环没有结束
        LoopFound := False;
        Dec(LoopCount);
        ReplaceIndex := LoopStart;
      end else begin // 循环已经结束
        LoopStart := ReplaceIndex;
        LoopCount := FReplaceList[ReplaceIndex].Loop;
        // 复位 LoopFound 标记，用于判断下次循环能否找到匹配项
        LoopFound := False;
      end;
    end;
    case FReplaceList[ReplaceIndex].Mode of
      // 普通替换操作
      rmNormal: begin
        ASearchText := UnEscape(FReplaceList[ReplaceIndex].Srch);
        AReplaceText := UnEscape(FReplaceList[ReplaceIndex].Repl);
        // 判断是否需要替换
        if StringPos(AText, ASearchText, 1, FReplaceList[ReplaceIndex].IgnoreCase) > 0 then begin
          AText := StringReplace(AText, ASearchText, AReplaceText, 0, FReplaceList[ReplaceIndex].IgnoreCase);
          FTextChanged := True;
          LoopFound := True;
        end;
      end;
      // 正则表达式替换操作
      rmRegExpr: begin
        if FReplaceList[ReplaceIndex].IgnoreCase then
          ASearchText := '(?i)' + RegUnEscape(FReplaceList[ReplaceIndex].Srch)
        else
          ASearchText := '(?-i)' + RegUnEscape(FReplaceList[ReplaceIndex].Srch);
        AReplaceText := RegUnEscape(FReplaceList[ReplaceIndex].Repl);

        FExpr.Expression := ASearchText;
        // 判断是否需要替换
        if FExpr.Exec(AText) then begin
          AText := FExpr.Replace(AText, AReplaceText, True);
          FTextChanged := True;
          LoopFound := True;
        end;
      end;
      // 一对一替换操作
      rmOneOnOne: begin
        Ones1 := TStringList.Create;
        Ones2 := TStringList.Create;
        try
          // 分割各个原子
          Ones1.Delimiter := '|';
          Ones2.Delimiter := '|';
          Ones1.StrictDelimiter := True;
          Ones2.StrictDelimiter := True;
          Ones1.DelimitedText := FReplaceList[ReplaceIndex].Srch;
          Ones2.DelimitedText := FReplaceList[ReplaceIndex].Repl;
          // 依次对各个原子进行替换操作
          for OneIndex := 0 to Pred(Ones1.Count) do begin
            ASearchText := UnEscape(Ones1[OneIndex]);
            if OneIndex < Ones2.Count then
              AReplaceText := UnEscape(Ones2[OneIndex])
            else
              AReplaceText := '';
            // 判断是否需要替换
            if StringPos(AText, ASearchText, 1, FReplaceList[ReplaceIndex].IgnoreCase) > 0 then begin
              AText := StringReplace(AText, ASearchText, AReplaceText, 0, FReplaceList[ReplaceIndex].IgnoreCase);
              FTextChanged := True;
              LoopFound := True;
            end;
          end;
        finally
          Ones1.Free;
          Ones2.Free;
        end;
      end;
    end;
    Inc(ReplaceIndex);
  end;
  Result := AText;
end;

end.

