program ExampleLowLevel;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Neslib.Clang.Api in '..\Neslib.Clang.Api.pas';

function ChildVisitor(ACursor, AParent: TCXCursor;
  AClientData: TCXClientData): TCXChildVisitResult; cdecl;
var
  Kind: TCXCursorKind;
  Str: TCXString;
  S: AnsiString;
begin
  Kind := clang_getCursorKind(ACursor);
  Str := clang_getCursorKindSpelling(Kind);

  S := AnsiString(clang_getCString(Str));
  Write(S);
  clang_disposeString(Str);

  Str := clang_getCursorSpelling(ACursor);
  S := AnsiString(clang_getCString(Str));
  if (S = '') then
    WriteLn
  else
    WriteLn(' (', S, ')');
  clang_disposeString(Str);

  Result := CXChildVisit_Recurse;
end;

procedure Run;
var
  Index: TCXIndex;
  TU: TCXTranslationUnit;
  Cursor: TCXCursor;
begin
  Index := clang_createIndex(0, 0);
  try
    TU := clang_parseTranslationUnit(Index, 'sample.cpp', nil, 0, nil, 0, 0);
    if (TU = nil) then
      raise Exception.Create('Unable to parse C++ file');

    try
      Cursor := clang_getTranslationUnitCursor(TU);
      clang_visitChildren(Cursor, ChildVisitor, nil);
    finally
      clang_disposeTranslationUnit(TU);
    end;
  finally
    clang_disposeIndex(Index);
  end;

  WriteLn;
  Write('Press Enter to finish...');
  ReadLn;
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
