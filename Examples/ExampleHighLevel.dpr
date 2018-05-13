program ExampleHighLevel;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Neslib.Clang.Api in '..\Neslib.Clang.Api.pas',
  Neslib.Clang in '..\Neslib.Clang.pas';

procedure Run;
var
  Index: IIndex;
  TU: ITranslationUnit;
  Cursor: TCursor;
begin
  Index := TIndex.Create(False, False);
  TU := Index.ParseTranslationUnit('sample.cpp', []);
  if (TU = nil) then
    raise Exception.Create('Unable to parse C++ file');

  Cursor := TU.Cursor;
  Cursor.VisitChildren(
    function(const ACursor, AParent: TCursor): TChildVisitResult
    begin
      Write(ACursor.Kind.Spelling);

      if (ACursor.Spelling = '') then
        WriteLn
      else
        WriteLn(' (', ACursor.Spelling, ')');

      Result := TChildVisitResult.Recurse;
    end);

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
