unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StrUtils, Vcl.ExtCtrls, FileCtrl, ExplodeFunc;

type
  TForm1 = class(TForm)
    btnFile: TButton;
    memoOut: TMemo;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnFileClick(Sender: TObject);
  private
    { Private declarations }
    procedure AddLine(s: string; size: integer; percent: single);
    function PadString(s: string; i: integer): string;
    function GetSize(s: string): integer;
    procedure RunCommand(id: integer; exename, infile, outfile: string);
    procedure RunCommand_(command: string);
    function FindCommand(exename: string): boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  inifile: textfile;
  thisfolder, testfile: string;
  commands: array of string;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var inifile: textfile;
  s: string;
begin
  thisfolder := ExtractFilePath(Application.ExeName); // Get folder for this program.
  if FileExists('bin\commands.ini') then // Check if ini file exists.
    begin
    AssignFile(inifile,'bin\commands.ini'); // Open ini file.
    Reset(inifile);
    while not eof(inifile) do
      begin
      ReadLn(inifile,s);
      if s <> '' then // Check if line is blank.
        begin
        SetLength(commands,Length(commands)+1); // Expand list of commands.
        commands[Length(commands)-1] := s; // Add line from ini file.
        end;
      end;
    CloseFile(inifile);
    end
  else
    begin
    ShowMessage('bin\commands.ini not found.'); // ini file missing.
    btnFile.Enabled := false; // Prevent further actions.
    end;
end;

procedure TForm1.btnFileClick(Sender: TObject);
var rec: TSearchRec;
  insize, outsize, i: integer;
begin
  if dlgOpen.Execute then
    begin
    testfile := dlgOpen.FileName;
    insize := GetSize(testfile); // Get size of uncompressed file.
    AddLine('Uncompressed',insize,0);
    if FindFirst(thisfolder+'bin\*.exe', faAnyFile-faDirectory, rec) = 0 then // Every exe file in bin folder.
      begin
      repeat
      if FindCommand('bin\'+rec.Name) = false then // Check if exe has its own command in ini file.
        begin
        RunCommand(0,'bin\'+rec.Name,testfile,'temp.bin'); // Run exe and create temp file.
        outsize := GetSize(thisfolder+'temp.bin'); // Get size of compressed file.
        if outsize > 0 then AddLine(rec.Name,outsize,outsize/insize);
        DeleteFile(thisfolder+'temp.bin'); // Delete temp file.
        end;
      until FindNext(rec) <>0;
      FindClose(rec);
      end;
    if Length(commands) > 1 then // Check if additional commands are listed in ini.
      for i := 1 to Length(commands)-1 do
        begin
        RunCommand(i,'',testfile,'temp.bin'); // Run exe and create temp file.
        outsize := GetSize(thisfolder+'temp.bin'); // Get size of compressed file.
        if outsize > 0 then AddLine(Explode(Explode(commands[i],'.exe',0),'\',1)+'.exe',outsize,outsize/insize);
        DeleteFile(thisfolder+'temp.bin'); // Delete temp file.
        end;
    end;
end;

procedure TForm1.AddLine(s: string; size: integer; percent: single);
begin
  s := PadString(s,30);
  s := PadString(s+IntToStr(size),40);
  if percent <> 0 then s := s+FloatToStr(Round(percent*100))+'%'; // Display percentage compression ratio.
  memoOut.Lines.Add(s);
end;

function TForm1.PadString(s: string; i: integer): string;
begin
  while Length(s) < i do s := s+' '; // Add spaces until string is specified length.
  result := s;
end;

function TForm1.GetSize(s: string): integer;
var myfile: file;
begin
  if FileExists(s) then
    begin
    AssignFile(myfile,s);
    Reset(myfile,1);
    result := FileSize(myfile); // Get file size of specified file.
    CloseFile(myfile);
    end
  else result := 0; // Return 0 if file not found.
end;

procedure TForm1.RunCommand(id: integer; exename, infile, outfile: string);
var s: string;
begin
  s := ReplaceStr(commands[id],'{program}',exename);
  s := ReplaceStr(s,'{infile}',infile);
  s := ReplaceStr(s,'{outfile}',outfile); // Insert parameters into line from ini.
  RunCommand_(s); // Actually run command.
end;

procedure TForm1.RunCommand_(command: string);
var StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
begin
  FillChar(StartInfo,SizeOf(TStartupInfo),#0);
  FillChar(ProcInfo,SizeOf(TProcessInformation),#0);
  StartInfo.cb := SizeOf(TStartupInfo);
  // Run program.
  if not CreateProcess(nil,PChar(command),nil,nil,false,CREATE_NEW_PROCESS_GROUP+NORMAL_PRIORITY_CLASS+CREATE_NO_WINDOW,nil,nil,StartInfo,ProcInfo) then
    begin
    memoOut.Lines.Add('Command failed - '+command);
    end
  else
    begin
    while WaitForSingleObject(ProcInfo.hProcess, 10) > 0 do Application.ProcessMessages;
    CloseHandle(ProcInfo.hProcess);
    CloseHandle(ProcInfo.hThread);
    end;
end;

function TForm1.FindCommand(exename: string): boolean;
var i: integer;
begin
  result := false;
  for i := 1 to Length(commands) do
    if AnsiPos(exename,commands[i]) <> 0 then result := true; // True if any command contains exe name.
end;

end.
