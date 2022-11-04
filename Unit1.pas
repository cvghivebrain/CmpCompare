unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StrUtils, Vcl.ExtCtrls, FileCtrl;

type
  TForm1 = class(TForm)
    btnFile: TButton;
    memoOut: TMemo;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnFileClick(Sender: TObject);
  private
    { Private declarations }
    function PadString(s: string; i: integer): string;
    function GetSize(s: string): integer;
    procedure RunCommand(command: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  inifile: textfile;
  thisfolder, testfile: string;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  thisfolder := ExtractFilePath(Application.ExeName); // Get folder for this program.
end;

procedure TForm1.btnFileClick(Sender: TObject);
var rec: TSearchRec;
  fs, ts: integer;
  s: string;
begin
  if dlgOpen.Execute then
    begin
    testfile := dlgOpen.FileName;
    //memoOut.Lines.Add(testfile);
    fs := GetSize(testfile); // Get size of uncompressed file.
    s := PadString('Uncompressed',30);
    s := PadString(s+IntToStr(fs),40);
    memoOut.Lines.Add(s);
    if FindFirst(thisfolder+'bin\*.exe', faAnyFile-faDirectory, rec) = 0 then // Every exe file in bin folder.
      begin
      repeat
      RunCommand('"'+thisfolder+'bin\'+rec.Name+'" "'+testfile+'" "'+thisfolder+'temp.bin"'); // Run exe and create temp file.
      ts := GetSize(thisfolder+'temp.bin'); // Get size of compressed file.
      s := PadString(rec.Name,30);
      s := PadString(s+IntToStr(ts),40);
      s := PadString(s+FloatToStr(Round((ts/fs)*100))+'%',40); // Display percentage compression ratio.
      memoOut.Lines.Add(s);
      DeleteFile(thisfolder+'temp.bin'); // Delete temp file.
      until FindNext(rec) <>0;
      FindClose(rec);
      end;
    end;
end;

function TForm1.PadString(s: string; i: integer): string;
begin
  while Length(s) < i do s := s+' '; // Add spaces until string is specified length.
  result := s;
end;

function TForm1.GetSize(s: string): integer;
var myfile: file;
begin
  AssignFile(myfile,s);
  Reset(myfile,1);
  result := FileSize(myfile);
  CloseFile(myfile);
end;

procedure TForm1.RunCommand(command: string);
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
    //memoOut.Lines.Add(command);
    while WaitForSingleObject(ProcInfo.hProcess, 10) > 0 do Application.ProcessMessages;
    CloseHandle(ProcInfo.hProcess);
    CloseHandle(ProcInfo.hThread);
    end;
end;

end.
