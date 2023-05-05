unit Fstatus;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{-------------------------------------------------------------------}
{                    Unit:    Fstatus.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                    Ported to Lazarus by: Zorba Lopez Rivera       }
{                    Date:    15/05/23                              }
{                                                                   }
{   MDI child form that lists error/warning messages and the        }
{   times at which status changes occur after a network             }
{   simulation has been run.                                        }
{-------------------------------------------------------------------}

interface

uses
{$IFDEF WINDOWS}
  Windows, Messages,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, fileutil,
  Forms, Dialogs, StdCtrls, ComCtrls, Clipbrd, System.UITypes,
  Xprinter, Uglobals, Uutils, ResourceStrings;

type

  { TStatusForm }

  TStatusForm = class(TForm)
    FileViewer: TListBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    procedure SetFont;
  public
    { Public declarations }
    procedure CopyTo;
    procedure Print(Destination: TDestination);
    procedure RefreshStatusReport;
    procedure SelectText(const S: String);
  end;


implementation

{$R *.lfm}

uses Dcopy, Fmain;


procedure TStatusForm.FormClose(Sender: TObject; var Action: TCloseAction);
//--------------------------------------
// OnClose handler for form.
//--------------------------------------
begin
  MainForm.DeleteMDIMnu(self);
  MainForm.MapForm.FormActivate(self);
  Action := caFree;
end;


procedure TStatusForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F1) then MainForm.LaunchHelp(177);
end;


procedure TStatusForm.FormActivate(Sender: TObject);
//---------------------------------------------------
// OnActivate handler for the form.
// Disables Options speedbutton on MainForm.
//---------------------------------------------------
begin
  MainForm.TBOptions.Enabled := False;
  MainForm.FormActive:= Caption;
  MainForm.FormActivated(self);
end;

procedure TStatusForm.FormCreate(Sender: TObject);
begin


end;


procedure TStatusForm.SetFont;
//----------------------------------------------------
// Sets font style of the FileViewer control.
//----------------------------------------------------
begin
  if BoldFonts then FileViewer.Font.Style := [fsBold]
  else FileViewer.Font.Style := [];
end;


procedure TStatusForm.RefreshStatusReport;
//----------------------------------------------------
// Reloads the FileViewer control with the contents
// of the Status Report File generated from a
// network analysis.
//----------------------------------------------------
begin
// Set the FileViewer's font & clear its contents
  FileViewer.Clear;
  SetFont;

// Make sure that the report file exists
  if FileExists(TempReportFile) then with FileViewer do
  try

  // Load the contents of the file into the FileViewer
    Items.LoadFromFile(TempReportFile);
  except
    On E: Exception do
      Uutils.MsgDlg(Format(MSG_1, [IntToStr(Items.Count)]), mtWarning, [mbOK],
      MainForm);
  end;
  if FileViewer.Items.Count >= 1 then FileViewer.ItemIndex := 0;
end;


procedure TStatusForm.Print(Destination: TDestination);
//---------------------------------------------------------------
// Prints Status Report to Destination (printer or preview form).
//---------------------------------------------------------------
var
  i: Integer;
begin
  with MainForm.thePrinter do
  begin
    BeginJob;
    SetDestination(Destination);
    with FileViewer.Font do
      SetFontInformation(Name, Size, Style);
    with FileViewer do
      for i := 0 to Items.Count - 1 do PrintLine(Items[i]);
    EndJob;
  end;
end;


procedure TStatusForm.SelectText(const S: String);
//--------------------------------------------------
// Locates text string S in the FileViewer.
// (Called from MainForm to locate a Warning message
// after an analysis ends with warning messages).
//--------------------------------------------------
{$IFDEF WINDOWS}
var
  Buf: array[0..255] of Char;
begin
  StrPCopy(Buf, S);
  with FileViewer do
  begin
    ItemIndex := Perform(LB_SELECTSTRING,0,LongInt(@Buf));
  end;
{$ELSE}
begin
  FileViewer.ItemIndex := FileViewer.Items.IndexOf(S);
{$ENDIF}
end;


procedure TStatusForm.CopyTo;
//----------------------------------------------------
// Copies contents of the FileViewer to either a file
// or to the Clipboard.
//----------------------------------------------------
begin
// Create the CopyTo dialog form
  with TCopyToForm.Create(self) do
  try

  // Disable format selection (since it has to be Text)
    FormatGroup.ItemIndex := 2;
    FormatGroup.Enabled := False;

  // Show the form modally
    if ShowModal = mrOK then with FileViewer do
    begin

    // If user supplies a file name then copy contents of FileViewer to it
      if Length(DestFileName) > 0 then
        CopyFile(PChar(TempReportFile), PChar(DestFileName), FALSE)

    // Otherwise copy the contents into the Clipboard
      else Clipboard.SetTextBuf(PChar(Items.Text));
    end;

// Free the CopyTo dialog form
  finally
    Free;
  end;
end;

end.
