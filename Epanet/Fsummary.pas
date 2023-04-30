unit Fsummary;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{-------------------------------------------------------------------}
{                    Unit:    Fsummary.pas                          }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                    Ported to Lazarus by: Zorba Lopez Rivera       }
{                    Date:    15/05/23                              }
{                                                                   }
{   Form unit that displays a Project Summary that allows the       }
{   user to edit a project's title and descriptive notes.           }
{-------------------------------------------------------------------}

interface

uses
{$IFDEF WINDOWS}
  Windows, HtmlHelp, Messages,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ExtCtrls, System.UITypes, Uglobals;

type
  TSummaryForm = class(TForm)
    GroupBox1: TGroupBox;
    EditTitle: TEdit;
    GroupBox2: TGroupBox;
    MemoNotes: TMemo;
    GroupBox3: TGroupBox;
    BtnOK: TButton;
    BtnCancel: TButton;
    MemoStats: TMemo;
    CheckHeader: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure EditTitleChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    LocalChange: Boolean;
  public
    { Public declarations }
  end;

implementation

{$R *.lfm}

uses Fmain, ResourceStrings;

const
  S: array[0..8] of String =
      (TXT_NO_JUNCTIONS,
       TXT_NO_RESERVOIRS,
       TXT_NO_TANKS,
       TXT_NO_PIPES,
       TXT_NO_PUMPS,
       TXT_NO_VALVES,
       TXT_FLOWUNITS,
       TXT_HEADLOSSFORMULA,
       TXT_QUALITYPARAMETER);


procedure TSummaryForm.FormCreate(Sender: TObject);
//-------------------------------------------------
// OnCreate handler for form.
//-------------------------------------------------
begin
//Set font style
  Uglobals.SetFont(self);
  MemoStats.Font.Style := Font.Style;
  {$IFDEF DARWIN}
  if MainForm.PropEditForm.Editor.isMacDarkMode then MemoStats.Font.Color := clAqua;
  {$ENDIF}

// Load project title & notes into form's controls
  EditTitle.Text := Network.Options.Title;
  CheckHeader.Checked := TitleAsHeader;
  MemoNotes.Lines.Assign(Network.Options.Notes);

// Display project statistics
  with MemoStats.Lines do
  begin
    Add(S[0] + IntToStr(Network.Lists[JUNCS].Count));
    Add(S[1] + IntToStr(Network.Lists[RESERVS].Count));
    Add(S[2] + IntToStr(Network.Lists[TANKS].Count));
    Add(S[3] + IntToStr(Network.Lists[PIPES].Count));
    Add(S[4] + IntToStr(Network.Lists[PUMPS].Count));
    Add(S[5] + IntToStr(Network.Lists[VALVES].Count));
    Add(S[6] + Network.Options.Data[FLOW_UNITS_INDEX]);
    Add(S[7] + Network.Options.Data[HLOSS_FORM_INDEX]);
    Add(S[8] + Network.Options.Data[QUAL_PARAM_INDEX]);
  end;
  MemoStats.SelStart := 0;
  LocalChange := False;
  BtnCancel.Visible := False;
end;

procedure TSummaryForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then MainForm.LaunchHelp(143);
end;

procedure TSummaryForm.BtnOKClick(Sender: TObject);
//-------------------------------------------------
// OnClick handler for OK button.
//-------------------------------------------------
begin
  if LocalChange = True then
  begin
    Network.Options.Title := EditTitle.Text;
    Network.Options.Notes.Assign(MemoNotes.Lines);
    HasChanged := True;
    TitleAsHeader := CheckHeader.Checked;
    if CheckHeader.Checked then with MainForm do
    begin
      PageSetupDialog.Header.Text := EditTitle.Text;
      PageSetup;
    end;
  end;
end;

procedure TSummaryForm.EditTitleChange(Sender: TObject);
//------------------------------------------------------
// OnChange handler for EditTitle & MemoNotes controls.
//------------------------------------------------------
begin
  LocalChange := True;
  BtnCancel.Visible := True;
end;

end.
