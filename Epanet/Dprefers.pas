unit Dprefers;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{-------------------------------------------------------------------}
{                    Unit:    Dprefers.pas                          }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                    Ported to Lazarus by: Zorba Lopez Rivera       }
{                    Date:    15/05/23                              }
{                                                                   }
{   Form unit with a dialog box for setting program preferences.    }
{-------------------------------------------------------------------}

interface

uses
{$IFDEF WINDOWS}
  Windows, Messages,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Spin, StdCtrls, ComCtrls, FileCtrl, ExtCtrls, System.UITypes,
  Uglobals, Uutils, ResourceStrings;


type

  { TPreferencesForm }

  TPreferencesForm = class(TForm)
    LangVarBox: TComboBox;
    Label5: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    CheckBlinking: TCheckBox;
    CheckFlyOvers: TCheckBox;
    CheckAutoBackup: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    NodeVarBox: TComboBox;
    NodeVarSpin: TSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    LinkVarBox: TComboBox;
    LinkVarSpin: TSpinEdit;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    Panel1: TPanel;
    Label6: TLabel;
    CheckConfirmDelete: TCheckBox;
    CheckClearFileList: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure NodeVarSpinChange(Sender: TObject);
    procedure LinkVarSpinChange(Sender: TObject);
    procedure NodeVarBoxChange(Sender: TObject);
    procedure LinkVarBoxChange(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
  private
    { Private declarations }
    NodeDigits: array[DEMAND..NODEQUAL] of Integer;
    LinkDigits: array[FLOW..LINKQUAL] of Integer;
    Lang : Integer;
    function SetPreferences: Boolean;
  public
    { Public declarations }
  end;


implementation

{$R *.lfm}

uses Fmain, Fbrowser;

procedure TPreferencesForm.FormCreate(Sender: TObject);
//----------------------------------------------------
// OnCreate handler for form.
//----------------------------------------------------
var
  i: Integer;
begin
// Set font size & style
  Uglobals.SetFont(self);

// Initialize general preferences
  CheckBlinking.Checked := Blinking;
  CheckFlyOvers.Checked := FlyOvers;
  CheckAutoBackup.Checked := AutoBackup;
  CheckConfirmDelete.Checked := ConfirmDelete;

// Assign items to node & link variable combo boxes
  for i := DEMAND to NODEQUAL do
  begin
    NodeVarBox.Items.Add(NodeVariable[i].Name);
    NodeDigits[i] := NodeUnits[i].Digits;
  end;
  for i := FLOW to LINKQUAL do
  begin
    LinkVarBox.Items.Add(LinkVariable[i].Name);
    LinkDigits[i] := LinkUnits[i].Digits;
  end;
  NodeVarBox.ItemIndex := 0;
  NodeVarSpin.Value := NodeDigits[DEMAND];
  LinkVarBox.ItemIndex := 0;
  LinkVarSpin.Value := LinkDigits[FLOW];
  Label6.Caption := MSG_SELECT_NUMBER_OF + #13 + MSG_WHEN_DISPLAYING;
  PageControl1.ActivePage := TabSheet1;

  // Assign items to language variable combo box
  LangVarBox.Items.SetStrings(Languages);
  LangVarBox.ItemIndex := Language;
end;

procedure TPreferencesForm.NodeVarSpinChange(Sender: TObject);
//-----------------------------------------------------------
// OnChange handler for SpinEdit control that
// sets decimal places for a node variable.
//-----------------------------------------------------------
begin
  NodeDigits[NodeVarBox.ItemIndex+DEMAND] := NodeVarSpin.Value;
end;

procedure TPreferencesForm.LinkVarSpinChange(Sender: TObject);
//-----------------------------------------------------------
// OnChange handler for SpinEdit control that
// sets decimal places for a link variable.
//-----------------------------------------------------------
begin
  LinkDigits[LinkVarBox.ItemIndex+FLOW] := LinkVarSpin.Value;
end;

procedure TPreferencesForm.NodeVarBoxChange(Sender: TObject);
//-----------------------------------------------------------
// OnChange handler for ComboBox control that
// selects a node variable.
//-----------------------------------------------------------
begin
  NodeVarSpin.Value := NodeDigits[NodeVarBox.ItemIndex+DEMAND];
end;

procedure TPreferencesForm.LinkVarBoxChange(Sender: TObject);
//-----------------------------------------------------------
// OnChange handler for ComboBox control that
// selects a link variable.
//-----------------------------------------------------------
begin
  LinkVarSpin.Value := LinkDigits[LinkVarBox.ItemIndex+FLOW];
end;

procedure TPreferencesForm.BtnOKClick(Sender: TObject);
//------------------------------------------------------
// OnClick handler for OK button.
//------------------------------------------------------
begin
  if SetPreferences then ModalResult := mrOK;
end;

procedure TPreferencesForm.BtnCancelClick(Sender: TObject);
//------------------------------------------------------
// OnClick handler for Cancel button.
//------------------------------------------------------
begin
  ModalResult := mrCancel;
end;

function TPreferencesForm.SetPreferences: Boolean;
//------------------------------------------------------------
// Transfers contents of form to program preference variables.
//------------------------------------------------------------
var
  j: Integer;
begin

// Save the other preferences to their respective global variables.
  Blinking := CheckBlinking.Checked;
  FlyOvers := CheckFlyOvers.Checked;
  AutoBackup := CheckAutoBackup.Checked;
  ConfirmDelete := CheckConfirmDelete.Checked;
  for j := DEMAND to NODEQUAL do NodeUnits[j].Digits := NodeDigits[j];
  for j := FLOW to LINKQUAL do LinkUnits[j].Digits := LinkDigits[j];

// Clear Most Recently Used file list
  if CheckClearFileList.Checked then
    for j := 0 to MainForm.MRUList.Count-1 do MainForm.MRUList[j] := '';

  if Language <> LangVarBox.ItemIndex then
    begin
      Language := LangVarBox.ItemIndex;
      Uutils.MsgDlg(MSG_LANG_RESTART, mtInformation, [mbOK], self);
    end;
  Result := True;
end;

procedure TPreferencesForm.BtnHelpClick(Sender: TObject);
var
  HC: Integer;
begin
   with PageControl1 do
     if ActivePage = TabSheet1 then HC := 137 else HC := 142;
   MainForm.LaunchHelp(HC);
end;

end.
