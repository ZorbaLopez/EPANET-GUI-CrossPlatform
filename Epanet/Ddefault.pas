unit Ddefault;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{-------------------------------------------------------------------}
{                    Unit:    Ddefault.pas                          }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                    Ported to Lazarus by: Zorba Lopez Rivera       }
{                    Date:    15/05/23                              }
{                                                                   }
{   Form unit with a dialog box that selects default settings       }
{   for the current project.                                        }
{-------------------------------------------------------------------}

interface

uses
{$IFDEF WINDOWS}
  Windows, Messages,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  PropEdit, Spin,  ComCtrls, Uglobals, Uutils, ResourceStrings;

const
  MAXPREFIX = 6; //Max. chars. in an ID prefix

// A TPropRecord record determines how properties are displayed
// and edited in the PropEdit control (see PropEdit.pas unit).

  PrefixProps: array[0..8] of TPropRecord =
   ((Name: TXT_JUNCTIONS;    Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX;  List:''),
    (Name: TXT_RESERVOIRS;   Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX;  List:''),
    (Name: TXT_TANKS;        Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX;  List:''),
    (Name: TXT_PIPES;        Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX;  List:''),
    (Name: TXT_PUMPS;        Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX;  List:''),
    (Name: TXT_VALVES;       Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX;  List:''),
    (Name: TXT_PATTERNS;     Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX;  List:''),
    (Name: TXT_CURVES;       Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX;  List:''),
    (Name: TXT_ID_INCREMENT; Style:esEdit;    Mask:emPosNumber; Length:0;  List:''));

  NetworkProps: array[0..6] of TPropRecord =
   ((Name: TXT_NODE_ELEVATION; Style:esEdit;      Mask:emNumber; Length:0;  List:''),
    (Name: TXT_TANK_DIAMETER;  Style:esEdit;      Mask:emPosNumber; Length:0;  List:''),
    (Name: TXT_TANK_HEIGHT;    Style:esEdit;      Mask:emPosNumber; Length:0;  List:''),
    (Name: TXT_PIPE_LENGTH;    Style:esEdit;      Mask:emPosNumber; Length:0;  List:''),
    (Name: TXT_AUTLENGTH;      Style:esComboList; Mask:emNone;  Length:0;
     List:'Off'#13'On'),
    (Name: TXT_PIPE_DIAMETER;  Style:esEdit;      Mask:emPosNumber; Length:0;  List:''),
    (Name: TXT_PIPE_ROUGHNESS; Style:esEdit;      Mask:emPosNumber; Length:0;  List:''));

  AutoList: array [0..1] of String = ('Off', 'On');

type

  { TDefaultsForm }

  TDefaultsForm = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    CheckDefault: TCheckBox;
    TabControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    PropList  : array[0..2] of TStringlist;
    procedure GetDefaults(const I: Integer);
    procedure SetDefaults;
    procedure ValidateOption(Sender: TObject; Index: Integer; var S: String;
      var Errmsg: String; var IsValid: Boolean);
  public
    { Public declarations }
    PropEdit1: TPropEdit;
    Modified: Boolean;
  end;


implementation

{$R *.lfm}

uses Uinput, Uinifile, Fmain;

procedure TDefaultsForm.FormCreate(Sender: TObject);
//-------------------------------------------------
// Form's OnCreate event handler.
//-------------------------------------------------
var
  i: Integer;

begin
// Set font size & style
  Uglobals.SetFont(self);

// Create Property Editor
  PropEdit1 := TPropEdit.Create(self);
  with PropEdit1 do
  begin
    Parent := TabSheet1;
    Align := alNone;
    BorderStyle := bsNone;
    Left := 1;
    Top := 1;
    ColHeading1 := TXT_OBJECT;
    ColHeading2 := TXT_ID_PREFIX;
    {$IFDEF DARWIN}
    if isMacDarkMode then ValueColor := clAqua else ValueColor := clInfoText;
    {$ELSE}
    ValueColor := clNavy;
    {$ENDIF}
    OnValidate := ValidateOption;
  end;

// Load current default values for ID prefixes, Node/Link parameters,
// and Hydraulic Options into a work array of stringlists.
  for i := 0 to 2 do
  begin
    PropList[i] := TStringList.Create;
    GetDefaults(i);
  end;

// Align Property Editor & Panel with client
  PropEdit1.Align := alClient;
  Modified := False;
end;

procedure TDefaultsForm.FormShow(Sender: TObject);
//-----------------------------------------------
// Form's OnShow event handler.
//-----------------------------------------------
begin
  TabControl1.TabIndex := 0;
  TabControl1Change(Sender);
end;

procedure TDefaultsForm.FormDestroy(Sender: TObject);
//--------------------------------------------------
// Form's OnDestroy event handler.
//--------------------------------------------------
var
  i: Integer;
begin
  for i := 0 to 2 do PropList[i].Free;
  PropEdit1.Free;
end;

procedure TDefaultsForm.GetDefaults(const I: Integer);
//---------------------------------------------------
// Loads current set of default values into the
// work array of stringlists (PropList).
//---------------------------------------------------
var
  j: Integer;
begin

  case I of
  0: begin
       for j := JUNCS to VALVES do PropList[i].Add(IDPrefix[j]);
       PropList[I].Add(IDPrefix[PATTERNS]);
       PropList[I].Add(IDPrefix[CURVES]);
       PropList[I].Add(IntToStr(IDIncrement));
     end;
  1: begin
       PropList[I].Add(DefProp[JUNCS].Data[JUNC_ELEV_INDEX]);
       PropList[I].Add(DefProp[TANKS].Data[TANK_DIAM_INDEX]);
       PropList[I].Add(DefProp[TANKS].Data[TANK_MAXLVL_INDEX]);
       PropList[I].Add(DefProp[PIPES].Data[PIPE_LEN_INDEX]);
       PropList[I].Add(AutoList[Ord(AutoLength)]);
       PropList[I].Add(DefProp[PIPES].Data[PIPE_DIAM_INDEX]);
       PropList[I].Add(DefProp[PIPES].Data[PIPE_ROUGH_INDEX]);
     end;
  2: begin
       for j := FLOW_UNITS_INDEX to STATUS_RPT_INDEX do
         PropList[I].Add(Network.Options.Data[j]);
     end;
  end;
end;

procedure TDefaultsForm.SetDefaults;
//-------------------------------------------------------
// Transfers values from work array of stringlists to the
// global variables holding the project defaults.
//--------------------------------------------------------
var
  j,code,v: Integer;
begin

  for j := 0 to 5 do IDPrefix[j] := PropList[0].Strings[j];
  IDPrefix[PATTERNS] := PropList[0].Strings[6];
  IDPrefix[CURVES] := PropList[0].Strings[7];
  Val(PropList[0].Strings[8],v,code);
  if code = 0 then
  begin
    IDIncrement := v;
    for j := JUNCS to CURVES do
    begin
      if (NextID[j] <= v)
      or (Network.Lists[j].Count = 0) then NextID[j] := v
      else NextID[j] := NextID[j] + v;
    end;
  end;

  DefProp[JUNCS].Data[JUNC_ELEV_INDEX] := PropList[1].Strings[0];
  DefProp[RESERVS].Data[RES_HEAD_INDEX] := DefProp[JUNCS].Data[JUNC_ELEV_INDEX];
  Defprop[TANKS].Data[TANK_ELEV_INDEX] := DefProp[JUNCS].Data[JUNC_ELEV_INDEX];
  DefProp[TANKS].Data[TANK_DIAM_INDEX] := PropList[1].Strings[1];
  DefProp[TANKS].Data[TANK_MAXLVL_INDEX] := PropList[1].Strings[2];

  DefProp[PIPES].Data[PIPE_LEN_INDEX] := PropList[1].Strings[3];
  AutoLength := (CompareText(PropList[1].Strings[4],AutoList[1]) = 0);
  DefProp[PIPES].Data[PIPE_DIAM_INDEX] := PropList[1].Strings[5];
  DefProp[PIPES].Data[PIPE_ROUGH_INDEX] := PropList[1].Strings[6];
  DefProp[VALVES].Data[VALVE_DIAM_INDEX] := DefProp[PIPES].Data[PIPE_DIAM_INDEX];
  for j := FLOW_UNITS_INDEX to STATUS_RPT_INDEX do
    Network.Options.Data[j] := PropList[2].Strings[j-FLOW_UNITS_INDEX];

  Uinput.UpdateAllUnits;

end;

procedure TDefaultsForm.BtnOKClick(Sender: TObject);
//-------------------------------------------------
// OnClick event handler for the OK button.
//-------------------------------------------------
begin
// Validate last entry in the PropEdit control
  PropEdit1.IsValid;

// Retrieve updated project defaults
  SetDefaults;

// Set the global Modified flag if edits were made
  if PropEdit1.Modified then Modified := True;

// If Default checkbox check then save defaults to file
  if CheckDefault.Checked then Uinifile.SaveDefaults;
  ModalResult := mrOK;
end;

procedure TDefaultsForm.BtnCancelClick(Sender: TObject);
//-----------------------------------------------------
// OnClick event handler for the Cancel button.
//-----------------------------------------------------
begin
  ModalResult := mrCancel;
end;

procedure TDefaultsForm.TabControl1Change(Sender: TObject);
//------------------------------------------------------------
// OnClick event handler for the TabControl1.
// Switches the set of default properties edited in PropEdit1.
//------------------------------------------------------------
begin
  PropEdit1.IsValid;
  case TabControl1.TabIndex of
  0:  begin
        PropEdit1.Parent := TabSheet1;
        PropEdit1.Align := alClient;
        PropEdit1.ColHeading1 := TXT_OBJECT;;
        PropEdit1.ColHeading2 := TXT_ID_PREFIX;
        PropEdit1.SetProps(PrefixProps,PropList[0]);
        PropEdit1.Edit;
      end;
  1:  begin
        PropEdit1.Parent := TabSheet2;
        PropEdit1.Align := alClient;
        PropEdit1.ColHeading1 := TXT_PROPERTY;
        PropEdit1.ColHeading2 := TXT_DEF_VALUE;
        PropEdit1.SetProps(NetworkProps,PropList[1]);
        PropEdit1.Edit;
      end;
  2:  begin
        PropEdit1.Parent := TabSheet3;
        PropEdit1.Align := alClient;
        PropEdit1.ColHeading1 := TXT_OPTION;
        PropEdit1.ColHeading2 := TXT_DEF_VALUE;
        PropEdit1.SetProps(HydraulicProps,PropList[2]);
        PropEdit1.Edit;
      end;
  end;
end;

procedure TDefaultsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-------------------------------------------------------------------
// OnKeyDown event handler for the Form (KeyPreview was set to True).
// Allows use of Ctrl-Tab keystroke to change tabs.
//-------------------------------------------------------------------
begin
  if (ssCtrl in Shift) and (Key = VK_TAB) then with TabControl1 do
  begin
    if TabIndex < PageCount - 1 then TabIndex := TabIndex + 1
    else TabIndex := 0;
    TabControl1Change(Sender);
  end;
end;

procedure TDefaultsForm.ValidateOption(Sender: TObject; Index: Integer;
  var S: String; var Errmsg: String; var IsValid: Boolean);
//-------------------------------------------------------------
// OnValidate event handler for the TPropEdit editor component.
//------------------------------------------------------------
begin
// If hydraulics option is left blank, then use factory default
  if TabControl1.TabIndex = 2 then
  begin
    if Length(S) = 0 then S := DefOptions[Index];
  end;
  IsValid := True;
end;

procedure TDefaultsForm.BtnHelpClick(Sender: TObject);
var
  HC: Integer;
begin
  Case TabControl1.TabIndex of
    0:  HC := 138;
    1:  HC := 140;
    2:  HC := 139;
    else HC := 0;
  end;
  if HC > 0 then MainForm.LaunchHelp(HC);
end;

end.
