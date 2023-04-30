unit Fproped;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{-------------------------------------------------------------------}
{                    Unit:    Fproped.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                    Ported to Lazarus by: Zorba Lopez Rivera       }
{                    Date:    15/05/23                              }
{                                                                   }
{   Form unit that is a container for a TPropEdit component.        }
{   This component serves as the Property  Editor for data          }
{   objects and is styled after the Delphi object inspector.        }
{   The form is created on startup and remains active until the     }
{   application closes.                                             }
{-------------------------------------------------------------------}

interface

uses
{$IFDEF WINDOWS}
  Windows, Messages,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,  ExtCtrls, ComCtrls,
  StdCtrls, PropEdit, Xprinter, Uglobals, Uutils;

type

  { TPropEditForm1 }

  TPropEditForm1 = class(TFrame)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormEnter(Sender: TObject);
  private
    { Private declarations }
    procedure CallHelp;
    procedure Validate(Sender: TObject; Index: Integer; var S: String;
      var Errmsg: String; var IsValid: Boolean);
    procedure ButtonClick(Sender: TObject; Index: Integer);
  public
    { Public declarations }
    Editor: TPropEdit;
  end;


implementation

{$R *.lfm}

uses Fmain, Fbrowser, Uinput, ResourceStrings;


procedure TPropEditForm1.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate handler for the form. Creates a TPropEdit component to edit an
// object's properties. The event handler function for properties with an
// ellipsis button is ButtonClick. The event handler for validating
// editor input is Validate. The event handler for selecting a new
// row (i.e., property) is ShowPropertyHint.
//-----------------------------------------------------------------------------
begin
  //Lazarus - Assing events;
  OnKeyDown := FormKeyDown;
  DoubleBuffered := True;

  Editor := TPropEdit.Create(Self);
  with Editor do
  begin
    Parent := Self;
    ParentFont := True;
    ParentColor := True;
    Align := alClient;
    BorderStyle := bsNone;
    ColHeading1 := TXT_PROPERTY;
    ColHeading2 := TXT_VALUE;
    HeaderSplit := 50;
    ReadOnlyColor := clInfoBk;
    {$IFDEF DARWIN}
    if isMacDarkMode then ValueColor := clAqua else ValueColor := clInfoText;
    {$ELSE}
    ValueColor := clNavy;
    {$ENDIF}
    OnButtonClick := ButtonClick;
    OnValidate := Validate;
  end;
end;

procedure TPropEditForm1.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
// OnClose event handler for form.
//-----------------------------------------------------------------------------
begin
  Action := caHide;
end;

procedure TPropEditForm1.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
// OnDestroy event handler for form.
//-----------------------------------------------------------------------------
begin
  Editor.Free;
end;

procedure TPropEditForm1.FormDeactivate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnDeactivate event handler for form. Calls the Editor's IsValid function
// to validate the value of the current property when the form looses focus.
//-----------------------------------------------------------------------------
begin
  Editor.IsValid;
end;

procedure TPropEditForm1.FormEnter(Sender: TObject);
//-----------------------------------------------------------------------------
// OnEnter event handler for form. Set the Frame as Active Form.
//-----------------------------------------------------------------------------
begin
  MainForm.PropEditHeader.Color := clActiveCaption;
  MainForm.PropEditHeader.Font.Color := clCaptionText;
  MainForm.FormActive := TXT_MAP;
  MainForm.FormActivated(self);
end;


procedure TPropEditForm1.Validate(Sender: TObject; Index: Integer;
  var S: String; var Errmsg: String; var IsValid: Boolean);
//-----------------------------------------------------------------------------
// OnValidate event handler for the TPropEdit editor component.
// Passes the string value S of property index Index to the ValidateEditor
// function in the Uvalidate.pas unit.
//-----------------------------------------------------------------------------
begin
  IsValid := Uinput.ValidateInput(Index,S,Errmsg);
end;

procedure TPropEditForm1.ButtonClick(Sender: TObject; Index: Integer);
//-----------------------------------------------------------------------------
// OnButtonClick event handler. Activated when the user clicks the ellipsis
// button for a specific property.
//-----------------------------------------------------------------------------
begin
// User wants to edit a junction's list of demand categories
  if (CurrentList = JUNCS) and (Index = JUNC_DMNDCAT_INDEX+3) then
    Uinput.EditDemands(CurrentItem[CurrentList])

// User wants to edit a node's WQ source input
  else if (CurrentList in [JUNCS..TANKS]) then
    Uinput.EditSource(CurrentList, CurrentItem[CurrentList])

// User wants to edit a label's font
  else if (CurrentList = LABELS) then
    Uinput.EditLabelFont(CurrentItem[CurrentList])
end;

procedure TPropEditForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
// OnKeyDown event handler for the form. Processes certain keystrokes
// to change which object is being edited.
//-----------------------------------------------------------------------------
begin
  case Key of

    // Shift-PgDown loads first object into editor.
    // PgDown loads prior object into editor.
    vk_PRIOR:
      begin
        if (CurrentItem[CurrentList] > 0) then
        begin
          if (ssCtrl in Shift) then CurrentItem[CurrentList] := 0
          else Dec(CurrentItem[CurrentList]);
          MainForm.BrowserForm.UpdateBrowser(CurrentList, CurrentItem[CurrentList]);
        end;
        Key := 0;
      end;

    // Shift-PgUp loads last object into editor.
    // PgUp loads next object into editor.
    vk_NEXT:
      begin
        if (CurrentItem[CurrentList] < Network.Lists[CurrentList].Count-1) then
        begin
          if (ssCtrl in Shift) then
            CurrentItem[CurrentList] := Network.Lists[CurrentList].Count - 1
          else Inc(CurrentItem[CurrentList]);
          MainForm.BrowserForm.UpdateBrowser(CurrentList, CurrentItem[CurrentList]);
        end;
        Key := 0;
      end;

    // Shift-Tab shifts focus to the MainForm
    vk_TAB:
      begin
        if (ssCtrl in Shift) then MainForm.SetFocus;
        Key := 0;
      end;

    // F1 brings up context sensitive Help
    vk_F1: CallHelp;
  end;
end;


procedure TPropEditForm1.CallHelp;
//-----------------------------------------------------------------------------
// Determines which Help topic to display when F1 pressed.
//-----------------------------------------------------------------------------
var
  HC: Integer;
begin
  case CurrentList of
    JUNCS:   HC := 154;
    RESERVS: HC := 155;
    TANKS:   HC := 156;
    PIPES:   HC := 160;
    PUMPS:   HC := 161;
    VALVES:  HC := 162;
    LABELS:  HC := 284;
    OPTS:    case CurrentItem[OPTS] of
             0: HC := 144;  //Hydraulics
             1: HC := 145;  //Quality
             2: HC := 146;  //Reactions
             3: HC := 148;  //Times
             4: HC := 149;  //Energy
             else HC := 0;
             end;
    else     HC := 0;
  end;
  if HC > 0 then MainForm.LaunchHelp(HC);
end;

end.
