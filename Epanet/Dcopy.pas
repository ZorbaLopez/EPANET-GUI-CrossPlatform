unit Dcopy;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{-------------------------------------------------------------------}
{                    Unit:    Dcopy.pas                             }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                    Ported to Lazarus by: Zorba Lopez Rivera       }
{                    Date:    15/05/23                              }
{                                                                   }
{   Form unit with a dialog that gets choice of format and          }
{   destination that a view should be copied to.                    }
{-------------------------------------------------------------------}

interface

uses
{$IFDEF WINDOWS}
  Windows, Messages,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Uglobals, ResourceStrings;

const
  {$IFDEF WINDOWS}
  FilterTxt: array[0..2] of string =
   (TXT_FILTER_BMP, TXT_FILTER_EMF, TXT_FILTER_TXT);
  ExtensionTxt: array[0..2] of PChar =
    ('.bmp','.emf','.txt');
  {$ELSE}
  FilterTxt: array[0..2] of string =
   (TXT_FILTER_BMP, TXT_FILTER_SVG, TXT_FILTER_TXT);
  ExtensionTxt: array[0..2] of PChar =
    ('.bmp','.svg','.txt');
  {$ENDIF}

type
  TCopyToForm = class(TForm)
    DestGroup: TRadioGroup;
    FormatGroup: TRadioGroup;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DestFileName: String;
  end;


implementation

{$R *.lfm}

uses Fmain;

procedure TCopyToForm.FormCreate(Sender: TObject);
//---------------------------------------------
// OnCreate handler for form.
//---------------------------------------------
var
  s: String;
  n: Integer;
begin
  //Lazarus - Set Radiogroup caption
  DestGroup.Items.Clear;
  FormatGroup.Items.Clear;
  DestGroup.Items.AddStrings(CopyDestination);
  FormatGroup.Items.AddStrings(CopyFormat);
  DestGroup.ItemIndex:= 0;
  FormatGroup.ItemIndex:= 0;

  Uglobals.SetFont(self);
  s := '';
  with MainForm do
  begin
    s := FormActive;
    n := Pos(' -',s);
    if n > 0 then s := Copy(s, 1, n-1);
  end;
  Caption := TXT_COPY + s;
end;


procedure TCopyToForm.BtnOKClick(Sender: TObject);
//--------------------------------------------
// OnClick handler for OK button.
// Retrieves name of file to copy to.
//--------------------------------------------
var
  Ftype: Integer;
begin
// Use the MainForm's SaveDialog control to obtain the file name
  DestFileName := '';
  if DestGroup.ItemIndex = 1 then
  with MainForm.SaveDialog do
  begin
    Title := TXT_SAVE_AS;
    Ftype := FormatGroup.ItemIndex;
    Filter := FilterTxt[Ftype];
    DefaultExt := Copy(ExtensionTxt[Ftype],2,3);
    Filename := '*' + ExtensionTxt[Ftype];
    if Execute then
    begin
      DestFileName := Filename;
      ModalResult := mrOK;
    end
    else ModalResult := mrCancel;
    DefaultExt := '';
  end
  else ModalResult := mrOK;
  Hide;
end;


procedure TCopyToForm.BtnCancelClick(Sender: TObject);
//-------------------------------------------------
// OnClick handler for the Cancel button.
//-------------------------------------------------
begin
  ModalResult := mrCancel;
  Hide;
end;


procedure TCopyToForm.BtnHelpClick(Sender: TObject);
//-----------------------------------------------
// OnClick handler for the Help button.
//-----------------------------------------------
begin
  MainForm.LaunchHelp(301);
end;

end.
