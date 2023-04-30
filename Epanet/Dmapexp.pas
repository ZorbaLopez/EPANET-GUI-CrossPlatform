unit Dmapexp;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{-------------------------------------------------------------------}
{                    Unit:    Dmapexp.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                    Ported to Lazarus by: Zorba Lopez Rivera       }
{                    Date:    15/05/23                              }
{                                                                   }
{   Form unit with a dialog box that exports the Network            }
{   Map to a file in either text, metafile, or DXF format.          }
{-------------------------------------------------------------------}

interface

uses
{$IFDEF WINDOWS}
  Windows, Messages,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Uglobals, StdCtrls, ExtCtrls, ResourceStrings;

const
  MAP_FILE = 0;
  EMF_FILE = 1;
  DXF_FILE = 2;
  {$IFDEF WINDOWS}
  FilterTxt: array[MAP_FILE..DXF_FILE] of string =
    (TXT_MAP_FILTER, TXT_FILTER_EMF, TXT_FILTER_DXF);
  ExtensionTxt: array[MAP_FILE..DXF_FILE] of PChar =
    ('map','emf','dxf');
  {$ELSE}
  FilterTxt: array[MAP_FILE..DXF_FILE] of string =
    (TXT_MAP_FILTER, TXT_FILTER_SVG, TXT_FILTER_DXF);
  ExtensionTxt: array[MAP_FILE..DXF_FILE] of PChar =
    ('map','svg','dxf');
  {$ENDIF}

type
  TMapExportForm = class(TForm)
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioGroup1: TRadioGroup;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    procedure ExportMapFile(const Fname: String);
  public
    { Public declarations }
  end;


implementation

{$R *.lfm}

uses Fmap, Udxf, Uexport, Fmain, Uutils;

procedure TMapExportForm.FormCreate(Sender: TObject);
//-------------------------------------------------
// OnCreate handler for form.
//-------------------------------------------------
begin
  //Lazarus - Set Radiogroup caption
  {$IFNDEF WINDOWS}
  RadioButton2.Caption := TXT_SVG_DESCR;
  {$ENDIF}
  RadioGroup1.Items.Clear;
  RadioGroup1.Items.AddStrings(DXFNodes);

  Uglobals.SetFont(self);
  RadioButton1.Checked := True;
  RadioGroup1.ItemIndex := 0;
  RadioGroup1.Visible := False;
end;

procedure TMapExportForm.RadioButton1Click(Sender: TObject);
//------------------------------------------------
// OnClick handler for DXF RadioButton control.
// Displays junction drawing choices when DXF file
// format is selected.
//------------------------------------------------
begin
  if RadioButton3.Checked then
    RadioGroup1.Visible := True
  else
    RadioGroup1.Visible := False;
end;

procedure TMapExportForm.Button1Click(Sender: TObject);
//----------------------------------------------------
// OnClick handler for OK button.
//----------------------------------------------------
var
  FileType: Integer;
  JuncStyle: Integer;

begin
// Determine file format to use
  FileType := MAP_FILE;
  if RadioButton2.Checked then FileType := EMF_FILE;
  if RadioButton3.Checked then FileType := DXF_FILE;

// Hide the form and display the Save File dialog box
  Hide;
  with SaveDialog1 do
  begin
    Title := TXT_SAVE_MAP_TITLE;
    Filter := FilterTxt[FileType];
    Filename := '*.' + ExtensionTxt[FileType];
    DefaultExt := ExtensionTxt[FileType];
    if Execute then

  // Save map to file in selected format.
    begin
      case FileType of
        MAP_FILE: ExportMapFile(Filename);
        EMF_FILE: MainForm.MapForm.CopyToMetaFile(Filename);
        DXF_FILE: begin
                    JuncStyle := RadioGroup1.ItemIndex;
                    Udxf.DXFexport(Filename,JuncStyle);
                  end;
      end;
    end;
  end;
end;

procedure TMapExportForm.ExportMapFile(const Fname: String);
//----------------------------------------------------------
// Saves network map to file in EPANET text format.
//----------------------------------------------------------
var
  F: TextFile;
begin
  AssignFile(F,Fname);
  {$I-}
  Rewrite(F);
  {$I+}
  if IOResult = 0 then
  try
    Uexport.ExportMap(F);
  finally
  end;
  CloseFile(F);
end;

procedure TMapExportForm.Button3Click(Sender: TObject);
begin
  MainForm.LaunchHelp(280);
end;

end.
