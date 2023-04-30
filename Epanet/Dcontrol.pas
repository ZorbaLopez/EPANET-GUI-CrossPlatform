unit Dcontrol;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{-------------------------------------------------------------------}
{                    Unit:    Dcontrol.pas                          }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                    Ported to Lazarus by: Zorba Lopez Rivera       }
{                    Date:    15/05/23                              }
{                                                                   }
{   Form unit with a memo control that edits either Simple or       }
{   Rule-Based Controls.                                            }
{-------------------------------------------------------------------}

interface

uses
{$IFDEF WINDOWS}
  Windows, Messages,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, System.UITypes,
  Uglobals, Fmain;

type
  TControlsForm = class(TForm)
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    Panel2: TPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    procedure BtnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.lfm}

procedure TControlsForm.BtnOKClick(Sender: TObject);
begin
  if Memo1.Modified then HasChanged := True;
end;

procedure TControlsForm.FormCreate(Sender: TObject);
begin
  Uglobals.SetFont(self);
  Memo1.Font.Style := Font.Style;
end;

procedure TControlsForm.BtnHelpClick(Sender: TObject);
begin
  MainForm.LaunchHelp(HelpContext);
end;

end.
