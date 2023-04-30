{
   TOpenTxtFileDialog Component
   *****************************

   A Lazarus component that implements an Open File common dialog
   control with a preview window that displays the first few lines
   of the file selected to be opened. It is derived from the
   TPreviewFileDialog control using the Lazarus TOpenPictureDialog component
   as a model.

   The WordWrap property determines if text in the preview window is
   word wrapped or not. An OnPreview event is available that allows
   one to customize the text displayed in the preview window.

   Version: 1.0
   Ported to Lazarus by: Zorba Lopez Rivera
   Date:    15/05/23
}

unit OpenDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$R-}

interface

uses
{$IFDEF WINDOWS}
  Windows, Messages,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils, Classes, Controls, StdCtrls, Graphics,
  ExtCtrls, Buttons, Dialogs, ExtDlgs, ResourceStrings;

const
  MAXLINES = 50;

type

  TPreviewEvent = procedure(Sender: TObject; Fname: String; var S: String;
    var WW: Boolean) of Object;

  { TOpenTxtFileDialog }

  TOpenTxtFileDialog = class(TPreviewFileDialog)
  private
    FPreviewPanel: TGroupBox;
    FShowPreview: Boolean;
    FTextFileLabel: TLabel;
    FTextLabel: TLabel;
    FWordWrap:  Boolean;
    FPreviewFilename: string;
    FOnPreview: TPreviewEvent;
  protected
    procedure InitPreviewControl; override;
    procedure ClearPreview; virtual;
    procedure UpdatePreview; virtual;
    procedure SetShowPreview(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoClose; override;
    procedure DoSelectionChange; override;
    procedure DoShow; override;
  published
    property OnPreview: TPreviewEvent read FOnPreview write FOnPreview;
    property ShowPreview: Boolean read FShowPreview write SetShowPreview;
    property WordWrap:  Boolean read FWordWrap write SetWordWrap;
  end;


implementation

uses LazFileUtils{$IFDEF WINDOWS} , LCLType {$ENDIF};


{ TOpenTxtFileDialog }

{$R 'OpenDlg.dcr'}

constructor TOpenTxtFileDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FShowPreview := True;
  FWordWrap := False;
  FOnPreview := nil;
  FPreviewPanel := TGroupBox.Create(Self);
  with FPreviewPanel do
  begin
    Name := 'PreviewPanel';
    Caption := '';
    Align := alRight;
    {BevelOuter := bvNone;}
    BorderWidth := 6;
    TabOrder := 1;
    FTextFileLabel := TLabel.Create(Self);
    with FTextFileLabel do
    begin
      Name := 'TextFileLabel';
      Caption := TXT_PREVIEW;
      SetBounds(6, 6, FPreviewPanel.Width, 23);
      Align := alTop;
      AutoSize := True;
      Parent := FPreviewPanel;
    end;
    FTextLabel := TLabel.Create(Self);
    with FTextLabel do
    begin
      Name := 'TextLabel';
      Caption := '';
      Color := clWindow;
      SetBounds(6, 29, FPreviewPanel.Width, FPreviewPanel.Width-23);
      Align := alClient;
      AutoSize := True;
      Font.Name := 'Courier New';
      Font.Size := 8;
      Font.Color := clWindowText;
      WordWrap := FWordWrap;
      Parent := FPreviewPanel;
      ParentColor := True;
    end;
  end;
end;


destructor TOpenTxtFileDialog.Destroy;
begin
  FTextLabel.Free;
  FTextFileLabel.Free;
  FPreviewPanel.Free;
  inherited Destroy;
end;


procedure TOpenTxtFileDialog.DoClose;
begin
  ClearPreview;
  inherited DoClose;
end;


procedure TOpenTxtFileDialog.DoSelectionChange;
begin
  UpdatePreview;
  inherited DoSelectionChange;
end;


procedure TOpenTxtFileDialog.DoShow;

begin
  ClearPreview;
  inherited DoShow;
end;


procedure TOpenTxtFileDialog.InitPreviewControl;
begin
  inherited InitPreviewControl;
  FPreviewPanel.Parent:=PreviewFileControl;
end;


procedure TOpenTxtFileDialog.ClearPreview;
begin
  FTextLabel.Caption:='';
end;


procedure TOpenTxtFileDialog.UpdatePreview;
var
  CurFilename: String;
  FileIsValid: boolean;
  F: TextFile;
  S: String;
  Line: String;
  LineCount: Integer;
  WW: Boolean;

begin
  S := '';
  WW := FWordWrap;
  CurFilename := FileName;
  if CurFilename = FPreviewFilename then exit;
  FPreviewFilename := CurFilename;
  FileIsValid := FileExistsUTF8(FPreviewFilename)
                 and (not DirPathExists(FPreviewFilename))
                 and FileIsReadable(FPreviewFilename);
  if FileIsValid and FShowPreview then
  begin
      if Assigned(FOnPreview) then FOnPreview(self, CurFilename, S, WW);
      if S = '' then
      try
        WW := FWordWrap;
        AssignFile(F,CurFilename);
        {$I-}
        Reset(F);
        {$I+}
        if (IOResult = 0) then
          try
            LineCount := 0;
            while (LineCount < MAXLINES) and (not EOF(F)) do
            begin
              ReadLn(F,Line);
              S := S + Line + #13;
              Inc(LineCount);
            end;
          except
            FileIsValid := False;
          end;
      finally
        CloseFile(F);
      end;
  FTextLabel.WordWrap := Length(S) > 0;
  FTextLabel.Caption := S;
  if not FileIsValid then
    ClearPreview;
  end;
end;

procedure TOpenTxtFileDialog.SetShowPreview(Value: Boolean);
begin
  if FShowPreview <> Value then
  begin
    FShowPreview := Value;
  end;
end;


procedure TOpenTxtFileDialog.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    FTextLabel.WordWrap := Value;
  end;
end;


end.
