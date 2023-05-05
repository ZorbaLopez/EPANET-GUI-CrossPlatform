unit Fmain;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$IFDEF DARWIN}
  {$modeswitch objectivec2}
{$ENDIF}

{-------------------------------------------------------------------}
{                    Unit:    Fmain.pas                             }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                    Ported to Lazarus by: Zorba Lopez Rivera       }
{                    Date:    15/05/23                              }
{                                                                   }
{   Delphi form unit containing main unit for EPANET2W.             }
{                                                                   }
{   EPANET2W is the Windows version of EPANET 2 -- a program that   }
{   simulates the hydraulic and water quality behavior of           }
{   pressurized pipe networks.                                      }
{                                                                   }
{   This unit contains the main MDI parent form, MainForm.          }
{   It handles all user selections from the main menu.              }
{                                                                   }
{   Consult the files Uglobals.pas and Consts.txt for a             }
{   description of global constants, record and class types,        }
{   and global variables.                                           }
{-------------------------------------------------------------------}

interface

uses
{$IFDEF WINDOWS}
  Windows, HtmlHelp, Messages,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
{$IFDEF LINUX}
  HelpIntfs, LazHelpIntf, LazHelpCHM,
{$ENDIF}

  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, Fbrowser, Fmap,
  ExtCtrls, Buttons, StdCtrls, ComCtrls, ExtDlgs, ImgList, System.UITypes, Fproped,
  ToolWin, Printers, PgSetup, OpenDlg, Xprinter, Uglobals, Uutils, LCLTranslator,
  StrUtils;

const
  MDIClass : array [0..6] of string = ('TMainForm', 'TStatusForm', 'TEnergyForm',
                                       'TCalibReportForm', 'TTableForm', 'TGraphForm',
                                       'TContourForm');

type

  { TMainForm }

  TMainForm = class(TForm)
    ControlBar1: TCoolBar;
    ImageList1d: TImageList;
    MapToolBar: TToolBar;
    StdToolBar: TToolBar;

    MapHeader: TPanel;
      MapImage: TImage;
      MapCaption: TPanel;
    BrowserHeader: TPanel;
      BrowserImage: TImage;
      BrowserCaption: TPanel;
    PropEditHeader: TPanel;

    MapPanel: TPanel;
    BrowserPanel: TPanel;
    PropEditPanel: TPanel;

    Splitter1: TSplitter;
    Splitter2: TSplitter;

    MainMenu1: TMainMenu;
    MnuFile: TMenuItem;
      MnuNew: TMenuItem;
      MnuOpen: TMenuItem;
      MnuSave: TMenuItem;
      MnuSaveAs: TMenuItem;
      N1: TMenuItem;
      MnuImport: TMenuItem;
        MnuImportMap: TMenuItem;
        MnuImportScenario: TMenuItem;
        MnuImportNetwork: TMenuItem;
      MnuExport: TMenuItem;
        MnuExportMap: TMenuItem;
        MnuExportScenario: TMenuItem;
        MnuExportNetwork: TMenuItem;
      N10: TMenuItem;
      MnuPageSetup: TMenuItem;
      MnuPrintPreview: TMenuItem;
      MnuPrint: TMenuItem;
      N11: TMenuItem;
      MnuPreferences: TMenuItem;
      N2: TMenuItem;
      MRU1: TMenuItem;
      MRU2: TMenuItem;
      MRU3: TMenuItem;
      MRU4: TMenuItem;
      MRUSep: TMenuItem;
      MnuExit: TMenuItem;

    MnuEdit: TMenuItem;
      MnuCopy: TMenuItem;
      N3: TMenuItem;
      MnuSelectObject: TMenuItem;
      MnuSelectVertex: TMenuItem;
      MnuSelectRegion: TMenuItem;
      MnuSelectAll: TMenuItem;
      N16: TMenuItem;
      MnuGroupEdit: TMenuItem;

    MnuView: TMenuItem;
      MnuDimensions: TMenuItem;
      MnuBackdrop: TMenuItem;
        MnuBackdropLoad: TMenuItem;
        MnuBackdropUnload: TMenuItem;
        MnuBackdropAlign: TMenuItem;
        N15: TMenuItem;
        MnuBackdropShow: TMenuItem;
      N5: TMenuItem;
      MnuPan: TMenuItem;
      MnuZoomIn: TMenuItem;
      MnuZoomOut: TMenuItem;
      MnuFullExtent: TMenuItem;
      N14: TMenuItem;
      MnuFind: TMenuItem;
      MnuQuery: TMenuItem;
      N12: TMenuItem;
      MnuOVMap: TMenuItem;
      MnuLegends: TMenuItem;
        MnuNodeLegend: TMenuItem;
        MnuLinkLegend: TMenuItem;
        MnuTimeLegend: TMenuItem;
        N6: TMenuItem;
        MnuModifyLegend: TMenuItem;
          MnuModifyNodeLegend: TMenuItem;
          MnuModifyLinkLegend: TMenuItem;
      MnuToolbars: TMenuItem;
        MnuStdToolbar: TMenuItem;
        MnuMapToolbar: TMenuItem;
      N7: TMenuItem;
      MnuViewOptions: TMenuItem;

    MnuProject: TMenuItem;
      MnuProjectDefaults: TMenuItem;
      MnuProjectDescription: TMenuItem;
      MnuProjectCalibData: TMenuItem;
      N9: TMenuItem;
      MnuAnalysisOptions: TMenuItem;
      MnuProjectRunAnalysis: TMenuItem;

    MnuReport: TMenuItem;
      MnuReportStatus: TMenuItem;
      MnuReportEnergy: TMenuItem;
      MnuReportCalibration: TMenuItem;
      MnuReportReaction: TMenuItem;
      MnuReportFull: TMenuItem;
      N13: TMenuItem;
      MnuGraph: TMenuItem;
      MnuTable: TMenuItem;
      N8: TMenuItem;
      MnuReportOptions: TMenuItem;

    MnuWindow: TMenuItem;
      MnuArrange: TMenuItem;
      MnuCloseAll: TMenuItem;

    MnuHelp: TMenuItem;
      MnuHelpTopics: TMenuItem;
      MnuHelpUnits: TMenuItem;
      MnuHelpTutorial: TMenuItem;
      N4: TMenuItem;
      MnuAbout: TMenuItem;

    PopupMenu1: TPopupMenu;
      PopupAutoLength: TMenuItem;

    OpenPictureDialog: TOpenPictureDialog;
    SaveDialog: TSaveDialog;
    FontDialog: TFontDialog;
    ImageList: TImageList;
    ImageList1: TImageList;
    ImageList2: TImageList;
    ImageList2d: TImageList;

    ProgressPanel: TPanel;
      ProgressBar: TProgressBar;
      N17: TMenuItem;
    StatusPanel: TPanel;
      StatusBarPanel1: TPanel;
      StatusBarPanel2: TPanel;
      StatusBarPanel3: TPanel;
      StatusBarPanel4: TPanel;
      StatusBarPanel5: TPanel;
      RunStatusBox: TPaintBox;
    TBCopy: TToolButton;
    TBDelete: TToolButton;
    TBFind: TToolButton;
    TBGraph: TToolButton;
    TBNew: TToolButton;
    TBOpen: TToolButton;
    TBOptions: TToolButton;
    TBPrint: TToolButton;
    TBQuery: TToolButton;
    TBRun: TToolButton;
    TBSave: TToolButton;
    TBSep1: TToolButton;
    TBSep2: TToolButton;
    TBSep3: TToolButton;
    TBSep4: TToolButton;
    TBTable: TToolButton;

    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivated(Sender: TObject);

    {$IFDEF DARWIN}
    procedure MnuAppClick(Sender: TObject);
    {$ENDIF}
    procedure MnuFileClick(Sender: TObject);
    procedure MnuNewClick(Sender: TObject);
    procedure MnuOpenClick(Sender: TObject);
    procedure MnuSaveClick(Sender: TObject);
    procedure MnuSaveAsClick(Sender: TObject);
    procedure MnuImportMapClick(Sender: TObject);
    procedure MnuImportScenarioClick(Sender: TObject);
    procedure MnuImportNetworkClick(Sender: TObject);
    procedure MnuExportMapClick(Sender: TObject);
    procedure MnuExportScenarioClick(Sender: TObject);
    procedure MnuExportNetworkClick(Sender: TObject);
    procedure MnuPageSetupClick(Sender: TObject);
    procedure MnuPrintPreviewClick(Sender: TObject);
    procedure MnuPrintClick(Sender: TObject);
    procedure MnuPreferencesClick(Sender: TObject);
    procedure MRUClick(Sender: TObject);
    procedure MnuExitClick(Sender: TObject);

    procedure MnuEditClick(Sender: TObject);
    procedure MnuCopyClick(Sender: TObject);
    procedure MnuSelectObjectClick(Sender: TObject);
    procedure MnuSelectVertexClick(Sender: TObject);
    procedure MnuSelectRegionClick(Sender: TObject);
    procedure MnuSelectAllClick(Sender: TObject);
    procedure MnuGroupEditClick(Sender: TObject);

    procedure MapActionClick(Sender: TObject);
    procedure MnuBackdropClick(Sender: TObject);
    procedure MnuBackdropLoadClick(Sender: TObject);
    procedure MnuBackdropShowClick(Sender: TObject);
    procedure MnuDimensionsClick(Sender: TObject);
    procedure MnuBackdropUnloadClick(Sender: TObject);
    procedure MnuBackdropAlignClick(Sender: TObject);
    procedure MnuFindClick(Sender: TObject);
    procedure MnuQueryClick(Sender: TObject);
    procedure MnuOVMapClick(Sender: TObject);
    procedure MnuLegendsClick(Sender: TObject);
    procedure MnuNodeLegendClick(Sender: TObject);
    procedure MnuLinkLegendClick(Sender: TObject);
    procedure MnuTimeLegendClick(Sender: TObject);
    procedure MnuModifyLegendClick(Sender: TObject);
    procedure MnuModifyLinkLegendClick(Sender: TObject);
    procedure MnuModifyNodeLegendClick(Sender: TObject);
    procedure MnuStdToolbarClick(Sender: TObject);
    procedure MnuMapToolbarClick(Sender: TObject);
    procedure MnuViewOptionsClick(Sender: TObject);

    procedure MnuProjectDefaultsClick(Sender: TObject);
    procedure MnuProjectSummaryClick(Sender: TObject);
    procedure MnuProjectCalibDataClick(Sender: TObject);
    procedure MnuAnalysisOptionsClick(Sender: TObject);
    procedure MnuProjectRunAnalysisClick(Sender: TObject);

    procedure MnuReportClick(Sender: TObject);
    procedure MnuReportStatusClick(Sender: TObject);
    procedure MnuReportEnergyClick(Sender: TObject);
    procedure MnuReportCalibrationClick(Sender: TObject);
    procedure MnuReportReactionClick(Sender: TObject);
    procedure MnuReportFullClick(Sender: TObject);
    procedure MnuGraphClick(Sender: TObject);
    procedure MnuTableClick(Sender: TObject);
    procedure MnuReportOptionsClick(Sender: TObject);

    procedure MnuWindowClick(Sender: TObject);
    procedure MnuArrangeClick(Sender: TObject);
    procedure MnuCloseAllClick(Sender: TObject);
    procedure AddMDIMnu(Sender: TObject);
    procedure DeleteMDIMnu(Sender: TObject);
    procedure MDIMnuClick(Sender: TObject);

    procedure MnuHelpTopicsClick(Sender: TObject);
    procedure MnuHelpUnitsClick(Sender: TObject);
    procedure MnuHelpTutorialClick(Sender: TObject);
    procedure MnuAboutClick(Sender: TObject);

    procedure PopupMenu1Popup(Sender: TObject);
    procedure PopupAutoLengthClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton13Click(Sender: TObject);
    procedure TBDeleteClick(Sender: TObject);
    procedure StdToolBarClose(Sender: TObject);
    procedure MapToolBarClose(Sender: TObject);
    procedure RunStatusBoxPaint(Sender: TObject);
    procedure OpenTextFileDialogPreview(Sender: TObject; Fname: String;
      var S: String; var WW: Boolean);
    procedure TBOptionsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LanguageSelect(Sender: TObject);
    procedure HeaderActivated(Sender: TObject);

    procedure SplitterCanOffset(Sender: TObject; var NewOffset: Integer; var Accept: Boolean);

  private
    { Private declarations }
    {$IFDEF LINUX}
    HTMLBrowserHelpViewer1: TLHelpConnector;
    HTMLHelpDatabase1: TCHMHelpDatabase;
    HTMLHelpDatabase2: TCHMHelpDatabase;
    {$ENDIF}
    {$IFDEF DARWIN}
    AppMenu : TMenuItem;
    {$ENDIF}
    Tutorial: string;

    procedure ClearAll;
    procedure CloseForms;
    procedure CreateTempFiles;
    procedure DeleteTempFiles;
    procedure FindBackdropFile;
    procedure InitPageLayout;
    procedure MRUDisplay(Sender: TObject);
    procedure MRUUpdate(Sender: TObject; const AddFileName: String);
    procedure OpenFile(Sender: TObject; const Fname: String);
    procedure Print(Dest: TDestination);
    procedure RefreshForms;
    procedure RunSimulation;
    procedure SaveFile(Fname: String);
    function  SaveFileDlg(Sender: TObject): Integer;
    procedure ShowAutoLengthStatus;
    procedure ShowRunStatus;

    procedure CreateDialogs(Sender: TObject);

    function GetActiveChildForm: Integer;

 public
    { Public declarations }
    MapForm : TMapForm1;
    BrowserForm: TBrowserForm1;
    PropEditForm : TPropEditForm1;
    FormActive : String;
    MRUList:  TStringList;
    OpenTextFileDialog : TOpenTxtFileDialog;
    PageSetupDialog : TPageSetupDialogEx;
    thePrinter: TPrintControl;

    procedure CreateGraph(GraphSelection: TGraphSelection);
    function  FormExists(const Name: String): Boolean;
    procedure HideProgressBar;
    procedure PageSetup;
    procedure PanButtonClick;
    procedure RefreshMapForm;
    procedure SelectorButtonClick;
    procedure SetChangeFlags;
    procedure ShowProgressBar(const Msg: String);
    procedure UpdateProgressBar(var Count: Integer; const StepSize: Integer);

    procedure LaunchHelp(index: integer);

end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}
{$R Mycurs32.res}  // Resource file containing custom cursors

uses
  {$IFDEF DARWIN}
  CocoaAll,
  {$endif}
  Fcalib, Fcontour, Fenergy, Fgraph, Fovmap,
  Fsimul, Fstatus, Fsummary, Ftable, Dabout, Dcalib1, Dcalib2, Ddefault,
  Ddataexp, Dfind, Dgraph, Dgrouped, Dmapexp, Dprefers, Dquery,
  Dtable, Uexport, Ufileio, Uimport, Uinifile, Uinput, Uoutput, Ureport,
  ResourceStrings;


//===================================================================
//            Form Creation, Resizing, & Closing Handlers
//===================================================================


procedure TMainForm.FormCreate(Sender: TObject);
//----------------------------------------------
// Main form's OnCreate event handler.
//----------------------------------------------
var
  i  : Integer;

begin
  CreateDialogs(self);

// Load custom cursors
  Screen.Cursors[crXHAIR]   := LoadCursor(HInstance, PChar('xhair'));
  Screen.Cursors[crZOOMIN]  := LoadCursor(HInstance, PChar('zoomin'));
  Screen.Cursors[crZOOMOUT] := LoadCursor(HInstance, PChar('zoomout'));
  Screen.Cursors[crFIST]    := LoadCursor(HInstance, PChar('fist'));
  Screen.Cursors[crMOVE]    := LoadCursor(HInstance, PChar('move'));
  Screen.Cursors[crPENCIL]  := LoadCursor(HInstance, PChar('pencil'));
  Screen.Cursors[crARROWTIP]:= LoadCursor(HInstance, PChar('arrowtip'));

// Assign various directories
  EpanetDir := ExtractFilePath(Application.ExeName);
  WindowsDir := Uutils.GetWindowsDir;
  IniFileDir := Uutils.GetAppDataDir('EPANET', EpanetDir);
  RenameFile(EpanetDir+INIFILE, IniFileDir+INIFILE);

{$IFDEF WINDOWS}
  Application.HelpFile := EpanetDir + 'Help\' + 'en_US\' + HLPFILE;
  Tutorial := EpanetDir + 'Help\' + 'en_US\' + TUTORFILE;
{$ENDIF}
{$IFDEF LINUX}
  Application.HelpFile := EpanetDir + 'Help/' + 'en_US/'  + HLPFILE;
  Tutorial := EpanetDir + 'Help/' + 'en_US/' + TUTORFILE;
{$ENDIF}

  TempDir := GetTempDir;
  if TempDir = '' then TempDir := EpanetDir;

// Use '.' as decimal separator
// (DecimalSeparator is a built-in Delphi global variable)
  Application.UpdateFormatSettings := false;
  FormatSettings.DecimalSeparator := '.';

// Create most-recently-used file lists
  MRUList := TStringList.Create;

// Create calibration data lists
  for i := Low(NodeCalibData) to High(NodeCalibData) do
    NodeCalibData[i].Locations := TStringList.Create;
  for i := Low(LinkCalibData) to High(LinkCalibData) do
    LinkCalibData[i].Locations := TStringList.Create;

// Use default dimensions and backdrop for network map
  MapDimensions := DefMapDimensions;
  MapBackdrop := DefMapBackdrop;

// Use default number of decimal places
  for i := 0 to NODEVIEWS do NodeUnits[i].Digits := 2;
  for i := 0 to LINKVIEWS do LinkUnits[i].Digits := 2;
  LinkUnits[LINKLENGTH].Digits := 0;
  LinkUnits[FRICTION].Digits := 3;
  QueryColor := clRed;

// Create Network database object
  Network := TNetwork.Create;
  CurrentItem[CNTRLS] := 0;
  CurrentItem[OPTS] := 0;

// Disable printing options if there are no printers
  thePrinter.SetShowProgress(True);
  if Printer.Printers.Count = 0 then
  begin
    MnuPageSetup.Enabled := False;
    MnuPrintPreview.Enabled := False;
    MnuPrint.Enabled := False;
    TBPrint.Enabled := False;
  end;

// Set status flags
  RunStatus := rsNone;
  RunFlag := False;
  QueryFlag := False;
  HasChanged := False;
  ToolButton1.Down := True;

// Lazarus - Fix location of splitters
  Splitter2.Top:= ControlBar1.Height + Scale96ToScreen(300);
  Splitter1.Left:= Width - Scale96ToScreen(200);

// Create Property Editor form
  PropEditForm := TPropEditForm1.Create(self);
  PropEditForm.Parent := PropEditPanel;
  PropEditForm.Align:= alClient;
  PropEditForm.FormCreate(self);

// Retrieve preferences from .INI file
// (Must have created PropEditForm first)
  Uinifile.ReadMainFormSize;
  Uinifile.ReadIniFile;
  Uinifile.ReadDefaults;

//Lazarus - Select language
  LanguageSelect(self);

{$IFDEF DARWIN}
// Lazarus - Move About and Preferences to Apple main menu item
  AppMenu := TMenuItem.Create(Self);  {Application menu}
  AppMenu.Caption := #$EF#$A3#$BF;  {Unicode Apple logo char}
  MainMenu1.Items.Insert(0, AppMenu);

  MnuHelp.Remove(MnuAbout);
  MnuHelp.Remove(N4);
  AppMenu.Add(MnuAbout);
  AppMenu.Add(N4);

  MnuFile.Remove(MnuPreferences);
  MnuFile.Remove(N2);
  AppMenu.Add(MnuPreferences);
  AppMenu.Add(N2);

  AppMenu.OnClick := MnuAppClick;

//Lazarus - Detect Dark mode in MacOs
  if PropEditForm.Editor.isMacDarkMode then
  begin
    MapToolBar.Images := ImageList2d;
    StdToolBar.Images := ImageList1d;
    MapToolBar.DisabledImages := ImageList2;
    StdToolBar.DisabledImages := ImageList1;
  end;
{$ENDIF}

{$IFDEF LINUX}
//Set CHM Help
  HTMLBrowserHelpViewer1 := TLHelpConnector.Create(self);
  HTMLHelpDatabase1 := TCHMHelpDatabase.Create(self);
  HTMLHelpDatabase2 := TCHMHelpDatabase.Create(self);

  HTMLBrowserHelpViewer1.AutoRegister := true;
  HTMLBrowserHelpViewer1.LHelpPath := EpanetDir + 'lhelp';

  HTMLHelpDatabase1.AutoRegister := true;
  HTMLHelpDatabase1.KeywordPrefix := 'hlp';
  HTMLHelpDatabase1.Filename := Application.HelpFile;

  HTMLHelpDatabase2.AutoRegister := true;
  HTMLHelpDatabase2.KeywordPrefix := 'tut';
  HTMLHelpDatabase2.Filename := Tutorial;
{$ENDIF}

// Initialize checked status of view menu toolbar options
  MnuStdToolbar.Checked := StdToolBar.Visible;
  MnuMapToolbar.Checked := MapToolbar.Visible;

// Display status panel, hide progress meter panel
  ProgressPanel.Visible := False;
  StatusPanel.Visible := True;

// Set font style
  Uglobals.SetFont(self);

// Prevent form from repainting itself for now
{$IFDEF WINDOWS}
  LockWindowUpdate(Handle);
{$ELSE}
  Canvas.Lock;
{$ENDIF}

end;


procedure TMainForm.FormShow(Sender: TObject);
//-----------------------------------------------------
// Main form's OnShow handler.
// Creates Browser and Map forms
//-----------------------------------------------------
begin
  BrowserForm := TBrowserForm1.Create(self);
  BrowserForm.Parent := BrowserPanel;
  BrowserForm.Align := alClient;
  BrowserForm.FormCreate(self);

  MapForm := TMapForm1.Create(self);
  MapForm.Parent := MapPanel;
  MapForm.Align := alClient;
  MapForm.FormCreate(self);
  MapForm.SetPanelPos;
  MapForm.RedrawOnResize := True;
  FormActive := TXT_MAP;

  // Allow form to repaint itself
  {$IFDEF WINDOWS}
  Uinifile.ReadIniFile;
  LockWindowUpdate(0);
  {$ELSE}
  Canvas.Unlock;
  {$ENDIF}

  // Open file if one provided on command line,
  // otherwise simulate a click on File|New
  if ParamCount >= 1 then OpenFile(Sender, ParamStr(1))
  else MnuNewClick(Sender);
  //MnuArrangeClick(Sender);
  MainForm.Refresh;
end;


procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------
// Main form's OnClose handler; Frees all allocated resources.
//-----------------------------------------------------------------------
var
  i: Integer;
begin
// Save preferences to .INI file
  Uinifile.SaveIniFile;
  Uinifile.SaveMainFormSize;

// Clear any current output results
  CloseForms;
  Uoutput.ClearOutput;
  DeleteTempFiles;
  PropEditForm.FormDestroy(self);
  MapForm.FormDestroy(self);

// Free memory allocated for network database
  Network.Clear;
  Network.Free;

// Free MRU & calibration data string lists
  MRUList.Free;
  for i := Low(NodeCalibData) to High(NodeCalibData) do
    NodeCalibData[i].Locations.Free;
  for i := Low(LinkCalibData) to High(LinkCalibData) do
    LinkCalibData[i].Locations.Free;
end;


procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
//------------------------------------------------
// OnCloseQuery handler for Main form.
// Checks if user wants to save input file
// or cancel the close request.
//------------------------------------------------
begin
  if SaveFileDlg(Sender) = mrCancel then
    CanClose := False
  else
    CanClose := True;
end;

procedure TMainForm.HeaderActivated(Sender: TObject);
//------------------------------------------------
// Activates Frames on header click
//------------------------------------------------
begin
    if (TControl(Sender).Name = 'MapImage') or
       (TControl(Sender).Name = 'MapCaption') then
    begin
      MapForm.SetFocus;
    end

    else if (TControl(Sender).Name = 'BrowserImage') or
            (TControl(Sender).Name = 'BrowserCaption') then
    begin
      BrowserForm.SetFocus;
    end
    else if TControl(Sender).Name = 'PropEditHeader' then
    begin
      if PropEditForm.Visible then PropEditForm.SetFocus
    end;
end;

procedure TMainForm.FormActivated(Sender: TObject);
//------------------------------------------------
// Manages Activated Frames
//------------------------------------------------
begin
    if not Sender.ClassNameIs('TMapForm1') then
    begin
      MapHeader.Color := clInactiveCaption;
      MapCaption.Color := clInactiveCaption;
      MapCaption.Font.Color := clInactiveCaptionText;
    end;
    if not Sender.ClassNameIs('TBrowserForm1') then
    begin
      BrowserHeader.Color := clInactiveCaption;
      BrowserCaption.Color := clInactiveCaption;
      BrowserCaption.Font.Color := clInactiveCaptionText;
    end;
    if not Sender.ClassNameIs('TPropEditForm1') then
    begin
      PropEditHeader.Color := clInactiveCaption;
      PropEditHeader.Font.Color := clInactiveCaptionText;
    end;
end;


//===================================================================
//                        File Menu Handlers
//===================================================================

procedure TMainForm.MnuFileClick(Sender: TObject);
//-------------------------------------------------
// OnClick handler for File menu
//-------------------------------------------------
begin
// Enable/disable printing (if printing is allowed)
  if MnuPageSetup.Enabled then
  begin
    MnuPrint.Enabled :=
      (Screen.ActiveForm is TMainForm) or
      (Screen.ActiveForm is TStatusForm) or
      (Screen.ActiveForm is TEnergyForm) or
      (Screen.ActiveForm is TCalibReportForm) or
      (Screen.ActiveForm is TTableForm) or
      (Screen.ActiveForm is TGraphForm) or
      (Screen.ActiveForm is TContourForm);
    MnuPrintPreview.Enabled := MnuPrint.Enabled;
  end;

// Display menu items for most recently used files
  MRUDisplay(Sender);
end;


procedure TMainForm.MnuNewClick(Sender: TObject);
//-------------------------------------------------------------
// Creates a new project when File|New selected from main menu.
//-------------------------------------------------------------
begin
// Save current input data if it has changed
  if SaveFileDlg(Sender) = mrCancel then Exit;

// Close any output display forms
  CloseForms;

// Re-set name of input data file
  InputFileName := '';
  InputFileType := iftNET;
  Caption := TXT_MAIN_CAPTION;
  ReadOnlyFlag := False;

// Clear previous network database
  ClearAll;
  ShowRunStatus;
  PageSetup;

// Make Junctions the current object in Browser
  CurrentList := -1;
  BrowserForm.UpdateBrowser(JUNCS,-1);
  FormActive := TXT_MAP;
end;


procedure TMainForm.MnuOpenClick(Sender: TObject);
//------------------------------------------------------------------
// Opens existing project file when File|Open selected from main menu.
//------------------------------------------------------------------
begin
// Prompt user to save current network data or cancel
  if SaveFileDlg(Sender) = mrCancel then Exit;

// Execute open file dialog
  with OpenTextFileDialog do
  begin
  // Set options for Open File Dialog control
    Title := TXT_OPEN_PROJECT_TITLE;
    Filter := TXT_OPEN_PROJECT_FILTER;
    if InputFileType = iftINP then FilterIndex := 2
    else FilterIndex := 1;
    Filename := ExtractFileName(InputFileName);
    Options := Options - [ofHideReadOnly] + [ofEnableSizing];
    if Execute then
    begin
      ReadOnlyFlag := (ofReadOnly in Options) or
                      (HasAttr(FileName, faReadOnly));
      OpenFile(Sender,Filename);
    end;
    FilterIndex := 1;
  end;
end;


procedure TMainForm.MnuSaveClick(Sender: TObject);
//------------------------------------------------
// OnClick handler for File|Save menu item.
//------------------------------------------------
begin
// Implement SaveAs for new, un-named project or for .INP input file
  if (Length(InputFileName) = 0)
  or (InputFileType = iftINP)
  then MnuSaveAsClick(Sender)

// Otherwise save project under current file name
  else SaveFile(InputFileName);
end;


procedure TMainForm.MnuSaveAsClick(Sender: TObject);
//---------------------------------------------------------------------
// Saves project to new file when File|Save As selected from main menu.
//---------------------------------------------------------------------
begin
  with SaveDialog do
  begin
    Title := TXT_SAVE_PROJECT_TITLE;
    Filter := TXT_SAVE_PROJECT_FILTER;
    DefaultExt := 'net';
    if Length(InputFileName) > 0 then Filename :=
      ChangeFileExt(ExtractFileName(InputFileName),'.net')
    else Filename := '*.net';
    if Execute then SaveFile(Filename);
    DefaultExt := '';
  end;
end;


procedure TMainForm.MnuImportScenarioClick(Sender: TObject);
//-----------------------------------------------------------------
// Imports previously saved data when File|Import|Scenario selected
//-----------------------------------------------------------------
begin
  with OpenTextFileDialog do
  begin
    Title := TXT_OPEN_SCENARIO_TITLE;
    Filter := TXT_SCENARIO_FILTER;
    Filename := '*.scn';
    Options := Options + [ofHideReadOnly];
    if Execute then Uimport.ReadScnFile(Filename);
  end;
end;


procedure TMainForm.MnuImportMapClick(Sender: TObject);
//----------------------------------------------------------
// Imports network map when File|Import|Map selected
//----------------------------------------------------------
begin
  with OpenTextFileDialog do
  begin

  // Set options & execute Open File dialog
    Title := TXT_OPEN_MAP_TITLE;
    Filter := TXT_MAP_FILTER;
    Filename := '*.map';
    Options := Options + [ofHideReadOnly];
    if Execute then
    begin

    // Read new map file & redraw network map
      if (Uimport.ImportMapFile(Filename)) then
      begin
        FindBackdropFile;
        MapForm.Map.Rescale(MapDimensions);
        MapForm.OpenMapBackdrop;
        MapForm.RedrawMap;
        OVMapForm.Rescale;
      end
      else Uutils.MsgDlg(Format(Msg_NO_MAP_FILE, [Filename]), mtError, [mbOK]);
    end;
  end;
end;


procedure TMainForm.MnuImportNetworkClick(Sender: TObject);
//---------------------------------------------------------
// Imports new network from file saved in ASCII text format
// when File|Import|Network selected.
//---------------------------------------------------------
begin
// Save current network data if it has changed
  if SaveFileDlg(Sender) = mrCancel then Exit;
  with OpenTextFileDialog do
  begin

  // Set options for Open File Dialog control
    Title := TXT_OPEN_NETWORK_TITLE;
    Filter := TXT_NETWORK_FILTER;
    Filename := '*.inp';
    Options := Options - [ofHideReadOnly];

  // Execute dialog & open input file
    if Execute then
    begin
      ReadOnlyFlag := (ofReadOnly in Options) or
                      (HasAttr(FileName, faReadOnly));
      OpenFile(Sender,Filename);
    end;
  end;
end;


procedure TMainForm.MnuExportScenarioClick(Sender: TObject);
//---------------------------------------------------------
// Displays dialog box allowing user to save selected
// category of network data when File|Export|Scenario selected.
//----------------------------------------------------------
begin
  with TDataExportForm.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;


procedure TMainForm.MnuExportMapClick(Sender: TObject);
//------------------------------------------------------
// Saves network map to Windows metafile or AutoCad DXF
// file when File|Export|Map selected.
//------------------------------------------------------
begin
  with TMapExportForm.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;


procedure TMainForm.MnuExportNetworkClick(Sender: TObject);
//--------------------------------------------------------
// Saves network data to ASCII text file when
// File|Export|Network selected.
//--------------------------------------------------------
begin
  with SaveDialog do
  begin
    Title := TXT_SAVE_NETWORK_TITLE;
    Filter := TXT_NETWORK_FILTER;
    if (Length(InputFileName) > 0) then Filename :=
      ChangeFileExt(ExtractFileName(InputFileName),'.inp')
    else Filename := '*.inp';
    if Execute then
    begin
      if ExtractFileExt(Filename) = '' then Filename := Filename + '.inp';
      if (CompareText(InputFileName,Filename) = 0)
      and ReadOnlyFlag
      then Uutils.MsgDlg(Format(MSG_READONLY, [ExtractFileName(InputFileName)]),
                         mtInformation, [mbOK])
      else Uexport.ExportDataBase(Filename,True)
    end;
  end;
end;


procedure TMainForm.MnuPageSetupClick(Sender: TObject);
//----------------------------------------------------------
// Displays Page Setup dialog when File|Page Setup selected
//----------------------------------------------------------
begin

  with PageSetupDialog, PageLayout do
  begin
  // Transfer current margin settings from PageLayout to
  // the PageSetupDialog component
    PageMargins.Left   := LMargin;
    PageMargins.Right  := RMargin;
    PageMargins.Top    := TMargin;
    PageMargins.Bottom := BMargin;
    Printer.Orientation := TPrinterOrientation(Orientation);

  // Execute the dialog
    if PageSetupDialog.Execute then
    begin
    // Transfer new margins to PageLayout
      LMargin := PageMargins.Left;
      RMargin := PageMargins.Right;
      TMargin := PageMargins.Top;
      BMargin := PageMargins.Bottom;

    // Setup the printed page again
      Orientation := Ord(Printer.Orientation);
      PageSetup;
      HasChanged := True;
    end;
  end;
end;


procedure TMainForm.MnuPrintPreviewClick(Sender: TObject);
//----------------------------------------------------------------------
// Prints active window to preview form when File|Print Preview selected
//----------------------------------------------------------------------
begin
  Print(dPreview);
end;


procedure TMainForm.MnuPrintClick(Sender: TObject);
//----------------------------------------------------------------------
// Prints active window to printer form when File|Print selected
//----------------------------------------------------------------------
begin
  Print(dPrinter);
end;


procedure TMainForm.Print(Dest: TDestination);
//-------------------------------------------------------------
// Prints the active window to Dest (Preview window or printer)
//-------------------------------------------------------------
var
  i : Integer;

begin
    if FormActive = TXT_MAP then MapForm.Print(Dest)
    else if MatchStr(Screen.ActiveForm.ClassName, MDIClass) then
    begin
      i := GetActiveChildForm;
      if Screen.Forms[i] is TGraphForm then TGraphForm(Screen.Forms[i]).Print(Dest)
      else if Screen.Forms[i] is TTableForm then TTableForm(Screen.Forms[i]).Print(Dest)
      else if Screen.Forms[i] is TStatusForm then TStatusForm(Screen.Forms[i]).Print(Dest)
      else if Screen.Forms[i] is TContourForm then TContourForm(Screen.Forms[i]).Print(Dest)
      else if Screen.Forms[i] is TCalibReportForm then TCalibReportForm(Screen.Forms[i]).Print(Dest)
      else if Screen.Forms[i] is TEnergyForm then TEnergyForm(Screen.Forms[i]).Print(Dest);
    end;
end;


procedure TMainForm.MnuPreferencesClick(Sender: TObject);
//----------------------------------------------------------------
// Displays Preferences dialog box when File|Preferences selected
//----------------------------------------------------------------
begin
  with TPreferencesForm.Create(self) do
  try
    if ShowModal = mrOK then MapForm.RedrawMap;
  finally
    Free;
  end;
end;


procedure TMainForm.MnuExitClick(Sender: TObject);
//-----------------------------------------------------------
// Shuts down program when File|Exit selected from main menu.
//-----------------------------------------------------------
begin
  Close;
end;


//===================================================================
//                         Edit Menu Handlers
//===================================================================

procedure TMainForm.MnuEditClick(Sender: TObject);
//----------------------------------------------------
// OnClick handler for Edit menu
//----------------------------------------------------
begin
// Group editing applies only if fenceline drawn on map
  MnuGroupEdit.Enabled := (Not MapForm.Linking) and
                          (MapForm.NumFencePts > 0);

// Can only copy graph, table, or report forms
  MnuCopy.Enabled := (Screen.ActiveForm is TMainForm) or
                     (Screen.ActiveForm is TGraphForm) or
                     (Screen.ActiveForm is TTableForm) or
                     (Screen.ActiveForm is TContourForm) or
                     (Screen.ActiveForm is TStatusForm) or
                     (Screen.ActiveForm is TCalibReportForm) or
                     (Screen.ActiveForm is TEnergyForm);
end;


procedure TMainForm.MnuCopyClick(Sender: TObject);
//-------------------------------------------------------------
// Copies active window to clipboard when Edit|Copy To selected
//-------------------------------------------------------------
var
  i : Integer;
begin
    if FormActive = TXT_MAP then MapForm.CopyTo
    else if MatchStr(Screen.ActiveForm.ClassName, MDIClass) then
    begin
      i := GetActiveChildForm;
      if Screen.Forms[i] is TGraphForm then TGraphForm(Screen.Forms[i]).CopyTo
      else if Screen.Forms[i] is TTableForm then TTableForm(Screen.Forms[i]).CopyTo
      else if Screen.Forms[i] is TStatusForm then TStatusForm(Screen.Forms[i]).CopyTo
      else if Screen.Forms[i] is TContourForm then TContourForm(Screen.Forms[i]).CopyTo
      else if Screen.Forms[i] is TCalibReportForm then TCalibReportForm(Screen.Forms[i]).CopyTo
      else if Screen.Forms[i] is TEnergyForm then TEnergyForm(Screen.Forms[i]).CopyTo;
    end;
end;


procedure TMainForm.MnuSelectObjectClick(Sender: TObject);
//---------------------------------------------------------------------
// Puts map into Object Selection mode when Edit|Select Object selected
//---------------------------------------------------------------------
begin
  SelectorButtonClick;
end;


procedure TMainForm.MnuSelectVertexClick(Sender: TObject);
//---------------------------------------------------------------------
// Puts map into Vertex Selection mode when Edit|Select Vertex selected
//---------------------------------------------------------------------
begin
  ToolButton1Click(ToolButton14);
end;


procedure TMainForm.MnuSelectRegionClick(Sender: TObject);
//---------------------------------------------------------------------
// Puts map into Region Selection mode when Edit|Select Region selected
//---------------------------------------------------------------------
begin
  ToolButton1Click(ToolButton2);
end;


procedure TMainForm.MnuSelectAllClick(Sender: TObject);
//-----------------------------------------------------------------------
// Draws selection region around entire map when Edit|Select All selected
//-----------------------------------------------------------------------
begin
  MapForm.SelectAll;
end;


procedure TMainForm.MnuGroupEditClick(Sender: TObject);
//-----------------------------------------------------------
// Displays a Group Edit dialog when Edit|Group Edit selected
//-----------------------------------------------------------
var
  ObjType     : Integer;  //Type of object to be edited
  PropIndex   : Integer;  //Property to be edited
  ActionType  : Integer;  //Type of editing action
  Value       : String;   //Value for updating selected objects
  Filter      : TFilter;  //Filter on objects selected for editing
begin
  with TGroupEditForm.Create(self) do
  try
    if (ShowModal = mrOK) then
    begin
      GetGroupEditParams(ObjType,PropIndex,ActionType,Value,Filter);
      Uinput.GroupEdit(ObjType,PropIndex,ActionType,Value,Filter);
    end;
  finally
    Free;
  end;
end;


//===================================================================
//                         View Menu Handlers
//===================================================================

procedure TMainForm.MnuDimensionsClick(Sender: TObject);
//------------------------------------------------------
// OnClick handler for View|Dimensions menu item.
//------------------------------------------------------
begin
  MapForm.ModifyMapDimensions;
end;


procedure TMainForm.MnuBackdropClick(Sender: TObject);
//---------------------------------------------------
// OnClick handler for View|Backdrop menu item.
//---------------------------------------------------
var
  EnableFlag: Boolean;
begin
  if MapBackdrop.Visible then MnuBackdropShow.Caption := TXT_HIDE
  else MnuBackdropShow.Caption := TXT_SHOW;
  EnableFlag := (Length(MapBackdrop.Filename) > 0);
  MnuBackdropUnload.Enabled := EnableFlag;
  MnuBackdropAlign.Enabled := EnableFlag;
  MnuBackdropShow.Enabled := EnableFlag;
  MapForm.PopupBackdrop.Checked := MapBackdrop.Visible;
end;


procedure TMainForm.MnuBackdropLoadClick(Sender: TObject);
//--------------------------------------------------------
// Loads backdrop image into map display
//--------------------------------------------------------
begin
  with OpenPictureDialog do
    if Execute then
    begin
      MapBackdrop.Filename := Filename;
      MapBackdrop.Offset.X := 0.00;
      MapBackdrop.Offset.Y := 0.00;
      MapBackdrop.Visible := True;
      MapForm.OpenMapBackdrop;
      MapForm.RedrawMap;
      if OVmapForm.Visible then OVmapForm.Redraw;
      HasChanged := True;
    end;
end;


procedure TMainForm.MnuBackdropUnloadClick(Sender: TObject);
//--------------------------------------------------------
// OnClick handler for View|Backdrop|Unload menu item.
//--------------------------------------------------------
begin
  MapBackdrop := DefMapBackdrop;
  MapForm.RedrawMap;
  OVmapForm.Redraw;
  HasChanged := True;
end;


procedure TMainForm.MnuBackdropAlignClick(Sender: TObject);
//--------------------------------------------------------
// OnClick handler for View|Backdrop|Align menu item.
//--------------------------------------------------------
begin
  MapForm.BeginAligning(Sender);
end;


procedure TMainForm.MnuBackdropShowClick(Sender: TObject);
//--------------------------------------------------------
// OnClick handler for View|Backdrop|Show menu item.
//--------------------------------------------------------
begin
  MapForm.PopupBackdropClick(Sender);
end;


procedure TMainForm.MapActionClick(Sender: TObject);
//--------------------------------------------------
// OnClick handler for Map menu items Full Extent,
// Rescale, Pan, and Zoom.
//--------------------------------------------------
begin
  with MapForm do
  begin
  // Restore the map display
    //Show;
    SetFocus;
    //WindowState := wsNormal;

  // The Tag property of the menu item was set to a
  // constant corresponding to the action it controls.
    if Sender is TMenuItem then with TMenuItem(Sender) do
    case Tag of

    // Display map at full extent
      FULLEXTENT:
      begin
        FormResize(Sender);
        OVMapForm.ShowMapExtent;
        SelectorButtonClick;
      end;

    // Panning & zooming handled by ToolButtons 3-5
      PAN:     ToolButton1Click(ToolButton3);
      ZOOMIN:  ToolButton1Click(ToolButton4);
      ZOOMOUT: ToolButton1Click(ToolButton5);
    end;
  end;
end;


procedure TMainForm.MnuViewOptionsClick(Sender: TObject);
//-------------------------------------------------------
// Displays Map Options dialog when View|Options selected
//-------------------------------------------------------
begin
  MapForm.SetMapOptions;
end;


procedure TMainForm.MnuFindClick(Sender: TObject);
//----------------------------------------------------
// Activates the Find dialog to locate an object on
// the map when View|Find selected
//-----------------------------------------------------
begin
  FindForm.Visible := True;
end;


procedure TMainForm.MnuQueryClick(Sender: TObject);
//--------------------------------------------------
// Activates the Query dialog to highlight items
// on the map that meet a specific criteria when
// View|Query is selected.
//--------------------------------------------------
begin
  QueryForm.Visible := True;
end;


procedure TMainForm.MnuOVMapClick(Sender: TObject);
//------------------------------------------------
// Toggles display of the Overview Map when
// View|Overview Map selected.
//------------------------------------------------
begin
  MnuOVMap.Checked := not MnuOVMap.Checked;
  OVMapForm.Visible := MnuOVMap.Checked;
end;


procedure TMainForm.MnuLegendsClick(Sender: TObject);
//----------------------------------------------------------
// Enables/disables submenu items when View|Legends selected
//----------------------------------------------------------
begin
// Legend items disabled if no view variable or map is in Query mode
  MnuNodeLegend.Enabled := (not QueryFlag) and (CurrentNodeVar <> NOVIEW);
  MnuLinkLegend.Enabled := (not QueryFlag) and (CurrentLinkVar <> NOVIEW);
  MnuTimeLegend.Enabled := RunFlag;
  MnuModifyLegend.Enabled := (not QueryFlag);  {*** Updated 8/11/00 ***}
end;


procedure TMainForm.MnuLinkLegendClick(Sender: TObject);
//------------------------------------------------------
// Toggles display of Map's Link legend when
// View|Legends|Link selected
//------------------------------------------------------
begin
  MapForm.ToggleLinkLegend;
end;


procedure TMainForm.MnuNodeLegendClick(Sender: TObject);
//------------------------------------------------------
// Toggles display of Map's Node legend when
// View|Legends|Node selected
//------------------------------------------------------
begin
  MapForm.ToggleNodeLegend;
end;


procedure TMainForm.MnuTimeLegendClick(Sender: TObject);
//-----------------------------------------------------
// Toggles display of Map's Time legend when
// View|Legends|Time selected
//-----------------------------------------------------
begin
  MapForm.ToggleTimeLegend;
end;


procedure TMainForm.MnuModifyLegendClick(Sender: TObject);
//-----------------------------------------------------
// Enables/disables submenu items when
// View|Legends|Modify selected
//-----------------------------------------------------
begin
  MnuModifyNodeLegend.Enabled := (CurrentNodeVar <> NOVIEW);
  MnuModifyLinkLegend.Enabled := (CurrentLinkVar <> NOVIEW);
end;


procedure TMainForm.MnuModifyNodeLegendClick(Sender: TObject);
//------------------------------------------------------------
// Displays Legend Editor form when View|Legends|Modify|Node
// is selected.
//------------------------------------------------------------
begin
  MapForm.ModifyNodeLegend;
end;


procedure TMainForm.MnuModifyLinkLegendClick(Sender: TObject);
//------------------------------------------------------------
// Displays Legend Editor form when View|Legends|Modify|Link
// is selected.
//------------------------------------------------------------
begin
  MapForm.ModifyLinkLegend;
end;


procedure TMainForm.MnuStdToolbarClick(Sender: TObject);
//------------------------------------------------------
// Toggles display of Standard Toolbar when
// View|Toolbars|Standard selected
//------------------------------------------------------
begin
  MnuStdToolbar.Checked := not MnuStdToolbar.Checked;
  StdToolBar.Visible := MnuStdToolbar.Checked;
end;


procedure TMainForm.MnuMapToolbarClick(Sender: TObject);
//------------------------------------------------------
// Toggles display of Map Toolabr when
// View|Toolbars|Map selected
//------------------------------------------------------
begin
  MnuMapToolbar.Checked := not MnuMapToolbar.Checked;
  MapToolbar.Visible := MnuMapToolbar.Checked;
end;

procedure TMainForm.StdToolBarClose(Sender: TObject);
//-----------------------------------------------------------
// OnClose handler for Standard Toolbar -
// removes checked status for View|Toolbars|Standard menu item
//-----------------------------------------------------------
begin
  MnuStdToolbar.Checked := False;
end;


procedure TMainForm.MapToolBarClose(Sender: TObject);
//-------------------------------------------------------
// OnClose handler for Map Toolbar -
// removes checked status for View|Toolbars|Map menu item
//-------------------------------------------------------
begin
  MnuMapToolbar.Checked := False;
end;


//===================================================================
//                        Project Menu Handlers
//===================================================================


procedure TMainForm.MnuProjectDefaultsClick(Sender: TObject);
//----------------------------------------------------------------
// Displays Project Defaults dialog when Project|Defaults selected
//----------------------------------------------------------------
begin
  PropEditForm.Hide;
  with TDefaultsForm.Create(self) do
  try
    if (ShowModal = mrOK) and (Modified = True) then
    begin
      SetChangeFlags;
      ShowAutoLengthStatus;
    end;
  finally
    Free;
  end;
end;


procedure TMainForm.MnuProjectSummaryClick(Sender: TObject);
//--------------------------------------------------------------
// Displays Project Summary dialog when Project|Summary selected
//--------------------------------------------------------------
begin
  with TSummaryForm.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;


procedure TMainForm.MnuProjectCalibDataClick(Sender: TObject);
//--------------------------------------------------------------
// Displays Calibration Data dialog to register calibration data
// when Project|Calibration Data selected
//--------------------------------------------------------------
begin
  with TCalibDataForm.Create(self) do
  try
    if (ShowModal = mrOK) then Ufileio.RegisterCalibData;
  finally
    Free;
  end;
end;


procedure TMainForm.MnuAnalysisOptionsClick(Sender: TObject);
//-----------------------------------------------------------
// Causes Property Editor to display project analysis options
// when Project|Analysis Options is selected
//-----------------------------------------------------------
begin
  BrowserForm.SetOptions;
end;


procedure TMainForm.MnuProjectRunAnalysisClick(Sender: TObject);
//--------------------------------------------------------------
// Analyzes pipe network when Project|Run Analysis is selected
//--------------------------------------------------------------
begin
  RunSimulation;
end;


//===================================================================
//                      Report Menu Handlers
//===================================================================


procedure TMainForm.MnuReportClick(Sender: TObject);
//--------------------------------------------------
// Controls what kind of reports can be generated
// when Report menu is selected
//--------------------------------------------------
var
  ShowFlag: Boolean;
begin
// Reports available only if an analysis run has been made
  ShowFlag := (RunStatus in [rsSuccess, rsError, rsWarning]);
  MnuReportStatus.Enabled := ShowFlag;
  MnuReportEnergy.Enabled := RunFlag;
  MnuReportCalibration.Enabled := RunFlag;
  MnuReportReaction.Enabled := RunFlag and (QualFlag = 1);
  MnuReportFull.Enabled := RunFlag and not UpdateFlag;

// Options menu item is enabled depending on type of active window
  MnuReportOptions.Enabled :=
    (Screen.ActiveForm is TCalibReportForm) or
    (Screen.ActiveForm is TTableForm) or
    (Screen.ActiveForm is TGraphForm) or
    (Screen.ActiveForm is TContourForm);
end;


procedure TMainForm.MnuReportStatusClick(Sender: TObject);
//--------------------------------------------------------
// Displays EPANET run status report when
// Report|Status selected
//---------------------------------------------------------
var
  I : integer;

begin
// Check if Status Report form already exists
  if FormExists('StatusForm') then Exit;

// Otherwise create it
  if FileExists(TempReportFile) then
    with TStatusForm.Create(self) do
    try
      Caption := TXT_STATUS_REPORT;
      RefreshStatusReport;
      SetFocus;
    finally
    end;
  for I := Screen.FormCount - 1 downto 0 do
  begin
    if (Screen.Forms[I] is TStatusForm) then AddMDIMnu(Screen.Forms[I]);
  end;
end;


procedure TMainForm.MnuReportEnergyClick(Sender: TObject);
//--------------------------------------------------------
// Displays an Energy Report form when
// Report|Energy is selected
//--------------------------------------------------------
begin
// No report if no pumps in network
  if (Npumps = 0) then Uutils.MsgDlg(MSG_NO_PUMPS, mtInformation, [mbOK], self)

// Check if Energy Report form already exists
  else if FormExists('EnergyForm') then Exit

// Otherwise create & refresh the energy report
  else with TEnergyForm.Create(self) do
  try
    RefreshEnergyReport;
  finally
  end;
end;


procedure TMainForm.MnuReportReactionClick(Sender: TObject);
//----------------------------------------------------------
// Displays a Reactions Report form when
// Report|Reactions is selected
//----------------------------------------------------------
var
  GraphSelection: TGraphSelection;
begin
  with GraphSelection do
  begin
    GraphType := REACTRATEPLOT;
    VarType := 0;
    ObjectType := 0;
    Period := 0;
    Items := TStringList.Create;
  end;
  with TGraphForm.Create(self) do
  try
    if CreateGraph(GraphSelection) then Show
    else Close;
  finally
    GraphSelection.Items.Free;
  end;
end;


procedure TMainForm.MnuReportCalibrationClick(Sender: TObject);
//-------------------------------------------------------------
// Generates a Calibration Report when
// Report|Calibration is selected
//-------------------------------------------------------------
var
  RptType: Integer;  // Report type - nodes or links
  RptVar:  Integer;  // Report variable
begin
// Display Calibration Selection dialog form to select
// variable for Calibration Report
  RptType := NONE;
  with TCalibOptionsForm.Create(self) do
  try
    if VariablesList.Items.Count = 0 then
      Uutils.MsgDlg(MSG_NO_CALIB_DATA, mtInformation, [mbOK], self)
    else if ShowModal = mrOK then GetOptions(RptType, RptVar)
  finally
    Free;
  end;

// Create Calibration Report form for selected variable
  if RptType <> NONE then with TCalibReportForm.Create(self) do
  try
    CreateCalibReport(RptType, RptVar);
  finally
  end;
end;


procedure TMainForm.MnuReportFullClick(Sender: TObject);
//-------------------------------------------------------------
// Generates a Full Report to file when
// Report|Full is selected
//-------------------------------------------------------------
begin
  Ureport.CreateFullReport;
end;


procedure TMainForm.MnuGraphClick(Sender: TObject);
//-------------------------------------------------
// Displays the Graph Selection dialog when
// Report|Graph selected
//-------------------------------------------------
begin
//Create dialog if it isn't already showing
  if not FormExists('GraphSelectForm') then
    with TGraphSelectForm.Create(self) do
  try
    Show;
  finally
  end;
end;


procedure TMainForm.MnuTableClick(Sender: TObject);
//---------------------------------------------------------
// Displays the Table Selection dialog and creates a table
// when Report|Table is selected
//---------------------------------------------------------
var
  TableOptions: TTableOptions;
begin
// Display Table Selection dialog form
  with TTableOptionsForm.Create(self) do
  try
    LoadOptions(DefTableOptions);
    if ShowModal = mrOK then
      UnloadOptions(TableOptions)
    else
      TableOptions.TableType := NONE;
  finally
    Free;
  end;

// If a table was defined, then create it
  if TableOptions.TableType <> NONE then
  with TTableForm.Create(self) do
  try
    CreateTable(TableOptions);
  finally
  end;
end;


procedure TMainForm.MnuReportOptionsClick(Sender: TObject);
//---------------------------------------------------------
// Displays the appropriate options selection form
// when Report|Options is selected
//---------------------------------------------------------
var
  i : Integer;
begin
    i := GetActiveChildForm;
    if Screen.Forms[i] is TGraphForm then TGraphForm(Screen.Forms[i]).SetGraphOptions
    else if Screen.Forms[i] is TTableForm then TTableForm(Screen.Forms[i]).SetTableOptions
    else if Screen.Forms[i] is TContourForm then TContourForm(Screen.Forms[i]).SetContourOptions
    else if Screen.Forms[i] is TCalibReportForm then TCalibReportForm(Screen.Forms[i]).SetCalibOptions;
end;


//===================================================================
//                       Window Menu Handlers
//===================================================================


procedure TMainForm.MnuWindowClick(Sender: TObject);
//--------------------------------------------------
// Disables Window|Close All menu item if only the
// Map and Browser are displayed when the Window
// menu is pointed to
//--------------------------------------------------
var
  i : integer;

begin
  if Screen.FormCount <= 2 then
    MnuCloseAll.Enabled := False
  else
    MnuCloseAll.Enabled := True;
  for i := 3 to (MnuWindow.Count - 1) do
    with MnuWindow do
  begin
    if Items[i].Caption = Screen.ActiveForm.Caption then Items[i].Checked := True
    else Items[i].Checked := False;
  end;
end;


procedure TMainForm.MnuArrangeClick(Sender: TObject);
//--------------------------------------------------------
// Arranges MDI child windows when Window|Arrange selected
//--------------------------------------------------------
var
  DockHeight: Integer;
begin
// Account for height of Toolbars
  if ControlBar1.Visible then DockHeight := ControlBar1.Height
  else DockHeight := 0;

  Splitter2.Top:= DockHeight + Scale96ToScreen(300);
  Splitter1.Left:= Width - Scale96ToScreen(200);

  WindowState := wsMaximized;
  BringToFront;
  BrowserForm.SetFocus;
end;


procedure TMainForm.MnuCloseAllClick(Sender: TObject);
//----------------------------------------------------
// Closes all MDI children (except the Map and Browser)
// when Window|Close All is selected
//----------------------------------------------------
begin
  CloseForms;
end;


procedure TMainForm.AddMDIMnu(Sender: TObject);
//-------------------------------------------------
// Adds MDI form to Windows menu
//-------------------------------------------------
var
  i : Integer;
  item : TMenuItem;
begin
  for i := (MnuWindow.Count - 1) downto 0 do
  with MnuWindow do
  begin
    if Items[i].Caption = TForm(Sender).Caption then exit;
  end;
  item := TMenuItem.Create(MnuWindow);
  item.Caption := TForm(Sender).Caption;
  item.Name := TForm(Sender).Name;
  item.OnClick := MDIMnuClick;
  MnuWindow.Add(item);
  item.Visible := True;
end;


procedure TMainForm.DeleteMDIMnu(Sender: TObject);
//-------------------------------------------------
// Adds MDI form to Windows menu
//-------------------------------------------------
var
  i : integer;
begin
  for i := (MnuWindow.Count - 1) downto 0 do
    with MnuWindow do
  begin
    if Items[i].Caption = TForm(Sender).Caption then Items[i].Free;
  end;
end;


procedure TMainForm.MDIMnuClick(Sender: TObject);
//-------------------------------------------------
// Adds MDI form to Windows menu
//-------------------------------------------------
var
  i, j : integer;
  aFormH : HWND;

begin
  with Sender as TMenuItem do
  begin
    for i := Screen.FormCount - 1 downto 0 do
      if Screen.Forms[i].Caption = Caption then
      begin
        aFormH := Screen.Forms[i].Handle;
        break;
      end;

    for j := (MnuWindow.Count - 1) downto 0 do
      with MnuWindow do
    begin
      if Items[j].Caption = Screen.Forms[i].Caption then Items[j].Checked := True
      else Items[j].Checked := False;
    end;
    if Screen.Forms[i] is TGraphForm then TGraphForm(Screen.Forms[i]).FormActivate(self)
    else if Screen.Forms[i] is TTableForm then TTableForm(Screen.Forms[i]).FormActivate(self)
    else if Screen.Forms[i] is TStatusForm then TStatusForm(Screen.Forms[i]).FormActivate(self)
    else if Screen.Forms[i] is TContourForm then TContourForm(Screen.Forms[i]).FormActivate(self)
    else if Screen.Forms[i] is TCalibReportForm then TCalibReportForm(Screen.Forms[i]).FormActivate(self)
    else if Screen.Forms[i] is TEnergyForm then TEnergyForm(Screen.Forms[i]).FormActivate(self);
    TForm(FindControl(aFormH)).WindowState := wsNormal;
    TForm(FindControl(aFormH)).BringToFront;
  end;
end;

//===================================================================
//                       Window Menu Handlers
//===================================================================

procedure TMainForm.MnuAboutClick(Sender: TObject);
//-------------------------------------------------
// Displays About form when Help|About selected
//-------------------------------------------------
begin
  with TAboutBoxForm.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;


//===================================================================
//                     Toolbar Button Handlers
//===================================================================

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  with PopupAutoLength do
    Caption := Format(TXT_AUTOLENGTH, [AutoLengthStatus[Ord(not AutoLength)]]);
end;


procedure TMainForm.PopupAutoLengthClick(Sender: TObject);
begin
  AutoLength := not AutoLength;
  ShowAutoLengthStatus;
end;


procedure TMainForm.TBOptionsClick(Sender: TObject);
//--------------------------------------------------
// OnClick handler for Options toolbar button.
//--------------------------------------------------
begin
  if FormActive = TXT_MAP then MnuViewOptionsClick(Sender)
  else
    MnuReportOptionsClick(Sender);
end;


procedure TMainForm.TBDeleteClick(Sender: TObject);
//---------------------------------------------------------
// OnClick handler for Delete toolbar button -
// same as clicking Browser's Delete button.
//---------------------------------------------------------
begin
  with BrowserForm do
    if BtnDelete.Enabled then BtnDeleteClick(Sender);
end;


procedure TMainForm.ToolButton13Click(Sender: TObject);
//------------------------------------------------------
// OnClick handler for Full Extent button on Map Toolbar
//------------------------------------------------------
begin
  MapActionClick(MnuFullExtent);
end;


procedure TMainForm.ToolButton1Click(Sender: TObject);
//----------------------------------------------------
// OnClick handler for Map toolbar buttons -
// invokes ToolButtonClick procedure on Map Form.
// Tag property of ToolButton stores button code:
// ToolButton  Tag   Action
// ----------  ----  -----------------------------
//    1         11   Activate Map Selection tool
//    2         12   Activate Group Selection tool
//    3         13   Activate Map Panning tool
//    4         14   Activate Map Zoom In tool
//    5         15   Activate Map Zoom Out tool
//    14        16   Activate Vertex Selection tool
//    6-12     0-6   Activate Add Object tool
//----------------------------------------------------
begin
  TToolButton(Sender).Down := True;
  with MapForm do
  begin
    Show;
    SetFocus;
    ToolButtonClick(TToolButton(Sender).Tag);
  end;
end;


procedure TMainForm.SelectorButtonClick;
//---------------------------------------------
// Activates the Select tool button on toolbar.
//---------------------------------------------
begin
  ToolButton1.Down := True;
  MapForm.ToolButtonClick(ToolButton1.Tag);
end;


procedure TMainForm.PanButtonClick;
//---------------------------------------------
// Activates the Pan tool button on toolbar.
//---------------------------------------------
begin
  ToolButton3.Down := True;
  MapForm.ToolButtonClick(ToolButton3.Tag);
end;


//===================================================================
//              Most Recently Used (MRU) File Procedures
//===================================================================

procedure TMainForm.MRUUpdate(Sender: TObject; const AddFileName: String);
//-----------------------------------------------------------------------
// Updates MRU list when new file (AddFileName) is opened.
//-----------------------------------------------------------------------
var
  Index: Integer;
begin
  Index := 0;
  while Index < (MRUList.Count - 1) do
    if AddFileName = MRUList[Index] then
      MRUList.Delete(Index)
    else
      Index := Index + 1;
  while MRUList.Count > 3 do
    MRUList.Delete(MRUList.Count - 1);
  while MRUList.Count < 3 do
    MRUList.Add('');
  MRUList.Insert(0,AddFileName);
end;


procedure TMainForm.MRUDisplay(Sender: TObject);
//----------------------------------------------
// Displays MRU file list on File menu
//----------------------------------------------
begin
  MRU1.Caption := '&1 ' + ExtractFileName(MRUList[0]);
  MRU1.Visible := (MRUList[0] <> '');
  MRUSep.Visible := MRU1.Visible;
  MRU2.Caption := '&2 ' + ExtractFileName(MRUList[1]);
  MRU2.Visible := (MRUList[1] <> '');
  MRU3.Caption := '&3 ' + ExtractFileName(MRUList[2]);
  MRU3.Visible := (MRUList[2] <> '');
  MRU4.Caption := '&4 ' + ExtractFileName(MRUList[3]);
  MRU4.Visible := (MRUList[3] <> '');
end;


procedure TMainForm.MRUClick(Sender: TObject);
//----------------------------------------------
// OnClick handler for File|MRU File menu item -
// opens the selected file
//----------------------------------------------
var
  Index: Integer;
  Fname: String;
begin
  Index := TMenuItem(Sender).Tag;
  Fname := MRUList[Index];
  if not FileExists(Fname) then
    Uutils.MsgDlg(MSG_NO_INPUT_FILE, mtInformation, [mbOK])
  else if SaveFileDlg(Sender) <> mrCancel then
  begin
    ReadOnlyFlag := (HasAttr(Fname, faReadOnly));
    OpenFile(Sender, Fname);
  end;
end;

//===================================================================
//                    Splitter Change Bounds Procedures
//===================================================================

procedure TMainForm.SplitterCanOffset(Sender: TObject; var NewOffset: Integer;
  var Accept: Boolean);
//-------------------------------------------------------
// OnCanOffset event handler for the Splitters
//-------------------------------------------------------
begin
  if (Sender = Splitter1) then
    with Splitter1 do
    begin
      if (Left + Width + NewOffset) > self.Width - Scale96ToScreen(125) then
        NewOffset := self.Width - Left - Width - Scale96ToScreen(125)
      else if Left + NewOffset < 0 then NewOffset := -1 * Left;
    end
  else if (Sender = Splitter2) then
    with Splitter2 do
    begin
      if (Top + Height + NewOffset) > self.Height - Controlbar1.Height - Scale96ToScreen(150) then
        NewOffset := self.Height - Controlbar1.Height- Top - Height - Scale96ToScreen(150)
      else if Top + NewOffset < Controlbar1.Height + Scale96ToScreen(100) then
        NewOffset := Controlbar1.Height + Scale96ToScreen(100) - Top;
    end;
  Accept := True;
end;


//===================================================================
//                    File Open & Save Procedures
//===================================================================

procedure TMainForm.OpenTextFileDialogPreview(Sender: TObject;
  Fname: String; var S: String; var WW: Boolean);
//-------------------------------------------------------------
// OnPreview handler for the OpenTextFileDialog component.
// Overrides the normal text file preview feature for .net
// files so that project Title and Notes are displayed.
// Fname is name of file being previewed, S holds the preview
// text, and WW is TRUE if text should be word-wrapped.
//-------------------------------------------------------------
var
  FileStream: TFileStream;
  Reader    : TReader;
  Signature : String;
  i         : Integer;
begin
// Create a file stream object
  S := '';
  try
    FileStream := TFileStream.Create(Fname, fmOpenRead or fmShareExclusive);
  except
    WW := False;
    Exit;
  end;

// Create a Reader object
  try
    Reader := TReader.Create(FileStream, $ff);
    try
       with Reader do
       try

      // Check if file is an EPANET .net file
        Signature := ReadString;
        if Trim(Signature) <> '<EPANET2>' then
          raise EReadError.Create(MSG_NOT_EPANET_FILE);

      // Skip over the component counts in the file
        ReadInteger;  //Version number
        for i := JUNCS to CNTRLS do ReadInteger;

      // Start reading the project's title and notes
        S := TXT_TITLE;
        S := S + #13 + ReadString + #13 + #13 + TXT_NOTES;
        ReadListBegin;
        while not EndOfList do S := S + #13 + ReadString;
        ReadListEnd;
        S := S + #13;
      except
        S := '';
      end;

    finally
      Reader.Free;
    end;

  finally
    FileStream.Free;
  end;

// Set word-wrapping true if any title/notes text is found
  WW := Length(S) > 0;
end;


procedure TMainForm.OpenFile(Sender: TObject; const Fname: String);
//----------------------------------------------------------------
// Opens an existing network data file named Fname.
//----------------------------------------------------------------
var
  I: Integer;
begin
// Close all output display forms
  CloseForms;

// Re-set file names
  InputFileName := Fname;
  SetCurrentDir(ExtractFileDir(Fname));
  MRUUpdate(Self,InputFileName);
  Caption := TXT_MAIN_CAPTION + ' - ' + ExtractFileName(InputFileName);

// Clear all existing data
  ClearAll;
  ShowRunStatus;

// If can't read data from input file then create a new project
  InputFileType := Ufileio.OpenProject(InputFileName);
  if InputFileType = iftNone then
  begin
    MnuNewClick(Sender);
    Exit;
  end;

// Check that backdrop map named in input file actually exists
  FindBackdropFile;

// Reset printed page properties
  PageSetup;

// Update variable units & Browser's map page
  Uinput.UpdateAllUnits;
  AutoLength := False;
  ShowAutoLengthStatus;
  BrowserForm.InitMapPage;

// Initialize current item for each object category in database
  for I := JUNCS to CURVES do
  begin
    if Network.Lists[I].Count = 0 then
      CurrentItem[I] := -1
    else
      CurrentItem[I] := 0;
  end;
  BrowserForm.UpdateBrowser(JUNCS,CurrentItem[JUNCS]);

// Re-scale and redraw network & overview map
  MapForm.Map.Options := MapOptions;
  MapForm.Map.Rescale(MapDimensions);
  MapForm.OpenMapBackdrop;
  RefreshMapForm;
  OVMapForm.Rescale;
  MapForm.SetFocus;
  FormActive := TXT_MAP;
end;


procedure TMainForm.FindBackdropFile;
//-----------------------------------------------------
// Let's user search for backdrop file
//-----------------------------------------------------
begin
  if  (Length(MapBackdrop.Filename) > 0)
  and (not FileExists(MapBackdrop.Filename))then
  begin
    if Uutils.MsgDlg(Format(MSG_FIND_BACKDROP, [MapBackdrop.Filename]),
    mtError, [mbYes,mbNo]) = mrYes then
    begin
      with OpenPictureDialog do
      begin
        Filename := ExtractFileName(MapBackdrop.Filename);
        if Execute then MapBackdrop.Filename := Filename
        else MapBackdrop := DefMapBackdrop;
      end;
    end
    else MapBackdrop := DefMapBackdrop;
  end;
end;

function TMainForm.SaveFileDlg(Sender: TObject): Integer;
//-----------------------------------------------------
// Checks if user wants to save current project to file
//-----------------------------------------------------
begin
// Have user confirm the save operation
  if not ReadOnlyFlag and HasChanged then
  begin
    Result := Uutils.MsgDlg(TXT_SAVE_CHANGES,mtConfirmation,mbYesNoCancel, self);
    if Result = mrYes then MnuSaveClick(Sender);
  end
  else Result := mrNo;
end;


procedure TMainForm.SaveFile(Fname: String);
//------------------------------------------------
// Saves project in NET format to file Fname
//------------------------------------------------
begin
// Append .net extension to file name if none exists
  if ExtractFileExt(Fname) = '' then Fname := Fname + '.net';

// Check if project file is read-only
  if ReadOnlyFlag
  and (CompareText(Fname,InputFileName) = 0)
  then Uutils.MsgDlg(Format(MSG_READONLY, [ExtractFileName(InputFileName)]),
                     mtInformation, [mbOK])

// Save project under new name
  else
  begin
    Ufileio.SaveProject(Fname);
    InputFileName := Fname;
    InputFileType := iftNET;
    Caption := Txt_MAIN_CAPTION + ' - ' + ExtractFileName(InputFileName);
    MRUUpdate(Self,InputFileName);
    HasChanged := False;
    ReadOnlyFlag := False;
  end;
end;


procedure TMainForm.ClearAll;
//---------------------------------------------
// Clears the entire network database
// (Called when File|New or File|Open selected)
//---------------------------------------------
var
  i: Integer;
begin
// Clear all output and input data
  Uoutput.ClearOutput;
  DeleteTempFiles;
  Network.Clear;

// Hide the Property Editor
  PropEditForm.Hide;
  EditorObject := -1;
  EditorIndex := -1;

// Read project defaults from the INI file
// and reset units of all display variables
  Uinifile.ReadDefaults;
  Uinput.UpdateAllUnits;
  Uinput.UpdateQualParam;
  ShowAutoLengthStatus;

// Clear the Network Map and the Browser
  MapForm.Map.Options := MapOptions;
  MapForm.ClearMap;
  BrowserForm.InitDataPage;
  BrowserForm.InitMapPage;

// Reset printed page layout
  InitPageLayout;

// Initialize current item and next ID
// number for each object category
  for i := JUNCS to CURVES do
  begin
    CurrentItem[i] := -1;
    NextID[i] := IDIncrement;
  end;

// Clear all calibration file and location information
  for i := Low(NodeCalibData) to High(NodeCalibData) do
  begin
    NodeCalibData[i].FileName := '';
    NodeCalibData[i].Locations.Clear;
  end;
  for i := Low(LinkCalibData) to High(LinkCalibData) do
  begin
    LinkCalibData[i].FileName := '';
    LinkCalibData[i].Locations.Clear;
  end;

// Reset the HasChanged flag
  HasChanged := False;
  MainForm.SelectorButtonClick;
end;


procedure TMainForm.CreateTempFiles;
//-----------------------------------------------------
// Creates temporary files that begin with letters 'en'
//-----------------------------------------------------
var
  tmp : string;
begin
  tmp  := Uutils.GetTempFile(TempDir,'en');
  TempInputFile  := ChangeFileExt(tmp,'.inp');
  TempReportFile := ChangeFileExt(tmp,'.rpt');
  TempOutputFile := ChangeFileExt(tmp,'.out');
end;


procedure TMainForm.DeleteTempFiles;
//---------------------------------
// Deletes temporary files.
//---------------------------------
begin
  SysUtils.DeleteFile((TempInputFile));
  SysUtils.DeleteFile((TempReportFile));
  SysUtils.DeleteFile((TempOutputFile));
end;


//===================================================================
//                     Form-Related Procedures
//===================================================================

function TMainForm.FormExists(const Name: String): Boolean;
//---------------------------------------------------------
// Checks if form with given name already exists
//---------------------------------------------------------
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[i].Name = Name then
    begin
      with Screen.Forms[i] do
      begin
        WindowState := wsNormal;
        Show;
        SetFocus;
      end;
      Result := True;
      Exit;
    end;
  end;
end;


procedure TMainForm.CreateGraph(GraphSelection: TGraphSelection);
//---------------------------------------------
// Creates a new graph form
//---------------------------------------------
begin
  if GraphSelection.GraphType = CONTOURPLOT then
  begin
    with TContourForm.Create(self) do
    try
      if CreateContourPlot(GraphSelection) then Show
      else Close;
    finally
    end;
  end
  else
  begin
    with TGraphForm.Create(self) do
    try
      if CreateGraph(GraphSelection) then Show
      else Close;
    finally
    end;
  end;
end;


procedure TMainForm.CloseForms;
//----------------------------------------------
// Closes all forms (except for Map and Browser)
//----------------------------------------------
var
  I : Integer;
begin
// Close MDI child forms
   for I := Screen.FormCount - 1 downto 0 do
   begin
     if (Screen.Forms[I] is TGraphForm)
     or (Screen.Forms[I] is TTableForm)
     or (Screen.Forms[I] is TContourForm)
     or (Screen.Forms[I] is TStatusForm)
     or (Screen.Forms[I] is TCalibReportForm)
     or (Screen.Forms[I] is TEnergyForm)
     then
     begin;
     Screen.Forms[I].Close;
     end;
   end;

// Hide overview map if visible
  if MnuOVMap.Checked then
  begin
    MnuOVMap.Checked := False;
    OVMapForm.Hide;
  end;

{*** Updated 8/12/00 ***}
// Hide Map Query and Find forms if visible
  with QueryForm do
  begin
    Close;
    Clear;
  end;
  with FindForm do
  begin
    Close;
    Clear;
  end;

// Remove Graph selection dialog if visible
  if FormExists('GraphSelectForm') then
    with Screen.ActiveForm as TGraphSelectForm do Close;
end;


procedure TMainForm.RefreshMapForm;
//--------------------------------------------------------
// Refreshes Map form after new network data is retreived.
//--------------------------------------------------------
begin
  Uoutput.SetNodeColors;
  Uoutput.SetLinkColors;
  MapForm.RedrawMap;
end;


procedure TMainForm.RefreshForms;
//-----------------------------------------------
// Refreshes all open output display forms
// after new analysis is made.
//-----------------------------------------------
var
  I: Integer;
begin
   for I := Screen.FormCount - 1 downto 0 do
   begin
     if (Screen.Forms[I] is TGraphForm) then
       with Screen.Forms[I] as TGraphForm do RefreshGraph;
     if (Screen.Forms[I] is TContourForm) then
       with Screen.Forms[I] as TContourForm do RefreshContourPlot;
     if (Screen.Forms[I] is TTableForm) then
       with Screen.Forms[I] as TTableForm do RefreshTable;
     if (Screen.Forms[I] is TStatusForm) then
       with Screen.Forms[I] as TStatusForm do RefreshStatusReport;
     if (Screen.Forms[I] is TCalibReportForm) then
       with Screen.Forms[I] as TCalibReportForm do RefreshCalibReport;
     if (Screen.Forms[I] is TEnergyForm) then
       with Screen.Forms[I] as TEnergyForm do RefreshEnergyReport;
   end;
end;


procedure TMainForm.InitPageLayout;
//---------------------------------
// Initializes printed page layout
//---------------------------------
begin
  with PageLayout do
  begin
    LMargin := 1.0;
    TMargin := 1.5;
    RMargin := 1.0;
    BMargin := 1.0;
  end;
  with PageSetupDialog do
  begin
    Header.Text := '';
    Header.Alignment := taCenter;
    Header.Enabled := True;
    Footer.Text := TXT_MAIN_CAPTION;
    Footer.Alignment := taLeftJustify;
    Footer.Enabled := True;
    PageNumbers := pnLowerRight;
  end;
  TitleAsHeader := True;
  Orientation := Ord(poPortrait);
end;


procedure TMainForm.PageSetup;
//-----------------------------------------------------------------
// Transfers page margins & header/footer options to Printer object
//-----------------------------------------------------------------
var
  Y: Single;
  Justify: TJustify;
begin

  if Printer.Printers.Count > 0 then with thePrinter do
  begin
  // Set printer orientation
    SetOrientation(TPrinterOrientation(Orientation));

  // Set page margines
    with PageLayout do
      SetMargins(TMargin,BMargin,LMargin,RMargin);

    with PageSetupDialog do
    begin
    // Define header line (0.5 inches above top margin)
      Justify := TJustify(Ord(Header.Alignment));
      SetHeaderInformation(1,PageLayout.TMargin-0.5,Header.Text,Justify,
        'Arial',14,[fsBold]);
      SetHeaders(Header.Enabled);

    // Define footer line (0.5 inches from bottom of page)
      Justify := TJustify(Ord(Footer.Alignment));
      SetFooterInformation(1,GetPageHeight-0.5,Footer.Text,Justify,
        'Arial',10,[fsBold, fsItalic]);
      SetFooters(Footer.Enabled);

    // Set page number location
      Justify := jRight;
      if PageNumbers in [pnUpperLeft, pnLowerLeft] then Justify := jLeft;
      if PageNumbers in [pnUpperCenter, pnLowerCenter] then Justify := jCenter;
      Y := 0.5;
      if PageNumbers in [pnLowerLeft, pnLowerCenter, pnLowerRight] then
        Y := GetPageHeight-0.5;
      SetPageNumberInformation(Y, TXT_PAGE,Justify,'Arial',10,[]);
      SetPageNumbers(not (PageNumbers = pnNone));
    end;
  end;
end;


//===================================================================
//                       Progress Bar Procedures
//===================================================================

procedure TMainForm.ShowProgressBar(const Msg: String);
//-----------------------------------------------------
// Activates the ProgressBar by hiding the StatusBar
// panel and making the ProgressBar panel visible
//-----------------------------------------------------
var
  W: Integer;
begin
// Display message on ProgressBar's panel
  W := Canvas.TextWidth(Msg) + 2;
  ProgressPanel.Caption := Msg;

// Position ProgressBar to right of message
  ProgressBar.Left := W;
  ProgressBar.Position := 0;

// Switch visibility of StatusBar and ProgressBar panels
  StatusPanel.Visible := False;
  ProgressPanel.Visible := True;
  ProgressPanel.Update;
end;


procedure TMainForm.HideProgressBar;
//------------------------------------------------
// Hides the ProgressBar by switching visibility
// of ProgressBar panel and StatusBar panel
//------------------------------------------------
begin
  ProgressPanel.Visible := False;
  StatusPanel.Visible := True;
end;


procedure TMainForm.UpdateProgressBar(var Count: Integer;
            const StepSize: Integer);
//--------------------------------------------------------
// Updates display of ProgressBar's meter
//--------------------------------------------------------
begin
  Inc(Count);
  if Count >= StepSize then
  begin
    Count := 0;
    ProgressBar.StepIt;
  end;
end;


//===================================================================
//                       Run Simulation Procedure
//===================================================================


procedure TMainForm.RunSimulation;
//--------------------------------
// Runs an EPANET simulation
//--------------------------------
begin
// Clear all previous results
  Uoutput.ClearOutput;
  BrowserForm.InitMapPage;
  RunStatus := rsNone;
  ShowRunStatus;

// Create temporary files
  DeleteTempFiles;
  CreateTempFiles;

// Display simulation dialog form
  with TSimulationForm.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;

// Delete temporary files if run ended prematurely
  if (RunStatus = rsShutdown)               //Fatal error in solver DLL
  or (RunStatus = rsCancelled) then         //User cancelled run
  begin
    DeleteTempFiles;
  end;

// Set RunFlag if run produced results
  if RunStatus in [rsSuccess, rsWarning] then
    RunFlag := True;
  if RunStatus = rsError then MnuReportStatusClick(Self);

// Retrieve results if run was successful
  if RunFlag then
  begin
    Screen.Cursor := crHourGlass;
    ShowRunStatus;
    Uoutput.GetBasicOutput;
    BrowserForm.EnableTimeControls;
    Screen.Cursor := crDefault;
  end;

// Set pressure and flow as default parameters in map, if none
  if BrowserForm.NodeViewBox.ItemIndex <= 0 then
  begin
     BrowserForm.NodeViewBox.ItemIndex:= 6;
     BrowserForm.NodeViewBoxChange(BrowserForm);
  end;
  if BrowserForm.LinkViewBox.ItemIndex <= 0 then
  begin
     BrowserForm.LinkViewBox.ItemIndex:= 6;
     BrowserForm.LinkViewBoxChange(BrowserForm);
  end;

// Refresh map display and all existing output display forms
  BrowserForm.RefreshMap;
  MapForm.DrawNodeLegend;
  MapForm.DrawLinkLegend;
  RefreshForms;

// Display any warning messages in Status Report
  if RunStatus = rsWarning then
  begin
    MnuReportStatusClick(Self);
    if Screen.ActiveForm is TStatusForm then
      TStatusForm(Screen.ActiveForm).SelectText(TXT_WARNING);
  end;
end;


//===================================================================
//                       Status Bar Procedures
//===================================================================

procedure TMainForm.ShowAutoLengthStatus;
//--------------------------------------------------
// Displays cuurent value of AutoLength in StatusBar
//--------------------------------------------------
begin
  StatusBarPanel5.Caption := Format(TXT_AUTOLENGTH,
                                    [AutoLengthStatus[Ord(AutoLength)]]);
end;


procedure TMainForm.ShowRunStatus;
//-------------------------------------------------------
// Displays analysis success or failure icon in StatusBar
//-------------------------------------------------------
begin
  RunStatusBox.Refresh;
end;


procedure TMainForm.RunStatusBoxPaint(Sender: TObject);
//--------------------------------------------------------
// OnPaint handler for PaintBox in StatusBar that displays
// icon showing analysis success or failure (the two icons
// are stored in the ImageList control).
//--------------------------------------------------------
var
  index: Integer;
begin
  if not RunFlag then index := 0
  else if not UpdateFlag then index := 1
  else index := 2;
  ImageList.Draw(RunStatusBox.Canvas,0,0,index);
end;

procedure TMainForm.SetChangeFlags;
//-------------------------------------------------------
// Updates change flags after a change made to project.
// HasChanged: True if database has changed
// UpdateFlag: True if analysis results need updating
//-------------------------------------------------------
begin
  HasChanged := True;
  if RunFlag and not UpdateFlag then
  begin
    UpdateFlag := True;
    ShowRunStatus;
  end;
end;


//===================================================================
//                     Help System Procedures
//===================================================================
procedure TMainForm.MnuHelpTopicsClick(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  HtmlHelp.HtmlHelp(GetDesktopWindow, PChar(Application.HelpFile), HH_DISPLAY_TOC, 0);
  {$ELSE}
  LaunchHelp(100);
  {$ENDIF}
end;

procedure TMainForm.MnuHelpUnitsClick(Sender: TObject);
begin
  if UnitSystem = usUS then
    LaunchHelp(293)
  else
    LaunchHelp(308);
end;

procedure TMainForm.MnuHelpTutorialClick(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  HtmlHelp.HtmlHelp(GetDesktopWindow, PChar(Tutorial), HH_DISPLAY_TOC, 0);
  {$ELSE}
    {$IFDEF DARWIN}
    LaunchHelp(1000);
    {$ELSE}
    ShowHelpOrErrorForKeyword(HTMLHelpDatabase2.ID, 'tut/'+'epanet_tutorial.htm');
    {$ENDIF}
  {$ENDIF}
end;

procedure TMainForm.LaunchHelp(index: integer);
{$IFDEF WINDOWS}
begin
  HtmlHelp.HtmlHelp(GetDesktopWindow, PChar(Application.HelpFile), HH_HELP_CONTEXT, index)
{$ELSE}
var
  i: integer;

begin
  for i := 0 to (length(HelpID) - 1) do
  begin
    if HelpID[i] = index then break;
  end;
  {$IFDEF DARWIN}
  NSHelpManager.sharedHelpManager.openHelpAnchor_inBook(NSSTR(HelpPage[i]),NSSTR('Epanet Help'));
  {$ELSE}
  ShowHelpOrErrorForKeyword(HTMLHelpDatabase1.ID, 'hlp/'+HelpPage[i]);
  {$ENDIF}
{$ENDIF}
end;


//===================================================================
//                     Language System Procedures
//===================================================================
procedure TMainForm.LanguageSelect(Sender: TObject);
begin
  Case Language of
    0 :
    begin
      Exit;
    end;
    1 :
    begin
      {$IFDEF WINDOWS}
      SetDefaultLang('es_ES');
      Application.HelpFile := EpanetDir + 'Help\' + 'es_ES\' + HLPFILE;
      Tutorial := EpanetDir + 'Help\' + 'es_ES\' + TUTORFILE;
      {$ENDIF}
      {$IFDEF DARWIN}
      SetDefaultLang('es_ES',
                    ExtractFilePath(ExcludeTrailingPathDelimiter(EpanetDir)) +
                    'Resources/es.lproj/');
      //Application.HelpFile := ExtractFilePath(ExcludeTrailingPathDelimiter(EpanetDir)) +
      //'Resources/epanet2w.help/Contents/Resources/es.lproj/';
      //Tutorial := Application.HelpFile + 'Tutorial/';
      {$ENDIF}
      {$IFDEF LINUX}
      SetDefaultLang('es_ES');
      Application.HelpFile := EpanetDir + 'Help/' + 'es_ES/' + HLPFILE;
      Tutorial := EpanetDir + 'Help/' + 'es_ES/' + TUTORFILE;
      {$ENDIF}
    end;
  end;
  Network.Lists[CNTRLS].Clear;
  Network.Lists[OPTS].Clear;
  Network.Lists[CNTRLS].AddStrings(ControlCategories);
  Network.Lists[OPTS].AddStrings(OptionCategories);
  PropEditForm.Editor.ColHeading1 := TXT_PROPERTY;
  PropEditForm.Editor.ColHeading2 := TXT_VALUE;
end;

function TMainForm.GetActiveChildForm: Integer;
var
  i : integer;
begin
  for i := Screen.FormCount - 1 downto 0 do
  begin
    if Screen.Forms[i].Caption = FormActive then
    begin
      Result := i;
      exit;
    end;
  end;
end;

procedure TMainForm.CreateDialogs(Sender: TObject);
begin
  OpenTextFileDialog := TOpenTxtFileDialog.Create(self);
  OpenTextFileDialog.Options := [ofHideReadOnly, ofFileMustExist];
  OpenTextFileDialog.OnPreview := OpenTextFileDialogPreview;
  OpenTextFileDialog.ShowPreview := True;
  OpenTextFileDialog.WordWrap := False;

  PageSetupDialog := TPageSetupDialogEx.Create(self);
  PageSetupDialog.BoldFont := False;
  PageSetupDialog.HelpContext := 0;

  thePrinter:= TPrintControl.Create(self);
end;

{$IFDEF DARWIN}
procedure TMainForm.MnuAppClick(Sender: TObject);
var
  mnMain: NSMenu;
  mnApp : NSMenu;
  mnItem: NSMenuItem;
  i     : integer;
  s1, s2, s3, s4, s5 : String;

begin
  Case Language of
    0 : Exit;
    1 :
    begin
      s1 := 'Servicios';
      s2 := 'Ocultar otros';
      s3 := 'Mostrar todo';
      s4 := 'Ocultar ' + Application.Title;
      s5 := 'Salir de ' + Application.Title;
    end;
  end;

  mnMain := NSApplication(NSApp).Mainmenu;
  mnItem := NSMenuItem (mnMain.itemArray.objectAtIndex(0));

  if (mnItem.hasSubmenu) then
  begin
    mnApp := mnItem.submenu;
    for i:=0 to mnApp.itemArray.count-1 do
    begin
      mnItem := NSMenuItem(mnApp.itemArray.objectAtIndex(i));
      if (NOT mnItem.isSeparatorItem) then
      begin
        if (mnItem.title.UTF8String = 'Services') then
          mnItem.setTitle (NSString.stringWithUTF8string(PChar(s1)));
        if (mnItem.title.UTF8String = 'Hide Others') then
          mnItem.setTitle (NSString.stringWithUTF8string(PChar(s2)));
        if (mnItem.title.UTF8String = 'Show All') then
          mnItem.setTitle (NSString.stringWithUTF8string(PChar(s3)));
        if (mnItem.title.UTF8String = 'Hide ' + Application.Title) then
          mnItem.setTitle (NSString.stringWithUTF8string(PChar(s4)));
        if (mnItem.title.UTF8String = 'Quit ' + Application.Title) then
          mnItem.setTitle (NSString.stringWithUTF8string(PChar(s5)));
      end;
    end;
  end;
end;
{$ENDIF}

end.
