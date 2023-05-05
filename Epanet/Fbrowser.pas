unit Fbrowser;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{-------------------------------------------------------------------}
{                    Unit:    Fbrowser.pas                          }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                    Ported to Lazarus by: Zorba Lopez Rivera       }
{                    Date:    15/05/23                              }
{                                                                   }
{   MDI child form that controls access to the pipe network         }
{   database and selection of variables to view on the network      }
{   map.                                                            }
{-------------------------------------------------------------------}

interface

uses
{$IFDEF WINDOWS}
  Windows, Messages,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, Grids,
  Types, System.UITypes, ResourceStrings,
  Xprinter, Uglobals, Uutils, VirtList;

const
  TimeStat: array[1..4] of string =
     (TXT_AVERAGE, TXT_MINIMUM, TXT_MAXIMUM, TXT_RANGE);

type

  { TBrowserForm1 }

  TBrowserForm1 = class(TFrame)
    ImageList1: TImageList;
    ImageList1d: TImageList;
    PageControl1: TPageControl;
      TabSheet1: TTabSheet;
        ObjectListBox: TComboBox;
        ItemListBox: TVirtualListBox;
        BtnAdd: TSpeedButton;
        BtnDelete: TSpeedButton;
        BtnEdit: TSpeedButton;
      TabSheet2: TTabSheet;
        NodeViewBox: TComboBox;
        LinkViewBox: TComboBox;
        TimeListBox: TComboBox;
        TimeScrollBar: TScrollBar;
        Label2: TLabel;
        Label3: TLabel;
    TimeLabel: TLabel;
    VCRTimer: TTimer;
    VCRrewindBtn: TSpeedButton;
    VCRBackBtn: TSpeedButton;
    VCRPauseBtn: TSpeedButton;
    VCRFwdBtn: TSpeedButton;
    VCRSpeedBar: TTrackBar;

    procedure FormCreate(Sender: TObject);
    procedure FormEnter(Sender: TObject);
    procedure ObjectListBoxChange(Sender: TObject);
    procedure ItemListBoxClick(Sender: TObject);
    procedure ItemListBoxDblClick(Sender: TObject);
    procedure ItemListBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure NodeViewBoxChange(Sender: TObject);
    procedure LinkViewBoxChange(Sender: TObject);
    procedure TimeListBoxClick(Sender: TObject);
    procedure TimeScrollBarChange(Sender: TObject);
    procedure TimeScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure VCRSpeedBarChange(Sender: TObject);
    procedure VCRBackBtnClick(Sender: TObject);
    procedure VCRFwdBtnClick(Sender: TObject);
    procedure VCRPauseBtnClick(Sender: TObject);
    procedure VCRrewindBtnClick(Sender: TObject);
    procedure VCRTimerTimer(Sender: TObject);
    procedure ItemListBoxKeyPress(Sender: TObject; var Key: Char);
    procedure ItemListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ItemListBoxGetItem(Sender: TObject; Index: Integer;
      var Value: String; var aColor: TColor);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  private
    { Private declarations }
    OldQualParam: TWaterQuality;
    FOnBtnPaint : TNotifyEvent;
    function  GetLastNodes(var N1, N2: TNode): Boolean;
    procedure RefreshTimeLegend;
    procedure UpdateVCRStatus;

  protected
    { Protected declarations }
    property OnBtnPaint: TNotifyEvent read FOnBtnPaint write FOnBtnPaint;
    procedure BtnPaint(Sender: TObject);

  public
    { Public declarations }
    procedure AddObject(const ObjType: Integer; const Index: Integer);
    procedure EnableTimeControls;
    procedure InitDataPage;
    procedure InitMapPage;
    procedure RefreshMap;
    procedure SetOptions;
    procedure UpdateBrowser(const ObjType: Integer; const Index: Integer);
    procedure UpdateQualName;

  end;


implementation

{$R *.lfm}

uses
  Dcurve, Dquery, Fmain, Fmap, Fovmap, Fproped, Uinput, Uoutput;


procedure TBrowserForm1.FormCreate(Sender: TObject);
//-------------------------------------------------
// OnCreate handler for Browser Form
//-------------------------------------------------
var
  i: Integer;
begin
//Lazarus - Assign events
  OnKeyDown := FormKeyDown;
  OnBtnPaint := BtnPaint;
  DoubleBuffered := True;

{$IFDEF DARWIN}
//Lazarus - Detect Dark mode in MacOs
  if MainForm.PropEditForm.Editor.isMacDarkMode then
  begin
    ItemListBox.Color := clGradientActiveCaption;
    BtnAdd.Images := ImageList1d;
    BtnDelete.Images := ImageList1d;
    BtnEdit.Images := ImageList1d;
    VCRrewindBtn.Images := ImageList1d;
    VCRBackBtn.Images := ImageList1d;
    VCRPauseBtn.Images := ImageList1d;
    VCRFwdBtn.Images := ImageList1d;
  end;
{$ENDIF}

//Lazarus - Add OnEnabledChanged event to buttons
  BtnAdd.AddHandlerOnEnabledChanged(OnBtnPaint, False);
  BtnDelete.AddHandlerOnEnabledChanged(OnBtnPaint, False);
  BtnEdit.AddHandlerOnEnabledChanged(OnBtnPaint, False);
  VCRrewindBtn.AddHandlerOnEnabledChanged(OnBtnPaint, False);
  VCRBackBtn.AddHandlerOnEnabledChanged(OnBtnPaint, False);
  VCRPauseBtn.AddHandlerOnEnabledChanged(OnBtnPaint, False);
  VCRFwdBtn.AddHandlerOnEnabledChanged(OnBtnPaint, False);

// Initialize animation speed
  VCRSpeedBarChange(Sender);

// Add object labels to ObjectListBox
  for i := JUNCS to OPTS do
    ObjectListBox.Items.Add(ObjectLabels[i]);
  ObjectListBox.ItemIndex := -1;

// Add view variable names to NodeViewBox & LinkViewBox
  for i := 0 to NODEVIEWS do
    NodeViewBox.Items.Add(NodeVariable[i].Name);
  for i := 0 to LINKVIEWS do
    LinkViewBox.Items.Add(LinkVariable[i].Name);
  NodeViewBox.ItemIndex := 0;
  LinkViewBox.ItemIndex := 0;
  CurrentNodeVar := NOVIEW;
  CurrentLinkVar := NOVIEW;
  OldQualParam := wqNone;

  ItemListBox.Canvas.Font.Color := clWindowText;
  PageControl1.ActivePageIndex := 0;

end;


procedure TBrowserForm1.FormEnter(Sender: TObject);
//-----------------------------------------------------------------------------
// OnEnter event handler for form. Set the Frame as Active Form.
//-----------------------------------------------------------------------------
begin
  MainForm.BrowserHeader.Color := clActiveCaption;
  MainForm.BrowserCaption.Color := clActiveCaption;
  MainForm.BrowserCaption.Font.Color := clCaptionText;
  MainForm.FormActive := TXT_MAP;
  MainForm.FormActivated(self);
end;


procedure TBrowserForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  HC: Integer;
begin
  if Key = vk_F1 then
  begin
    if PageControl1.ActivePageIndex = 0 then HC := 158 else HC := 304;
    MainForm.LaunchHelp(HC);
  end;
end;


//===================================================================
//                   Procedures for Database Page
//===================================================================

procedure TBrowserForm1.BtnAddClick(Sender: TObject);
//-----------------------------------------------
// OnClick handler for BtnAdd button -
// adds new object to network data base.
//-----------------------------------------------
var
  N1, N2: TNode;
  A: array[0..0] of TPoint;
begin
// Activate Select Object toolbar button
  FormEnter(self);
  MainForm.SelectorButtonClick;

// Add object to data base
  case CurrentList of
    JUNCS..TANKS:   Uinput.AddNode(CurrentList,MISSING,MISSING);
    PIPES..VALVES:  if GetLastNodes(N1,N2) then
                      Uinput.AddLink(CurrentList,N1,N2,A,0);
    PATTERNS:       Uinput.AddPattern;
    CURVES:         Uinput.AddCurve;
  end;

// Edit the new object
  BtnEditClick(Sender);
end;


function TBrowserForm1.GetLastNodes(var N1, N2: TNode): Boolean;
//------------------------------------------------
// Retrieves last two nodes added to the database
// (used to supply default end nodes for a new link)
//------------------------------------------------
var
  i,j: Integer;
begin
  N1 := nil;
  N2 := nil;
  for i := JUNCS to TANKS do
  begin
    for j := Network.Lists[i].Count-1 downto 0 do
    begin
      if N1 = nil then N1 := Node(i,j)
      else
      begin
        N2 := Node(i,j);
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;


procedure TBrowserForm1.BtnDeleteClick(Sender: TObject);
//-------------------------------------------------
// OnClick handler for BtnDelete button -
// deletes selected item from network database.
//-------------------------------------------------
var
  i : Integer;
begin
// Activate Select Object toolbar button
  FormEnter(self);
  MainForm.SelectorButtonClick;

// Check for group deletion
  if MainForm.MapForm.NumFencePts > 0 then
  begin
    Uinput.GroupDelete;
    Exit;
  end;

// Make sure there's an object to delete
  i := CurrentItem[CurrentList];
  if i < 0 then Exit;

// Ask for confirmation of deletion
  if ConfirmDelete then
    if Uutils.MsgDlg(TXT_DELETE_OBJECT,mtConfirmation,[mbYes,mbNo],MainForm)
      = mrNo then Exit;

// Erase visual object from map
// (which also deletes it from the database)
  if (CurrentList in [JUNCS..LABELS]) then
    MainForm.MapForm.EraseObject(CurrentList,i)

// Delete non-visual object from database.
  else
  begin
    DeleteNetworkObject(CurrentList,i);
  end;

// Update Browser controls
  ItemListBox.Count := ItemListBox.Count - 1;
  UpdateBrowser(CurrentList, CurrentItem[CurrentList]);

// Update change flags (SetChangeFlags sees if a new analysis is needed)
  if CurrentList = LABELS then HasChanged := True
  else MainForm.SetChangeFlags;
end;


procedure TBrowserForm1.BtnEditClick(Sender: TObject);
//---------------------------------------------------
// OnClick handler for BtnEdit button -
// edits currently selected object in database.
//---------------------------------------------------
var
  i : Integer;
begin
// Activate Select Object toolbar button
  FormEnter(self);
  MainForm.SelectorButtonClick;

// Make sure there's an object to edit
  i := CurrentItem[CurrentList];
  if i < 0 then Exit;

// Hide Property Editor if not applicable
  if CurrentList in [PATTERNS,CURVES,CNTRLS] then MainForm.PropEditForm.Hide;

// Use appropriate editor for selected item
  case CurrentList of

  // Use Property Editor for visual objects
  JUNCS..LABELS, OPTS:
  begin
    MainForm.PropEditForm.Show;
    Uinput.UpdateEditor(CurrentList,i);
    MainForm.PropEditForm.BringToFront;
    MainForm.PropEditForm.Editor.Edit;
  end;

  // Use specific dialog form editor for other objects
  PATTERNS: Uinput.EditPattern(i);
  CURVES:   Uinput.EditCurve(i);
  CNTRLS:   Uinput.EditControls(i);
  end;
end;


procedure TBrowserForm1.ItemListBoxClick(Sender: TObject);
//--------------------------------------------------------------
// OnClick handler for ItemListBox list box -
// makes ItemListBox selection the new selected database object
//--------------------------------------------------------------
begin
// Update Browser with selected item
  FormEnter(self);
  CurrentList := ObjectListBox.ItemIndex;
  with ItemListBox do
    if ItemIndex >= 0 then
      UpdateBrowser(CurrentList,ItemIndex);
end;


procedure TBrowserForm1.ItemListBoxDblClick(Sender: TObject);
//--------------------------------------------------------------
// OnDblClick handler for ItemListBox list box -
// makes ItemListBox selection the new selected item & edits it
//--------------------------------------------------------------
begin
// End any drag operation begun on a MouseDown action
  ItemListBox.EndDrag(False);

// Select & edit the list box item
  ItemListBoxClick(Sender);
  BtnEditClick(Sender);
end;


procedure TBrowserForm1.ItemListBoxKeyPress(Sender: TObject; var Key: Char);
//---------------------------------------------------------------------------
// OnKeyPress handler for ItemListBox - edits selected item if Enter pressed.
//---------------------------------------------------------------------------
begin
  if Key = #13 then
  begin
    ItemListBoxDblClick(Sender);
    Key := #0;
  end;
end;


procedure TBrowserForm1.ItemListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//---------------------------------------------
// OnKeyDown handler for ItemListBox control.
//---------------------------------------------
begin
  case Key of
  VK_DELETE: if CurrentList in [JUNCS..CURVES] then BtnDeleteClick(Sender);
  VK_INSERT: if CurrentList in [JUNCS..CURVES] then BtnAddClick(Sender);
  end;
end;

procedure TBrowserForm1.ItemListBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//----------------------------------------------------------
// OnMouseDown handler for ItemListBox -
// initiates a BeginDrag action (used when dragging item
// from the listbox to the Graph Selection form)
//----------------------------------------------------------
var
  i: Integer;
begin
// Activate Select Object toolbar button
  MainForm.SelectorButtonClick;

// See if Graph Selection form has been launched
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[i].Name = 'GraphSelectForm' then
    begin

    // Check that item exists in listbox under mouse pointer
      if (Button = mbLeft) then with Sender as TVirtualListBox do
      begin
        if ItemAtPos(Point(X,Y), True) >= 0 then BeginDrag(False);
      end;
      Exit;
    end;
  end;
end;

procedure TBrowserForm1.ItemListBoxGetItem(Sender: TObject; Index: Integer;
  var Value: String; var aColor: TColor);
//--------------------------------------------------
// OnGetItem procedure for ItemListBox.
// Retrieves text string associated with item Index.
//---------------------------------------------------
begin
// Check for valid item index
  Value := '';
  aColor := clVLB;
  if Application.Terminated then exit;
  if (Index < 0)
  or (Index >= Network.Lists[CurrentList].Count) then exit;

// Get database ID label of item at current index
  if CurrentList in [JUNCS..TANKS] then
  begin
    if (Node(CurrentList,Index).X = MISSING)
    or (Node(CurrentList,Index).Y = MISSING)
    then aColor := clGray;
  end;
  Value := GetID(CurrentList,Index);
end;


procedure TBrowserForm1.ObjectListBoxChange(Sender: TObject);
//-----------------------------------------------------------------
// OnChange event handler for ObjectListBox list box -
// changes entries in item listbox on selection of new object type.
//-----------------------------------------------------------------
var
  ObjType: Integer;
begin
// Activate the Select Object toolbar button
  MainForm.SelectorButtonClick;

// Save object selected in ObjectListBox
  ObjType := ObjectListBox.ItemIndex;

// Update the Browser
  UpdateBrowser(ObjType,CurrentItem[ObjType]);
  ItemListBox.SetFocus;
end;


procedure TBrowserForm1.UpdateBrowser(const ObjType: Integer;
  const Index: Integer);
//-----------------------------------------------------------------------
// Updates Browser after new object selected in the Browser or on the Map
//-----------------------------------------------------------------------
var
  flag: Boolean;
begin
// If new object type selected then update contents of ItemListBox
  if CurrentList <> ObjType then
  begin
    CurrentList := ObjType;
    ObjectListBox.ItemIndex := CurrentList;
    ItemListBox.Count := Network.Lists[CurrentList].Count;
  end;

// Select current item in ItemListBox
  CurrentItem[CurrentList] := Index;
  ItemListBox.ItemIndex := Index;

// Hide Add button for link objects if node count < 2
  BtnAdd.Enabled := True;
  if (CurrentList in [PIPES..VALVES]) then
    BtnAdd.Enabled := (Network.Lists[JUNCS].Count +
                       Network.Lists[RESERVS].Count +
                       Network.Lists[TANKS].Count >= 2);

// Hide Edit & Delete buttons if no items exist for current object
  if ItemListBox.Count = 0 then flag := False
  else flag := True;
  BtnEdit.Enabled := flag;
  BtnDelete.Enabled := flag;

// Hide Add button for Labels (they must be added via the Map Toolbar)
  if CurrentList = LABELS then BtnAdd.Enabled := False;

// Hide Add and Delete buttons for Controls & Options
// (they can only be edited).
  if CurrentList in [CNTRLS, OPTS] then
  begin
    BtnAdd.Enabled := False;
    BtnDelete.Enabled := False;
  end;

// Hide Property Editor if no item selected
  if Index < 0 then MainForm.PropEditForm.Hide;

// Update property editor and highlight map if visual object selected
  if ObjType in [JUNCS..LABELS] then
  begin
    if Index >= 0 then Uinput.UpdateEditor(ObjType,Index);
    MainForm.MapForm.ChangeHiliteObject(ObjType,Index);
  end
  else MainForm.MapForm.ChangeHiliteObject(-1,-1);
  if ObjType = OPTS then Uinput.UpdateEditor(ObjType,Index);
end;


procedure TBrowserForm1.SetOptions;
//------------------------------------------
// Edits network OPTIONS.
// Called when Project|Analysis Options
// selected from main menu.
//------------------------------------------
begin
  PageControl1.ActivePage := TabSheet1;
  UpdateBrowser(OPTS, CurrentItem[OPTS]);
  BtnEditClick(self);
end;


procedure TBrowserForm1.AddObject(const ObjType: Integer;
  const Index: Integer);
//------------------------------------------------------------
// Adds new item into an object's ItemListBox display.
//------------------------------------------------------------
begin
  ItemListBox.Count := ItemListBox.Count + 1;
  UpdateBrowser(ObjType, Index);
  MainForm.SetChangeFlags;
  if ObjType in [JUNCS..LABELS] then OVMapForm.NeedsUpdating := True;
end;


procedure TBrowserForm1.InitDataPage;
//----------------------------------------------
// Initializes data page to begin a new project.
//----------------------------------------------
begin
  CurrentList := -1;
  ObjectListBox.ItemIndex := -1;
  ItemListBox.Count := 0;
  ItemListBox.ItemIndex := -1;
  BtnAdd.Enabled := False;
  BtnDelete.Enabled := False;
  BtnEdit.Enabled := True;
end;


//================================================================
//                     Procedures for Map Page
//================================================================

procedure TBrowserForm1.InitMapPage;
//--------------------------------------------------
// Initializes map page prior to running an analysis
//--------------------------------------------------
var
  i: Integer;
begin
// Update name of WQ parameter
  UpdateQualName;

// Disable time controls
  TimeLabel.Caption := TXT_TIME;
  TimeListBox.Clear;
  TimeListBox.Color := clBtnFace;
  TimeListBox.Enabled := False;
  TimeScrollBar.Enabled := False;
  MainForm.MapForm.TimeLegendPanel.Caption := '';
  MainForm.MapForm.TimeLegendPanel.Visible := False;
  UpdateVCRStatus;
  VCRTimer.Enabled := False;

// Assign units to output view variables
  for i := 0 to NODEVIEWS do
    if NodeVariable[i].Source = vsOutput then
      NodeUnits[i].Units := BaseNodeUnits[i,UnitSystem];
  for i := 0 to LINKVIEWS do
    if LinkVariable[i].Source = vsOutput then
      LinkUnits[i].Units := BaseLinkUnits[i,UnitSystem];
  NodeUnits[DEMAND].Units := FlowUnits;
  LinkUnits[FLOW].Units := FlowUnits;
  NodeUnits[NODEQUAL].Units := QualUnits;
  LinkUnits[LINKQUAL].Units := QualUnits;

// Save current Starting Time of Day option
  StartTime := Network.Options.Data[START_TIME_INDEX];

end;


procedure TBrowserForm1.UpdateQualName;
//----------------------------------------
// Updates name of displayed WQ parameter.
//----------------------------------------
var
  i: Integer;
  s: String;
begin
// Get name of quality parameter
  if QualParam = wqNone then s := TXT_QUALITY
  else
  begin
    s := Network.Options.Data[QUAL_PARAM_INDEX];
    if s = 'Trace' then s := TXT_TRACE
    else if s = 'Age' then s := TXT_AGE
    else if s = 'Chemical' then s:= TXT_CHEMICAL;
  end;
  if QualParam = wqTrace then
    s := s + ' ' + Network.Options.Data[TRACE_NODE_INDEX];

// Update quality parameter name in Node View listbox
  with NodeViewBox do
  begin
    i := ItemIndex;
    Items[NODEQUAL] := s;
    ItemIndex := i;
  end;

// Update quality parameter name in Link View listbox
  with LinkViewBox do
  begin
    i := ItemIndex;
    Items[LINKQUAL] := s;
    ItemIndex := i;
  end;
end;


procedure TBrowserForm1.NodeViewBoxChange(Sender: TObject);
//-------------------------------------------------------
// OnChange handler for NodeViewBox -
// updates map display when node view changes.
//-------------------------------------------------------
var
  I: Integer;
begin
  Update;
  I := NodeViewBox.ItemIndex;
  if I <> CurrentNodeVar then
  begin
    CurrentNodeVar := I;
    Uoutput.SetNodeColors;
    MainForm.MapForm.RedrawMap;
    MainForm.MapForm.DrawNodeLegend;
    UpdateVCRStatus;
  end;
end;


procedure TBrowserForm1.LinkViewBoxChange(Sender: TObject);
//-------------------------------------------------------
// OnChange handler for LinkViewBox -
// updates map display when link view changes.
//-------------------------------------------------------
var
  I: Integer;
begin
  Update;
  I := LinkViewBox.ItemIndex;
  if I <> CurrentLinkVar then
  begin
    CurrentLinkVar := I;
    Uoutput.SetLinkColors;
    MainForm.MapForm.RedrawMap;
    MainForm.MapForm.DrawLinkLegend;
    UpdateVCRStatus;
  end;
end;


procedure TBrowserForm1.TimeListBoxClick(Sender: TObject);
//------------------------------------------------------
// OnClick handler for TimeListBox -
// resets curent time period & updates map when a new
// time period is selected.
//------------------------------------------------------
begin
  FormEnter(self);
  CurrentPeriod := TimeListBox.ItemIndex;
  TimeScrollBar.Position := CurrentPeriod;
  RefreshTimeLegend;
  RefreshMap;
end;


procedure TBrowserForm1.TimeScrollBarChange(Sender: TObject);
//---------------------------------------------------------
// OnChange handler for TimeScrollBar -
// updates selected item in TimeListBox.
//---------------------------------------------------------
begin
  TimeListBox.ItemIndex := TimeScrollBar.Position;
end;


procedure TBrowserForm1.TimeScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
//------------------------------------------------------
// OnScroll handler for TimeScrollBar -
// changes current time period after scrolling is done.
//------------------------------------------------------
begin
  if ScrollCode in [scLineUp, scLineDown, scPageUp,
                    scPageDown, scPosition] then
  begin
    TimeListBox.ItemIndex := ScrollPos;
    TimeListBoxClick(Sender);
  end;
end;


procedure TBrowserForm1.EnableTimeControls;
//-----------------------------------------
// Enables time period selection controls.
//-----------------------------------------
var
  n: Integer;
begin
// Add "Single Period" entry to TimeListBox if
// single period simulation was made
  TimeLabel.Caption := TXT_TIME;
  if Dur = 0 then
    TimeListBox.Items.Add(TXT_SINGLE_PERIOD)

// Add statistic label to TimeListBox if
// time statistic simulation was made
  else if TimeStatFlag > 0 then
  begin
    TimeListBox.Items.Add(TimeStat[TimeStatFlag]);
    TimeLabel.Caption := Format(TXT_STATISTIC, [TXT_TIME]);
  end

// Update TimeListBox & TimeScrollBar controls if
// extended period simulation was made
  else
  begin

  // Add time period labels to TimeListBox
    with TimeListBox.Items do
    begin
      BeginUpdate;
      for n := 0 to Nperiods-1 do
        Add(Format(TXT_HOURS, [Uutils.GetTimeString(Rstart + n*Rstep)]));
      EndUpdate;
    end;

  // Set parameters of TimeScrollBar
    TimeScrollBar.Max := (Dur - Rstart) div Rstep;
    TimeScrollBar.Position := 0;
    TimeScrollBar.Enabled := True;
  end;

// Enable the TimeListBox
  with TimeListBox do
  begin
    ItemIndex := 0;
    if Items.Count > 1 then Color := clWindow;
    Enabled := True;
  end;

// Initialize VCR settings
  UpdateVCRStatus;

// Display MapForm's Time Legend
  CurrentPeriod := 0;
  RefreshTimeLegend;
  MainForm.MapForm.TimeLegendPanel.Visible := MainForm.MapForm.PopupTimeLegend.Checked;
end;


procedure TBrowserForm1.RefreshTimeLegend;
//--------------------------------------------------------------
// Refreshes contents of Time Legend panel displayed on MapForm.
//--------------------------------------------------------------
var
  days : Double;
  aTime: TDateTime;
  hours: Single;
  stime: String;
begin
  try
    days := (Rstart + CurrentPeriod*Rstep) / 86400;
    if GetSingle(StartTime, hours) then
      stime := Uutils.GetTimeString(Round(hours*3600))
    else stime := StartTime;
    aTime := StrToTime(stime) + days;

    MainForm.MapForm.TimeLegendPanel.Caption := Format(TXT_DAY,
             [IntToStr(Trunc(days)+1), FormatDateTime('h:nn AM/PM',aTime)]);
  except
    MainForm.MapForm.TimeLegendPanel.Caption := '';
  end;
end;


procedure TBrowserForm1.RefreshMap;
//-------------------------------------------------
// Refreshes map display when time period changes.
//-------------------------------------------------
var
  redrawflag: Boolean;
begin
// If analysis results are available
  redrawflag := False;
  if RunFlag then
  begin

  // Get flow directions
    Uoutput.GetFlowDir(CurrentPeriod);

  // Update node colors for output view variable
    if (NodeVariable[CurrentNodeVar].Source = vsOutput) then
    begin
      Uoutput.SetNodeColors;
      redrawflag := True;
    end;

  // Update link colors for output view variable
    if (LinkVariable[CurrentLinkVar].Source = vsOutput) then
    begin
      Uoutput.SetLinkColors;
      redrawflag := True;
    end;

  // Set redrawflag to True if displaying flow arrows
    if  (MainForm.MapForm.Map.Options.ArrowStyle <> asNone)
    and (MapZoomRatio >= MainForm.MapForm.Map.Options.ArrowZoom)
    then redrawflag := True;
  end;

// Redraw map if required.
  if redrawflag then
  begin
    MainForm.MapForm.RedrawMap;
    if QueryFlag then QueryForm.UpdateQueryCaption;
  end;

// Update the output values displayed in the Property Editor
  Uinput.UpdateEditor(EditorObject,EditorIndex);
end;


//==============================================================
//               Animation VCR Control Procedures
//==============================================================

procedure TBrowserForm1.UpdateVCRStatus;
//------------------------------------------------------------
// Updates status of the VCR controls (used to animate the
// network map) after a new analysis has been made or a new
// node or link variable was chosen for viewing on the map.
//------------------------------------------------------------
var
  vcrEnabled: Boolean;
begin
// Controls enabled only if there is more than 1 time period
// and current view variables are computed (output) values.
  vcrEnabled := True;
  if not RunFlag then vcrEnabled := False
  else if Nperiods = 1 then vcrEnabled := False
  else if (NodeVariable[CurrentNodeVar].Source = vsInput)
  and     (LinkVariable[CurrentLinkVar].Source = vsInput)
  then    vcrEnabled := False;
  if not vcrEnabled then VCRPauseBtnClick(Self);
  vcrRewindBtn.Enabled := vcrEnabled;
  vcrBackBtn.Enabled := vcrEnabled;
  vcrPauseBtn.Enabled := vcrEnabled;
  vcrFwdBtn.Enabled := vcrEnabled;
  vcrSpeedBar.Enabled := vcrEnabled;
end;


procedure TBrowserForm1.VCRSpeedBarChange(Sender: TObject);
//--------------------------------------------------------
// OnChange handler for TTrackbar component that controls
// animation speed.
//--------------------------------------------------------
begin
  with VCRSpeedBar do
    VCRTimer.Interval := 100*(Max+1-Position);
end;


procedure TBrowserForm1.VCRBackBtnClick(Sender: TObject);
begin
  FormEnter(self);
  VCRTimer.Enabled := True;
end;


procedure TBrowserForm1.VCRFwdBtnClick(Sender: TObject);
begin
  FormEnter(self);
  VCRTimer.Enabled := True;
end;


procedure TBrowserForm1.VCRPauseBtnClick(Sender: TObject);
begin
  FormEnter(self);
  VCRTimer.Enabled := False;
  vcrBackBtn.Down := False;
  vcrFwdBtn.Down := False;
end;


procedure TBrowserForm1.VCRrewindBtnClick(Sender: TObject);
begin
  FormEnter(self);
  VCRPauseBtnClick(Sender);
  TimeListBox.ItemIndex := 0;
  TimeListBoxClick(Sender);
end;


procedure TBrowserForm1.VCRTimerTimer(Sender: TObject);
//--------------------------------------------------
// OnTimer handler for Timer component that updates
// the time period for network map animation.
//--------------------------------------------------
begin
  with TimeListBox do
  begin
    if vcrFwdBtn.Down then
    begin
      if ItemIndex = Items.Count-1 then
        ItemIndex := 0
      else ItemIndex := ItemIndex + 1;
    end
    else if vcrBackBtn.Down then
    begin
      if ItemIndex = 0 then
        ItemIndex := Items.Count - 1
      else ItemIndex := ItemIndex - 1;
    end
    else Exit;
  end;
  TimeListBoxClick(Sender);
end;


procedure TBrowserForm1.BtnPaint(Sender: TObject);
begin
  with Sender as TSpeedButton do
    {$IFDEF DARWIN}
    if (not Enabled) then
    begin
      if MainForm.PropEditForm.Editor.isMacDarkMode then Images := ImageList1
      else Images := ImageList1d
    end
    else
    begin
      if MainForm.PropEditForm.Editor.isMacDarkMode then Images := ImageList1d
      else Images := ImageList1
    end;
    {$ELSE}
    if (not Enabled) then
      Images := ImageList1d
    else
      Images := ImageList1;
    {$ENDIF}
end;

end.
