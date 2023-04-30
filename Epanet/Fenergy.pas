unit Fenergy;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{-------------------------------------------------------------------}
{                    Unit:    Fenergy.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                    Ported to Lazarus by: Zorba Lopez Rivera       }
{                    Date:    15/05/23                              }
{                                                                   }
{   MDI child form that displays an Energy Report summarizing       }
{   the energy usage for each pump in the network.                  }
{                                                                   }
{   The form contains a PageControl where Tabsheet1 displays        }
{   tabular statistics in a StringGrid while Tabsheet2 displays     }
{   a graphical comparison of a particular statistic in barchart    }
{   form. The choice of statistic to compare is selected from a     }
{   RadioGroup.                                                     }
{-------------------------------------------------------------------}

interface

uses
{$IFDEF WINDOWS}
  Windows, TADrawerWMF, Messages, LConvEncoding,
{$ELSE}
  LCLIntf, LCLType, LMessages, FPVectorial, TADrawerSVG,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Grids,
  ComCtrls, ExtCtrls, StdCtrls, Clipbrd, TAGraph, TASeries, Types,
  System.UITypes, Xprinter, Uglobals, Uutils, TAChartUtils,
  TACustomSource, ResourceStrings;

const
  ColHeading1: array[0..6] of String =
    (' ', TXT_PERCENT, TXT_AVERAGE, TXT_KW_HR, TXT_AVERAGE, TXT_PEAK, TXT_COST);
  ColHeading2: array[0..6] of String =
    (TXT__PUMP, TXT_UTILIZATION, TXT_EFFICIENCY, ' ', TXT_KW, TXT_KW, TXT_perDAY);

type
  TEnergyForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    StringGrid1: TStringGrid;
    RadioGroup1: TRadioGroup;
    Chart1: TChart;
    Series1: TBarSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; vCol, vRow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Chart1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  private
    { Private declarations }
    ChartType: Integer;
    procedure CopyToBitmap(const Fname: String);
    procedure CopyToMetafile(const Fname: String);
    procedure CopyToString(const Fname: String);
    procedure RefreshTable;
    procedure RefreshChart;

  public
    { Public declarations }
    procedure CopyTo;
    procedure Print(Destination: TDestination);
    procedure RefreshEnergyReport;
  end;


implementation

{$R *.lfm}

uses Dcopy, Fmain, Uoutput, Dchart;

procedure TEnergyForm.FormCreate(Sender: TObject);
//------------------------------------------------
// Form's OnCreate handler.
//------------------------------------------------
begin
  //Lazarus - Set Radiogroup caption
  RadioGroup1.Items.Clear;
  RadioGroup1.Items.AddStrings(EnerGraph);

// Set form's font size
  Uglobals.SetFont(self);

// Set dimensions of table's grid
  with StringGrid1 do
  begin

  // Adjust row heights to accomodate font. Notice use of
  // minus sign on Font.Height to conform to Delphi convention.
    RowHeights[0] := DefaultRowHeight + (-Font.Height);

  // Adjust width of grid's columns
    ColCount := 7;
    DefaultColWidth := 72;
    ColWidths[0] := 112;
  end;

// Set initial page to display
  PageControl1.ActivePage := TabSheet1;
  RadioGroup1.ItemIndex := 2;
  MainForm.AddMDIMnu(self);
end;


procedure TEnergyForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F1) then MainForm.LaunchHelp(268);
end;


procedure TEnergyForm.FormClose(Sender: TObject; var Action: TCloseAction);
//----------------------------------------
// Form's OnClose handler.
// Frees all memory associated with form.
//----------------------------------------
begin
  MainForm.DeleteMDIMnu(self);
  MainForm.MapForm.FormActivate(self);
  Action := caFree;
end;


procedure TEnergyForm.FormActivate(Sender: TObject);
//--------------------------------------------------
// Form's OnActivate handler.
// Disables Options speedbutton on Mainform.
//--------------------------------------------------
begin
  MainForm.TBOptions.Enabled := False;
  MainForm.FormActive:= Caption;
  MainForm.FormActivated(self);
  StringGrid1.Refresh;
end;


procedure TEnergyForm.StringGrid1DrawCell(Sender: TObject; vCol,
  vRow: Integer; Rect: TRect; State: TGridDrawState);
//--------------------------------------------------------
// OnDrawCell handler for StringGrid1.
// Provides customized display of entries in Energy table.
//--------------------------------------------------------
var
  s : String;
  Buff: array[0..255] of Char;
  {$IFNDEF WINDOWS} ts: TTextStyle; {$ENDIF}

begin
  with Sender as TStringGrid do
  begin
  // Draw column headings in row 0
    if vRow = 0 then
    begin

    //Use Win API DrawText function to enable word-wraping
      {$IFDEF WINDOWS}
      s := UTF8ToCP1252(ColHeading1[vCol]) + LineEnding + UTF8ToCP1252(ColHeading2[vCol]);
      {$ELSE}
      s := ColHeading1[vCol] + LineEnding + ColHeading2[vCol];
      {$ENDIF}
      StrPCopy(Buff, s);
      if vCol = 0 then
        DrawText(Canvas.Handle, Buff, StrLen(Buff), Rect,
               DT_LEFT OR DT_VCENTER OR DT_WORDBREAK)
      else
        DrawText(Canvas.Handle, Buff, StrLen(Buff), Rect,
               DT_CENTER OR DT_VCENTER OR DT_WORDBREAK);
    end
  // Draw cell in leftmost column
    else if vCol = 0 then
    begin
      Canvas.FillRect(Rect);
      {$IFDEF WINDOWS}
      SetTextAlign(Canvas.Handle, TA_LEFT);
            Canvas.TextOut(Rect.Left+2,Rect.Top+2,Cells[vCol,vRow]);
      {$ELSE}
      ts := Canvas.TextStyle;
      ts.Alignment := taLeftJustify;
      ts.Layout := tlCenter;
      Canvas.TextRect(Rect, 2, 2, Cells[vCol,vRow], ts);
      {$ENDIF}
    end

  // Draw cell value for body of table
    else
    begin
      Canvas.FillRect(Rect);
      {$IFDEF WINDOWS}
      SetTextAlign(Canvas.Handle, TA_RIGHT);
      Canvas.TextOut(Rect.Right-2,Rect.Top+2,Cells[vCol,vRow]);
      {$ELSE}
      Rect.Right := Rect.Right - 2;
      ts := Canvas.TextStyle;
      ts.Alignment := taRightJustify;
      ts.Layout := tlCenter;
      Canvas.TextRect(Rect, 2, 2, Cells[vCol,vRow], ts);
      Rect.Right := Rect.Right + 2;
      {$ENDIF}
    end;
  end;
end;


procedure TEnergyForm.RadioGroup1Click(Sender: TObject);
//-------------------------------------------------------
// OnClick handler for RadioGroup1.
// Redraws chart with newly selected variable.
//-------------------------------------------------------
begin
  RefreshChart;
end;


procedure TEnergyForm.Chart1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//------------------------------------------------------------------
// OnMouseDown event handler for Chart1.
// Invokes Chart Options dialog when right button clicked on chart.
//------------------------------------------------------------------
var
  default: Boolean;
  startPage: Integer;
begin
  if (Button = mbRight) then
  begin
    default := False;
    startPage := 0;
    Dchart.Execute(self, Chart1, startPage, default);
  end;
end;


procedure TEnergyForm.RefreshEnergyReport;
//-----------------------------------------------
// Refreshes report after a new analysis is made.
//-----------------------------------------------
begin
  RefreshTable;
  RefreshChart;
end;


procedure TEnergyForm.RefreshTable;
//---------------------------------------
// Refreshes table page of Energy Report.
//---------------------------------------
var
  i, j, k: Integer;
  x: array[0..5] of Single;
  Dcharge: Single;
  Csum: Single;

begin
// Get flow volume units of Kw-hr per unit of flow
// (Mil. gal. for US units, cubic meters for SI)
  if (UnitSystem = usSI) then
    ColHeading2[3] := TXT_perM3
  else
    ColHeading2[3] := TXT_perMGAL;
  RadioGroup1.Items[2] := ColHeading1[3] + ColHeading2[3];

// Table has one row for each pump plus rows for
// Headings, Demand Charge & Total Cost
  Csum := 0;
  StringGrid1.RowCount := Network.Lists[PUMPS].Count + 3;

// Examine each pump
  for j := 0 to Network.Lists[PUMPS].Count-1 do
  begin

  // Column 0 contains pump ID
    StringGrid1.Cells[0,j+1] := GetID(PUMPS,j);

  // If no energy usage results available then fill cells with N/A
    k := Link(PUMPS,j).Zindex;
    if (k < 0) then for i := 1 to 6 do
      StringGrid1.Cells[i,j+1] := NA

  // Otherwise retrieve energy results in array x
    else
    begin
      Uoutput.GetPumpEnergy(k,x,Dcharge);
      with StringGrid1 do
      begin

      // Fill cells with string format of numerical values
        for i := 1 to 6 do
          Cells[i,j+1] := FloatToStrF(x[i-1], ffFixed, 7, 2);

      // Accumulate total pumping cost
        Csum := Csum + x[5];
      end;
    end;
  end;

// Display total cost & demand charge
  with StringGrid1 do
  begin
    Cells[0,RowCount-2] := TXT_TOTAL_COST;
    Cells[0,RowCount-1] := TXT_DEMAND_CHARGE;
    for i := 1 to 5 do
    begin
      Cells[i,RowCount-2] := '';
      Cells[i,RowCount-1] := '';
    end;
    Cells[6,RowCount-2] := FloatToStrF(Csum, ffFixed, 7, 2);
    Cells[6,RowCount-1] := FloatToStrF(Dcharge, ffFixed, 7, 2);
    Row := 1;
  end;
  StringGrid1.Refresh;
end;


procedure TEnergyForm.RefreshChart;
//---------------------------------------
// Refreshes chart page of Energy Report.
//---------------------------------------
var
  r: Integer;
  y: Single;
  u: Single;

begin
// Determine which statistic to plot
  ChartType := RadioGroup1.ItemIndex + 1;
  Chart1.Title.Text[0] := RadioGroup1.Items[ChartType-1];

// Add data to chart
  Series1.Clear;
  with Series1, StringGrid1 do
  begin
    for r := 1 to RowCount - 3 do
    begin

    //Add value to chart only if pump is utilized
      Uutils.GetSingle(Cells[1,r],u);
      if u > 0 then
        if Uutils.GetSingle(Cells[ChartType,r],y) then
          AddY(y,Cells[0,r],clDefault);
    end;

    // Lazarus - Add dummy series if only 1 pump
    if RowCount = 4 then AddY(0,'',clDefault);

  end;
  Chart1.BottomAxis.Marks.Source := Series1.Source;
  Chart1.BottomAxis.Marks.Style := smsLabel;
end;


procedure TEnergyForm.CopyTo;
//---------------------------------------------------
// Copies table or chart to file or to the Clipboard.
//---------------------------------------------------
begin
// Create the CopyTo dialog form
  with TCopyToForm.Create(self) do
  try

  // If table is showing then disable the graphic format choices
    if PageControl1.ActivePage = TabSheet1 then
    begin
      FormatGroup.ItemIndex := 2;
      FormatGroup.Enabled := False;
    end;

  // Show the dialog and retrieve name of file (DestFileName)
  // (If name is empty then graph is copied to the Clipboard)
    if ShowModal = mrOK then
    begin

    // Copy using selected format
      case FormatGroup.ItemIndex of
      0: CopyToBitmap(DestFileName);
      1: CopyToMetafile(DestFileName);
      2: CopyToString(DestFileName);
      end;
    end;
  finally
    Free;
  end;
end;


procedure TEnergyForm.CopyToBitmap(const Fname: String);
//-----------------------------------------------------
// Copies energy chart to file Fname (or to Clipboard)
// in bitmap format.
//-----------------------------------------------------
begin
  if Length(Fname) > 0 then
    Chart1.SaveToBitmapFile(Fname)
  else
    Chart1.CopyToClipboardBitmap;
end;


procedure TEnergyForm.CopyToMetafile(const Fname: String);
//-----------------------------------------------------
// Copies energy chart to file Fname (or to Clipboard)
// in enhanced metafile format.
//-----------------------------------------------------
{$IFDEF WINDOWS}
begin
  if Length(Fname) > 0 then
    Chart1.SaveToWMF(Fname)
  else
    Chart1.CopyToClipBoardMetaFile;
{$ELSE}
var
  vMs:TMemoryStream;
  ClipboardFormat: TClipboardFormat;

begin
// Copy to file if file name supplied, otherwise copy to clipboard
  if Length(Fname) > 0 then
    Chart1.SaveToSVGFile(Fname)
  else
  begin
    vMs := TMemoryStream.Create;
    Chart1.Draw(TSVGDrawer.Create(vMs, true), Rect(0, 0, Chart1.Width, Chart1.Height));
    ClipboardFormat := RegisterClipboardFormat({$IFDEF DARWIN} 'image/tiff' {$ELSE} 'image/svg+xml' {$ENDIF});
    Clipboard.SetFormat(ClipboardFormat, vMs);
    vMs.Free;
  end;
{$ENDIF}

end;


procedure TEnergyForm.CopyToString(const Fname: String);
//-------------------------------------------------------
// Copies energy table to file Fname or to Clipboard.
//-------------------------------------------------------
var
  Slist: TStringList;
  C,R  : LongInt;
  S    : String;
begin
// Create a stringlist to hold each row of grid
  Slist := TStringList.Create;
  try
    with StringGrid1 do
    begin

    // Add title to stringlist
      Slist.Add(Network.Options.Title);
      Slist.Add(Caption);
      Slist.Add(' ');

    // Add column headings to stringlist
      S := Format('%-16s',[ColHeading1[0]]);
      for C := 1 to ColCount-1 do
        S := S + #9 + Format('%-16s',[ColHeading1[C]]);
      Slist.Add(S);
      S := Format('%-16s',[ColHeading2[0]]);
      for C := 1 to ColCount-1 do
        S := S + #9 + Format('%-16s',[ColHeading2[C]]);
      Slist.Add(S);

    // Add remaining rows to stringlist
      for R := 1 to RowCount-1 do
      begin

      // Build up tab-delimited string of entry in each column
        S := Format('%-16s',[Cells[0,R]]);
        for C := 1 to ColCount-1 do
          S := S + #9 + Format('%-16s',[Cells[C,R]]);

      // Add tab-delimited string to list
        Slist.Add(S);
      end;
    end;

  // Save stringlist to file if file name supplied
    if Length(Fname) > 0 then Slist.SaveToFile(Fname)

  // Otherwise place text of stringlist onto clipboard
    else Clipboard.SetTextBuf(PChar(Slist.Text));

// Free the stringlist.
  finally
    Slist.Free;
  end;
end;


procedure TEnergyForm.Print(Destination: TDestination);
//----------------------------------------------------------------
// Prints Energy Report to Destination using thePrinter object.
//----------------------------------------------------------------

var
  j,r,n   : Integer;
  Pwidth,
  Pheight : Single;
  T,W,L,H : Single;
  aPicture: TPicture;
begin
  aPicture := TPicture.Create;
  with MainForm.thePrinter, PageLayout do
  try
  // Initiate the print job
    BeginJob;
    SetDestination(Destination);
    SetFontInformation('Times New Roman',11,[]);

  // Get width & height of printable area
    Pwidth := GetPageWidth - LMargin - RMargin;
    Pheight := GetPageHeight - TMargin - BMargin;

  // Print window caption
    PrintCenter(Caption);
    NextLine;
    NextLine;

  // Print the report's table
    if PageControl1.ActivePage = TabSheet1 then with StringGrid1 do
    begin
      CreateTable(ColCount);
      W := Pwidth/ColCount;
      if (W > 1) then W := 1;
      L := LMargin + (Pwidth - ColCount*W)/2;
      for j := 1 to ColCount do
      begin
        SetColumnHeaderText(j,1,ColHeading1[j-1]);
        SetColumnHeaderText(j,2,ColHeading2[j-1]);
        SetColumnDimensions(j,L,W);
        SetColumnHeaderAlignment(j,jCenter);
        L := L + W;
      end;
      SetColumnHeaderAlignment(1,jLeft);
      SetTableStyle([sBorder, sVerticalGrid, sHorizontalGrid]);
      n := RowCount-3;
      BeginTable;
      for r := 1 to n do
      begin
        PrintColumnLeft(1,Cells[0,r]);
        for j := 2 to ColCount do
           PrintColumnCenter(j,Cells[j-1,r]);
        NextTableRow((r >= n));
      end;
      EndTable;
      SetTableStyle([sBorder, sHorizontalGrid]);
      for r := n+1 to n+2 do
      begin
        PrintColumnLeft(1,Cells[0,r]);
        PrintColumnCenter(ColCount,Cells[ColCount-1,r]);
        NextTableRow((r >= n+2));
      end;
      EndTable;
    end

  // Print the report's chart
    else
    begin
      Uutils.ChartToPicture(Chart1, aPicture);
      Uutils.FitChartToPage(Chart1, PWidth, PHeight, W, H);
      T := GetYPos;;
      L := LMargin + (Pwidth - W)/2;
      StretchGraphic(L, T, L+W, T+H, aPicture);
    end;
    EndJob;
  finally
    aPicture.Free;
  end;
end;

end.
