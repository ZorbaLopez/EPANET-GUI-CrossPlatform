unit Fcontour;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{-------------------------------------------------------------------}
{                    Unit:    Fcontour.pas                          }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                    Ported to Lazarus by: Zorba Lopez Rivera       }
{                    Date:    15/05/23                              }
{                                                                   }
{   MDI child form that displays a contour plot of a node           }
{   variable at a specific time period superimposed on the          }
{   network map.                                                    }
{                                                                   }
{   The plot is created by calling CreateContourPlot() and is       }
{   updated by calling RefreshContourPlot().                        }
{-------------------------------------------------------------------}

interface

uses
{$IFDEF WINDOWS}
  Windows, TADrawerWMF, Messages,
{$ELSE}
  LCLIntf, LCLType, LMessages, FPVectorial, fpvutils,
{$ENDIF}
   SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   ExtCtrls, Clipbrd, System.UITypes, Types,
   Xprinter, Umap, Uglobals, Uutils, ResourceStrings;

const
  DEFNdx = 20;      {# x-grid divisions}
  DEFNdy = 20;      {# y-grid divisions}
  DEFNnear = 6;     {# nearest nodes for gridding}
  MAXPTS = 200;     {Max. nodes used for gridding}
  MAXGRID = 50;     {Max. # of grid divisions}

type
  TGridArray = array [1..MAXGRID,1..MAXGRID] of Single;
  TContourForm = class(TForm)
    LegendPanel: TPanel;
    LegendBox: TPaintBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure LegendBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LegendBoxPaint(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LegendBoxDblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    ContourVar  : Integer;      //Variable being contoured
    CVname      : String;       //Name of contour variable
    ContourFlag : Boolean;      //True after map is first created
    Zgrid       : TGridArray;   //Gridded values
    procedure CopyToBitmap(const Fname: String);
    procedure CopyToMetafile(const Fname: String);
    procedure CopyToString(const Fname: String);
    procedure DisplayPlot;
    procedure DrawContours(C: TCanvas);
    {$IFDEF WINDOWS}
    procedure DrawToMetafile(aMetafile: TMetafile);
    {$ELSE}
    procedure DrawToMetafile(aMetafile: TvVectorialPage);
    procedure DrawContoursSVG(C: TvVectorialPage);
    procedure FillTriangleSVG(const X: array of Single; const Y: array of Single;
      const Z: array of Single; C: TvVectorialPage);

    {$ENDIF}
    procedure FillTriangle(const X: array of Single;
      const Y: array of Single; const Z: array of Single;
      C: TCanvas);
    procedure LoadData(var n: Integer; var x: PSingleArray;
      var y: PSingleArray; var z: PSingleArray);
    procedure SetMapSize;
  public
    { Public declarations }
    Options  : TContourOptions;//Display options
    Dx       : Single;         //x-grid spacing
    Dy       : Single;         //y-grid spacing
    Ndx      : Integer;        //# of grid points in x-direction
    Ndy      : Integer;        //# of grid points in y-direction
    Nnear    : Integer;        //# of points for gridding neighborhood
    Cmap     : TMap;           //Contour map
    Legend   : TMapLegend;     //Map legend
    LegendFrame : TLegendFrame;//Map legend frame
    MapColor : array [0..MAXINTERVALS] of TColor;//Contour colors
    TimePeriod  : Integer;     //Time period being contoured
    procedure CopyTo;
    function  CreateContourPlot(GraphSelection: TGraphSelection): Boolean;
    procedure Print(Destination: TDestination);
    procedure RefreshContourPlot;
    procedure SetContourOptions;
  end;


implementation

{$R *.lfm}

uses
  Dcontour, Dcopy, Fmain, Fbrowser, Fmap, Uoutput;

const
  MAXNN = 20;
  HUGE  = 1.0E20;

var
  NearestPoint  : array [1..MAXNN] of Integer;
  NearestDist   : array [1..MAXNN] of Single;

  procedure GridData(const n     : Integer;
                   const nn    : Integer;
                   const ndx   : Integer;
                   const ndy   : Integer;
                   const dim   : TMapDimensions;
                   var x       : PSingleArray;
                   var y       : PSingleArray;
                   var z       : PSingleArray;
                   dx, dy      : Single;
                   var zgrid   : TGridArray); forward;


//================================================================
//                    Control Procedures
//================================================================

function TContourForm.CreateContourPlot(GraphSelection: TGraphSelection):
  Boolean;
//----------------------------------------------
// Creates a new contour plot.
//----------------------------------------------
var
  i: Integer;
begin
// Initialize plot parameters.
  Caption := TXT_CONTOUR_PLOT;
  ContourFlag := False;
  ContourVar := GraphSelection.VarType;
  TimePeriod := GraphSelection.Period;
  if ContourVar = NODEQUAL then
    CVname := MainForm.BrowserForm.NodeViewBox.Items[NODEQUAL]
  else CVname := NodeVariable[ContourVar].Name;
  Caption := Caption + CVname;
  if (NodeVariable[ContourVar].Source = vsOutput) and (Nperiods > 1) then
    Caption := Caption + TXT_AT + MainForm.BrowserForm.TimeListBox.Items[TimePeriod];
  MainForm.FormActive:= Caption;
  MainForm.FormActivated(self);

  Ndx := DEFNdx;
  Ndy := DEFNdy;
  Nnear := DEFNnear;

// Set color intervals to that of Network map
  with NodeLegend[ContourVar] do
  begin
    Legend.Nintervals := Nintervals;
    Legend.Ltype := NETNODES;
    Legend.ViewVar := ContourVar;
    for i := 1 to Nintervals do
      Legend.Intervals[i] := Intervals[i];
  end;
  for i := 0 to MAXINTERVALS do
    MapColor[i] := MapNodeColor[i];

// Refresh contour plot
  with Cmap.Dimensions do
  begin
    Dx := (UpperRight.X - LowerLeft.X)/(Ndx-1);
    Dy := (UpperRight.Y - LowerLeft.Y)/(Ndy-1);
  end;
  RefreshContourPlot;
  if (ContourFlag = False) then
    Uutils.MsgDlg(MSG_TOO_FEW_NODES, mtInformation, [mbOK]);
  Result := ContourFlag;
  MainForm.AddMDIMnu(self);
end;


procedure TContourForm.RefreshContourPlot;
//----------------------------------------------
// Re-contours data for current contour options.
//----------------------------------------------
var
  npts  : Integer;
  x,y,z : PSingleArray;
begin
// Allocate memory for contoured data
  ContourFlag := False;
  GetMem(x, (MAXPTS+1)*SizeOf(Single));
  GetMem(y, (MAXPTS+1)*SizeOf(Single));
  GetMem(z, (MAXPTS+1)*SizeOf(Single));
  try

  //Select a maximum of MAXPTS junctions for gridding.
    LoadData(npts,x,y,z);

  // If there are enough points to grid from,
  // then interpolate data to a uniform grid.
    if (npts >= Nnear) then
    begin
      Screen.Cursor := crHourGlass;
      GridData(npts,Nnear,Ndx,Ndy,Cmap.Dimensions,x,y,z,Dx,Dy,Zgrid);
      Screen.Cursor := crDefault;
      ContourFlag := True;
    end;

// Free allocated memory & display map
  finally
    FreeMem(x, (MAXPTS+1)*SizeOf(Single));
    FreeMem(y, (MAXPTS+1)*SizeOf(Single));
    FreeMem(z, (MAXPTS+1)*SizeOf(Single));
  end;
  DisplayPlot;
end;


procedure TContourForm.SetContourOptions;
//------------------------------------------------------------
// Invokes dialog form which sets contour plot display options.
//------------------------------------------------------------
begin
  with TContourOptionsForm.Create(self) do
  try
    LoadOptions(self);
    if (ShowModal = mrOK) then
    begin
      UnloadOptions;
      Self.Color := Options.BackColor;
      FormResize(Self);
    end;
  finally
    Free;
  end;
end;


//=============================================================
//                      Event Handlers
//=============================================================

procedure TContourForm.FormCreate(Sender: TObject);
//-------------------------------------------------
// OnCreate handler for form.
//-------------------------------------------------
var
  dxw, dyw, rmap : Single;
begin
// Create a map object for the Contour plot
// (The Tmap class is defined in the Umap.pas unit)
  Cmap := TMap.Create;
  Cmap.Dimensions := MainForm.MapForm.Map.Dimensions;
  Cmap.Options := MainForm.MapForm.Map.Options;
  Options := DefContourOptions;
  Color := Options.BackColor;
  ContourFlag := False;

// Adjust size of default window to match aspect ratio of map.
  with Cmap.Dimensions do
    rmap := (UpperRight.X - LowerLeft.X)/(UpperRight.Y - LowerLeft.Y);
  dxw := ClientWidth - LegendPanel.Width;
  dyw := ClientHeight;
  if (dxw/dyw) > rmap then
    ClientWidth  := Trunc(dyw*rmap) + LegendPanel.Width
  else
    ClientHeight := Trunc(dxw/rmap);

// Position legend at top right of window.
  LegendPanel.Height := MainForm.MapForm.NodeLegendPanel.Height;
  LegendPanel.Top := 0;
  LegendPanel.Left := ClientWidth - LegendPanel.Width;
  LegendPanel.Visible := True;
  LegendFrame.X := 0;
  LegendFrame.Y := 0;
  LegendFrame.Framed := False;
end;


procedure TContourForm.FormActivate(Sender: TObject);
//-------------------------------------------------------
// OnActivate handler for form.
// Enables Options speedbutton on MainForm.
//-------------------------------------------------------
begin
  MainForm.TBOptions.Enabled := True;
  MainForm.FormActive:= Caption;
  MainForm.FormActivated(self);
end;


procedure TContourForm.FormPaint(Sender: TObject);
//------------------------------------------------------
// OnPaint handler for form.
// Copies contour plot's virtual bitmap to form's canvas.
//------------------------------------------------------
begin
  Canvas.Draw(0,0,Cmap.Bitmap);
end;


procedure TContourForm.FormResize(Sender: TObject);
//-----------------------------------------------------
// OnResize handler for form.
// Clears form & re-displays contour plot at new size.
//------------------------------------------------------
begin
  if ContourFlag then
  begin
    Canvas.FillRect(ClientRect);
    DisplayPlot;
  end;
end;


procedure TContourForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
//---------------------------------------------------
// OnClose handler for form.
// Frees the form and its allocated memory.
//---------------------------------------------------
begin
  MainForm.DeleteMDIMnu(self);
  ContourFlag := False;
  Cmap.Free;
  MainForm.MapForm.FormActivate(self);
  Action := caFree;
end;


procedure TContourForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------
// OnMouseDown handler for form.
// Invokes options dialog box if right button pressed.
//-----------------------------------------------------
begin
  if Button = mbRight then SetContourOptions;
end;


procedure TContourForm.LegendBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//---------------------------------------------------------
// OnMouseDown handler for Legend box
//---------------------------------------------------------
begin
// Pressing right button invokes dialog to modify contour intervals
  if Button = mbRight then
  begin
    if Umap.EditLegend(Legend,TimePeriod,MapColor,LegendFrame.Framed)
      then DisplayPlot;
  end;
end;


procedure TContourForm.LegendBoxDblClick(Sender: TObject);
//--------------------------------------------------------
// OnDoubleClick handler for Legend Box.
// Hides legend from view.
//--------------------------------------------------------
begin
  LegendPanel.Visible := not LegendPanel.Visible;
  FormResize(Self);
end;


procedure TContourForm.LegendBoxPaint(Sender: TObject);
//-----------------------------------------------------
// OnPaint handler for Legend box.
// Causes the legend to be re-drawn.
//-----------------------------------------------------
begin
  Umap.DrawLegend(LegendBox.BoundsRect,
    LegendBox.Canvas,
    Options.BackColor,
    LegendFrame.Framed,
    MainForm.BrowserForm.NodeViewBox.Items[ContourVar],
    Legend,
    MapColor);
end;


//===================================================================
//                   Contouring Procedures
// The following 4 procedures (ScaleData, Neighbor, GridData,
// and LoadData) are used to interpolate over a uniform grid.
//===================================================================

procedure ScaleData(const n     : Integer;
                    const dim   : TMapDimensions;
                    var dx      : Single;
                    var dy      : Single;
                    var x       : PSingleArray;
                    var y       : PSingleArray);
//---------------------------------------------------------
// Scales locations of nodes to be gridded to 0-1 interval.
//---------------------------------------------------------
var
  w : Single;
  h : Single;
  d : Single;
  x0: Single;
  y0: Single;
  i : Integer;
begin
  x0 := dim.LowerLeft.X;
  y0 := dim.LowerLeft.Y;
  w := dim.UpperRight.X - x0;
  h := dim.UpperRight.Y - y0;
  d := w;
  if h > w then d := h;
  d := 1.0/d;
  for i := 1 to n do
  begin
    x^[i] := (x^[i] - x0)*d;
    y^[i] := (y^[i] - y0)*d;
  end;
  dx := dx*d;
  dy := dy*d;
end;


procedure Neighbor(const n  : Integer;
                   const nn : Integer;
                   const xg : Single;
                   const yg : Single;
                   const x  : PSingleArray;
                   const y  : PSingleArray);
//--------------------------------------------------------
// Finds nn nearest neighboring data points from x[], y[]
// to grid point (xg,yg).
//--------------------------------------------------------
var
  i,j,k        : Integer;
  d,dmax,dx,dy : Single;

begin
// Initialize nearest neighbor arrays.
  for j := 1 to nn do
  begin
    NearestDist[j] := HUGE;
    NearestPoint[j] := -1;
  end;
  dmax := HUGE;

// Examine each of n data points:
  for i := 1 to n do
  begin

  // See if current point qualifies as a neighbor.
    dx := xg - x^[i];
    d := dx*dx;
    if d < dmax then
    begin
      dy := yg - y^[i];
      d := d + dy*dy;
      if d < dmax then
      begin

      // Current point is a neighbor; find point it replaces.
        for j := 1 to nn do
        begin
          if d <= NearestDist[j] then
          begin
            if j < nn then for k := nn downto j+1 do
            begin
              NearestDist[k] := NearestDist[k-1];
              NearestPoint[k] := NearestPoint[k-1];
            end;
            NearestDist[j] := d;
            NearestPoint[j] := i;
            dmax := NearestDist[nn];
            break;   //quit the for loop on j
          end;
        end;   //next j

      end;   //end of 2nd comparison on d
    end;   //end of 1st comparison on d
  end;   //next data pt. i
end;


procedure GridData(const n     : Integer;        {# data points}
                   const nn    : Integer;        {# neighbor points}
                   const ndx   : Integer;        {# x-grid points}
                   const ndy   : Integer;        {# y-grid points}
                   const dim   : TMapDimensions; {map dimensions}
                   var x       : PSingleArray;   {x-coordinates}
                   var y       : PSingleArray;   {y-coordinates}
                   var z       : PSingleArray;   {response values}
                   dx, dy      : Single;         {grid width & height}
                   var zgrid   : TGridArray);    {gridded values}
//---------------------------------------------------
// Applies inverse distance weighting to interpolate
// z-values on a uniform x-y grid.
//---------------------------------------------------
var
  i,j,k,m    : Integer;
  xg,yg,s1,s2,t : Single;

begin
//Re-scale data to [0,1] interval
  ScaleData(n,dim,dx,dy,x,y);

//Process each row & column of grid points
  for j := 1 to ndy do
  begin
    yg := (j-1)*dy;
    for i := 1 to ndx do
    begin
      xg := (i-1)*dx;

    // Find nn nearest data pts. to (xg,yg)
    // & weight z-values of these points.
      Neighbor(n,nn,xg,yg,x,y);
      k := NearestPoint[1];
      zgrid[i,j] := z^[k];        //if grid pt. lies on data pt.
      if NearestDist[1] > 0 then
      begin
        s1 := 0.0;
        s2 := 0.0;
        for m := 1 to nn do
        begin
          t := 1.0/NearestDist[m];
          t := t*t;
          k := NearestPoint[m];
          s1 := s1 + t*z^[k];
          s2 := s2 + t;
        end;
        zgrid[i,j] := s1/s2;
      end;
    end;    //next x-level of grid
  end;   //next y-level of grid
end;


procedure TContourForm.LoadData(var n: Integer;
                                var x: PSingleArray;
                                var y: PSingleArray;
                                var z: PSingleArray);
//-----------------------------------------------------------------
// Samples up to MAXPTS junctions from network for use in gridding.
//-----------------------------------------------------------------
var
  i, m, nj  : Integer;
  pass      : Integer;
  v         : Single;
  aNode     : TNode;

begin
// m = number of junctions in each of MAXPTS groups
  n := 0;
  nj := Network.Lists[JUNCS].Count;
  m := (nj div MAXPTS) + 1;
  if nj <= Nnear then Exit;

// Sample the first junction from each group
  pass := 1;
  while (pass <= m) do
  begin
    i := (m div pass);
    while (i < nj) and (n < MAXPTS) do
    begin
      aNode := Node(JUNCS,i-1);
      if (aNode.X <> MISSING) and (aNode.Y <> MISSING) then
      begin
        v := Uoutput.GetNodeValue(ContourVar,TimePeriod,JUNCS,i-1);
        if (v <> MISSING) then
        begin
          Inc(n);
          x^[n] := aNode.X;
          y^[n] := aNode.Y;
          z^[n] := v;
        end;
      end;
      Inc(i,m);
    end;
    Inc(pass);
  end;
end;


//==============================================================
//                      Plot Display Procedures
//==============================================================

procedure TContourForm.SetMapSize;
//-------------------------------------------------------------------
// Sets contour plot size and scaling to preserve aspect ration.
//-------------------------------------------------------------------
var
  w, h: Single;
  SFx, SFy: Single;
  R: TRect;
begin
// Determine bounding rectangle for display area
   R := Rect(0,0,ClientWidth,ClientHeight);
   if LegendPanel.Visible then R.Right := R.Right - LegendPanel.Width;

// Find portion of display occupied by contour plot
  with Cmap.Window do
  begin

  // Find scaling factor that preserves aspect ratio
    with Cmap.Dimensions do
    begin
      w := UpperRight.X - LowerLeft.X;
      h := UpperRight.Y - LowerLeft.Y;
    end;
    SFx := (R.Right - R.Left) / w;
    SFy := (R.Bottom - R.Top) / h;
    if SFy < SFx then PPW := SFy
    else PPW := SFx;

  // Set pixel width & height of map window
    Pwidth := Trunc(w*PPW);
    Pheight := Trunc(h*PPW);
    MapRect := Rect(0,0,Pwidth,Pheight);

  // Resize map's bitmap drawing area
    with Cmap.Bitmap do
    begin
      Width := Pwidth;
      Height := Pheight;
    end;

  // Position plot at upper left of window
  // & legend just to right of the plot
    Poffset.X := 0;
    Poffset.Y := Pheight;
    Woffset.X := Cmap.Dimensions.LowerLeft.X;
    Woffset.Y := Cmap.Dimensions.LowerLeft.Y;
    if LegendPanel.Visible then LegendPanel.Left := Pwidth;
  end;

  //Lazarus - Resize main form width
  ClientWidth := Cmap.Bitmap.Width + LegendPanel.Width + 1;

end;


procedure TContourForm.DisplayPlot;
//--------------------------------
// Displays the contour plot.
//--------------------------------
begin
  Screen.Cursor := crHourGlass;
  SetMapSize;
  DrawContours(Cmap.Bitmap.Canvas);
  Cmap.DrawOutline(Options.LinkSize,Options.ForeColor);
  Canvas.Draw(0,0,Cmap.Bitmap);
  LegendBox.Refresh;
  Screen.Cursor := crDefault;
end;


procedure TContourForm.DrawContours(C: TCanvas);
//----------------------------------------------------
// Draws filled contours for gridded data on canvas C
//----------------------------------------------------
var
  x,y,z   : array[0..3] of Single;
  x0,y0   : Single;
  i,j     : Integer;
begin
  Uutils.Cls(C,Cmap.Window.MapRect,Options.BackColor);
  if not ContourFlag then Exit;

//Process each grid rectangle
  x0 := Cmap.Dimensions.LowerLeft.X;
  y0 := Cmap.Dimensions.LowerLeft.Y;
  for i := 1 to Ndx-1 do
  begin
    for j := 1 to Ndy-1 do
    begin

    //Process upper-left triangle
      x[1] := x0 + (i-1)*Dx;
      y[1] := y0 + (j-1)*Dy;
      z[1] := zgrid[i,j];
      x[2] := x[1];
      y[2] := y0 + j*Dy;
      z[2] := zgrid[i,j+1];
      x[3] := x0 + i*Dx;
      y[3] := y[2];
      z[3] := zgrid[i+1,j+1];
      FillTriangle(x,y,z,C);

    //Process lower-right triangle
      x[2] := x[3];
      y[2] := y[1];
      z[2] := zgrid[i+1,j];
      FillTriangle(x,y,z,C);
    end;
  end;
end;


procedure TContourForm.FillTriangle(const X: array of Single;
  const Y: array of Single; const Z: array of Single; C: TCanvas);
//----------------------------------------------------------------
// Fills polygon slice of triangle with contour level color
//----------------------------------------------------------------
var
  i, j, jstar       : Integer;
  V1, V2, V3, VT    : Integer;
  P1, P2, P3        : TPoint;
  Pfirst, Psecond,
  Pthird, Pfourth   : TPoint;
  PC1, PC2          : TPoint;
  dz31, dz32, dz21,
  zz                : Single;
  V2flag            : Boolean;
  Pcount            : Integer;
  Clevel            : Single;
  startlevel        : Single;
  deltalevel        : Single;
  nlines            : Integer;
begin
// Sort vertex points by Z-value
  V1 := 1;
  V2 := 2;
  V3 := 3;
  if Z[V2] < Z[V1] then
  begin
    VT := V1;
    V1 := V2;
    V2 := Vt;
  end;
  if Z[V3] < Z[V1] then
  begin
    VT := V1;
    V1 := V3;
    V3 := VT;
  end;
  if Z[V3] < Z[V2] then
  begin
    VT := V2;
    V2 := V3;
    V3 := VT;
  end;

// Compute Z-value differences
  dz31 := Z[V3] - Z[V1];
  dz21 := Z[V2] - Z[V1];
  dz32 := Z[V3] - Z[V2];

// Get pixel coords. of vertex points
  P1 := Point(Cmap.GetXpix(X[V1]),Cmap.GetYpix(Y[V1]));
  P2 := Point(Cmap.GetXpix(X[V2]),Cmap.GetYpix(Y[V2]));
  P3 := Point(Cmap.GetXpix(X[V3]),Cmap.GetYpix(Y[V3]));

// Make V1 the first point
  Pcount := 0;
  Pfirst := P1;
  V2Flag := False;
  if Options.Style = csFilled then nlines := 1
  else nlines := Options.NumLines;

  with Legend do
  begin
    for j := 1 to Nintervals + 1 do
    begin
      jstar := j-1;
      deltalevel := 0;
      if j = 1 then
      begin
        startlevel := 2*Intervals[1] - Intervals[2];
        if startlevel < 0 then startlevel := 0;
        deltalevel := (Intervals[1] - startlevel)/nlines;
      end
      else if j > Nintervals then
      begin
        startlevel := Intervals[Nintervals];
        if Options.Style = csFilled then deltalevel := HUGE;
      end
      else
      begin
        startlevel := Intervals[j-1];
        deltalevel := (Intervals[j] - Intervals[j-1])/nlines;
      end;
      for i := 1 to nlines do
      begin
        Clevel := startlevel + i*deltalevel;
        if Clevel >= Z[V1] then
        begin

        // Find where contour level crosses side from V1 to V3
          C.Brush.Color := MapColor[jstar];
          C.Pen.Color := MapColor[jstar];
          if dz31 = 0.0 then Pthird := P3
          else
          begin
            zz := (Clevel - Z[V1])/dz31;
            if zz > 1.0 then zz := 1.0;
            Pthird.X := P1.X + Round(zz*(P3.X-P1.X));
            Pthird.Y := P1.Y + Round(zz*(P3.Y-P1.Y));
          end;

        // If contour level <= Z[V2] then find where it crosses side V1-V2
          if Clevel <= Z[V2] then
          begin
            if dz21 = 0.0 then Pfourth := P2
            else
            begin
              zz := (Clevel - Z[V1])/dz21;
              if zz > 1.0 then zz := 1.0;
              Pfourth.X := P1.X + Round(zz*(P2.X-P1.X));
              Pfourth.Y := P1.Y + Round(zz*(P2.Y-P1.Y));
            end;

          // Draw filled polygon
            if Options.Style = csFilled then
            begin
              if Pcount = 0 then C.Polygon([Pfirst,Pthird,Pfourth])
              else C.Polygon([Pfirst,Psecond,Pthird,Pfourth]);
            end;
          end

        // Otherwise find where contour level crosses side V2-V3
          else
          begin
            if dz32 = 0.0 then Pfourth := P2
            else
            begin
              zz := (Clevel - Z[V2])/dz32;
              if zz > 1.0 then zz := 1.0;
              Pfourth.X := P2.X + Round(zz*(P3.X-P2.X));
              Pfourth.Y := P2.Y + Round(zz*(P3.Y-P2.Y));
            end;

          // Draw filled polygon
            if Options.Style = csFilled then
            begin
              if V2Flag = False then
              begin
                if Pcount = 0 then C.Polygon([Pfirst,Pthird,Pfourth,P2])
                else C.Polygon([Pfirst,Psecond,Pthird,Pfourth,P2]);
                V2Flag := True;
              end
              else
              begin
                if Pcount = 0 then C.Polygon([Pfirst,Pthird,Pfourth])
                else C.Polygon([Pfirst,Psecond,Pthird,Pfourth]);
              end;
            end;
          end;

        // Highlight the contour lines in black
          if Options.Style = csFilled then
          begin
              C.Pen.Color := clBlack;
              C.MoveTo(PC2.X,PC2.Y);
              C.LineTo(PC1.X,PC1.Y);
          end;

        // Draw contour line if called for
          if (Z[V3] >= Clevel) then
          begin
            if (Options.Style = csLines) then
            begin
              if i = nlines then C.Pen.Width := Options.LineSize
              else C.Pen.Width := 1;
              C.MoveTo(Pthird.X,Pthird.Y);
              C.LineTo(Pfourth.X,Pfourth.Y);
              C.Pen.Width := 1;
            end
            else
            begin
              PC1 := Pfourth;
              PC2 := Pthird;
            end;
          end;

        // Start next polygon
          if Clevel >= Z[V3] then exit;
          Inc(Pcount);
          Pfirst := Pfourth;
          Psecond := Pthird;
        end;
      end;
    end;
  end;
end;


//==========================================================
//                Copying & Printing Procedures
//==========================================================

procedure TContourForm.CopyTo;
//----------------------------------------------
// Launches CopyTo dialog to copy contour map
// to clipboard or to file.
//----------------------------------------------
begin
  with TCopyToForm.Create(self) do
  try
    if ShowModal = mrOK then
    begin
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


procedure TContourForm.CopyToBitmap(const Fname: String);
//---------------------------------------------
// Copies contour map in bitmap format
// to either file or clipboard.
//---------------------------------------------
var
  aBitmap: TBitmap;
begin
  aBitmap := GetFormImage;
  try
    if Length(Fname) > 0 then aBitmap.SaveToFile(Fname)
    else Clipboard.Assign(aBitmap);
  finally
    aBitmap.Free;
  end;
end;


procedure TContourForm.CopyToMetafile(const Fname: String);
//---------------------------------------------
// Copies contour map in Windows enhanced
// metafile format to either file or clipboard.
//---------------------------------------------
{$IFDEF WINDOWS}
var
  MyMetafile: TMetafile;
begin
  MyMetafile := TMetaFile.Create; //Create the metafile
  try
    DrawToMetafile(MyMetafile);     // Draw map on metafile
    if Length(Fname) > 0 then
      MyMetafile.SaveToFile(Fname)  // Save metafile to file
    else
      ClipBoard.Assign(MyMetafile); //Copy metafile to clipboard
  finally
    MyMetafile.Free;              //Free the metafile
  end;
{$ELSE}
var
  MyDoc: TvVectorialDocument;
  MyMetafile: TvVectorialPage;
  vMs:TMemoryStream;
  ClipboardFormat: TClipboardFormat;

begin
  MyDoc := TvVectorialDocument.Create;
  MyDoc.Width := ClientWidth;
  MyDoc.Height := ClientHeight;
  MyMetafile := MyDoc.AddPage(True);   // Create the metafile

  try
    DrawToMetafile(MyMetafile);               // Draw map on metafile
    if Length(Fname) > 0 then
      MyDoc.WriteToFile(Fname, vfSVG)    // Save metafile to file
    else
    begin
      vMs := TMemoryStream.Create;
      MyDoc.WriteToStream(vMs, vfSVG);     // Save metafile to stream
      ClipboardFormat := RegisterClipboardFormat({$IFDEF DARWIN} 'image/tiff' {$ELSE} 'image/svg+xml' {$ENDIF});
      Clipboard.SetFormat(ClipboardFormat, vMs);
      vMs.Free;
    end;
  finally
    MyDoc.Destroy;
  end;
{$ENDIF}
end;


procedure TContourForm.CopyToString(const Fname: String);
//-----------------------------------------------------
// Copies data for contour map in string (text) format
// to either file or clipboard.
//-----------------------------------------------------
var
  Slist: TStringList;
  I,J  : Integer;
  x0,y0: Single;
  x,y,z: Single;
  S    : String;
begin
// Create a stringlist to hold value for each grid point
  Slist := TStringList.Create;
  try

  // Add titles to stringlist
    Slist.Add(Network.Options.Title);
    Slist.Add(Caption);
    Slist.Add(' ');
    Slist.Add('X ' + #9 + 'Y ' + #9 + CVname);

  // Iterate over each grid point
    x0 := Cmap.Dimensions.LowerLeft.X;
    y0 := Cmap.Dimensions.LowerLeft.Y;
    for i := 1 to Ndx do
    begin
      x := x0 + (i-1)*Dx;
      for j := 1 to Ndy do
      begin
        y := y0 + (j-1)*Dy;
        z := zgrid[i,j];

      // Build up tab-delimited string of X, Y, & Z values
        S := FloatToStr(x) + '' + #9;
        S := S + FloatToStr(y) + #9;
        S := S + FloatToStr(z);

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


{$IFDEF WINDOWS}
procedure TContourForm.DrawToMetafile(aMetafile: TMetafile);
{$ELSE}
procedure TContourForm.DrawToMetafile(aMetafile: TvVectorialPage);
{$ENDIF}
//---------------------------------------------------------
// Draws contour map & background network map on aMetafile.
//---------------------------------------------------------
var
  {$IFDEF WINDOWS}
  aMetafileCanvas: TMetafileCanvas;
  ClippingRgn: HRGN;
  {$ENDIF}
  WidthInMM, HeightInMM: Integer;
  WidthInPixels, HeightInPixels: Integer;
  MMPerPixelHorz, MMPerPixelVert: Integer;
  DC: HDC;
  LR: TRect;

begin
//Set dimensions of metafile
  {$IFDEF WINDOWS}
  DC := CMap.Bitmap.Canvas.Handle;
  WidthInMM := GetDeviceCaps(DC, HORZSIZE);
  HeightInMM := GetDeviceCaps(DC, VERTSIZE);
  WidthInPixels := GetDeviceCaps(DC, HORZRES);
  HeightInPixels := GetDeviceCaps(DC, VERTRES);
  MMPerPixelHorz := (WidthInMM*100) div WidthInPixels;
  MMPerPixelVert := (HeightInMM*100) div HeightInPixels;

  aMetafile.MMWidth := ClientWidth*MMPerPixelHorz;
  aMetafile.MMHeight := ClientHeight*MMPerPixelVert;

//Create a canvas for the metafile & a clipping region for the canvas
  aMetafileCanvas := TMetafileCanvas.Create(aMetafile,0);
  with CMap.Window.MapRect do
   ClippingRgn := CreateRectRgn(Left,Top,Right,Bottom);

  //Make the metafile canvas the map's canvas & redraw map on it
  try
    Cmap.Canvas := TCanvas(aMetafileCanvas);
    DrawContours(Cmap.Canvas);
    Cmap.DrawOutline(Options.LinkSize,Options.ForeColor);
    if LegendPanel.Visible then
    begin
      LR := LegendPanel.BoundsRect;
      OffsetRect(LR,2,0);
      Umap.DrawLegend(LR,
                      Cmap.Canvas,
                      Options.BackColor,
                      LegendFrame.Framed,
                      MainForm.BrowserForm.NodeViewBox.Items[ContourVar],
                      Legend,
                      MapColor);
    end;

//Restore map's canvas & free the metafile canvas & clipping region
  finally
    Cmap.Canvas := Cmap.Bitmap.Canvas;
    aMetafileCanvas.Free;
    DeleteObject(ClippingRgn);
  end;

  {$ELSE}
  aMetafile.Width := ClientWidth;
  aMetafile.Height := ClientHeight;

//Make redraw map on the TvVectorialPage
  try
    DrawContoursSVG(aMetafile);
    Cmap.DrawOutlineSVG(Options.LinkSize,Options.ForeColor, aMetafile);
    if LegendPanel.Visible then
    begin
      LR := LegendPanel.BoundsRect;
      OffsetRect(LR,2,0);
      Umap.DrawLegendSVG(LR,
                      aMetafile,
                      CMap.Bitmap.Canvas,
                      Options.BackColor,
                      LegendFrame.Framed,
                      MainForm.BrowserForm.NodeViewBox.Items[ContourVar],
                      Legend,
                      MapColor);
    end;
  finally
  end;
  {$ENDIF}
end;


procedure TContourForm.Print(Destination: TDestination);
//----------------------------------------------------------------
// Prints Contour Map to Destination using thePrinter object.
//----------------------------------------------------------------
var
  MapWidth,
  MapHeight,
  FrameLeft,
  FrameTop,
  FrameWidth,
  Frameheight,
  SF            : Single;
  aPicture      : TPicture;
  {$IFNDEF WINDOWS}
  MyDoc         : TvVectorialDocument;
  MyPage        : TvVectorialPage;
  {$ENDIF}

begin
// Begin job on thePrinter's destination (printer or print preview)
  Screen.Cursor := crHourglass;        {*** Updated 12/29/00 ***}
  with MainForm.thePrinter do
  begin
    BeginJob;
    SetDestination(Destination);
    SetFontInformation('Times New Roman',11,[]);

  // Find size of display area on printed page
    with PageLayout do
    begin
      FrameTop := GetYPos;
      FrameLeft := LMargin;
      FrameWidth := GetPageWidth - LMargin - RMargin;
      FrameHeight := GetPageHeight - FrameTop - BMargin - 0.5;
    end;
    MapWidth := FrameWidth;
    MapHeight := FrameHeight;

  // Adjust bounding box to preserve aspect ratio & center it
    SF := ClientHeight/ClientWidth;
    if MapHeight/SF > MapWidth
    then MapHeight := MapWidth*SF
    else MapWidth  := MapHeight/SF;
    FrameLeft := FrameLeft + (FrameWidth - MapWidth)/2;

    {$IFDEF WINDOWS}
    // Copy map to a metafile saved to the clipboard
    CopyToMetafile('');

    // Create a Picture object to hold the clipboard metafile
    // and draw it on the printed page
    aPicture := TPicture.Create;
    try
      aPicture.Assign(Clipboard);
      StretchGraphic(FrameLeft, FrameTop, FrameLeft+MapWidth,
        FrameTop+MapHeight, aPicture);
    finally
      Clipboard.Clear;
      aPicture.Free;
    end;
    {$ELSE}
    MyDoc := TvVectorialDocument.Create;
    MyDoc.Width := ClientWidth;
    MyDoc.Height := ClientHeight;
    MyPage := MyDoc.AddPage(True);
    aPicture := TPicture.Create;
    DrawToMetafile(MyPage);

    try
      aPicture.Bitmap.Width := ClientWidth + 2;
      aPicture.Bitmap.Height := ClientHeight + 2;
      MyPage.Render(aPicture.Bitmap.Canvas, 1, 1);
      StretchGraphic(FrameLeft, FrameTop, FrameLeft+MapWidth,
          FrameTop+MapHeight, aPicture);
    finally
      aPicture.Free;
      MyDoc.Free;
    end;
    {$ENDIF}

  // Print time period label
    GoToYPos(FrameTop+MapHeight);
    NextLine;
    if GetLinesLeft > 1 then PrintCenter(MainForm.MapForm.TimeLegendPanel.Caption);
    Screen.Cursor := crDefault;
    EndJob;
  end;
end;

{$IFNDEF WINDOWS}
procedure TContourForm.DrawContoursSVG(C: TvVectorialPage);
//----------------------------------------------------
// Draws filled contours for gridded data on fpVectorial C
// and the outline of the network
//----------------------------------------------------
var
  x,y,z   : array[0..3] of Single;
  x0,y0   : Single;
  i,j     : Integer;
  Background : TvRectangle;

begin
//Add background to the file
  Background := TvRectangle.Create(C);
  Background.Brush.Color := TColorToFPColor(Options.BackColor);
  Background.Brush.Style := bsSolid;
  Background.Pen.Color := TColorToFPColor(clBlack);
  if Options.BackColor = clBlack then Background.Pen.Color := TColorToFPColor(clWhite);
  Background.X := 0;
  Background.Y := 0;
  Background.CX := ClientWidth;
  Background.CY := ClientHeight;
  C.AddEntity(Background);

  if not ContourFlag then Exit;

//Process each grid rectangle
  x0 := Cmap.Dimensions.LowerLeft.X;
  y0 := Cmap.Dimensions.LowerLeft.Y;
  for i := 1 to Ndx-1 do
  begin
    for j := 1 to Ndy-1 do
    begin

    //Process upper-left triangle
      x[1] := x0 + (i-1)*Dx;
      y[1] := y0 + (j-1)*Dy;
      z[1] := zgrid[i,j];
      x[2] := x[1];
      y[2] := y0 + j*Dy;
      z[2] := zgrid[i,j+1];
      x[3] := x0 + i*Dx;
      y[3] := y[2];
      z[3] := zgrid[i+1,j+1];
      FillTriangleSVG(x,y,z,C);

    //Process lower-right triangle
      x[2] := x[3];
      y[2] := y[1];
      z[2] := zgrid[i+1,j];
      FillTriangleSVG(x,y,z,C);
    end;
  end;
end;

procedure TContourForm.FillTriangleSVG(const X: array of Single;
  const Y: array of Single; const Z: array of Single; C: TvVectorialPage);
//----------------------------------------------------------------
// Fills polygon slice of triangle with contour level color
//----------------------------------------------------------------
var
  i, j, jstar       : Integer;
  V1, V2, V3, VT    : Integer;
  P1, P2, P3        : TPoint;
  Pfirst, Psecond,
  Pthird, Pfourth   : TPoint;
  PC1, PC2          : TPoint;
  dz31, dz32, dz21,
  zz                : Single;
  V2flag            : Boolean;
  Pcount            : Integer;
  Clevel            : Single;
  startlevel        : Single;
  deltalevel        : Single;
  nlines            : Integer;
  Triangle          : TvPolygon;
  Line              : TPath;

begin
// Sort vertex points by Z-value
  V1 := 1;
  V2 := 2;
  V3 := 3;
  if Z[V2] < Z[V1] then
  begin
    VT := V1;
    V1 := V2;
    V2 := Vt;
  end;
  if Z[V3] < Z[V1] then
  begin
    VT := V1;
    V1 := V3;
    V3 := VT;
  end;
  if Z[V3] < Z[V2] then
  begin
    VT := V2;
    V2 := V3;
    V3 := VT;
  end;

// Compute Z-value differences
  dz31 := Z[V3] - Z[V1];
  dz21 := Z[V2] - Z[V1];
  dz32 := Z[V3] - Z[V2];

// Get pixel coords. of vertex points
  with Cmap do
  begin
    P1 := Point(GetXpix(X[V1]),GetYpix(Y[V1]));
    P2 := Point(GetXpix(X[V2]),GetYpix(Y[V2]));
    P3 := Point(GetXpix(X[V3]),GetYpix(Y[V3]));
  end;

// Make V1 the first point
  Pcount := 0;
  Pfirst := P1;
  V2Flag := False;
  if Options.Style = csFilled then nlines := 1
  else nlines := Options.NumLines;

  with Legend do
  begin
    for j := 1 to Nintervals + 1 do
    begin
      jstar := j-1;
      deltalevel := 0;
      if j = 1 then
      begin
        startlevel := 2*Intervals[1] - Intervals[2];
        if startlevel < 0 then startlevel := 0;
        deltalevel := (Intervals[1] - startlevel)/nlines;
      end
      else if j > Nintervals then
      begin
        startlevel := Intervals[Nintervals];
        if Options.Style = csFilled then deltalevel := HUGE;
      end
      else
      begin
        startlevel := Intervals[j-1];
        deltalevel := (Intervals[j] - Intervals[j-1])/nlines;
      end;
      for i := 1 to nlines do
      begin
        Clevel := startlevel + i*deltalevel;
        if Clevel >= Z[V1] then
        begin

        // Find where contour level crosses side from V1 to V3
          Triangle := TvPolygon.Create(C);
          Triangle.Brush.Color := TColorToFPColor(MapColor[jstar]);
          Triangle.Brush.Style := bsSolid;
          Triangle.Pen.Color := TColorToFPColor(MapColor[jstar]);
          if dz31 = 0.0 then Pthird := P3
          else
          begin
            zz := (Clevel - Z[V1])/dz31;
            if zz > 1.0 then zz := 1.0;
            Pthird.X := P1.X + Round(zz*(P3.X-P1.X));
            Pthird.Y := P1.Y + Round(zz*(P3.Y-P1.Y));
          end;

        // If contour level <= Z[V2] then find where it crosses side V1-V2
          if Clevel <= Z[V2] then
          begin
            if dz21 = 0.0 then Pfourth := P2
            else
            begin
              zz := (Clevel - Z[V1])/dz21;
              if zz > 1.0 then zz := 1.0;
              Pfourth.X := P1.X + Round(zz*(P2.X-P1.X));
              Pfourth.Y := P1.Y + Round(zz*(P2.Y-P1.Y));
            end;

          // Draw filled polygon
            if Options.Style = csFilled then
            begin
              if Pcount = 0 then Triangle.Points := [Make3DPoint(Pfirst.X, Pfirst.Y),
                                                  Make3DPoint(Pthird.X, Pthird.Y),
                                                  Make3DPoint(Pfourth.X, Pfourth.Y)]
              else Triangle.Points := [Make3DPoint(Pfirst.X, Pfirst.Y),
                                    Make3DPoint(Psecond.X, Psecond.Y),
                                    Make3DPoint(Pthird.X, Pthird.Y),
                                    Make3DPoint(Pfourth.X, Pfourth.Y)];
            end;
          end

        // Otherwise find where contour level crosses side V2-V3
          else
          begin
            if dz32 = 0.0 then Pfourth := P2
            else
            begin
              zz := (Clevel - Z[V2])/dz32;
              if zz > 1.0 then zz := 1.0;
              Pfourth.X := P2.X + Round(zz*(P3.X-P2.X));
              Pfourth.Y := P2.Y + Round(zz*(P3.Y-P2.Y));
            end;

          // Draw filled polygon
            if Options.Style = csFilled then
            begin
              if V2Flag = False then
              begin
                if Pcount = 0 then Triangle.Points := [Make3DPoint(Pfirst.X, Pfirst.Y),
                                    Make3DPoint(Pthird.X, Pthird.Y),
                                    Make3DPoint(Pfourth.X, Pfourth.Y),
                                    Make3DPoint(P2.X, P2.Y)]
                else Triangle.Points := [Make3DPoint(Pfirst.X, Pfirst.Y),
                                    Make3DPoint(Psecond.X, Psecond.Y),
                                    Make3DPoint(Pthird.X, Pthird.Y),
                                    Make3DPoint(Pfourth.X, Pfourth.Y),
                                    Make3DPoint(P2.X, P2.Y)];

                V2Flag := True;
              end
              else
              begin
                if Pcount = 0 then Triangle.Points := [Make3DPoint(Pfirst.X, Pfirst.Y),
                                                  Make3DPoint(Pthird.X, Pthird.Y),
                                                  Make3DPoint(Pfourth.X, Pfourth.Y)]
                else Triangle.Points := [Make3DPoint(Pfirst.X, Pfirst.Y),
                                    Make3DPoint(Psecond.X, Psecond.Y),
                                    Make3DPoint(Pthird.X, Pthird.Y),
                                    Make3DPoint(Pfourth.X, Pfourth.Y)];
              end;
            end;
          end;
          C.AddEntity(Triangle);

        // Highlight the contour lines in black
          if Options.Style = csFilled then
          begin
              C.StartPath(PC2.X,PC2.Y);
              C.AddLineToPath(PC1.X,PC1.Y, TColorToFPColor(clBlack));
              C.EndPath();
          end;

        // Draw contour line if called for
          if (Z[V3] >= Clevel) then
          begin
            if (Options.Style = csLines) then
            begin
              Line := TPath.Create(C);
              Line.Pen.Color := TColorToFPColor(MapColor[jstar]);
              if i = nlines then Line.Pen.Width := Options.LineSize
              else Line.Pen.Width := 1;
              Line.AppendMoveToSegment(Pthird.X,Pthird.Y);
              Line.AppendLineToSegment(Pfourth.X, Pfourth.Y);
              C.AddEntity(Line);
            end
            else
            begin
              PC1 := Pfourth;
              PC2 := Pthird;
            end;
          end;

        // Start next polygon
          if Clevel >= Z[V3] then exit;
          Inc(Pcount);
          Pfirst := Pfourth;
          Psecond := Pthird;
        end;
      end;
    end;
  end;
end;
{$ENDIF}

end.
