unit Dchart;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{
   Unit:    Dchart.pas
   Project: EPANET4W
   Author:  L. Rossman
   Version: 2.2
   Date:    6/24/19
   Ported to Lazarus by: Zorba Lopez Rivera
   Date:    15/05/23

   This is dialog form used to set display options for a
   TChart component.
}

interface

uses
{$IFDEF WINDOWS}
  Windows, Messages,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, Spin, ExtCtrls, ComCtrls, ColorBox, Math, StrUtils,
  System.UITypes, TAGraph, TASeries, TAChartAxis, TACustomSeries, TALegend,
  TAChartUtils, TATypes, TACustomSource, ResourceStrings, Types;

const
  LineStyleText: array[0..4] of string =
    (TXT_LINESTYLE_SOLID, TXT_LINESTYLE_DASH, TXT_LINESTYLE_DOT,
     TXT_LINESTYLE_DASHDOT, TXT_LINESTYLE_DASHDOTDOT);
  LegendPosText: array[0..7] of string =
    (TXT_LEGENDPOS_TL, TXT_LEGENDPOS_CL, TXT_LEGENDPOS_BL, TXT_LEGENDPOS_TC,
     TXT_LEGENDPOS_BC, TXT_LEGENDPOS_TR, TXT_LEGENDPOS_CR, TXT_LEGENDPOS_BR);
  MarkStyleText: array[0..19] of string =
    (TXT_MARKSTYLE_NONE, TXT_MARKSTYLE_RECT, TXT_MARKSTYLE_CIRC,
     TXT_MARKSTYLE_CROS, TXT_MARKSTYLE_DCROS, TXT_MARKSTYLE_STAR,
     TXT_MARKSTYLE_LBRAC, TXT_MARKSTYLE_HBRAC, TXT_MARKSTYLE_FBRAC,
     TXT_MARKSTYLE_RBRAC, TXT_MARKSTYLE_DIAM, TXT_MARKSTYLE_TRIA,
     TXT_MARKSTYLE_FTRIA, TXT_MARKSTYLE_RTRIA, TXT_MARKSTYLE_VBAR,
     TXT_MARKSTYLE_HBAR, TXT_MARKSTYLE_POIN, TXT_MARKSTYLE_DTRIA,
     TXT_MARKSTYLE_HEXA, TXT_MARKSTYLE_FSTAR);
  FillStyleText: array[0..7] of string =
    (TXT_FILLSTYLE_SOLID, TXT_FILLSTYLE_CLEAR, TXT_FILLSTYLE_HORIZ,
     TXT_FILLSTYLE_VERTI, TXT_FILLSTYLE_FDIAG, TXT_FILLSTYLE_BDIAG,
     TXT_FILLSTYLE_CROSS, TXT_FILLSTYLE_DCROS);
  StackStyleText: array[0..3] of string =
    (TXT_STACKSTYLE_NONE, TXT_STACKSTYLE_SIDE, TXT_STACKSTYLE_STAC,
     TXT_STACKSTYLE_FSTA);
  LabelStyleText: array[0..10] of string =
    ('', TXT_LABELSTYLE_NONE, TXT_LABELSTYLE_VALU, TXT_LABELSTYLE_PERC,
     TXT_LABELSTYLE_LABL, TXT_LABELSTYLE_LBPC, TXT_LABELSTYLE_LBVL,
     TXT_LABELSTYLE_LEGD, TXT_LABELSTYLE_PTOT, TXT_LABELSTYLE_LBTO,
     TXT_LABELSTYLE_XVAL);

type
//Graph series types
  TSeriesType = (stLine, stFastLine, stPoint, stBar, stHorizBar, stArea, stPie);

//Axis types
  TAxisType = (atX, atY);

//Axis information
  TAxisInfo = record
    DataMin: String;
    DataMax: String;
    AxisMin: String;
    AxisMax: String;
    AxisInc: String;
  end;

//Graph series options
  TSeriesOptions = class(TObject)
    Constructor Create;
    public
      SeriesType      : TSeriesType;
      LineVisible     : Boolean;
      LineStyle       : Integer;
      LineColor       : TColor;
      LineWidth       : Integer;
      PointVisible    : Boolean;
      PointStyle      : Integer;
      PointColor      : TColor;
      PointSize       : Integer;
      AreaFillStyle   : Integer;
      AreaFillColor   : TColor;
      AreaStacking    : Integer;
      PieCircled      : Boolean;
      PieUsePatterns  : Boolean;
      PieRotation     : Integer;
      LabelsVisible   : Boolean;
      LabelsTransparent: Boolean;
      LabelsArrows    : Boolean;
      LabelsBackColor : TColor;
      LabelsStyle     : Integer;
    end;

  { TChartOptionsDlg }

  TChartOptionsDlg = class(TForm)
    DefaultBox: TCheckBox;
    FontDialog1: TFontDialog;
    OkBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    PageControl1: TPageControl;
    GeneralPage: TTabSheet;
    XaxisPage: TTabSheet;
    LegendPage: TTabSheet;
    StylesPage: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PanelColorBox: TColorBox;
    BackColorBox1: TColorBox;
    View3DBox: TCheckBox;
    GraphTitleBox: TEdit;
    Pct3DUpDown: TUpDown;
    Pct3DEdit: TEdit;
    XminLabel: TLabel;
    XmaxLabel: TLabel;
    XIncrementLabel: TLabel;
    Label11: TLabel;
    XDataMinLabel: TLabel;
    XDataMaxLabel: TLabel;
    Xmin: TEdit;
    Xmax: TEdit;
    Xinc: TEdit;
    Xtitle: TEdit;
    Xgrid: TCheckBox;
    Label18: TLabel;
    Label19: TLabel;
    LegendFrameBox: TCheckBox;
    LegendVisibleBox: TCheckBox;
    LegendPosBox: TComboBox;
    LegendColorBox: TColorBox;
    LegendCheckBox: TCheckBox;
    LegendShadowBox: TCheckBox;
    Label21: TLabel;
    Label22: TLabel;
    SeriesComboBox: TComboBox;
    SeriesTitle: TEdit;
    Panel6: TPanel;
    PageControl2: TPageControl;
    LineOptionsSheet: TTabSheet;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    LineStyleBox: TComboBox;
    LineColorBox: TColorBox;
    LineVisibleBox: TCheckBox;
    LineSizeEdit: TEdit;
    LineSizeUpDown: TUpDown;
    MarkOptionsSheet: TTabSheet;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    MarkVisibleBox: TCheckBox;
    MarkStyleBox: TComboBox;
    MarkColorBox: TColorBox;
    MarkSizeEdit: TEdit;
    MarkSizeUpDown: TUpDown;
    AreaOptionsSheet: TTabSheet;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    AreaFillStyleBox: TComboBox;
    AreaColorBox: TColorBox;
    StackStyleBox: TComboBox;
    PieOptionsSheet: TTabSheet;
    Label32: TLabel;
    PieCircledBox: TCheckBox;
    PiePatternBox: TCheckBox;
    PieRotateEdit: TEdit;
    PieRotateUpDown: TUpDown;
    LabelsOptionsSheet: TTabSheet;
    Label33: TLabel;
    Label34: TLabel;
    LabelsStyleBox: TComboBox;
    LabelsBackColorBox: TColorBox;
    LabelsTransparentBox: TCheckBox;
    LabelsArrowsBox: TCheckBox;
    LabelsVisibleBox: TCheckBox;
    LegendTransparentBox: TCheckBox;
    Label3: TLabel;
    GraphTitleFontLabel: TLabel;
    XaxisFontLabel: TLabel;
    LegendFontLabel: TLabel;
    LegendWidthUpDown: TUpDown;
    LegendWidthEdit: TEdit;
    YaxisPage: TTabSheet;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Ymin: TEdit;
    Ymax: TEdit;
    Yinc: TEdit;
    YDataMaxLabel: TLabel;
    YDataMinLabel: TLabel;
    Ygrid: TCheckBox;
    Label10: TLabel;
    YaxisFontLabel: TLabel;
    Ytitle: TEdit;
    XautoScale: TLabel;
    YautoScale: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MarkOptionsSheetContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure SeriesComboBoxClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure StylesPageExit(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure GraphTitleFontLabelLinkClick(Sender: TObject; const Link: string);
    procedure XaxisFontLabelLinkClick(Sender: TObject; const Link: string);
    procedure LegendFontLabelLinkClick(Sender: TObject; const Link: string);
    procedure YaxisFontLabelLinkClick(Sender: TObject; const Link: string);
    procedure XautoScaleLinkClick(Sender: TObject; const Link: string);
    procedure YautoScaleLinkClick(Sender: TObject; const Link: string);
  private
    { Private declarations }
    SeriesIndex: Integer;
    theSeries: TStringlist;
    IsPieChart: Boolean;
    IsDateTime: Boolean;
    //Lazarus - Backwall dimensions in pixels
    BackWallHeight: Integer;
    BackWallWidth: Integer;

    procedure SaveSeriesOptions(const Index: Integer);
    procedure SetSeriesOptions(const Index: Integer);
    procedure SetAxisScaling(theAxis: TChartAxis; const Smin, Smax,
                Sinc: String; theChart: TChart);
    function GetAxisValue(S: string): Double;
  public
    { Public declarations }
    UseDefaultPanelColor: Boolean;
    procedure LoadOptions(theChart: TChart);
    procedure UnloadOptions(theChart: TChart);
  end;

procedure Execute(theForm: TForm;
                  theChart: TChart;
                  var startPage: Integer;
                  var default: Boolean);

implementation

{$R *.lfm}

uses Uglobals, Uutils, Fgraph, Fmain;

{------------------------------------------------------------------------------
  Procedure called externally to launch the dialog.
  theForm: the form with the chart on it
  theChart: the chart whose options are being set
  startPage: on input the dialog page to display first,
             on output the page currently displayed
  default: on input determines if DefaultBox checkbox is visible,
           on output contains state of DefaultBox checkbox.
------------------------------------------------------------------------------}
procedure Execute(theForm: TForm;
                  theChart: TChart;
                  var startPage: Integer;
                  var default: Boolean);
var
  ChartOptionsDlg: TChartOptionsDlg;
begin
  ChartOptionsDlg := TChartOptionsDlg.Create(theForm);
  with ChartOptionsDlg do
  try
    BackWallHeight := TGraphForm(theForm).BackWallHeight;
    BackWallWidth := TGraphForm(theForm).BackWallWidth;
    PageControl1.ActivePageIndex := startPage;
    DefaultBox.Visible := default;
    LoadOptions(theChart);
    if ShowModal = mrOK then
    begin
      startPage := PageControl1.ActivePageIndex;
      UnloadOptions(theChart);
      default := DefaultBox.Checked;
    end;
  finally
    ChartOptionsDlg.Free;
  end;

end;

{Constructor for TSeriesOptions}
Constructor TSeriesOptions.Create;
begin
  Inherited Create;
end;

{Dialog's Constructor}
procedure TChartOptionsDlg.FormCreate(Sender: TObject);
var
  i: Integer;
begin
//  Uglobals.SetFont(self);
{ Load option choices into comboboxes }
  for i := 0 to High(LineStyleText) do
    LineStyleBox.Items.Add(LineStyleText[i]);
  for i := 0 to High(LegendPosText) do
    LegendPosBox.Items.Add(LegendPosText[i]);
  for i := 0 to High(FillStyleText) do
    AreaFillStyleBox.Items.Add(FillStyleText[i]);
  for i := 0 to High(MarkStyleText) do
    MarkStyleBox.Items.Add(MarkStyleText[i]);
  for i := 0 to High(StackStyleText) do
    StackStyleBox.Items.Add(StackStyleText[i]);
  for i := 0 to High(LabelStyleText) do
    LabelsStyleBox.Items.Add(LabelStyleText[i]);

  UseDefaultPanelColor := False;
  LegendColorBox.DefaultColorColor := PanelColorBox.DefaultColorColor;
  {$IFNDEF WINDOWS}
  LegendTransparentBox.Enabled := False;
  LabelsTransparentBox.Enabled := False;
  {$ENDIF}

{ Create a stringlist to hold data series options }
  theSeries := TStringlist.Create;
  PageControl1.ActivePage := GeneralPage;

end;

{Dialog's OnDestroy handler.}
procedure TChartOptionsDlg.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  with theSeries do
  begin
    for i := 0 to Count - 1 do
      // Free the TSeriesOptions objects created in LoadOptions()
      Objects[i].Free;
    Free;
  end;
end;

procedure TChartOptionsDlg.MarkOptionsSheetContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

{Change the font used for the chart's title.}
procedure TChartOptionsDlg.GraphTitleFontLabelLinkClick(Sender: TObject;
  const Link: string);
begin
  with FontDialog1 do
  begin
    Font.Assign(GraphTitleBox.Font);
    if Execute then GraphTitleBox.Font.Assign(Font);
  end;
end;

procedure TChartOptionsDlg.XautoScaleLinkClick(Sender: TObject;
  const Link: string);
var
  XminValue : Double;
  XmaxValue : Double;
  XincValue : Double;
begin
  XminValue := GetAxisValue(XdataMinLabel.Caption);
  XmaxValue := GetAxisValue(XdataMaxLabel.Caption);
  Uutils.AutoScale(XminValue, XmaxValue, XincValue);
  Xmin.Text := Format('%f', [XminValue]);
  Xmax.Text := Format('%f', [XmaxValue]);
  Xinc.Text := Format('%f', [XincValue]);
end;

procedure TChartOptionsDlg.YautoScaleLinkClick(Sender: TObject;
  const Link: string);
var
  YminValue : Double;
  YmaxValue : Double;
  YincValue : Double;
begin
  YminValue := GetAxisValue(YdataMinLabel.Caption);
  YmaxValue := getAxisValue(YdataMaxLabel.Caption);
  Uutils.AutoScale(YminValue, YmaxValue, YincValue);
  Ymin.Text := Format('%f', [YminValue]);
  Ymax.Text := Format('%f', [YmaxValue]);
  Yinc.Text := Format('%f', [YincValue]);
end;

function  TChartOptionsDlg.GetAxisValue(S: string): Double;
begin
  S := StrUtils.MidStr(S, 2, S.Length-2);
  Result := StrToFloat(S);
end;

{Change the font used for a chart's axis labels.}
procedure TChartOptionsDlg.XaxisFontLabelLinkClick(Sender: TObject;
  const Link: string);
begin
  with FontDialog1 do
  begin
    Font.Assign(Xtitle.Font);
    if Execute then
    begin
      Xtitle.Font.Assign(Font);
    end;
  end;
end;

procedure TChartOptionsDlg.YaxisFontLabelLinkClick(Sender: TObject;
  const Link: string);
begin
  with FontDialog1 do
  begin
    Font.Assign(Ytitle.Font);
    if Execute then
    begin
      Ytitle.Font.Assign(Font);
    end;
  end;
end;

{Change the font used for the chart's legend.}
procedure TChartOptionsDlg.LegendFontLabelLinkClick(Sender: TObject;
  const Link: string);
begin
  with FontDialog1 do
  begin
    Font.Assign(SeriesTitle.Font);
    if Execute then SeriesTitle.Font.Assign(Font);
  end;
end;

{OnClick handler for user's choice of a chart series to edit.}
procedure TChartOptionsDlg.SeriesComboBoxClick(Sender: TObject);
begin
  if (Sender is TComboBox) then
    with Sender as TComboBox do
    begin
      SaveSeriesOptions(SeriesIndex); {Store options for current series}
      SeriesIndex := ItemIndex;       {Update value of current series}
      SetSeriesOptions(SeriesIndex);  {Load series options into form}
    end;
end;

{OnExit handler for the dialog's Styles page.}
procedure TChartOptionsDlg.StylesPageExit(Sender: TObject);
begin
  SaveSeriesOptions(SeriesIndex);
end;


{Transfer options from the chart to the dialog.}
procedure TChartOptionsDlg.LoadOptions(theChart: TChart);

var
  i: Integer;
  s: String;
  SeriesOptions: TSeriesOptions;
  min, max: Double;
begin
  IsPieChart := False;
  with theChart do
  begin
  { General Page }
    if (Depth = 0) then
      begin
        View3DBox.Checked := false;
        Pct3DUpDown.Position := GraphOptions.Percent3D;
      end
    else
      begin
        View3DBox.Checked := true;
        Pct3DUpDown.Position := GraphOptions.Percent3D;
      end;

    PanelColorBox.Selected := theChart.Color;
    BackColorBox1.Selected := BackColor;
    GraphTitleBox.Font.Assign(Title.Font);
    if (Title.Text.Count > 0) then
      GraphTitleBox.Text := Title.Text[0];

  { Series Page - do before Axis pages to get value for IsPieChart }
  { Save current line series options }
    IsDateTime := False;
    SeriesTitle.Font.Assign(Legend.Font);
    for i := 0 to SeriesCount-1 do
    begin
      if Series[i].Active then
      begin
        SeriesOptions := TSeriesOptions.Create;
        s := 'Series' + IntToStr(i+1);
        SeriesComboBox.Items.Add(s);
        { #todo : Implement Date time }
        {if Series[i].XValues.DateTime then IsDateTime := True;
        if HorAxis.Marks.Source.ClassName = 'TDateTimeIntervalChartSource' then
           IsDateTime := True;}

        with TChartSeries(Series[i]), SeriesOptions do
        begin
          LabelsVisible := Marks.Visible;
          LabelsArrows := Marks.Arrow.Visible;
          LabelsTransparent := false;
          LabelsBackColor := Marks.LabelBrush.Color;
          LabelsStyle := Ord(Marks.Style);
        end;
        if Series[i] is TLineSeries then
          with Series[i] as TLineSeries, SeriesOptions do
          begin
            SeriesType := stLine;
            if LinePen.Style = psClear then LineVisible := False
            else LineVisible := True;
            LineStyle := Ord(LinePen.Style);
            LineColor := SeriesColor;
            LineWidth := LinePen.Width;
            AreaFillStyle := Ord(Brush.Style);
            PointVisible := Pointer.Visible;
            PointStyle := Ord(Pointer.Style);
            PointColor := Pointer.Pen.Color;
            PointSize := Pointer.VertSize;
          end
        else if Series[i] is TBarSeries then
          with Series[i] as TBarSeries, SeriesOptions do
          begin
            SeriesType := stBar;
            AreaFillStyle := Ord(BarBrush.Style);
            if BarBrush.Style = bsSolid then
            begin
              AreaFillColor := SeriesColor;
              LineColor := BarBrush.Color;
            end
            else
            begin
              LineColor := SeriesColor;
              AreaFillColor := BarBrush.Color;
            end;
            {AreaStacking := Ord(MultiBar);}
          end
        else if Series[i] is TAreaSeries then
          with Series[i] as TAreaSeries, SeriesOptions do
          begin
            SeriesType := stArea;
            if AreaLinesPen.Style = psClear then LineVisible := False
            else LineVisible := True;
            LineStyle := Ord(AreaLinesPen.Style);
            LineColor := AreaLinesPen.Color;
            LineWidth := AreaLinesPen.Width;
            AreaFillColor := SeriesColor;
            AreaFillStyle := Ord(AreaBrush.Style);
          end
        else if Series[i] is TPieSeries then
          with Series[i] as TPieSeries, SeriesOptions do
          begin
            SeriesType := stPie;
            IsPieChart := True;
            if EdgePen.Style = psClear then LineVisible := False
            else LineVisible := True;
            LineStyle := Ord(EdgePen.Style);
            LineColor := EdgePen.Color;  //SeriesColor;
            LineWidth := EdgePen.Width;
            PieCircled := true;
            PieUsePatterns := false;
            PieRotation := StartAngle;
          end;
        if Length(TChartSeries(Series[i]).Title) > 0 then
           s := TChartSeries(Series[i]).Title;
        theSeries.AddObject(s,SeriesOptions);
      end;
    end;

  { X Axis }
    if IsPieChart then XaxisPage.TabVisible := False
    else
    begin
      XdataMinLabel.Caption := Format('(%f)',[theChart.XGraphMin]);
      XdataMaxLabel.Caption := Format('(%f)',[theChart.XGraphMax]);
      with BottomAxis do
      begin
        theChart.GetAllSeriesAxisLimits(theChart.BottomAxis, min, max);
        Xmin.Text := Format('%f',[min]);
        Xmax.Text := Format('%f',[max]);
        Xinc.Text := Format('%f',[(max-min) / Intervals.Count]);
        Xgrid.Checked := Grid.Visible;
        Xtitle.Font.Assign(Title.LabelFont);
        Xtitle.Text := Title.Caption;
      end;
    end;

  { Y Axis }
    if IsPieChart then YaxisPage.TabVisible := False
    else
    begin
      YdataMinLabel.Caption := Format('(%f)',[theChart.YGraphMin]);
      YdataMaxLabel.Caption := Format('(%f)',[theChart.YGraphMax]);
      with LeftAxis do
      begin
        theChart.GetAllSeriesAxisLimits(theChart.LeftAxis, min, max);
        Ymin.Text := Format('%f',[min]);
        Ymax.Text := Format('%f',[max]);
        Yinc.Text := Format('%f',[(max-min) / Intervals.Count]);
        Ygrid.Checked := Grid.Visible;
        Ytitle.Font.Assign(Title.LabelFont);
        Ytitle.Font.Orientation:= 0;
        Ytitle.Text := Title.Caption;
      end;
    end;

  { Legend Page }
    LegendPosBox.ItemIndex := Ord(Legend.Alignment);
    LegendColorBox.Selected := Legend.BackgroundBrush.Color;
    LegendCheckBox.Checked := false;
    LegendShadowBox.Checked := false;
    LegendFrameBox.Checked := Legend.Frame.Visible;
    if Legend.Transparency = 0 then LegendTransparentBox.Checked := False
    else LegendTransparentBox.Checked := True;
    LegendVisibleBox.Checked := Legend.Visible;
    LegendWidthUpDown.Position := Legend.SymbolWidth;
  end;

//Set current series to first series & update dialog entries
  if theChart.SeriesCount > 0 then
  begin
    SeriesIndex := 0;
    SeriesComboBox.ItemIndex := 0;
    SetSeriesOptions(0);
  end
  else StylesPage.TabVisible := False;
end;

{OnChange handler for the dialog's page control.}
procedure TChartOptionsDlg.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage = StylesPage
    then SetSeriesOptions(SeriesIndex);
end;

{Transfer otions from the dialog back to the chart.}
procedure TChartOptionsDlg.UnloadOptions(theChart: TChart);
var
  i,j, d: Integer;
  s     : String;
  SeriesOptions: TSeriesOptions;
begin
  with theChart do
  begin

  { General Page }
    if View3DBox.Checked = False then
    begin
      Depth := 0;
      GraphOptions.View3D := False;
      GraphOptions.Percent3D := Pct3DUpDown.Position;
      BottomAxis.ZPosition:= 0;
      LeftAxis.ZPosition:= 0;
      for I := 0 to SeriesCount - 1 do
        Series[I].Depth:= 0;
    end
    else
    begin
      GraphOptions.View3D := True;
      GraphOptions.Percent3D := Pct3DUpDown.Position;
      Depth := round(GraphOptions.Percent3D / 100 * 0.71 * BackWallHeight);
      BottomAxis.ZPosition:= Depth;
      LeftAxis.ZPosition:= Depth;
      for I := 0 to SeriesCount - 1 do
        Series[I].Depth:= round(Depth / SeriesCount);
    end;
    theChart.BackColor := BackColorBox1.Selected;
    with PanelColorBox do
    begin
      if Selected = DefaultColorColor then UseDefaultPanelColor := True
      else UseDefaultPanelColor := False;
      theChart.Color := Selected;
    end;
    s := GraphTitleBox.Text;
    Title.Text.Clear;
    if (Length(s) > 0) then Title.Text.Add(s);
    Title.Font.Assign(GraphTitleBox.Font);
    Title.Brush.Color := PanelColorBox.Selected;

  { X Axis Page }
    if not IsPieChart then with BottomAxis do
    begin
      SetAxisScaling(BottomAxis,Xmin.Text,Xmax.Text,Xinc.Text, theChart);
      Grid.Visible := Xgrid.Checked;
      Title.Caption := Xtitle.Text;
      Title.LabelFont := Xtitle.Font;
      Marks.LabelFont.Assign(Xtitle.Font);
    end;

  { Y Axis Page }
    if not IsPieChart then with LeftAxis do
    begin
      SetAxisScaling(LeftAxis,Ymin.Text,Ymax.Text,Yinc.Text, theChart);
      Grid.Visible := Ygrid.Checked;
      Title.Caption := Ytitle.Text;
      Title.LabelFont := Ytitle.Font;
      Title.LabelFont.Orientation := 900;
      Marks.LabelFont.Assign(Ytitle.Font);
    end;

  { Legend Page }
    Legend.Alignment := TLegendAlignment(LegendPosBox.ItemIndex);
    if (LegendPosBox.ItemIndex in [3, 4]) then Legend.ColumnCount:= 100
    else Legend.ColumnCount:= 1;
    with LegendColorBox do
    begin
      if ((Items[ItemIndex] = 'Default') and (Language = 0)) or
         ((Items[ItemIndex] = 'Predeterminado') and (Language = 1)) then
         Legend.BackgroundBrush.Color := DefaultColorColor
      else Legend.BackgroundBrush.Color := LegendColorBox.Selected;
    end;
    Legend.Frame.Visible := LegendFrameBox.Checked;
    if LegendTransparentBox.Checked = False then Legend.Transparency := 0
    else Legend.Transparency := 60;
    Legend.Visible := LegendVisibleBox.Checked;
    Legend.SymbolWidth := LegendWidthUpDown.Position;
    Legend.Font.Assign(SeriesTitle.Font);

  { Series Page }
    if SeriesCount > 0 then
    begin
      SaveSeriesOptions(SeriesIndex);
      j := 0;
      for i := 0 to SeriesCount-1 do
      begin
        if Series[i].Active then
        begin
          SeriesOptions := TSeriesOptions(theSeries.Objects[j]);
          TChartSeries(Series[i]).Title := theSeries.Strings[j];
          with TChartSeries(Series[i]), SeriesOptions do
          begin
            Marks.Visible := LabelsVisible;
            Marks.Arrow.Visible := LabelsArrows;
            Marks.LabelBrush.Color := LabelsBackColor;
            Marks.LabelFont.Color := InvertColor(LabelsBackColor);
            Marks.Style := TSeriesMarksStyle(LabelsStyle);
            Marks.LinkPen.Color := Marks.Frame.Color;
            with TGraphForm(Parent).Graph do
            begin
              if GraphType in  [TIMESERIESPLOT, PROFILEPLOT] then
              begin

                if ObjectType in [NODESERIES, NETNODES] then
                  d := NodeUnits[VarType].Digits
                else
                  d := LinkUnits[VarType].Digits;

                case LabelsStyle of
                  Ord(smsCustom), Ord(smsNone): ;
                  Ord(smsValue): Marks.Format := '%0:.'+IntToStr(d)+'f';
                  Ord(smsPercent): Marks.Format := '%1:.1f%%';
                  Ord(smsLabel): Marks.Format := '%2:s';
                  Ord(smsLabelPercent): Marks.Format := '%2:s %1:.1f%%';
                  Ord(smsLabelValue): Marks.Format := '%2:s %0:.'+IntToStr(d)+'f';
                  //Ord(smsLegend): Marks.Format := ;
                  Ord(smsPercentTotal): Marks.Format := '%1:.1f%% of %0:.'+IntToStr(d)+'f';
                  Ord(smsLabelPercentTotal): Marks.Format := '%2:s %1:.1f%% of %0:.'+IntToStr(d)+'f';
                  Ord(smsXValue): Marks.Format := '%4:.0f';
                end;
              end;
            end;
          end;

          if Series[i] is TLineSeries then
          with Series[i] as TLineSeries, SeriesOptions do
          begin
            if LineVisible = False then LinePen.Style := psClear
            else LinePen.Style := TPenStyle(LineStyle);
            SeriesColor := LineColor;
            LinePen.Width := LineWidth;
            Pointer.Visible := PointVisible;
            Pointer.Style := TSeriesPointerStyle(PointStyle);
            Pointer.Pen.Color := PointColor;
            Pointer.Brush.Color := PointColor;
            Pointer.HorizSize := PointSize;
            Pointer.VertSize := PointSize;
            Brush.Style := TBrushStyle(AreaFillStyle);
            if (not Pointer.Visible) and (LinePen.Style = psClear) then
              ShowinLegend := False
            else ShowinLegend := True;
          end

          else if Series[i] is TBarSeries then
          with Series[i] as TBarSeries, SeriesOptions do
          begin
            BarBrush.Style := TBrushStyle(AreaFillStyle);
            if BarBrush.Style = bsSolid then
            begin
              SeriesColor := AreaFillColor;
              BarBrush.Color := AreaFillColor
            end
            else
            begin
              SeriesColor := LineColor;
              BarBrush.Color := AreaFillColor;
            end;
          end

          else if Series[i] is TAreaSeries then
          with Series[i] as TAreaSeries, SeriesOptions do
          begin
            AreaBrush.Style := TBrushStyle(AreaFillStyle);
            SeriesColor := AreaFillColor;
            AreaBrush.Color := AreaFillColor;
            if LineVisible = False then AreaLinesPen.Style := psClear
            else AreaLinesPen.Style := TPenStyle(LineStyle);
            AreaLinesPen.Color := LineColor;
            AreaLinesPen.Width := LineWidth;
            AreaContourPen.Color:= LineColor;
            AreaContourPen.Width := LineWidth;
          end

          else if Series[i] is TPieSeries then
          with Series[i] as TPieSeries, SeriesOptions do
          begin
            if LineVisible = False then EdgePen.Style := psClear
            else EdgePen.Style := TPenStyle(LineStyle);
            EdgePen.Color := LineColor;
            EdgePen.Width := LineWidth;
            StartAngle := PieRotation;
          end;
          Inc(j);
        end;
      end;
    end;
    Refresh;
  end;
end;

procedure TChartOptionsDlg.SetAxisScaling(theAxis: TChartAxis;
            const Smin,Smax,Sinc: String; theChart: TChart);
{-------------------------------------------------
   Retrieves axis scaling options from form.
--------------------------------------------------}
var
  zMin, zMax, zInc : Double;
  i : Integer;
begin
  zMin := StrToFloatDef(Smin, 0);
  zMax := StrToFloatDef(Smax, 0);
  zInc := StrToFloatDef(Sinc, 0);
  if (zMin >= zMax) or (zInc < 0) or (zInc > zMax - zMin) then exit;
  with theAxis do
  begin
    if ((zMax - zMin) mod zInc) > 0 then
    Uutils.AutoScale(zMin, zMax, zInc);
    if IsVertical = False then
    begin
      theChart.Extent.UseXMin := True;
      theChart.Extent.UseXMax := True;
      theChart.Extent.XMin := zMin;
      theChart.Extent.XMax := zMax;
    end
    else
    begin
      theChart.Extent.UseYMin := True;
      theChart.Extent.UseYMax := True;
      theChart.Extent.YMin := zMin;
      theChart.Extent.YMax := zMax;
    end;
    Intervals.Options:=[aipUseCount];
    Intervals.Count := Trunc((zMax - zMin) / zInc);
    if zInc > 1.0 then Marks.Format := '%0:.0f' else
    begin
      i := 0;
      while zInc < 1.0 do
      begin
          zInc := zInc * 10;
          Inc(i);
       end;
      if i < 3 then Marks.Format := '%0:.' + IntToStr(i) + 'f'
      else Marks.Format := '%0:10.2e';
    end;
  end;
end;


{Transfer options for a data series to the dialog.}
procedure TChartOptionsDlg.SetSeriesOptions(const Index: Integer);
var
  SeriesOptions: TSeriesOptions;
begin
  SeriesTitle.Text := theSeries.Strings[Index];
  SeriesOptions := TSeriesOptions(theSeries.Objects[Index]);
  with SeriesOptions do
  begin
    LineStyleBox.ItemIndex := LineStyle;
    LineColorBox.Selected := LineColor;
    LineSizeUpDown.Position := LineWidth;
    LineVisibleBox.Checked := LineVisible;
    MarkStyleBox.ItemIndex := PointStyle;
    MarkColorBox.Selected := PointColor;
    MarkSizeUpDown.Position := PointSize;
    MarkVisibleBox.Checked := PointVisible;
    AreaFillStyleBox.ItemIndex := AreaFillStyle;
    AreaColorBox.Selected := AreaFillColor;
    StackStyleBox.ItemIndex := AreaStacking;
    PieCircledBox.Checked := PieCircled;
    PiePatternBox.Checked := PieUsePatterns;
    PieRotateUpDown.Position := PieRotation;
    LabelsVisibleBox.Checked := LabelsVisible;
    LabelsTransparentBox.Checked := LabelsTransparent;
    LabelsBackColorBox.Selected := LabelsBackColor;
    LabelsArrowsBox.Checked := LabelsArrows;
    LabelsStyleBox.ItemIndex := LabelsStyle;
  end;
  PieOptionsSheet.TabVisible := False;
  case SeriesOptions.SeriesType of
  stLine:
  begin
    LineOptionsSheet.TabVisible := True;
    LineVisibleBox.Visible := False;
    MarkOptionsSheet.TabVisible := True;
    AreaOptionsSheet.TabVisible := False;
    LabelsOptionsSheet.TabVisible := True;
    PageControl2.ActivePage := LineOptionsSheet;
  end;
  stFastLine:
  begin
    LineOptionsSheet.TabVisible := True;
    LineVisibleBox.Visible := False;
    MarkOptionsSheet.TabVisible := False;
    AreaOptionsSheet.TabVisible := False;
    LabelsOptionsSheet.TabVisible := False;
    PageControl2.ActivePage := LineOptionsSheet;
  end;
  stPoint:
  begin
    LineOptionsSheet.TabVisible := False;
    MarkOptionsSheet.TabVisible := True;
    AreaOptionsSheet.TabVisible := False;
    LabelsOptionsSheet.TabVisible := True;
    PageControl2.ActivePage := MarkOptionsSheet;
  end;
  stBar, stHorizBar:
  begin
    LineOptionsSheet.TabVisible := True;
    MarkOptionsSheet.TabVisible := False;
    AreaOptionsSheet.TabVisible := True;
    LabelsOptionsSheet.TabVisible := True;
    PageControl2.ActivePage := AreaOptionsSheet;
  end;
  stArea:
  begin
    LineOptionsSheet.TabVisible := True;
    MarkOptionsSheet.TabVisible := False;
    AreaOptionsSheet.TabVisible := True;
    LabelsOptionsSheet.TabVisible := True;
    PageControl2.ActivePage := AreaOptionsSheet;
  end;
  stPie:
  begin
    LineOptionsSheet.TabVisible := True;
    MarkOptionsSheet.TabVisible := False;
    AreaOptionsSheet.TabVisible := False;
    PieOptionsSheet.TabVisible := True;
    LabelsOptionsSheet.TabVisible := True;
    PageControl2.ActivePage := PieOptionsSheet;
  end;
  end;
end;

{Transfer options from the dialog to a data series.}
procedure TChartOptionsDlg.SaveSeriesOptions(const Index: Integer);
var
  SeriesOptions: TSeriesOptions;
begin
  theSeries.Strings[Index] := SeriesTitle.Text;
  SeriesOptions := TSeriesOptions(theSeries.Objects[Index]);
  with SeriesOptions do
  begin
    if LineOptionsSheet.TabVisible then
    begin
      LineStyle := LineStyleBox.ItemIndex;
      LineColor := LineColorBox.Selected;
      LineWidth := LineSizeUpDown.Position;
      LineVisible := LineVisibleBox.Checked;
    end;
    if MarkOptionsSheet.TabVisible then
    begin
      PointStyle := MarkStyleBox.ItemIndex;
      PointColor := MarkColorBox.Selected;
      PointSize := MarkSizeUpDown.Position;
      PointVisible := MarkVisibleBox.Checked;
    end;
    if AreaOptionsSheet.TabVisible then
    begin
      AreaFillStyle := AreaFillStyleBox.ItemIndex;
      AreaFillColor := AreaColorBox.Selected;
      AreaStacking := StackStyleBox.ItemIndex;
    end;
    if PieOptionsSheet.TabVisible then
    begin
      PieCircled := PieCircledBox.Checked;
      PieUsePatterns := PiePatternBox.Checked;
      PieRotation := PieRotateUpDown.Position;
    end;
    if LabelsOptionsSheet.TabVisible then
    begin
      LabelsVisible := LabelsVisibleBox.Checked;
      LabelsArrows := LabelsArrowsBox.Checked;
      LabelsTransparent := LabelsTransparentBox.Checked;
      LabelsBackColor := LabelsBackColorBox.Selected;
      LabelsStyle := LabelsStyleBox.ItemIndex;
    end;
  end;
end;

procedure TChartOptionsDlg.HelpBtnClick(Sender: TObject);
var
  HC: Integer;
begin
  case PageControl1.ActivePageIndex of
    0:   HC := 250;
    1:   HC := 251;
    2:   HC := 251;
    3:   HC := 252;
    4:   HC := 253;
    else HC := 0;
  end;
  if HC > 0 then MainForm.LaunchHelp(HC);
end;

end.
