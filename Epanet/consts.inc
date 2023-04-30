{-------------------------------------------------------------------}
{                    Unit:    Consts.txt                            }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                    Ported to Lazarus by: Zorba Lopez Rivera       }
{                    Date:    15/05/23                              }
{                                                                   }
{  Constants used with EPANET2W and imported into Uglobals.pas.     }
{-------------------------------------------------------------------}

const

// File names
  INIFILE = 'epanet2.ini';
  HLPFILE = 'EPANET2.chm';
  TUTORFILE = 'tutorial.chm';

// Labels for network objects
  ObjectLabel: array[JUNCS..OPTS] of string =
    (TXT_JUNCTION, TXT_RESERVOIR, TXT_TANK, TXT_PIPE, TXT_PUMP, TXT_VALVE,
     TXT_LABEL, TXT_PATTERN, TXT_CURVE, TXT_CONTROL, TXT_OPTION);
  ObjectLabels: array[JUNCS..OPTS] of string =
    (TXT_JUNCTIONS, TXT_RESERVOIRS, TXT_TANKS, TXT_PIPEs, TXT_PUMPS, TXT_VALVES,
     TXT_LABELS, TXT_PATTERNS, TXT_CURVES, TXT_CONTROLS, TXT_OPTIONS);

//Control categories
  ControlCategories: array [0..1] of string =(TXT_SIMPLE, TXT_RULEBASED);

//Option categories
  OptionCategories: array [0..4] of string =
    (TXT_HYDRAULICS, TXT_QUALITY, TXT_REACTIONS, TXT_TIMES, TXT_ENERGY);

//GraphTypes
  GraphTypes: array [0..4] of string =
    (TXT_GTIME_SERIES, TXT_GPROFILE_PLOT, TXT_GCONTOUR_PLOT,
     TXT_GFREQUENCY_PLOT, TXT_GSYSTEM_FLOW);

//GObjectTypes
  GObjectTypes: array [0..1] of string = (TXT_G_NODES, TXT_G_LINKS);

//FindTypes
  FindTypes: array [0..2] of string =
    (TXT_NODE, TXT_LINK, TXT_SOURCES);

//DimUnits
  DimUnits: array [0..3] of string =
    (TXT_DFEET, TXT_DMETER, TXT_DDEGREE, TXT_DNONE);

//EnerGraph
  EnerGraph: array [0..5] of string =
    (TXT_UTILIZATION, TXT_EFFICIENCY, TXT_KWH_MGAL, TXT_AVG_KWH, TXT_PEAK_KWH,
     TXT_COST_DAY);

//CopyDestination
  CopyDestination: array [0..1] of string = (TXT_CLIPBOARD, TXT_FILE);

//CopyFormat
{$IFDEF WINDOWS}
  CopyFormat: array [0..2] of string = (TXT_BITMAP, TXT_METAFILE, TXT_DATATEXT);
{$ELSE}
  CopyFormat: array [0..2] of string = (TXT_BITMAP, TXT_SVG, TXT_DATATEXT);
{$ENDIF}

//DXFNodes
  DXFNodes: array [0..2] of string =
    (TXT_DXF_OPENC, TXT_DXF_FILLC, TXT_DXF_FILLS);

// Labels of X-Y curve types
  CurveLabel: array [0..3] of PChar =
    ('VOLUME','PUMP','EFFICIENCY','HEADLOSS');
  CurveLabelT: array [0..3] of string =
     (TXT_CUR_VOLUM, TXT_CUR_PUMP, TXT_CUR_EFF, TXT_CUR_HEADL);

// Water Quality parameters
  QualParams: array[0..3] of PChar = ('None', 'Chemical', 'Trace', 'Age');

//WQ source types
  SourceType: array[0..3] of PChar =
    ('CONCEN','MASS','SETPOINT','FLOWPACED');
//WQ source types for Dsources
  SourceTypeR: array[0..3] of string =
    (TXT_CONCENTRATION, TXT_MASSBOOSTER, TXT_SETPOINTBOOS, TXT_FLOWPACEDBOOS);

//Data reporting units
  USFlowUnits: array[0..4] of PChar =
                 ('CFS', 'GPM', 'MGD', 'IMGD', 'AFD');
  SIFlowUnits: array[0..4] of PChar =
                 ('LPS', 'LPM', 'MLD', 'CMH', 'CMD');
  BaseQualUnits: array[0..3] of PChar =
                 ('', 'mg/L', 'percent', 'hours');
  MassUnits:     array[0..1] of PChar = ('mg/L', 'ug/L');
  BaseNodeUnits: array[0..NODEVIEWS,TUnitSystem] of PChar =
                 (('',''),('ft','m'),('CFS','LPS'),('mg/L','mg/L'),
                  ('CFS','LPS'),('ft','m'),('psi','m'),('mg/L','mg/L'));
  BaseLinkUnits: array[0..LINKVIEWS,TUnitSystem] of PChar =
                 (('',''),('ft','m'),('in','mm'),('mft','mm'),('',''),
                  ('',''),('CFS','LPS'),('fps','m/s'),('ft/Kft','m/km'),
                  ('',''),('mg/L/d','mg/L/d'),('mg/L','mg/L'));
  MapUnits: array[0..3] of PChar =
                  ('Feet', 'Meters', 'Degrees', 'None');

//Link status conditions
  //LinkStatus: array[0..7] of PChar =
    //('Closed', 'Closed', 'Closed', 'Open', 'Active', 'Open', 'Open', 'Open');
  LinkStatus: array[0..7] of string =
    (TXT_CLOSED, TXT_CLOSED, TXT_CLOSED, TXT_OPEN, TXT_ACTIVE, TXT_OPEN, TXT_OPEN, TXT_OPEN);

//Auto-Length status
  AutoLengthStatus: array[0..1] of string = (TXT_AUTOLENGTH_OFF, TXT_AUTOLENGTH_ON);

//Map Label meter types
  MeterTypes: array[0..2] of PChar = ('None','Node','Link');

//Relations used in search filters (these correspond to the elements of
//TRelationType which is declared in the unit Uutils.pas)
  FilterRelation: array[0..2] of string = (TXT_FILTER_BELOW,TXT_FILTER_EQUAL,
                                           TXT_FILTER_ABOVE);
//Available GUI Language translations
  Languages: array[0..1] of string = ('English', 'Español');

//Network object property lists (* denotes a required parameter)
//The fields of a TPropRecord are defined in the PropEdit.pas unit.
  JuncProps: array [0..15] of TPropRecord =
   ((Name:TXT_PR_JUNCID;      Style:esEdit;    Mask:emNoSpace; Length:MAXID; List:''),
    (Name:TXT_PR_X;           Style:esEdit;    Mask:emNumber;  Length:0; List:''),
    (Name:TXT_PR_Y;           Style:esEdit;    Mask:emNumber;  Length:0; List:''),
    (Name:TXT_PR_DESCR;       Style:esEdit;    Mask:emNone;    Length:0; List:''),
    (Name:TXT_PR_TAG;         Style:esEdit;    Mask:emNoSpace; Length:0; List:''),
    (Name:TXT_PR__ELEV;       Style:esEdit;    Mask:emNumber;  Length:0; List:''),
    (Name:TXT_COLL_LABELS_BASEDEMAND;       Style:esEdit;    Mask:emNumber;  Length:0; List:''),
    (Name:TXT_PR_DEMANDPAT;   Style:esEdit;    Mask:emNoSpace; Length:MAXID; List:''),
    (Name:TXT_PR_DEMANDCAT;   Style:esButton;  Mask:emNone;    Length:0; List:''),
    (Name:TXT_PR_EMITTCOEF;   Style:esEdit;    Mask:emNumber;  Length:0; List:''),
    (Name:TXT_PR_INITQ;       Style:esEdit;    Mask:emNumber;  Length:0; List:''),
    (Name:TXT_PR_SOURCEQ;     Style:esButton;  Mask:emNone;    Length:0; List:''),
    (Name:TXT_PR_ACTUALD;     Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_THEAD;       Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_PRESSURE;    Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_QUALITY;        Style:esReadOnly;Mask:emNone; Length:0; List:''));

  ReservProps: array [0..12] of TPropRecord =
   ((Name:TXT_PR_RESVID;     Style:esEdit;    Mask:emNoSpace; Length:MAXID; List:''),
    (Name:TXT_PR_X;          Style:esEdit;    Mask:emNumber; Length:0; List:''),
    (Name:TXT_PR_Y;          Style:esEdit;    Mask:emNumber; Length:0; List:''),
    (Name:TXT_PR_DESCR;      Style:esEdit;    Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_TAG;        Style:esEdit;    Mask:emNoSpace; Length:0; List:''),
    (Name:TXT_PR__THEAD;     Style:esEdit;    Mask:emNumber; Length:0; List:''),
    (Name:TXT_PR_HEADPAT;    Style:esEdit;    Mask:emNoSpace; Length:MAXID; List:''),
    (Name:TXT_PR_INITQ;      Style:esEdit;    Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR_SOURCEQ;    Style:esButton;  Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_NETINFLOW;  Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_ELEV;       Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_PRESSURE;   Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_QUALITY;       Style:esReadOnly;Mask:emNone; Length:0; List:''));

  TankProps:  array [0..21] of TPropRecord =
   ((Name:TXT_PR_TANKID;    Style:esEdit;    Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_X;         Style:esEdit;    Mask:emNumber; Length:0; List:''),
    (Name:TXT_PR_Y;         Style:esEdit;    Mask:emNumber),
    (Name:TXT_PR_DESCR;     Style:esEdit;    Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_TAG;       Style:esEdit;    Mask:emNoSpace; Length:0; List:''),
    (Name:TXT_PR_SLAB;      Style:esEdit;    Mask:emNumber; Length:0; List:''),
    (Name:TXT_PR_LEVINI;    Style:esEdit;    Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR_LEVMIN;    Style:esEdit;    Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR_LEVMAX;    Style:esEdit;    Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR_DIA;       Style:esEdit;    Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR_VOLMIN;    Style:esEdit;    Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR_VOLCUR;    Style:esEdit;    Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_CANOVER;   Style:esComboList; Mask:emNone;   Length:0;
     List:'No'#13'Yes'),
    (Name:TXT_PR_MIXMOD;    Style:esComboList; Mask:emNone;   Length:0;
     List:'Mixed'#13'2Comp'#13'FIFO'#13'LIFO'),
    (Name:TXT_PR_MIXFRA;    Style:esEdit;    Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR_REACTC;    Style:esEdit;    Mask:emNumber; Length:0; List:''),
    (Name:TXT_PR_INITQ;     Style:esEdit;    Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR_SOURCEQ;   Style:esButton;  Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_NETINFLOW; Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_ELEV;      Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_PRESSURE;  Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_QUALITY;      Style:esReadOnly;Mask:emNone; Length:0; List:''));

  PipeProps:  array [0..18] of TPropRecord =
   ((Name:TXT_PR_PIPEID;   Style:esEdit; Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_NODESTART;Style:esEdit; Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_NODEEND;  Style:esEdit; Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_DESCR;    Style:esEdit; Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_TAG;      Style:esEdit; Mask:emNoSpace; Length:0; List:''),
    (Name:TXT_PR_LENGTH;   Style:esEdit; Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR_DIA;      Style:esEdit; Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR_ROUGHNESS;Style:esEdit; Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR_LOSSCOEF; Style:esEdit; Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR_INITSTATUS;Style:esComboList; Mask:emNone; Length:0;
     List:'Open'#13'Closed'#13'CV'),
    (Name:TXT_PR_BULKCOEF; Style:esEdit; Mask:emNumber; Length:0; List:''),
    (Name:TXT_PR_WALLCOEF; Style:esEdit; Mask:emNumber; Length:0; List:''),
    (Name:TXT_FLOW;        Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_VELOCITY; Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_UNITHL;   Style:esReadOnly;Mask:emNone; Length:0; List:''), {*** Updated 3/1/01 ***}
    (Name:TXT_PR_FRICTFACT;Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_REACTRATE;Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_QUALITY;     Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_STATUS;   Style:esReadOnly;Mask:emNone; Length:0; List:''));

  PumpProps:  array [0..16] of TPropRecord =
   ((Name:TXT_PR_PUMPID;   Style:esEdit; Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_NODESTART;Style:esEdit; Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_NODEEND;  Style:esEdit; Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_DESCR;    Style:esEdit; Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_TAG;      Style:esEdit; Mask:emNoSpace; Length:0; List:''),
    (Name:TXT_PR_PUMPCUR;  Style:esEdit; Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_POWER;    Style:esEdit; Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR_SPEED;    Style:esEdit; Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PATTERN;     Style:esEdit; Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_INITSTATUS;Style:esComboList; Mask:emNone; Length:0;
     List:'Open'#13'Closed'),
    (Name:TXT_PR_EFFCUR;   Style:esEdit; Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_ENERPRICE;Style:esEdit; Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR_PRICEPAT; Style:esEdit; Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_FLOW;        Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_HEADLOSS;    Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_QUALITY;     Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_STATUS;   Style:esReadOnly;Mask:emNone; Length:0; List:''));

  ValveProps: array [0..14] of TPropRecord =
   ((Name:TXT_PR_VALVEID;   Style:esEdit;    Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_NODESTART; Style:esEdit;    Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_NODEEND;   Style:esEdit;    Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_DESCR;     Style:esEdit;    Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_TAG;       Style:esEdit;    Mask:emNoSpace; Length:0; List:''),
    (Name:TXT_PR_DIA;       Style:esEdit;    Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR__TYPE;     Style:esComboList; Mask:emNone;   Length:0;
     List:'PRV'#13'PSV'#13'PBV'#13'FCV'#13'TCV'#13'GPV'),
    (Name:TXT_PR__SETTING;  Style:esEdit;    Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_LOSSCOEF;  Style:esEdit;    Mask:emPosNumber; Length:0; List:''),
    (Name:TXT_PR_FIXSTATUS; Style:esComboList; Mask:emNone;   Length:0;
     List:'None'#13'Open'#13'Closed'),
    (Name:TXT_FLOW;         Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_VELOCITY;  Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_HEADLOSS;     Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_QUALITY;      Style:esReadOnly;Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_STATUS;    Style:esReadOnly;Mask:emNone; Length:0; List:''));

  LabelProps: array [0..6] of TPropRecord =
   ((Name:TXT_PR_TEXT;      Style:esEdit;      Mask:emNone; Length:0; List:''),
    (Name:TXT_PR_X;         Style:esEdit;      Mask:emNumber; Length:0; List:''),
    (Name:TXT_PR_Y;         Style:esEdit;      Mask:emNumber; Length:0; List:''),
    (Name:TXT_PR_ANCHORNODE;Style:esEdit;      Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_METERTYPE; Style:esComboList; Mask:emNone;     Length:0;
     List:'None'#13'Node'#13'Link'),
    (Name:TXT_PR_METERID;   Style:esEdit;      Mask:emNoSpace;  Length:MAXID; List:''),
    (Name:TXT_PR_FONT;      Style:esButton;    Mask:emNone; Length:0; List:''));

  HydraulicProps: array [0..19] of TPropRecord =
    ((Name:TXT_OP_FLOWUNITS;     Style:esComboList;  Mask:emNone; Length:0;
      List:'CFS'#13'GPM'#13'MGD'#13'IMGD'#13'AFD'#13 +
           'LPS'#13'LPM'#13'MLD'#13'CMH'#13'CMD'),
     (Name:TXT_OP_HLFORMULA;     Style:esComboList;  Mask:emNone; Length:0;
      List:'H-W'#13'D-W'#13'C-M'),
     (Name:TXT_OP_SPGRAVITY;     Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_OP_VISCOSITY; Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_OP_MAXTRIALS;     Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_OP_ACCURACY;      Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_OP_UNBALANCED;    Style:esComboList; Mask:emNone;  Length:0;
      List:'Stop'#13'Continue'),
     (Name:TXT_OP_DEFPATTERN;    Style:esEdit; Mask:emNoSpace;    Length:MAXID; List:''),
     (Name:TXT_OP_DEMANDMULT;    Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_OP_EMITTEXP;      Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_OP_STATUSREP;     Style:esComboList; Mask:emNone;  Length:0;
      List:'No'#13'Yes'#13'Full'),

     (Name:TXT_OP_HEADERROR;     Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_OP_FLOWCHANGE;    Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_OP_DEMANDMODEL;   Style:esComboList; Mask:emNone; Length:0; List:'DDA'#13'PDA'),
     (Name:TXT_OP_PRESSMIN;      Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_OP_PRESSREQ;      Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_OP_PRESSEXP;      Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_OP_CHECKFREQ;     Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_OP_MAXCHECK;      Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_OP_DAMPLIMIT;     Style:esEdit; Mask:emPosNumber; Length:0; List:''));

  QualityProps: array[0..4] of TPropRecord =
    ((Name:TXT_OP_PARAMETER;     Style:esComboEdit; Mask:emNone; Length:0;
      List:'None'#13'Chemical'#13'Trace'#13'Age'),
     (Name:TXT_OP_MASSUNIT;      Style:esComboList; Mask:emNone; Length:0;
      List:'mg/L'#13'ug/L'#13),
     (Name:TXT_OP_DIFFUSIV;      Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_OP_TRACENODE;     Style:esEdit; Mask:emNoSpace; Length:MAXID; List:''),
     (Name:TXT_OP_TOLQ;          Style:esEdit; Mask:emPosNumber; Length:0; List:''));

  ReactionProps: array[0..5] of TPropRecord =
    ((Name:TXT_OP_BULKORDER;     Style:esEdit; Mask:emNumber; Length:0; List:''),
     (Name:TXT_OP_WALLORDER;     Style:esComboList; Mask:emNone; Length:0;
      List:'Zero'#13'First'),
     (Name:TXT_OP_GLOBALBULK;    Style:esEdit; Mask:emNumber; Length:0; List:''),
     (Name:TXT_OP_GLOBALWALL;    Style:esEdit; Mask:emNumber; Length:0; List:''),
     (Name:TXT_OP_LIMITCONC;     Style:esEdit; Mask:emNumber; Length:0; List:''),
     (Name:TXT_OP_WALLCORREL;    Style:esEdit; Mask:emNumber; Length:0; List:''));

  TimeProps: array [0..8] of TPropRecord =
    ((Name:TXT_OP_DURATION;      Style:esEdit; Mask:emNone; Length:0; List:''),
     (Name:TXT_OP_HSTEP;         Style:esEdit; Mask:emNone; Length:0; List:''),
     (Name:TXT_OP_QSTEP;         Style:esEdit; Mask:emNone; Length:0; List:''),
     (Name:TXT_OP_PSTEP;         Style:esEdit; Mask:emNone; Length:0; List:''),
     (Name:TXT_OP_PATSTART;      Style:esEdit; Mask:emNone; Length:0; List:''),
     (Name:TXT_OP_RSTEP;         Style:esEdit; Mask:emNone; Length:0; List:''),
     (Name:TXT_OP_REPSTART;      Style:esEdit; Mask:emNone; Length:0; List:''),
     (Name:TXT_OP_CLOCKSTART;    Style:esEdit; Mask:emNone; Length:0; List:''),
     (Name:TXT_OP_STAT;          Style:esComboList; Mask:emNone; Length:0;
      List:'None'#13'Average'#13'Minimum'#13'Maximum'#13'Range'));

  EnergyProps: array[0..3] of TPropRecord =
    ((Name:TXT_OP_PEFF;          Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_OP_ENERPRICE;     Style:esEdit; Mask:emPosNumber; Length:0; List:''),
     (Name:TXT_PR_PRICEPAT;      Style:esEdit; Mask:emNoSpace;  Length:MAXID; List:''),
     (name:TXT_OP_DEMANDCHARGE;  Style:esEdit; Mask:emPosNumber; Length:0; List:''));

//Default property values for network objects
  DefJunc:   array[0..10] of String =               //Index in JuncProps:
                  ('',        //0 - Comment               (3)
                   '',        //1 - Tag                   (4)
                   '0',       //2 - Elevation             (5)
                   '0',       //3 - Primary Demand        (6)
                   '',        //4 - Prmary Demand Pattern (7)
                   '1',       //5 - Demand Categories     (8)
                   '',        //6 - Emitter Coeff.        (9)
                   '',        //7 - Initial Quality       (10)
                   '',        //8 - Source Quality        (11)
                   '',        //9 - Source Pattern
                   'CONCEN'); //10 - Source Type

  DefReserv: array [0..7] of String =               //Index in ReservProps:
                    ('',        //0 - Comment             (3)
                     '',        //1 - Tag                 (4)
                     '0',       //2 - Head                (5)
                     '',        //3 - Head Pattern        (6)
                     '',        //4 - Initial Quality     (7)
                     '',        //5 - Source Quality      (8)
                     '',        //6 - Source Pattern
                     'CONCEN'); //7 - Source Type

  DefTank:   array [0..16] of String =              //Index in TankProps:
                   ('',        //0 - Comment              (3)
                    '',        //1 - Tag                  (4)
                    '0',       //2 - Elevation            (5)
                    '10',      //3 - Initial Level        (6)
                    '0',       //4 - Min. Level           (7)
                    '20',      //5 - Max. Level           (8)
                    '50',      //6 - Diameter             (9)
                    '',        //7 - Min. Volume          (10)
                    '',        //8 - Volume Curve         (11)
                    'Mixed',   //9 - Mixing Model         (12)
                    '',        //10 - Mixing Fraction     (13)
                    '',        //11 - Bulk React. Coeff.  (14)
                    '',        //12 - Initial Quality     (15)
                    '',        //13 - Source Quality      (16)
                    '',        //14 - Source Pattern
                    'CONCEN',  //15 - Source Type
                    'No');     //16 - Can Overflow

  DefPipe:   array [0..8] of String =               //Index in PipeProps:
                   ('',        //0 - Comment              (3)
                    '',        //1 - Tag                  (4)
                    '1000',    //2 - Length               (5)
                    '12',      //3 - Diameter             (6)
                    '100',     //4 - Roughness Coeff.     (7)
                    '0',       //5 - Minor Loss Coeff.    (8)
                    'Open',    //6 - Status               (9)
                    '',        //7 - Bulk React. Coeff.   (10)
                    '');       //8 - Wall React. Coeff.   (11)

  DefPump:   array [0..9] of String =               //Index in PumpProps:
                   ('',        //0 - Comment              (3)
                    '',        //1 - Tag                  (4)
                    '',        //2 - Head Curve           (5)
                    '',        //3 - Power                (6)
                    '',        //4 - Speed                (7)
                    '',        //5 - Speed Pattern        (8)
                    'Open',    //6 - Status               (9)
                    '',        //7 - Effic. Curve         (10)
                    '',        //8 - Energy Price         (11)
                    '');       //9 - Price Pattern        (12)

  DefValve:  array [0..6] of String =               //Index in ValveProps
                   ('',        //0 - Comment              (3)
                    '',        //1 - Tag                  (4)
                    '12',      //2 - Diameter             (5)
                    'PRV',     //3 - Valve Type           (6)
                    '0',       //4 - Setting              (7)
                    '0',       //5 - Minor Loss Coeff.    (8)
                    'None');   //6 - Fixed Status         (9)

  DefOptions: array [0..MAXOPTIONS] of String =
                    ('LPS',         {0-Flow units}
                     'D-W',         {1-Headloss formula}
                     '1',           {2-Specific grav.}
                     '1',           {3-Rel. Viscosity}
                     '40',          {4-Max. trials}
                     '0.001',       {5-Accuracy}
                     'Continue',    {6-Unbalanced}
                     '1',           {7-Global Pattern}
                     '1.0',         {8-Demand Multiplier}
                     '0.5',         {9-Emitter Exponent}
                     'No',          {10-Status Report}

                     'None',        {11-Quality param.}
                     'mg/L',        {12-Quality units}
                     '1',           {13-Rel. Diffusivity}
                     '',            {14-Trace node}
                     '0.01',        {15-Quality tolerance}
                     '',            {16-Max. Segments (not used)}

                     '1',           {17-Bulk react. order}
                     'First',       {18-Wall react. order}
                     '0',           {19-Global Kbulk}
                     '0',           {20-Global Kwall}
                     '0',           {21-Limiting quality}
                     '0',           {22-Wall correl.}

                     '0',           {23-Duration}
                     '1:00',        {24-Hyd. timestep}
                     '0:05',        {25-Qual. timestep}
                     '1:00',        {26-Pat. timestep}
                     '0:00',        {27-Pat. start}
                     '1:00',        {28-Rpt. timesetp}
                     '0:00',        {29-Rpt. start}
                     '12 am',       {30-Start time}
                     'None',        {31-Rpt. statistic}

                     '75',          {32-Pump efficiency}
                     '0',           {33-Energy price}
                     '',            {34-Price pattern}
                     '0',           {35-Demand charge}

                     '2',           {36-CHECKFREQ}
                     '10',          {37-MAXCHECK}
                     '0',           {38-DAMPLIMIT}

                     '0',           {39-Max Head Error}
                     '0',           {40-Max Flow Change}
                     'DDA',         {41-Demand Model}
                     '0',           {42-Minimum Pressure}
                     '0.1',         {43-Required Pressure}
                     '0.5');        {44-Pressure Exponent}

//Node reporting variables
//  Name         = name of variable
//  Source       = vsInput if input variable;
//               = vsOutput if computed variable
//  SourceIndex  = for input variables, index in Data[] array;
//                 for computed variables, order in which written
//                 by solver to output file (see Output File topic
//                 in EPANET Programmers Toolkit Help file)
//  DefIntervals = default legend intervals (# intervals =
//                 MAXINTERVALS constant defined in Uglobals.pas)
  NodeVariable: array[0..NODEQUAL] of TNodeVariable =
    (
     (Name:TXT_NOVIEW;
      Source:vsInput;
      SourceIndex:(-1,-1,-1);
      DefIntervals:(0,0,0,0)),

     (Name:TXT_NODE_ELEVATION;
      Source:vsInput;
      SourceIndex:(2,2,2);
      DefIntervals:(25,50,75,100)),

     (Name:TXT_COLL_LABELS_BASEDEMAND;
      Source:vsInput;
      SourceIndex:(3,-1,-1);
      DefIntervals:(25,50,75,100)),

     (Name:TXT_PR_INITQ;
      Source:vsInput;
      SourceIndex:(7,4,12);
      DefIntervals:(0.25,0.5,0.75,1.0)),

     (Name:TXT_DEMAND;
      Source:vsOutput;
      SourceIndex:(1,1,1);
      DefIntervals:(25,50,75,100)),

     (Name:TXT_HEAD;
      Source:vsOutput;
      SourceIndex:(2,2,2);
      DefIntervals:(25,50,75,100)),

     (Name:TXT_PR_PRESSURE;
      Source:vsOutput;
      SourceIndex:(3,3,3);
      DefIntervals:(25,50,75,100)),

     (Name:TXT_QUALITY;
      Source:vsOutput;
      SourceIndex:(4,4,4);
      DefIntervals:(0.25,0.5,0.75,1.0))
    );

//Link reporting variables -- see notes above for Node reporting variables
  LinkVariable: array[0..LINKSET] of TLinkVariable =
    (
     (Name:TXT_NOVIEW;
      Source:vsInput;
      SourceIndex:(-1,-1,-1);
      DefIntervals:(0,0,0,0)),

     (Name:TXT_LENGTH;
      Source:vsInput;
      SourceIndex:(2,-1,-1);
      DefIntervals:(100,500,1000,5000)),

     (Name:TXT_DIAMETER;
      Source:vsInput;
      SourceIndex:(3,-1,2);
      DefIntervals:(6,12,24,36)),

     (Name:TXT_ROUGHNESS;
      Source:vsInput;
      SourceIndex:(4,-1,-1);
      DefIntervals:(50,75,100,125)),

     (Name:TXT_PR_BULKCOEF;
      Source:vsInput;
      SourceIndex:(7,-1,-1);
      DefIntervals:(0.25,0.5,0.75,1.0)),

     (Name:TXT_PR_WALLCOEF;
      Source:vsInput;
      SourceIndex:(8,-1,-1);
      DefIntervals:(0.25,0.5,0.75,1.0)),

     (Name:TXT_FLOW;
      Source:vsOutput;
      SourceIndex:(1,1,1);
      DefIntervals:(25,50,75,100)),

     (Name:TXT_PR_VELOCITY;
      Source:vsOutput;
      SourceIndex:(2,2,2);
      DefIntervals:(0.01,0.1,1.0,2.0)),

     (Name:TXT_PR_UNITHL;
      Source:vsOutput;
      SourceIndex:(3,3,3);
      DefIntervals:(0.025,0.05,0.075,0.1)),

     (Name:TXT_PR_FRICTFACT;
      Source:vsOutput;
      SourceIndex:(8,8,8);
      DefIntervals:(0.001, 0.01, 0.1, 1.0)),

     (Name:TXT_PR_REACTRATE;
      Source:vsOutput;
      SourceIndex:(7,7,7);
      DefIntervals:(0.1, 0.5, 1.0, 5.0)),

     (Name:TXT_QUALITY;
      Source:vsOutput;
      SourceIndex:(4,4,4);
      DefIntervals:(0.25,0.5,0.75,1.0)),

     (Name:TXT_PR_STATUS;
      Source:vsOutput;
      SourceIndex:(5,5,5);
      DefIntervals:(0,0,0,0)),

     (Name:TXT_SETTING;
      Source:vsOutput;
      SourceIndex:(6,6,6);
      DefIntervals:(0,0,0,0))
    );

// Quality variables (used to set default legend intervals for
// the different types of WQ parameters)
  QualVariable: array[0..MAXQUALPARAMS] of TNodeVariable =
    (
     (Name:TXT_NONE;
      Source:vsInput;
      SourceIndex:(-1,-1,-1);
      DefIntervals:(0.2,0.4,0.6,0.8)),
     (Name:TXT_CHEMICAL;
      Source:vsInput;
      SourceIndex:(-1,-1,-1);
      DefIntervals:(0.2,0.4,0.6,0.8)),
     (Name:TXT_TRACE;
      Source:vsInput;
      SourceIndex:(-1,-1,-1);
      DefIntervals:(20,40,60,80)),
     (Name:TXT_AGE;
      Source:vsInput;
      SourceIndex:(-1,-1,-1);
      DefIntervals:(2,6,12,24))
    );

//Lazarus - HC to HTML arrays
{$IFNDEF WINDOWS}
  HelpID : array [0..134] of integer = (100,102,103,104,106,107,108,109,118,119,
  120,132,133,134,135,137, 138,139,140,142,143,144,145,146,148,149,150,153,154,
  155,156,158,160,161,162, 163,164,165,166,167,168,169,170,171,174,175,176,177,
  178,179,180,181,182,183, 184,185,186,190,191,192,193,194,195,196,197,198,199,
  200,201,202,203,204,206, 208,212,213,216,217,230,231,232,233,234,245,250,251,
  252,253,263,264,265,266, 267,268,269,270,271,272,273,274,275,276,277,278,279,
  280,282,284,286,287,288, 291,293,298,299,300,301,302,303,304,306,307,308,320,
  340,500,510,520,580,590, 600,610,620,630,1000);

  HelpPage : array [0..134] of String = ('contents.htm', 'what_is_epanet.htm',
  'network_components.htm',
  'water_quality_modeling_capabil.htm', 'menu_bar.htm', 'toolbars.htm',
  'network_map.htm', 'status_bar.htm', 'types_of_objects.htm', 'junctions.htm',
  'property_editor.htm', 'analysis_options.htm', 'creating_a_new_project.htm',
  'opening_an_existing_project.htm', 'saving_a_project.htm',
  'program_preferences-general.htm', 'project_defaults-id_labels.htm',
  'project_defaults-hydraulics.htm', 'project_defaults-properties.htm',
  'program_preferences-formats.htm', 'viewing_a_project_summary.htm',
  'analysis_options-hydraulics.htm', 'analysis_options-quality.htm',
  'analysis_options-reactions.htm', 'analysis_options-times.htm',
  'analysis_options-energy.htm', 'hydraulic_modeling_capabilitie.htm',
  'setting_program_preferences.htm', 'junction_properties.htm',
  'reservoir_properties.htm', 'tank_properties.htm', 'data_browser.htm',
  'pipe_properties.htm', 'pump_properties.htm', 'valve_properties.htm',
  'setting_project_defaults.htm', 'simple_controls.htm',
  'rule-based_controls.htm', 'selecting_a_map_view.htm',
  'setting_the_map_s_dimensions.htm', 'zooming_the_map.htm',
  'panning_the_map.htm', 'overview_map.htm', 'map_legends.htm',
  'troubleshooting_results.htm', 'running_an_analysis.htm',
  'steps_in_using_epanet.htm', 'status_report.htm',
  'registering_calibration_data.htm', 'adding_an_object.htm',
  'adding_a_node.htm', 'adding_a_link.htm', 'adding_a_map_label.htm',
  'adding_a_curve.htm', 'adding_a_time_pattern.htm', 'selecting_an_object.htm',
  'editing_an_object.htm', 'setting_analysis_options.htm',
  'finding_an_object.htm', 'map_display_options.htm', 'map_options-nodes.htm',
  'map_options-links.htm', 'map_options-labels.htm', 'map_options-symbols.htm',
  'map_options-arrows.htm', 'map_options-notation.htm',
  'map_options-background.htm', 'deleting_an_object.htm',
  'moving_an_object.htm', 'selecting_a_group_of_objects.htm',
  'editing_a_group_of_objects.htm', 'viewing_results_on_the_map.htm',
  'viewing_results_with_a_graph.htm', 'creating_a_graph.htm',
  'customizing_a_graph.htm', 'viewing_results_with_a_table.htm',
  'demand_editor.htm', 'legend_editor.htm', 'graph_options_dialog.htm',
  'contour_options_dialog.htm', 'controls_editor.htm', 'curve_editor.htm',
  'pattern_editor.htm', 'source_quality_editor.htm',
  'graph_options-general.htm', 'graph_options-axes.htm',
  'graph_options-legend.htm', 'graph_options-series.htm',
  'modifying_a_table.htm', 'table_options-type.htm',
  'table_options-columns.htm', 'table_options-filters.htm',
  'viewing_special_reports.htm', 'energy_report.htm', 'calibration_report.htm',
  'calibration_report-statistics.htm', 'calibration_report-correlation.htm',
  'calibration_report-mean_compar.htm', 'reaction_report.htm',
  'selecting_a_printer.htm', 'setting_the_page_format.htm', 'print_preview.htm',
  'printing_the_current_view.htm', 'importing_a_scenario.htm',
  'exporting_a_scenario.htm', 'exporting_the_network_map.htm',
  'group_edit_dialog_box.htm', 'map_label_properties.htm',
  'calibration_file.htm', 'graph_selection_dialog_box.htm',
  'map_dimensions_dialog.htm', 'project_scenarios.htm', 'us_units.htm',
  'error_messages.htm', 'exporting_to_text_file.htm', 'copying_a_view.htm',
  'copy_dialog.htm', 'importing_a_network_map.htm',
  'copying_and_pasting_objects.htm', 'map_browser.htm',
  'creating_a_calibration_report.htm', 'viewing_a_calibration_report.htm',
  'metric_units.htm', 'reference.htm', 'project_browser.htm', 'valve_types.htm',
  'pump_curve_types.htm', 'head_loss_formulas.htm', 'pump_problems.htm',
  'disconnected_network.htm', 'system_unbalanced.htm', 'negative_pressures.htm',
  'unsolveable_equations.htm', 'demand_models.htm', 'epanet_tutorial.htm');
{$ENDIF}

