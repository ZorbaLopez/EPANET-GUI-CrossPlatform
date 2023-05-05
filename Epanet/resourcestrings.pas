unit ResourceStrings;

{$mode objfpc}

{-------------------------------------------------------------------}
{                    Unit:    ResourceStrings.pas                   }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Author: Zorba Lopez Rivera                     }
{                    Date:    15/05/23                              }
{    Unit containing text strings to allow for translations         }
{-------------------------------------------------------------------}

interface

uses
  Classes, SysUtils;

resourcestring

  // Strings from FMain.pas
  MSG_NO_MAP_FILE = 'Could not read map file %s';
  MSG_NO_PUMPS = 'Cannot generate an Energy Report. Network contains no pumps.';
  MSG_NO_CALIB_DATA = 'No calibration data has been registered for this project.';
  MSG_NO_INPUT_FILE = 'Input file no longer exists.';
  MSG_NOT_EPANET_FILE = 'Not an EPANET file.';
  MSG_READONLY = '%s is read-only.'#10 + 'Use File >> Save As command to save it under a different name.';
  MSG_FIND_BACKDROP = 'Could not find backdrop file %s. Do you want to search for it?';

  TXT_MAIN_CAPTION = 'EPANET 2.2';
  TXT_AUTOLENGTH = 'Auto-Length %s';
  TXT_STATUS_REPORT = 'Status Report';
  TXT_SAVE_CHANGES = 'Save changes made to current project?';
  TXT_WARNING = '  WARNING:';
  TXT_TITLE = 'TITLE:';
  TXT_NOTES = 'NOTES:';
  TXT_HIDE = '&Hide';
  TXT_SHOW = '&Show';

  TXT_OPEN_PROJECT_TITLE = 'Open a Project';
  TXT_OPEN_MAP_TITLE = 'Open a Map';
  TXT_OPEN_SCENARIO_TITLE = 'Open a Scenario';
  TXT_OPEN_NETWORK_TITLE = 'Open a Network';
  TXT_SAVE_PROJECT_TITLE = 'Save Project As';
  TXT_SAVE_SCENARIO_TITLE = 'Save Scenario As';
  TXT_SAVE_NETWORK_TITLE = 'Save Network As';

  TXT_OPEN_PROJECT_FILTER = 'Network files (*.NET)|*.NET;*.net;*.[Nn][Ee][Tt]|Input file (*.INP)|*.INP;*.inp;*.[Ii][Nn][Pp]|' + 'Backup files (*.BAK)|*.BAK;*.bak;*.[Bb][Aa][Kk]|All files|*.*';
  TXT_SAVE_PROJECT_FILTER = 'Network files (*.NET)|*.NET;*.net;*.[Nn][Ee][Tt]|All files|*.*';
  TXT_NETWORK_FILTER = 'Input files (*.INP)|*.INP;*.inp;*.[Ii][Nn][Pp]|All files|*.*';
  TXT_SCENARIO_FILTER = 'Scenario files (*.SCN)|*.SCN;*.scn;*.[Ss][Cc][Nn]|All files|*.*';
  TXT_MAP_FILTER  = 'Map files (*.MAP)|*.MAP;*.map;*.[Mm][Aa][Pp]|All files|*.*';

  //Others not included
  TXT_PAGE = 'Page ';


  // Strings from FBrowser.pas
  TXT_SINGLE_PERIOD = 'Single Period';
  TXT_STATISTIC = '%s Statistic';
  TXT_QUALITY = 'Quality';
  TXT_DELETE_OBJECT = 'Delete selected object?';
  TXT_DAY = 'Day %s, %s';
  TXT_HOURS = '%sHrs';
  TXT_TIME = 'Time';
  TXT_PERIOD = 'Period';
  TXT_MULTIPLIER = 'Multiplier';
  TXT_CONTINUED = ' (continued)';
  TXT_XVALUES = 'X-Values';
  TXT_YVALUES = 'Y-Values';
  TXT_SIMPLE_CONTROLS = 'Simple Controls';
  TXT_RULE_CONTROLS = 'Rule-Based Controls';

  //Others not included
  TXT_AVERAGE = 'Average';
  TXT_MINIMUM = 'Minimum';
  TXT_MAXIMUM = 'Maximum';
  TXT_RANGE = 'Range';


  // Strings from FMap.pas
  MSG_NO_FIND_BACKDROP = 'Could not find backdrop file %s';
  MSG_NO_READ_BACKDROP = 'Could not read backdrop file %s';
  TXT_RESTORE_JUNCS = 'Restoring display of junctions on the map.';
  TXT_RESTORE_LABELS = 'Restoring display of labels on the map.';
  TXT_MAP_NODE = 'Node ';
  TXT_MAP = 'Netwotk Map';


  // Strings from Ufileio.pas
  MSG_READING_INPUT = 'Reading input file...';
  MSG_READ_ERR = 'Invalid file format.';
  FILE_ERR1a = 'Calibration file %s does not exist.';
  FILE_ERR2a = 'Could not access calibration file %s.'#10#10'Check that file is not in use by another program.';
  FILE_ERR3a = 'Could not open file %s.'#10#10'Check that file is not in use by another program.';
  FILE_ERR4a = 'Could not read file %s.'#10#10'File is probably not a valid EPANET project or input file.';
  FILE_ERR5  = 'Data could not be saved to file.';


  // Strings from Uimport.pas
  MSG_INVALID_FILE = '%s is not a valid EPANET data file.';
  MSG_IMPORTING_DATA = 'Importing network data...';
  MSG_MAP_FILE = 'Map file %s does not exist.';
  MSG_READING_MAP_FILE = 'Reading map file...';
  TXT_ERROR = 'Error %s at line %s:';
  TXT_MORE_ERRORS = '%s more errors found in file.';
  TXT_ERROR_REPORT = 'Error Report for File %s';


  // Strings from Uinput.pas
  FMT_NODE_EXISTS = 'Node %s already exists.';
  FMT_LINK_EXISTS = 'Link %s already exists.';
  MSG_ALREADY_HAVE = 'You already have a %s named %s';
  MSG_CONFIRM_DELETE = 'Are you sure that you want to delete all objects in the selected region?';
  TXT_INP_TIME = 'Hrs:Min';
  TXT_VALUE = 'Value';
  TXT_LABEL_EDITOR = 'Label Editor';
  TXT_DEMAND_EDITOR = 'Demands for Junction %s';
  TXT_SOURCE_EDITOR = 'Source Editor for Node %s';
  TXT_OPTIONS_EDITOR = '%s Options';
  TXT_CONTROLS_EDITOR = 'Simple Controls Editor';
  TXT_RULES_EDITOR = 'Rule-Based Controls Editor';
  TXT_WERE_UPDATED = '%s %ss were updated.';
  TXT_NO_NODE = 'There is no node named %s';
  TXT_NO_LINK = 'There is no link named %s';
  TXT_NAMED = ' named ';
  TXT_NOT_A_NUMBER = '-%s- is not a valid number.';
  TXT_BAD_CONNECTION = 'Node cannot be connected to itself.';


  // Strings from Dpattern.pas
  FMT_LEFT_AXIS = 'Avg. = %.2f';
  TXT_REPLACE_ALL = 'Replace all references to Pattern %s with %s?';
  TXT_TIME_PERIOD = 'Time Period';
  TXT_AXIS_TITLE =  'Time (Time Period = %s hrs)';
  TXT_OPEN_PATTERN_TITLE = 'Open a Pattern';
  TXT_SAVE_PATTERN_TITLE = 'Save Pattern As';
  TXT_PATTERN_FILTER = 'Pattern files (*.PAT)|*.PAT;*.pat;*.[Pp][Aa][Tt]|All files|*.*';
  TXT_PATTERN_HEADER = 'EPANET Pattern Data';


  // Strings from Dcurve.pas
  MSG_OUT_OF_ORDER = '%s values are not in ascending order.';
  MSG_BAD_CURVE = 'Illegal pump curve. Continue editing?';
  FMT_EQN = ' Head = %f%-.4g(Flow)^%f';
  TXT_CUR_PERCENT = ' (%)';
  TXT_CUBIC = ' (cubic %s)';
  TXT_CUR_PUMP = 'PUMP';
  TXT_BAD_CURVE = ' Illegal pump curve.';
  TXT_OPEN_CURVE_TITLE = 'Open a Curve';
  TXT_SAVE_CURVE_TITLE = 'Save Curve As';
  TXT_CURVE_FILTER = 'Curve files (*.CRV)|*.CRV;*.crv;*.[Cc][Rr][Vv]|All files|*.*';
  TXT_CURVE_HEADER = 'EPANET Curve Data';

  //Others not included

  TXT_CUR_HEIGHT = ' Height';
  TXT_CUR_FLOW = ' Flow';
  TXT_CUR_VOLUME = ' Volume';
  TXT_CUR_HEAD = ' Head';
  TXT_CUR_EFFICIENCY = ' Efficiency';
  TXT_CUR_HEADLOSS = ' Headloss';


  // Strings from Dmapdim.pas
  MSG_ILLEGAL_MAP_LIMITS = 'Illegal map limits.';
  //Others not included
  TXT_DFEET = 'Feet';
  TXT_DMETER = 'Meters';
  TXT_DDEGREE = 'Degrees';
  TXT_DNONE = 'None';

  // Strings from Fproped.pas
  TXT_PROPERTY = 'Property';


  // Strings from Dlegend.pas
  MSG_RANGE_TOO_SMALL = 'Range too small.';
  MSG_NO_INTERVALS = 'No intervals specified.';


  // Strings from Fstatus.pas
  MSG_1 = 'Unable to load more than %s lines into Status Report.';


  // Strings from Fgraph.pas
  TXT_NODE = 'Node';
  TXT_LINK = 'Link';
  TXT_SERIES = 'Series';
  TXT_COMPUTED = 'Computed';
  TXT_OBSERVED = 'Observed';
  TXT_TIME_HRS = 'Time (hours)';
  TXT_FLOW = 'Flow';
  TXT_HEAD = 'Head';
  TXT_HEADLOSS = 'Headloss';
  TXT_DISTANCE = 'Distance';
  TXT_FEET = ' (feet)';
  TXT_METERS = ' (meters)';
  TXT_NO_REACTION = 'No reactions occurred';
  TXT_AVG_RATES = 'Average Reaction Rates (kg/day)';
  TXT_BULK = 'Bulk';
  TXT_WALL = 'Wall';
  TXT_TANKS = 'Tanks';
  TXT_PRODUCED = 'Produced';
  TXT_CONSUMED = 'Consumed';
  TXT_TIME_SERIES_PLOT = ' Time Series Plot - %s %s';
  TXT_FREQUENCY_PLOT = ' Frequency Plot - %s';
  TXT_PROFILE_PLOT = ' Profile Plot - %s';
  TXT_REACT_REPORT = 'Reaction Report';
  TXT_SYSTEM_FLOW = ' System Flow Balance';
  TXT_DOES_NOT_EXIST = ' does not exist.';
  TXT_DISTRIBUTION = 'Distribution of %s';
  TXT_PERCENT_LESS = 'Percent Less Than';
  TXT_PERCENT_NLESS = 'Percent of Nodes Less Than';
  TXT_PERCENT_LLESS = 'Percent of Links Less Than';
  TXT_FREQUENCY = 'Frequency';
  TXT_PROFILE = 'Profile of %s';
  TXT_FOR_LINK = 'for Link %s';
  TXT_FOR_SELECTED_LINKS = 'for Selected Links';
  TXT_FOR_NODE = 'for Node %s';
  TXT_FOR_SELECTED_NODES = 'for Selected Nodes';
  TXT_AT = ' at ';
  FMT_INFLOW = 'Inflow Rate = %.0f';


  // Strings from Dfind.pas
  TXT_NO_SUCH_OBJECT = 'There is no such object on the map';
  TXT_NO_SOURCE_NODES = 'There are no WQ source nodes.';
  TXT_LINK_NOT_ON_MAP = 'Link exists but is not on the map.';
  TXT_NODE_NOT_ON_MAP = 'Node exists but is not on the map.';

  //Others not included
  TXT_ADJ_LINKS = 'Adjacent Links';
  TXT_ADJ_NODES = 'Adjacent Nodes';
  TXT_SOURCE_NODES = 'Source Nodes';
  TXT_SOURCES = 'Sources';

  // Strings from Dgrouped.pas
  TXT_JUNCTIONS = 'Junctions';
  TXT_PIPES     = 'Pipes';
  TXT_ACTION_ITEMS_REPLACE = 'Replace';
  TXT_ACTION_ITEMS_MULTIPLY = 'Multiply';
  TXT_ACTION_ITEMS_ADD = 'Add';
  TXT_ACTION_ADVERB_WITH =  'with';
  TXT_ACTION_ADVERB_BY =  'by';
  TXT_ACTION_ADVERB_TO =  'to';


  // Strings from Dcalib1.pas
  TXT_PARAMETER = 'Parameter';
  TXT_NAME_OF_FILE = 'Name of Calibration File';
  TXT_SELECT_FILE = 'Select a Calibration File';
  TXT_FILE_FILTER = 'Data files (*.DAT)|*.DAT;*.dat;*.[Dd][Aa][Tt]|All files|*.*';


  // Strings from Fcalib.pas
  TXT_REPORT = 'Calibration Report - %s';
  TXT_NETWORK = 'Network';
  TXT_NO_DATA = ' *** No observed data during simulation period. ***';
  TXT_CORRELATION = '  Correlation Between Means: ';
  TXT_CAL_TITLE = ' Calibration Statistics for %s';
  TXT_HEADING1 = '                Num    Observed    Computed    Mean     RMS';
  TXT_HEADING2 = '  Location      Obs        Mean        Mean   Error   Error';
  TXT_HEADING3 = '  ---------------------------------------------------------';

  //Others not included
  TXT_CALPLOT_TITLE = 'Comparison of Mean Values for %s';
  TXT_CORPLOT_TITLE = 'Correlation Plot for %s';


  // Strings from Ftable.pas
  TXT_NETWORK_NODES = 'Network Table - Nodes';
  TXT_NETWORK_LINKS = 'Network Table - Links';
  TXT_NODE_SERIES = 'Time Series Table - Node %s';
  TXT_LINK_SERIES = 'Time Series Table - Link %s';
  TXT_NODE_ID = ' Node ID';
  TXT_LINK_ID = ' Link ID';
  TXT_TABLE_TIME = ' Time';
  TXT_TABLE_HOURS = ' Hours';
  TXT_ITEMS_WITH = '%s items with'#10'%s';

  //Others not included
  TXT_TABLE_JUNC = 'Junc ';
  TXT_TABLE_RESVR = 'Resvr ';
  TXT_TABLE_TANK = 'Tank ';
  TXT_TABLE_PIPE = 'Pipe ';
  TXT_TABLE_PUMP = 'Pump ';
  TXT_TABLE_VALVE = 'Valve ';


  // Strings from Dcalib2.pas
  TXT_AT_NODES = 'Measured at Nodes:';
  TXT_IN_LINKS = 'Measured in Links:';


  // Strings from Fsummary.pas
  //Others not included
  TXT_NO_JUNCTIONS     = ' Number of Junctions     ';
  TXT_NO_RESERVOIRS    = ' Number of Reservoirs    ';
  TXT_NO_TANKS         = ' Number of Tanks         ';
  TXT_NO_PIPES         = ' Number of Pipes         ';
  TXT_NO_PUMPS         = ' Number of Pumps         ';
  TXT_NO_VALVES        = ' Number of Valves        ';
  TXT_FLOWUNITS        = ' Flow Units              ';
  TXT_HEADLOSSFORMULA  = ' Headloss Formula        ';
  TXT_QUALITYPARAMETER = ' Quality Parameter       ';


  // Strings from Fcontour.pas
  MSG_TOO_FEW_NODES = 'Too few nodes to contour.';
  TXT_CONTOUR_PLOT = 'Contour Plot - %s%s%s';

  // Strings from Dtable.pas
  MSG_NO_NODE = 'There is no Node %s';
  MSG_NO_LINK = 'There is no Link %s';
  MSG_FILTER_LIMIT = 'Maximum number of filters has been reached.';
  TXT_CAPTION1 = 'Table Selection';
  TXT_CAPTION2 = 'Table Options';
  TXT_NODES = 'Network Nodes';
  TXT_LINKS = 'Network Links';
  TXT_SORTED_BY = 'Sorted by ';


  // Strings from Ddefault.pas
  TXT_OBJECT = 'Object';
  TXT_ID_PREFIX = 'ID Prefix';
  TXT_DEF_VALUE = 'Default Value';
  TXT_OPTION = 'Option';

  //Others not included
  TXT_TAB_LABELS = 'ID Labels';
  TXT_TAB_PROPERTIES = 'Properties';
  TXT_TAB_HYDRAULICS = 'Hydraulics';

  TXT_RESERVOIRS = 'Reservoirs';
  TXT_PUMPS = 'Pumps';
  TXT_VALVES = 'Valves';
  TXT_PATTERNS = 'Patterns';
  TXT_CURVES = 'Curves';
  TXT_ID_INCREMENT = 'ID Increment';

  TXT_NODE_ELEVATION = 'Node Elevation';
  TXT_TANK_DIAMETER = 'Tank Diameter';
  TXT_TANK_HEIGHT = 'Tank Height';
  TXT_PIPE_LENGTH = 'Pipe Length';
  TXT_PIPE_DIAMETER = 'Pipe Diameter';
  TXT_PIPE_ROUGHNESS = 'Pipe Roughness';


  // Strings from Dgraph.pas
  TXT_NO_VARIABLE = 'No parameter was selected';
  TXT_NO_ITEMS = 'No items to graph';
  TXT_TOO_MANY_ITEMS = 'Too many items to graph';
  TXT_NO_RESULTS = 'No results available to graph';
  TXT_NO_NODE_SELECTED = 'Must select a node object from Browser';
  TXT_NO_LINK_SELECTED = 'Must select a link object from Browser';
  TXT_OPEN_PROFILE_TITLE = 'Open Profile';
  TXT_SAVE_PROFILE_TITLE = 'Save Profile As';
  TXT_PROFILE_FILTER = 'Profile files (*.PRO)|*.PRO;*.pro;*.[Pp][Rr][Oo]|All files|*.*';
  TXT_PROFILE_FOR = 'Profile for %s';
  TXT_SAVE_PROFILE = 'Save Profile';
  TXT_PROFILE_ID = 'Profile identifier:';
  TXT_NODES_TO_GRAPH = 'Nodes to Graph';
  TXT_LINKS_TO_GRAPH = 'Links to Graph';
  //Others not included
  TXT_GTIME_SERIES = 'Time Series';
  TXT_GPROFILE_PLOT = 'Profile Plot';
  TXT_GCONTOUR_PLOT = 'Contour Plot';
  TXT_GFREQUENCY_PLOT = 'Frequency Plot';
  TXT_GSYSTEM_FLOW = 'System Flow';
  TXT_G_NODES = 'Nodes';
  TXT_G_LINKS = 'Links';


  // Strings from Ddemand.pas
  //Others not included
  TXT_COLL_LABELS_BASEDEMAND = 'Base Demand';
  TXT_COLL_LABELS_TIMEPATTERN = 'Time Pattern';
  TXT_COLL_LABELS_CATEGORY = 'Category';

  // Strings from Fsimul.pas
  TXT_STATUS_RUNING = 'Running EPANET simulator...';
  TXT_STATUS_NONE = 'Unable to run simulator.';
  TXT_STATUS_WRONGVERSION = 'Run was unsuccessful. Wrong version of simulator.';
  TXT_STATUS_FAILED = 'Run was unsuccessful due to system error.';
  TXT_STATUS_ERROR = 'Run was unsuccessful. See Status Report for reasons.';
  TXT_STATUS_WARNING = 'Warning messages were generated. See Status Report for details.';
  TXT_STATUS_SUCCESS = 'Run was successful.';
  TXT_STATUS_SHUTDOWN = 'Simulator performed an illegal operation and was shut down.';
  TXT_STATUS_CANCELLED = 'Run cancelled by user.';
  TXT_COMPILING = 'Compiling network data...';
  TXT_CHECKING = 'Checking network data...';
  TXT_REORDERING = 'Re-ordering network nodes...';
  TXT_SOLVING_HYD = 'Solving hydraulics at hour';
  TXT_SAVING_HYD  = 'Saving hydraulics at hour';
  TXT_SOLVING_WQ  = 'Solving quality at hour';


  // Strings from Fenergy.pas
  TXT__PUMP = ' Pump';
  TXT_DEMAND_CHARGE = 'Demand Charge';
  TXT_TOTAL_COST = 'Total Cost';
  TXT_perM3 = '/m3';
  TXT_perMGAL = '/Mgal';

  //Others not included
  TXT_PERCENT = 'Percent';
  TXT_KW_HR = 'kW-hr';
  TXT_PEAK = 'Peak';
  TXT_COST = 'Cost';

  TXT_PUMP = 'Pump';
  TXT_UTILIZATION = 'Utilization';
  TXT_EFFICIENCY = 'Efficiency';
  TXT_KW = 'kW';
  TXT_perDAY = '/day';

  TXT_KWH_MGAL = 'kW-hr/Mgal';
  TXT_AVG_KWH = 'Average kW';
  TXT_PEAK_KWH = 'Peak kW';
  TXT_COST_DAY = 'Cost/day';


  // Strings from Dquery.pas
  TXT_NODES_WITH = 'Find Nodes with';
  TXT_LINKS_WITH = 'Find Links with';
  TXT_ITEMS_FOUND = '%s items found';


  // Strings from Dprefers.pas
  MSG_NO_DIRECTORY = ' - directory does not exist.';
  MSG_SELECT_NUMBER_OF = 'Select number of decimal places to '#10'use when displaying computed results';
  MSG_LANG_RESTART = 'Language will be updated next time you open EPANET';


  // Strings from Dcopy.pas
  TXT_COPY = 'Copy %s';
  TXT_SAVE_AS = 'Save As';

  //Others not included
  TXT_FILTER_BMP = 'Bitmap files (*.BMP)|*.BMP;*.bmp;*.[Bb][Mm][Pp]|All files|*.*';
  TXT_FILTER_EMF = 'EMF files (*.EMF)|*.EMF;*.emf;*.[Ee][Mm][Ff]|All files|*.*';
  TXT_FILTER_TXT = 'Text files (*.TXT)|*.TXT;*.txt;*.[Tt][Xx][Tt]|All files|*.*';
  TXT_FILTER_SVG = 'SVG files (*.SVG)|*.SVG;*.svg;*.[Ss][Vv][Gg]|All files|*.*';

  TXT_CLIPBOARD = 'Clipboard';
  TXT_FILE = 'File';

  TXT_BITMAP = 'Bitmap';
  TXT_METAFILE = 'Metafile';
  TXT_DATATEXT = 'Data (Text)';
  TXT_SVG = 'SVG';

  // Strings from Dmapexp.pas
  TXT_SAVE_MAP_TITLE = 'Save Map As';

  //Others not included
  TXT_FILTER_DXF = 'DXF files (*.DXF)|*.DXF;*.dxf;*.[Dd][Xx][Ff]|All files|*.*';
  TXT_DXF_OPENC = 'Open circles';
  TXT_DXF_FILLC = 'Filled circles';
  TXT_DXF_FILLS = 'Filled squares';
  TXT_SVG_DESCR = 'Scalable Vector (.svg)';


  // Strings from Ureport.pas
  FMT18  = '  Page 1                                          %22s';
  FMT82  = '  Page %-4d %60s';
  FMT71  = 'Energy Usage:';
  FMT72  = '                  Usage   Avg.     Kw-hr      Avg.      Peak      Cost';
  FMT73  = 'Pump             Factor Effic.     %s        Kw        Kw      /day';
  FMT74  = '%45s Demand Charge: %9.2f';
  FMT75  = '%45s Total Cost:    %9.2f';
  TXT_INPUT_FILE = 'Input File: ';
  TXT_LINK_INFO = 'Link - Node Table:';
  TXT_NODE_RESULTS = 'Node Results';
  TXT_LINK_RESULTS = 'Link Results';
  TXT_ID = 'ID';
  TXT_START = 'Start';
  TXT_END = 'End';
  MSG_REPORT_SIZE1 = 'This full report will use over %s Mbytes of disk space. Do you wish to proceed?';
  MSG_WRITING_REPORT = 'Writing full report...';
  MSG_NO_WRITE = 'Could not write full report to file %s';
  TXT_REPORT_FILTER = 'Report files (*.RPT)|*.RPT;*.rpt;*.[Rr][Pp][Tt]|All files|*.*';

  // Strings from Dchart.pas
  //Others not included
  TXT_LINESTYLE_SOLID = 'Solid';
  TXT_LINESTYLE_DASH = 'Dash';
  TXT_LINESTYLE_DOT = 'Dot';
  TXT_LINESTYLE_DASHDOT = 'DashDot';
  TXT_LINESTYLE_DASHDOTDOT = 'DashDotDot';

  TXT_LEGENDPOS_TL = 'Top Left';
  TXT_LEGENDPOS_CL = 'Center Left';
  TXT_LEGENDPOS_BL = 'Bottom Left';
  TXT_LEGENDPOS_TC = 'Top Center';
  TXT_LEGENDPOS_BC = 'Bottom Center';
  TXT_LEGENDPOS_TR = 'Top Right';
  TXT_LEGENDPOS_CR = 'Center Right';
  TXT_LEGENDPOS_BR = 'Bottom Right';

  TXT_MARKSTYLE_NONE = 'None';
  TXT_MARKSTYLE_RECT = 'Rectangle';
  TXT_MARKSTYLE_CIRC = 'Circle';
  TXT_MARKSTYLE_CROS = 'Cross';
  TXT_MARKSTYLE_DCROS = 'Diagonal Cross';
  TXT_MARKSTYLE_STAR = 'Star';
  TXT_MARKSTYLE_LBRAC = 'Low Bracket';
  TXT_MARKSTYLE_HBRAC = 'High Bracket';
  TXT_MARKSTYLE_FBRAC = 'Left Bracket';
  TXT_MARKSTYLE_RBRAC = 'Right Bracket';
  TXT_MARKSTYLE_DIAM = 'Diamond';
  TXT_MARKSTYLE_TRIA = 'Triangle';
  TXT_MARKSTYLE_FTRIA = 'Left Triangle';
  TXT_MARKSTYLE_RTRIA = 'Right Triangle';
  TXT_MARKSTYLE_VBAR = 'Vertical Bar';
  TXT_MARKSTYLE_HBAR = 'Horizontal Bar';
  TXT_MARKSTYLE_POIN = 'Point';
  TXT_MARKSTYLE_DTRIA = 'Down Triangle';
  TXT_MARKSTYLE_HEXA = 'Hexagon';
  TXT_MARKSTYLE_FSTAR = 'Full Star';

  TXT_FILLSTYLE_SOLID = 'Solid';
  TXT_FILLSTYLE_CLEAR = 'Clear';
  TXT_FILLSTYLE_HORIZ = 'Horizontal';
  TXT_FILLSTYLE_VERTI = 'Vertical';
  TXT_FILLSTYLE_FDIAG = 'Forward Diagonal';
  TXT_FILLSTYLE_BDIAG = 'Back Diagonal';
  TXT_FILLSTYLE_CROSS = 'Cross';
  TXT_FILLSTYLE_DCROS = 'Diagonal Cross';

  TXT_STACKSTYLE_NONE = 'None';
  TXT_STACKSTYLE_SIDE = 'Side';
  TXT_STACKSTYLE_STAC = 'Stacked';
  TXT_STACKSTYLE_FSTA = 'Stacked 100%';

  TXT_LABELSTYLE_NONE = 'None';
  TXT_LABELSTYLE_VALU = 'Value';
  TXT_LABELSTYLE_PERC = 'Percent';
  TXT_LABELSTYLE_LABL = 'Label';
  TXT_LABELSTYLE_LBPC = 'Label & %';
  TXT_LABELSTYLE_LBVL = 'Label & Value';
  TXT_LABELSTYLE_LEGD = 'Legend';
  TXT_LABELSTYLE_PTOT = '% Total';
  TXT_LABELSTYLE_LBTO = 'Label & % Total';
  TXT_LABELSTYLE_XVAL = 'X Value';

  // Strings from Dmap.pas
  //Others not included in options
  TXT_MAPOPT_NODES = '  Nodes';
  TXT_MAPOPT_LINKS = '  Links';
  TXT_MAPOPT_LABELS = '  Labels';
  TXT_MAPOPT_NOTATION = '  Notation';
  TXT_MAPOPT_SYMBOLS = '  Symbols';
  TXT_MAPOPT_ARROWS = '  Flow Arrows';
  TXT_MAPOPT_BACKGROUND = '  Background';

  // Strings from Dcolramp.pas
  //Others not included in options
  TXT_RAINBOW = 'Rainbow';
  TXT_RED = 'Red';
  TXT_ORANGE = 'Orange';
  TXT_YELLOW = 'Yellow';
  TXT_GREEN = 'Green';
  TXT_BLUE = 'Blue';
  TXT_PURPLE = 'Purple';
  TXT_MAGENTA = 'Magenta';
  TXT_GRAY = 'Gray';

  // Strings from Dsource.pas
  //Others not included in options
  TXT_CONCENTRATION = 'Concentration';
  TXT_MASSBOOSTER = 'Mass Booster';
  TXT_SETPOINTBOOS = 'Set Point Booster';
  TXT_FLOWPACEDBOOS = 'Flow Paced Booster';

  // Strings from consts.txt
  TXT_JUNCTION = 'Junction';
  TXT_RESERVOIR = 'Reservoir';
  TXT_TANK = 'Tank';
  TXT_PIPE = 'Pipe';
  //TXT_PUMP = 'Pump';
  TXT_VALVE = 'Valve';
  TXT_LABEL = 'Label';
  TXT_PATTERN = 'Pattern';
  TXT_CURVE = 'Curve';
  TXT_CONTROL = 'Control';
  //TXT_OPTION = 'Option';
  TXT_LABELS = 'Labels';
  TXT_CONTROLS = 'Controls';
  TXT_OPTIONS = 'Options';

  TXT_SIMPLE = 'Simple';
  TXT_RULEBASED = 'Rule-Based';

  TXT_HYDRAULICS = 'Hydraulics';
  //TXT_QUALITY = 'Quality';
  TXT_REACTIONS = 'Reactions';
  TXT_TIMES = 'Times';
  TXT_ENERGY = 'Energy';

  TXT_CUR_VOLUM = 'VOLUME';
  //TXT_CUR_PUMP = 'PUMP'
  TXT_CUR_EFF = 'EFFICIENCY';
  TXT_CUR_HEADL = 'HEADLOSS';

  TXT_AUTOLENGTH_OFF = 'Off';
  TXT_AUTOLENGTH_ON = 'On';

  TXT_FILTER_BELOW = 'Below';
  TXT_FILTER_EQUAL = 'Equal To';
  TXT_FILTER_ABOVE = 'Above';

  // For TPropRecord
  // Name
  TXT_PR_JUNCID = '*Junction ID';
  TXT_PR_X = 'X-Coordinate';
  TXT_PR_Y = 'Y-Coordinate';
  TXT_PR_DESCR = 'Description';
  TXT_PR_TAG = 'Tag';
  TXT_PR__ELEV = '*Elevation';
     //TXT_COLL_LABELS_BASEDEMAND = 'Base Demand';
  TXT_PR_DEMANDPAT = 'Demand Pattern';
  TXT_PR_DEMANDCAT = 'Demand Categories';
  TXT_PR_EMITTCOEF = 'Emitter Coeff.';
  TXT_PR_INITQ = 'Initial Quality';
  TXT_PR_SOURCEQ = 'Source Quality';
  TXT_PR_ACTUALD = 'Actual Demand';
  TXT_PR_THEAD = 'Total Head';
  TXT_PR_PRESSURE = 'Pressure';
     //TXT_QUALITY = 'Quality';

  TXT_PR_RESVID = '*Reservoir ID';
     //TXT_PR_X = 'X-Coordinate';
     //TXT_PR_Y = 'Y-Coordinate';
     //TXT_PR_DESCR = 'Description';
     //TXT_PR_TAG = 'Tag';
  TXT_PR__THEAD = '*Total Head';
  TXT_PR_HEADPAT = 'Head Pattern';
     //TXT_PR_INITQ = 'Initial Quality';
     //TXT_PR_SOURCEQ = 'Source Quality';
  TXT_PR_NETINFLOW = 'Net Inflow';
  TXT_PR_ELEV = 'Elevation';
     //TXT_PR_PRESSURE = 'Pressure';
     //TXT_QUALITY = 'Quality';

  TXT_PR_TANKID = '*Tank ID';
     //TXT_PR_X = 'X-Coordinate';
     //TXT_PR_Y = 'Y-Coordinate';
     //TXT_PR_DESCR = 'Description';
     //TXT_PR_TAG = 'Tag';
  TXT_PR_SLAB   = '*Elevation';
  TXT_PR_LEVINI = '*Initial Level';
  TXT_PR_LEVMIN = '*Minimum Level';
  TXT_PR_LEVMAX = '*Maximum Level';
  TXT_PR_DIA = '*Diameter';
  TXT_PR_VOLMIN = 'Minimum Volume';
  TXT_PR_VOLCUR = 'Volume Curve';
  TXT_PR_CANOVER = 'Can Overflow';
  TXT_PR_MIXMOD = 'Mixing Model';
  TXT_PR_MIXFRA = 'Mixing Fraction';
  TXT_PR_REACTC = 'Reaction Coeff.';
     //TXT_PR_INITQ = 'Initial Quality';
     //TXT_PR_SOURCEQ = 'Source Quality';
     //TXT_PR_NETINFLOW = 'Net Inflow';
     //TXT_PR_ELEV = 'Elevation';
     //TXT_PR_PRESSURE = 'Pressure';
     //TXT_QUALITY = 'Quality';

  TXT_PR_PIPEID = '*Pipe ID';
  TXT_PR_NODESTART = '*Start Node';
  TXT_PR_NODEEND = '*End Node';
     //TXT_PR_DESCR = 'Description';
     //TXT_PR_TAG = 'Tag';
  TXT_PR_LENGTH = '*Length';
     //TXT_PR_DIA = '*Diameter';
  TXT_PR_ROUGHNESS = '*Roughness';
  TXT_PR_LOSSCOEF = 'Loss Coeff.';
  TXT_PR_INITSTATUS = 'Initial Status';
  TXT_PR_BULKCOEF = 'Bulk Coeff.';
  TXT_PR_WALLCOEF = 'Wall Coeff.';
     //TXT_FLOW = 'Flow';
  TXT_PR_VELOCITY = 'Velocity';
  TXT_PR_UNITHL = 'Unit Headloss';
  TXT_PR_FRICTFACT = 'Friction Factor';
  TXT_PR_REACTRATE = 'Reaction Rate';
     //TXT_QUALITY = 'Quality';
  TXT_PR_STATUS = 'Status';

  TXT_PR_PUMPID = '*Pump ID';
     //TXT_PR_NODESTART = '*Start Node';
     //TXT_PR_NODEEND = '*End Node';
     //TXT_PR_DESCR = 'Description';
     //TXT_PR_TAG = 'Tag';
  TXT_PR_PUMPCUR = 'Pump Curve';
  TXT_PR_POWER = 'Power';
  TXT_PR_SPEED = 'Speed';
     //TXT_PATTERN = 'Pattern';
     //TXT_PR_INITSTATUS = 'Initial Status';
  TXT_PR_EFFCUR = 'Effic. Curve';
  TXT_PR_ENERPRICE = 'Energy Price';
  TXT_PR_PRICEPAT = 'Price Pattern';
     //TXT_FLOW = 'Flow';
     //TXT_HEADLOSS = 'Headloss';
     //TXT_QUALITY = 'Quality';
     //TXT_PR_STATUS = 'Status';

  TXT_PR_VALVEID = '*Valve ID';
     //TXT_PR_NODESTART = '*Start Node';
     //TXT_PR_NODEEND = '*End Node';
     //TXT_PR_DESCR = 'Description';
     //TXT_PR_TAG = 'Tag';
     //TXT_PR_DIA = '*Diameter';
  TXT_PR__TYPE = '*Type';
  TXT_PR__SETTING = '*Setting';
     //TXT_PR_LOSSCOEF = 'Loss Coeff.';
  TXT_PR_FIXSTATUS = 'Fixed Status';
     //TXT_FLOW = 'Flow';
     //TXT_PR_VELOCITY = 'Velocity';
     //TXT_HEADLOSS = 'Headloss';
     //TXT_QUALITY = 'Quality';
     //TXT_PR_STATUS = 'Status';

  TXT_PR_TEXT = 'Text';
     //TXT_PR_X = 'X-Coordinate';
     //TXT_PR_Y = 'Y-Coordinate';
  TXT_PR_ANCHORNODE = 'Anchor Node';
  TXT_PR_METERTYPE = 'Meter Type';
  TXT_PR_METERID = 'Meter ID';
  TXT_PR_FONT = 'Font';

  TXT_OP_FLOWUNITS = 'Flow Units';
  TXT_OP_HLFORMULA = 'Headloss Formula';
  TXT_OP_SPGRAVITY = 'Specific Gravity';
  TXT_OP_VISCOSITY = 'Relative Viscosity';
  TXT_OP_MAXTRIALS = 'Maximum Trials';
  TXT_OP_ACCURACY = 'Accuracy';
  TXT_OP_UNBALANCED = 'If Unbalanced';
  TXT_OP_DEFPATTERN = 'Default Pattern';
  TXT_OP_DEMANDMULT = 'Demand Multiplier';
  TXT_OP_EMITTEXP = 'Emitter Exponent';
  TXT_OP_STATUSREP = 'Status Report';
  TXT_OP_HEADERROR = 'Max. Head Error';
  TXT_OP_FLOWCHANGE = 'Max. Flow Change';
  TXT_OP_DEMANDMODEL = 'Demand Model';
  TXT_OP_PRESSMIN = 'Minimum Pressure';
  TXT_OP_PRESSREQ = 'Required Pressure';
  TXT_OP_PRESSEXP = 'Pressure Exponent';
  TXT_OP_CHECKFREQ = 'CHECKFREQ';
  TXT_OP_MAXCHECK = 'MAXCHECK';
  TXT_OP_DAMPLIMIT = 'DAMPLIMIT';

  TXT_OP_PARAMETER = 'Parameter';
  TXT_OP_MASSUNIT = 'Mass Units';
  TXT_OP_DIFFUSIV = 'Relative Diffusivity';
  TXT_OP_TRACENODE = 'Trace Node';
  TXT_OP_TOLQ = 'Quality Tolerance';

  TXT_OP_BULKORDER = 'Bulk Reaction Order';
  TXT_OP_WALLORDER = 'Wall Reaction Order';
  TXT_OP_GLOBALBULK = 'Global Bulk Coeff.';
  TXT_OP_GLOBALWALL = 'Global Wall Coeff.';
  TXT_OP_LIMITCONC = 'Limiting Concentration';
  TXT_OP_WALLCORREL = 'Wall Coeff. Correlation';

  TXT_OP_DURATION = 'Total Duration';
  TXT_OP_HSTEP = 'Hydraulic Time Step';
  TXT_OP_QSTEP = 'Quality Time Step';
  TXT_OP_PSTEP = 'Pattern Time Step';
  TXT_OP_PATSTART = 'Pattern Start Time';
  TXT_OP_RSTEP = 'Reporting Time Step';
  TXT_OP_REPSTART = 'Report Start Time';
  TXT_OP_CLOCKSTART = 'Clock Start Time';
  TXT_OP_STAT = 'Statistic';

  TXT_OP_PEFF = 'Pump Efficiency (%)';
  TXT_OP_ENERPRICE = 'Energy Price/kWh';
     //TXT_PR_PRICEPAT = 'Price Pattern';
  TXT_OP_DEMANDCHARGE = 'Demand Charge';

  //List - Translation at runtime through function ResourceListT
  TXT_NO = 'No';
  TXT_YES = 'Yes';

  TXT_MIXED = 'Mixed';
  TXT_2COMP = '2Comp';
  TXT_FIFO = 'FIFO';
  TXT_LIFO = 'LIFO';

  TXT_OPEN = 'Open';
  TXT_CLOSED = 'Closed';
  TXT_POPEN = 'Open';
  TXT_PCLOSED = 'Closed';
  TXT_CV = 'CV';
  TXT_NONE = 'None';
  TXT_ACTIVE = 'Active';

  TXT_FILLED = 'Filled';
  TXT_FANCY = 'Fancy';

  TXT_PRV = 'PRV';
  TXT_PSV = 'PSV';
  TXT_PBV = 'PBV';
  TXT_FCV = 'FCV';
  TXT_TCV = 'TCV';
  TXT_GPV = 'GPV';

  TXT_CFS = 'CFS';
  TXT_GPM = 'GPM';
  TXT_MGD = 'MGD';
  TXT_IMGD = 'IMGD';
  TXT_AFD = 'AFD';
  TXT_LPS = 'LPS';
  TXT_LPM = 'LPM';
  TXT_MLD = 'MLD';
  TXT_CMH = 'CMH';
  TXT_CMD = 'CMD';

  TXT_H_W = 'H-W';
  TXT_D_W = 'D-W';
  TXT_C_M = 'C-M';

  TXT_STOP ='Stop';
  TXT_CONTINUE ='Continue';

  TXT_FULL = 'Full';

  TXT_CHEMICAL = 'Chemical';
  TXT_TRACE = 'Trace';
  TXT_AGE = 'Age';

  TXT_MGL = 'mg/L';
  TXT_UGL = 'ug/L';
  TXT_UPERCENT = 'percent';
  TXT_UHOURS = 'hours';
  TXT_UFEET = 'ft';
  TXT_UMETER = 'm';
  TXT_UPSI = 'psi';
  TXT_UINCHES = 'in';
  TXT_UMILLIMETER = 'mm';
  TXT_UMILLIFEET = 'mft';
  TXT_UFTPERSEC = 'fps';
  TXT_UMTPERSEC = 'm/s';
  TXT_UFTPERKFT = 'ft/Kft';
  TXT_UMTPERKM = 'm/km';

  TXT_ZERO = 'Zero';
  TXT_FIRST = 'First';

  //Others
  TXT_NOVIEW = 'No View';
  TXT_DEMAND = 'Demand';
  TXT_LENGTH = 'Length';
  TXT_DIAMETER = 'Diameter';
  TXT_ROUGHNESS = 'Roughness';
  TXT_SETTING = 'Setting';

  // Strings from OpenDlg.pas
  TXT_PREVIEW = 'Preview';

  // Strings from PgSetup.pas
  PRINTINGMSG = 'Printer is currently printing.';
  NOPRINTERMSG = 'There are no installed printers.';


  // Strings from PSForm.pas
  MSG_BADMARGINS = 'Page margins are too large for paper size.';
  TXT_WIDTH = 'Width: %.1f ';
  TXT_HEIGHT = 'Height: %.1f ';
  TXT_MARGINS_MM = 'Margins (mm)';
  TXT_MARGINS_IN = 'Margins (inches)';

  TXT_PORTRAIT = 'Portrait';
  TXT_LANDSCAPE = 'Landscape';

  TXT_PG_NONE = 'None';
  TXT_PG_UL = 'Upper Left';
  TXT_PG_UC = 'Upper Center';
  TXT_PG_UR = 'Upper Right';
  TXT_PG_LL = 'Lower Left';
  TXT_PG_LC = 'Lower Center';
  TXT_PG_LR = 'Lower Right';


  // Strings from Xprinter.pas
  TXT_PR_PAGE = 'Printing page %d of %d';
  TXT_PR_ABORT = 'Printing aborted';
  TXT_PG_NO = 'Page %d of %d';


function ResourceListT(listID : string): string;
function ConvertUnitText(aUnit: String): String;

implementation

function ResourceListT(listID : string): string;
var
  List : string;

begin
  case listID of
    'No'#13'Yes' : List:= TXT_NO+','+TXT_YES;

    'Mixed'#13'2Comp'#13'FIFO'#13'LIFO' : List := TXT_MIXED+','+TXT_2COMP+','+
                                                  TXT_FIFO+','+TXT_LIFO;

    'Open'#13'Closed'#13'CV' : List:= TXT_OPEN+','+TXT_CLOSED+','+TXT_CV;

    'Open'#13'Closed' : List:= TXT_POPEN+','+TXT_PCLOSED;

    'PRV'#13'PSV'#13'PBV'#13'FCV'#13'TCV'#13'GPV' : List:= TXT_PRV+','+
                                                           TXT_PSV+','+
                                                           TXT_PBV+','+
                                                           TXT_FCV+','+
                                                           TXT_TCV+','+
                                                           TXT_GPV;

    'None'#13'Open'#13'Closed' : List:= TXT_NONE+','+TXT_OPEN+','+TXT_CLOSED;

    'None'#13'Node'#13'Link' : List := TXT_NONE+','+TXT_NODE+','+TXT_LINK;

    'CFS'#13'GPM'#13'MGD'#13'IMGD'#13'AFD'#13 +
           'LPS'#13'LPM'#13'MLD'#13'CMH'#13'CMD' : List := TXT_CFS+','+
                                                           TXT_GPM+','+
                                                           TXT_MGD+','+
                                                           TXT_IMGD+','+
                                                           TXT_AFD+','+
                                                           TXT_LPS+','+
                                                           TXT_LPM+','+
                                                           TXT_MLD+','+
                                                           TXT_CMH+','+
                                                           TXT_CMD;

    'H-W'#13'D-W'#13'C-M' : List :=   TXT_H_W+','+TXT_D_W+','+TXT_C_M;

    'Stop'#13'Continue' : List := TXT_STOP+','+TXT_CONTINUE;

    'No'#13'Yes'#13'Full' : List := TXT_NO+','+TXT_YES+','+TXT_FULL;

    'None'#13'Chemical'#13'Trace'#13'Age' : List := TXT_NONE+','+
                                                    TXT_CHEMICAL+','+
                                                    TXT_TRACE+','+
                                                    TXT_AGE;

    'mg/L'#13'ug/L'#13 : List := TXT_MGL+','+TXT_UGL;

    'Zero'#13'First' : List := TXT_ZERO+','+TXT_FIRST;

    'None'#13'Average'#13'Minimum'#13'Maximum'#13'Range' : List := TXT_NONE+','+
                                                                   TXT_AVERAGE+','+
                                                                   TXT_MINIMUM+','+
                                                                   TXT_MAXIMUM+','+
                                                                   TXT_RANGE;

    'Off'#13'On' : List:= TXT_AUTOLENGTH_OFF+','+TXT_AUTOLENGTH_ON;
    'DDA'#13'PDA' : List := 'DDA,PDA';
  end;
  Result := List;
end;

function ConvertUnitText(aUnit: String): String;
var
  UnitList : TStringList;
  List  : array [0..28] of AnsiString =  ('CFS','GPM','MGD','IMGD','AFD','LPS','LPM',
                        'MLD','CMH','CMD','mg/L','ug/L','percent','hours','ft','m',
                        'psi','in','mm','mft','fps','m/s','ft/Kft','m/km','Feet',
                        'Meters','Degrees','None','');
  ListT : array [0..28] of AnsiString = (TXT_CFS, TXT_GPM, TXT_MGD, TXT_IMGD, TXT_AFD,
                        TXT_LPS, TXT_LPM, TXT_MLD, TXT_CMH, TXT_CMD, TXT_MGL, TXT_UGL,
                        TXT_UPERCENT, TXT_UHOURS, TXT_UFEET, TXT_UMETER, TXT_UPSI,
                        TXT_UINCHES, TXT_UMILLIMETER, TXT_UMILLIFEET, TXT_UFTPERSEC,
                        TXT_UMTPERSEC, TXT_UFTPERKFT, TXT_UMTPERKM, TXT_DFEET, TXT_DMETER,
                        TXT_DDEGREE, TXT_DNONE, '');
begin

  UnitList := TStringList.Create;
  UnitList.AddStrings(List);

  try
    Result := ListT[UnitList.indexOf(aUnit)];
  except
    Result := aUnit;
  end;

  UnitList.Free;

end;


end.
