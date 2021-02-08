unit KMLFile;

interface

uses
  Windows, SysUtils, Types, Classes, Variants, Generics.Collections, Dialogs,
  Controls, ExtCtrls, XMLLeafTypes;


type

  TKMLFile = class;
  TKMLDocument = class;
  TKMLFolder = class;
  TKMLPlacemark = class;
  TKMLGeometry = class;
  TKMLMultiGeometry = class;
  TKMLPoint = class;
  TKMLPolygon = class;
  TKMLBoundary = class;
  TKMLLinearRing = class;
  TKMLLineString = class;
  TKMLLookAt = class;
  TKMLLatLonBox = class;
  TKMLGroundOverlay = class;
  TKMLExtendedData = class;
  TKMLData = class;
  TKMLSize = class;
  TKMLStyle = class;
  TKMLIconStyle = class;
  TKMLIcon = class;
  TKMLLineStyle = class;
  TKMLBalloonStyle = class;
  TKMLStyleMap = class;
  TKMLScreenOverlay = class;
  TKMLPair = class;

  TKMLFile = class(TObject)
  private
    FXmlns: String;
    FXmlnsExsit: Boolean;
    FDocument: TKMLDocument;
    FFolders: TList<TKMLFolder>;
    procedure SetXmlns(const _Value: String);
    procedure SetXmlnsExsit(const Value: Boolean);
    procedure SetDocument(const _Value: TKMLDocument);
    procedure SetFolders(const _Value: TList<TKMLFolder>);
    function GetFolder(Index: Integer): TKMLFolder;
    procedure SetFolder(Index: Integer; const _Value: TKMLFolder);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddXmlns: String;
    procedure XmlnsRemove;
    procedure AddFolder(_Value: TKMLFolder);
    function AddNewFolder: TKMLFolder;
    procedure FolderClear;
    function FolderCount: Integer;
    procedure RemoveFolder(_Value: TKMLFolder);
    procedure DeleteFolder(Index: Integer);
    property Xmlns: String read FXmlns write SetXmlns;
    property XmlnsExsit: Boolean read FXmlnsExsit Write SetXmlnsExsit;
    property Document: TKMLDocument read FDocument write SetDocument;
    property Folders: TList<TKMLFolder> read FFolders write SetFolders;
    property Folder[Index: Integer]: TKMLFolder read GetFolder write SetFolder;
  end;

  TKMLDocument = class(TObject)
  private
    FName: String;
    FNameExsit: Boolean;
    FOpen: Integer;
    FOpenExsit: Boolean;
    FVisibility: Integer;
    FVisibilityExsit: Boolean;
    FDescription: String;
    FDescriptionExsit: Boolean;
    FLookAt: TKMLLookAt;
    FLookAtExsit: Boolean;
    FStyleMaps: TList<TKMLStyleMap>;
    FStyles: TList<TKMLStyle>;
    FPlacemarks: TList<TKMLPlacemark>;
    FFolders: TList<TKMLFolder>;
    procedure SetName(const _Value: String);
    procedure SetNameExsit(const Value: Boolean);
    procedure SetOpen(const _Value: Integer);
    procedure SetOpenExsit(const Value: Boolean);
    procedure SetVisibility(const _Value: Integer);
    procedure SetVisibilityExsit(const Value: Boolean);
    procedure SetDescription(const _Value: String);
    procedure SetDescriptionExsit(const Value: Boolean);
    procedure SetLookAt(const _Value: TKMLLookAt);
    procedure SetLookAtExsit(const Value: Boolean);
    procedure SetStyleMaps(const _Value: TList<TKMLStyleMap>);
    function GetStyleMap(Index: Integer): TKMLStyleMap;
    procedure SetStyleMap(Index: Integer; const _Value: TKMLStyleMap);
    procedure SetStyles(const _Value: TList<TKMLStyle>);
    function GetStyle(Index: Integer): TKMLStyle;
    procedure SetStyle(Index: Integer; const _Value: TKMLStyle);
    procedure SetPlacemarks(const _Value: TList<TKMLPlacemark>);
    function GetPlacemark(Index: Integer): TKMLPlacemark;
    procedure SetPlacemark(Index: Integer; const _Value: TKMLPlacemark);
    procedure SetFolders(const _Value: TList<TKMLFolder>);
    function GetFolder(Index: Integer): TKMLFolder;
    procedure SetFolder(Index: Integer; const _Value: TKMLFolder);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddName: String;
    procedure NameRemove;
    function AddOpen: Integer;
    procedure OpenRemove;
    function AddVisibility: Integer;
    procedure VisibilityRemove;
    function AddDescription: String;
    procedure DescriptionRemove;
    function AddLookAt: TKMLLookAt;
    procedure LookAtRemove;
    procedure AddStyleMap(_Value: TKMLStyleMap);
    function AddNewStyleMap: TKMLStyleMap;
    procedure StyleMapClear;
    function StyleMapCount: Integer;
    procedure RemoveStyleMap(_Value: TKMLStyleMap);
    procedure DeleteStyleMap(Index: Integer);
    procedure AddStyle(_Value: TKMLStyle);
    function AddNewStyle: TKMLStyle;
    procedure StyleClear;
    function StyleCount: Integer;
    procedure RemoveStyle(_Value: TKMLStyle);
    procedure DeleteStyle(Index: Integer);
    procedure AddPlacemark(_Value: TKMLPlacemark);
    function AddNewPlacemark: TKMLPlacemark;
    procedure PlacemarkClear;
    function PlacemarkCount: Integer;
    procedure RemovePlacemark(_Value: TKMLPlacemark);
    procedure DeletePlacemark(Index: Integer);
    procedure AddFolder(_Value: TKMLFolder);
    function AddNewFolder: TKMLFolder;
    procedure FolderClear;
    function FolderCount: Integer;
    procedure RemoveFolder(_Value: TKMLFolder);
    procedure DeleteFolder(Index: Integer);
    property Name: String read FName write SetName;
    property NameExsit: Boolean read FNameExsit Write SetNameExsit;
    property Open: Integer read FOpen write SetOpen;
    property OpenExsit: Boolean read FOpenExsit Write SetOpenExsit;
    property Visibility: Integer read FVisibility write SetVisibility;
    property VisibilityExsit: Boolean read FVisibilityExsit Write SetVisibilityExsit;
    property Description: String read FDescription write SetDescription;
    property DescriptionExsit: Boolean read FDescriptionExsit Write SetDescriptionExsit;
    property LookAt: TKMLLookAt read FLookAt write SetLookAt;
    property LookAtExsit: Boolean read FLookAtExsit write SetLookAtExsit;
    property StyleMaps: TList<TKMLStyleMap> read FStyleMaps write SetStyleMaps;
    property StyleMap[Index: Integer]: TKMLStyleMap read GetStyleMap write SetStyleMap;
    property Styles: TList<TKMLStyle> read FStyles write SetStyles;
    property Style[Index: Integer]: TKMLStyle read GetStyle write SetStyle;
    property Placemarks: TList<TKMLPlacemark> read FPlacemarks write SetPlacemarks;
    property Placemark[Index: Integer]: TKMLPlacemark read GetPlacemark write SetPlacemark;
    property Folders: TList<TKMLFolder> read FFolders write SetFolders;
    property Folder[Index: Integer]: TKMLFolder read GetFolder write SetFolder;
  end;

  TKMLFolder = class(TObject)
  private
    FName: String;
    FNameExsit: Boolean;
    FOpen: Integer;
    FOpenExsit: Boolean;
    FVisibility: Integer;
    FVisibilityExsit: Boolean;
    FDescription: String;
    FDescriptionExsit: Boolean;
    FLookAt: TKMLLookAt;
    FLookAtExsit: Boolean;
    FDocument: TKMLDocument;
    FDocumentExsit: Boolean;
    FGroundOverlays: TList<TKMLGroundOverlay>;
    FPlacemarks: TList<TKMLPlacemark>;
    FScreenOverlays: TList<TKMLScreenOverlay>;
    FFolders: TList<TKMLFolder>;
    procedure SetName(const _Value: String);
    procedure SetNameExsit(const Value: Boolean);
    procedure SetOpen(const _Value: Integer);
    procedure SetOpenExsit(const Value: Boolean);
    procedure SetVisibility(const _Value: Integer);
    procedure SetVisibilityExsit(const Value: Boolean);
    procedure SetDescription(const _Value: String);
    procedure SetDescriptionExsit(const Value: Boolean);
    procedure SetLookAt(const _Value: TKMLLookAt);
    procedure SetLookAtExsit(const Value: Boolean);
    procedure SetDocument(const _Value: TKMLDocument);
    procedure SetDocumentExsit(const Value: Boolean);
    procedure SetGroundOverlays(const _Value: TList<TKMLGroundOverlay>);
    function GetGroundOverlay(Index: Integer): TKMLGroundOverlay;
    procedure SetGroundOverlay(Index: Integer; const _Value: TKMLGroundOverlay);
    procedure SetPlacemarks(const _Value: TList<TKMLPlacemark>);
    function GetPlacemark(Index: Integer): TKMLPlacemark;
    procedure SetPlacemark(Index: Integer; const _Value: TKMLPlacemark);
    procedure SetScreenOverlays(const _Value: TList<TKMLScreenOverlay>);
    function GetScreenOverlay(Index: Integer): TKMLScreenOverlay;
    procedure SetScreenOverlay(Index: Integer; const _Value: TKMLScreenOverlay);
    procedure SetFolders(const _Value: TList<TKMLFolder>);
    function GetFolder(Index: Integer): TKMLFolder;
    procedure SetFolder(Index: Integer; const _Value: TKMLFolder);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddName: String;
    procedure NameRemove;
    function AddOpen: Integer;
    procedure OpenRemove;
    function AddVisibility: Integer;
    procedure VisibilityRemove;
    function AddDescription: String;
    procedure DescriptionRemove;
    function AddLookAt: TKMLLookAt;
    procedure LookAtRemove;
    function AddDocument: TKMLDocument;
    procedure DocumentRemove;
    procedure AddGroundOverlay(_Value: TKMLGroundOverlay);
    function AddNewGroundOverlay: TKMLGroundOverlay;
    procedure GroundOverlayClear;
    function GroundOverlayCount: Integer;
    procedure RemoveGroundOverlay(_Value: TKMLGroundOverlay);
    procedure DeleteGroundOverlay(Index: Integer);
    procedure AddPlacemark(_Value: TKMLPlacemark);
    function AddNewPlacemark: TKMLPlacemark;
    procedure PlacemarkClear;
    function PlacemarkCount: Integer;
    procedure RemovePlacemark(_Value: TKMLPlacemark);
    procedure DeletePlacemark(Index: Integer);
    procedure AddScreenOverlay(_Value: TKMLScreenOverlay);
    function AddNewScreenOverlay: TKMLScreenOverlay;
    procedure ScreenOverlayClear;
    function ScreenOverlayCount: Integer;
    procedure RemoveScreenOverlay(_Value: TKMLScreenOverlay);
    procedure DeleteScreenOverlay(Index: Integer);
    procedure AddFolder(_Value: TKMLFolder);
    function AddNewFolder: TKMLFolder;
    procedure FolderClear;
    function FolderCount: Integer;
    procedure RemoveFolder(_Value: TKMLFolder);
    procedure DeleteFolder(Index: Integer);
    property Name: String read FName write SetName;
    property NameExsit: Boolean read FNameExsit Write SetNameExsit;
    property Open: Integer read FOpen write SetOpen;
    property OpenExsit: Boolean read FOpenExsit Write SetOpenExsit;
    property Visibility: Integer read FVisibility write SetVisibility;
    property VisibilityExsit: Boolean read FVisibilityExsit Write SetVisibilityExsit;
    property Description: String read FDescription write SetDescription;
    property DescriptionExsit: Boolean read FDescriptionExsit Write SetDescriptionExsit;
    property LookAt: TKMLLookAt read FLookAt write SetLookAt;
    property LookAtExsit: Boolean read FLookAtExsit write SetLookAtExsit;
    property Document: TKMLDocument read FDocument write SetDocument;
    property DocumentExsit: Boolean read FDocumentExsit write SetDocumentExsit;
    property GroundOverlays: TList<TKMLGroundOverlay> read FGroundOverlays write SetGroundOverlays;
    property GroundOverlay[Index: Integer]: TKMLGroundOverlay read GetGroundOverlay write SetGroundOverlay;
    property Placemarks: TList<TKMLPlacemark> read FPlacemarks write SetPlacemarks;
    property Placemark[Index: Integer]: TKMLPlacemark read GetPlacemark write SetPlacemark;
    property ScreenOverlays: TList<TKMLScreenOverlay> read FScreenOverlays write SetScreenOverlays;
    property ScreenOverlay[Index: Integer]: TKMLScreenOverlay read GetScreenOverlay write SetScreenOverlay;
    property Folders: TList<TKMLFolder> read FFolders write SetFolders;
    property Folder[Index: Integer]: TKMLFolder read GetFolder write SetFolder;
  end;

  TKMLPlacemark = class(TObject)
  private
    FName: String;
    FNameExsit: Boolean;
    FVisibility: Integer;
    FVisibilityExsit: Boolean;
    FDescription: String;
    FDescriptionExsit: Boolean;
    FLookAt: TKMLLookAt;
    FLookAtExsit: Boolean;
    FStyleUrl: String;
    FStyleUrlExsit: Boolean;
    FGeometry: TKMLGeometry;
    FGeometryExsit: Boolean;
    FExtendedData: TKMLExtendedData;
    FExtendedDataExsit: Boolean;
    procedure SetName(const _Value: String);
    procedure SetNameExsit(const Value: Boolean);
    procedure SetVisibility(const _Value: Integer);
    procedure SetVisibilityExsit(const Value: Boolean);
    procedure SetDescription(const _Value: String);
    procedure SetDescriptionExsit(const Value: Boolean);
    procedure SetLookAt(const _Value: TKMLLookAt);
    procedure SetLookAtExsit(const Value: Boolean);
    procedure SetStyleUrl(const _Value: String);
    procedure SetStyleUrlExsit(const Value: Boolean);
    procedure SetGeometry(const _Value: TKMLGeometry);
    procedure SetGeometryExsit(const Value: Boolean);
    procedure SetExtendedData(const _Value: TKMLExtendedData);
    procedure SetExtendedDataExsit(const Value: Boolean);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddName: String;
    procedure NameRemove;
    function AddVisibility: Integer;
    procedure VisibilityRemove;
    function AddDescription: String;
    procedure DescriptionRemove;
    function AddLookAt: TKMLLookAt;
    procedure LookAtRemove;
    function AddStyleUrl: String;
    procedure StyleUrlRemove;
    function AddPoint: TKMLPoint;
    function AddPolygon: TKMLPolygon;
    function AddLinearRing: TKMLLinearRing;
    function AddLineString: TKMLLineString;
    function AddMultiGeometry: TKMLMultiGeometry;
    procedure GeometryRemove;
    function AddExtendedData: TKMLExtendedData;
    procedure ExtendedDataRemove;
    property Name: String read FName write SetName;
    property NameExsit: Boolean read FNameExsit Write SetNameExsit;
    property Visibility: Integer read FVisibility write SetVisibility;
    property VisibilityExsit: Boolean read FVisibilityExsit Write SetVisibilityExsit;
    property Description: String read FDescription write SetDescription;
    property DescriptionExsit: Boolean read FDescriptionExsit Write SetDescriptionExsit;
    property LookAt: TKMLLookAt read FLookAt write SetLookAt;
    property LookAtExsit: Boolean read FLookAtExsit write SetLookAtExsit;
    property StyleUrl: String read FStyleUrl write SetStyleUrl;
    property StyleUrlExsit: Boolean read FStyleUrlExsit Write SetStyleUrlExsit;
    property Geometry: TKMLGeometry read FGeometry write SetGeometry;
    property GeometryExsit: Boolean read FGeometryExsit write SetGeometryExsit;
    property ExtendedData: TKMLExtendedData read FExtendedData write SetExtendedData;
    property ExtendedDataExsit: Boolean read FExtendedDataExsit write SetExtendedDataExsit;
  end;

  TKMLGeometry = class abstract(TObject)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TKMLMultiGeometry = class(TKMLGeometry)
  private
    FGeometrys: TList<TKMLGeometry>;
    procedure SetGeometrys(const _Value: TList<TKMLGeometry>);
    function GetGeometry(Index: Integer): TKMLGeometry;
    procedure SetGeometry(Index: Integer; const _Value: TKMLGeometry);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddGeometry(_Value: TKMLGeometry);
    function AddNewPoint: TKMLPoint;
    function AddNewPolygon: TKMLPolygon;
    function AddNewLinearRing: TKMLLinearRing;
    function AddNewLineString: TKMLLineString;
    function AddNewMultiGeometry: TKMLMultiGeometry;
    procedure GeometryClear;
    function GeometryCount: Integer;
    procedure RemoveGeometry(_Value: TKMLGeometry);
    procedure DeleteGeometry(Index: Integer);
    property Geometrys: TList<TKMLGeometry> read FGeometrys write SetGeometrys;
    property Geometry[Index: Integer]: TKMLGeometry read GetGeometry write SetGeometry;
  end;

  TKMLPoint = class(TKMLGeometry)
  private
    FExtrude: String;
    FExtrudeExsit: Boolean;
    FAltitudeMode: String;
    FAltitudeModeExsit: Boolean;
    FCoordinates: ArrayCoordinates;
    procedure SetExtrude(const _Value: String);
    procedure SetExtrudeExsit(const Value: Boolean);
    procedure SetAltitudeMode(const _Value: String);
    procedure SetAltitudeModeExsit(const Value: Boolean);
    procedure SetCoordinates(const _Value: ArrayCoordinates);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddExtrude: String;
    procedure ExtrudeRemove;
    function AddAltitudeMode: String;
    procedure AltitudeModeRemove;
    property Extrude: String read FExtrude write SetExtrude;
    property ExtrudeExsit: Boolean read FExtrudeExsit Write SetExtrudeExsit;
    property AltitudeMode: String read FAltitudeMode write SetAltitudeMode;
    property AltitudeModeExsit: Boolean read FAltitudeModeExsit Write SetAltitudeModeExsit;
    property Coordinates: ArrayCoordinates read FCoordinates write SetCoordinates;
  end;

  TKMLPolygon = class(TKMLGeometry)
  private
    FExtrude: String;
    FExtrudeExsit: Boolean;
    FAltitudeMode: String;
    FAltitudeModeExsit: Boolean;
    FOuterBoundaryIss: TList<TKMLBoundary>;
    FInnerBoundaryIss: TList<TKMLBoundary>;
    FTessellate: String;
    FTessellateExsit: Boolean;
    procedure SetExtrude(const _Value: String);
    procedure SetExtrudeExsit(const Value: Boolean);
    procedure SetAltitudeMode(const _Value: String);
    procedure SetAltitudeModeExsit(const Value: Boolean);
    procedure SetOuterBoundaryIss(const _Value: TList<TKMLBoundary>);
    function GetOuterBoundaryIs(Index: Integer): TKMLBoundary;
    procedure SetOuterBoundaryIs(Index: Integer; const _Value: TKMLBoundary);
    procedure SetInnerBoundaryIss(const _Value: TList<TKMLBoundary>);
    function GetInnerBoundaryIs(Index: Integer): TKMLBoundary;
    procedure SetInnerBoundaryIs(Index: Integer; const _Value: TKMLBoundary);
    procedure SetTessellate(const _Value: String);
    procedure SetTessellateExsit(const Value: Boolean);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddExtrude: String;
    procedure ExtrudeRemove;
    function AddAltitudeMode: String;
    procedure AltitudeModeRemove;
    procedure AddOuterBoundaryIs(_Value: TKMLBoundary);
    function AddNewOuterBoundaryIs: TKMLBoundary;
    procedure OuterBoundaryIsClear;
    function OuterBoundaryIsCount: Integer;
    procedure RemoveOuterBoundaryIs(_Value: TKMLBoundary);
    procedure DeleteOuterBoundaryIs(Index: Integer);
    procedure AddInnerBoundaryIs(_Value: TKMLBoundary);
    function AddNewInnerBoundaryIs: TKMLBoundary;
    procedure InnerBoundaryIsClear;
    function InnerBoundaryIsCount: Integer;
    procedure RemoveInnerBoundaryIs(_Value: TKMLBoundary);
    procedure DeleteInnerBoundaryIs(Index: Integer);
    function AddTessellate: String;
    procedure TessellateRemove;
    property Extrude: String read FExtrude write SetExtrude;
    property ExtrudeExsit: Boolean read FExtrudeExsit Write SetExtrudeExsit;
    property AltitudeMode: String read FAltitudeMode write SetAltitudeMode;
    property AltitudeModeExsit: Boolean read FAltitudeModeExsit Write SetAltitudeModeExsit;
    property OuterBoundaryIss: TList<TKMLBoundary> read FOuterBoundaryIss write SetOuterBoundaryIss;
    property OuterBoundaryIs[Index: Integer]: TKMLBoundary read GetOuterBoundaryIs write SetOuterBoundaryIs;
    property InnerBoundaryIss: TList<TKMLBoundary> read FInnerBoundaryIss write SetInnerBoundaryIss;
    property InnerBoundaryIs[Index: Integer]: TKMLBoundary read GetInnerBoundaryIs write SetInnerBoundaryIs;
    property Tessellate: String read FTessellate write SetTessellate;
    property TessellateExsit: Boolean read FTessellateExsit Write SetTessellateExsit;
  end;

  TKMLBoundary = class(TObject)
  private
    FLinearRing: TKMLLinearRing;
    procedure SetLinearRing(const _Value: TKMLLinearRing);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property LinearRing: TKMLLinearRing read FLinearRing write SetLinearRing;
  end;

  TKMLLinearRing = class(TKMLGeometry)
  private
    FCoordinates: ArrayCoordinates;
    procedure SetCoordinates(const _Value: ArrayCoordinates);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property Coordinates: ArrayCoordinates read FCoordinates write SetCoordinates;
  end;

  TKMLLineString = class(TKMLGeometry)
  private
    FTessellate: String;
    FTessellateExsit: Boolean;
    FAltitudeMode: String;
    FAltitudeModeExsit: Boolean;
    FExtrude: String;
    FExtrudeExsit: Boolean;
    FCoordinates: ArrayCoordinates;
    procedure SetTessellate(const _Value: String);
    procedure SetTessellateExsit(const Value: Boolean);
    procedure SetAltitudeMode(const _Value: String);
    procedure SetAltitudeModeExsit(const Value: Boolean);
    procedure SetExtrude(const _Value: String);
    procedure SetExtrudeExsit(const Value: Boolean);
    procedure SetCoordinates(const _Value: ArrayCoordinates);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddTessellate: String;
    procedure TessellateRemove;
    function AddAltitudeMode: String;
    procedure AltitudeModeRemove;
    function AddExtrude: String;
    procedure ExtrudeRemove;
    property Tessellate: String read FTessellate write SetTessellate;
    property TessellateExsit: Boolean read FTessellateExsit Write SetTessellateExsit;
    property AltitudeMode: String read FAltitudeMode write SetAltitudeMode;
    property AltitudeModeExsit: Boolean read FAltitudeModeExsit Write SetAltitudeModeExsit;
    property Extrude: String read FExtrude write SetExtrude;
    property ExtrudeExsit: Boolean read FExtrudeExsit Write SetExtrudeExsit;
    property Coordinates: ArrayCoordinates read FCoordinates write SetCoordinates;
  end;

  TKMLLookAt = class(TObject)
  private
    FLongitude: Double;
    FLongitudeExsit: Boolean;
    FLatitude: Double;
    FLatitudeExsit: Boolean;
    FAltitude: Double;
    FAltitudeExsit: Boolean;
    FHeading: Double;
    FHeadingExsit: Boolean;
    FTilt: Double;
    FTiltExsit: Boolean;
    FRange: Double;
    FRangeExsit: Boolean;
    procedure SetLongitude(const _Value: Double);
    procedure SetLongitudeExsit(const Value: Boolean);
    procedure SetLatitude(const _Value: Double);
    procedure SetLatitudeExsit(const Value: Boolean);
    procedure SetAltitude(const _Value: Double);
    procedure SetAltitudeExsit(const Value: Boolean);
    procedure SetHeading(const _Value: Double);
    procedure SetHeadingExsit(const Value: Boolean);
    procedure SetTilt(const _Value: Double);
    procedure SetTiltExsit(const Value: Boolean);
    procedure SetRange(const _Value: Double);
    procedure SetRangeExsit(const Value: Boolean);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddLongitude: Double;
    procedure LongitudeRemove;
    function AddLatitude: Double;
    procedure LatitudeRemove;
    function AddAltitude: Double;
    procedure AltitudeRemove;
    function AddHeading: Double;
    procedure HeadingRemove;
    function AddTilt: Double;
    procedure TiltRemove;
    function AddRange: Double;
    procedure RangeRemove;
    property Longitude: Double read FLongitude write SetLongitude;
    property LongitudeExsit: Boolean read FLongitudeExsit Write SetLongitudeExsit;
    property Latitude: Double read FLatitude write SetLatitude;
    property LatitudeExsit: Boolean read FLatitudeExsit Write SetLatitudeExsit;
    property Altitude: Double read FAltitude write SetAltitude;
    property AltitudeExsit: Boolean read FAltitudeExsit Write SetAltitudeExsit;
    property Heading: Double read FHeading write SetHeading;
    property HeadingExsit: Boolean read FHeadingExsit Write SetHeadingExsit;
    property Tilt: Double read FTilt write SetTilt;
    property TiltExsit: Boolean read FTiltExsit Write SetTiltExsit;
    property Range: Double read FRange write SetRange;
    property RangeExsit: Boolean read FRangeExsit Write SetRangeExsit;
  end;

  TKMLLatLonBox = class(TObject)
  private
    FNorth: Double;
    FSouth: Double;
    FEast: Double;
    FWest: Double;
    FRotation: Double;
    procedure SetNorth(const _Value: Double);
    procedure SetSouth(const _Value: Double);
    procedure SetEast(const _Value: Double);
    procedure SetWest(const _Value: Double);
    procedure SetRotation(const _Value: Double);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property North: Double read FNorth write SetNorth;
    property South: Double read FSouth write SetSouth;
    property East: Double read FEast write SetEast;
    property West: Double read FWest write SetWest;
    property Rotation: Double read FRotation write SetRotation;
  end;

  TKMLGroundOverlay = class(TObject)
  private
    FName: String;
    FNameExsit: Boolean;
    FVisibility: Integer;
    FVisibilityExsit: Boolean;
    FDescription: String;
    FDescriptionExsit: Boolean;
    FLookAt: TKMLLookAt;
    FLookAtExsit: Boolean;
    FIcon: TKMLIcon;
    FIconExsit: Boolean;
    FLatLonBox: TKMLLatLonBox;
    FLatLonBoxExsit: Boolean;
    procedure SetName(const _Value: String);
    procedure SetNameExsit(const Value: Boolean);
    procedure SetVisibility(const _Value: Integer);
    procedure SetVisibilityExsit(const Value: Boolean);
    procedure SetDescription(const _Value: String);
    procedure SetDescriptionExsit(const Value: Boolean);
    procedure SetLookAt(const _Value: TKMLLookAt);
    procedure SetLookAtExsit(const Value: Boolean);
    procedure SetIcon(const _Value: TKMLIcon);
    procedure SetIconExsit(const Value: Boolean);
    procedure SetLatLonBox(const _Value: TKMLLatLonBox);
    procedure SetLatLonBoxExsit(const Value: Boolean);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddName: String;
    procedure NameRemove;
    function AddVisibility: Integer;
    procedure VisibilityRemove;
    function AddDescription: String;
    procedure DescriptionRemove;
    function AddLookAt: TKMLLookAt;
    procedure LookAtRemove;
    function AddIcon: TKMLIcon;
    procedure IconRemove;
    function AddLatLonBox: TKMLLatLonBox;
    procedure LatLonBoxRemove;
    property Name: String read FName write SetName;
    property NameExsit: Boolean read FNameExsit Write SetNameExsit;
    property Visibility: Integer read FVisibility write SetVisibility;
    property VisibilityExsit: Boolean read FVisibilityExsit Write SetVisibilityExsit;
    property Description: String read FDescription write SetDescription;
    property DescriptionExsit: Boolean read FDescriptionExsit Write SetDescriptionExsit;
    property LookAt: TKMLLookAt read FLookAt write SetLookAt;
    property LookAtExsit: Boolean read FLookAtExsit write SetLookAtExsit;
    property Icon: TKMLIcon read FIcon write SetIcon;
    property IconExsit: Boolean read FIconExsit write SetIconExsit;
    property LatLonBox: TKMLLatLonBox read FLatLonBox write SetLatLonBox;
    property LatLonBoxExsit: Boolean read FLatLonBoxExsit write SetLatLonBoxExsit;
  end;

  TKMLExtendedData = class(TObject)
  private
    FDatas: TList<TKMLData>;
    procedure SetDatas(const _Value: TList<TKMLData>);
    function GetData(Index: Integer): TKMLData;
    procedure SetData(Index: Integer; const _Value: TKMLData);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddData(_Value: TKMLData);
    function AddNewData: TKMLData;
    procedure DataClear;
    function DataCount: Integer;
    procedure RemoveData(_Value: TKMLData);
    procedure DeleteData(Index: Integer);
    property Datas: TList<TKMLData> read FDatas write SetDatas;
    property Data[Index: Integer]: TKMLData read GetData write SetData;
  end;

  TKMLData = class(TObject)
  private
    FName: String;
    FValue: String;
    procedure SetName(const _Value: String);
    procedure SetValue(const _Value: String);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property Name: String read FName write SetName;
    property Value: String read FValue write SetValue;
  end;

  TKMLSize = class(TObject)
  private
    FX: Double;
    FY: Double;
    FXUnits: String;
    FYUnits: String;
    procedure SetX(const _Value: Double);
    procedure SetY(const _Value: Double);
    procedure SetXUnits(const _Value: String);
    procedure SetYUnits(const _Value: String);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property X: Double read FX write SetX;
    property Y: Double read FY write SetY;
    property XUnits: String read FXUnits write SetXUnits;
    property YUnits: String read FYUnits write SetYUnits;
  end;

  TKMLStyle = class(TObject)
  private
    FID: String;
    FIconStyle: TKMLIconStyle;
    FIconStyleExsit: Boolean;
    FLineStyle: TKMLLineStyle;
    FLineStyleExsit: Boolean;
    FPolyStyle: TKMLLineStyle;
    FPolyStyleExsit: Boolean;
    FBalloonStyle: TKMLBalloonStyle;
    FBalloonStyleExsit: Boolean;
    procedure SetID(const _Value: String);
    procedure SetIconStyle(const _Value: TKMLIconStyle);
    procedure SetIconStyleExsit(const Value: Boolean);
    procedure SetLineStyle(const _Value: TKMLLineStyle);
    procedure SetLineStyleExsit(const Value: Boolean);
    procedure SetPolyStyle(const _Value: TKMLLineStyle);
    procedure SetPolyStyleExsit(const Value: Boolean);
    procedure SetBalloonStyle(const _Value: TKMLBalloonStyle);
    procedure SetBalloonStyleExsit(const Value: Boolean);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddIconStyle: TKMLIconStyle;
    procedure IconStyleRemove;
    function AddLineStyle: TKMLLineStyle;
    procedure LineStyleRemove;
    function AddPolyStyle: TKMLLineStyle;
    procedure PolyStyleRemove;
    function AddBalloonStyle: TKMLBalloonStyle;
    procedure BalloonStyleRemove;
    property ID: String read FID write SetID;
    property IconStyle: TKMLIconStyle read FIconStyle write SetIconStyle;
    property IconStyleExsit: Boolean read FIconStyleExsit write SetIconStyleExsit;
    property LineStyle: TKMLLineStyle read FLineStyle write SetLineStyle;
    property LineStyleExsit: Boolean read FLineStyleExsit write SetLineStyleExsit;
    property PolyStyle: TKMLLineStyle read FPolyStyle write SetPolyStyle;
    property PolyStyleExsit: Boolean read FPolyStyleExsit write SetPolyStyleExsit;
    property BalloonStyle: TKMLBalloonStyle read FBalloonStyle write SetBalloonStyle;
    property BalloonStyleExsit: Boolean read FBalloonStyleExsit write SetBalloonStyleExsit;
  end;

  TKMLIconStyle = class(TObject)
  private
    FIcon: TKMLIcon;
    procedure SetIcon(const _Value: TKMLIcon);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property Icon: TKMLIcon read FIcon write SetIcon;
  end;

  TKMLIcon = class(TObject)
  private
    Fhref: String;
    procedure Sethref(const _Value: String);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property href: String read Fhref write Sethref;
  end;

  TKMLLineStyle = class(TObject)
  private
    Fcolor: String;
    FcolorExsit: Boolean;
    Fwidth: Double;
    FwidthExsit: Boolean;
    procedure Setcolor(const _Value: String);
    procedure SetcolorExsit(const Value: Boolean);
    procedure Setwidth(const _Value: Double);
    procedure SetwidthExsit(const Value: Boolean);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function Addcolor: String;
    procedure colorRemove;
    function Addwidth: Double;
    procedure widthRemove;
    property color: String read Fcolor write Setcolor;
    property colorExsit: Boolean read FcolorExsit Write SetcolorExsit;
    property width: Double read Fwidth write Setwidth;
    property widthExsit: Boolean read FwidthExsit Write SetwidthExsit;
  end;

  TKMLBalloonStyle = class(TObject)
  private
    Ftext: String;
    procedure Settext(const _Value: String);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property text: String read Ftext write Settext;
  end;

  TKMLStyleMap = class(TObject)
  private
    FPairs: TList<TKMLPair>;
    FID: String;
    procedure SetPairs(const _Value: TList<TKMLPair>);
    function GetPair(Index: Integer): TKMLPair;
    procedure SetPair(Index: Integer; const _Value: TKMLPair);
    procedure SetID(const _Value: String);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPair(_Value: TKMLPair);
    function AddNewPair: TKMLPair;
    procedure PairClear;
    function PairCount: Integer;
    procedure RemovePair(_Value: TKMLPair);
    procedure DeletePair(Index: Integer);
    property Pairs: TList<TKMLPair> read FPairs write SetPairs;
    property Pair[Index: Integer]: TKMLPair read GetPair write SetPair;
    property ID: String read FID write SetID;
  end;

  TKMLScreenOverlay = class(TObject)
  private
    FName: String;
    FNameExsit: Boolean;
    FVisibility: Integer;
    FVisibilityExsit: Boolean;
    FDescription: String;
    FDescriptionExsit: Boolean;
    FIcon: TKMLIcon;
    FIconExsit: Boolean;
    FOverlayXY: TKMLSize;
    FOverlayXYExsit: Boolean;
    FScreenXY: TKMLSize;
    FScreenXYExsit: Boolean;
    FRotationXY: TKMLSize;
    FRotationXYExsit: Boolean;
    FSize: TKMLSize;
    FSizeExsit: Boolean;
    procedure SetName(const _Value: String);
    procedure SetNameExsit(const Value: Boolean);
    procedure SetVisibility(const _Value: Integer);
    procedure SetVisibilityExsit(const Value: Boolean);
    procedure SetDescription(const _Value: String);
    procedure SetDescriptionExsit(const Value: Boolean);
    procedure SetIcon(const _Value: TKMLIcon);
    procedure SetIconExsit(const Value: Boolean);
    procedure SetOverlayXY(const _Value: TKMLSize);
    procedure SetOverlayXYExsit(const Value: Boolean);
    procedure SetScreenXY(const _Value: TKMLSize);
    procedure SetScreenXYExsit(const Value: Boolean);
    procedure SetRotationXY(const _Value: TKMLSize);
    procedure SetRotationXYExsit(const Value: Boolean);
    procedure SetSize(const _Value: TKMLSize);
    procedure SetSizeExsit(const Value: Boolean);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddName: String;
    procedure NameRemove;
    function AddVisibility: Integer;
    procedure VisibilityRemove;
    function AddDescription: String;
    procedure DescriptionRemove;
    function AddIcon: TKMLIcon;
    procedure IconRemove;
    function AddOverlayXY: TKMLSize;
    procedure OverlayXYRemove;
    function AddScreenXY: TKMLSize;
    procedure ScreenXYRemove;
    function AddRotationXY: TKMLSize;
    procedure RotationXYRemove;
    function AddSize: TKMLSize;
    procedure SizeRemove;
    property Name: String read FName write SetName;
    property NameExsit: Boolean read FNameExsit Write SetNameExsit;
    property Visibility: Integer read FVisibility write SetVisibility;
    property VisibilityExsit: Boolean read FVisibilityExsit Write SetVisibilityExsit;
    property Description: String read FDescription write SetDescription;
    property DescriptionExsit: Boolean read FDescriptionExsit Write SetDescriptionExsit;
    property Icon: TKMLIcon read FIcon write SetIcon;
    property IconExsit: Boolean read FIconExsit write SetIconExsit;
    property OverlayXY: TKMLSize read FOverlayXY write SetOverlayXY;
    property OverlayXYExsit: Boolean read FOverlayXYExsit write SetOverlayXYExsit;
    property ScreenXY: TKMLSize read FScreenXY write SetScreenXY;
    property ScreenXYExsit: Boolean read FScreenXYExsit write SetScreenXYExsit;
    property RotationXY: TKMLSize read FRotationXY write SetRotationXY;
    property RotationXYExsit: Boolean read FRotationXYExsit write SetRotationXYExsit;
    property Size: TKMLSize read FSize write SetSize;
    property SizeExsit: Boolean read FSizeExsit write SetSizeExsit;
  end;

  TKMLPair = class(TObject)
  private
    FKey: String;
    FKeyExsit: Boolean;
    FStyleUrl: String;
    FStyleUrlExsit: Boolean;
    procedure SetKey(const _Value: String);
    procedure SetKeyExsit(const Value: Boolean);
    procedure SetStyleUrl(const _Value: String);
    procedure SetStyleUrlExsit(const Value: Boolean);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddKey: String;
    procedure KeyRemove;
    function AddStyleUrl: String;
    procedure StyleUrlRemove;
    property Key: String read FKey write SetKey;
    property KeyExsit: Boolean read FKeyExsit Write SetKeyExsit;
    property StyleUrl: String read FStyleUrl write SetStyleUrl;
    property StyleUrlExsit: Boolean read FStyleUrlExsit Write SetStyleUrlExsit;
  end;


implementation

{  KMLFile}
constructor TKMLFile.Create;
begin
  FDocument := TKMLDocument.Create;
  FFolders := TList<TKMLFolder>.Create;
end;

destructor TKMLFile.Destroy;
begin
  FDocument.Free;
  FolderClear;
  FFolders.Free;
  inherited;
end;

procedure TKMLFile.SetXmlns(const _Value: String);
begin
  FXmlnsExsit := True;
  FXmlns := _Value;
end;

procedure TKMLFile.SetXmlnsExsit(const Value: Boolean);
begin
  FXmlnsExsit := Value;
end;

function TKMLFile.AddXmlns: String;
begin;
  Result := FXmlns;
  FXmlnsExsit := True;
end;

procedure TKMLFile.XmlnsRemove;
begin
  if FXmlnsExsit then
  begin
    FXmlnsExsit := False;
  end;
end;

procedure TKMLFile.SetDocument(const _Value: TKMLDocument);
begin
  FDocument.Free;
  FDocument := _Value;
end;

procedure TKMLFile.SetFolders(const _Value: TList<TKMLFolder>);
begin
  FolderClear;
  FFolders := _Value;
end;

function TKMLFile.GetFolder(Index: Integer): TKMLFolder;
begin
  Result := FFolders[Index];
end;

procedure TKMLFile.SetFolder(Index: Integer;
  const _Value: TKMLFolder);
begin
  FFolders[Index].Free;
  FFolders[Index] := _Value;
end;

procedure TKMLFile.AddFolder(_Value: TKMLFolder);
begin;
  FFolders.Add(_Value);
end;

function TKMLFile.AddNewFolder: TKMLFolder;
var
  Foldertmp: TKMLFolder;
begin;
  Foldertmp := TKMLFolder.Create;
  FFolders.Add(Foldertmp);
  Result := Foldertmp;
end;

procedure TKMLFile.FolderClear;
begin
  while FFolders.Count > 0 do
  begin
    FFolders.Items[0].Free;
    FFolders.Delete(0);
  end;
end;

function TKMLFile.FolderCount: Integer;
begin
  Result := FFolders.Count;
end;

procedure TKMLFile.RemoveFolder(_Value: TKMLFolder);
begin
  FFolders.Remove(_Value);
  _Value.Free;
end;

procedure TKMLFile.DeleteFolder(Index: Integer);
begin
  FFolders.Items[Index].Free;
  FFolders.Delete(Index);
end;

{  Document}
constructor TKMLDocument.Create;
begin
  FStyleMaps := TList<TKMLStyleMap>.Create;
  FStyles := TList<TKMLStyle>.Create;
  FPlacemarks := TList<TKMLPlacemark>.Create;
  FFolders := TList<TKMLFolder>.Create;
end;

destructor TKMLDocument.Destroy;
begin
  if FLookAtExsit then
    FLookAt.Free;
  StyleMapClear;
  FStyleMaps.Free;
  StyleClear;
  FStyles.Free;
  PlacemarkClear;
  FPlacemarks.Free;
  FolderClear;
  FFolders.Free;
  inherited;
end;

procedure TKMLDocument.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TKMLDocument.SetNameExsit(const Value: Boolean);
begin
  FNameExsit := Value;
end;

function TKMLDocument.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TKMLDocument.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TKMLDocument.SetOpen(const _Value: Integer);
begin
  FOpenExsit := True;
  FOpen := _Value;
end;

procedure TKMLDocument.SetOpenExsit(const Value: Boolean);
begin
  FOpenExsit := Value;
end;

function TKMLDocument.AddOpen: Integer;
begin;
  Result := FOpen;
  FOpenExsit := True;
end;

procedure TKMLDocument.OpenRemove;
begin
  if FOpenExsit then
  begin
    FOpenExsit := False;
  end;
end;

procedure TKMLDocument.SetVisibility(const _Value: Integer);
begin
  FVisibilityExsit := True;
  FVisibility := _Value;
end;

procedure TKMLDocument.SetVisibilityExsit(const Value: Boolean);
begin
  FVisibilityExsit := Value;
end;

function TKMLDocument.AddVisibility: Integer;
begin;
  Result := FVisibility;
  FVisibilityExsit := True;
end;

procedure TKMLDocument.VisibilityRemove;
begin
  if FVisibilityExsit then
  begin
    FVisibilityExsit := False;
  end;
end;

procedure TKMLDocument.SetDescription(const _Value: String);
begin
  FDescriptionExsit := True;
  FDescription := _Value;
end;

procedure TKMLDocument.SetDescriptionExsit(const Value: Boolean);
begin
  FDescriptionExsit := Value;
end;

function TKMLDocument.AddDescription: String;
begin;
  Result := FDescription;
  FDescriptionExsit := True;
end;

procedure TKMLDocument.DescriptionRemove;
begin
  if FDescriptionExsit then
  begin
    FDescriptionExsit := False;
  end;
end;

procedure TKMLDocument.SetLookAt(const _Value: TKMLLookAt);
begin
  if FLookAtExsit then
    FLookAt.Free;
  FLookAtExsit := True;
  FLookAt := _Value;
end;

procedure TKMLDocument.SetLookAtExsit(const Value: Boolean);
begin
  FLookAtExsit := Value;
end;

function TKMLDocument.AddLookAt: TKMLLookAt;
begin;
  if not FLookAtExsit then
    FLookAt := TKMLLookAt.Create;
  Result := FLookAt;
  FLookAtExsit := True;
end;

procedure TKMLDocument.LookAtRemove;
begin
  if FLookAtExsit then
  begin
    FLookAt.Free;
    FLookAtExsit := False;
  end;
end;

procedure TKMLDocument.SetStyleMaps(const _Value: TList<TKMLStyleMap>);
begin
  StyleMapClear;
  FStyleMaps := _Value;
end;

function TKMLDocument.GetStyleMap(Index: Integer): TKMLStyleMap;
begin
  Result := FStyleMaps[Index];
end;

procedure TKMLDocument.SetStyleMap(Index: Integer;
  const _Value: TKMLStyleMap);
begin
  FStyleMaps[Index].Free;
  FStyleMaps[Index] := _Value;
end;

procedure TKMLDocument.AddStyleMap(_Value: TKMLStyleMap);
begin;
  FStyleMaps.Add(_Value);
end;

function TKMLDocument.AddNewStyleMap: TKMLStyleMap;
var
  StyleMaptmp: TKMLStyleMap;
begin;
  StyleMaptmp := TKMLStyleMap.Create;
  FStyleMaps.Add(StyleMaptmp);
  Result := StyleMaptmp;
end;

procedure TKMLDocument.StyleMapClear;
begin
  while FStyleMaps.Count > 0 do
  begin
    FStyleMaps.Items[0].Free;
    FStyleMaps.Delete(0);
  end;
end;

function TKMLDocument.StyleMapCount: Integer;
begin
  Result := FStyleMaps.Count;
end;

procedure TKMLDocument.RemoveStyleMap(_Value: TKMLStyleMap);
begin
  FStyleMaps.Remove(_Value);
  _Value.Free;
end;

procedure TKMLDocument.DeleteStyleMap(Index: Integer);
begin
  FStyleMaps.Items[Index].Free;
  FStyleMaps.Delete(Index);
end;

procedure TKMLDocument.SetStyles(const _Value: TList<TKMLStyle>);
begin
  StyleClear;
  FStyles := _Value;
end;

function TKMLDocument.GetStyle(Index: Integer): TKMLStyle;
begin
  Result := FStyles[Index];
end;

procedure TKMLDocument.SetStyle(Index: Integer;
  const _Value: TKMLStyle);
begin
  FStyles[Index].Free;
  FStyles[Index] := _Value;
end;

procedure TKMLDocument.AddStyle(_Value: TKMLStyle);
begin;
  FStyles.Add(_Value);
end;

function TKMLDocument.AddNewStyle: TKMLStyle;
var
  Styletmp: TKMLStyle;
begin;
  Styletmp := TKMLStyle.Create;
  FStyles.Add(Styletmp);
  Result := Styletmp;
end;

procedure TKMLDocument.StyleClear;
begin
  while FStyles.Count > 0 do
  begin
    FStyles.Items[0].Free;
    FStyles.Delete(0);
  end;
end;

function TKMLDocument.StyleCount: Integer;
begin
  Result := FStyles.Count;
end;

procedure TKMLDocument.RemoveStyle(_Value: TKMLStyle);
begin
  FStyles.Remove(_Value);
  _Value.Free;
end;

procedure TKMLDocument.DeleteStyle(Index: Integer);
begin
  FStyles.Items[Index].Free;
  FStyles.Delete(Index);
end;

procedure TKMLDocument.SetPlacemarks(const _Value: TList<TKMLPlacemark>);
begin
  PlacemarkClear;
  FPlacemarks := _Value;
end;

function TKMLDocument.GetPlacemark(Index: Integer): TKMLPlacemark;
begin
  Result := FPlacemarks[Index];
end;

procedure TKMLDocument.SetPlacemark(Index: Integer;
  const _Value: TKMLPlacemark);
begin
  FPlacemarks[Index].Free;
  FPlacemarks[Index] := _Value;
end;

procedure TKMLDocument.AddPlacemark(_Value: TKMLPlacemark);
begin;
  FPlacemarks.Add(_Value);
end;

function TKMLDocument.AddNewPlacemark: TKMLPlacemark;
var
  Placemarktmp: TKMLPlacemark;
begin;
  Placemarktmp := TKMLPlacemark.Create;
  FPlacemarks.Add(Placemarktmp);
  Result := Placemarktmp;
end;

procedure TKMLDocument.PlacemarkClear;
begin
  while FPlacemarks.Count > 0 do
  begin
    FPlacemarks.Items[0].Free;
    FPlacemarks.Delete(0);
  end;
end;

function TKMLDocument.PlacemarkCount: Integer;
begin
  Result := FPlacemarks.Count;
end;

procedure TKMLDocument.RemovePlacemark(_Value: TKMLPlacemark);
begin
  FPlacemarks.Remove(_Value);
  _Value.Free;
end;

procedure TKMLDocument.DeletePlacemark(Index: Integer);
begin
  FPlacemarks.Items[Index].Free;
  FPlacemarks.Delete(Index);
end;

procedure TKMLDocument.SetFolders(const _Value: TList<TKMLFolder>);
begin
  FolderClear;
  FFolders := _Value;
end;

function TKMLDocument.GetFolder(Index: Integer): TKMLFolder;
begin
  Result := FFolders[Index];
end;

procedure TKMLDocument.SetFolder(Index: Integer;
  const _Value: TKMLFolder);
begin
  FFolders[Index].Free;
  FFolders[Index] := _Value;
end;

procedure TKMLDocument.AddFolder(_Value: TKMLFolder);
begin;
  FFolders.Add(_Value);
end;

function TKMLDocument.AddNewFolder: TKMLFolder;
var
  Foldertmp: TKMLFolder;
begin;
  Foldertmp := TKMLFolder.Create;
  FFolders.Add(Foldertmp);
  Result := Foldertmp;
end;

procedure TKMLDocument.FolderClear;
begin
  while FFolders.Count > 0 do
  begin
    FFolders.Items[0].Free;
    FFolders.Delete(0);
  end;
end;

function TKMLDocument.FolderCount: Integer;
begin
  Result := FFolders.Count;
end;

procedure TKMLDocument.RemoveFolder(_Value: TKMLFolder);
begin
  FFolders.Remove(_Value);
  _Value.Free;
end;

procedure TKMLDocument.DeleteFolder(Index: Integer);
begin
  FFolders.Items[Index].Free;
  FFolders.Delete(Index);
end;

{  Folder}
constructor TKMLFolder.Create;
begin
  FGroundOverlays := TList<TKMLGroundOverlay>.Create;
  FPlacemarks := TList<TKMLPlacemark>.Create;
  FScreenOverlays := TList<TKMLScreenOverlay>.Create;
  FFolders := TList<TKMLFolder>.Create;
end;

destructor TKMLFolder.Destroy;
begin
  if FLookAtExsit then
    FLookAt.Free;
  if FDocumentExsit then
    FDocument.Free;
  GroundOverlayClear;
  FGroundOverlays.Free;
  PlacemarkClear;
  FPlacemarks.Free;
  ScreenOverlayClear;
  FScreenOverlays.Free;
  FolderClear;
  FFolders.Free;
  inherited;
end;

procedure TKMLFolder.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TKMLFolder.SetNameExsit(const Value: Boolean);
begin
  FNameExsit := Value;
end;

function TKMLFolder.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TKMLFolder.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TKMLFolder.SetOpen(const _Value: Integer);
begin
  FOpenExsit := True;
  FOpen := _Value;
end;

procedure TKMLFolder.SetOpenExsit(const Value: Boolean);
begin
  FOpenExsit := Value;
end;

function TKMLFolder.AddOpen: Integer;
begin;
  Result := FOpen;
  FOpenExsit := True;
end;

procedure TKMLFolder.OpenRemove;
begin
  if FOpenExsit then
  begin
    FOpenExsit := False;
  end;
end;

procedure TKMLFolder.SetVisibility(const _Value: Integer);
begin
  FVisibilityExsit := True;
  FVisibility := _Value;
end;

procedure TKMLFolder.SetVisibilityExsit(const Value: Boolean);
begin
  FVisibilityExsit := Value;
end;

function TKMLFolder.AddVisibility: Integer;
begin;
  Result := FVisibility;
  FVisibilityExsit := True;
end;

procedure TKMLFolder.VisibilityRemove;
begin
  if FVisibilityExsit then
  begin
    FVisibilityExsit := False;
  end;
end;

procedure TKMLFolder.SetDescription(const _Value: String);
begin
  FDescriptionExsit := True;
  FDescription := _Value;
end;

procedure TKMLFolder.SetDescriptionExsit(const Value: Boolean);
begin
  FDescriptionExsit := Value;
end;

function TKMLFolder.AddDescription: String;
begin;
  Result := FDescription;
  FDescriptionExsit := True;
end;

procedure TKMLFolder.DescriptionRemove;
begin
  if FDescriptionExsit then
  begin
    FDescriptionExsit := False;
  end;
end;

procedure TKMLFolder.SetLookAt(const _Value: TKMLLookAt);
begin
  if FLookAtExsit then
    FLookAt.Free;
  FLookAtExsit := True;
  FLookAt := _Value;
end;

procedure TKMLFolder.SetLookAtExsit(const Value: Boolean);
begin
  FLookAtExsit := Value;
end;

function TKMLFolder.AddLookAt: TKMLLookAt;
begin;
  if not FLookAtExsit then
    FLookAt := TKMLLookAt.Create;
  Result := FLookAt;
  FLookAtExsit := True;
end;

procedure TKMLFolder.LookAtRemove;
begin
  if FLookAtExsit then
  begin
    FLookAt.Free;
    FLookAtExsit := False;
  end;
end;

procedure TKMLFolder.SetDocument(const _Value: TKMLDocument);
begin
  if FDocumentExsit then
    FDocument.Free;
  FDocumentExsit := True;
  FDocument := _Value;
end;

procedure TKMLFolder.SetDocumentExsit(const Value: Boolean);
begin
  FDocumentExsit := Value;
end;

function TKMLFolder.AddDocument: TKMLDocument;
begin;
  if not FDocumentExsit then
    FDocument := TKMLDocument.Create;
  Result := FDocument;
  FDocumentExsit := True;
end;

procedure TKMLFolder.DocumentRemove;
begin
  if FDocumentExsit then
  begin
    FDocument.Free;
    FDocumentExsit := False;
  end;
end;

procedure TKMLFolder.SetGroundOverlays(const _Value: TList<TKMLGroundOverlay>);
begin
  GroundOverlayClear;
  FGroundOverlays := _Value;
end;

function TKMLFolder.GetGroundOverlay(Index: Integer): TKMLGroundOverlay;
begin
  Result := FGroundOverlays[Index];
end;

procedure TKMLFolder.SetGroundOverlay(Index: Integer;
  const _Value: TKMLGroundOverlay);
begin
  FGroundOverlays[Index].Free;
  FGroundOverlays[Index] := _Value;
end;

procedure TKMLFolder.AddGroundOverlay(_Value: TKMLGroundOverlay);
begin;
  FGroundOverlays.Add(_Value);
end;

function TKMLFolder.AddNewGroundOverlay: TKMLGroundOverlay;
var
  GroundOverlaytmp: TKMLGroundOverlay;
begin;
  GroundOverlaytmp := TKMLGroundOverlay.Create;
  FGroundOverlays.Add(GroundOverlaytmp);
  Result := GroundOverlaytmp;
end;

procedure TKMLFolder.GroundOverlayClear;
begin
  while FGroundOverlays.Count > 0 do
  begin
    FGroundOverlays.Items[0].Free;
    FGroundOverlays.Delete(0);
  end;
end;

function TKMLFolder.GroundOverlayCount: Integer;
begin
  Result := FGroundOverlays.Count;
end;

procedure TKMLFolder.RemoveGroundOverlay(_Value: TKMLGroundOverlay);
begin
  FGroundOverlays.Remove(_Value);
  _Value.Free;
end;

procedure TKMLFolder.DeleteGroundOverlay(Index: Integer);
begin
  FGroundOverlays.Items[Index].Free;
  FGroundOverlays.Delete(Index);
end;

procedure TKMLFolder.SetPlacemarks(const _Value: TList<TKMLPlacemark>);
begin
  PlacemarkClear;
  FPlacemarks := _Value;
end;

function TKMLFolder.GetPlacemark(Index: Integer): TKMLPlacemark;
begin
  Result := FPlacemarks[Index];
end;

procedure TKMLFolder.SetPlacemark(Index: Integer;
  const _Value: TKMLPlacemark);
begin
  FPlacemarks[Index].Free;
  FPlacemarks[Index] := _Value;
end;

procedure TKMLFolder.AddPlacemark(_Value: TKMLPlacemark);
begin;
  FPlacemarks.Add(_Value);
end;

function TKMLFolder.AddNewPlacemark: TKMLPlacemark;
var
  Placemarktmp: TKMLPlacemark;
begin;
  Placemarktmp := TKMLPlacemark.Create;
  FPlacemarks.Add(Placemarktmp);
  Result := Placemarktmp;
end;

procedure TKMLFolder.PlacemarkClear;
begin
  while FPlacemarks.Count > 0 do
  begin
    FPlacemarks.Items[0].Free;
    FPlacemarks.Delete(0);
  end;
end;

function TKMLFolder.PlacemarkCount: Integer;
begin
  Result := FPlacemarks.Count;
end;

procedure TKMLFolder.RemovePlacemark(_Value: TKMLPlacemark);
begin
  FPlacemarks.Remove(_Value);
  _Value.Free;
end;

procedure TKMLFolder.DeletePlacemark(Index: Integer);
begin
  FPlacemarks.Items[Index].Free;
  FPlacemarks.Delete(Index);
end;

procedure TKMLFolder.SetScreenOverlays(const _Value: TList<TKMLScreenOverlay>);
begin
  ScreenOverlayClear;
  FScreenOverlays := _Value;
end;

function TKMLFolder.GetScreenOverlay(Index: Integer): TKMLScreenOverlay;
begin
  Result := FScreenOverlays[Index];
end;

procedure TKMLFolder.SetScreenOverlay(Index: Integer;
  const _Value: TKMLScreenOverlay);
begin
  FScreenOverlays[Index].Free;
  FScreenOverlays[Index] := _Value;
end;

procedure TKMLFolder.AddScreenOverlay(_Value: TKMLScreenOverlay);
begin;
  FScreenOverlays.Add(_Value);
end;

function TKMLFolder.AddNewScreenOverlay: TKMLScreenOverlay;
var
  ScreenOverlaytmp: TKMLScreenOverlay;
begin;
  ScreenOverlaytmp := TKMLScreenOverlay.Create;
  FScreenOverlays.Add(ScreenOverlaytmp);
  Result := ScreenOverlaytmp;
end;

procedure TKMLFolder.ScreenOverlayClear;
begin
  while FScreenOverlays.Count > 0 do
  begin
    FScreenOverlays.Items[0].Free;
    FScreenOverlays.Delete(0);
  end;
end;

function TKMLFolder.ScreenOverlayCount: Integer;
begin
  Result := FScreenOverlays.Count;
end;

procedure TKMLFolder.RemoveScreenOverlay(_Value: TKMLScreenOverlay);
begin
  FScreenOverlays.Remove(_Value);
  _Value.Free;
end;

procedure TKMLFolder.DeleteScreenOverlay(Index: Integer);
begin
  FScreenOverlays.Items[Index].Free;
  FScreenOverlays.Delete(Index);
end;

procedure TKMLFolder.SetFolders(const _Value: TList<TKMLFolder>);
begin
  FolderClear;
  FFolders := _Value;
end;

function TKMLFolder.GetFolder(Index: Integer): TKMLFolder;
begin
  Result := FFolders[Index];
end;

procedure TKMLFolder.SetFolder(Index: Integer;
  const _Value: TKMLFolder);
begin
  FFolders[Index].Free;
  FFolders[Index] := _Value;
end;

procedure TKMLFolder.AddFolder(_Value: TKMLFolder);
begin;
  FFolders.Add(_Value);
end;

function TKMLFolder.AddNewFolder: TKMLFolder;
var
  Foldertmp: TKMLFolder;
begin;
  Foldertmp := TKMLFolder.Create;
  FFolders.Add(Foldertmp);
  Result := Foldertmp;
end;

procedure TKMLFolder.FolderClear;
begin
  while FFolders.Count > 0 do
  begin
    FFolders.Items[0].Free;
    FFolders.Delete(0);
  end;
end;

function TKMLFolder.FolderCount: Integer;
begin
  Result := FFolders.Count;
end;

procedure TKMLFolder.RemoveFolder(_Value: TKMLFolder);
begin
  FFolders.Remove(_Value);
  _Value.Free;
end;

procedure TKMLFolder.DeleteFolder(Index: Integer);
begin
  FFolders.Items[Index].Free;
  FFolders.Delete(Index);
end;

{  Placemark}
constructor TKMLPlacemark.Create;
begin
end;

destructor TKMLPlacemark.Destroy;
begin
  if FLookAtExsit then
    FLookAt.Free;
  if FGeometryExsit then
    FGeometry.Free;
  if FExtendedDataExsit then
    FExtendedData.Free;
  inherited;
end;

procedure TKMLPlacemark.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TKMLPlacemark.SetNameExsit(const Value: Boolean);
begin
  FNameExsit := Value;
end;

function TKMLPlacemark.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TKMLPlacemark.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TKMLPlacemark.SetVisibility(const _Value: Integer);
begin
  FVisibilityExsit := True;
  FVisibility := _Value;
end;

procedure TKMLPlacemark.SetVisibilityExsit(const Value: Boolean);
begin
  FVisibilityExsit := Value;
end;

function TKMLPlacemark.AddVisibility: Integer;
begin;
  Result := FVisibility;
  FVisibilityExsit := True;
end;

procedure TKMLPlacemark.VisibilityRemove;
begin
  if FVisibilityExsit then
  begin
    FVisibilityExsit := False;
  end;
end;

procedure TKMLPlacemark.SetDescription(const _Value: String);
begin
  FDescriptionExsit := True;
  FDescription := _Value;
end;

procedure TKMLPlacemark.SetDescriptionExsit(const Value: Boolean);
begin
  FDescriptionExsit := Value;
end;

function TKMLPlacemark.AddDescription: String;
begin;
  Result := FDescription;
  FDescriptionExsit := True;
end;

procedure TKMLPlacemark.DescriptionRemove;
begin
  if FDescriptionExsit then
  begin
    FDescriptionExsit := False;
  end;
end;

procedure TKMLPlacemark.SetLookAt(const _Value: TKMLLookAt);
begin
  if FLookAtExsit then
    FLookAt.Free;
  FLookAtExsit := True;
  FLookAt := _Value;
end;

procedure TKMLPlacemark.SetLookAtExsit(const Value: Boolean);
begin
  FLookAtExsit := Value;
end;

function TKMLPlacemark.AddLookAt: TKMLLookAt;
begin;
  if not FLookAtExsit then
    FLookAt := TKMLLookAt.Create;
  Result := FLookAt;
  FLookAtExsit := True;
end;

procedure TKMLPlacemark.LookAtRemove;
begin
  if FLookAtExsit then
  begin
    FLookAt.Free;
    FLookAtExsit := False;
  end;
end;

procedure TKMLPlacemark.SetStyleUrl(const _Value: String);
begin
  FStyleUrlExsit := True;
  FStyleUrl := _Value;
end;

procedure TKMLPlacemark.SetStyleUrlExsit(const Value: Boolean);
begin
  FStyleUrlExsit := Value;
end;

function TKMLPlacemark.AddStyleUrl: String;
begin;
  Result := FStyleUrl;
  FStyleUrlExsit := True;
end;

procedure TKMLPlacemark.StyleUrlRemove;
begin
  if FStyleUrlExsit then
  begin
    FStyleUrlExsit := False;
  end;
end;

procedure TKMLPlacemark.SetGeometry(const _Value: TKMLGeometry);
begin
  if FGeometryExsit then
    FGeometry.Free;
  FGeometryExsit := True;
  FGeometry := _Value;
end;

procedure TKMLPlacemark.SetGeometryExsit(const Value: Boolean);
begin
  FGeometryExsit := Value;
end;

function TKMLPlacemark.AddPoint: TKMLPoint;
begin;
  if not FGeometryExsit then
  FGeometry.Free;
  FGeometry := TKMLPoint.Create;
  Result := TKMLPoint(FGeometry);
  FGeometryExsit := True;
end;

function TKMLPlacemark.AddPolygon: TKMLPolygon;
begin;
  if not FGeometryExsit then
  FGeometry.Free;
  FGeometry := TKMLPolygon.Create;
  Result := TKMLPolygon(FGeometry);
  FGeometryExsit := True;
end;

function TKMLPlacemark.AddLinearRing: TKMLLinearRing;
begin;
  if not FGeometryExsit then
  FGeometry.Free;
  FGeometry := TKMLLinearRing.Create;
  Result := TKMLLinearRing(FGeometry);
  FGeometryExsit := True;
end;

function TKMLPlacemark.AddLineString: TKMLLineString;
begin;
  if not FGeometryExsit then
  FGeometry.Free;
  FGeometry := TKMLLineString.Create;
  Result := TKMLLineString(FGeometry);
  FGeometryExsit := True;
end;

function TKMLPlacemark.AddMultiGeometry: TKMLMultiGeometry;
begin;
  if not FGeometryExsit then
  FGeometry.Free;
  FGeometry := TKMLMultiGeometry.Create;
  Result := TKMLMultiGeometry(FGeometry);
  FGeometryExsit := True;
end;

procedure TKMLPlacemark.GeometryRemove;
begin
  if FGeometryExsit then
  begin
    FGeometry.Free;
    FGeometryExsit := False;
  end;
end;

procedure TKMLPlacemark.SetExtendedData(const _Value: TKMLExtendedData);
begin
  if FExtendedDataExsit then
    FExtendedData.Free;
  FExtendedDataExsit := True;
  FExtendedData := _Value;
end;

procedure TKMLPlacemark.SetExtendedDataExsit(const Value: Boolean);
begin
  FExtendedDataExsit := Value;
end;

function TKMLPlacemark.AddExtendedData: TKMLExtendedData;
begin;
  if not FExtendedDataExsit then
    FExtendedData := TKMLExtendedData.Create;
  Result := FExtendedData;
  FExtendedDataExsit := True;
end;

procedure TKMLPlacemark.ExtendedDataRemove;
begin
  if FExtendedDataExsit then
  begin
    FExtendedData.Free;
    FExtendedDataExsit := False;
  end;
end;

{  Geometry}
constructor TKMLGeometry.Create;
begin
end;

destructor TKMLGeometry.Destroy;
begin
  inherited;
end;

{  MultiGeometry}
constructor TKMLMultiGeometry.Create;
begin
  FGeometrys := TList<TKMLGeometry>.Create;
end;

destructor TKMLMultiGeometry.Destroy;
begin
  GeometryClear;
  FGeometrys.Free;
  inherited;
end;

procedure TKMLMultiGeometry.SetGeometrys(const _Value: TList<TKMLGeometry>);
begin
  GeometryClear;
  FGeometrys := _Value;
end;

function TKMLMultiGeometry.GetGeometry(Index: Integer): TKMLGeometry;
begin
  Result := FGeometrys[Index];
end;

procedure TKMLMultiGeometry.SetGeometry(Index: Integer;
  const _Value: TKMLGeometry);
begin
  FGeometrys[Index].Free;
  FGeometrys[Index] := _Value;
end;

procedure TKMLMultiGeometry.AddGeometry(_Value: TKMLGeometry);
begin;
  FGeometrys.Add(_Value);
end;

function TKMLMultiGeometry.AddNewPoint: TKMLPoint;
var
  Pointtmp: TKMLPoint;
begin;
  Pointtmp := TKMLPoint.Create;
  FGeometrys.Add(Pointtmp);
  Result := Pointtmp;
end;

function TKMLMultiGeometry.AddNewPolygon: TKMLPolygon;
var
  Polygontmp: TKMLPolygon;
begin;
  Polygontmp := TKMLPolygon.Create;
  FGeometrys.Add(Polygontmp);
  Result := Polygontmp;
end;

function TKMLMultiGeometry.AddNewLinearRing: TKMLLinearRing;
var
  LinearRingtmp: TKMLLinearRing;
begin;
  LinearRingtmp := TKMLLinearRing.Create;
  FGeometrys.Add(LinearRingtmp);
  Result := LinearRingtmp;
end;

function TKMLMultiGeometry.AddNewLineString: TKMLLineString;
var
  LineStringtmp: TKMLLineString;
begin;
  LineStringtmp := TKMLLineString.Create;
  FGeometrys.Add(LineStringtmp);
  Result := LineStringtmp;
end;

function TKMLMultiGeometry.AddNewMultiGeometry: TKMLMultiGeometry;
var
  MultiGeometrytmp: TKMLMultiGeometry;
begin;
  MultiGeometrytmp := TKMLMultiGeometry.Create;
  FGeometrys.Add(MultiGeometrytmp);
  Result := MultiGeometrytmp;
end;

procedure TKMLMultiGeometry.GeometryClear;
begin
  while FGeometrys.Count > 0 do
  begin
    FGeometrys.Items[0].Free;
    FGeometrys.Delete(0);
  end;
end;

function TKMLMultiGeometry.GeometryCount: Integer;
begin
  Result := FGeometrys.Count;
end;

procedure TKMLMultiGeometry.RemoveGeometry(_Value: TKMLGeometry);
begin
  FGeometrys.Remove(_Value);
  _Value.Free;
end;

procedure TKMLMultiGeometry.DeleteGeometry(Index: Integer);
begin
  FGeometrys.Items[Index].Free;
  FGeometrys.Delete(Index);
end;

{  Point}
constructor TKMLPoint.Create;
begin
end;

destructor TKMLPoint.Destroy;
begin
  inherited;
end;

procedure TKMLPoint.SetExtrude(const _Value: String);
begin
  FExtrudeExsit := True;
  FExtrude := _Value;
end;

procedure TKMLPoint.SetExtrudeExsit(const Value: Boolean);
begin
  FExtrudeExsit := Value;
end;

function TKMLPoint.AddExtrude: String;
begin;
  Result := FExtrude;
  FExtrudeExsit := True;
end;

procedure TKMLPoint.ExtrudeRemove;
begin
  if FExtrudeExsit then
  begin
    FExtrudeExsit := False;
  end;
end;

procedure TKMLPoint.SetAltitudeMode(const _Value: String);
begin
  FAltitudeModeExsit := True;
  FAltitudeMode := _Value;
end;

procedure TKMLPoint.SetAltitudeModeExsit(const Value: Boolean);
begin
  FAltitudeModeExsit := Value;
end;

function TKMLPoint.AddAltitudeMode: String;
begin;
  Result := FAltitudeMode;
  FAltitudeModeExsit := True;
end;

procedure TKMLPoint.AltitudeModeRemove;
begin
  if FAltitudeModeExsit then
  begin
    FAltitudeModeExsit := False;
  end;
end;

procedure TKMLPoint.SetCoordinates(const _Value: ArrayCoordinates);
begin
  FCoordinates := _Value;
end;

{  Polygon}
constructor TKMLPolygon.Create;
begin
  FOuterBoundaryIss := TList<TKMLBoundary>.Create;
  FInnerBoundaryIss := TList<TKMLBoundary>.Create;
end;

destructor TKMLPolygon.Destroy;
begin
  OuterBoundaryIsClear;
  FOuterBoundaryIss.Free;
  InnerBoundaryIsClear;
  FInnerBoundaryIss.Free;
  inherited;
end;

procedure TKMLPolygon.SetExtrude(const _Value: String);
begin
  FExtrudeExsit := True;
  FExtrude := _Value;
end;

procedure TKMLPolygon.SetExtrudeExsit(const Value: Boolean);
begin
  FExtrudeExsit := Value;
end;

function TKMLPolygon.AddExtrude: String;
begin;
  Result := FExtrude;
  FExtrudeExsit := True;
end;

procedure TKMLPolygon.ExtrudeRemove;
begin
  if FExtrudeExsit then
  begin
    FExtrudeExsit := False;
  end;
end;

procedure TKMLPolygon.SetAltitudeMode(const _Value: String);
begin
  FAltitudeModeExsit := True;
  FAltitudeMode := _Value;
end;

procedure TKMLPolygon.SetAltitudeModeExsit(const Value: Boolean);
begin
  FAltitudeModeExsit := Value;
end;

function TKMLPolygon.AddAltitudeMode: String;
begin;
  Result := FAltitudeMode;
  FAltitudeModeExsit := True;
end;

procedure TKMLPolygon.AltitudeModeRemove;
begin
  if FAltitudeModeExsit then
  begin
    FAltitudeModeExsit := False;
  end;
end;

procedure TKMLPolygon.SetOuterBoundaryIss(const _Value: TList<TKMLBoundary>);
begin
  OuterBoundaryIsClear;
  FOuterBoundaryIss := _Value;
end;

function TKMLPolygon.GetOuterBoundaryIs(Index: Integer): TKMLBoundary;
begin
  Result := FOuterBoundaryIss[Index];
end;

procedure TKMLPolygon.SetOuterBoundaryIs(Index: Integer;
  const _Value: TKMLBoundary);
begin
  FOuterBoundaryIss[Index].Free;
  FOuterBoundaryIss[Index] := _Value;
end;

procedure TKMLPolygon.AddOuterBoundaryIs(_Value: TKMLBoundary);
begin;
  FOuterBoundaryIss.Add(_Value);
end;

function TKMLPolygon.AddNewOuterBoundaryIs: TKMLBoundary;
var
  OuterBoundaryIstmp: TKMLBoundary;
begin;
  OuterBoundaryIstmp := TKMLBoundary.Create;
  FOuterBoundaryIss.Add(OuterBoundaryIstmp);
  Result := OuterBoundaryIstmp;
end;

procedure TKMLPolygon.OuterBoundaryIsClear;
begin
  while FOuterBoundaryIss.Count > 0 do
  begin
    FOuterBoundaryIss.Items[0].Free;
    FOuterBoundaryIss.Delete(0);
  end;
end;

function TKMLPolygon.OuterBoundaryIsCount: Integer;
begin
  Result := FOuterBoundaryIss.Count;
end;

procedure TKMLPolygon.RemoveOuterBoundaryIs(_Value: TKMLBoundary);
begin
  FOuterBoundaryIss.Remove(_Value);
  _Value.Free;
end;

procedure TKMLPolygon.DeleteOuterBoundaryIs(Index: Integer);
begin
  FOuterBoundaryIss.Items[Index].Free;
  FOuterBoundaryIss.Delete(Index);
end;

procedure TKMLPolygon.SetInnerBoundaryIss(const _Value: TList<TKMLBoundary>);
begin
  InnerBoundaryIsClear;
  FInnerBoundaryIss := _Value;
end;

function TKMLPolygon.GetInnerBoundaryIs(Index: Integer): TKMLBoundary;
begin
  Result := FInnerBoundaryIss[Index];
end;

procedure TKMLPolygon.SetInnerBoundaryIs(Index: Integer;
  const _Value: TKMLBoundary);
begin
  FInnerBoundaryIss[Index].Free;
  FInnerBoundaryIss[Index] := _Value;
end;

procedure TKMLPolygon.AddInnerBoundaryIs(_Value: TKMLBoundary);
begin;
  FInnerBoundaryIss.Add(_Value);
end;

function TKMLPolygon.AddNewInnerBoundaryIs: TKMLBoundary;
var
  InnerBoundaryIstmp: TKMLBoundary;
begin;
  InnerBoundaryIstmp := TKMLBoundary.Create;
  FInnerBoundaryIss.Add(InnerBoundaryIstmp);
  Result := InnerBoundaryIstmp;
end;

procedure TKMLPolygon.InnerBoundaryIsClear;
begin
  while FInnerBoundaryIss.Count > 0 do
  begin
    FInnerBoundaryIss.Items[0].Free;
    FInnerBoundaryIss.Delete(0);
  end;
end;

function TKMLPolygon.InnerBoundaryIsCount: Integer;
begin
  Result := FInnerBoundaryIss.Count;
end;

procedure TKMLPolygon.RemoveInnerBoundaryIs(_Value: TKMLBoundary);
begin
  FInnerBoundaryIss.Remove(_Value);
  _Value.Free;
end;

procedure TKMLPolygon.DeleteInnerBoundaryIs(Index: Integer);
begin
  FInnerBoundaryIss.Items[Index].Free;
  FInnerBoundaryIss.Delete(Index);
end;

procedure TKMLPolygon.SetTessellate(const _Value: String);
begin
  FTessellateExsit := True;
  FTessellate := _Value;
end;

procedure TKMLPolygon.SetTessellateExsit(const Value: Boolean);
begin
  FTessellateExsit := Value;
end;

function TKMLPolygon.AddTessellate: String;
begin;
  Result := FTessellate;
  FTessellateExsit := True;
end;

procedure TKMLPolygon.TessellateRemove;
begin
  if FTessellateExsit then
  begin
    FTessellateExsit := False;
  end;
end;

{  Boundary}
constructor TKMLBoundary.Create;
begin
  FLinearRing := TKMLLinearRing.Create;
end;

destructor TKMLBoundary.Destroy;
begin
  FLinearRing.Free;
  inherited;
end;

procedure TKMLBoundary.SetLinearRing(const _Value: TKMLLinearRing);
begin
  FLinearRing.Free;
  FLinearRing := _Value;
end;

{  LinearRing}
constructor TKMLLinearRing.Create;
begin
end;

destructor TKMLLinearRing.Destroy;
begin
  inherited;
end;

procedure TKMLLinearRing.SetCoordinates(const _Value: ArrayCoordinates);
begin
  FCoordinates := _Value;
end;

{  LineString}
constructor TKMLLineString.Create;
begin
end;

destructor TKMLLineString.Destroy;
begin
  inherited;
end;

procedure TKMLLineString.SetTessellate(const _Value: String);
begin
  FTessellateExsit := True;
  FTessellate := _Value;
end;

procedure TKMLLineString.SetTessellateExsit(const Value: Boolean);
begin
  FTessellateExsit := Value;
end;

function TKMLLineString.AddTessellate: String;
begin;
  Result := FTessellate;
  FTessellateExsit := True;
end;

procedure TKMLLineString.TessellateRemove;
begin
  if FTessellateExsit then
  begin
    FTessellateExsit := False;
  end;
end;

procedure TKMLLineString.SetAltitudeMode(const _Value: String);
begin
  FAltitudeModeExsit := True;
  FAltitudeMode := _Value;
end;

procedure TKMLLineString.SetAltitudeModeExsit(const Value: Boolean);
begin
  FAltitudeModeExsit := Value;
end;

function TKMLLineString.AddAltitudeMode: String;
begin;
  Result := FAltitudeMode;
  FAltitudeModeExsit := True;
end;

procedure TKMLLineString.AltitudeModeRemove;
begin
  if FAltitudeModeExsit then
  begin
    FAltitudeModeExsit := False;
  end;
end;

procedure TKMLLineString.SetExtrude(const _Value: String);
begin
  FExtrudeExsit := True;
  FExtrude := _Value;
end;

procedure TKMLLineString.SetExtrudeExsit(const Value: Boolean);
begin
  FExtrudeExsit := Value;
end;

function TKMLLineString.AddExtrude: String;
begin;
  Result := FExtrude;
  FExtrudeExsit := True;
end;

procedure TKMLLineString.ExtrudeRemove;
begin
  if FExtrudeExsit then
  begin
    FExtrudeExsit := False;
  end;
end;

procedure TKMLLineString.SetCoordinates(const _Value: ArrayCoordinates);
begin
  FCoordinates := _Value;
end;

{  LookAt}
constructor TKMLLookAt.Create;
begin
end;

destructor TKMLLookAt.Destroy;
begin
  inherited;
end;

procedure TKMLLookAt.SetLongitude(const _Value: Double);
begin
  FLongitudeExsit := True;
  FLongitude := _Value;
end;

procedure TKMLLookAt.SetLongitudeExsit(const Value: Boolean);
begin
  FLongitudeExsit := Value;
end;

function TKMLLookAt.AddLongitude: Double;
begin;
  Result := FLongitude;
  FLongitudeExsit := True;
end;

procedure TKMLLookAt.LongitudeRemove;
begin
  if FLongitudeExsit then
  begin
    FLongitudeExsit := False;
  end;
end;

procedure TKMLLookAt.SetLatitude(const _Value: Double);
begin
  FLatitudeExsit := True;
  FLatitude := _Value;
end;

procedure TKMLLookAt.SetLatitudeExsit(const Value: Boolean);
begin
  FLatitudeExsit := Value;
end;

function TKMLLookAt.AddLatitude: Double;
begin;
  Result := FLatitude;
  FLatitudeExsit := True;
end;

procedure TKMLLookAt.LatitudeRemove;
begin
  if FLatitudeExsit then
  begin
    FLatitudeExsit := False;
  end;
end;

procedure TKMLLookAt.SetAltitude(const _Value: Double);
begin
  FAltitudeExsit := True;
  FAltitude := _Value;
end;

procedure TKMLLookAt.SetAltitudeExsit(const Value: Boolean);
begin
  FAltitudeExsit := Value;
end;

function TKMLLookAt.AddAltitude: Double;
begin;
  Result := FAltitude;
  FAltitudeExsit := True;
end;

procedure TKMLLookAt.AltitudeRemove;
begin
  if FAltitudeExsit then
  begin
    FAltitudeExsit := False;
  end;
end;

procedure TKMLLookAt.SetHeading(const _Value: Double);
begin
  FHeadingExsit := True;
  FHeading := _Value;
end;

procedure TKMLLookAt.SetHeadingExsit(const Value: Boolean);
begin
  FHeadingExsit := Value;
end;

function TKMLLookAt.AddHeading: Double;
begin;
  Result := FHeading;
  FHeadingExsit := True;
end;

procedure TKMLLookAt.HeadingRemove;
begin
  if FHeadingExsit then
  begin
    FHeadingExsit := False;
  end;
end;

procedure TKMLLookAt.SetTilt(const _Value: Double);
begin
  FTiltExsit := True;
  FTilt := _Value;
end;

procedure TKMLLookAt.SetTiltExsit(const Value: Boolean);
begin
  FTiltExsit := Value;
end;

function TKMLLookAt.AddTilt: Double;
begin;
  Result := FTilt;
  FTiltExsit := True;
end;

procedure TKMLLookAt.TiltRemove;
begin
  if FTiltExsit then
  begin
    FTiltExsit := False;
  end;
end;

procedure TKMLLookAt.SetRange(const _Value: Double);
begin
  FRangeExsit := True;
  FRange := _Value;
end;

procedure TKMLLookAt.SetRangeExsit(const Value: Boolean);
begin
  FRangeExsit := Value;
end;

function TKMLLookAt.AddRange: Double;
begin;
  Result := FRange;
  FRangeExsit := True;
end;

procedure TKMLLookAt.RangeRemove;
begin
  if FRangeExsit then
  begin
    FRangeExsit := False;
  end;
end;

{  LatLonBox}
constructor TKMLLatLonBox.Create;
begin
end;

destructor TKMLLatLonBox.Destroy;
begin
  inherited;
end;

procedure TKMLLatLonBox.SetNorth(const _Value: Double);
begin
  FNorth := _Value;
end;

procedure TKMLLatLonBox.SetSouth(const _Value: Double);
begin
  FSouth := _Value;
end;

procedure TKMLLatLonBox.SetEast(const _Value: Double);
begin
  FEast := _Value;
end;

procedure TKMLLatLonBox.SetWest(const _Value: Double);
begin
  FWest := _Value;
end;

procedure TKMLLatLonBox.SetRotation(const _Value: Double);
begin
  FRotation := _Value;
end;

{  GroundOverlay}
constructor TKMLGroundOverlay.Create;
begin
end;

destructor TKMLGroundOverlay.Destroy;
begin
  if FLookAtExsit then
    FLookAt.Free;
  if FIconExsit then
    FIcon.Free;
  if FLatLonBoxExsit then
    FLatLonBox.Free;
  inherited;
end;

procedure TKMLGroundOverlay.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TKMLGroundOverlay.SetNameExsit(const Value: Boolean);
begin
  FNameExsit := Value;
end;

function TKMLGroundOverlay.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TKMLGroundOverlay.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TKMLGroundOverlay.SetVisibility(const _Value: Integer);
begin
  FVisibilityExsit := True;
  FVisibility := _Value;
end;

procedure TKMLGroundOverlay.SetVisibilityExsit(const Value: Boolean);
begin
  FVisibilityExsit := Value;
end;

function TKMLGroundOverlay.AddVisibility: Integer;
begin;
  Result := FVisibility;
  FVisibilityExsit := True;
end;

procedure TKMLGroundOverlay.VisibilityRemove;
begin
  if FVisibilityExsit then
  begin
    FVisibilityExsit := False;
  end;
end;

procedure TKMLGroundOverlay.SetDescription(const _Value: String);
begin
  FDescriptionExsit := True;
  FDescription := _Value;
end;

procedure TKMLGroundOverlay.SetDescriptionExsit(const Value: Boolean);
begin
  FDescriptionExsit := Value;
end;

function TKMLGroundOverlay.AddDescription: String;
begin;
  Result := FDescription;
  FDescriptionExsit := True;
end;

procedure TKMLGroundOverlay.DescriptionRemove;
begin
  if FDescriptionExsit then
  begin
    FDescriptionExsit := False;
  end;
end;

procedure TKMLGroundOverlay.SetLookAt(const _Value: TKMLLookAt);
begin
  if FLookAtExsit then
    FLookAt.Free;
  FLookAtExsit := True;
  FLookAt := _Value;
end;

procedure TKMLGroundOverlay.SetLookAtExsit(const Value: Boolean);
begin
  FLookAtExsit := Value;
end;

function TKMLGroundOverlay.AddLookAt: TKMLLookAt;
begin;
  if not FLookAtExsit then
    FLookAt := TKMLLookAt.Create;
  Result := FLookAt;
  FLookAtExsit := True;
end;

procedure TKMLGroundOverlay.LookAtRemove;
begin
  if FLookAtExsit then
  begin
    FLookAt.Free;
    FLookAtExsit := False;
  end;
end;

procedure TKMLGroundOverlay.SetIcon(const _Value: TKMLIcon);
begin
  if FIconExsit then
    FIcon.Free;
  FIconExsit := True;
  FIcon := _Value;
end;

procedure TKMLGroundOverlay.SetIconExsit(const Value: Boolean);
begin
  FIconExsit := Value;
end;

function TKMLGroundOverlay.AddIcon: TKMLIcon;
begin;
  if not FIconExsit then
    FIcon := TKMLIcon.Create;
  Result := FIcon;
  FIconExsit := True;
end;

procedure TKMLGroundOverlay.IconRemove;
begin
  if FIconExsit then
  begin
    FIcon.Free;
    FIconExsit := False;
  end;
end;

procedure TKMLGroundOverlay.SetLatLonBox(const _Value: TKMLLatLonBox);
begin
  if FLatLonBoxExsit then
    FLatLonBox.Free;
  FLatLonBoxExsit := True;
  FLatLonBox := _Value;
end;

procedure TKMLGroundOverlay.SetLatLonBoxExsit(const Value: Boolean);
begin
  FLatLonBoxExsit := Value;
end;

function TKMLGroundOverlay.AddLatLonBox: TKMLLatLonBox;
begin;
  if not FLatLonBoxExsit then
    FLatLonBox := TKMLLatLonBox.Create;
  Result := FLatLonBox;
  FLatLonBoxExsit := True;
end;

procedure TKMLGroundOverlay.LatLonBoxRemove;
begin
  if FLatLonBoxExsit then
  begin
    FLatLonBox.Free;
    FLatLonBoxExsit := False;
  end;
end;

{  ExtendedData}
constructor TKMLExtendedData.Create;
begin
  FDatas := TList<TKMLData>.Create;
end;

destructor TKMLExtendedData.Destroy;
begin
  DataClear;
  FDatas.Free;
  inherited;
end;

procedure TKMLExtendedData.SetDatas(const _Value: TList<TKMLData>);
begin
  DataClear;
  FDatas := _Value;
end;

function TKMLExtendedData.GetData(Index: Integer): TKMLData;
begin
  Result := FDatas[Index];
end;

procedure TKMLExtendedData.SetData(Index: Integer;
  const _Value: TKMLData);
begin
  FDatas[Index].Free;
  FDatas[Index] := _Value;
end;

procedure TKMLExtendedData.AddData(_Value: TKMLData);
begin;
  FDatas.Add(_Value);
end;

function TKMLExtendedData.AddNewData: TKMLData;
var
  Datatmp: TKMLData;
begin;
  Datatmp := TKMLData.Create;
  FDatas.Add(Datatmp);
  Result := Datatmp;
end;

procedure TKMLExtendedData.DataClear;
begin
  while FDatas.Count > 0 do
  begin
    FDatas.Items[0].Free;
    FDatas.Delete(0);
  end;
end;

function TKMLExtendedData.DataCount: Integer;
begin
  Result := FDatas.Count;
end;

procedure TKMLExtendedData.RemoveData(_Value: TKMLData);
begin
  FDatas.Remove(_Value);
  _Value.Free;
end;

procedure TKMLExtendedData.DeleteData(Index: Integer);
begin
  FDatas.Items[Index].Free;
  FDatas.Delete(Index);
end;

{  Data}
constructor TKMLData.Create;
begin
end;

destructor TKMLData.Destroy;
begin
  inherited;
end;

procedure TKMLData.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TKMLData.SetValue(const _Value: String);
begin
  FValue := _Value;
end;

{  Size}
constructor TKMLSize.Create;
begin
end;

destructor TKMLSize.Destroy;
begin
  inherited;
end;

procedure TKMLSize.SetX(const _Value: Double);
begin
  FX := _Value;
end;

procedure TKMLSize.SetY(const _Value: Double);
begin
  FY := _Value;
end;

procedure TKMLSize.SetXUnits(const _Value: String);
begin
  FXUnits := _Value;
end;

procedure TKMLSize.SetYUnits(const _Value: String);
begin
  FYUnits := _Value;
end;

{  Style}
constructor TKMLStyle.Create;
begin
end;

destructor TKMLStyle.Destroy;
begin
  if FIconStyleExsit then
    FIconStyle.Free;
  if FLineStyleExsit then
    FLineStyle.Free;
  if FPolyStyleExsit then
    FPolyStyle.Free;
  if FBalloonStyleExsit then
    FBalloonStyle.Free;
  inherited;
end;

procedure TKMLStyle.SetID(const _Value: String);
begin
  FID := _Value;
end;

procedure TKMLStyle.SetIconStyle(const _Value: TKMLIconStyle);
begin
  if FIconStyleExsit then
    FIconStyle.Free;
  FIconStyleExsit := True;
  FIconStyle := _Value;
end;

procedure TKMLStyle.SetIconStyleExsit(const Value: Boolean);
begin
  FIconStyleExsit := Value;
end;

function TKMLStyle.AddIconStyle: TKMLIconStyle;
begin;
  if not FIconStyleExsit then
    FIconStyle := TKMLIconStyle.Create;
  Result := FIconStyle;
  FIconStyleExsit := True;
end;

procedure TKMLStyle.IconStyleRemove;
begin
  if FIconStyleExsit then
  begin
    FIconStyle.Free;
    FIconStyleExsit := False;
  end;
end;

procedure TKMLStyle.SetLineStyle(const _Value: TKMLLineStyle);
begin
  if FLineStyleExsit then
    FLineStyle.Free;
  FLineStyleExsit := True;
  FLineStyle := _Value;
end;

procedure TKMLStyle.SetLineStyleExsit(const Value: Boolean);
begin
  FLineStyleExsit := Value;
end;

function TKMLStyle.AddLineStyle: TKMLLineStyle;
begin;
  if not FLineStyleExsit then
    FLineStyle := TKMLLineStyle.Create;
  Result := FLineStyle;
  FLineStyleExsit := True;
end;

procedure TKMLStyle.LineStyleRemove;
begin
  if FLineStyleExsit then
  begin
    FLineStyle.Free;
    FLineStyleExsit := False;
  end;
end;

procedure TKMLStyle.SetPolyStyle(const _Value: TKMLLineStyle);
begin
  if FPolyStyleExsit then
    FPolyStyle.Free;
  FPolyStyleExsit := True;
  FPolyStyle := _Value;
end;

procedure TKMLStyle.SetPolyStyleExsit(const Value: Boolean);
begin
  FPolyStyleExsit := Value;
end;

function TKMLStyle.AddPolyStyle: TKMLLineStyle;
begin;
  if not FPolyStyleExsit then
    FPolyStyle := TKMLLineStyle.Create;
  Result := FPolyStyle;
  FPolyStyleExsit := True;
end;

procedure TKMLStyle.PolyStyleRemove;
begin
  if FPolyStyleExsit then
  begin
    FPolyStyle.Free;
    FPolyStyleExsit := False;
  end;
end;

procedure TKMLStyle.SetBalloonStyle(const _Value: TKMLBalloonStyle);
begin
  if FBalloonStyleExsit then
    FBalloonStyle.Free;
  FBalloonStyleExsit := True;
  FBalloonStyle := _Value;
end;

procedure TKMLStyle.SetBalloonStyleExsit(const Value: Boolean);
begin
  FBalloonStyleExsit := Value;
end;

function TKMLStyle.AddBalloonStyle: TKMLBalloonStyle;
begin;
  if not FBalloonStyleExsit then
    FBalloonStyle := TKMLBalloonStyle.Create;
  Result := FBalloonStyle;
  FBalloonStyleExsit := True;
end;

procedure TKMLStyle.BalloonStyleRemove;
begin
  if FBalloonStyleExsit then
  begin
    FBalloonStyle.Free;
    FBalloonStyleExsit := False;
  end;
end;

{  IconStyle}
constructor TKMLIconStyle.Create;
begin
  FIcon := TKMLIcon.Create;
end;

destructor TKMLIconStyle.Destroy;
begin
  FIcon.Free;
  inherited;
end;

procedure TKMLIconStyle.SetIcon(const _Value: TKMLIcon);
begin
  FIcon.Free;
  FIcon := _Value;
end;

{  Icon}
constructor TKMLIcon.Create;
begin
end;

destructor TKMLIcon.Destroy;
begin
  inherited;
end;

procedure TKMLIcon.Sethref(const _Value: String);
begin
  Fhref := _Value;
end;

{  LineStyle}
constructor TKMLLineStyle.Create;
begin
end;

destructor TKMLLineStyle.Destroy;
begin
  inherited;
end;

procedure TKMLLineStyle.Setcolor(const _Value: String);
begin
  FcolorExsit := True;
  Fcolor := _Value;
end;

procedure TKMLLineStyle.SetcolorExsit(const Value: Boolean);
begin
  FcolorExsit := Value;
end;

function TKMLLineStyle.Addcolor: String;
begin;
  Result := Fcolor;
  FcolorExsit := True;
end;

procedure TKMLLineStyle.colorRemove;
begin
  if FcolorExsit then
  begin
    FcolorExsit := False;
  end;
end;

procedure TKMLLineStyle.Setwidth(const _Value: Double);
begin
  FwidthExsit := True;
  Fwidth := _Value;
end;

procedure TKMLLineStyle.SetwidthExsit(const Value: Boolean);
begin
  FwidthExsit := Value;
end;

function TKMLLineStyle.Addwidth: Double;
begin;
  Result := Fwidth;
  FwidthExsit := True;
end;

procedure TKMLLineStyle.widthRemove;
begin
  if FwidthExsit then
  begin
    FwidthExsit := False;
  end;
end;

{  BalloonStyle}
constructor TKMLBalloonStyle.Create;
begin
end;

destructor TKMLBalloonStyle.Destroy;
begin
  inherited;
end;

procedure TKMLBalloonStyle.Settext(const _Value: String);
begin
  Ftext := _Value;
end;

{  StyleMap}
constructor TKMLStyleMap.Create;
begin
  FPairs := TList<TKMLPair>.Create;
end;

destructor TKMLStyleMap.Destroy;
begin
  PairClear;
  FPairs.Free;
  inherited;
end;

procedure TKMLStyleMap.SetPairs(const _Value: TList<TKMLPair>);
begin
  PairClear;
  FPairs := _Value;
end;

function TKMLStyleMap.GetPair(Index: Integer): TKMLPair;
begin
  Result := FPairs[Index];
end;

procedure TKMLStyleMap.SetPair(Index: Integer;
  const _Value: TKMLPair);
begin
  FPairs[Index].Free;
  FPairs[Index] := _Value;
end;

procedure TKMLStyleMap.AddPair(_Value: TKMLPair);
begin;
  FPairs.Add(_Value);
end;

function TKMLStyleMap.AddNewPair: TKMLPair;
var
  Pairtmp: TKMLPair;
begin;
  Pairtmp := TKMLPair.Create;
  FPairs.Add(Pairtmp);
  Result := Pairtmp;
end;

procedure TKMLStyleMap.PairClear;
begin
  while FPairs.Count > 0 do
  begin
    FPairs.Items[0].Free;
    FPairs.Delete(0);
  end;
end;

function TKMLStyleMap.PairCount: Integer;
begin
  Result := FPairs.Count;
end;

procedure TKMLStyleMap.RemovePair(_Value: TKMLPair);
begin
  FPairs.Remove(_Value);
  _Value.Free;
end;

procedure TKMLStyleMap.DeletePair(Index: Integer);
begin
  FPairs.Items[Index].Free;
  FPairs.Delete(Index);
end;

procedure TKMLStyleMap.SetID(const _Value: String);
begin
  FID := _Value;
end;

{  ScreenOverlay}
constructor TKMLScreenOverlay.Create;
begin
end;

destructor TKMLScreenOverlay.Destroy;
begin
  if FIconExsit then
    FIcon.Free;
  if FOverlayXYExsit then
    FOverlayXY.Free;
  if FScreenXYExsit then
    FScreenXY.Free;
  if FRotationXYExsit then
    FRotationXY.Free;
  if FSizeExsit then
    FSize.Free;
  inherited;
end;

procedure TKMLScreenOverlay.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TKMLScreenOverlay.SetNameExsit(const Value: Boolean);
begin
  FNameExsit := Value;
end;

function TKMLScreenOverlay.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TKMLScreenOverlay.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TKMLScreenOverlay.SetVisibility(const _Value: Integer);
begin
  FVisibilityExsit := True;
  FVisibility := _Value;
end;

procedure TKMLScreenOverlay.SetVisibilityExsit(const Value: Boolean);
begin
  FVisibilityExsit := Value;
end;

function TKMLScreenOverlay.AddVisibility: Integer;
begin;
  Result := FVisibility;
  FVisibilityExsit := True;
end;

procedure TKMLScreenOverlay.VisibilityRemove;
begin
  if FVisibilityExsit then
  begin
    FVisibilityExsit := False;
  end;
end;

procedure TKMLScreenOverlay.SetDescription(const _Value: String);
begin
  FDescriptionExsit := True;
  FDescription := _Value;
end;

procedure TKMLScreenOverlay.SetDescriptionExsit(const Value: Boolean);
begin
  FDescriptionExsit := Value;
end;

function TKMLScreenOverlay.AddDescription: String;
begin;
  Result := FDescription;
  FDescriptionExsit := True;
end;

procedure TKMLScreenOverlay.DescriptionRemove;
begin
  if FDescriptionExsit then
  begin
    FDescriptionExsit := False;
  end;
end;

procedure TKMLScreenOverlay.SetIcon(const _Value: TKMLIcon);
begin
  if FIconExsit then
    FIcon.Free;
  FIconExsit := True;
  FIcon := _Value;
end;

procedure TKMLScreenOverlay.SetIconExsit(const Value: Boolean);
begin
  FIconExsit := Value;
end;

function TKMLScreenOverlay.AddIcon: TKMLIcon;
begin;
  if not FIconExsit then
    FIcon := TKMLIcon.Create;
  Result := FIcon;
  FIconExsit := True;
end;

procedure TKMLScreenOverlay.IconRemove;
begin
  if FIconExsit then
  begin
    FIcon.Free;
    FIconExsit := False;
  end;
end;

procedure TKMLScreenOverlay.SetOverlayXY(const _Value: TKMLSize);
begin
  if FOverlayXYExsit then
    FOverlayXY.Free;
  FOverlayXYExsit := True;
  FOverlayXY := _Value;
end;

procedure TKMLScreenOverlay.SetOverlayXYExsit(const Value: Boolean);
begin
  FOverlayXYExsit := Value;
end;

function TKMLScreenOverlay.AddOverlayXY: TKMLSize;
begin;
  if not FOverlayXYExsit then
    FOverlayXY := TKMLSize.Create;
  Result := FOverlayXY;
  FOverlayXYExsit := True;
end;

procedure TKMLScreenOverlay.OverlayXYRemove;
begin
  if FOverlayXYExsit then
  begin
    FOverlayXY.Free;
    FOverlayXYExsit := False;
  end;
end;

procedure TKMLScreenOverlay.SetScreenXY(const _Value: TKMLSize);
begin
  if FScreenXYExsit then
    FScreenXY.Free;
  FScreenXYExsit := True;
  FScreenXY := _Value;
end;

procedure TKMLScreenOverlay.SetScreenXYExsit(const Value: Boolean);
begin
  FScreenXYExsit := Value;
end;

function TKMLScreenOverlay.AddScreenXY: TKMLSize;
begin;
  if not FScreenXYExsit then
    FScreenXY := TKMLSize.Create;
  Result := FScreenXY;
  FScreenXYExsit := True;
end;

procedure TKMLScreenOverlay.ScreenXYRemove;
begin
  if FScreenXYExsit then
  begin
    FScreenXY.Free;
    FScreenXYExsit := False;
  end;
end;

procedure TKMLScreenOverlay.SetRotationXY(const _Value: TKMLSize);
begin
  if FRotationXYExsit then
    FRotationXY.Free;
  FRotationXYExsit := True;
  FRotationXY := _Value;
end;

procedure TKMLScreenOverlay.SetRotationXYExsit(const Value: Boolean);
begin
  FRotationXYExsit := Value;
end;

function TKMLScreenOverlay.AddRotationXY: TKMLSize;
begin;
  if not FRotationXYExsit then
    FRotationXY := TKMLSize.Create;
  Result := FRotationXY;
  FRotationXYExsit := True;
end;

procedure TKMLScreenOverlay.RotationXYRemove;
begin
  if FRotationXYExsit then
  begin
    FRotationXY.Free;
    FRotationXYExsit := False;
  end;
end;

procedure TKMLScreenOverlay.SetSize(const _Value: TKMLSize);
begin
  if FSizeExsit then
    FSize.Free;
  FSizeExsit := True;
  FSize := _Value;
end;

procedure TKMLScreenOverlay.SetSizeExsit(const Value: Boolean);
begin
  FSizeExsit := Value;
end;

function TKMLScreenOverlay.AddSize: TKMLSize;
begin;
  if not FSizeExsit then
    FSize := TKMLSize.Create;
  Result := FSize;
  FSizeExsit := True;
end;

procedure TKMLScreenOverlay.SizeRemove;
begin
  if FSizeExsit then
  begin
    FSize.Free;
    FSizeExsit := False;
  end;
end;

{  Pair}
constructor TKMLPair.Create;
begin
end;

destructor TKMLPair.Destroy;
begin
  inherited;
end;

procedure TKMLPair.SetKey(const _Value: String);
begin
  FKeyExsit := True;
  FKey := _Value;
end;

procedure TKMLPair.SetKeyExsit(const Value: Boolean);
begin
  FKeyExsit := Value;
end;

function TKMLPair.AddKey: String;
begin;
  Result := FKey;
  FKeyExsit := True;
end;

procedure TKMLPair.KeyRemove;
begin
  if FKeyExsit then
  begin
    FKeyExsit := False;
  end;
end;

procedure TKMLPair.SetStyleUrl(const _Value: String);
begin
  FStyleUrlExsit := True;
  FStyleUrl := _Value;
end;

procedure TKMLPair.SetStyleUrlExsit(const Value: Boolean);
begin
  FStyleUrlExsit := Value;
end;

function TKMLPair.AddStyleUrl: String;
begin;
  Result := FStyleUrl;
  FStyleUrlExsit := True;
end;

procedure TKMLPair.StyleUrlRemove;
begin
  if FStyleUrlExsit then
  begin
    FStyleUrlExsit := False;
  end;
end;



end.
