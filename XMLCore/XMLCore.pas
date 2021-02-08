unit XMLCore;

interface

uses
  SysUtils, Types, Classes, TeeTree, Menus, Generics.Defaults, Generics.Collections,
  Variants, XMLDoc, XMLIntf, XMLLeafTypes;

type

  XMLInspectorEvent = procedure(obj: TObject; Index: Integer; _Value: String) of object;

  TXML = class;
  TXMLTree = class;
  TXMLClass = class of TXML;
  TXMLTreeClass = class of TXMLTree;

  TXML = class abstract(TObject)
  private
  protected
    procedure FromXML(node: IXMLNode); virtual; abstract;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; virtual; abstract;
    function ToGeoObj: TObject; virtual; abstract;
    procedure FromGeoObj(const obj: TObject); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadXML(const fileName: string);
    procedure SaveXML(const fileName: string);
    function Xml: string;
    property GeoObj: TObject read ToGeoObj write FromGeoObj;

  end;

  TXMLTree = class abstract(TXML)
  private
    FTargetObject: TObject;
    FTreeNodeShape: TTreeNodeShape;
    FOnSetEvent: XMLInspectorEvent;
    FTargetNode: TTreeNodeShape;
    procedure SetTargetObject(const Value: TObject);
  protected
  public
    procedure SetXMLProperty(obj: TObject; Index: Integer; _Value: String); virtual; abstract;
    procedure ToTree(TreeNodeShape: TTreeNodeShape; obj: TObject = nil); virtual; abstract;
    procedure ToPopupMenu(Sender: TObject; obj: TObject = nil); virtual; abstract;
    procedure ToInspector(Sender: TObject; obj: TObject = nil); virtual; abstract;
    procedure ChildDeleteEvent(par, del: TObject); virtual; abstract;
    procedure PasteToEvent(items: TList<TObject>; const isCut: Boolean = False); virtual; abstract;
    property TargetObject: TObject read FTargetObject write SetTargetObject;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape write FTreeNodeShape;
    property TargetNode: TTreeNodeShape read FTargetNode write FTargetNode;
    property OnSetEvent: XMLInspectorEvent read FOnSetEvent write FOnSetEvent;
  end;

var
  XML_FilePath: string;

implementation

constructor TXML.Create;
begin
  inherited Create;
end;

destructor TXML.Destroy;
begin
  inherited;
end;

procedure TXML.LoadXML(const fileName: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
begin
  doc := LoadXMLDocument(fileName);
  node := doc.DocumentElement;
  FromXML(node);
end;

procedure TXML.SaveXML(const fileName: string);
var
  doc: IXMLDocument;
begin
  doc := NewXMLDocument();
  doc.Encoding := 'UTF-8';
  doc.NodeIndentStr := '  ';
  doc.Options := [doNodeAutoIndent];
  ToXML(doc.node);
  doc.SaveToFile(fileName);
  XML_FilePath := ExtractFilePath(fileName);
end;

function TXML.Xml: string;
var
  doc: IXMLDocument;
  node: IXMLNode;
begin
  doc := NewXMLDocument();
  node := doc.DocumentElement;
  ToXML(node);
  Result := node.Xml;
end;

procedure TXMLTree.SetTargetObject(const Value: TObject);
begin
  FTargetObject := Value;
end;

initialization

finalization

end.
