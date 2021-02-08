unit XML2CodeToPas;

interface

uses
  Windows, SysUtils, Types, Classes, Variants, Generics.Collections, XMLDoc, XMLIntf,
  Dialogs, Controls, ExtCtrls, StrUtils, TeeTree, Menus, XMLLeafTypes, XML2Code;

type
  TXML2CodePas = class helper for TXML2Code
  private
    { Common }
    function ClassOf(const dt: String): TXML2CodeClass;
    { Class Code }
    function BaseUse: string;
    function BaseTypes: string;
    function BaseStatements: string;
    function BaseImplements: string;
    { XML Code }
    function XMLImplements: string;
    function XMLStatements: string;
    function XMLTypes: string;
    function XMLUse: string;
    // XML Implements
    function XMLFromXMLImplement(treenull: string): string;
    function XMLToXMLImplement(treenull: string): string;
    function XMLFromGeoObjImplement(treenull: string): string;
    function XMLToGeoObjImplement(treenull: string): string;
    function XMLSetObjImplement(treenull: string): string;
    { XMLTree Code }
    function XMLTreeUse: string;
    function XMLTreeTypes: string;
    function XMLTreeStatements: string;
    function XMLTreeImplements: string;
    // XMLTree Implements
    function XMLTreeToTreeImplement: string;
    function XMLTreeSetXMLPropertyImplement: string;
    function XMLTreeToPopupMenuImplement: string;
    function XMLTreeToInspectorImplement: string;
    function XMLTreeChildDeleteEventImplement: string;
    function XMLTreePasteToEventImplement: string;
  public
    procedure ExportClassUnit(filePath: string);
    procedure ExportXMLUnit(filePath: string);
    procedure ExportXMLTreeUnit(filePath: string);
  end;

  TXML2CodeClassPas = class helper for TXML2CodeClass
  private
    { Class Code }
    function BaseConstructorImplements: string;
    function BaseConstructorStatements: string;
    function BaseDestructorImplements: string;
    function BaseDestructorStatements: string;
    function BaseImplements: string;
    function BasePrivateStatements: string;
    function BaseProtectedStatements: string;
    function BasePublicStatements: string;
    function BaseStatements: string;

    { XML Code }
    function FromXMLStatement: string;
    function FromXMLImplement(treenull: string): string;
    function ToXMLStatement: string;
    function ToXMLImplement(treenull: string): string;

    { XMLTree Code }
    function XMLTreeStatements: string;
    function XMLTreeImplements: string;
    function ChildDeleteEventStatement: string;
    function ChildDeleteEventImplement: string;
    function CopyXMLStatement: string;
    function CopyXMLImplement: string;
    function PasteToEventStatement: string;
    function PasteToEventImplement: string;
    function SetXMLPropertyStatement: string;
    function SetXMLPropertyImplement: string;
    function ToInspectorStatement: string;
    function ToInspectorImplement: string;
    function ToPopupMenuStatement: string;
    function ToPopupMenuImplement: string;
    function ToTreeStatement: string;
    function ToTreeImplement: string;
  end;

  TXML2CodeChildPas = class helper for TXML2CodeChild
  private
    { Common }
    function ConvertStr: string;
    function ConvertXML(_Value: string = 'nodeTmp.Text'): string;
    function ChildClass: TList<TXML2CodeClass>;
    function LeafType: string;
    function NodeType: TNodeType;
    { Class Code }
    // Child
    function ChildPrivateStatements: string;
    function ChildProtectedStatements: string;
    function ChildPublicStatements: string;
    function ChildPropertyStatement: string;
    function ChildFieldStatement: string;
    function ChildBaseImplements(par: TXML2CodeClass): string;
    function ChildAddStatement: string;
    function ChildAddImplement(par: TXML2CodeClass): string;
    function ChildAddNewStatement: string;
    function ChildAddNewImplement(par: TXML2CodeClass): string;
    function ChildClearStatement: string;
    function ChildClearImplement(par: TXML2CodeClass): string;
    function ChildCountStatement: string;
    function ChildCountImplement(par: TXML2CodeClass): string;
    function ChildDeleteStatement: string;
    function ChildDeleteImplement(par: TXML2CodeClass): string;
    function ChildRemoveStatement: string;
    function ChildRemoveImplement(par: TXML2CodeClass): string;
    function ChildExsitProperty: string;
    function ChildSetExsitStatement: string;
    function ChildSetExsitImplement(par: TXML2CodeClass): string;
    function ChildIndexPropertyStatement: string;
    function ChildGetIndexStatement: string;
    function ChildGetIndexImplement(par: TXML2CodeClass): string;
    function ChildSetIndexStatement: string;
    function ChildSetIndexImplement(par: TXML2CodeClass): string;
    function ChildSetStatement: string;
    function ChildSetImplement(par: TXML2CodeClass): string;
    // Leaf
    function LeafPrivateStatements: string;
    function LeafProtectedStatements: string;
    function LeafPublicStatements: string;
    function LeafFieldStatement: string;
    function LeafPropertyStatement: string;
    function LeafBaseImplements(par: TXML2CodeClass): string;
    function LeafAddStatement: string;
    function LeafAddImplement(par: TXML2CodeClass): string;
    function LeafAddNewStatement: string;
    function LeafAddNewImplement(par: TXML2CodeClass): string;
    function LeafClearStatement: string;
    function LeafClearImplement(par: TXML2CodeClass): string;
    function LeafDeleteStatement: string;
    function LeafDeleteImplement(par: TXML2CodeClass): string;
    function LeafRemoveStatement: string;
    function LeafRemoveImplement(par: TXML2CodeClass): string;
    function LeafCountStatement: string;
    function LeafCountImplement(par: TXML2CodeClass): string;
    function LeafExsitProperty: string;
    function LeafSetExsitStatement: string;
    function LeafSetExsitImplement(par: TXML2CodeClass): string;
    function LeafIndexPropertyStatement: string;
    function LeafGetIndexStatement: string;
    function LeafGetIndexImplement(par: TXML2CodeClass): string;
    function LeafSetIndexStatement: string;
    function LeafSetIndexImplement(par: TXML2CodeClass): string;
    function LeafSetStatement: string;
    function LeafSetImplement(par: TXML2CodeClass): string;

    function LeafAddEventStatement: string;
    function LeafAddEventImplement(par: TXML2CodeClass): string;
    function LeafAddEventMenuImplement(par: TXML2CodeClass): string;
    function LeafAddEventMenuStatement: string;
    function LeafSetXMLPropertyImplement(var Index: Integer): string;
    function LeafToInspectorImplement: string;
    function LeafToTree: string;
    function ReadAttribute: string;
    function ReadChild: string;
    function ReadText: string;
    function WriteAttribute: string;
    function WriteChild: string;
    function WriteText: string;
    function ChildAddEventMenuStatement: string;
    function ChildAddEventMenuImplement(par: TXML2CodeClass): string;
    function ChildAddEventStatement(par: TXML2CodeClass): string;
    function ChildAddEventImplement(par: TXML2CodeClass): string;
    function ChildDeleteEventStatement(par: TXML2CodeClass): string;
    function ChildDeleteEventImplement(par: TXML2CodeClass): string;
    function ChildToTree: string;
  end;

var
  x_2_c: TXML2Code;

implementation

function TXML2CodePas.ClassOf(const dt: String): TXML2CodeClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to XMLClassCount - 1 do
  begin
    if XMLClass[I].DataType = dt then
    begin
      Result := XMLClass[I];
      Exit;
    end;
  end;
end;

function TXML2CodePas.BaseUse: string;
var
  I, count: Integer;
begin
  Result := //
    'uses'#13#10 +
    '  Windows, SysUtils, Types, Classes, Variants, Generics.Collections, Dialogs,'#13#10 + // 0
    '  Controls, ExtCtrls, XMLLeafTypes';
  if Self.UsingExsit then
  begin
    count := Length(Self.Using);
    for I := 0 to count - 1 do
    begin
      Result := Result + ', ' + Self.Using[I];
    end;
  end;
  Result := Result + ';'#13#10#13#10;
end;

function TXML2CodePas.BaseTypes: string;
var
  I: Integer;
begin
  Result := 'type'#13#10#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + '  ' + XMLClass[I].DataType + ' = class;'#13#10;
  end;
end;

function TXML2CodePas.BaseStatements: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + XMLClass[I].BaseStatements;
  end;
end;

{ TXML2CodePas }
function TXML2CodePas.BaseImplements: string;
var
  I: Integer;
begin
  Result := 'implementation'#13#10#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + XMLClass[I].BaseImplements;
  end;
  Result := Result + #13#10;
end;

function TXML2CodePas.XMLImplements: string;
var
  I: Integer;
begin
  Result := 'implementation'#13#10#13#10;
  Result := Result + Self.XMLFromXMLImplement('');
  Result := Result + Self.XMLToXMLImplement('');
  Result := Result + Self.XMLFromGeoObjImplement('');
  Result := Result + Self.XMLToGeoObjImplement('');
  Result := Result + Self.XMLSetObjImplement('');
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + XMLClass[I].FromXMLImplement('');
    Result := Result + XMLClass[I].ToXMLImplement('');
  end;
end;

function TXML2CodePas.XMLStatements: string;
var
  I: Integer;
begin
  Result := //
    '  T' + Name + ' = class(TXMLTree)'#13#10 + // 0
    '  private'#13#10 + // 1
    '    F' + Target + 'Obj: ' + Target + ';'#13#10 + // 2
    '    procedure SetObj(const Value: ' + Target + ');'#13#10 + // 3
    '  protected'#13#10 + // 4
    '    procedure FromXML(node: IXMLNode); override;'#13#10 + // 5
    '    function ToXML(par: IXMLNode; pt: string = ''''): IXMLNode; override;'#13#10 + //
    '    function ToGeoObj: TObject; override;'#13#10 + // 17
    '    procedure FromGeoObj(const Obj: TObject); override;'#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + '    { ' + XMLClass[I].DataType + ' }'#13#10;
    Result := Result + XMLClass[I].FromXMLStatement;
    Result := Result + XMLClass[I].ToXMLStatement;
  end;
  Result := Result + //
    '  public'#13#10 + // 16
    '    property ' + Target + 'Obj: ' + Target + ' read F' + Target + 'Obj write SetObj;'#13#10 +
  // 19
    '  end;'#13#10;
end;

function TXML2CodePas.XMLTypes: string;
var
  I: Integer;
begin
  Result := 'type'#13#10#13#10 + '  T' + Name + ' = class;'#13#10;
end;

function TXML2CodePas.XMLUse: string;
var
  I, count: Integer;
begin
  Result := 'uses'#13#10 + //
    '  Windows, SysUtils, Types, Classes, Variants, Generics.Collections, Dialogs, Controls, ExtCtrls,'#13#10
    + '  XMLCore, XMLDoc, XMLIntf, XMLLeafTypes,' + Self.TargetUnit;
  if Self.UsingExsit then
  begin
    count := Length(Self.Using);
    for I := 0 to count - 1 do
    begin
      Result := Result + ', ' + Self.Using[I];
    end;
  end;
  Result := Result + ';'#13#10#13#10;
end;

function TXML2CodePas.XMLFromXMLImplement(treenull: string): string;
begin
  Result := //
    'procedure ' + DataType + treenull + '.FromXML(node: IXMLNode);'#13#10 + // 0
    'begin'#13#10 + // 1
    '  inherited;'#13#10 + // 2
    '  if not Assigned(F' + Target + 'Obj) then'#13#10 + // 3
    '    F' + Target + 'Obj := ' + Target + '.Create;'#13#10 + // 4
    '  ' + Target + '_FromXML(F' + Target + 'Obj, node);'#13#10 + // 5
    'end;'#13#10 + // 6
    ''#13#10 + // 7
    ''#13#10;
end;

function TXML2CodePas.XMLToXMLImplement(treenull: string): string;
begin
  Result := //
    'function ' + DataType + treenull + '.ToXML(par: IXMLNode; pt: string): IXMLNode;'#13#10 + // 0
    'begin'#13#10 + // 1
    '  if Assigned(F' + Target + 'Obj) then'#13#10 + // 2
    '    ' + Target + '_ToXML(F' + Target + 'Obj, par, pt);'#13#10 + // 3
    'end;'#13#10 + // 4
    ''#13#10 + // 5
    ''#13#10;
end;

function TXML2CodePas.XMLFromGeoObjImplement(treenull: string): string;
begin
  Result := //
    'procedure ' + DataType + treenull + '.FromGeoObj(const Obj: TObject);'#13#10 + // 0
    'begin'#13#10 + // 1
    '  inherited;'#13#10 + // 2
    '  F' + Target + 'Obj := ' + Target + '(Obj);'#13#10 + // 3
    'end;'#13#10 + // 4
    ''#13#10 + // 5
    ''#13#10;
end;

function TXML2CodePas.XMLToGeoObjImplement(treenull: string): string;
begin
  Result := //
    'function ' + DataType + treenull + '.ToGeoObj: TObject;'#13#10 + // 0
    'begin'#13#10 + // 1
    '  Result := F' + Target + 'Obj;'#13#10 + // 2
    'end;'#13#10 + // 3
    ''#13#10 + // 4
    ''#13#10;
end;

function TXML2CodePas.XMLSetObjImplement(treenull: string): string;
begin
  Result := //
    'procedure ' + DataType + treenull + '.SetObj(const Value: ' + Target + ');'#13#10 + // 0
    'begin'#13#10 + // 1
    '  F' + Target + 'Obj := Value;'#13#10 + // 2
    'end;'#13#10 + // 3
    ''#13#10 + // 4
    ''#13#10;
end;

function TXML2CodePas.XMLTreeUse: string;
var
  I, count: Integer;
begin
  Result := 'uses'#13#10 + //
    '  Windows, SysUtils, Types, Classes, Variants, Generics.Collections, Dialogs, Controls, ExtCtrls,'#13#10
    + '  XMLCore, XMLDoc, XMLIntf, XMLLeafTypes, TeeTree, Menus, XMLInspector,' + Self.TargetUnit;
  if Self.UsingExsit then
  begin
    count := Length(Self.Using);
    for I := 0 to count - 1 do
    begin
      Result := Result + ', ' + Self.Using[I];
    end;
  end;
  Result := Result + ';'#13#10#13#10;
end;

function TXML2CodePas.XMLTreeTypes: string;
var
  I: Integer;
begin
  Result := 'type'#13#10#13#10 + '  T' + Name + 'Tree = class;'#13#10;
end;

function TXML2CodePas.XMLTreeStatements: string;
var
  I: Integer;
begin
  Result := //
    '  T' + Name + 'Tree = class(TXMLTree)'#13#10 + // 0
    '  private'#13#10 + // 1
    '    F' + Target + 'Obj: ' + Target + ';'#13#10 + // 2
    '    procedure SetObj(const Value: ' + Target + ');'#13#10 + // 3
    '  protected'#13#10 + // 4
    '    { Inherited from TXML }'#13#10 + // 0
    '    procedure FromXML(node: IXMLNode); override;'#13#10 + // 5
    '    function ToXML(par: IXMLNode; pt: string = ''''): IXMLNode; override;'#13#10 + //
    '    function ToGeoObj: TObject; override;'#13#10 + // 17
    '    procedure FromGeoObj(const Obj: TObject); override;'#13#10;

  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + '    { ' + XMLClass[I].DataType + ' }'#13#10;
    Result := Result + XMLClass[I].FromXMLStatement;
    Result := Result + XMLClass[I].ToXMLStatement;
  end;
  Result := Result + //
    '  public'#13#10 + // 16
    '    { Inherited from TXMLTree }'#13#10 + // 0
    '    procedure ToTree(TreeNodeShape: TTreeNodeShape; obj: TObject = nil); override;'#13#10 +
    '    procedure SetXMLProperty(obj: TObject; Index: Integer; _Value: String); override;'#13#10 +
    '    procedure ToPopupMenu(Sender: TObject; obj: TObject = nil); override;'#13#10 + // 3
    '    procedure ToInspector(Sender: TObject; obj: TObject = nil); override;'#13#10 + // 4
    '    procedure ChildDeleteEvent(par, del: TObject); override;'#13#10 + // 5
    '    procedure PasteToEvent(items: TList<TObject>; const isCut: Boolean = False); override;'#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + XMLClass[I].XMLTreeStatements;
  end;
  Result := Result + // 18
    '    property ' + Target + 'Obj: ' + Target + ' read F' + Target + 'Obj write SetObj;'#13#10;
  Result := Result + '  end;'#13#10;
end;

function TXML2CodePas.XMLTreeImplements: string;
var
  I: Integer;
begin
  Result := 'implementation'#13#10#13#10;
  Result := Result + Self.XMLFromXMLImplement('Tree');
  Result := Result + Self.XMLToXMLImplement('Tree');
  Result := Result + Self.XMLFromGeoObjImplement('Tree');
  Result := Result + Self.XMLToGeoObjImplement('Tree');
  Result := Result + Self.XMLSetObjImplement('Tree');
  Result := Result + Self.XMLTreeToTreeImplement;
  Result := Result + Self.XMLTreeSetXMLPropertyImplement;
  Result := Result + Self.XMLTreeToPopupMenuImplement;
  Result := Result + Self.XMLTreeToInspectorImplement;
  Result := Result + Self.XMLTreeChildDeleteEventImplement;
  Result := Result + Self.XMLTreePasteToEventImplement;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + XMLClass[I].FromXMLImplement('Tree');
    Result := Result + XMLClass[I].ToXMLImplement('Tree');
  end;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + XMLClass[I].XMLTreeImplements;
  end;
end;

function TXML2CodePas.XMLTreeToTreeImplement: string;
var
  I: Integer;
begin
  Result := Result + //
    'procedure ' + DataType +
    'Tree.ToTree(TreeNodeShape: TTreeNodeShape; obj: TObject = nil);'#13#10 + // 0
    'var'#13#10 + // 1
    '  tar: TObject;'#13#10 + // 2
    'begin'#13#10 + // 3
    '  inherited;'#13#10 + // 4
    '  if obj <> nil then'#13#10 + // 5
    '  begin'#13#10 + // 6
    '    tar := obj;'#13#10 + // 7
    '  end'#13#10 + // 8
    '  else'#13#10 + // 9
    '  begin'#13#10 + // 10
    '    tar := TargetObject;'#13#10 + // 11
    '  end;'#13#10 + // 12
    '  if tar is ' + DataType + 'Tree then'#13#10 + // 13
    '  begin'#13#10 + // 14
    '    ' + Target + '_ToTree(' + DataType + 'Tree(tar).F' + Target +
    'Obj, TreeNodeShape);'#13#10 + // 15
    '  end'#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + //
      '  else if tar is ' + XMLClass[I].DataType + ' then'#13#10 + // 25
      '  begin'#13#10 + // 26
      '    ' + XMLClass[I].DataType + '_ToTree(' + XMLClass[I].DataType +
      '(tar), TreeNodeShape);'#13#10 + // 27
      '  end';
  end;
  Result := Result + //
    ';'#13#10 + // 28
    'end;'#13#10#13#10;
end;

function TXML2CodePas.XMLTreeSetXMLPropertyImplement: string;
var
  I: Integer;
begin
  Result := Result + //
    'procedure ' + DataType +
    'Tree.SetXMLProperty(obj: TObject; Index: Integer; _Value: String);'#13#10 + // 0
    'begin'#13#10 + // 1
    '  inherited;'#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + //
      '  if obj is ' + XMLClass[I].DataType + ' then'#13#10 + // 7
      '  begin'#13#10 + // 8
      '    ' + XMLClass[I].DataType + '_SetXMLProperty(' + XMLClass[I].DataType +
      '(obj), Index, _Value);'#13#10 + // 9
      '	  Exit;'#13#10 + // 0
      '  end;'#13#10;
  end;
  Result := Result + //
    'end;'#13#10#13#10;
end;

function TXML2CodePas.XMLTreeToPopupMenuImplement: string;
var
  I: Integer;
begin
  Result := Result + //
    'procedure ' + DataType + 'Tree.ToPopupMenu(Sender: TObject; obj: TObject = nil);'#13#10 + // 0
    'var'#13#10 + // 1
    '  tar: TObject;'#13#10 + // 2
    'begin'#13#10 + // 3
    '  inherited;'#13#10 + // 4
    '  if obj <> nil then'#13#10 + // 5
    '  begin'#13#10 + // 6
    '    tar := obj;'#13#10 + // 7
    '  end'#13#10 + // 8
    '  else'#13#10 + // 9
    '  begin'#13#10 + // 10
    '    tar := TargetObject;'#13#10 + // 11
    '  end;'#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + //
      '  if tar is ' + XMLClass[I].DataType + ' then'#13#10 + // 13
      '  begin'#13#10 + // 14
      '    ' + XMLClass[I].DataType + '_ToPopupMenu(' + XMLClass[I].DataType +
      '(tar), Sender);'#13#10 + // 15
      '	  Exit;'#13#10 + // 0
      '  end;'#13#10;
  end;
  Result := Result + //
    'end;'#13#10#13#10;
end;

function TXML2CodePas.XMLTreeToInspectorImplement: string;
var
  I: Integer;
begin
  Result := Result + //
    'procedure ' + DataType + 'Tree.ToInspector(Sender: TObject; obj: TObject = nil);'#13#10 + // 0
    'var'#13#10 + // 1
    '  tar: TObject;'#13#10 + // 2
    'begin'#13#10 + // 3
    '  inherited;'#13#10 + // 4
    '  if obj <> nil then'#13#10 + // 5
    '  begin'#13#10 + // 6
    '    tar := obj;'#13#10 + // 7
    '  end'#13#10 + // 8
    '  else'#13#10 + // 9
    '  begin'#13#10 + // 10
    '    tar := TargetObject;'#13#10 + // 11
    '  end;'#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + //
      '  if tar is ' + XMLClass[I].DataType + ' then'#13#10 + // 13
      '  begin'#13#10 + // 14
      '    ' + XMLClass[I].DataType + '_ToInspector(' + XMLClass[I].DataType +
      '(tar), Sender);'#13#10 + // 15
      '	  Exit;'#13#10 + // 0
      '  end;'#13#10;
  end;
  Result := Result + //
    'end;'#13#10#13#10;
end;

function TXML2CodePas.XMLTreeChildDeleteEventImplement: string;
var
  I: Integer;
begin
  Result := Result + //
    'procedure ' + DataType + 'Tree.ChildDeleteEvent(par, del: TObject);'#13#10 + // 0
    'begin'#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + //
      '  if TargetObject is ' + XMLClass[I].DataType + ' then'#13#10 + // 3
      '  begin'#13#10 + // 4
      '    ' + XMLClass[I].DataType + '_ChildDeleteEvent(' + XMLClass[I].DataType +
      '(par), del);'#13#10 + // 5
      '	  Exit;'#13#10 + // 0
      '  end;'#13#10;
  end;
  Result := Result + //
    'end;'#13#10#13#10;
end;

function TXML2CodePas.XMLTreePasteToEventImplement: string;
var
  I: Integer;
begin
  Result := Result + //
    'procedure ' + DataType +
    'Tree.PasteToEvent(items: TList<TObject>; const isCut: Boolean = False);'#13#10 + // 0
    'begin'#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + //
      '  if TargetObject is ' + XMLClass[I].DataType + ' then'#13#10 + // 3
      '  begin'#13#10 + // 4
      '    ' + XMLClass[I].DataType + '_PasteToEvent(' + XMLClass[I].DataType +
      '(TargetObject), items, isCut);'#13#10 + // 5
      '	  Exit;'#13#10 + // 0
      '  end;'#13#10;
  end;
  Result := Result + //
    'end;'#13#10#13#10;
end;

procedure TXML2CodePas.ExportClassUnit(filePath: string);
var
  strs: TStringList;
begin
  if Self.TargetUnit = '' then
    Self.TargetUnit := XMLClass[0].Name;
  if Self.Target = '' then
    Self.Target := XMLClass[0].DataType;
  if Self.DataType = '' then
    Self.DataType := 'T' + Name;
  strs := TStringList.Create;
  strs.Add('unit ' + Self.TargetUnit + ';'#13#10 + ''#13#10 + 'interface'#13#10);
  strs.Add(Self.BaseUse);
  strs.Add(Self.BaseTypes);
  strs.Add(Self.BaseStatements);
  strs.Add(Self.BaseImplements);
  strs.Add('end.');
  strs.SaveToFile(filePath + '\' + Self.TargetUnit + '.pas');
  strs.Free;
end;

procedure TXML2CodePas.ExportXMLUnit(filePath: string);
var
  strs: TStringList;
begin
  strs := TStringList.Create;
  strs.Add('unit ' + Name + ';'#13#10 + ''#13#10 + 'interface'#13#10);
  strs.Add(Self.XMLUse);
  strs.Add(Self.XMLTypes);
  strs.Add(Self.XMLStatements);
  strs.Add(Self.XMLImplements);
  strs.Add('end.');
  strs.SaveToFile(filePath + '\' + Name + '.pas');
  strs.Free;
end;

procedure TXML2CodePas.ExportXMLTreeUnit(filePath: string);
var
  strs: TStringList;
begin
  strs := TStringList.Create;
  strs.Add('unit ' + Name + 'Tree;'#13#10 + ''#13#10 + 'interface'#13#10);
  strs.Add(Self.XMLTreeUse);
  strs.Add(Self.XMLTreeTypes);
  strs.Add(Self.XMLTreeStatements);
  strs.Add(Self.XMLTreeImplements);
  strs.Add('end.');
  strs.SaveToFile(filePath + '\' + Name + 'Tree.pas');
  strs.Free;
end;

{ TXML2CodeClassPas }
function TXML2CodeClassPas.BaseConstructorImplements: string;
var
  I: Integer;
begin
  Result := 'constructor ' + DataType + '.Create;'#13#10;
  Result := Result + 'begin'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      case Child[I].Number of
        0:
          begin

          end;
        1:
          begin
          end;
        2:
          begin
            Result := Result + '  F' + Child[I].Name + 's := TList<' + Child[I]
              .LeafType + '>.Create;'#13#10;
          end;
      end;
    end
    else
    begin
      case Child[I].Number of
        0:
          begin

          end;
        1:
          begin
            Result := Result + '  F' + Child[I].Name + ' := ' + Child[I]
              .DataType + '.Create;'#13#10;
          end;
        2:
          begin
            Result := Result + '  F' + Child[I].Name + 's := TList<' + Child[I]
              .DataType + '>.Create;'#13#10;
          end;
      end;
    end;
  end;
  Result := Result + 'end;'#13#10#13#10;
end;

function TXML2CodeClassPas.BaseConstructorStatements: string;
begin
  Result := '    constructor Create;'#13#10;
end;

function TXML2CodeClassPas.BaseDestructorImplements: string;
var
  I: Integer;
begin
  Result := 'destructor ' + DataType + '.Destroy;'#13#10;
  Result := Result + 'begin'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      case Child[I].Number of
        0:
          ;
        1:
          ;
        2:
          begin
            Result := Result + '  F' + Child[I].Name + 's.Free;'#13#10;
          end;
      end;
    end
    else
    begin
      case Child[I].Number of
        0:
          begin
            Result := Result + '  if F' + Child[I].Name + 'Exsit then'#13#10 + '    F' + Child[I]
              .Name + '.Free;'#13#10;
          end;
        1:
          begin
            Result := Result + '  F' + Child[I].Name + '.Free;'#13#10;
          end;
        2:
          begin
            Result := Result + '  ' + Child[I].Name + 'Clear;'#13#10;
            Result := Result + '  F' + Child[I].Name + 's.Free;'#13#10;
          end;
      end;
    end;
  end;
  Result := Result + '  inherited;'#13#10;
  Result := Result + 'end;'#13#10#13#10;
end;

function TXML2CodeClassPas.BaseDestructorStatements: string;
begin
  Result := '    destructor Destroy; override;'#13#10;
end;

function TXML2CodeClassPas.BaseImplements: string;
var
  I: Integer;
begin
  Result := '{  ' + Name + '}'#13#10;
  Result := Result + Self.BaseConstructorImplements;
  Result := Result + Self.BaseDestructorImplements;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafBaseImplements(Self);
    end
    else
    begin
      Result := Result + Child[I].ChildBaseImplements(Self);
    end;
  end;
end;

function TXML2CodeClassPas.BasePrivateStatements: string;
var
  I: Integer;
begin
  Result := '  private'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafFieldStatement;
    end
    else
    begin
      Result := Result + Child[I].ChildFieldStatement;
    end;
  end;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafSetStatement;
      Result := Result + Child[I].LeafGetIndexStatement;
      Result := Result + Child[I].LeafSetIndexStatement;
      Result := Result + Child[I].LeafSetExsitStatement;
    end
    else
    begin
      Result := Result + Child[I].ChildSetStatement;
      Result := Result + Child[I].ChildGetIndexStatement;
      Result := Result + Child[I].ChildSetIndexStatement;
      Result := Result + Child[I].ChildSetExsitStatement;
    end;
  end;
end;

function TXML2CodeClassPas.BaseProtectedStatements: string;
begin
  Result := '  protected'#13#10;
end;

function TXML2CodeClassPas.BasePublicStatements: string;
var
  I: Integer;
begin
  Result := '  public'#13#10;
  Result := Result + Self.BaseConstructorStatements;
  Result := Result + Self.BaseDestructorStatements;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafPublicStatements;
    end
    else
    begin
      Result := Result + Child[I].ChildPublicStatements;
    end;
  end;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafPropertyStatement;
      Result := Result + Child[I].LeafExsitProperty;
    end
    else
    begin
      Result := Result + Child[I].ChildPropertyStatement;
      Result := Result + Child[I].ChildExsitProperty;
    end;
  end;
end;

function TXML2CodeClassPas.BaseStatements: string;
var
  absclass: string;
begin
  if Self.isAbstract then
  begin
    absclass := ' = class abstract';
  end
  else
  begin
    absclass := ' = class';
  end;
  if not Self.ParentClassExsit then
  begin
    Result := '  ' + DataType + absclass + '(TObject)'#13#10;
  end
  else
  begin
    if Self.ParentClass = '' then
    begin
      Result := '  ' + DataType + absclass + '(TObject)'#13#10;
    end
    else
    begin
      Result := '  ' + DataType + absclass + '(' + Self.ParentClass + ')'#13#10;
    end;
  end;
  Result := Result + Self.BasePrivateStatements;
  Result := Result + Self.BaseProtectedStatements;
  Result := Result + Self.BasePublicStatements;
  Result := Result + '  end;'#13#10#13#10;
end;

function TXML2CodeClassPas.FromXMLStatement: string;
begin
  Result := '    procedure ' + DataType + '_FromXML(Obj: ' + DataType + '; node: IXMLNode);'#13#10;
  if isAbstract then
    Result := '    procedure ' + DataType + '_FromXML(Obj: ' + DataType +
      '; node: IXMLNode); virtual;'#13#10;
end;

function TXML2CodeClassPas.FromXMLImplement(treenull: string): string;
var
  I, J: Integer;
  str: string;
begin
  Result := 'procedure ' + x_2_c.DataType + treenull + '.' + DataType + '_FromXML(Obj: ' +
    DataType + '; node: IXMLNode);'#13#10 + //
    'var'#13#10 + //
    '  I: Integer;'#13#10 + //
    '  nodeTmp: IXMLNode;'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if not Child[I].Leaf then
    begin
      case Child[I].Number of
        0:
          ;
        1:
          ;
        2:
          begin
            Result := Result + '  ' + Child[I].Name + 'Tmp: ' + Child[I].DataType + ';'#13#10;
          end;
      end;
    end;
  end;
  Result := Result + 'begin'#13#10 + '  try'#13#10;
  if Self.ParentClassExsit then
    if not(Self.ParentClass = '') then
      Result := Result + '    ' + Self.ParentClass + '_FromXML(Obj, node);'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Number = 2 then
    begin
      Result := Result + '    Obj.' + Child[I].Name + 'Clear;'#13#10;
    end
    else if Child[I].Number = 0 then
    begin
      Result := Result + '    Obj.' + Child[I].Name + 'Exsit := False;'#13#10;
    end;
  end;
  Result := Result + '    for I := 0 to node.ChildNodes.Count - 1 do'#13#10 + '    begin'#13#10 +
    '      nodeTmp := node.ChildNodes.Get(I);'#13#10;
  str := '';
  for I := 0 to ChildCount - 1 do
  begin
    str := str + Child[I].ReadChild;
  end;
  Delete(str, 7, 5);
  Delete(str, Length(str) - 1, 2);
  str := str + ';'#13#10;
  Result := Result + str;
  Result := Result + '    end;'#13#10 +
    '    for I := 0 to node.AttributeNodes.Count - 1 do'#13#10 + '    begin'#13#10 +
    '      nodeTmp := node.AttributeNodes.Get(I);'#13#10;
  str := '';
  for I := 0 to ChildCount - 1 do
  begin
    str := str + Child[I].ReadAttribute;
  end;
  Delete(str, 7, 5);
  Delete(str, Length(str) - 1, 2);
  str := str + ';'#13#10;
  Result := Result + str;
  for I := 0 to ChildCount - 1 do
  begin
    Result := Result + Child[I].ReadText;
  end;
  Result := Result + '    end;'#13#10 + '  except'#13#10 + '    raise Exception.Create(''' + Name +
    ' Read XML Error!'' + node.Xml);'#13#10 + '  end;'#13#10 + 'end;'#13#10#13#10;
end;

function TXML2CodeClassPas.ToXMLStatement: string;
begin
  Result := '    procedure ' + DataType + '_ToXML(Obj: ' + DataType +
    '; par: IXMLNode; pt: string = ''' + Path + ''');'#13#10;
  if isAbstract then
    Result := '    procedure ' + DataType + '_ToXML(Obj: ' + DataType +
      '; par: IXMLNode; pt: string = ''' + Path + '''); virtual;'#13#10;
end;

function TXML2CodeClassPas.ToXMLImplement(treenull: string): string;
var
  I, J: Integer;
begin
  Result := //
    'procedure ' + x_2_c.DataType + treenull + '.' + DataType + '_ToXML(Obj: ' + DataType +
    '; par: IXMLNode; pt: string);'#13#10 + //
    'var'#13#10 + //
    '  doc: IXMLDocument;'#13#10 + '  node: IXMLNode;'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + '  ' + Child[I].Name + 'Tmp: IXMLNode;'#13#10;
    end;
  end;
  Result := Result + '  I: Integer;'#13#10 + 'begin'#13#10 + '  try'#13#10 +
    '    doc := par.OwnerDocument;'#13#10;
  if Path = '' then
  begin
    Result := Result + '  if (pt = '''') or (pt[1] = ''#'') then'#13#10 + '    pt := ''' + Name +
      ''';'#13#10;
  end
  else
  begin
    Result := Result + '  if (pt = '''') or (pt[1] = ''#'') then'#13#10 + '    pt := ''' + Path +
      ''';'#13#10;
  end;
  Result := Result + '    node := doc.CreateNode(pt);'#13#10 +
    '    par.ChildNodes.Add(node);'#13#10;
  if Self.ParentClassExsit then
    if not(Self.ParentClass = '') then
      Result := Result + '    ' + Self.ParentClass + '_ToXML(Obj, node);'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    Result := Result + Child[I].WriteChild;
  end;
  for I := 0 to ChildCount - 1 do
  begin
    Result := Result + Child[I].WriteAttribute;
  end;
  for I := 0 to ChildCount - 1 do
  begin
    Result := Result + Child[I].WriteText;
  end;
  Result := Result + '  except'#13#10 +
    '    raise Exception.Create(''XML2Code Write XML Error!'');'#13#10 + '  end;'#13#10 +
    'end;'#13#10#13#10;
end;

function TXML2CodeClassPas.XMLTreeStatements: string;
var
  I: Integer;
begin
  Result := '    { ' + DataType + ' }'#13#10;
  Result := Result + Self.ToTreeStatement;
  Result := Result + Self.ToPopupMenuStatement;
  Result := Result + Self.ToInspectorStatement;
  Result := Result + Self.SetXMLPropertyStatement;
  Result := Result + Self.CopyXMLStatement;
  Result := Result + Self.PasteToEventStatement;
  Result := Result + Self.ChildDeleteEventStatement;
  for I := 0 to ChildCount - 1 do
  begin
    Result := Result + Child[I].ChildAddEventStatement(Self);
    Result := Result + Child[I].ChildDeleteEventStatement(Self);
  end;
end;

function TXML2CodeClassPas.XMLTreeImplements: string;
var
  I: Integer;
begin
  Result := '    { ' + DataType + ' }'#13#10;
  Result := Result + Self.ToTreeImplement;
  Result := Result + Self.ToPopupMenuImplement;
  Result := Result + Self.ToInspectorImplement;
  Result := Result + Self.SetXMLPropertyImplement;
  Result := Result + Self.CopyXMLImplement;
  Result := Result + Self.PasteToEventImplement;
  Result := Result + Self.ChildDeleteEventImplement;
  for I := 0 to ChildCount - 1 do
  begin
    Result := Result + Child[I].ChildAddEventImplement(Self);
    Result := Result + Child[I].ChildDeleteEventImplement(Self);
  end;
end;

function TXML2CodeClassPas.ChildDeleteEventStatement: string;
begin
  Result := '    procedure ' + DataType + '_ChildDeleteEvent(obj: ' + DataType +
    '; del: TObject);'#13#10;
end;

function TXML2CodeClassPas.ChildDeleteEventImplement: string;
var
  I: Integer;
begin
  Result := 'procedure ' + x_2_c.DataType + 'Tree.' + DataType + '_ChildDeleteEvent(obj: ' +
    DataType + '; del: TObject);'#13#10;
  Result := Result + 'begin'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if (Child[I].Leaf) or (Child[I].Number <> 2) then
    begin
      Continue;
    end;
    Result := Result + '  if del is ' + Child[I].DataType + ' then'#13#10 + // 0
      '  begin'#13#10 + // 1
      '    ' + DataType + '(obj).Remove' + Child[I].Name + '(' + Child[I]
      .DataType + '(del));'#13#10 + // 0
      '    ' + DataType + '_ToTree(' + DataType + '(obj), TargetNode);'#13#10 + // 1
      '  end;'#13#10;
  end;
  Result := Result + 'end;'#13#10#13#19;
end;

function TXML2CodeClassPas.CopyXMLStatement: string;
begin
  Result := '    procedure ' + DataType + '_Copy(source, target: ' + DataType + ');'#13#10;
end;

function TXML2CodeClassPas.CopyXMLImplement: string;
var
  I, J: Integer;
  childclasses: TList<TXML2CodeClass>;
  tmpSource: string;
begin
  Result := 'procedure ' + x_2_c.DataType + 'Tree.' + DataType + '_Copy(source, target: ' +
    DataType + ');'#13#10;
  tmpSource := 'source';
  Result := Result + //
    'var'#13#10 + //
    '  I:Integer;'#13#10;
  Result := Result + 'begin'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      case Child[I].Number of
        0:
          begin
            Result := Result + '  target.' + Child[I].Name + ' := ' + tmpSource + '.' + Child[I]
              .Name + ';'#13#10;
            Result := Result + '  target.' + Child[I].Name + 'Exsit := ' + tmpSource + '.' + Child
              [I].Name + 'Exsit;'#13#10;
          end;
        1:
          begin
            Result := Result + '  target.' + Child[I].Name + ' := ' + tmpSource + '.' + Child[I]
              .Name + ';'#13#10;
          end;
        2:
          begin
            Result := Result + '  target.' + Child[I].Name + 'Clear;'#13#10;
            Result := Result + //
              '  for I := 0 to ' + tmpSource + '.' + Child[I].Name + 'Count - 1 do'#13#10 + // 0
              '  begin'#13#10 + // 1
              '    Add' + Child[I].Name + ';'#13#10 + // 2
              '    target.' + Child[I].Name + '[I] := ' + tmpSource + '.' + Child[I]
              .Name + '[I];'#13#10 + //
              '  end;'#13#10;
          end;
      end;
    end
    else
    begin
      case Child[I].Number of
        0:
          begin
            Result := Result + //
              '  target.' + Child[I].Name + 'Exsit := ' + tmpSource + '.' + Child[I]
              .Name + 'Exsit;'#13#10;
            Result := Result + //
              '  if ' + tmpSource + '.' + Child[I].Name + 'Exsit then'#13#10 + // 0
              '  begin'#13#10;
            if Child[I].isVirtual then
            begin
              childclasses := Child[I].ChildClass;
              for J := 0 to childclasses.count - 1 do
              begin
                Result := Result + //
                  '    if ' + tmpSource + '.' + Child[I].Name + ' is ' + childclasses.Items[J]
                  .DataType + ' then'#13#10 + // 0
                  '      ' + Child[I].DataType + '_Copy(source.' + Child[I].Name + ', target.Add' +
                  childclasses.Items[J].Name + ');'#13#10;
              end;
              childclasses.Free;
            end
            else
            begin
              Result := Result + //
                '      ' + Child[I].DataType + '_Copy(source.' + Child[I]
                .Name + ', target.Add' + Child[I].Name + ');'#13#10;
            end;
            Result := Result + '  end;'#13#10;
          end;
        1:
          begin
            if Child[I].isVirtual then
            begin
              childclasses := Child[I].ChildClass;
              for J := 0 to childclasses.count - 1 do
              begin
                Result := Result + //
                  '    if ' + tmpSource + '.' + Child[I].Name + ' is ' + childclasses.Items[J]
                  .DataType + ' then'#13#10;
                Result := Result + //
                  '      ' + Child[I].DataType + '_Copy(source.' + Child[I]
                  .Name + ', target.' + Child[I].Name + ');'#13#10;
              end;
              childclasses.Free;
            end
            else
            begin
              Result := Result + //
                '      ' + Child[I].DataType + '_Copy(source.' + Child[I].Name + ', target.' + Child
                [I].Name + ');'#13#10;
            end;
          end;
        2:
          begin
            Result := Result + '  target.' + Child[I].Name + 'Clear;'#13#10;
            Result := Result + //
              '  for I := 0 to ' + tmpSource + '.' + Child[I].Name + 'Count - 1 do'#13#10 + // 0
              '  begin'#13#10;
            if Child[I].isVirtual then
            begin
              childclasses := Child[I].ChildClass;
              for J := 0 to childclasses.count - 1 do
              begin
                Result := Result + //
                  '    if ' + tmpSource + '.' + Child[I].Name + '[I] is ' + childclasses.Items[J]
                  .DataType + ' then'#13#10 + // 0
                  '      ' + Child[I].DataType + '_Copy(source.' + Child[I]
                  .Name + '[I], target.AddNew' + childclasses.Items[J].Name + ');'#13#10;
              end;
              childclasses.Free;
            end
            else
            begin
              Result := Result + //
                '      ' + Child[I].DataType + '_Copy(source.' + Child[I].Name +
                '[I], target. AddNew' + Child[I].Name + ');'#13#10;
            end;
            Result := Result + '  end;'#13#10;
          end;
      end;
    end;
  end;
  Result := Result + //
    '  ' + DataType + '_ToTree(target, TargetNode);'#13#10 + // 0
    'end;'#13#10#13#10;
end;

function TXML2CodeClassPas.PasteToEventStatement: string;
begin
  Result := '    procedure ' + DataType + '_PasteToEvent(obj: ' + DataType +
    ';items: TList<TObject>; const isCut: Boolean = False);'#13#10;
end;

function TXML2CodeClassPas.PasteToEventImplement: string;
var
  I, J: Integer;
  childclasses: TList<TXML2CodeClass>;
begin
  Result := 'procedure ' + x_2_c.DataType + 'Tree.' + DataType + '_PasteToEvent(obj: ' + DataType +
    '; items: TList<TObject>; const isCut: Boolean = False);'#13#10;
  Result := Result + //
    'var'#13#10 + // 0
    '  I: Integer;'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf or (Child[I].Number <> 2) then
    begin
      Continue;
    end;
    Result := Result + '  tmp' + Child[I].Name + ': ' + Child[I].DataType + ';'#13#10;
  end;
  Result := Result + 'begin'#13#10;
  Result := Result + //
    '  if (items.Count = 1) and (items[0] is ' + DataType + ') and (not isCut) then'#13#10 + // 0
    '  begin'#13#10 + // 1
    '    ' + DataType + '_Copy(' + DataType + '(items[0]), obj);'#13#10 + // 0
    '    ' + DataType + '_ToTree(obj, TargetNode);'#13#10 + // 1
    '    Exit;'#13#10 + // 4
    '  end;'#13#10;
  Result := Result + //
    '  for I := 0 to items.Count - 1 do'#13#10 + // 0
    '  begin'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf or (Child[I].Number <> 2) then
    begin
      Continue;
    end;
    Result := Result + //
      '    if items[I] is ' + Child[I].DataType + ' then'#13#10 + // 0
      '    begin'#13#10 + // 1
      '      tmp' + Child[I].Name + ' := ' + Child[I].DataType + '(items[I]);'#13#10 + // 2
      '      if isCut then'#13#10 + // 3
      '      begin'#13#10 + // 4
      '      end'#13#10 + // 9
      '      else'#13#10 + // 10
      '      begin'#13#10;
    if Child[I].isVirtual then
    begin
      childclasses := Child[I].ChildClass;
      for J := 0 to childclasses.count - 1 do
      begin
        Result := Result + //
          '        if tmp' + Child[I].Name + ' is ' + childclasses.Items[J]
          .DataType + ' then'#13#10 +
        // 0
          '          ' + childclasses.Items[J].DataType + '_Copy(' + childclasses.Items[J]
          .DataType + '(tmp' + Child[I].Name + '), obj.AddNew' + childclasses.Items[J]
          .Name + ');'#13#10;
      end;
      childclasses.Free;
    end
    else
    begin
      Result := Result + //
        '        ' + Child[I].DataType + '_Copy(tmp' + Child[I].Name + ', obj.AddNew' + Child[I]
        .Name + ');'#13#10;
    end;
    Result := Result + //
      '      end;'#13#10 + // 13
      '    end;'#13#10;
  end;
  Result := Result + //
    '  end;'#13#10 + // 0
    '  ' + DataType + '_ToTree(obj, TargetNode);'#13#10 + // 0
    'end;'#13#10#13#10;
end;

function TXML2CodeClassPas.SetXMLPropertyStatement: string;
begin
  Result := '    procedure ' + DataType + '_SetXMLProperty(obj: ' + DataType +
    '; Index: Integer; _Value: String);'#13#10;
end;

function TXML2CodeClassPas.SetXMLPropertyImplement: string;
var
  Index, I: Integer;
begin
  Result := Result + 'procedure ' + x_2_c.DataType + 'Tree.' + DataType + '_SetXMLProperty(obj: ' +
    DataType + '; Index: Integer; _Value: String);'#13#10 + 'begin'#13#10 + '  case index of'#13#10;
  Index := 0;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + TXML2CodeChild(Child[I]).LeafSetXMLPropertyImplement(Index);
    end;
  end;
  Result := Result + '  end;'#13#10;
  Result := Result + '  ' + DataType + '_ToTree(obj, TargetNode);'#13#10;
  Result := Result + 'end;'#13#10#13#10;
  if Index = 0 then
  begin
    Result := 'procedure ' + x_2_c.DataType + 'Tree.' + DataType + '_SetXMLProperty(obj: ' +
      DataType + '; Index: Integer; _Value: String);'#13#10 + 'begin'#13#10 + 'end;'#13#10#13#10;
  end;
end;

function TXML2CodeClassPas.ToInspectorStatement: string;
begin
  Result := '    procedure ' + DataType + '_ToInspector(obj: ' + DataType +
    '; ins: TObject);'#13#10;
end;

function TXML2CodeClassPas.ToInspectorImplement: string;
var
  I: Integer;
begin
  Result := //
    'procedure ' + x_2_c.DataType + 'Tree.' + DataType + '_ToInspector(obj: ' + DataType +
    '; ins: TObject);'#13#10 + //
    'var'#13#10 + //
    '  Names_Value: TStringList;'#13#10 + //
    '  Types_Value: TList<XMLTypes>;'#13#10 + //
    '  _Values_Value: TStringList;'#13#10 + //
    'begin'#13#10 + //
    '  if not Assigned(ins) then'#13#10 + // 0
    '    Exit;'#13#10 + // 1
    '  Names_Value := TStringList.Create;'#13#10 + //
    '  Types_Value := TList<XMLTypes>.Create;'#13#10 + //
    '  _Values_Value := TStringList.Create;'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafToInspectorImplement;
    end;
  end;
  Result := Result + //
    '  TXMLInspector(ins).SetData(Names_Value, _Values_Value, Types_Value, Obj, Self);'#13#10 + //
    'end;'#13#10#13#10;
end;

function TXML2CodeClassPas.ToPopupMenuStatement: string;
begin
  Result := '    procedure ' + DataType + '_ToPopupMenu(obj: ' + DataType +
    '; Sender: TObject);'#13#10;
end;

function TXML2CodeClassPas.ToPopupMenuImplement: string;
var
  I: Integer;
begin
  Result := //
    'procedure ' + x_2_c.DataType + 'Tree.' + DataType + '_ToPopupMenu(obj: ' + DataType +
    '; Sender: TObject);'#13#10 + //
    'var'#13#10;
  Result := Result + //
    '  pop: TPopupMenu;'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafAddEventMenuStatement;
    end
    else
    begin
      Result := Result + Child[I].ChildAddEventMenuStatement;
    end;
  end;
  Result := Result + //
    'begin'#13#10 + //
    '  if Assigned(Sender) and (Sender is TPopupMenu) then'#13#10 + //
    '  begin'#13#10 + //
    '    pop := TPopupMenu(Sender);'#13#10 + // 0
    '    pop.Items.Clear;'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafAddEventMenuImplement(Self);
    end
    else
    begin
      Result := Result + Child[I].ChildAddEventMenuImplement(Self);
    end;
  end;
  Result := Result + //
    '  end;'#13#10 + // 2
    'end;'#13#10#13#10;
end;

function TXML2CodeClassPas.ToTreeStatement: string;
begin
  Result := '    procedure ' + DataType + '_ToTree(Obj: ' + DataType +
    '; TreeNodeShape: TTreeNodeShape);'#13#10;
end;

function TXML2CodeClassPas.ToTreeImplement: string;
var
  I: Integer;
begin
  Result := 'procedure ' + x_2_c.DataType + 'Tree.' + DataType + '_ToTree(Obj: ' + DataType +
    '; TreeNodeShape: TTreeNodeShape);'#13#10 + // 0
    'var'#13#10 + // 1
    '  I: Integer;'#13#10 + // 2
    'begin'#13#10 + // 3
    '  if not Assigned(TreeNodeShape) then'#13#10 + // 4
    '  begin'#13#10 + // 5
    '    Exit;'#13#10 + // 6
    '  end;'#13#10 + // 7
    '  TreeNodeShape.Clear;'#13#10 + // 8
    '  TreeNodeShape.Data := Obj;'#13#10 + // 0
    '  TreeNodeShape.Text.Clear;'#13#10 + // 9
    '  TreeNodeShape.Text.Add(''' + Name + ''');'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafToTree;
    end
    else
    begin
      Result := Result + Child[I].ChildToTree;
    end;
  end;
  Result := Result + 'end;'#13#10#13#10;
end;

function TXML2CodeChildPas.ConvertStr: string;
begin
  Result := '';
  case Number of
    0:
      Result := XMLConvertStr(String2XMLType(DataType), 'Obj.' + Name);
    1:
      Result := XMLConvertStr(String2XMLType(DataType), 'Obj.' + Name);
    2:
      Result := XMLConvertStr(String2XMLType(DataType), 'Obj.' + Name + '[I]');
  end;
end;

function TXML2CodeChildPas.ConvertXML(_Value: string = 'nodeTmp.Text'): string;
begin
  Result := StrConvertXML(String2XMLType(DataType), _Value);
end;

function TXML2CodeChildPas.ChildClass: TList<TXML2CodeClass>;
var
  x2cclass, tmp: TXML2CodeClass;
  I: Integer;
begin
  x2cclass := x_2_c.ClassOf(DataType);
  if x2cclass = nil then
  begin
    Result := nil;
    Exit;
  end;
  if not x2cclass.ChildClassExsit then
  begin
    Result := nil;
    Exit;
  end;
  Result := TList<TXML2CodeClass>.Create;
  for I := 0 to Length(x2cclass.ChildClass) - 1 do
  begin
    tmp := x_2_c.ClassOf(x2cclass.ChildClass[I]);
    if tmp = nil then
      Continue;
    Result.Add(tmp);
  end;
  if Result.count = 0 then
  begin
    Result.Free;
    Result := nil;
  end;
end;

function TXML2CodeChildPas.LeafType: string;
begin
  Result := XMLType2PascalType(String2XMLType(DataType));
end;

function TXML2CodeChildPas.NodeType: TNodeType;
begin
  if Path = '' then
  begin
    Result := ntText;
  end
  else if Path[1] = '~' then
  begin
    Result := ntCData;
  end
  else if Path[1] = '@' then
  begin
    Result := ntAttribute;
  end
  else
  begin
    Result := ntElement;
  end;
end;

function TXML2CodeChildPas.ChildPrivateStatements: string;
begin
  Result := '';
  Result := Result + Self.ChildFieldStatement;
  Result := Result + Self.ChildSetStatement;
  Result := Result + Self.ChildGetIndexStatement;
  Result := Result + Self.ChildSetIndexStatement;
end;

function TXML2CodeChildPas.ChildProtectedStatements: string;
begin
  Result := '';
end;

function TXML2CodeChildPas.ChildPublicStatements: string;
begin
  Result := '';
  Result := Result + Self.ChildAddStatement;
  Result := Result + Self.ChildAddNewStatement;
  Result := Result + Self.ChildClearStatement;
  Result := Result + Self.ChildCountStatement;
  Result := Result + Self.ChildRemoveStatement;
  Result := Result + Self.ChildDeleteStatement;
end;

function TXML2CodeChildPas.ChildPropertyStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    property ' + Name + ': ' + DataType + ' read F' + Name + ' write Set' +
          Name + ';'#13#10;
      end;
    1:
      begin
        Result := '    property ' + Name + ': ' + DataType + ' read F' + Name + ' write Set' +
          Name + ';'#13#10;
      end;
    2:
      begin
        Result := '    property ' + Name + 's: TList<' + DataType + '> read F' + Name +
          's write Set' + Name + 's;'#13#10;
        Result := Result + Self.ChildIndexPropertyStatement;
      end;
  end;
end;

function TXML2CodeChildPas.ChildFieldStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    F' + Name + ': ' + DataType + ';'#13#10 + '    F' + Name +
          'Exsit: Boolean;'#13#10;
      end;
    1:
      begin
        Result := '    F' + Name + ': ' + DataType + ';'#13#10;
      end;
    2:
      begin
        Result := '    F' + Name + 's: TList<' + DataType + '>;'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildBaseImplements(par: TXML2CodeClass): string;
begin
  Result := '';
  Result := Result + ChildSetImplement(par);
  Result := Result + ChildGetIndexImplement(par);
  Result := Result + ChildSetIndexImplement(par);
  Result := Result + ChildSetExsitImplement(par);
  Result := Result + ChildAddImplement(par);
  Result := Result + ChildAddNewImplement(par);
  Result := Result + ChildClearImplement(par);
  Result := Result + ChildCountImplement(par);
  Result := Result + ChildRemoveImplement(par);
  Result := Result + ChildDeleteImplement(par);
end;

function TXML2CodeChildPas.ChildAddStatement: string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case Number of
    0:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to ChildClass.count - 1 do
          begin
            Result := Result + '    function Add' + childclasses[I].Name + ': ' + childclasses[I]
              .DataType + ';'#13#10;
          end;
        end
        else
        begin
          Result := '    function Add' + Name + ': ' + DataType + ';'#13#10;
        end;
      end;
    1:
      ;
    2:
      begin
        Result := '    procedure Add' + Name + '(_Value: ' + DataType + ');'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildAddImplement(par: TXML2CodeClass): string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case Number of
    0:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to ChildClass.count - 1 do
          begin
            Result := Result + 'function ' + par.DataType + '.Add' + childclasses[I]
              .Name + ': ' + childclasses[I].DataType + ';'#13#10 + 'begin;'#13#10 +
              '  if not F' + Name + 'Exsit then'#13#10 + '  F' + Name + '.Free;'#13#10 + '  F' +
              Name + ' := ' + childclasses[I].DataType + '.Create;'#13#10 + '  Result := ' +
              childclasses[I].DataType + '(F' + Name + ');'#13#10 + '  F' + Name +
              'Exsit := True;'#13#10 + 'end;'#13#10#13#10;
          end;
        end
        else
        begin
          Result := 'function ' + par.DataType + '.Add' + Name + ': ' + DataType + ';'#13#10 + //
            'begin;'#13#10 + //
            '  if not F' + Name + 'Exsit then'#13#10 + //
            '    F' + Name + ' := ' + DataType + '.Create;'#13#10 + //
            '  Result := F' + Name + ';'#13#10 + //
            '  F' + Name + 'Exsit := True;'#13#10 + //
            'end;'#13#10#13#10;
        end;
      end;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + par.DataType + '.Add' + Name + '(_Value: ' + DataType +
          ');'#13#10 + //
          'begin;'#13#10 + //
          '  F' + Name + 's.Add(_Value);'#13#10 + // 0
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildAddNewStatement: string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to ChildClass.count - 1 do
          begin
            Result := Result + '    function AddNew' + childclasses[I].Name + ': ' + childclasses[I]
              .DataType + ';'#13#10;
          end;
        end
        else
        begin
          Result := '    function AddNew' + Name + ': ' + DataType + ';'#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildPas.ChildAddNewImplement(par: TXML2CodeClass): string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to ChildClass.count - 1 do
          begin
            Result := Result + 'function ' + par.DataType + '.AddNew' + childclasses[I]
              .Name + ': ' + childclasses[I].DataType + ';'#13#10 + 'var'#13#10 + '  ' +
              childclasses[I].Name + 'tmp: ' + childclasses[I].DataType + ';'#13#10 +
              'begin;'#13#10 + '  ' + childclasses[I].Name + 'tmp := ' + childclasses
              [I].DataType + '.Create;'#13#10 + '  F' + Name + 's.Add(' + childclasses[I]
              .Name + 'tmp);'#13#10 + '  Result := ' + childclasses[I].Name + 'tmp;'#13#10 +
              'end;'#13#10#13#10;
          end;
        end
        else
        begin
          Result := 'function ' + par.DataType + '.AddNew' + Name + ': ' + DataType + ';'#13#10 +
            'var'#13#10 + '  ' + Name + 'tmp: ' + DataType + ';'#13#10 + 'begin;'#13#10 + '  ' +
            Name + 'tmp := ' + DataType + '.Create;'#13#10 + '  F' + Name + 's.Add(' + Name +
            'tmp);'#13#10 + '  Result := ' + Name + 'tmp;'#13#10 + 'end;'#13#10#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildPas.ChildClearStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    procedure ' + Name + 'Clear;'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildClearImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + par.DataType + '.' + Name + 'Clear;'#13#10 + 'begin'#13#10 +
          '  while F' + Name + 's.Count > 0 do'#13#10 + '  begin'#13#10 + '    F' + Name +
          's.Items[0].Free;'#13#10 + '    F' + Name + 's.Delete(0);'#13#10 + '  end;'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildCountStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    function ' + Name + 'Count: Integer;'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildCountImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'function ' + par.DataType + '.' + Name + 'Count: Integer;'#13#10 +
          'begin'#13#10 + '  Result := F' + Name + 's.Count;'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildDeleteStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    procedure Delete' + Name + '(Index: Integer);'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildDeleteImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + par.DataType + '.Delete' + Name + '(Index: Integer);'#13#10 +
          'begin'#13#10 + '  F' + Name + 's.Items[Index].Free;'#13#10 + '  F' + Name +
          's.Delete(Index);'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildRemoveStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    procedure ' + Name + 'Remove;'#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := '    procedure Remove' + Name + '(_Value: ' + DataType + ');'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildRemoveImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'procedure ' + par.DataType + '.' + Name + 'Remove;'#13#10 + 'begin'#13#10 +
          '  if F' + Name + 'Exsit then'#13#10 + '  begin'#13#10 + '    F' + Name +
          '.Free;'#13#10 + '    F' + Name + 'Exsit := False;'#13#10 + '  end;'#13#10 +
          'end;'#13#10#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + par.DataType + '.Remove' + Name + '(_Value: ' + DataType +
          ');'#13#10 + 'begin'#13#10 + '  F' + Name + 's.Remove(_Value);'#13#10 +
          '  _Value.Free;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildExsitProperty: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    property ' + Name + 'Exsit: Boolean read F' + Name + 'Exsit write Set' +
          Name + 'Exsit;'#13#10;
      end;
    1:
      ;
    2:
      ;
  end;
end;

function TXML2CodeChildPas.ChildSetExsitStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    procedure Set' + Name + 'Exsit(const Value: Boolean);'#13#10;
      end;
    1:
      ;
    2:
      ;
  end;
end;

function TXML2CodeChildPas.ChildSetExsitImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := //
          'procedure ' + par.DataType + '.Set' + Name + 'Exsit(const Value: Boolean);'#13#10 + // 0
          'begin'#13#10 + // 1
          '  F' + Name + 'Exsit := Value;'#13#10 + // 2
          'end;'#13#10#13#10;
      end;
    1:
      ;
    2:
      ;
  end;
end;

function TXML2CodeChildPas.ChildIndexPropertyStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    property ' + Name + '[Index: Integer]: ' + DataType + ' read Get' + Name +
          ' write Set' + Name + ';'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildGetIndexStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    function Get' + Name + '(Index: Integer): ' + DataType + ';'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildGetIndexImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'function ' + par.DataType + '.Get' + Name + '(Index: Integer): ' + DataType +
          ';'#13#10 + 'begin'#13#10 + '  Result := F' + Name + 's[Index];'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildSetIndexStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    procedure Set' + Name + '(Index: Integer; const _Value: ' + DataType +
          ');'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildSetIndexImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + par.DataType + '.Set' + Name + '(Index: Integer;'#13#10 +
          '  const _Value: ' + DataType + ');'#13#10 + 'begin'#13#10 + '  F' + Name +
          's[Index].Free;'#13#10 + '  F' + Name + 's[Index] := _Value;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildSetStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    procedure Set' + Name + '(const _Value: ' + DataType + ');'#13#10;
      end;
    1:
      begin
        Result := '    procedure Set' + Name + '(const _Value: ' + DataType + ');'#13#10;
      end;
    2:
      begin
        Result := '    procedure Set' + Name + 's(const _Value: TList<' + DataType + '>);'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ChildSetImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'procedure ' + par.DataType + '.Set' + Name + '(const _Value: ' + DataType +
          ');'#13#10 + 'begin'#13#10 + '  if F' + Name + 'Exsit then'#13#10 + '    F' + Name +
          '.Free;'#13#10 + '  F' + Name + 'Exsit := True;'#13#10 + '  F' + Name +
          ' := _Value;'#13#10 + 'end;'#13#10#13#10;
      end;
    1:
      begin
        Result := 'procedure ' + par.DataType + '.Set' + Name + '(const _Value: ' + DataType +
          ');'#13#10 + 'begin'#13#10 + '  F' + Name + '.Free;'#13#10 + '  F' + Name +
          ' := _Value;'#13#10 + 'end;'#13#10#13#10;
      end;
    2:
      begin
        Result := 'procedure ' + par.DataType + '.Set' + Name + 's(const _Value: TList<' +
          DataType + '>);'#13#10 + 'begin'#13#10 + '  ' + Name + 'Clear;'#13#10 + '  F' +
          Name + 's := _Value;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafPrivateStatements: string;
begin
  Result := '';
  Result := Result + Self.LeafFieldStatement;
  Result := Result + Self.LeafSetStatement;
  Result := Result + Self.LeafGetIndexStatement;
  Result := Result + Self.LeafSetIndexStatement;
end;

function TXML2CodeChildPas.LeafProtectedStatements: string;
begin
  Result := '';
end;

function TXML2CodeChildPas.LeafPublicStatements: string;
begin
  Result := '';
  Result := Result + Self.LeafAddStatement;
  Result := Result + Self.LeafAddNewStatement;
  Result := Result + Self.LeafClearStatement;
  Result := Result + Self.LeafCountStatement;
  Result := Result + Self.LeafRemoveStatement;
  Result := Result + Self.LeafDeleteStatement;
end;

function TXML2CodeChildPas.LeafFieldStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    F' + Name + ': ' + LeafType + ';'#13#10 + '    F' + Name +
          'Exsit: Boolean;'#13#10;
      end;
    1:
      begin
        Result := '    F' + Name + ': ' + LeafType + ';'#13#10;
      end;
    2:
      begin
        Result := '    F' + Name + 's: TList<' + LeafType + '>;'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafPropertyStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    property ' + Name + ': ' + LeafType + ' read F' + Name + ' write Set' +
          Name + ';'#13#10;
      end;
    1:
      begin
        Result := '    property ' + Name + ': ' + LeafType + ' read F' + Name + ' write Set' +
          Name + ';'#13#10;
      end;
    2:
      begin
        Result := '    property ' + Name + 's: TList<' + LeafType + '> read F' + Name +
          's write Set' + Name + 's;'#13#10;
        Result := Result + Self.LeafIndexPropertyStatement;
      end;
  end;
end;

function TXML2CodeChildPas.LeafBaseImplements(par: TXML2CodeClass): string;
begin
  Result := '';
  Result := Result + LeafSetImplement(par);
  Result := Result + LeafGetIndexImplement(par);
  Result := Result + LeafSetIndexImplement(par);
  Result := Result + LeafSetExsitImplement(par);
  Result := Result + LeafAddImplement(par);
  Result := Result + LeafAddNewImplement(par);
  Result := Result + LeafClearImplement(par);
  Result := Result + LeafCountImplement(par);
  Result := Result + LeafRemoveImplement(par);
  Result := Result + LeafDeleteImplement(par);
end;

function TXML2CodeChildPas.LeafAddStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    function Add' + Name + ': ' + LeafType + ';'#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := '    procedure Add' + Name + '(_Value: ' + LeafType + ');'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafAddImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'function ' + par.DataType + '.Add' + Name + ': ' + LeafType + ';'#13#10 +
          'begin;'#13#10 + '  Result := F' + Name + ';'#13#10 + '  F' + Name +
          'Exsit := True;'#13#10 + 'end;'#13#10#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + par.DataType + '.Add' + Name + '(_Value: ' + LeafType +
          '£©;'#13#10 + //
          'begin;'#13#10 + //
          '  F' + Name + 's.Add(_Value);'#13#10 + //
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafAddNewStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    function AddNew' + Name + ': ' + LeafType + ';'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafAddNewImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'function ' + par.DataType + '.AddNew' + Name + ': ' + LeafType + ';'#13#10 +
          'var'#13#10 + Name + 'tmp: ' + LeafType + ';'#13#10 + 'begin;'#13#10 + '  F' + Name +
          's.Add(' + Name + 'tmp);'#13#10 + '  Result := ' + Name + 'tmp;'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafClearStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    procedure ' + Name + 'Clear;'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafClearImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + par.DataType + '.' + Name + 'Clear;'#13#10 + 'begin'#13#10 +
          '  F' + Name + 's.Clear;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafDeleteStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    procedure Delete' + Name + '(Index: Integer);'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafDeleteImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + par.DataType + '.Delete' + Name + '(Index: Integer);'#13#10 +
          'begin'#13#10 + '  F' + Name + 's.Delete(Index);'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafRemoveStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    procedure ' + Name + 'Remove;'#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := '    procedure Remove' + Name + '(_Value: ' + LeafType + ');'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafRemoveImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'procedure ' + par.DataType + '.' + Name + 'Remove;'#13#10 + 'begin'#13#10 +
          '  if F' + Name + 'Exsit then'#13#10 + '  begin'#13#10 + '    F' + Name +
          'Exsit := False;'#13#10 + '  end;'#13#10 + 'end;'#13#10#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + par.DataType + '.Remove' + Name + '(_Value: ' + LeafType +
          ');'#13#10 + 'begin'#13#10 + '  F' + Name + 's.Remove(_Value);'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafCountStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    function ' + Name + 'Count: Integer;'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafCountImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'function ' + par.DataType + '.' + Name + 'Count: Integer;'#13#10 +
          'begin'#13#10 + '  Result := F' + Name + 's.Count;'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafExsitProperty: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    property ' + Name + 'Exsit: Boolean read F' + Name + 'Exsit Write Set' +
          Name + 'Exsit;'#13#10;
      end;
    1:
      ;
    2:
      ;
  end;
end;

function TXML2CodeChildPas.LeafSetExsitStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    procedure Set' + Name + 'Exsit(const Value: Boolean);'#13#10;
      end;
    1:
      ;
    2:
      ;
  end;
end;

function TXML2CodeChildPas.LeafSetExsitImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := //
          'procedure ' + par.DataType + '.Set' + Name + 'Exsit(const Value: Boolean);'#13#10 + // 0
          'begin'#13#10 + // 1
          '  F' + Name + 'Exsit := Value;'#13#10 + // 2
          'end;'#13#10#13#10;
      end;
    1:
      ;
    2:
      ;
  end;
end;

function TXML2CodeChildPas.LeafIndexPropertyStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    property ' + Name + '[Index: Integer]: ' + LeafType + ' read Get' + Name +
          ' write Set' + Name + ';'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafGetIndexStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    function Get' + Name + '(Index: Integer): ' + LeafType + ';'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafGetIndexImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'function ' + par.DataType + '.Get' + Name + '(Index: Integer): ' + LeafType +
          ';'#13#10 + 'begin'#13#10 + '  Result := F' + Name + 's[Index];'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafSetIndexStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    procedure Set' + Name + '(Index: Integer; const _Value: ' + LeafType +
          ');'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafSetIndexImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + par.DataType + '.Set' + Name + '(Index: Integer;'#13#10 +
          '  const _Value: ' + LeafType + ');'#13#10 + 'begin'#13#10 + '  F' + Name +
          's[Index] := _Value;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafSetStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    procedure Set' + Name + '(const _Value: ' + LeafType + ');'#13#10;
      end;
    1:
      begin
        Result := '    procedure Set' + Name + '(const _Value: ' + LeafType + ');'#13#10;
      end;
    2:
      begin
        Result := '    procedure Set' + Name + 's(const _Value: TList<' + LeafType + '>);'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafSetImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'procedure ' + par.DataType + '.Set' + Name + '(const _Value: ' + LeafType +
          ');'#13#10 + 'begin'#13#10 + '  F' + Name + 'Exsit := True;'#13#10 + '  F' + Name +
          ' := _Value;'#13#10 + 'end;'#13#10#13#10;
      end;
    1:
      begin
        Result := 'procedure ' + par.DataType + '.Set' + Name + '(const _Value: ' + LeafType +
          ');'#13#10 + 'begin'#13#10 + '  F' + Name + ' := _Value;'#13#10 + 'end;'#13#10#13#10;
      end;
    2:
      begin
        Result := 'procedure ' + par.DataType + '.Set' + Name + 's(const _Value: TList<' +
          LeafType + '>);'#13#10 + 'begin'#13#10 + '  F' + Name + 's.Clear;'#13#10 +
          '  F' + Name + 's := _Value;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafAddEventStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    procedure Add' + Name + 'Event(Sender: TObject);'#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := '    procedure Add' + Name + 'Event(Sender: TObject);'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafAddEventImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'procedure ' + par.DataType + '.Add' + Name + 'Event(Sender: TObject);'#13#10 +
          'begin'#13#10 + '  Add' + Name + ';'#13#10 + 'end;'#13#10#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + par.DataType + '.Add' + Name + 'Event(Sender: TObject);'#13#10 +
          'begin'#13#10 + '  Add' + Name + ';'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafAddEventMenuImplement(par: TXML2CodeClass): string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := //
          '    ' + Name + 'AddMenu := TMenuItem.Create(pop);'#13#10 + //
          '    ' + Name + 'AddMenu.Caption := ''Add ' + Name + ''';'#13#10 + //
          '    ' + Name + 'AddMenu.OnClick := ' + par.DataType + '_Add' + Name + 'Event;'#13#10 +
        //
          '    pop.Items.Add(' + Name + 'AddMenu);'#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := //
          '    ' + Name + 'AddMenu := TMenuItem.Create(pop);'#13#10 + //
          '    ' + Name + 'AddMenu.Caption := ''Add ' + Name + ''';'#13#10 + //
          '    ' + Name + 'AddMenu.OnClick := ' + par.DataType + '_Add' + Name + 'Event;'#13#10 +
        //
          '    pop.Items.Add(' + Name + 'AddMenu);'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafAddEventMenuStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '  ' + Name + 'AddMenu: TMenuItem;'#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := '  ' + Name + 'AddMenu: TMenuItem;'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.LeafSetXMLPropertyImplement(var Index: Integer): string;
begin
  Result := '';
  if Visual then
  begin
    case Number of
      0:
        begin
          Result := Result + '    ' + IntToStr(Index) + ':'#13#10 + '      begin'#13#10 +
            '        obj.' + Name + ' := ' + StrConvertXML(String2XMLType(DataType), '_Value')
            + ';'#13#10 + '      end;'#13#10;
          Inc(Index);
        end;
      1:
        begin
          Result := Result + '    ' + IntToStr(Index) + ':'#13#10 + '      begin'#13#10 +
            '        obj.' + Name + ' := ' + StrConvertXML(String2XMLType(DataType), '_Value')
            + ';'#13#10 + '      end;'#13#10;
          Inc(Index);
        end;
      2:
        begin
        end;
    end;
  end;
end;

function TXML2CodeChildPas.LeafToInspectorImplement: string;
begin
  Result := '';
  if Visual then
  begin
    case Number of
      0:
        begin
          Result := Result + '  Names_Value.Add(''' + Name + ''');'#13#10 + '  Types_Value.Add(' +
            XMLTypeStrings[String2XMLType(DataType)] + ');'#13#10 + '  _Values_Value.Add(' +
            XMLConvertStr(String2XMLType(DataType), 'Obj.' + Name) + ');'#13#10;
        end;
      1:
        begin
          Result := Result + '  Names_Value.Add(''' + Name + ''');'#13#10 + '  Types_Value.Add(' +
            XMLTypeStrings[String2XMLType(DataType)] + ');'#13#10 + '  _Values_Value.Add(' +
            XMLConvertStr(String2XMLType(DataType), 'Obj.' + Name) + ');'#13#10;
        end;
      2:
        begin
        end;
    end;
  end;
end;

function TXML2CodeChildPas.LeafToTree: string;
begin
  if not Visual then
  begin
    Result := '';
    Exit;
  end;
  case Number of
    0:
      begin
        Result := '  if Obj.' + Name + 'Exsit then'#13#10 + //
          '    TreeNodeShape.AddChild(''' + Name + ':'' + ' + ConvertStr + ');'#13#10;
      end;
    1:
      begin
        Result := '  TreeNodeShape.AddChild(''' + Name + ':'' + ' + ConvertStr + ');'#13#10;
      end;
    2:
      begin
        Result := '  for I := 0 to ' + Name + 'Count - 1 do'#13#10 + // 0
          '  begin'#13#10 + // 1
          '    TreeNodeShape.AddChild(''' + Name + '[I]:'' + ' + ConvertStr + ');'#13#10;
      end;
  end;
end;

function TXML2CodeChildPas.ReadAttribute: string;
var
  tmp: string;
begin
  Result := '';
  if NodeType = ntAttribute then
  begin
    tmp := Path;
    Delete(tmp, 1, 1);
    case Number of
      0:
        Result := '      else if nodeTmp.NodeName = ''' + tmp + ''' then'#13#10 +
          '      begin'#13#10 + '        Obj.' + Name + ' := ' + Self.ConvertXML + ';'#13#10 +
          '        Obj.' + Name + 'Exsit := True;'#13#10 + '      end'#13#10;
      1:
        Result := '      else if nodeTmp.NodeName = ''' + tmp + ''' then'#13#10 +
          '      begin'#13#10 + '        Obj.' + Name + ' := ' + Self.ConvertXML + ';'#13#10 +
          '      end'#13#10;
      2:
        ;
    end;
  end;
end;

function TXML2CodeChildPas.ReadChild: string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case NodeType of
    ntElement:
      begin
        if Leaf then
        begin
          case Number of
            0:
              begin
                Result := '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 +
                  '      begin'#13#10 + '        Obj.' + Name + ' := ' + Self.ConvertXML +
                  ';'#13#10 + '        Obj.' + Name + 'Exsit := True;'#13#10 +
                  '      end'#13#10;
              end;
            1:
              begin
                Result := '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 +
                  '      begin'#13#10 + '        Obj.' + Name + ' := ' + Self.ConvertXML +
                  ';'#13#10 + '      end'#13#10;
              end;
            2:
              begin
                Result := '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 +
                  '      begin'#13#10 + '        Obj.' + Name + 'Add(' + Self.ConvertXML +
                  ');'#13#10 + '      end'#13#10;
              end;
          end;
        end
        else
        begin
          case Number of
            0:
              begin
                if isVirtual then
                begin
                  childclasses := Self.ChildClass;
                  for I := 0 to childclasses.count - 1 do
                  begin
                    Result := Result + //
                      '      else if nodeTmp.NodeName = ''' + childclasses.Items[I]
                      .Path + ''' then'#13#10 + //
                      '      begin'#13#10 + //
                      '        Obj.' + Name + ' := ' + childclasses.Items[I]
                      .DataType + '.Create;'#13#10 + //
                      '        ' + childclasses.Items[I].DataType + '_FromXML(' + childclasses.Items
                      [I].DataType + '(Obj.' + Name + '), nodeTmp);'#13#10 + //
                      '        Obj.' + Name + 'Exsit := True;'#13#10 + '      end'#13#10;
                  end;
                  childclasses.Free;
                end
                else
                begin
                  Result := //
                    '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 + //
                    '      begin'#13#10 + //
                    '        Obj.' + Name + ' := ' + DataType + '.Create;'#13#10 + //
                    '        ' + DataType + '_FromXML(Obj.' + Name + ', nodeTmp);'#13#10 + //
                    '        Obj.' + Name + 'Exsit := True;'#13#10 + '      end'#13#10;
                end;
              end;
            1:
              begin
                if isVirtual then
                begin
                  childclasses := Self.ChildClass;
                  for I := 0 to childclasses.count - 1 do
                  begin
                    Result := Result + //
                      '      else if nodeTmp.NodeName = ''' + childclasses.Items[I]
                      .Path + ''' then'#13#10 + //
                      '      begin'#13#10 + '        Obj.' + Name + ' := ' + childclasses.Items[I]
                      .DataType + '.Create;'#13#10 + //
                      '        ' + childclasses.Items[I].DataType + '_FromXML(' + childclasses.Items
                      [I].DataType + '(Obj.' + Name + '), nodeTmp);'#13#10 + '      end'#13#10;
                  end;
                  childclasses.Free;
                end
                else
                begin
                  Result := //
                    '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 + //
                    '      begin'#13#10 + //
                    '        Obj.' + Name + ' := ' + DataType + '.Create;'#13#10 + //
                    '        ' + DataType + '_FromXML(Obj.' + Name + ', nodeTmp);'#13#10 + //
                    '      end'#13#10;
                end;
              end;
            2:
              begin
                if isVirtual then
                begin
                  childclasses := Self.ChildClass;
                  for I := 0 to childclasses.count - 1 do
                  begin
                    Result := Result + //
                      '      else if nodeTmp.NodeName = ''' + childclasses.Items[I]
                      .Path + ''' then'#13#10 + //
                      '      begin'#13#10 + //
                      '        ' + Name + 'Tmp := ' + childclasses.Items[I]
                      .DataType + '.Create;'#13#10 + //
                      '        ' + childclasses.Items[I].DataType + '_FromXML(' + childclasses.Items
                      [I].DataType + '(' + Name + 'Tmp), nodeTmp);'#13#10 + //
                      '        Obj.Add' + Name + '(' + Name + 'Tmp);'#13#10 + //
                      '      end'#13#10;
                  end;
                  childclasses.Free;
                end
                else
                begin
                  Result := //
                    '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 + //
                    '      begin'#13#10 + //
                    '        ' + Name + 'Tmp := ' + DataType + '.Create;'#13#10 + //
                    '        ' + DataType + '_FromXML(' + Name + 'Tmp, nodeTmp);'#13#10 + //
                    '        Obj.Add' + Name + '(' + Name + 'Tmp);'#13#10 + //
                    '      end'#13#10;
                end;
              end;
          end;
        end;
      end;
    ntCData:
      begin
        case Number of
          0:
            begin
              Result := '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 +
                '      begin'#13#10 + '        Obj.' + Name + ' := ' + Self.ConvertXML +
                ';'#13#10 + '      end'#13#10;
            end;
          1:
            begin
              Result := '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 +
                '      begin'#13#10 + '        Obj.' + Name + ' := ' + Self.ConvertXML +
                ';'#13#10 + '      end'#13#10;
            end;
          2:
            begin
              Result := '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 +
                '      begin'#13#10 + '        Obj.' + Name + 'Add(' + ConvertXML + ');'#13#10 +
                '      end'#13#10;
            end;
        end;
      end;
  end;
end;

function TXML2CodeChildPas.ReadText: string;
begin
  Result := '';
  if NodeType = ntText then
  begin
    case Number of
      0:
        begin
          Result := '    Obj.' + Name + ' := ' + Self.ConvertXML('node.Text') + ';'#13#10;
        end;
      1:
        begin
          Result := '    Obj.' + Name + ' := ' + Self.ConvertXML('node.Text') + ';'#13#10;
        end;
      2:
        begin
        end;
    end;
  end;
end;

function TXML2CodeChildPas.WriteAttribute: string;
var
  tmp: string;
begin
  Result := '';
  if NodeType = ntAttribute then
  begin
    tmp := Path;
    Delete(tmp, 1, 1);
    case Number of
      0:
        Result := '    if Obj.' + Name + 'Exsit then '#13#10 + '    begin'#13#10 + '      ' +
          Name + 'Tmp := doc.CreateNode(''' + tmp + ''', ntAttribute);'#13#10 + '      ' + Name +
          'Tmp.NodeValue := ' + ConvertStr + ';'#13#10 + '      node.AttributeNodes.Add(' + Name +
          'Tmp);'#13#10 + '    end;'#13#10;
      1:
        Result := '    ' + Name + 'Tmp := doc.CreateNode(''' + tmp + ''', ntAttribute);'#13#10 +
          '    ' + Name + 'Tmp.NodeValue := ' + ConvertStr + ';'#13#10 +
          '    node.AttributeNodes.Add(' + Name + 'Tmp);'#13#10;
      2:
        ;
    end;
  end;
end;

function TXML2CodeChildPas.WriteChild: string;
var
  tmp: string;
begin
  Result := '';
  case NodeType of
    ntElement:
      begin
        if Leaf then
        begin
          case Number of
            0:
              begin
                Result := '    if Obj.' + Name + 'Exsit then'#13#10 + '    begin'#13#10 +
                  '      ' + Name + 'Tmp := doc.CreateNode(''' + Path + ''', ntElement);'#13#10 +
                  '      ' + Name + 'Tmp.NodeValue := ' + ConvertStr + ';'#13#10 +
                  '      node.ChildNodes.Add(' + Name + 'Tmp);'#13#10 + '    end;'#13#10;
              end;
            1:
              begin
                Result := '    ' + Name + 'Tmp := doc.CreateNode(''' + Path +
                  ''', ntElement);'#13#10 + '    ' + Name + 'Tmp.NodeValue := ' + ConvertStr +
                  ';'#13#10 + '    node.ChildNodes.Add(' + Name + 'Tmp);'#13#10;
              end;
            2:
              begin
                Result := '    for I := 0 to Obj.' + Name + 'Count - 1 do'#13#10 + 'begin'#13#10 +
                  '      ' + Name + 'Tmp := doc.CreateNode(''' + Path + ''', ntElement);'#13#10 +
                  '      ' + Name + 'Tmp.NodeValue := ' + ConvertStr + ';'#13#10 +
                  '      node.ChildNodes.Add(' + Name + 'Tmp);'#13#10 + '    end;'#13#10;
              end;
          end;
        end
        else
        begin
          case Number of
            0:
              begin
                Result := //
                  '    if Obj.' + Name + 'Exsit then'#13#10 + //
                  '      ' + DataType + '_ToXML(Obj.' + Name + ', node, ''' + Path + ''');'#13#10;
              end;
            1:
              begin
                Result := //
                  '    ' + DataType + '_ToXML(Obj.' + Name + ', node, ''' + Path + ''');'#13#10;
              end;
            2:
              begin
                Result := //
                  '    for I := 0 to Obj.' + Name + 's.Count - 1 do'#13#10 + //
                  '       ' + DataType + '_ToXML(Obj.' + Name + '[I], node, ''' + Path +
                  ''');'#13#10;
              end;
          end;
        end;
      end;
    ntCData:
      begin
        tmp := Path;
        Delete(tmp, 1, 1);
        case Number of
          0:
            begin
              Result := '    if Obj.' + Name + 'Exsit then'#13#10 + 'begin'#13#10 + '      ' +
                Name + 'Tmp := doc.CreateNode(''' + tmp + ''', ntCData);'#13#10 + '      ' +
                Name + 'Tmp.NodeValue := ' + ConvertStr + ';'#13#10 +
                '      node.ChildNodes.Add(' + Name + 'Tmp);'#13#10 + '    end;'#13#10;
            end;
          1:
            begin
              Result := '    ' + Name + 'Tmp := doc.CreateNode(''' + tmp + ''', ntCData);'#13#10 +
                '    ' + Name + 'Tmp.NodeValue := ' + ConvertStr + ';'#13#10 +
                '    node.ChildNodes.Add(' + Name + 'Tmp);'#13#10;
            end;
          2:
            begin
              Result := '    for I := 0 to Obj.' + Name + 'Count - 1 do'#13#10 + 'begin'#13#10 +
                '      ' + Name + 'Tmp := doc.CreateNode(''' + tmp + ''', ntCData);'#13#10 +
                '      ' + Name + 'Tmp.NodeValue := ' + ConvertStr + ';'#13#10 +
                '      node.ChildNodes.Add(' + Name + 'Tmp);'#13#10 + '    end;'#13#10;
            end;
        end;
      end;
  end;
end;

function TXML2CodeChildPas.WriteText: string;
begin
  Result := '';
  if NodeType = ntText then
  begin
    case Number of
      0:
        begin
          Result := '    if Obj.' + Name + 'Exsit then'#13#10 + '    begin'#13#10 +
            '      node.NodeValue := ' + ConvertStr + ';'#13#10 + '    end;'#13#10;
        end;
      1:
        begin
          Result := '    node.NodeValue := ' + ConvertStr + ';'#13#10;
        end;
      2:
        begin
        end;
    end;
  end;
end;

function TXML2CodeChildPas.ChildAddEventMenuStatement: string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case Number of
    0:
      begin
        if isVirtual then
        begin
          Result := '  ' + Name + 'AddMenu: TMenuItem;'#13#10;
          childclasses := Self.ChildClass;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + '  ' + childclasses[I].Name + 'AddMenu: TMenuItem;'#13#10;
          end;
        end
        else
        begin
          Result := '  ' + Name + 'AddMenu: TMenuItem;'#13#10;
        end;
      end;
    1:
      ;
    2:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          Result := '  ' + Name + 'AddMenu: TMenuItem;'#13#10;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + '  ' + childclasses[I].Name + 'AddMenu: TMenuItem;'#13#10;
          end;
        end
        else
        begin
          Result := '  ' + Name + 'AddMenu: TMenuItem;'#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildPas.ChildAddEventMenuImplement(par: TXML2CodeClass): string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case Number of
    0:
      begin
        if isVirtual then
        begin
          Result := //
            '    ' + Name + 'AddMenu := TMenuItem.Create(pop);'#13#10 + //
            '    ' + Name + 'AddMenu.Caption := ''Add ' + Name + ''';'#13#10 + //
            '    pop.Items.Add(' + Name + 'AddMenu);'#13#10;
          childclasses := Self.ChildClass;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + //
              '    ' + childclasses[I].Name + 'AddMenu := TMenuItem.Create(' + Name +
              'AddMenu);'#13#10 + //
              '    ' + childclasses[I].Name + 'AddMenu.Caption := ''' + childclasses[I]
              .Name + ''';'#13#10 + //
              '    ' + childclasses[I].Name + 'AddMenu.OnClick := ' + par.DataType + '_Add' +
              childclasses[I].Name + 'Event;'#13#10 + //
              '    ' + Name + 'AddMenu.Add(' + childclasses[I].Name + 'AddMenu);'#13#10;
          end;
        end
        else
        begin
          Result := //
            '    ' + Name + 'AddMenu := TMenuItem.Create(pop);'#13#10 + //
            '    ' + Name + 'AddMenu.Caption := ''Add ' + Name + ''';'#13#10 + //
            '    ' + Name + 'AddMenu.OnClick := ' + par.DataType + '_Add' + Name + 'Event;'#13#10 +
          //
            '    pop.Items.Add(' + Name + 'AddMenu);'#13#10;
        end;
      end;
    1:
      ;
    2:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          Result := //
            '    ' + Name + 'AddMenu := TMenuItem.Create(pop);'#13#10 + //
            '    ' + Name + 'AddMenu.Caption := ''Add ' + Name + ''';'#13#10 + //
            '    pop.Items.Add(' + Name + 'AddMenu);'#13#10;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + //
              '    ' + childclasses[I].Name + 'AddMenu := TMenuItem.Create(' + Name +
              'AddMenu);'#13#10 + //
              '    ' + childclasses[I].Name + 'AddMenu.Caption := ''' + childclasses[I]
              .Name + ''';'#13#10 + //
              '    ' + childclasses[I].Name + 'AddMenu.OnClick := ' + par.DataType + '_Add' +
              childclasses[I].Name + 'Event;'#13#10 + //
              '    ' + Name + 'AddMenu.Add(' + childclasses[I].Name + 'AddMenu);'#13#10;

          end;
        end
        else
        begin
          Result := //
            '    ' + Name + 'AddMenu := TMenuItem.Create(pop);'#13#10 + //
            '    ' + Name + 'AddMenu.Caption := ''Add ' + Name + ''';'#13#10 + //
            '    ' + Name + 'AddMenu.OnClick := ' + par.DataType + '_Add' + Name + 'Event;'#13#10 +
          //
            '    pop.Items.Add(' + Name + 'AddMenu);'#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildPas.ChildAddEventStatement(par: TXML2CodeClass): string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case Number of
    0:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + '    procedure ' + par.DataType + '_Add' + childclasses[I].Name +
              'Event(Sender: TObject);'#13#10;
          end;
        end
        else
        begin
          Result := '    procedure ' + par.DataType + '_Add' + Name +
            'Event(Sender: TObject);'#13#10;
        end;
      end;
    1:
      ;
    2:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + '    procedure ' + par.DataType + '_Add' + childclasses[I].Name +
              'Event(Sender: TObject);'#13#10;
          end;
        end
        else
        begin
          Result := '    procedure ' + par.DataType + '_Add' + Name +
            'Event(Sender: TObject);'#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildPas.ChildAddEventImplement(par: TXML2CodeClass): string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case Number of
    0:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + 'procedure ' + x_2_c.DataType + 'Tree.' + par.DataType + '_Add' +
              childclasses[I].Name + 'Event(Sender: TObject);'#13#10 + 'begin'#13#10 + //
              '  ' + par.DataType + '(TargetObject).Add' + childclasses[I].Name + ';'#13#10 + // 0
              '  ' + par.DataType + '_ToTree(' + par.DataType +
              '(TargetObject), TargetNode);'#13#10 +
            //
              'end;'#13#10#13#10;
          end;
        end
        else
        begin
          Result := 'procedure ' + x_2_c.DataType + 'Tree.' + par.DataType + '_Add' + Name +
            'Event(Sender: TObject);'#13#10 + 'begin'#13#10 + //
            '  ' + par.DataType + '(TargetObject).Add' + Name + ';'#13#10 + // 0
            '  ' + par.DataType + '_ToTree(' + par.DataType +
            '(TargetObject), TargetNode);'#13#10 + //
            'end;'#13#10#13#10;
        end;
      end;
    1:
      ;
    2:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + 'procedure ' + x_2_c.DataType + 'Tree.' + par.DataType + '_Add' +
              childclasses[I].Name + 'Event(Sender: TObject);'#13#10 + 'begin'#13#10 + //
              '  ' + par.DataType + '(TargetObject).AddNew' + childclasses[I].Name + ';'#13#10 +
            // 0
              '  ' + par.DataType + '_ToTree(' + par.DataType +
              '(TargetObject), TargetNode);'#13#10 +
            //
              'end;'#13#10#13#10;
          end;
        end
        else
        begin
          Result := 'procedure ' + x_2_c.DataType + 'Tree.' + par.DataType + '_Add' + Name +
            'Event(Sender: TObject);'#13#10 + 'begin'#13#10 + //
            '  ' + par.DataType + '(TargetObject).AddNew' + Name + ';'#13#10 + // 0
            '  ' + par.DataType + '_ToTree(' + par.DataType +
            '(TargetObject), TargetNode);'#13#10 + //
            'end;'#13#10#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildPas.ChildDeleteEventStatement(par: TXML2CodeClass): string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  if Leaf then
    Exit;
  if Number = 1 then
    Exit;
  Result := Result + //
    '    procedure ' + par.DataType + '_' + Name + 'DeleteEvent(obj: ' + par.DataType +
    '; del: TObject);'#13#10;
end;

function TXML2CodeChildPas.ChildDeleteEventImplement(par: TXML2CodeClass): string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  if Leaf then
    Exit;
  if Number = 1 then
    Exit;
  Result := Result + //
    'procedure ' + x_2_c.DataType + 'Tree.' + par.DataType + '_' + Name + 'DeleteEvent(obj: ' +
    par.DataType + '; del: TObject);'#13#10 + 'begin'#13#10;
  case Number of
    0:
      begin
        Result := Result + //
          '  if del is ' + DataType + ' then'#13#10 + // 0
          '  begin'#13#10 + // 1
          '    ' + par.DataType + '(obj).' + Name + 'Remove;'#13#10 + // 2
          '    ' + par.DataType + '_ToTree(' + par.DataType + '(obj), TargetNode);'#13#10 + // 3
          '  end;'#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := Result + //
          '  if del is ' + DataType + ' then'#13#10 + // 0
          '  begin'#13#10 + // 1
          '    ' + par.DataType + '(obj).Remove' + Name + '(' + DataType + '(del));'#13#10 + // 2
          '    ' + par.DataType + '_ToTree(' + par.DataType + '(obj), TargetNode);'#13#10 + // 3
          '  end;'#13#10;
      end;
  end;
  Result := Result + 'end;'#13#10#13#10;
end;

function TXML2CodeChildPas.ChildToTree: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '  if Obj.' + Name + 'Exsit then'#13#10 + //
          '  begin'#13#10 + //
          '    ' + DataType + '_ToTree(obj.' + Name + ', TreeNodeShape.AddChildObject(''' + Name +
          ''', obj.' + Name + '));'#13#10 + // 0
          '  end;'#13#10;
      end;
    1:
      begin
        Result := '  ' + DataType + '_ToTree(obj.' + Name + ', TreeNodeShape.AddChildObject(''' +
          Name + ''', obj.' + Name + '));'#13#10;
      end;
    2:
      begin
        Result := '  for I := 0 to obj.' + Name + 'Count - 1 do'#13#10 + // 0
          '  begin'#13#10 + // 1
          '    ' + DataType + '_ToTree(obj.' + Name + '[I], TreeNodeShape.AddChildObject(''' +
          Name + ''', obj.' + Name + '[I]));'#13#10 + '  end;'#13#10;
      end;
  end;
end;

end.
