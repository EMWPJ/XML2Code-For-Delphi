unit XML2Code;

interface

uses
  Windows, SysUtils, Types, Classes, Variants, Generics.Collections,
  Dialogs, Controls, ExtCtrls, StrUtils, TeeTree, Menus,
  XMLCore, XMLDoc, XMLIntf, XMLLeafTypes, XMLInspector, XML2CodeBase;

type

  TXML2CodeHelper = class helper for TXML2Code
  private
    function ClassOf(const dt: String): TXML2CodeClass;
    function BaseUse: string;
    function BaseTypes: string;
    function BaseImplements: string;
    function BaseStatements: string;
    function BaseVars: string;
    function Use: string;
    function Types: string;
    function Implements: string;
    function Statements: string;
  public
    procedure ExportBaseCode(filePath: string);
    procedure ExportCode(filePath: string);

    procedure Load(const fileName: string); overload;
  end;

  TXML2CodeClassHelper = class helper for TXML2CodeClass
  private
    function ParentX2C: TXML2Code;
    function PrivateStatements: string;
    function ProtectedStatements: string;
    function PublicStatements: string;
    function Implements: string;
    function Statements: string;
    function FromXMLImplement: string;
    function FromXMLStatement: string;
    function ToXMLImplement: string;
    function ToXMLStatement: string;
    function AfterCreateStatement: string;
    function AfterCreateImplement: string;
    function AppendToXMLImplement: string;
    function AppendToXMLStatement: string;
    function CopyFromXMLImplement: string;
    function CopyFromXMLStatement: string;
    function PasteToEventImplement: string;
    function PasteToEventStatement: string;
    function ChildDeleteEventImplement: string;
    function ChildDeleteEventStatement: string;
    function BaseConstructorImplements: string;
    function BaseConstructorStatements: string;
    function BaseDestructorImplements: string;
    function BaseDestructorStatements: string;
    function BaseImplements: string;
    function BasePrivateStatements: string;
    function BaseProtectedStatements: string;
    function BasePublicStatements: string;
    function BaseStatements: string;
    function ToTreeImplement: string;
    function ToTreeStatement: string;
    function ToPopupMenuImplement: string;
    function ToPopupMenuStatement: string;
    function ToInspectorStatement: string;
    function ToInspectorImplement: string;

    function SetXMLPropertyStatement: string;
    function SetXMLPropertyImplement: string;

  public
  end;

  TXML2CodeChildHelper = class helper for TXML2CodeChild
  private
    function ChildClass: TList<TXML2CodeClass>;
    function ChildFieldStatement: string;
    function ChildAddImplement: string;
    function ChildAddStatement: string;
    function ChildCountImplement: string;
    function ChildCountStatement: string;
    function ChildPropertyStatement: string;
    function ChildSetImplement: string;
    function ChildSetStatement: string;
    function ChildRemoveImplement: string;
    function ChildRemoveStatement: string;
    function ChildClearImplement: string;
    function ChildClearStatement: string;
    function ChildDeleteImplement: string;
    function ChildDeleteStatement: string;
    function ChildIndexPropertyStatement: string;
    function ChildGetIndexImplement: string;
    function ChildGetIndexStatement: string;
    function ChildSetIndexImplement: string;
    function ChildSetIndexStatement: string;
    function ChildExsitProperty: string;

    function ChildAddEventImplement: string;
    function ChildAddEventStatement: string;
    function ChildAddEventMenuImplement: string;
    function ChildAddEventMenuStatement: string;
    function ChildToTree: string;

    function ChildPrivateStatements: string;
    function ChildPublicStatements: string;
    function ChildProtectedStatements: string;
    function ChildBaseImplements: string;

    function LeafFieldStatement: string;
    function LeafAddImplement: string;
    function LeafAddStatement: string;
    function LeafClearImplement: string;
    function LeafClearStatement: string;
    function LeafCountImplement: string;
    function LeafCountStatement: string;
    function LeafPropertyStatement: string;
    function LeafSetImplement: string;
    function LeafSetStatement: string;
    function LeafRemoveImplement: string;
    function LeafRemoveStatement: string;
    function LeafDeleteImplement: string;
    function LeafDeleteStatement: string;
    function LeafIndexPropertyStatement: string;
    function LeafGetIndexImplement: string;
    function LeafGetIndexStatement: string;
    function LeafSetIndexImplement: string;
    function LeafSetIndexStatement: string;
    function LeafExsitProperty: string;

    function LeafAddEventImplement: string;
    function LeafAddEventStatement: string;
    function LeafAddEventMenuImplement: string;
    function LeafAddEventMenuStatement: string;
    function LeafToTree: string;

    function LeafPrivateStatements: string;
    function LeafProtectedStatements: string;
    function LeafPublicStatements: string;
    function LeafBaseImplements: string;

    function LeafToInspectorImplement: string;
    function LeafSetXMLPropertyImplement(var Index: Integer): string;
    function LeafType: string;
    function ParentClass: TXML2CodeClass;
    function ConvertStr: string;
    function ConvertXML(_Value: string = 'nodeTmp.Text'): string;
    function NodeType: TNodeType;

    function ReadAttribute: string;
    function ReadChild: string;
    function ReadText: string;

    function WriteAttribute: string;
    function WriteChild: string;
    function WriteText: string;
  public
  end;

var
  XML2CodeVisualCode: Boolean;
  XML2CodeInheritedModel: Boolean;

implementation

{ TXML2CodeHelper }
function TXML2CodeHelper.BaseImplements: string;
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

function TXML2CodeHelper.BaseStatements: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + XMLClass[I].BaseStatements;
  end;
end;

function TXML2CodeHelper.BaseTypes: string;
var
  I: Integer;
begin
  Result := 'type'#13#10#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + '  ' + XMLClass[I].DataType + ' = class;'#13#10;
  end;
end;

function TXML2CodeHelper.BaseUse: string;
var
  I, count: Integer;
begin
  if XML2CodeVisualCode then
    Result := 'uses'#13#10 +
      '  Windows, SysUtils, Types, Classes, Variants, Generics.Collections, Dialogs, Controls, ExtCtrls,'#13#10 +
    // 0
      '  XMLCore, XMLDoc, XMLIntf, XMLLeafTypes, TeeTree, Menus, XMLInspector'
  else
    Result := 'uses'#13#10 +
      '  Windows, SysUtils, Types, Classes, Variants, Generics.Collections, Dialogs, Controls, ExtCtrls,'#13#10 +
    // 0
      '  XMLCore, XMLDoc, XMLIntf, XMLLeafTypes';
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

function TXML2CodeHelper.BaseVars: string;
var
  I: Integer;
begin
  // Result := 'var'#13#10;
  // for I := 0 to XMLClassCount - 1 do
  // begin
  // Result := Result + //
  // '  Copyed' + XMLClass[I].DataType + ': ' + XMLClass[I].DataType + ';'#13#10;
  // end;
end;

function TXML2CodeHelper.ClassOf(const dt: String): TXML2CodeClass;
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

procedure TXML2CodeHelper.ExportBaseCode(filePath: string);
var
  strs: TStringList;
begin
  strs := TStringList.Create;
  strs.Add('unit ' + Name + 'Base;'#13#10 + ''#13#10 + 'interface'#13#10);
  strs.Add(Self.BaseUse);
  strs.Add(Self.BaseTypes);
  strs.Add(Self.BaseStatements);
  strs.Add(Self.BaseVars);
  strs.Add(Self.BaseImplements);
  strs.Add('end.');
  strs.SaveToFile(filePath + '\' + Self.Name + 'Base.pas');
  strs.Free;
end;

procedure TXML2CodeHelper.ExportCode(filePath: string);
var
  strs: TStringList;
begin
  strs := TStringList.Create;
  strs.Add('unit ' + Name + ';'#13#10 + ''#13#10 + 'interface'#13#10);
  strs.Add(Self.Use);
  strs.Add(Self.Types);
  strs.Add(Self.Statements);
  strs.Add(Self.Implements);
  strs.Add('end.');
  strs.SaveToFile(filePath + '\' + Name + '.pas');
  strs.Free;
end;

function TXML2CodeHelper.Implements: string;
var
  I: Integer;
begin
  Result := 'implementation'#13#10#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + XMLClass[I].Implements;
  end;
  Result := Result + #13#10;
end;

procedure TXML2CodeHelper.Load(const fileName: string);
var
  I, J: Integer;
begin
  inherited Load(fileName);
  for I := 0 to XMLClassCount - 1 do
  begin
    if XMLClass[I].Path = '' then
    begin
      XMLClass[I].AddPath;
      XMLClass[I].Path := XMLClass[I].Name;
    end;
    if not XMLClass[I].isAbstractExsit then
    begin
      XMLClass[I].AddisAbstract;
      XMLClass[I].isAbstract := False;
    end;
    for J := 0 to XMLClass[I].ChildCount - 1 do
    begin
      if not XMLClass[I].Child[J].isVirtualExsit then
      begin
        XMLClass[I].Child[J].AddisVirtual;
        XMLClass[I].Child[J].isVirtual := False;
      end;
    end;
  end;
end;

function TXML2CodeHelper.Statements: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + XMLClass[I].Statements + #13#10;
  end;
end;

function TXML2CodeHelper.Types: string;
var
  I: Integer;
begin
  Result := 'type'#13#10#13#10;
end;

function TXML2CodeHelper.Use: string;
var
  I, count: Integer;
begin
  Result := 'uses'#13#10 + //
    '  Windows, SysUtils, Types, Classes, Variants, Generics.Collections,'#13#10 + // 0
    '  Dialogs, Controls, ExtCtrls, StrUtils, TeeTree, Menus,'#13#10 + // 1
    '  XMLCore, XMLDoc, XMLIntf, XMLLeafTypes, XMLInspector, '#13#10 + // 2
    '  ' + Name + 'Base';
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

function TXML2CodeClassHelper.FromXMLImplement: string;
var
  I, J: Integer;
  str: string;
begin
  Result := 'procedure ' + DataType + '.FromXML(node: IXMLNode);'#13#10 + //
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
      Result := Result + '    inherited FromXML(node);'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Number = 2 then
    begin
      Result := Result + '    ' + Child[I].Name + 'Clear;'#13#10;
    end
    else if Child[I].Number = 0 then
    begin
      Result := Result + '    F' + Child[I].Name + 'Exsit := False;'#13#10;
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

function TXML2CodeClassHelper.FromXMLStatement: string;
begin
  Result := '    procedure FromXML(node: IXMLNode); override;'#13#10;
  if isAbstract then
    Result := '    procedure FromXML(node: IXMLNode); virtual;'#13#10;
end;

function TXML2CodeClassHelper.Implements: string;
var
  I: Integer;
begin
  Result := '{  ' + Name + '}'#13#10;
end;

function TXML2CodeClassHelper.ToPopupMenuImplement: string;
var
  I: Integer;
begin
  if not XML2CodeVisualCode then
  begin
    Result := '';
    Exit;
  end;
  Result := //
    'procedure ' + DataType + '.ToPopupMenu(Sender: TObject);'#13#10 + //
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
      Result := Result + Child[I].LeafAddEventMenuImplement;
    end
    else
    begin
      Result := Result + Child[I].ChildAddEventMenuImplement;
    end;
  end;
  Result := Result + //
    '  end;'#13#10 + // 2
    'end;'#13#10#13#10;
end;

function TXML2CodeClassHelper.ToPopupMenuStatement: string;
begin
  if not XML2CodeVisualCode then
  begin
    Result := '';
    Exit;
  end;
  Result := '    procedure ToPopupMenu(Sender: TObject); override;'#13#10;
end;

function TXML2CodeClassHelper.ParentX2C: TXML2Code;
begin
  Result := TXML2Code(Self.Parent);
end;

function TXML2CodeClassHelper.PasteToEventImplement: string;
var
  I, J: Integer;
  childclasses: TList<TXML2CodeClass>;
begin
  if not XML2CodeVisualCode then
  begin
    Result := '';
    Exit;
  end;
  Result := 'procedure ' + DataType +
    '.PasteToEvent(items: TList<TXMLTreeNode>; const isCut: Boolean = False);'#13#10;
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
    '    Self.CopyFrom(' + DataType + '(items[0]));'#13#10 + // 2
    '    Self.ToTree;'#13#10 + // 3
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
      '        ' + DataType + '(tmp' + Child[I].Name + '.Parent).' + Child[I]
      .Name + 's.Remove(tmp' + Child[I].Name + ');'#13#10 + // 5
      '        Self.' + Child[I].Name + 's.Add(tmp' + Child[I].Name + ');'#13#10 + // 6
      '        tmp' + Child[I].Name + '.Parent := Self;'#13#10 + // 7
      '        tmp' + Child[I].Name + '.TreeNodeShape.Parent := Self.TreeNodeShape;'#13#10 + // 8
      '      end'#13#10 + // 9
      '      else'#13#10 + // 10
      '      begin'#13#10;
    if Child[I].isVirtual then
    begin
      childclasses := Child[I].ChildClass;
      for J := 0 to childclasses.count - 1 do
      begin
        Result := Result + //
          '        if tmp' + Child[I].Name + ' is ' + childclasses[J].DataType + ' then'#13#10 +
        // 0
          '          Add' + childclasses[J].Name + '.CopyFrom(tmp' + Child[I].Name + ');'#13#10;
      end;
      childclasses.Free;
    end
    else
    begin
        Result := Result + //
          '        Add' + Child[I].Name + '.CopyFrom(tmp' + Child[I].Name + ');'#13#10;
    end;
    Result := Result + //
      '      end;'#13#10 + // 13
      '    end;'#13#10;
  end;
  Result := Result + //
    '  end;'#13#10 + // 0
    '  Self.ToTree;'#13#10 + //
    'end;'#13#10#13#10;
end;

function TXML2CodeClassHelper.PasteToEventStatement: string;
begin
  if not XML2CodeVisualCode then
  begin
    Result := '';
    Exit;
  end;
  Result :=
    '    procedure PasteToEvent(items: TList<TXMLTreeNode>; const isCut: Boolean = False); override;'#13#10;
end;

function TXML2CodeClassHelper.PrivateStatements: string;
begin
  Result := '  private'#13#10;
end;

function TXML2CodeClassHelper.ProtectedStatements: string;
begin
  Result := '  protected'#13#10;
end;

function TXML2CodeClassHelper.PublicStatements: string;
begin
  Result := '  public'#13#10;
end;

function TXML2CodeClassHelper.ToInspectorImplement: string;
var
  I: Integer;
begin
  if not XML2CodeVisualCode then
  begin
    Result := '';
    Exit;
  end;
  Result := //
    'procedure ' + DataType + '.ToInspector;'#13#10 + //
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
    '  TXMLInspector(ins).SetData(Names_Value, _Values_Value, Types_Value, Self);'#13#10 + //
    'end;'#13#10#13#10;
end;

function TXML2CodeClassHelper.ToInspectorStatement: string;
begin
  if not XML2CodeVisualCode then
  begin
    Result := '';
    Exit;
  end;
  Result := '    procedure ToInspector(ins: TObject); override;'#13#10;
end;

function TXML2CodeClassHelper.SetXMLPropertyImplement: string;
var
  Index, I: Integer;
begin
  if not XML2CodeVisualCode then
  begin
    Result := '';
    Exit;
  end;
  Result := Result + 'procedure ' + DataType +
    '.SetXMLProperty(Index: Integer; _Value: String);'#13#10 + 'begin'#13#10 +
    '  case index of'#13#10;
  Index := 0;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + TXML2CodeChild(Child[I]).LeafSetXMLPropertyImplement(Index);
    end;
  end;
  Result := Result + '  end;'#13#10 + '  ToTree;'#13#10 + 'end;'#13#10#13#10;
  if Index = 0 then
  begin
    Result := 'procedure ' + DataType + '.SetXMLProperty(Index: Integer; _Value: String);'#13#10 +
      'begin'#13#10 + 'end;'#13#10#13#10;
  end;
end;

function TXML2CodeClassHelper.SetXMLPropertyStatement: string;
begin
  if not XML2CodeVisualCode then
  begin
    Result := '';
    Exit;
  end;
  Result := '    procedure SetXMLProperty(Index: Integer; _Value: String); override;'#13#10;
end;

function TXML2CodeClassHelper.Statements: string;
begin
  if XML2CodeInheritedModel then
  begin
    Result := '  ' + DataType + 'B = class(' + DataType + ')'#13#10;
  end
  else
  begin
    Result := '  ' + DataType + 'Helper = class helper for ' + DataType + #13#10;
  end;
  Result := Result + Self.PrivateStatements;
  Result := Result + Self.ProtectedStatements;
  Result := Result + Self.PublicStatements;
  Result := Result + '  end;'#13#10;
end;

function TXML2CodeClassHelper.ToTreeImplement: string;
var
  I: Integer;
begin
  if not XML2CodeVisualCode then
  begin
    Result := '';
    Exit;
  end;
  Result := 'procedure ' + DataType + '.ToTree;'#13#10 + // 0
    'var'#13#10 + // 1
    '  I: Integer;'#13#10 + // 2
    'begin'#13#10 + // 3
    '  if not Assigned(TreeNodeShape) then'#13#10 + // 4
    '  begin'#13#10 + // 5
    '    Exit;'#13#10 + // 6
    '  end;'#13#10 + // 7
    '  TreeNodeShape.Clear;'#13#10 + // 8
    '  TreeNodeShape.Data := Self;'#13#10 + // 0
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

function TXML2CodeClassHelper.ToTreeStatement: string;
begin
  if not XML2CodeVisualCode then
  begin
    Result := '';
    Exit;
  end;
  Result := '    procedure ToTree; override;'#13#10;
end;

function TXML2CodeClassHelper.ToXMLImplement: string;
var
  I, J: Integer;
begin
  Result := 'function ' + DataType + '.ToXML(par: IXMLNode; pt: string): IXMLNode;'#13#10 +
    'var'#13#10 + '  doc: IXMLDocument;'#13#10 + '  node: IXMLNode;'#13#10;
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
      Result := Result + '    inherited AppendToXML(node);'#13#10;
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
  Result := Result + '    Result := node;'#13#10 + '  except'#13#10 +
    '    raise Exception.Create(''XML2Code Write XML Error!'');'#13#10 + '  end;'#13#10 +
    'end;'#13#10#13#10;
end;

function TXML2CodeClassHelper.AfterCreateImplement: string;
var
  I: Integer;
begin
  Result := 'procedure ' + DataType + '.AfterCreate;'#13#10;
  Result := Result + 'begin'#13#10;
  Result := Result + '  inherited;'#13#10;
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
            Result := Result + '  F' + Child[I].Name + ' := ' + Child[I].DataType +
              '.Create(Self);'#13#10;
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

function TXML2CodeClassHelper.AfterCreateStatement: string;
begin
  Result := '    procedure AfterCreate; override;'#13#10;
end;

function TXML2CodeClassHelper.AppendToXMLImplement: string;
var
  I, J: Integer;
begin
  Result := '';
  if not ChildClassExsit then
    Exit;
  if Length(ChildClass) < 0 then
    Exit;
  Result := 'function ' + DataType + '.AppendToXML(node: IXMLNode; pt: string): IXMLNode;'#13#10 +
    'var'#13#10 + '  doc: IXMLDocument;'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + '  ' + Child[I].Name + 'Tmp: IXMLNode;'#13#10;
    end;
  end;
  Result := Result + '  I: Integer;'#13#10 + 'begin'#13#10 + '  try'#13#10 +
    '    doc := node.OwnerDocument;'#13#10;
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
  Result := Result + '    Result := node;'#13#10 + '  except'#13#10 +
    '    raise Exception.Create(''XML2Code Write XML Error!'');'#13#10 + '  end;'#13#10 +
    'end;'#13#10#13#10;
end;

function TXML2CodeClassHelper.AppendToXMLStatement: string;
begin
  Result := '';
  if ChildClassExsit then
    if Length(ChildClass) > 0 then
      Result := '    function AppendToXML(node: IXMLNode; pt: string = ''''): IXMLNode;'#13#10;
end;

{ TXML2CodeClassHelper }
function TXML2CodeClassHelper.BaseConstructorImplements: string;
var
  I: Integer;
begin
  Result := 'constructor ' + DataType + '.Create(par: TXML = nil);'#13#10;
  Result := Result + 'begin'#13#10;
  Result := Result + '  inherited Create(par);'#13#10;
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
            Result := Result + '  F' + Child[I].Name + ' := ' + Child[I].DataType +
              '.Create(Self);'#13#10;
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

function TXML2CodeClassHelper.BaseConstructorStatements: string;
begin
  Result := '    constructor Create(par: TXML = nil);'#13#10;
end;

function TXML2CodeClassHelper.BaseDestructorImplements: string;
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

function TXML2CodeClassHelper.BaseDestructorStatements: string;
begin
  Result := '    destructor Destroy; override;'#13#10;
end;

function TXML2CodeClassHelper.BaseImplements: string;
var
  I: Integer;
begin
  Result := '{  ' + Name + '}'#13#10;
  Result := Result + Self.BaseConstructorImplements;
  Result := Result + Self.BaseDestructorImplements;
  Result := Result + Self.AfterCreateImplement;
  Result := Result + Self.FromXMLImplement;
  Result := Result + Self.ToXMLImplement;
  Result := Result + Self.AppendToXMLImplement;
  if XML2CodeVisualCode then
  begin
    Result := Result + Self.ToTreeImplement;
    Result := Result + Self.ToPopupMenuImplement;
    Result := Result + Self.ToInspectorImplement;
    Result := Result + Self.CopyFromXMLImplement;
    Result := Result + Self.PasteToEventImplement;
    Result := Result + Self.ChildDeleteEventImplement;
    Result := Result + Self.SetXMLPropertyImplement;
  end;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafBaseImplements;
    end
    else
    begin
      Result := Result + Child[I].ChildBaseImplements;
    end;
  end;
end;

function TXML2CodeClassHelper.BasePrivateStatements: string;
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
    end
    else
    begin
      Result := Result + Child[I].ChildSetStatement;
      Result := Result + Child[I].ChildGetIndexStatement;
      Result := Result + Child[I].ChildSetIndexStatement;
    end;
  end;
end;

function TXML2CodeClassHelper.BaseProtectedStatements: string;
var
  I: Integer;
begin
  Result := '  protected'#13#10;
  Result := Result + Self.FromXMLStatement;
  Result := Result + Self.ToXMLStatement;
  Result := Result + Self.AppendToXMLStatement;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      if XML2CodeVisualCode then
      begin
        Result := Result + Child[I].LeafAddEventStatement;
      end;
    end
    else
    begin
      if XML2CodeVisualCode then
      begin
        Result := Result + Child[I].ChildAddEventStatement;
      end;
    end;
  end;
  Result := Result + Self.AfterCreateStatement;
end;

function TXML2CodeClassHelper.BasePublicStatements: string;
var
  I: Integer;
begin
  Result := '  public'#13#10;
  Result := Result + Self.BaseConstructorStatements;
  Result := Result + Self.BaseDestructorStatements;
  Result := Result + Self.CopyFromXMLStatement;
  if XML2CodeVisualCode then
  begin
    Result := Result + Self.ToTreeStatement;
    Result := Result + Self.ToPopupMenuStatement;
    Result := Result + Self.ToInspectorStatement;
    Result := Result + Self.PasteToEventStatement;
    Result := Result + Self.ChildDeleteEventStatement;
    Result := Result + Self.SetXMLPropertyStatement;
  end;
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
    end
    else
    begin
      Result := Result + Child[I].ChildPropertyStatement;
    end;
  end;
  if XML2CodeVisualCode then
  begin
    for I := 0 to ChildCount - 1 do
    begin
      if Child[I].Leaf then
      begin
        Result := Result + Child[I].LeafExsitProperty;
      end
      else
      begin
        Result := Result + Child[I].ChildExsitProperty;
      end;
    end;
  end;
end;

function TXML2CodeClassHelper.BaseStatements: string;
var
  absclass: string;
  visualClass: string;
begin
  if XML2CodeVisualCode then
  begin
    visualClass := 'TXMLTreeNode';
  end
  else
  begin
    visualClass := 'TXML';
  end;
  if Self.isAbstract then
  begin
    absclass := ' = class abstract(';
  end
  else
  begin
    absclass := ' = class(';
  end;
  if not Self.ParentClassExsit then
  begin
    Result := '  ' + DataType + absclass + visualClass + ')'#13#10;
  end
  else
  begin
    if Self.ParentClass = '' then
    begin
      Result := '  ' + DataType + absclass + visualClass + ')'#13#10;
    end
    else
    begin
      Result := '  ' + DataType + absclass + Self.ParentClass + ')'#13#10;
    end;
  end;
  Result := Result + Self.BasePrivateStatements;
  Result := Result + Self.BaseProtectedStatements;
  Result := Result + Self.BasePublicStatements;
  Result := Result + '  end;'#13#10#13#10;
end;

function TXML2CodeClassHelper.ChildDeleteEventImplement: string;
var
  I: Integer;
begin
  if not XML2CodeVisualCode then
  begin
    Result := '';
    Exit;
  end;
  Result := 'procedure ' + DataType + '.ChildDeleteEvent(del: TXMLTreeNode);'#13#10;
  Result := Result + 'begin'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if (Child[I].Leaf) or (Child[I].Number <> 2) then
    begin
      Continue;
    end;
    Result := Result + '  if del is ' + Child[I].DataType + ' then'#13#10 + // 0
      '  begin'#13#10 + // 1
      '    Self.Remove' + Child[I].Name + '(' + Child[I].DataType + '(del));'#13#10 + // 2
      '  end;'#13#10;
  end;
  Result := Result + 'end;'#13#10#13#19;
end;

function TXML2CodeClassHelper.ChildDeleteEventStatement: string;
begin
  if not XML2CodeVisualCode then
  begin
    Result := '';
    Exit;
  end;
  Result := '    procedure ChildDeleteEvent(del: TXMLTreeNode); override;'#13#10;
end;

function TXML2CodeClassHelper.CopyFromXMLImplement: string;
var
  I, J: Integer;
  childclasses: TList<TXML2CodeClass>;
  tmpSource: string;
begin
  if ParentClassExsit then
  begin
    Result := 'procedure ' + DataType + '.CopyFrom(source: ' + ParentClass + ');'#13#10;
    tmpSource := DataType + '(source)';
  end
  else
  begin
    Result := 'procedure ' + DataType + '.CopyFrom(source: ' + DataType + ');'#13#10;
    tmpSource := 'source';
  end;
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
            Result := Result + '  F' + Child[I].Name + ' := ' + tmpSource + '.' + Child[I]
              .Name + ';'#13#10;
            Result := Result + '  F' + Child[I].Name + 'Exsit := ' + tmpSource + '.F' + Child[I]
              .Name + 'Exsit;'#13#10;
          end;
        1:
          begin
            Result := Result + '  F' + Child[I].Name + ' := ' + tmpSource + '.' + Child[I]
              .Name + ';'#13#10;
          end;
        2:
          begin
            Result := Result + '    ' + Child[I].Name + 'Clear;'#13#10;
            Result := Result + //
              '  for I := 0 to ' + tmpSource + '.' + Child[I].Name + 'Count - 1 do'#13#10 + // 0
              '  begin'#13#10 + // 1
              '    Add' + Child[I].Name + ';'#13#10 + // 2
              '    F' + Child[I].Name + '[I] := ' + tmpSource + '.' + Child[I]
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
              '  F' + Child[I].Name + 'Exsit := ' + tmpSource + '.F' + Child[I]
              .Name + 'Exsit;'#13#10;
            Result := Result + //
              '  if ' + tmpSource + '.F' + Child[I].Name + 'Exsit then'#13#10 + // 0
              '  begin'#13#10;
            if Child[I].isVirtual then
            begin
              childclasses := Child[I].ChildClass;
              for J := 0 to childclasses.count - 1 do
              begin
                Result := Result + //
                  '    if ' + tmpSource + '.' + Child[I].Name + ' is ' + childclasses.Items[J]
                  .DataType + ' then'#13#10 + // 0
                  '      Add' + childclasses.Items[J].Name + '.CopyFrom(' + tmpSource + '.' + Child
                  [I].Name + ');'#13#10;
              end;
              childclasses.Free;
            end
            else
            begin
              Result := Result + '    Add' + Child[I].Name + '.CopyFrom(' + tmpSource + '.' + Child
                [I].Name + ');'#13#10;
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
                  .DataType + ' then'#13#10 + // 0
                  '      ' + childclasses.Items[J].Name + '.CopyFrom(' + tmpSource + '.' + Child[I]
                  .Name + ');'#13#10;
              end;
              childclasses.Free;
            end
            else
            begin
              Result := Result + '    ' + Child[I].Name + '.CopyFrom(source.' + Child[I]
                .Name + ');'#13#10;
            end;
          end;
        2:
          begin
            Result := Result + '    ' + Child[I].Name + 'Clear;'#13#10;
            Result := Result + //
              '  for I := 0 to ' + Child[I].Name + 'Count - 1 do'#13#10 + // 0
              '  begin'#13#10;
            if Child[I].isVirtual then
            begin
              childclasses := Child[I].ChildClass;
              for J := 0 to childclasses.count - 1 do
              begin
                Result := Result + //
                  '    if ' + tmpSource + '.' + Child[I].Name + '[I] is ' + childclasses.Items[J]
                  .DataType + ' then'#13#10 + // 0
                  '      Add' + childclasses.Items[J].Name + '.CopyFrom(' + tmpSource + '.' + Child
                  [I].Name + '[I]);'#13#10;
              end;
              childclasses.Free;
            end
            else
            begin
              Result := Result + '    Add' + Child[I].Name + '.CopyFrom(' + tmpSource + '.' + Child
                [I].Name + '[I]);'#13#10;
            end;
            Result := Result + '  end;'#13#10;
          end;
      end;
    end;
  end;
  Result := Result + //
    '  Self.ToTree;'#13#10 + //
    'end;'#13#10#13#10;
end;

function TXML2CodeClassHelper.CopyFromXMLStatement: string;
begin
  if isAbstract then
  begin
    Result := '    procedure CopyFrom(source: ' + DataType + '); virtual;'#13#10;
    Exit;
  end;
  if ParentClassExsit then
  begin
    Result := '    procedure CopyFrom(source: ' + ParentClass + '); override;'#13#10;
    Exit;
  end;
  Result := '    procedure CopyFrom(source: ' + DataType + ');'#13#10;
end;

function TXML2CodeClassHelper.ToXMLStatement: string;
begin
  Result := '    function ToXML(par: IXMLNode; pt: string = ''''): IXMLNode; override;'#13#10;
  if isAbstract then
    Result := '    function ToXML(par: IXMLNode; pt: string = ''''): IXMLNode; virtual;'#13#10;
end;

function TXML2CodeChildHelper.ChildAddEventImplement: string;
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
            Result := Result + 'procedure ' + ParentClass.DataType + '.Add' + childclasses[I]
              .Name + 'Event(Sender: TObject);'#13#10 + 'begin'#13#10 + '  Add' + childclasses[I]
              .Name + ';'#13#10 + '  F' + Name + '.ToTree;'#13#10 + 'end;'#13#10#13#10;
          end;
        end
        else
        begin
          Result := 'procedure ' + ParentClass.DataType + '.Add' + Name +
            'Event(Sender: TObject);'#13#10 + 'begin'#13#10 + '  Add' + Name + ';'#13#10 + '  F' +
            Name + '.ToTree;'#13#10 + 'end;'#13#10#13#10;
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
            Result := Result + 'procedure ' + ParentClass.DataType + '.Add' + childclasses[I]
              .Name + 'Event(Sender: TObject);'#13#10 + 'var'#13#10 + '  tmp: ' + childclasses[I]
              .DataType + ';'#13#10 + 'begin'#13#10 + '  tmp := Add' + childclasses[I]
              .Name + ';'#13#10 + '  tmp.TreeNodeShape := TreeNodeShape.AddChildObject(''' +
              childclasses[I].Name + ''', tmp);'#13#10 + '  tmp.ToTree;'#13#10 + 'end;'#13#10#13#10;
          end;
        end
        else
        begin
          Result := 'procedure ' + ParentClass.DataType + '.Add' + Name +
            'Event(Sender: TObject);'#13#10 + 'var'#13#10 + '  tmp: ' + DataType + ';'#13#10 +
            'begin'#13#10 + '  tmp := Add' + Name + ';'#13#10 +
            '  tmp.TreeNodeShape := TreeNodeShape.AddChildObject(''' + Name + ''', tmp);'#13#10 +
            '  tmp.ToTree;'#13#10 + 'end;'#13#10#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildAddEventStatement: string;
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
            Result := Result + '    procedure Add' + childclasses[I].Name +
              'Event(Sender: TObject);'#13#10;
          end;
        end
        else
        begin
          Result := '    procedure Add' + Name + 'Event(Sender: TObject);'#13#10;
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
            Result := Result + '    procedure Add' + childclasses[I].Name +
              'Event(Sender: TObject);'#13#10;
          end;
        end
        else
        begin
          Result := '    procedure Add' + Name + 'Event(Sender: TObject);'#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildAddEventMenuImplement: string;
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
              '    ' + childclasses[I].Name + 'AddMenu.OnClick := Add' + childclasses[I]
              .Name + 'Event;'#13#10 + //
              '    ' + Name + 'AddMenu.Add(' + childclasses[I].Name + 'AddMenu);'#13#10;
          end;
        end
        else
        begin
          Result := //
            '    ' + Name + 'AddMenu := TMenuItem.Create(pop);'#13#10 + //
            '    ' + Name + 'AddMenu.Caption := ''Add ' + Name + ''';'#13#10 + //
            '    ' + Name + 'AddMenu.OnClick := Add' + Name + 'Event;'#13#10 + //
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
              '    ' + childclasses[I].Name + 'AddMenu.OnClick := Add' + childclasses[I]
              .Name + 'Event;'#13#10 + //
              '    ' + Name + 'AddMenu.Add(' + childclasses[I].Name + 'AddMenu);'#13#10;

          end;
        end
        else
        begin
          Result := //
            '    ' + Name + 'AddMenu := TMenuItem.Create(pop);'#13#10 + //
            '    ' + Name + 'AddMenu.Caption := ''Add ' + Name + ''';'#13#10 + //
            '    ' + Name + 'AddMenu.OnClick := Add' + Name + 'Event;'#13#10 + //
            '    pop.Items.Add(' + Name + 'AddMenu);'#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildAddEventMenuStatement: string;
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

function TXML2CodeChildHelper.ChildAddImplement: string;
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
            Result := Result + 'function ' + ParentClass.DataType + '.Add' + childclasses[I]
              .Name + ': ' + childclasses[I].DataType + ';'#13#10 + 'begin;'#13#10 +
              '  if not F' + Name + 'Exsit then'#13#10 + '  F' + Name + '.Free;'#13#10 + '  F' +
              Name + ' := ' + childclasses[I].DataType + '.Create(Self);'#13#10 + '  Result := ' +
              childclasses[I].DataType + '(F' + Name + ');'#13#10 + '  F' + Name +
              'Exsit := True;'#13#10 + 'end;'#13#10#13#10;
          end;
        end
        else
        begin
          Result := 'function ' + ParentClass.DataType + '.Add' + Name + ': ' + DataType +
            ';'#13#10 + 'begin;'#13#10 + '  if not F' + Name + 'Exsit then'#13#10 +
            '    F' + Name + ' := ' + DataType + '.Create(Self);'#13#10 + '  Result := F' +
            Name + ';'#13#10 + '  F' + Name + 'Exsit := True;'#13#10 + 'end;'#13#10#13#10;
        end;
      end;
    1:
      ;
    2:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to ChildClass.count - 1 do
          begin
            Result := Result + 'function ' + ParentClass.DataType + '.Add' + childclasses[I]
              .Name + ': ' + childclasses[I].DataType + ';'#13#10 + 'var'#13#10 + '  ' +
              childclasses[I].Name + 'tmp: ' + childclasses[I].DataType + ';'#13#10 +
              'begin;'#13#10 + '  ' + childclasses[I].Name + 'tmp := ' + childclasses
              [I].DataType + '.Create(Self);'#13#10 + '  F' + Name + 's.Add(' + childclasses[I]
              .Name + 'tmp);'#13#10 + '  Result := ' + childclasses[I].Name + 'tmp;'#13#10 +
              'end;'#13#10#13#10;
          end;
        end
        else
        begin
          Result := 'function ' + ParentClass.DataType + '.Add' + Name + ': ' + DataType +
            ';'#13#10 + 'var'#13#10 + '  ' + Name + 'tmp: ' + DataType + ';'#13#10 +
            'begin;'#13#10 + '  ' + Name + 'tmp := ' + DataType + '.Create(Self);'#13#10 + '  F' +
            Name + 's.Add(' + Name + 'tmp);'#13#10 + '  Result := ' + Name + 'tmp;'#13#10 +
            'end;'#13#10#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildAddStatement: string;
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
  end;
end;

function TXML2CodeChildHelper.ChildBaseImplements: string;
begin
  Result := '';
  Result := Result + ChildAddImplement;
  Result := Result + ChildSetImplement;
  Result := Result + ChildClearImplement;
  Result := Result + ChildCountImplement;
  Result := Result + ChildGetIndexImplement;
  Result := Result + ChildSetIndexImplement;
  Result := Result + ChildRemoveImplement;
  Result := Result + ChildDeleteImplement;
  if XML2CodeVisualCode then
  begin
    Result := Result + ChildAddEventImplement;
  end;
end;

function TXML2CodeChildHelper.ChildClass: TList<TXML2CodeClass>;
var
  x2c: TXML2Code;
  x2cclass, tmp: TXML2CodeClass;
  I: Integer;
begin
  x2c := Self.ParentClass.ParentX2C;
  x2cclass := x2c.ClassOf(DataType);
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
    tmp := x2c.ClassOf(x2cclass.ChildClass[I]);
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

function TXML2CodeChildHelper.ChildClearImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.' + Name + 'Clear;'#13#10 +
          'begin'#13#10 + '  while F' + Name + 's.Count > 0 do'#13#10 +
          '  begin'#13#10 + '    F' + Name + 's.Items[0].Free;'#13#10 + '    F' +
          Name + 's.Delete(0);'#13#10 + '  end;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildClearStatement: string;
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

function TXML2CodeChildHelper.ChildCountImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'function ' + ParentClass.DataType + '.' + Name + 'Count: Integer;'#13#10 +
          'begin'#13#10 + '  Result := F' + Name + 's.Count;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildCountStatement: string;
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

function TXML2CodeChildHelper.ChildDeleteImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Delete' + Name +
          '(Index: Integer);'#13#10 + 'begin'#13#10 + '  F' + Name +
          's.Items[Index].Free;'#13#10 + '  F' + Name + 's.Delete(Index);'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildDeleteStatement: string;
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

function TXML2CodeChildHelper.ChildExsitProperty: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    property ' + Name + 'Exsit: Boolean read F' + Name + 'Exsit;'#13#10;
      end;
    1:
      ;
    2:
      ;
  end;
end;

function TXML2CodeChildHelper.ChildFieldStatement: string;
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

function TXML2CodeChildHelper.ChildGetIndexImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'function ' + ParentClass.DataType + '.Get' + Name + '(Index: Integer): ' +
          DataType + ';'#13#10 + 'begin'#13#10 + '  Result := F' + Name + 's[Index];'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildGetIndexStatement: string;
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

function TXML2CodeChildHelper.ChildIndexPropertyStatement: string;
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

function TXML2CodeChildHelper.ChildPrivateStatements: string;
begin
  Result := '';
  Result := Result + Self.ChildFieldStatement;
  Result := Result + Self.ChildSetStatement;
  Result := Result + Self.ChildGetIndexStatement;
  Result := Result + Self.ChildSetIndexStatement;
end;

function TXML2CodeChildHelper.ChildPropertyStatement: string;
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

function TXML2CodeChildHelper.ChildProtectedStatements: string;
begin
  Result := '';
end;

function TXML2CodeChildHelper.ChildPublicStatements: string;
begin
  Result := '';
  Result := Result + Self.ChildAddStatement;
  Result := Result + Self.ChildClearStatement;
  Result := Result + Self.ChildCountStatement;
  Result := Result + Self.ChildRemoveStatement;
  Result := Result + Self.ChildDeleteStatement;
end;

function TXML2CodeChildHelper.ChildRemoveImplement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.' + Name + 'Remove;'#13#10 +
          'begin'#13#10 + '  if F' + Name + 'Exsit then'#13#10 + '  begin'#13#10 +
          '    F' + Name + '.Free;'#13#10 + '    F' + Name + 'Exsit := False;'#13#10 +
          '  end;'#13#10 + 'end;'#13#10#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Remove' + Name + '(_Value: ' + DataType +
          ');'#13#10 + 'begin'#13#10 + '  F' + Name + 's.Remove(_Value);'#13#10 +
          '  _Value.Free;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildRemoveStatement: string;
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

function TXML2CodeChildHelper.ChildSetImplement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name + '(const _Value: ' +
          DataType + ');'#13#10 + 'begin'#13#10 + '  if F' + Name + 'Exsit then'#13#10 +
          '    F' + Name + '.Free;'#13#10 + '  F' + Name + 'Exsit := True;'#13#10 + '  F' + Name +
          ' := _Value;'#13#10 + '  F' + Name + '.Parent := Self;'#13#10 + 'end;'#13#10#13#10;
      end;
    1:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name + '(const _Value: ' +
          DataType + ');'#13#10 + 'begin'#13#10 + '  F' + Name + '.Free;'#13#10 + '  F' +
          Name + ' := _Value;'#13#10 + '  F' + Name + '.Parent := Self;'#13#10 + 'end;'#13#10#13#10;
      end;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name + 's(const _Value: TList<' +
          DataType + '>);'#13#10 + 'begin'#13#10 + '  ' + Name + 'Clear;'#13#10 + '  F' + Name +
          's := _Value;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildSetStatement: string;
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

function TXML2CodeChildHelper.ChildToTree: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '  if ' + Name + 'Exsit then'#13#10 + '  begin'#13#10 + '    ' + Name +
          '.TreeNodeShape := TreeNodeShape.AddChildObject(''' + Name + ''', ' + Name + ');'#13#10 +
          '    ' + Name + '.ToTree;'#13#10 + '  end;'#13#10;
      end;
    1:
      begin
        Result := '  ' + Name + '.TreeNodeShape := TreeNodeShape.AddChildObject(''' + Name +
          ''', ' + Name + ');'#13#10 + '  ' + Name + '.ToTree;'#13#10;
      end;
    2:
      begin
        Result := '  for I := 0 to ' + Name + 'Count - 1 do'#13#10 + // 0
          '  begin'#13#10 + // 1
          '    ' + Name + 's[I].TreeNodeShape := TreeNodeShape.AddChildObject(''' + Name + ''', ' +
          Name + '[I]);'#13#10 + '    ' + Name + '[I].ToTree;'#13#10 + '  end;'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildSetIndexImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name + '(Index: Integer;'#13#10 +
          '  const _Value: ' + DataType + ');'#13#10 + 'begin'#13#10 +
          '  _Value.Parent := Self;'#13#10 + '  F' + Name + 's[Index].Free;'#13#10 + '  F' + Name +
          's[Index] := _Value;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildSetIndexStatement: string;
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

function TXML2CodeChildHelper.ConvertStr: string;
begin
  Result := '';
  case Number of
    0:
      Result := XMLConvertStr(String2XMLType(DataType), 'F' + Name);
    1:
      Result := XMLConvertStr(String2XMLType(DataType), 'F' + Name);
    2:
      Result := XMLConvertStr(String2XMLType(DataType), 'F' + Name + 's.Items[I]');
  end;
end;

function TXML2CodeChildHelper.ConvertXML(_Value: string = 'nodeTmp.Text'): string;
begin
  Result := StrConvertXML(String2XMLType(DataType), _Value);
end;

function TXML2CodeChildHelper.LeafAddEventImplement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Add' + Name +
          'Event(Sender: TObject);'#13#10 + 'begin'#13#10 + '  Add' + Name + ';'#13#10 +
          'end;'#13#10#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Add' + Name +
          'Event(Sender: TObject);'#13#10 + 'begin'#13#10 + '  Add' + Name + ';'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafAddEventStatement: string;
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

function TXML2CodeChildHelper.LeafAddEventMenuImplement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := //
          '    ' + Name + 'AddMenu := TMenuItem.Create(pop);'#13#10 + //
          '    ' + Name + 'AddMenu.Caption := ''Add ' + Name + ''';'#13#10 + //
          '    ' + Name + 'AddMenu.OnClick := Add' + Name + 'Event;'#13#10 + //
          '    pop.Items.Add(' + Name + 'AddMenu);'#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := //
          '    ' + Name + 'AddMenu := TMenuItem.Create(pop);'#13#10 + //
          '    ' + Name + 'AddMenu.Caption := ''Add ' + Name + ''';'#13#10 + //
          '    ' + Name + 'AddMenu.OnClick := Add' + Name + 'Event;'#13#10 + //
          '    pop.Items.Add(' + Name + 'AddMenu);'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafAddEventMenuStatement: string;
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

function TXML2CodeChildHelper.LeafAddImplement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'function ' + ParentClass.DataType + '.Add' + Name + ': ' + LeafType +
          ';'#13#10 + 'begin;'#13#10 + '  Result := F' + Name + ';'#13#10 + '  F' + Name +
          'Exsit := True;'#13#10 + 'end;'#13#10#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := 'function ' + ParentClass.DataType + '.Add' + Name + ': ' + LeafType +
          ';'#13#10 + 'var'#13#10 + Name + 'tmp: ' + LeafType + ';'#13#10 + 'begin;'#13#10 +
          '  F' + Name + 's.Add(' + Name + 'tmp);'#13#10 + '  Result := ' + Name +
          'tmp;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafAddStatement: string;
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
        Result := '    function Add' + Name + ': ' + LeafType + ';'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafBaseImplements: string;
begin
  Result := '';
  Result := Result + LeafAddImplement;
  Result := Result + LeafSetImplement;
  Result := Result + LeafClearImplement;
  Result := Result + LeafCountImplement;
  Result := Result + LeafGetIndexImplement;
  Result := Result + LeafSetIndexImplement;
  Result := Result + LeafRemoveImplement;
  Result := Result + LeafDeleteImplement;
  if XML2CodeVisualCode then
  begin
    Result := Result + LeafAddEventImplement;
  end;
end;

function TXML2CodeChildHelper.LeafClearImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.' + Name + 'Clear;'#13#10 +
          'begin'#13#10 + '  F' + Name + 's.Clear;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafClearStatement: string;
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

function TXML2CodeChildHelper.LeafCountImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'function ' + ParentClass.DataType + '.' + Name + 'Count: Integer;'#13#10 +
          'begin'#13#10 + '  Result := F' + Name + 's.Count;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafCountStatement: string;
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

function TXML2CodeChildHelper.LeafDeleteImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Delete' + Name +
          '(Index: Integer);'#13#10 + 'begin'#13#10 + '  F' + Name +
          's.Delete(Index);'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafDeleteStatement: string;
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

function TXML2CodeChildHelper.LeafExsitProperty: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    property ' + Name + 'Exsit: Boolean read F' + Name + 'Exsit;'#13#10;
      end;
    1:
      ;
    2:
      ;
  end;
end;

function TXML2CodeChildHelper.LeafFieldStatement: string;
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

function TXML2CodeChildHelper.LeafGetIndexImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'function ' + ParentClass.DataType + '.Get' + Name + '(Index: Integer): ' +
          LeafType + ';'#13#10 + 'begin'#13#10 + '  Result := F' + Name + 's[Index];'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafGetIndexStatement: string;
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

function TXML2CodeChildHelper.LeafIndexPropertyStatement: string;
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

function TXML2CodeChildHelper.LeafPrivateStatements: string;
begin
  Result := '';
  Result := Result + Self.LeafFieldStatement;
  Result := Result + Self.LeafSetStatement;
  Result := Result + Self.LeafGetIndexStatement;
  Result := Result + Self.LeafSetIndexStatement;
end;

function TXML2CodeChildHelper.LeafPropertyStatement: string;
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

function TXML2CodeChildHelper.LeafProtectedStatements: string;
begin
  Result := '';
end;

function TXML2CodeChildHelper.LeafPublicStatements: string;
begin
  Result := '';
  Result := Result + Self.LeafAddStatement;
  Result := Result + Self.LeafClearStatement;
  Result := Result + Self.LeafCountStatement;
  Result := Result + Self.LeafRemoveStatement;
  Result := Result + Self.LeafDeleteStatement;
end;

function TXML2CodeChildHelper.LeafRemoveImplement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.' + Name + 'Remove;'#13#10 +
          'begin'#13#10 + '  if F' + Name + 'Exsit then'#13#10 + '  begin'#13#10 +
          '    F' + Name + 'Exsit := False;'#13#10 + '  end;'#13#10 + 'end;'#13#10#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Remove' + Name + '(_Value: ' + LeafType +
          ');'#13#10 + 'begin'#13#10 + '  F' + Name + 's.Remove(_Value);'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafRemoveStatement: string;
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

function TXML2CodeChildHelper.LeafSetImplement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name + '(const _Value: ' +
          LeafType + ');'#13#10 + 'begin'#13#10 + '  F' + Name + 'Exsit := True;'#13#10 +
          '  F' + Name + ' := _Value;'#13#10 + 'end;'#13#10#13#10;
      end;
    1:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name + '(const _Value: ' +
          LeafType + ');'#13#10 + 'begin'#13#10 + '  F' + Name + ' := _Value;'#13#10 +
          'end;'#13#10#13#10;
      end;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name + 's(const _Value: TList<' +
          LeafType + '>);'#13#10 + 'begin'#13#10 + '  F' + Name + 's.Clear;'#13#10 + '  F' + Name +
          's := _Value;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafSetStatement: string;
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

function TXML2CodeChildHelper.LeafSetXMLPropertyImplement(var Index: Integer): string;
begin
  Result := '';
  if Visual then
  begin
    case Number of
      0:
        begin
          Result := Result + '    ' + IntToStr(Index)
            + ':'#13#10 + '      begin'#13#10 + '        ' + Name + ' := ' + StrConvertXML
            (String2XMLType(DataType), '_Value') + ';'#13#10 + '      end;'#13#10;
          Inc(Index);
        end;
      1:
        begin
          Result := Result + '    ' + IntToStr(Index)
            + ':'#13#10 + '      begin'#13#10 + '        ' + Name + ' := ' + StrConvertXML
            (String2XMLType(DataType), '_Value') + ';'#13#10 + '      end;'#13#10;
          Inc(Index);
        end;
      2:
        begin
        end;
    end;
  end;
end;

function TXML2CodeChildHelper.LeafSetIndexImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name + '(Index: Integer;'#13#10 +
          '  const _Value: ' + LeafType + ');'#13#10 + 'begin'#13#10 + '  F' + Name +
          's[Index] := _Value;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafSetIndexStatement: string;
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

function TXML2CodeChildHelper.LeafToInspectorImplement: string;
begin
  Result := '';
  if Visual then
  begin
    case Number of
      0:
        begin
          Result := Result + '  Names_Value.Add(''' + Name + ''');'#13#10 + '  Types_Value.Add(' +
            XMLTypeStrings[String2XMLType(DataType)] + ');'#13#10 + '  _Values_Value.Add(' +
            XMLConvertStr(String2XMLType(DataType), Name) + ');'#13#10;
        end;
      1:
        begin
          Result := Result + '  Names_Value.Add(''' + Name + ''');'#13#10 + '  Types_Value.Add(' +
            XMLTypeStrings[String2XMLType(DataType)] + ');'#13#10 + '  _Values_Value.Add(' +
            XMLConvertStr(String2XMLType(DataType), Name) + ');'#13#10;
        end;
      2:
        begin
        end;
    end;
  end;
end;

function TXML2CodeChildHelper.LeafToTree: string;
begin
  if not Visual then
  begin
    Result := '';
    Exit;
  end;
  case Number of
    0:
      begin
        Result := '  if ' + Name + 'Exsit then'#13#10 + //
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

function TXML2CodeChildHelper.LeafType: string;
begin
  Result := XMLType2PascalType(String2XMLType(DataType));
end;

function TXML2CodeChildHelper.NodeType: TNodeType;
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

function TXML2CodeChildHelper.ParentClass: TXML2CodeClass;
begin
  Result := TXML2CodeClass(Self.Parent);
end;

function TXML2CodeChildHelper.ReadAttribute: string;
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
          '      begin'#13#10 + '        F' + Name + ' := ' + Self.ConvertXML + ';'#13#10 +
          '        F' + Name + 'Exsit := True;'#13#10 + '      end'#13#10;
      1:
        Result := '      else if nodeTmp.NodeName = ''' + tmp + ''' then'#13#10 +
          '      begin'#13#10 + '        F' + Name + ' := ' + Self.ConvertXML + ';'#13#10 +
          '      end'#13#10;
      2:
        ;
    end;
  end;
end;

function TXML2CodeChildHelper.ReadChild: string;
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
                  '      begin'#13#10 + '        F' + Name + ' := ' + Self.ConvertXML + ';'#13#10 +
                  '        F' + Name + 'Exsit := True;'#13#10 + '      end'#13#10;
              end;
            1:
              begin
                Result := '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 +
                  '      begin'#13#10 + '        F' + Name + ' := ' + Self.ConvertXML + ';'#13#10 +
                  '      end'#13#10;
              end;
            2:
              begin
                Result := '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 +
                  '      begin'#13#10 + '        F' + Name + 's.Add(' + Self.ConvertXML +
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
                    Result := Result + '      else if nodeTmp.NodeName = ''' + childclasses[I]
                      .Path + ''' then'#13#10 + '      begin'#13#10 + '        F' + Name +
                      ' := ' + childclasses[I].DataType + '.Create(Self);'#13#10 + '        F' +
                      Name + '.FromXML(nodeTmp);'#13#10 + '        F' + Name +
                      'Exsit := True;'#13#10 + '      end'#13#10;
                  end;
                  childclasses.Free;
                end
                else
                begin
                  Result := '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 +
                    '      begin'#13#10 + '        F' + Name + ' := ' + DataType +
                    '.Create(Self);'#13#10 + '        F' + Name + '.FromXML(nodeTmp);'#13#10 +
                    '        F' + Name + 'Exsit := True;'#13#10 + '      end'#13#10;
                end;
              end;

            1:
              begin
                if isVirtual then
                begin
                  childclasses := Self.ChildClass;
                  for I := 0 to childclasses.count - 1 do
                  begin
                    Result := Result + '      else if nodeTmp.NodeName = ''' + childclasses[I]
                      .Path + ''' then'#13#10 + '      begin'#13#10 + '        F' + Name +
                      ' := ' + childclasses[I].DataType + '.Create(Self);'#13#10 + '        F' +
                      Name + '.FromXML(nodeTmp);'#13#10 + '      end'#13#10;
                  end;
                  childclasses.Free;
                end
                else
                begin
                  Result := '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 +
                    '      begin'#13#10 + '        F' + Name + ' := ' + DataType +
                    '.Create(Self);'#13#10 + '        F' + Name + '.FromXML(nodeTmp);'#13#10 +
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
                    Result := Result + '      else if nodeTmp.NodeName = ''' + childclasses[I]
                      .Path + ''' then'#13#10 + '      begin'#13#10 + '        ' + Name +
                      'Tmp := ' + childclasses[I].DataType + '.Create(Self);'#13#10 + '        ' +
                      Name + 'Tmp.FromXML(nodeTmp);'#13#10 + '        F' + Name + 's.Add(' + Name +
                      'Tmp);'#13#10 + '      end'#13#10;
                  end;
                  childclasses.Free;
                end
                else
                begin
                  Result := '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 +
                    '      begin'#13#10 + '        ' + Name + 'Tmp := ' + DataType +
                    '.Create(Self);'#13#10 + '        ' + Name + 'Tmp.FromXML(nodeTmp);'#13#10 +
                    '        F' + Name + 's.Add(' + Name + 'Tmp);'#13#10 + '      end'#13#10;
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
                '      begin'#13#10 + '        F' + Name + ' := ' + Self.ConvertXML + ';'#13#10 +
                '      end'#13#10;
            end;
          1:
            begin
              Result := '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 +
                '      begin'#13#10 + '        F' + Name + ' := ' + Self.ConvertXML + ';'#13#10 +
                '      end'#13#10;
            end;
          2:
            begin
              Result := '      else if nodeTmp.NodeName = ''' + Path + ''' then'#13#10 +
                '      begin'#13#10 + '        F' + Name + 's.Add(' + ConvertXML + ');'#13#10 +
                '      end'#13#10;
            end;
        end;
      end;
  end;
end;

function TXML2CodeChildHelper.ReadText: string;
begin
  Result := '';
  if NodeType = ntText then
  begin
    case Number of
      0:
        begin
          Result := '    F' + Name + ' := ' + Self.ConvertXML('node.Text') + ';'#13#10;
        end;
      1:
        begin
          Result := '    F' + Name + ' := ' + Self.ConvertXML('node.Text') + ';'#13#10;
        end;
      2:
        begin
        end;
    end;
  end;
end;

function TXML2CodeChildHelper.WriteAttribute: string;
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
        Result := '    if F' + Name + 'Exsit then '#13#10 + '    begin'#13#10 + '      ' + Name +
          'Tmp := doc.CreateNode(''' + tmp + ''', ntAttribute);'#13#10 + '      ' + Name +
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

function TXML2CodeChildHelper.WriteChild: string;
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
                Result := '    if F' + Name + 'Exsit then'#13#10 + '    begin'#13#10 + '      ' +
                  Name + 'Tmp := doc.CreateNode(''' + Path + ''', ntElement);'#13#10 + '      ' +
                  Name + 'Tmp.NodeValue := ' + ConvertStr + ';'#13#10 +
                  '      node.ChildNodes.Add(' + Name + 'Tmp);'#13#10 +
                  '    end;'#13#10;
              end;
            1:
              begin
                Result := '    ' + Name + 'Tmp := doc.CreateNode(''' + Path +
                  ''', ntElement);'#13#10 + '    ' + Name + 'Tmp.NodeValue := ' + ConvertStr +
                  ';'#13#10 + '    node.ChildNodes.Add(' + Name + 'Tmp);'#13#10;
              end;
            2:
              begin
                Result := '    for I := 0 to F' + Name + 's.Count - 1 do'#13#10 + 'begin'#13#10 +
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
                Result := '    if F' + Name + 'Exsit then'#13#10 + '      F' + Name +
                  '.ToXML(node, ''' + Path + ''');'#13#10;
              end;
            1:
              begin
                Result := '    F' + Name + '.ToXML(node, ''' + Path + ''');'#13#10;
              end;
            2:
              begin
                Result := '    for I := 0 to F' + Name + 's.Count - 1 do'#13#10 + '       F' +
                  Name + 's.Items[I].ToXML(node, ''' + Path + ''');'#13#10;
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
              Result := '    if F' + Name + 'Exsit then'#13#10 + 'begin'#13#10 + '      ' + Name +
                'Tmp := doc.CreateNode(''' + tmp + ''', ntCData);'#13#10 + '      ' + Name +
                'Tmp.NodeValue := ' + ConvertStr + ';'#13#10 + '      node.ChildNodes.Add(' +
                Name + 'Tmp);'#13#10 + '    end;'#13#10;
            end;
          1:
            begin
              Result := '    ' + Name + 'Tmp := doc.CreateNode(''' + tmp + ''', ntCData);'#13#10 +
                '    ' + Name + 'Tmp.NodeValue := ' + ConvertStr + ';'#13#10 +
                '    node.ChildNodes.Add(' + Name + 'Tmp);'#13#10;
            end;
          2:
            begin
              Result := '    for I := 0 to F' + Name + 's.Count - 1 do'#13#10 + 'begin'#13#10 +
                '      ' + Name + 'Tmp := doc.CreateNode(''' + tmp + ''', ntCData);'#13#10 +
                '      ' + Name + 'Tmp.NodeValue := ' + ConvertStr + ';'#13#10 +
                '      node.ChildNodes.Add(' + Name + 'Tmp);'#13#10 + '    end;'#13#10;
            end;
        end;
      end;
  end;
end;

function TXML2CodeChildHelper.WriteText: string;
begin
  Result := '';
  if NodeType = ntText then
  begin
    case Number of
      0:
        begin
          Result := '    if F' + Name + 'Exsit then'#13#10 + '    begin'#13#10 +
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

end.
