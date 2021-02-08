unit CMLTree;

interface

uses
  Windows, SysUtils, Types, Classes, Variants, Generics.Collections, Dialogs, Controls, ExtCtrls,
  XMLCore, XMLDoc, XMLIntf, XMLLeafTypes, TeeTree, Menus, XMLInspector,XML2Code;


type

  TCMLTree = class;

  TCMLTree = class(TXMLTree)
  private
    FTXML2CodeObj: TXML2Code;
    procedure SetObj(const Value: TXML2Code);
  protected
    { Inherited from TXML }
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    function ToGeoObj: TObject; override;
    procedure FromGeoObj(const Obj: TObject); override;
    { TXML2Code }
    procedure TXML2Code_FromXML(Obj: TXML2Code; node: IXMLNode);
    procedure TXML2Code_ToXML(Obj: TXML2Code; par: IXMLNode; pt: string = 'XML2Code');
    { TXML2CodeClass }
    procedure TXML2CodeClass_FromXML(Obj: TXML2CodeClass; node: IXMLNode);
    procedure TXML2CodeClass_ToXML(Obj: TXML2CodeClass; par: IXMLNode; pt: string = 'Class');
    { TXML2CodeChild }
    procedure TXML2CodeChild_FromXML(Obj: TXML2CodeChild; node: IXMLNode);
    procedure TXML2CodeChild_ToXML(Obj: TXML2CodeChild; par: IXMLNode; pt: string = 'Child');
  public
    { Inherited from TXMLTree }
    procedure ToTree(TreeNodeShape: TTreeNodeShape; obj: TObject = nil); override;
    procedure SetXMLProperty(obj: TObject; Index: Integer; _Value: String); override;
    procedure ToPopupMenu(Sender: TObject; obj: TObject = nil); override;
    procedure ToInspector(Sender: TObject; obj: TObject = nil); override;
    procedure ChildDeleteEvent(par, del: TObject); override;
    procedure PasteToEvent(items: TList<TObject>; const isCut: Boolean = False); override;
    { TXML2Code }
    procedure TXML2Code_ToTree(Obj: TXML2Code; TreeNodeShape: TTreeNodeShape);
    procedure TXML2Code_ToPopupMenu(obj: TXML2Code; Sender: TObject);
    procedure TXML2Code_ToInspector(obj: TXML2Code; ins: TObject);
    procedure TXML2Code_SetXMLProperty(obj: TXML2Code; Index: Integer; _Value: String);
    procedure TXML2Code_Copy(source, target: TXML2Code);
    procedure TXML2Code_PasteToEvent(obj: TXML2Code;items: TList<TObject>; const isCut: Boolean = False);
    procedure TXML2Code_ChildDeleteEvent(obj: TXML2Code; del: TObject);
    procedure TXML2Code_AddXMLClassEvent(Sender: TObject);
    procedure TXML2Code_ClassDeleteEvent(obj: TXML2Code; del: TObject);
    procedure TXML2Code_AddUsingEvent(Sender: TObject);
    { TXML2CodeClass }
    procedure TXML2CodeClass_ToTree(Obj: TXML2CodeClass; TreeNodeShape: TTreeNodeShape);
    procedure TXML2CodeClass_ToPopupMenu(obj: TXML2CodeClass; Sender: TObject);
    procedure TXML2CodeClass_ToInspector(obj: TXML2CodeClass; ins: TObject);
    procedure TXML2CodeClass_SetXMLProperty(obj: TXML2CodeClass; Index: Integer; _Value: String);
    procedure TXML2CodeClass_Copy(source, target: TXML2CodeClass);
    procedure TXML2CodeClass_PasteToEvent(obj: TXML2CodeClass;items: TList<TObject>; const isCut: Boolean = False);
    procedure TXML2CodeClass_ChildDeleteEvent(obj: TXML2CodeClass; del: TObject);
    procedure TXML2CodeClass_AddChildEvent(Sender: TObject);
    procedure TXML2CodeClass_ClassDeleteEvent(obj: TXML2CodeClass; del: TObject);
    procedure TXML2CodeClass_AddParentClassEvent(Sender: TObject);
    procedure TXML2CodeClass_AddChildClassEvent(Sender: TObject);
    procedure TXML2CodeClass_AddisAbstractEvent(Sender: TObject);
    procedure TXML2CodeClass_AddPathEvent(Sender: TObject);
    { TXML2CodeChild }
    procedure TXML2CodeChild_ToTree(Obj: TXML2CodeChild; TreeNodeShape: TTreeNodeShape);
    procedure TXML2CodeChild_ToPopupMenu(obj: TXML2CodeChild; Sender: TObject);
    procedure TXML2CodeChild_ToInspector(obj: TXML2CodeChild; ins: TObject);
    procedure TXML2CodeChild_SetXMLProperty(obj: TXML2CodeChild; Index: Integer; _Value: String);
    procedure TXML2CodeChild_Copy(source, target: TXML2CodeChild);
    procedure TXML2CodeChild_PasteToEvent(obj: TXML2CodeChild;items: TList<TObject>; const isCut: Boolean = False);
    procedure TXML2CodeChild_ChildDeleteEvent(obj: TXML2CodeChild; del: TObject);
    procedure TXML2CodeChild_AddisVirtualEvent(Sender: TObject);
    property TXML2CodeObj: TXML2Code read FTXML2CodeObj write SetObj;
  end;

implementation

procedure TCMLTree.FromXML(node: IXMLNode);
begin
  inherited;
  if not Assigned(FTXML2CodeObj) then
    FTXML2CodeObj := TXML2Code.Create;
  TXML2Code_FromXML(FTXML2CodeObj, node);
end;


function TCMLTree.ToXML(par: IXMLNode; pt: string): IXMLNode;
begin
  if Assigned(FTXML2CodeObj) then
    TXML2Code_ToXML(FTXML2CodeObj, par, pt);
end;


procedure TCMLTree.FromGeoObj(const Obj: TObject);
begin
  inherited;
  FTXML2CodeObj := TXML2Code(Obj);
end;


function TCMLTree.ToGeoObj: TObject;
begin
  Result := FTXML2CodeObj;
end;


procedure TCMLTree.SetObj(const Value: TXML2Code);
begin
  FTXML2CodeObj := Value;
end;


procedure TCMLTree.ToTree(TreeNodeShape: TTreeNodeShape; obj: TObject = nil);
var
  tar: TObject;
begin
  inherited;
  if obj <> nil then
  begin
    tar := obj;
  end
  else
  begin
    tar := TargetObject;
  end;
  if tar is TCMLTree then
  begin
    TXML2Code_ToTree(TCMLTree(tar).FTXML2CodeObj, TreeNodeShape);
  end
  else if tar is TXML2Code then
  begin
    TXML2Code_ToTree(TXML2Code(tar), TreeNodeShape);
  end  else if tar is TXML2CodeClass then
  begin
    TXML2CodeClass_ToTree(TXML2CodeClass(tar), TreeNodeShape);
  end  else if tar is TXML2CodeChild then
  begin
    TXML2CodeChild_ToTree(TXML2CodeChild(tar), TreeNodeShape);
  end;
end;

procedure TCMLTree.SetXMLProperty(obj: TObject; Index: Integer; _Value: String);
begin
  inherited;
  if obj is TXML2Code then
  begin
    TXML2Code_SetXMLProperty(TXML2Code(obj), Index, _Value);
	  Exit;
  end;
  if obj is TXML2CodeClass then
  begin
    TXML2CodeClass_SetXMLProperty(TXML2CodeClass(obj), Index, _Value);
	  Exit;
  end;
  if obj is TXML2CodeChild then
  begin
    TXML2CodeChild_SetXMLProperty(TXML2CodeChild(obj), Index, _Value);
	  Exit;
  end;
end;

procedure TCMLTree.ToPopupMenu(Sender: TObject; obj: TObject = nil);
var
  tar: TObject;
begin
  inherited;
  if obj <> nil then
  begin
    tar := obj;
  end
  else
  begin
    tar := TargetObject;
  end;
  if tar is TXML2Code then
  begin
    TXML2Code_ToPopupMenu(TXML2Code(tar), Sender);
	  Exit;
  end;
  if tar is TXML2CodeClass then
  begin
    TXML2CodeClass_ToPopupMenu(TXML2CodeClass(tar), Sender);
	  Exit;
  end;
  if tar is TXML2CodeChild then
  begin
    TXML2CodeChild_ToPopupMenu(TXML2CodeChild(tar), Sender);
	  Exit;
  end;
end;

procedure TCMLTree.ToInspector(Sender: TObject; obj: TObject = nil);
var
  tar: TObject;
begin
  inherited;
  if obj <> nil then
  begin
    tar := obj;
  end
  else
  begin
    tar := TargetObject;
  end;
  if tar is TXML2Code then
  begin
    TXML2Code_ToInspector(TXML2Code(tar), Sender);
	  Exit;
  end;
  if tar is TXML2CodeClass then
  begin
    TXML2CodeClass_ToInspector(TXML2CodeClass(tar), Sender);
	  Exit;
  end;
  if tar is TXML2CodeChild then
  begin
    TXML2CodeChild_ToInspector(TXML2CodeChild(tar), Sender);
	  Exit;
  end;
end;

procedure TCMLTree.ChildDeleteEvent(par, del: TObject);
begin
  if TargetObject is TXML2Code then
  begin
    TXML2Code_ChildDeleteEvent(TXML2Code(par), del);
	  Exit;
  end;
  if TargetObject is TXML2CodeClass then
  begin
    TXML2CodeClass_ChildDeleteEvent(TXML2CodeClass(par), del);
	  Exit;
  end;
  if TargetObject is TXML2CodeChild then
  begin
    TXML2CodeChild_ChildDeleteEvent(TXML2CodeChild(par), del);
	  Exit;
  end;
end;

procedure TCMLTree.PasteToEvent(items: TList<TObject>; const isCut: Boolean = False);
begin
  if TargetObject is TXML2Code then
  begin
    TXML2Code_PasteToEvent(TXML2Code(TargetObject), items, isCut);
	  Exit;
  end;
  if TargetObject is TXML2CodeClass then
  begin
    TXML2CodeClass_PasteToEvent(TXML2CodeClass(TargetObject), items, isCut);
	  Exit;
  end;
  if TargetObject is TXML2CodeChild then
  begin
    TXML2CodeChild_PasteToEvent(TXML2CodeChild(TargetObject), items, isCut);
	  Exit;
  end;
end;

procedure TCMLTree.TXML2Code_FromXML(Obj: TXML2Code; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  XMLClassTmp: TXML2CodeClass;
begin
  try
    Obj.XMLClassClear;
    Obj.UsingExsit := False;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'XMLClass' then
      begin
        XMLClassTmp := TXML2CodeClass.Create;
        TXML2CodeClass_FromXML(XMLClassTmp, nodeTmp);
        Obj.AddXMLClass(XMLClassTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        Obj.Name := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'type' then
      begin
        Obj.Datatype := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'target' then
      begin
        Obj.Target := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'targetUnit' then
      begin
        Obj.TargetUnit := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'using' then
      begin
        Obj.Using := String2String1D(nodeTmp.Text);
        Obj.UsingExsit := True;
      end;
    end;
  except
    raise Exception.Create('XML2Code Read XML Error!' + node.Xml);
  end;
end;

procedure TCMLTree.TXML2Code_ToXML(Obj: TXML2Code; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  DatatypeTmp: IXMLNode;
  TargetTmp: IXMLNode;
  TargetUnitTmp: IXMLNode;
  UsingTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'XML2Code';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    for I := 0 to Obj.XMLClasss.Count - 1 do
       TXML2CodeClass_ToXML(Obj.XMLClass[I], node, 'XMLClass');
    NameTmp := doc.CreateNode('name', ntAttribute);
    NameTmp.NodeValue := Obj.Name;
    node.AttributeNodes.Add(NameTmp);
    DatatypeTmp := doc.CreateNode('type', ntAttribute);
    DatatypeTmp.NodeValue := Obj.Datatype;
    node.AttributeNodes.Add(DatatypeTmp);
    TargetTmp := doc.CreateNode('target', ntAttribute);
    TargetTmp.NodeValue := Obj.Target;
    node.AttributeNodes.Add(TargetTmp);
    TargetUnitTmp := doc.CreateNode('targetUnit', ntAttribute);
    TargetUnitTmp.NodeValue := Obj.TargetUnit;
    node.AttributeNodes.Add(TargetUnitTmp);
    if Obj.UsingExsit then
    begin
      UsingTmp := doc.CreateNode('using', ntAttribute);
      UsingTmp.NodeValue := String1D2String(Obj.Using);
      node.AttributeNodes.Add(UsingTmp);
    end;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TCMLTree.TXML2CodeClass_FromXML(Obj: TXML2CodeClass; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  ChildTmp: TXML2CodeChild;
begin
  try
    Obj.ChildClear;
    Obj.ParentClassExsit := False;
    Obj.ChildClassExsit := False;
    Obj.isAbstractExsit := False;
    Obj.PathExsit := False;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Child' then
      begin
        ChildTmp := TXML2CodeChild.Create;
        TXML2CodeChild_FromXML(ChildTmp, nodeTmp);
        Obj.AddChild(ChildTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'type' then
      begin
        Obj.DataType := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'Root' then
      begin
        Obj.Root := String2Boolean(nodeTmp.Text);
      end
      else if nodeTmp.NodeName = 'name' then
      begin
        Obj.Name := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'parentClass' then
      begin
        Obj.ParentClass := nodeTmp.Text;
        Obj.ParentClassExsit := True;
      end
      else if nodeTmp.NodeName = 'childClass' then
      begin
        Obj.ChildClass := String2String1D(nodeTmp.Text);
        Obj.ChildClassExsit := True;
      end
      else if nodeTmp.NodeName = 'abstract' then
      begin
        Obj.isAbstract := String2Boolean(nodeTmp.Text);
        Obj.isAbstractExsit := True;
      end
      else if nodeTmp.NodeName = 'path' then
      begin
        Obj.Path := nodeTmp.Text;
        Obj.PathExsit := True;
      end;
    end;
  except
    raise Exception.Create('Class Read XML Error!' + node.Xml);
  end;
end;

procedure TCMLTree.TXML2CodeClass_ToXML(Obj: TXML2CodeClass; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  DataTypeTmp: IXMLNode;
  RootTmp: IXMLNode;
  NameTmp: IXMLNode;
  ParentClassTmp: IXMLNode;
  ChildClassTmp: IXMLNode;
  isAbstractTmp: IXMLNode;
  PathTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Class';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    for I := 0 to Obj.Childs.Count - 1 do
       TXML2CodeChild_ToXML(Obj.Child[I], node, 'Child');
    DataTypeTmp := doc.CreateNode('type', ntAttribute);
    DataTypeTmp.NodeValue := Obj.DataType;
    node.AttributeNodes.Add(DataTypeTmp);
    RootTmp := doc.CreateNode('Root', ntAttribute);
    RootTmp.NodeValue := Boolean2String(Obj.Root);
    node.AttributeNodes.Add(RootTmp);
    NameTmp := doc.CreateNode('name', ntAttribute);
    NameTmp.NodeValue := Obj.Name;
    node.AttributeNodes.Add(NameTmp);
    if Obj.ParentClassExsit then
    begin
      ParentClassTmp := doc.CreateNode('parentClass', ntAttribute);
      ParentClassTmp.NodeValue := Obj.ParentClass;
      node.AttributeNodes.Add(ParentClassTmp);
    end;
    if Obj.ChildClassExsit then
    begin
      ChildClassTmp := doc.CreateNode('childClass', ntAttribute);
      ChildClassTmp.NodeValue := String1D2String(Obj.ChildClass);
      node.AttributeNodes.Add(ChildClassTmp);
    end;
    if Obj.isAbstractExsit then
    begin
      isAbstractTmp := doc.CreateNode('abstract', ntAttribute);
      isAbstractTmp.NodeValue := Boolean2String(Obj.isAbstract);
      node.AttributeNodes.Add(isAbstractTmp);
    end;
    if Obj.PathExsit then
    begin
      PathTmp := doc.CreateNode('path', ntAttribute);
      PathTmp.NodeValue := Obj.Path;
      node.AttributeNodes.Add(PathTmp);
    end;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TCMLTree.TXML2CodeChild_FromXML(Obj: TXML2CodeChild; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    Obj.isVirtualExsit := False;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        Obj.Name := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'type' then
      begin
        Obj.DataType := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'path' then
      begin
        Obj.Path := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'number' then
      begin
        Obj.Number := StrToIntDef(nodeTmp.Text, 0);;
      end
      else if nodeTmp.NodeName = 'leaf' then
      begin
        Obj.Leaf := String2Boolean(nodeTmp.Text);
      end
      else if nodeTmp.NodeName = 'visual' then
      begin
        Obj.Visual := String2Boolean(nodeTmp.Text);
      end
      else if nodeTmp.NodeName = 'virtual' then
      begin
        Obj.isVirtual := String2Boolean(nodeTmp.Text);
        Obj.isVirtualExsit := True;
      end;
    end;
  except
    raise Exception.Create('Child Read XML Error!' + node.Xml);
  end;
end;

procedure TCMLTree.TXML2CodeChild_ToXML(Obj: TXML2CodeChild; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  DataTypeTmp: IXMLNode;
  PathTmp: IXMLNode;
  NumberTmp: IXMLNode;
  LeafTmp: IXMLNode;
  VisualTmp: IXMLNode;
  isVirtualTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Child';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    NameTmp := doc.CreateNode('name', ntAttribute);
    NameTmp.NodeValue := Obj.Name;
    node.AttributeNodes.Add(NameTmp);
    DataTypeTmp := doc.CreateNode('type', ntAttribute);
    DataTypeTmp.NodeValue := Obj.DataType;
    node.AttributeNodes.Add(DataTypeTmp);
    PathTmp := doc.CreateNode('path', ntAttribute);
    PathTmp.NodeValue := Obj.Path;
    node.AttributeNodes.Add(PathTmp);
    NumberTmp := doc.CreateNode('number', ntAttribute);
    NumberTmp.NodeValue := IntToStr(Obj.Number);
    node.AttributeNodes.Add(NumberTmp);
    LeafTmp := doc.CreateNode('leaf', ntAttribute);
    LeafTmp.NodeValue := Boolean2String(Obj.Leaf);
    node.AttributeNodes.Add(LeafTmp);
    VisualTmp := doc.CreateNode('visual', ntAttribute);
    VisualTmp.NodeValue := Boolean2String(Obj.Visual);
    node.AttributeNodes.Add(VisualTmp);
    if Obj.isVirtualExsit then
    begin
      isVirtualTmp := doc.CreateNode('virtual', ntAttribute);
      isVirtualTmp.NodeValue := Boolean2String(Obj.isVirtual);
      node.AttributeNodes.Add(isVirtualTmp);
    end;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

    { TXML2Code }
procedure TCMLTree.TXML2Code_ToTree(Obj: TXML2Code; TreeNodeShape: TTreeNodeShape);
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.Data := Obj;
  TreeNodeShape.Text.Clear;
  TreeNodeShape.Text.Add('XML2Code');
  for I := 0 to obj.XMLClassCount - 1 do
  begin
    TXML2CodeClass_ToTree(obj.XMLClass[I], TreeNodeShape.AddChildObject('XMLClass', obj.XMLClass[I]));
  end;
  TreeNodeShape.AddChild('Name:' + Obj.Name);
  TreeNodeShape.AddChild('Datatype:' + Obj.Datatype);
  TreeNodeShape.AddChild('Target:' + Obj.Target);
  TreeNodeShape.AddChild('TargetUnit:' + Obj.TargetUnit);
  if Obj.UsingExsit then
    TreeNodeShape.AddChild('Using:' + String1D2String(Obj.Using));
end;

procedure TCMLTree.TXML2Code_ToPopupMenu(obj: TXML2Code; Sender: TObject);
var
  pop: TPopupMenu;
  XMLClassAddMenu: TMenuItem;
  UsingAddMenu: TMenuItem;
begin
  if Assigned(Sender) and (Sender is TPopupMenu) then
  begin
    pop := TPopupMenu(Sender);
    pop.Items.Clear;
    XMLClassAddMenu := TMenuItem.Create(pop);
    XMLClassAddMenu.Caption := 'Add XMLClass';
    XMLClassAddMenu.OnClick := TXML2Code_AddXMLClassEvent;
    pop.Items.Add(XMLClassAddMenu);
    UsingAddMenu := TMenuItem.Create(pop);
    UsingAddMenu.Caption := 'Add Using';
    UsingAddMenu.OnClick := TXML2Code_AddUsingEvent;
    pop.Items.Add(UsingAddMenu);
  end;
end;

procedure TCMLTree.TXML2Code_ToInspector(obj: TXML2Code; ins: TObject);
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(ins) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Obj.Name);
  Names_Value.Add('Datatype');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Obj.Datatype);
  Names_Value.Add('Target');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Obj.Target);
  Names_Value.Add('TargetUnit');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Obj.TargetUnit);
  Names_Value.Add('Using');
  Types_Value.Add(xml_String1D);
  _Values_Value.Add(String1D2String(Obj.Using));
  TXMLInspector(ins).SetData(Names_Value, _Values_Value, Types_Value, Obj, Self);
end;

procedure TCMLTree.TXML2Code_SetXMLProperty(obj: TXML2Code; Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        obj.Name := _Value;
      end;
    1:
      begin
        obj.Datatype := _Value;
      end;
    2:
      begin
        obj.Target := _Value;
      end;
    3:
      begin
        obj.TargetUnit := _Value;
      end;
    4:
      begin
        obj.Using := String2String1D(_Value);
      end;
  end;
  TXML2Code_ToTree(obj, TargetNode);
end;

procedure TCMLTree.TXML2Code_Copy(source, target: TXML2Code);
var
  I:Integer;
begin
  target.XMLClassClear;
  for I := 0 to source.XMLClassCount - 1 do
  begin
      TXML2CodeClass_Copy(source.XMLClass[I], target. AddNewXMLClass);
  end;
  target.Name := source.Name;
  target.Datatype := source.Datatype;
  target.Target := source.Target;
  target.TargetUnit := source.TargetUnit;
  target.Using := source.Using;
  target.UsingExsit := source.UsingExsit;
  TXML2Code_ToTree(target, TargetNode);
end;

procedure TCMLTree.TXML2Code_PasteToEvent(obj: TXML2Code; items: TList<TObject>; const isCut: Boolean = False);
var
  I: Integer;
  tmpXMLClass: TXML2CodeClass;
begin
  if (items.Count = 1) and (items[0] is TXML2Code) and (not isCut) then
  begin
    TXML2Code_Copy(TXML2Code(items[0]), obj);
    TXML2Code_ToTree(obj, TargetNode);
    Exit;
  end;
  for I := 0 to items.Count - 1 do
  begin
    if items[I] is TXML2CodeClass then
    begin
      tmpXMLClass := TXML2CodeClass(items[I]);
      if isCut then
      begin
      end
      else
      begin
        TXML2CodeClass_Copy(tmpXMLClass, obj.AddNewXMLClass);
      end;
    end;
  end;
  TXML2Code_ToTree(obj, TargetNode);
end;

procedure TCMLTree.TXML2Code_ChildDeleteEvent(obj: TXML2Code; del: TObject);
begin
  if del is TXML2CodeClass then
  begin
    TXML2Code(obj).RemoveXMLClass(TXML2CodeClass(del));
    TXML2Code_ToTree(TXML2Code(obj), TargetNode);
  end;
end;
procedure TCMLTree.TXML2Code_AddXMLClassEvent(Sender: TObject);
begin
  TXML2Code(TargetObject).AddNewXMLClass;
  TXML2Code_ToTree(TXML2Code(TargetObject), TargetNode);
end;

procedure TCMLTree.TXML2Code_ClassDeleteEvent(obj: TXML2Code; del: TObject);
begin
  if del is TXML2CodeClass then
  begin
    TXML2Code(obj).RemoveXMLClass(TXML2CodeClass(del));
    TXML2Code_ToTree(TXML2Code(obj), TargetNode);
  end;
end;

procedure TCMLTree.TXML2Code_AddUsingEvent(Sender: TObject);
begin
  TXML2Code(TargetObject).AddUsing;
  TXML2Code_ToTree(TXML2Code(TargetObject), TargetNode);
end;

    { TXML2CodeClass }
procedure TCMLTree.TXML2CodeClass_ToTree(Obj: TXML2CodeClass; TreeNodeShape: TTreeNodeShape);
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.Data := Obj;
  TreeNodeShape.Text.Clear;
  TreeNodeShape.Text.Add('Class');
  for I := 0 to obj.ChildCount - 1 do
  begin
    TXML2CodeChild_ToTree(obj.Child[I], TreeNodeShape.AddChildObject('Child', obj.Child[I]));
  end;
  TreeNodeShape.AddChild('DataType:' + Obj.DataType);
  TreeNodeShape.AddChild('Root:' + Boolean2String(Obj.Root));
  TreeNodeShape.AddChild('Name:' + Obj.Name);
  if Obj.ParentClassExsit then
    TreeNodeShape.AddChild('ParentClass:' + Obj.ParentClass);
  if Obj.ChildClassExsit then
    TreeNodeShape.AddChild('ChildClass:' + String1D2String(Obj.ChildClass));
  if Obj.isAbstractExsit then
    TreeNodeShape.AddChild('isAbstract:' + Boolean2String(Obj.isAbstract));
  if Obj.PathExsit then
    TreeNodeShape.AddChild('Path:' + Obj.Path);
end;

procedure TCMLTree.TXML2CodeClass_ToPopupMenu(obj: TXML2CodeClass; Sender: TObject);
var
  pop: TPopupMenu;
  ChildAddMenu: TMenuItem;
  ParentClassAddMenu: TMenuItem;
  ChildClassAddMenu: TMenuItem;
  isAbstractAddMenu: TMenuItem;
  PathAddMenu: TMenuItem;
begin
  if Assigned(Sender) and (Sender is TPopupMenu) then
  begin
    pop := TPopupMenu(Sender);
    pop.Items.Clear;
    ChildAddMenu := TMenuItem.Create(pop);
    ChildAddMenu.Caption := 'Add Child';
    ChildAddMenu.OnClick := TXML2CodeClass_AddChildEvent;
    pop.Items.Add(ChildAddMenu);
    ParentClassAddMenu := TMenuItem.Create(pop);
    ParentClassAddMenu.Caption := 'Add ParentClass';
    ParentClassAddMenu.OnClick := TXML2CodeClass_AddParentClassEvent;
    pop.Items.Add(ParentClassAddMenu);
    ChildClassAddMenu := TMenuItem.Create(pop);
    ChildClassAddMenu.Caption := 'Add ChildClass';
    ChildClassAddMenu.OnClick := TXML2CodeClass_AddChildClassEvent;
    pop.Items.Add(ChildClassAddMenu);
    isAbstractAddMenu := TMenuItem.Create(pop);
    isAbstractAddMenu.Caption := 'Add isAbstract';
    isAbstractAddMenu.OnClick := TXML2CodeClass_AddisAbstractEvent;
    pop.Items.Add(isAbstractAddMenu);
    PathAddMenu := TMenuItem.Create(pop);
    PathAddMenu.Caption := 'Add Path';
    PathAddMenu.OnClick := TXML2CodeClass_AddPathEvent;
    pop.Items.Add(PathAddMenu);
  end;
end;

procedure TCMLTree.TXML2CodeClass_ToInspector(obj: TXML2CodeClass; ins: TObject);
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(ins) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('DataType');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Obj.DataType);
  Names_Value.Add('Root');
  Types_Value.Add(xs_boolean);
  _Values_Value.Add(Boolean2String(Obj.Root));
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Obj.Name);
  Names_Value.Add('ParentClass');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Obj.ParentClass);
  Names_Value.Add('ChildClass');
  Types_Value.Add(xml_String1D);
  _Values_Value.Add(String1D2String(Obj.ChildClass));
  Names_Value.Add('isAbstract');
  Types_Value.Add(xs_boolean);
  _Values_Value.Add(Boolean2String(Obj.isAbstract));
  Names_Value.Add('Path');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Obj.Path);
  TXMLInspector(ins).SetData(Names_Value, _Values_Value, Types_Value, Obj, Self);
end;

procedure TCMLTree.TXML2CodeClass_SetXMLProperty(obj: TXML2CodeClass; Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        obj.DataType := _Value;
      end;
    1:
      begin
        obj.Root := String2Boolean(_Value);
      end;
    2:
      begin
        obj.Name := _Value;
      end;
    3:
      begin
        obj.ParentClass := _Value;
      end;
    4:
      begin
        obj.ChildClass := String2String1D(_Value);
      end;
    5:
      begin
        obj.isAbstract := String2Boolean(_Value);
      end;
    6:
      begin
        obj.Path := _Value;
      end;
  end;
  TXML2CodeClass_ToTree(obj, TargetNode);
end;

procedure TCMLTree.TXML2CodeClass_Copy(source, target: TXML2CodeClass);
var
  I:Integer;
begin
  target.ChildClear;
  for I := 0 to source.ChildCount - 1 do
  begin
      TXML2CodeChild_Copy(source.Child[I], target. AddNewChild);
  end;
  target.DataType := source.DataType;
  target.Root := source.Root;
  target.Name := source.Name;
  target.ParentClass := source.ParentClass;
  target.ParentClassExsit := source.ParentClassExsit;
  target.ChildClass := source.ChildClass;
  target.ChildClassExsit := source.ChildClassExsit;
  target.isAbstract := source.isAbstract;
  target.isAbstractExsit := source.isAbstractExsit;
  target.Path := source.Path;
  target.PathExsit := source.PathExsit;
  TXML2CodeClass_ToTree(target, TargetNode);
end;

procedure TCMLTree.TXML2CodeClass_PasteToEvent(obj: TXML2CodeClass; items: TList<TObject>; const isCut: Boolean = False);
var
  I: Integer;
  tmpChild: TXML2CodeChild;
begin
  if (items.Count = 1) and (items[0] is TXML2CodeClass) and (not isCut) then
  begin
    TXML2CodeClass_Copy(TXML2CodeClass(items[0]), obj);
    TXML2CodeClass_ToTree(obj, TargetNode);
    Exit;
  end;
  for I := 0 to items.Count - 1 do
  begin
    if items[I] is TXML2CodeChild then
    begin
      tmpChild := TXML2CodeChild(items[I]);
      if isCut then
      begin
      end
      else
      begin
        TXML2CodeChild_Copy(tmpChild, obj.AddNewChild);
      end;
    end;
  end;
  TXML2CodeClass_ToTree(obj, TargetNode);
end;

procedure TCMLTree.TXML2CodeClass_ChildDeleteEvent(obj: TXML2CodeClass; del: TObject);
begin
  if del is TXML2CodeChild then
  begin
    TXML2CodeClass(obj).RemoveChild(TXML2CodeChild(del));
    TXML2CodeClass_ToTree(TXML2CodeClass(obj), TargetNode);
  end;
end;
procedure TCMLTree.TXML2CodeClass_AddChildEvent(Sender: TObject);
begin
  TXML2CodeClass(TargetObject).AddNewChild;
  TXML2CodeClass_ToTree(TXML2CodeClass(TargetObject), TargetNode);
end;

procedure TCMLTree.TXML2CodeClass_ClassDeleteEvent(obj: TXML2CodeClass; del: TObject);
begin
  if del is TXML2CodeChild then
  begin
    TXML2CodeClass(obj).RemoveChild(TXML2CodeChild(del));
    TXML2CodeClass_ToTree(TXML2CodeClass(obj), TargetNode);
  end;
end;

procedure TCMLTree.TXML2CodeClass_AddParentClassEvent(Sender: TObject);
begin
  TXML2CodeClass(TargetObject).AddParentClass;
  TXML2CodeClass_ToTree(TXML2CodeClass(TargetObject), TargetNode);
end;

procedure TCMLTree.TXML2CodeClass_AddChildClassEvent(Sender: TObject);
begin
  TXML2CodeClass(TargetObject).AddChildClass;
  TXML2CodeClass_ToTree(TXML2CodeClass(TargetObject), TargetNode);
end;

procedure TCMLTree.TXML2CodeClass_AddisAbstractEvent(Sender: TObject);
begin
  TXML2CodeClass(TargetObject).AddisAbstract;
  TXML2CodeClass_ToTree(TXML2CodeClass(TargetObject), TargetNode);
end;

procedure TCMLTree.TXML2CodeClass_AddPathEvent(Sender: TObject);
begin
  TXML2CodeClass(TargetObject).AddPath;
  TXML2CodeClass_ToTree(TXML2CodeClass(TargetObject), TargetNode);
end;

    { TXML2CodeChild }
procedure TCMLTree.TXML2CodeChild_ToTree(Obj: TXML2CodeChild; TreeNodeShape: TTreeNodeShape);
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.Data := Obj;
  TreeNodeShape.Text.Clear;
  TreeNodeShape.Text.Add('Child');
  TreeNodeShape.AddChild('Name:' + Obj.Name);
  TreeNodeShape.AddChild('DataType:' + Obj.DataType);
  TreeNodeShape.AddChild('Path:' + Obj.Path);
  TreeNodeShape.AddChild('Number:' + IntToStr(Obj.Number));
  TreeNodeShape.AddChild('Leaf:' + Boolean2String(Obj.Leaf));
  TreeNodeShape.AddChild('Visual:' + Boolean2String(Obj.Visual));
  if Obj.isVirtualExsit then
    TreeNodeShape.AddChild('isVirtual:' + Boolean2String(Obj.isVirtual));
end;

procedure TCMLTree.TXML2CodeChild_ToPopupMenu(obj: TXML2CodeChild; Sender: TObject);
var
  pop: TPopupMenu;
  isVirtualAddMenu: TMenuItem;
begin
  if Assigned(Sender) and (Sender is TPopupMenu) then
  begin
    pop := TPopupMenu(Sender);
    pop.Items.Clear;
    isVirtualAddMenu := TMenuItem.Create(pop);
    isVirtualAddMenu.Caption := 'Add isVirtual';
    isVirtualAddMenu.OnClick := TXML2CodeChild_AddisVirtualEvent;
    pop.Items.Add(isVirtualAddMenu);
  end;
end;

procedure TCMLTree.TXML2CodeChild_ToInspector(obj: TXML2CodeChild; ins: TObject);
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(ins) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Obj.Name);
  Names_Value.Add('DataType');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Obj.DataType);
  Names_Value.Add('Path');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Obj.Path);
  Names_Value.Add('Number');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(IntToStr(Obj.Number));
  Names_Value.Add('Leaf');
  Types_Value.Add(xs_boolean);
  _Values_Value.Add(Boolean2String(Obj.Leaf));
  Names_Value.Add('Visual');
  Types_Value.Add(xs_boolean);
  _Values_Value.Add(Boolean2String(Obj.Visual));
  Names_Value.Add('isVirtual');
  Types_Value.Add(xs_boolean);
  _Values_Value.Add(Boolean2String(Obj.isVirtual));
  TXMLInspector(ins).SetData(Names_Value, _Values_Value, Types_Value, Obj, Self);
end;

procedure TCMLTree.TXML2CodeChild_SetXMLProperty(obj: TXML2CodeChild; Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        obj.Name := _Value;
      end;
    1:
      begin
        obj.DataType := _Value;
      end;
    2:
      begin
        obj.Path := _Value;
      end;
    3:
      begin
        obj.Number := StrToIntDef(_Value, 0);;
      end;
    4:
      begin
        obj.Leaf := String2Boolean(_Value);
      end;
    5:
      begin
        obj.Visual := String2Boolean(_Value);
      end;
    6:
      begin
        obj.isVirtual := String2Boolean(_Value);
      end;
  end;
  TXML2CodeChild_ToTree(obj, TargetNode);
end;

procedure TCMLTree.TXML2CodeChild_Copy(source, target: TXML2CodeChild);
var
  I:Integer;
begin
  target.Name := source.Name;
  target.DataType := source.DataType;
  target.Path := source.Path;
  target.Number := source.Number;
  target.Leaf := source.Leaf;
  target.Visual := source.Visual;
  target.isVirtual := source.isVirtual;
  target.isVirtualExsit := source.isVirtualExsit;
  TXML2CodeChild_ToTree(target, TargetNode);
end;

procedure TCMLTree.TXML2CodeChild_PasteToEvent(obj: TXML2CodeChild; items: TList<TObject>; const isCut: Boolean = False);
var
  I: Integer;
begin
  if (items.Count = 1) and (items[0] is TXML2CodeChild) and (not isCut) then
  begin
    TXML2CodeChild_Copy(TXML2CodeChild(items[0]), obj);
    TXML2CodeChild_ToTree(obj, TargetNode);
    Exit;
  end;
  for I := 0 to items.Count - 1 do
  begin
  end;
  TXML2CodeChild_ToTree(obj, TargetNode);
end;

procedure TCMLTree.TXML2CodeChild_ChildDeleteEvent(obj: TXML2CodeChild; del: TObject);
begin
end;
procedure TCMLTree.TXML2CodeChild_AddisVirtualEvent(Sender: TObject);
begin
  TXML2CodeChild(TargetObject).AddisVirtual;
  TXML2CodeChild_ToTree(TXML2CodeChild(TargetObject), TargetNode);
end;


end.

