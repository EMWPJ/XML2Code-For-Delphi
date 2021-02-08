unit CML;

interface

uses
  Windows, SysUtils, Types, Classes, Variants, Generics.Collections, Dialogs, Controls, ExtCtrls,
  XMLCore, XMLDoc, XMLIntf, XMLLeafTypes, XML2Code;

type
  TCML = class;

  TCML = class(TXML)
  private
    FTXML2CodeObj: TXML2Code;
    procedure SetObj(const Value: TXML2Code);
  protected
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
    property TXML2CodeObj: TXML2Code read FTXML2CodeObj write SetObj;
  end;

implementation

procedure TCML.FromXML(node: IXMLNode);
begin
  inherited;
  if not Assigned(FTXML2CodeObj) then
    FTXML2CodeObj := TXML2Code.Create;
  TXML2Code_FromXML(FTXML2CodeObj, node);
end;

function TCML.ToXML(par: IXMLNode; pt: string): IXMLNode;
begin
  if Assigned(FTXML2CodeObj) then
    TXML2Code_ToXML(FTXML2CodeObj, par, pt);
end;

procedure TCML.FromGeoObj(const Obj: TObject);
begin
  inherited;
  FTXML2CodeObj := TXML2Code(Obj);
end;

function TCML.ToGeoObj: TObject;
begin
  Result := FTXML2CodeObj;
end;

procedure TCML.SetObj(const Value: TXML2Code);
begin
  FTXML2CodeObj := Value;
end;

procedure TCML.TXML2Code_FromXML(Obj: TXML2Code; node: IXMLNode);
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

procedure TCML.TXML2Code_ToXML(Obj: TXML2Code; par: IXMLNode; pt: string);
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

procedure TCML.TXML2CodeClass_FromXML(Obj: TXML2CodeClass; node: IXMLNode);
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
        Obj.Datatype := nodeTmp.Text;
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

procedure TCML.TXML2CodeClass_ToXML(Obj: TXML2CodeClass; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  DatatypeTmp: IXMLNode;
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
    DatatypeTmp := doc.CreateNode('type', ntAttribute);
    DatatypeTmp.NodeValue := Obj.Datatype;
    node.AttributeNodes.Add(DatatypeTmp);
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

procedure TCML.TXML2CodeChild_FromXML(Obj: TXML2CodeChild; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    Obj.isVirtualExsit := False;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I); ;
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
      else if nodeTmp.NodeName = 'path' then
      begin
        Obj.Path := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'number' then
      begin
        Obj.Number := StrToIntDef(nodeTmp.Text, 0); ;
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

procedure TCML.TXML2CodeChild_ToXML(Obj: TXML2CodeChild; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  DatatypeTmp: IXMLNode;
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
    DatatypeTmp := doc.CreateNode('type', ntAttribute);
    DatatypeTmp.NodeValue := Obj.Datatype;
    node.AttributeNodes.Add(DatatypeTmp);
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

end.
