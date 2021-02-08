unit XML2Code;

interface

uses
  Windows, SysUtils, Types, Classes, Variants, Generics.Collections, Dialogs,
  Controls, ExtCtrls, XMLLeafTypes;

type

  TXML2Code = class;
  TXML2CodeClass = class;
  TXML2CodeChild = class;

  TXML2Code = class(TObject)
  private
    FXMLClasss: TList<TXML2CodeClass>;
    FName: String;
    FDatatype: String;
    FTarget: String;
    FTargetUnit: String;
    FUsing: String1D;
    FUsingExsit: Boolean;
    procedure SetXMLClasss(const _Value: TList<TXML2CodeClass>);
    function GetXMLClass(Index: Integer): TXML2CodeClass;
    procedure SetXMLClass(Index: Integer; const _Value: TXML2CodeClass);
    procedure SetName(const _Value: String);
    procedure SetDatatype(const _Value: String);
    procedure SetTarget(const _Value: String);
    procedure SetTargetUnit(const _Value: String);
    procedure SetUsing(const _Value: String1D);
    procedure SetUsingExsit(const Value: Boolean);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddXMLClass(_Value: TXML2CodeClass);
    function AddNewXMLClass: TXML2CodeClass;
    procedure XMLClassClear;
    function XMLClassCount: Integer;
    procedure RemoveXMLClass(_Value: TXML2CodeClass);
    procedure DeleteXMLClass(Index: Integer);
    function AddUsing: String1D;
    procedure UsingRemove;
    property XMLClasss: TList<TXML2CodeClass>read FXMLClasss write SetXMLClasss;
    property XMLClass[Index: Integer]: TXML2CodeClass read GetXMLClass write SetXMLClass;
    property Name: String read FName write SetName;
    property Datatype: String read FDatatype write SetDatatype;
    property Target: String read FTarget write SetTarget;
    property TargetUnit: String read FTargetUnit write SetTargetUnit;
    property Using: String1D read FUsing write SetUsing;
    property UsingExsit: Boolean read FUsingExsit Write SetUsingExsit;
  end;

  TXML2CodeClass = class(TObject)
  private
    FChilds: TList<TXML2CodeChild>;
    FDatatype: String;
    FRoot: Boolean;
    FName: String;
    FParentClass: String;
    FParentClassExsit: Boolean;
    FChildClass: String1D;
    FChildClassExsit: Boolean;
    FisAbstract: Boolean;
    FisAbstractExsit: Boolean;
    FPath: String;
    FPathExsit: Boolean;
    procedure SetChilds(const _Value: TList<TXML2CodeChild>);
    function GetChild(Index: Integer): TXML2CodeChild;
    procedure SetChild(Index: Integer; const _Value: TXML2CodeChild);
    procedure SetDatatype(const _Value: String);
    procedure SetRoot(const _Value: Boolean);
    procedure SetName(const _Value: String);
    procedure SetParentClass(const _Value: String);
    procedure SetParentClassExsit(const Value: Boolean);
    procedure SetChildClass(const _Value: String1D);
    procedure SetChildClassExsit(const Value: Boolean);
    procedure SetisAbstract(const _Value: Boolean);
    procedure SetisAbstractExsit(const Value: Boolean);
    procedure SetPath(const _Value: String);
    procedure SetPathExsit(const Value: Boolean);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChild(_Value: TXML2CodeChild);
    function AddNewChild: TXML2CodeChild;
    procedure ChildClear;
    function ChildCount: Integer;
    procedure RemoveChild(_Value: TXML2CodeChild);
    procedure DeleteChild(Index: Integer);
    function AddParentClass: String;
    procedure ParentClassRemove;
    function AddChildClass: String1D;
    procedure ChildClassRemove;
    function AddisAbstract: Boolean;
    procedure isAbstractRemove;
    function AddPath: String;
    procedure PathRemove;
    property Childs: TList<TXML2CodeChild>read FChilds write SetChilds;
    property Child[Index: Integer]: TXML2CodeChild read GetChild write SetChild;
    property Datatype: String read FDatatype write SetDatatype;
    property Root: Boolean read FRoot write SetRoot;
    property Name: String read FName write SetName;
    property ParentClass: String read FParentClass write SetParentClass;
    property ParentClassExsit: Boolean read FParentClassExsit Write SetParentClassExsit;
    property ChildClass: String1D read FChildClass write SetChildClass;
    property ChildClassExsit: Boolean read FChildClassExsit Write SetChildClassExsit;
    property isAbstract: Boolean read FisAbstract write SetisAbstract;
    property isAbstractExsit: Boolean read FisAbstractExsit Write SetisAbstractExsit;
    property Path: String read FPath write SetPath;
    property PathExsit: Boolean read FPathExsit Write SetPathExsit;
  end;

  TXML2CodeChild = class(TObject)
  private
    FName: String;
    FDatatype: String;
    FPath: String;
    FNumber: Integer;
    FLeaf: Boolean;
    FVisual: Boolean;
    FisVirtual: Boolean;
    FisVirtualExsit: Boolean;
    procedure SetName(const _Value: String);
    procedure SetDatatype(const _Value: String);
    procedure SetPath(const _Value: String);
    procedure SetNumber(const _Value: Integer);
    procedure SetLeaf(const _Value: Boolean);
    procedure SetVisual(const _Value: Boolean);
    procedure SetisVirtual(const _Value: Boolean);
    procedure SetisVirtualExsit(const Value: Boolean);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function AddisVirtual: Boolean;
    procedure isVirtualRemove;
    property Name: String read FName write SetName;
    property Datatype: String read FDatatype write SetDatatype;
    property Path: String read FPath write SetPath;
    property Number: Integer read FNumber write SetNumber;
    property Leaf: Boolean read FLeaf write SetLeaf;
    property Visual: Boolean read FVisual write SetVisual;
    property isVirtual: Boolean read FisVirtual write SetisVirtual;
    property isVirtualExsit: Boolean read FisVirtualExsit Write SetisVirtualExsit;
  end;

implementation

{ XML2Code }
constructor TXML2Code.Create;
begin
  FXMLClasss := TList<TXML2CodeClass>.Create;
end;

destructor TXML2Code.Destroy;
begin
  XMLClassClear;
  FXMLClasss.Free;
  inherited;
end;

procedure TXML2Code.SetXMLClasss(const _Value: TList<TXML2CodeClass>);
begin
  XMLClassClear;
  FXMLClasss := _Value;
end;

function TXML2Code.GetXMLClass(Index: Integer): TXML2CodeClass;
begin
  Result := FXMLClasss[Index];
end;

procedure TXML2Code.SetXMLClass(Index: Integer; const _Value: TXML2CodeClass);
begin
  FXMLClasss[Index].Free;
  FXMLClasss[Index] := _Value;
end;

procedure TXML2Code.AddXMLClass(_Value: TXML2CodeClass);
begin ;
  FXMLClasss.Add(_Value);
end;

function TXML2Code.AddNewXMLClass: TXML2CodeClass;
var
  XMLClasstmp: TXML2CodeClass;
begin ;
  XMLClasstmp := TXML2CodeClass.Create;
  FXMLClasss.Add(XMLClasstmp);
  Result := XMLClasstmp;
end;

procedure TXML2Code.XMLClassClear;
begin
  while FXMLClasss.Count > 0 do
  begin
    FXMLClasss.Items[0].Free;
    FXMLClasss.Delete(0);
  end;
end;

function TXML2Code.XMLClassCount: Integer;
begin
  Result := FXMLClasss.Count;
end;

procedure TXML2Code.RemoveXMLClass(_Value: TXML2CodeClass);
begin
  FXMLClasss.Remove(_Value);
  _Value.Free;
end;

procedure TXML2Code.DeleteXMLClass(Index: Integer);
begin
  FXMLClasss.Items[Index].Free;
  FXMLClasss.Delete(Index);
end;

procedure TXML2Code.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TXML2Code.SetDatatype(const _Value: String);
begin
  FDatatype := _Value;
end;

procedure TXML2Code.SetTarget(const _Value: String);
begin
  FTarget := _Value;
end;

procedure TXML2Code.SetTargetUnit(const _Value: String);
begin
  FTargetUnit := _Value;
end;

procedure TXML2Code.SetUsing(const _Value: String1D);
begin
  FUsingExsit := True;
  FUsing := _Value;
end;

procedure TXML2Code.SetUsingExsit(const Value: Boolean);
begin
  FUsingExsit := Value;
end;

function TXML2Code.AddUsing: String1D;
begin ;
  Result := FUsing;
  FUsingExsit := True;
end;

procedure TXML2Code.UsingRemove;
begin
  if FUsingExsit then
  begin
    FUsingExsit := False;
  end;
end;

{ Class }
constructor TXML2CodeClass.Create;
begin
  FChilds := TList<TXML2CodeChild>.Create;
end;

destructor TXML2CodeClass.Destroy;
begin
  ChildClear;
  FChilds.Free;
  inherited;
end;

procedure TXML2CodeClass.SetChilds(const _Value: TList<TXML2CodeChild>);
begin
  ChildClear;
  FChilds := _Value;
end;

function TXML2CodeClass.GetChild(Index: Integer): TXML2CodeChild;
begin
  Result := FChilds[Index];
end;

procedure TXML2CodeClass.SetChild(Index: Integer; const _Value: TXML2CodeChild);
begin
  FChilds[Index].Free;
  FChilds[Index] := _Value;
end;

procedure TXML2CodeClass.AddChild(_Value: TXML2CodeChild);
begin ;
  FChilds.Add(_Value);
end;

function TXML2CodeClass.AddNewChild: TXML2CodeChild;
var
  Childtmp: TXML2CodeChild;
begin ;
  Childtmp := TXML2CodeChild.Create;
  FChilds.Add(Childtmp);
  Result := Childtmp;
end;

procedure TXML2CodeClass.ChildClear;
begin
  while FChilds.Count > 0 do
  begin
    FChilds.Items[0].Free;
    FChilds.Delete(0);
  end;
end;

function TXML2CodeClass.ChildCount: Integer;
begin
  Result := FChilds.Count;
end;

procedure TXML2CodeClass.RemoveChild(_Value: TXML2CodeChild);
begin
  FChilds.Remove(_Value);
  _Value.Free;
end;

procedure TXML2CodeClass.DeleteChild(Index: Integer);
begin
  FChilds.Items[Index].Free;
  FChilds.Delete(Index);
end;

procedure TXML2CodeClass.SetDatatype(const _Value: String);
begin
  FDatatype := _Value;
end;

procedure TXML2CodeClass.SetRoot(const _Value: Boolean);
begin
  FRoot := _Value;
end;

procedure TXML2CodeClass.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TXML2CodeClass.SetParentClass(const _Value: String);
begin
  FParentClassExsit := True;
  FParentClass := _Value;
end;

procedure TXML2CodeClass.SetParentClassExsit(const Value: Boolean);
begin
  FParentClassExsit := Value;
end;

function TXML2CodeClass.AddParentClass: String;
begin ;
  Result := FParentClass;
  FParentClassExsit := True;
end;

procedure TXML2CodeClass.ParentClassRemove;
begin
  if FParentClassExsit then
  begin
    FParentClassExsit := False;
  end;
end;

procedure TXML2CodeClass.SetChildClass(const _Value: String1D);
begin
  FChildClassExsit := True;
  FChildClass := _Value;
end;

procedure TXML2CodeClass.SetChildClassExsit(const Value: Boolean);
begin
  FChildClassExsit := Value;
end;

function TXML2CodeClass.AddChildClass: String1D;
begin ;
  Result := FChildClass;
  FChildClassExsit := True;
end;

procedure TXML2CodeClass.ChildClassRemove;
begin
  if FChildClassExsit then
  begin
    FChildClassExsit := False;
  end;
end;

procedure TXML2CodeClass.SetisAbstract(const _Value: Boolean);
begin
  FisAbstractExsit := True;
  FisAbstract := _Value;
end;

procedure TXML2CodeClass.SetisAbstractExsit(const Value: Boolean);
begin
  FisAbstractExsit := Value;
end;

function TXML2CodeClass.AddisAbstract: Boolean;
begin ;
  Result := FisAbstract;
  FisAbstractExsit := True;
end;

procedure TXML2CodeClass.isAbstractRemove;
begin
  if FisAbstractExsit then
  begin
    FisAbstractExsit := False;
  end;
end;

procedure TXML2CodeClass.SetPath(const _Value: String);
begin
  FPathExsit := True;
  FPath := _Value;
end;

procedure TXML2CodeClass.SetPathExsit(const Value: Boolean);
begin
  FPathExsit := Value;
end;

function TXML2CodeClass.AddPath: String;
begin ;
  Result := FPath;
  FPathExsit := True;
end;

procedure TXML2CodeClass.PathRemove;
begin
  if FPathExsit then
  begin
    FPathExsit := False;
  end;
end;

{ Child }
constructor TXML2CodeChild.Create;
begin
end;

destructor TXML2CodeChild.Destroy;
begin
  inherited;
end;

procedure TXML2CodeChild.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TXML2CodeChild.SetDatatype(const _Value: String);
begin
  FDatatype := _Value;
end;

procedure TXML2CodeChild.SetPath(const _Value: String);
begin
  FPath := _Value;
end;

procedure TXML2CodeChild.SetNumber(const _Value: Integer);
begin
  FNumber := _Value;
end;

procedure TXML2CodeChild.SetLeaf(const _Value: Boolean);
begin
  FLeaf := _Value;
end;

procedure TXML2CodeChild.SetVisual(const _Value: Boolean);
begin
  FVisual := _Value;
end;

procedure TXML2CodeChild.SetisVirtual(const _Value: Boolean);
begin
  FisVirtualExsit := True;
  FisVirtual := _Value;
end;

procedure TXML2CodeChild.SetisVirtualExsit(const Value: Boolean);
begin
  FisVirtualExsit := Value;
end;

function TXML2CodeChild.AddisVirtual: Boolean;
begin ;
  Result := FisVirtual;
  FisVirtualExsit := True;
end;

procedure TXML2CodeChild.isVirtualRemove;
begin
  if FisVirtualExsit then
  begin
    FisVirtualExsit := False;
  end;
end;

end.
