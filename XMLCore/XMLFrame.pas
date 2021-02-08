unit XMLFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TeeProcs, TeeTree, Generics.Collections, TypInfo, Rtti,
  XMLLeafTypes, XMLCore, XMLInspector, Menus;

type
  TFrameXML = class(TFrame)
    TreeXML: TTree;
    SplitterXML2Code: TSplitter;
    PopupMenuXMLFile: TPopupMenu;
    OpenXMLMenu: TMenuItem;
    SaveXMLMenu: TMenuItem;
    OpenDialogXML: TOpenDialog;
    SaveDialogXML: TSaveDialog;
    PopupMenuXMLNode: TPopupMenu;
    NewXMLMenu: TMenuItem;
    procedure OpenXMLMenuClick(Sender: TObject);
    procedure SaveXMLMenuClick(Sender: TObject);
    procedure DeleteChoosedXMLs(Sender: TObject);
    procedure CopyChoosedXMLs(Sender: TObject);
    procedure CutChoosedXMLs(Sender: TObject);
    procedure NewXMLMenuClick(Sender: TObject);
    procedure PasteChoosedXMLs(Sender: TObject);
    procedure TreeXMLClickShape(Sender: TTreeNodeShape; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
  private
    FXML: TXMLTree;
    FXMLClass: TXMLTreeClass;
    InspectorXML2Code: TXMLInspector;
    CopyedXMLNode: TList<TObject>;
    isCut: Boolean;
    procedure SetXML(const Value: TXMLTree);
    procedure SetXMLClass(const Value: TXMLTreeClass);
  public
    procedure OnCreate;
    procedure OnDestory;
    property XML: TXMLTree read FXML write SetXML;
  published
    property XMLClass: TXMLTreeClass read FXMLClass write SetXMLClass;
  end;

implementation

{$R *.dfm}

procedure TFrameXML.CopyChoosedXMLs(Sender: TObject);
var
  I, count: Integer;
  tmpNode: TObject;
begin
  isCut := False;
  count := TreeXML.Selected.count;
  CopyedXMLNode.Clear;
  for I := count - 1 downto 0 do
  begin
    if not Assigned(TreeXML.Selected[I]) then
    begin
      Continue;
    end;
    if TObject(TreeXML.Selected[I].Data) is TObject then
    begin
      CopyedXMLNode.Add(TObject(TreeXML.Selected[I].Data));
    end;
  end;
end;

procedure TFrameXML.CutChoosedXMLs(Sender: TObject);
var
  I, count: Integer;
  tmpNode: TObject;
begin
  isCut := True;
  count := TreeXML.Selected.count;
  for I := count - 1 downto 0 do
  begin
    if not Assigned(TreeXML.Selected[I]) then
    begin
      Continue;
    end;
    if TObject(TreeXML.Selected[I].Data) is TObject then
    begin
      CopyedXMLNode.Add(TObject(TreeXML.Selected[I].Data));
    end;
  end;
end;

procedure TFrameXML.DeleteChoosedXMLs(Sender: TObject);
var
  I, count: Integer;
  par, del: TObject;
begin
  count := TreeXML.Selected.count;
  for I := count - 1 downto 0 do
  begin
    par := TObject(TreeXML.Selected[I].Parent.Data);
    del := TObject(TreeXML.Selected[I].Data);
    if not Assigned(del) then
    begin
      Continue;
    end;
    FXML.TargetNode := TreeXML.Selected[I].Parent;
    FXML.TargetObject := par;
    FXML.ChildDeleteEvent(par, del);
  end;
end;

procedure TFrameXML.NewXMLMenuClick(Sender: TObject);
begin
  FXML := FXMLClass.Create;
  TreeXML.Clear;
  TreeXML.AddRoot('');
  FXML.TreeNodeShape := TreeXML.Roots[0];
  FXML.ToTree(TreeXML.Roots[0], Self);
end;

procedure TFrameXML.OnCreate;
begin
  InspectorXML2Code := TXMLInspector.Create(Self);
  InspectorXML2Code.Parent := Self;
  InspectorXML2Code.Align := alBottom;
  InspectorXML2Code.Height := 100;
  CopyedXMLNode := TList<TObject>.Create;
end;

procedure TFrameXML.OnDestory;
begin
  if Assigned(FXML) then
  begin
    FreeAndNil(FXML);
  end;
  FreeAndNil(InspectorXML2Code);
  FreeAndNil(CopyedXMLNode);
end;

procedure TFrameXML.OpenXMLMenuClick(Sender: TObject);
begin
  if OpenDialogXML.Execute then
  begin
    if Assigned(FXML) then
    begin
      FreeAndNil(FXML);
    end;
    FXML := FXMLClass.Create;
    FXML.LoadXML(OpenDialogXML.FileName);
    TreeXML.Clear;
    TreeXML.AddRoot('');
    FXML.TreeNodeShape := TreeXML.Roots[0];
    FXML.ToTree(TreeXML.Roots[0], FXML);
  end;
end;

procedure TFrameXML.PasteChoosedXMLs(Sender: TObject);
begin
  FXML.PasteToEvent(CopyedXMLNode, isCut);
  if isCut then
  begin
    //
  end;
end;

procedure TFrameXML.SaveXMLMenuClick(Sender: TObject);
begin
  if SaveDialogXML.Execute then
  begin
    FXML.SaveXML(SaveDialogXML.FileName);
  end;
end;

procedure TFrameXML.SetXML(const Value: TXMLTree);
begin
  FXML := Value;
end;

procedure TFrameXML.SetXMLClass(const Value: TXMLTreeClass);
begin
  FXMLClass := Value;
end;

procedure TFrameXML.TreeXMLClickShape(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  obj: TObject;
  tmp: TObject;
  DeleteChoosedMenu: TMenuItem;
  CopyChoosedMenu: TMenuItem;
  CutChoosedMenu: TMenuItem;
  PasteChoosedMenu: TMenuItem;
  pt: TPoint;
begin
  inherited;
  FXML.TargetObject := TObject(Sender.Data);
  FXML.TargetNode := Sender;
  if Assigned(FXML.TargetObject) then
  begin
    FXML.ToInspector(InspectorXML2Code);
    if Button = mbRight then
    begin
      PopupMenuXMLNode.Items.Clear;
      FXML.ToPopupMenu(PopupMenuXMLNode);
      DeleteChoosedMenu := TMenuItem.Create(PopupMenuXMLNode);
      DeleteChoosedMenu.Caption := 'Delete Choose';
      DeleteChoosedMenu.OnClick := DeleteChoosedXMLs;
      PopupMenuXMLNode.Items.Add(DeleteChoosedMenu);
      CopyChoosedMenu := TMenuItem.Create(PopupMenuXMLNode);
      CopyChoosedMenu.Caption := 'Copy Choose';
      CopyChoosedMenu.OnClick := CopyChoosedXMLs;
      PopupMenuXMLNode.Items.Add(CopyChoosedMenu);
      // CutChoosedMenu := TMenuItem.Create(PopupMenuXMLNode);
      // CutChoosedMenu.Caption := 'Cut Choose';
      // CutChoosedMenu.OnClick := CutChoosedXMLs;
      // PopupMenuXMLNode.Items.Add(CutChoosedMenu);
      PasteChoosedMenu := TMenuItem.Create(PopupMenuXMLNode);
      PasteChoosedMenu.Caption := 'Paste Choose';
      PasteChoosedMenu.OnClick := PasteChoosedXMLs;
      PopupMenuXMLNode.Items.Add(PasteChoosedMenu);
      if CopyedXMLNode.count = 0 then
      begin
        PasteChoosedMenu.Enabled := False;
      end;
      GetCursorPos(pt);
      PopupMenuXMLNode.Popup(pt.X, pt.Y);
    end;
  end;
end;

end.
