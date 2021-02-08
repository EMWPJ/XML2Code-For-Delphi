unit XML2CodeToolExe;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TeeTree, ExtCtrls, TeeProcs, FileCtrl,
  XMLFrame, XMLLeafTypes, XMLCore, XML2Code, CML, CMLTree, XMLInspector, Menus,
  XML2CodeToPas;

type
  TFormXML2Code = class(TForm)
    FrameXML2Code: TFrameXML;
    NewCML: TMenuItem;
    MainMenuXML2Code: TMainMenu;
    OpenFileMenu: TMenuItem;
    SaveFileMenu: TMenuItem;
    ExportXMLUnitMenu: TMenuItem;
    ExportClassUnitMenu: TMenuItem;
    ExportXMLTreeUnitMenu: TMenuItem;
    CMLs2Codes: TMenuItem;
    OpenDialogCML: TOpenDialog;
    procedure CMLs2CodesClick(Sender: TObject);
    procedure ExportXMLUnitMenuClick(Sender: TObject);
    procedure ExportXMLTreeUnitMenuClick(Sender: TObject);
    procedure ExportClassUnitMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NewCMLClick(Sender: TObject);
    procedure OpenFileMenuClick(Sender: TObject);
    procedure SaveFileMenuClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormXML2Code: TFormXML2Code;

implementation

{$R *.dfm}

procedure TFormXML2Code.CMLs2CodesClick(Sender: TObject);
var
  I: Integer;
  tmpCML: TCMLTree;
  path: string;
begin
  if OpenDialogCML.Execute then
  begin
    if SelectDirectory(path, [sdPerformCreate], 0) then
    begin
      tmpCML := TCMLTree.Create;
      for I := 0 to OpenDialogCML.Files.Count - 1 do
      begin
        tmpCML.LoadXML(OpenDialogCML.Files[I]);
        x_2_c := TXML2Code(tmpCML.GeoObj);
        x_2_c.ExportClassUnit(path);
        x_2_c.ExportXMLUnit(path);
        x_2_c.ExportXMLTreeUnit(path);
      end;
      FreeAndNil(tmpCML);
    end;
  end;
end;

procedure TFormXML2Code.ExportXMLUnitMenuClick(Sender: TObject);
var
  path: string;
begin
  path := '../XMLCodes';
  if SelectDirectory(path, [sdPerformCreate], 0) then
  begin
    x_2_c := TXML2Code(TCMLTree(FrameXML2Code.XML).GeoObj);
    x_2_c.ExportXMLUnit(path);
  end;
end;

procedure TFormXML2Code.ExportXMLTreeUnitMenuClick(Sender: TObject);
var
  path: string;
begin
  path := '../XMLCodes';
  if SelectDirectory(path, [sdPerformCreate], 0) then
  begin
    x_2_c := TXML2Code(TCMLTree(FrameXML2Code.XML).GeoObj);
    x_2_c.ExportXMLTreeUnit(path);
  end;
end;

procedure TFormXML2Code.ExportClassUnitMenuClick(Sender: TObject);
var
  path: string;
begin
  path := '../XMLCodes';
  if SelectDirectory(path, [sdPerformCreate], 0) then
  begin
    x_2_c := TXML2Code(FrameXML2Code.XML.GeoObj);
    x_2_c.ExportClassUnit(path);
  end;
end;

procedure TFormXML2Code.FormCreate(Sender: TObject);
begin
  FrameXML2Code.OnCreate;
  FrameXML2Code.XMLClass := TCMLTree;
//  NewCML.Caption := '新建(F1)';
//  OpenFileMenu.Caption := '打开(F2)';
//  SaveFileMenu.Caption := '保存(F3)';
//  ExportXMLUnitMenu.Caption := '导出XML单元(F4)';
//  ExportXMLTreeUnitMenu.Caption := '导出XMLTree单元(F5)';
//  ExportClassUnitMenu.Caption := '导出类单元(F6)';
//  CMLs2Codes.Caption := '批量导出代码(F7)';
end;

procedure TFormXML2Code.NewCMLClick(Sender: TObject);
begin
  FrameXML2Code.NewXMLMenuClick(Sender);
end;

procedure TFormXML2Code.OpenFileMenuClick(Sender: TObject);
begin
  FrameXML2Code.OpenXMLMenuClick(Sender);
end;

procedure TFormXML2Code.SaveFileMenuClick(Sender: TObject);
begin
  FrameXML2Code.SaveXMLMenuClick(Sender);
end;

end.
