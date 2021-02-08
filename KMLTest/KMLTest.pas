unit KMLTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, XMLCore, XMLLeafTypes, XMLInspector, KMLTree, XMLFrame;

type
  TForm2 = class(TForm)
    FrameXML1: TFrameXML;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  FrameXML1.OnCreate;
  FrameXML1.XMLClass := TKMLTree;
end;

end.
