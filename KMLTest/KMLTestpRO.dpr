program KMLTestpRO;

uses
  Forms,
  TextEditForm in '..\XMLCore\TextEditForm.pas' {FormTextEdit},
  TextEditFrame in '..\XMLCore\TextEditFrame.pas' {FrameTextEdit: TFrame},
  XMLCore in '..\xmlcore\XMLCore.pas',
  XMLFrame in '..\XMLCore\XMLFrame.pas' {FrameXML: TFrame},
  XMLInspector in '..\XMLCore\XMLInspector.pas',
  XMLLeafTypes in '..\XMLCore\XMLLeafTypes.pas',
  XMLTree in '..\XMLCore\XMLTree.pas',
  KML in 'KML.pas',
  KMLFile in 'KMLFile.pas',
  KMLTest in 'KMLTest.pas' {Form2},
  KMLTree in 'KMLTree.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TFormTextEdit, FormTextEdit);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
