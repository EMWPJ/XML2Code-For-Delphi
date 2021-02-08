program XML2CodePro;

uses
  Forms,
  XML2CodeToolExe in 'XML2CodeToolExe.pas' {FormXML2Code},
  XMLCore in '..\XMLCore\XMLCore.pas',
  XMLLeafTypes in '..\XMLCore\XMLLeafTypes.pas',
  XMLInspector in '..\XMLCore\XMLInspector.pas',
  TextEditFrame in '..\XMLCore\TextEditFrame.pas' {FrameTextEdit: TFrame},
  TextEditForm in '..\XMLCore\TextEditForm.pas' {FormTextEdit},
  CML in 'CML.pas',
  XMLFrame in '..\XMLCore\XMLFrame.pas' {FrameXML: TFrame},
  XML2Code in 'XML2Code.pas',
  CMLTree in 'CMLTree.pas',
  XML2CodeToPas in 'XML2CodeToPas.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormXML2Code, FormXML2Code);
  Application.CreateForm(TFormTextEdit, FormTextEdit);
  Application.Run;
end.
