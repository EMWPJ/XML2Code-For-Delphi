unit TextEditForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, XMLLeafTypes, TextEditFrame;

type
  TFormTextEdit = class(TForm)
    TextEdit: TFrameTextEdit;
    ButtonOK: TButton;
    GridPanel1: TGridPanel;
    ButtonCancel: TButton;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
    FText: string;
    FonSetString: StringEvent;
    function GetText: string;
    procedure SetText(const Value: string);
  public
    property Text: string read GetText write SetText;
  published
    property onSetString: StringEvent read FonSetString write FonSetString;
  end;

var
  FormTextEdit: TFormTextEdit;

implementation

{$R *.dfm}

procedure TFormTextEdit.ButtonCancelClick(Sender: TObject);
begin
  TextEdit.Text := FText;
  Self.Close;
end;

procedure TFormTextEdit.ButtonOKClick(Sender: TObject);
begin
  FText := TextEdit.Text;
  FonSetString(FText);
  Self.Close;
end;

function TFormTextEdit.GetText: string;
begin
  Result := FText;
end;

procedure TFormTextEdit.SetText(const Value: string);
begin
  FText := Value;
  TextEdit.Text := FText;
end;

end.
