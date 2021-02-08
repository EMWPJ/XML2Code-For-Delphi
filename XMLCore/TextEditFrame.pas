unit TextEditFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls;

type
  TFrameTextEdit = class(TFrame)
    MemoText: TMemo;
    PopupMenuText: TPopupMenu;
    OpenDialogText: TOpenDialog;
    SaveDialogText: TSaveDialog;
    LoadMenu: TMenuItem;
    SaveMenu: TMenuItem;
    procedure LoadMenuClick(Sender: TObject);
    procedure SaveMenuClick(Sender: TObject);
  private
    function GetText: String;
    procedure SetText(const Value: String);
    { Private declarations }
  public
    property Text: String read GetText write SetText;
    { Public declarations }
  end;

implementation

{$R *.dfm}

function TFrameTextEdit.GetText: String;
begin
  Result := MemoText.Text;
end;

procedure TFrameTextEdit.LoadMenuClick(Sender: TObject);
begin
  if OpenDialogText.Execute then
  begin
    MemoText.Clear;
    MemoText.Lines.LoadFromFile(OpenDialogText.FileName);
  end;
end;

procedure TFrameTextEdit.SaveMenuClick(Sender: TObject);
begin
  if SaveDialogText.Execute then
  begin
    MemoText.Lines.SaveToFile(SaveDialogText.FileName);
  end;
end;

procedure TFrameTextEdit.SetText(const Value: String);
begin
  MemoText.Text := Value;
end;

end.
