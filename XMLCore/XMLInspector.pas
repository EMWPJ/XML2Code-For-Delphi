unit XMLInspector;

interface

uses
  SysUtils, Types, Classes, Variants, Generics.Collections,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Spin,
  Rtti, XMLLeafTypes, XMLCore, ExtCtrls, ComStrs, ComCtrls, TextEditForm;

type
  TXMLInspector = class;
  TXMLEditorButton = class;

  TXMLInspector = class(TScrollBox)
  private
    FNames: TStringList;
    FOnSetEvent: XMLInspectorEvent;
    FTypes: TList<XMLTypes>;
    FValues: TStringList;
    FTarget: TObject;
    FXTree: TXMLTree;
    procedure SetNames(const value: TStringList);
    procedure SetTypes(const value: TList<XMLTypes>);
    procedure SetValues(const value: TStringList);
    procedure SetTarget(const value: TObject);
    procedure AddIndex(const Index: Integer);
    procedure AddLabel(const Index: Integer);
    procedure AddEdit(const Index: Integer);
    procedure AddCheckBox(const Index: Integer);
    procedure AddComboBox(const Index: Integer);
    procedure AddDateEdit(const Index: Integer);
    procedure AddTimeEdit(const Index: Integer);
    procedure AddDateTimeEdit(const Index: Integer);
    procedure AddNumberBox(const Index: Integer);
    procedure AddSpinBox(const Index: Integer);
    procedure AddXMLEditorButton(const Index: Integer; const xtype: XMLTypes);
    procedure SetXTree(const Value: TXMLTree);
  public
    procedure Change(Sender: TObject);
    procedure SetData(NamesValue, ValuesValue: TStringList; TypesValue:
        TList<XMLTypes>; Sender: TObject; tree: TXMLTree);
    procedure SetXMLProperty(Sender: TObject; Index: Integer; value: String);
        virtual;
    property Types: TList<XMLTypes>read FTypes write SetTypes;
    property Target: TObject read FTarget write SetTarget;
    property XTree: TXMLTree read FXTree write SetXTree;
  published
    property Names: TStringList read FNames write SetNames;
    property Values: TStringList read FValues write SetValues;
    property OnSetEvent: XMLInspectorEvent read FOnSetEvent write FOnSetEvent;
  end;

  TXMLEditorButton = class(TButton)
  private
    FOnChange: TNotifyEvent;
    FXValue: string;
    FXType: XMLTypes;
    procedure SetXValue(const value: string);
    procedure SetXType(const value: XMLTypes);
  public
    procedure ShowEditor(Sender: TObject);
    procedure SetValue(str: string);
    property XValue: string read FXValue write SetXValue;
    property xtype: XMLTypes read FXType write SetXType;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('XML2Code', [TXMLInspector]);
  RegisterComponents('XML2Code', [TXMLEditorButton]);
end;

procedure TXMLInspector.AddCheckBox(const Index: Integer);
var
  tmp: TCheckBox;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := alTop;
  tmp := TCheckBox.Create(pn);
  tmp.Parent := pn;
  tmp.Height := 20;
  tmp.Align := alTop;
  tmp.Caption := FNames[Index];
  tmp.Checked := String2Boolean(FValues[Index]);
  tmp.OnClick := Change;
end;

procedure TXMLInspector.AddComboBox(const Index: Integer);
var
  tmp: TComboBox;
  lb: TLabel;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := alTop;
  lb := TLabel.Create(pn);
  lb.Parent := pn;
  lb.Caption := FNames[Index];
  lb.Align := alLeft;
  tmp := TComboBox.Create(pn);
  tmp.Parent := pn;
  tmp.Align := alClient;
  tmp.OnChange := Change;
end;

procedure TXMLInspector.AddDateEdit(const Index: Integer);
var
  tmp: TDateTimePicker;
  lb: TLabel;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := alTop;
  lb := TLabel.Create(pn);
  lb.Parent := pn;
  lb.Caption := FNames[Index];
  lb.Align := alLeft;
  tmp := TDateTimePicker.Create(pn);
  tmp.Parent := pn;
  tmp.Align := alClient;
  tmp.Date := StrToDateDef(FValues[Index], Now(), XMLDateFormat);
  tmp.OnChange := Change;
end;

procedure TXMLInspector.AddDateTimeEdit(const Index: Integer);
var
  tmp: TDateTimePicker;
  lb: TLabel;
  pn: TPanel;
  dt: TDateTime;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := alTop;
  lb := TLabel.Create(pn);
  lb.Parent := pn;
  lb.Caption := FNames[Index];
  lb.Align := alLeft;
  dt := StrToDateTimeDef(FValues[Index], Now(), XMLDateTimeFormat);
  tmp := TDateTimePicker.Create(pn);
  tmp.Parent := pn;
  tmp.Align := alLeft;
  tmp.DateTime := dt;
  tmp.OnChange := Change;
end;

procedure TXMLInspector.AddEdit(const Index: Integer);
var
  tmp: TEdit;
  lb: TLabel;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := alTop;
  lb := TLabel.Create(pn);
  lb.Parent := pn;
  lb.Caption := FNames[Index];
  lb.Align := alLeft;
  tmp := TEdit.Create(pn);
  tmp.Parent := pn;
  tmp.Align := alClient;
  tmp.Text := FValues[Index];
  tmp.OnChange := Change;
end;

procedure TXMLInspector.AddIndex(const Index: Integer);
begin
  case FTypes[Index] of
    xs_ENTITIES:
      begin
        AddEdit(Index);
      end;
    xs_ENTITY:
      begin
        AddEdit(Index);
      end;
    xs_ID:
      begin
        AddEdit(Index);
      end;
    xs_IDREF:
      begin
        AddEdit(Index);
      end;
    xs_IDREFSlanguage:
      begin
        AddEdit(Index);
      end;
    xs_Name:
      begin
        AddEdit(Index);
      end;
    xs_NCName:
      begin
        AddEdit(Index);
      end;
    xs_NMTOKEN:
      begin
        AddEdit(Index);
      end;
    xs_NMTOKENS:
      begin
        AddEdit(Index);
      end;
    xs_normalizedString:
      begin
        AddEdit(Index);
      end;
    xs_QName:
      begin
        AddEdit(Index);
      end;
    xs_string:
      begin
        AddEdit(Index);
      end;
    xs_token:
      begin
        AddEdit(Index);
      end;
    xs_date:
      begin
        AddDateEdit(Index);
      end;
    xs_time:
      begin
        AddTimeEdit(Index);
      end;
    xs_dateTime:
      begin
        AddDateTimeEdit(Index);
      end;
    xs_duration:
      begin
        AddTimeEdit(Index);
      end;
    xs_byte:
      begin
        AddEdit(Index);
      end;
    xs_decimal:
      begin
        AddEdit(Index);
      end;
    xs_int:
      begin
        AddEdit(Index);
      end;
    xs_integer:
      begin
        AddEdit(Index);
      end;
    xs_long:
      begin
        AddEdit(Index);
      end;
    xs_negativeInteger:
      begin
        AddEdit(Index);
      end;
    xs_nonNegativeInteger:
      begin
        AddEdit(Index);
      end;
    xs_nonPositiveInteger:
      begin
        AddEdit(Index);
      end;
    xs_positiveInteger:
      begin
        AddNumberBox(Index);
      end;
    xs_short:
      begin
        AddEdit(Index);
      end;
    xs_unsignedLong:
      begin
        AddEdit(Index);
      end;
    xs_unsignedInt:
      begin
        AddEdit(Index);
      end;
    xs_unsignedShort:
      begin
        AddEdit(Index);
      end;
    xs_unsignedByte:
      begin
        AddEdit(Index);
      end;
    xs_anyURI:
      begin
        AddEdit(Index);
      end;
    xs_base64Binary:
      begin
        AddEdit(Index);
      end;
    xs_boolean:
      begin
        AddCheckBox(Index);
      end;
    xs_double:
      begin
        AddEdit(Index);
      end;
    xs_float:
      begin
        AddEdit(Index);
      end;
    xs_hexBinary:
      begin
        AddEdit(Index);
      end;
    xs_NOTATION:
      begin
        AddEdit(Index);
      end;
    xs_Class:
      begin
        AddEdit(Index);
      end;
    xml_Complex:
      begin
        AddEdit(Index);
      end;
    xml_Coordinate:
      begin
        AddEdit(Index);
      end;
    xml_ArrayCoordinates:
      begin
        AddXMLEditorButton(Index, xml_ArrayCoordinates);
      end;
    xml_String1D:
      begin
        AddXMLEditorButton(Index, xml_String1D);
      end;
    xml_Double1D:
      begin
        AddXMLEditorButton(Index, xml_Double1D);
      end;
    xml_Integer1D:
      begin
        AddXMLEditorButton(Index, xml_Integer1D);
      end;
    xml_Double2D:
      begin
        AddXMLEditorButton(Index, xml_Double2D);
      end;
    xml_Integer2D:
      begin
        AddXMLEditorButton(Index, xml_Integer2D);
      end;
    xml_Byte1D:
      begin
        AddXMLEditorButton(Index, xml_Byte1D);
      end;
    xml_Boolean1D:
      begin
        AddXMLEditorButton(Index, xml_Boolean1D);
      end;
    xml_Byte2D:
      begin
        AddXMLEditorButton(Index, xml_Byte2D);
      end;
    xml_Boolean2D:
      begin
        AddXMLEditorButton(Index, xml_Boolean2D);
      end;
    W_Point2I:
      begin
        AddEdit(Index);
      end;
    W_Point2D:
      begin
        AddEdit(Index);
      end;
    W_Point3I:
      begin
        AddEdit(Index);
      end;
    W_Point3D:
      begin
        AddEdit(Index);
      end;
    W_Point2Is:
      begin
        AddEdit(Index);
      end;
    W_Point2Ds:
      begin
        AddEdit(Index);
      end;
    W_Point3Is:
      begin
        AddEdit(Index);
      end;
    W_Point3Ds:
      begin
        AddEdit(Index);
      end;
    xml_Pointer:
      begin
        AddEdit(Index);
      end;
  end;
end;

procedure TXMLInspector.AddLabel(const Index: Integer);
var
  tmp: TLabel;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := alTop;
  tmp := TLabel.Create(pn);
  tmp.Parent := pn;
  tmp.Height := 20;
  tmp.Align := alTop;
  tmp.Caption := FNames[Index];
end;

procedure TXMLInspector.AddNumberBox(const Index: Integer);
var
  tmp: TEdit;
  lb: TLabel;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := alTop;
  lb := TLabel.Create(pn);
  lb.Parent := pn;
  lb.Caption := FNames[Index];
  lb.Align := alLeft;
  tmp := TEdit.Create(pn);
  tmp.Parent := pn;
  tmp.Align := alClient;
  tmp.Text := FValues[Index];
  tmp.OnChange := Change;
end;

procedure TXMLInspector.AddSpinBox(const Index: Integer);
var
  tmp: TSpinEdit;
  lb: TLabel;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := alTop;
  lb := TLabel.Create(pn);
  lb.Parent := pn;
  lb.Caption := FNames[Index];
  lb.Align := alLeft;
  tmp := TSpinEdit.Create(pn);
  tmp.Parent := pn;
  tmp.Align := alClient;
  tmp.Text := FValues[Index];
  tmp.OnChange := Change;
end;

procedure TXMLInspector.AddTimeEdit(const Index: Integer);
var
  tmp: TDateTimePicker;
  lb: TLabel;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := alTop;
  lb := TLabel.Create(pn);
  lb.Parent := pn;
  lb.Caption := FNames[Index];
  lb.Align := alLeft;
  tmp := TDateTimePicker.Create(pn);
  tmp.Parent := pn;
  tmp.Align := alClient;
  tmp.Time := StrToTimeDef(FValues[Index], Now(), XMLTimeFormat);
  tmp.OnChange := Change;
end;

procedure TXMLInspector.AddXMLEditorButton(const Index: Integer; const xtype: XMLTypes);
var
  tmp: TXMLEditorButton;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := alTop;
  tmp := TXMLEditorButton.Create(pn);
  tmp.Parent := pn;
  tmp.Align := alClient;
  tmp.Text := FNames[Index];
  tmp.XValue := FValues[Index];
  tmp.xtype := xtype;
  tmp.OnClick := tmp.ShowEditor;
  tmp.OnChange := Change;
end;

procedure TXMLInspector.Change(Sender: TObject);
var
  Index, I, J: Integer;
  findIndex: Boolean;
begin
  findIndex := False;
  for I := 0 to Self.ComponentCount - 1 do
  begin
    if Self.Components[I] = Sender then
    begin
      Index := I;
      Break;
    end;
    for J := 0 to Self.Components[I].ComponentCount - 1 do
    begin
      if Self.Components[I].Components[J] = Sender then
      begin
        Index := I;
        findIndex := True;
        Break;
      end;
    end;
    if findIndex then
    begin
      Break;
    end;
  end;
  case FTypes[Index] of
    xs_ENTITIES:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_ENTITY:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_ID:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_IDREF:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_IDREFSlanguage:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_Name:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_NCName:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_NMTOKEN:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_NMTOKENS:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_normalizedString:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_QName:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_string:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_token:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_date:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_time:
      begin
        FValues[Index] := FormatDateTime('hh:mm:ss', TDateTimePicker(Sender).Time);
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_dateTime:
      begin
        FValues[Index] := FormatDateTime('yyyy-mm-dd', TDateTimePicker(Sender).Date);
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_duration:
      begin
        FValues[Index] := FormatDateTime('yyyy-mm-dd', TDateTimePicker(Sender).DateTime);
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_byte:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_decimal:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_int:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_integer:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_long:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_negativeInteger:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_nonNegativeInteger:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_nonPositiveInteger:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_positiveInteger:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_short:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_unsignedLong:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_unsignedInt:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_unsignedShort:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_unsignedByte:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_anyURI:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_base64Binary:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_boolean:
      begin
        FValues[Index] := Boolean2String(TCheckBox(Sender).Checked);
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_double:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_float:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_hexBinary:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_NOTATION:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xs_Class:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xml_Complex:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xml_ArrayCoordinates:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xml_String1D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xml_Double1D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xml_Integer1D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xml_Double2D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xml_Integer2D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xml_Byte1D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xml_Boolean1D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xml_Byte2D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xml_Boolean2D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    W_Point2I:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    W_Point2D:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    W_Point3I:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    W_Point3D:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    W_Point2Is:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    W_Point2Ds:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    W_Point3Is:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    W_Point3Ds:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
    xml_Pointer:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Target, Index, FValues[Index]);
      end;
  else
    begin
      FValues[Index] := TEdit(Sender).Text;
      OnSetEvent(Target, Index, FValues[Index]);
    end;
  end;
end;

procedure TXMLInspector.SetData(NamesValue, ValuesValue: TStringList;
    TypesValue: TList<XMLTypes>; Sender: TObject; tree: TXMLTree);
var
  I, count: Integer;
begin
  FXTree := tree;
  if Assigned(FNames) then
    FNames.Free;
  if Assigned(FTypes) then
    FTypes.Free;
  if Assigned(FValues) then
    FValues.Free;
  FNames := NamesValue;
  FValues := ValuesValue;
  FTypes := TypesValue;
  FTarget := Sender;
  Self.FOnSetEvent := TXMLTree(FXTree).SetXMLProperty;
  TXMLTree(FXTree).OnSetEvent := Self.SetXMLProperty;
  while Self.ControlCount > 0 do
  begin
    if Assigned(Self.Controls[0]) then
      Self.Controls[0].Free;
  end;
  count := FNames.count;
  for I := 0 to count - 1 do
  begin
    AddIndex(I);
  end;
end;

procedure TXMLInspector.SetNames(const value: TStringList);
begin
  FNames := value;
end;

procedure TXMLInspector.SetXMLProperty(Sender: TObject; Index: Integer; value:
    String);
begin
  FValues[index] := value;
end;

procedure TXMLInspector.SetTypes(const value: TList<XMLTypes>);
begin
  FTypes := value;
end;

procedure TXMLInspector.SetValues(const value: TStringList);
begin
  FValues := value;
end;

procedure TXMLInspector.SetTarget(const value: TObject);
begin
  FTarget := value;
end;

procedure TXMLInspector.SetXTree(const Value: TXMLTree);
begin
  FXTree := Value;
end;

procedure TXMLEditorButton.SetXValue(const value: string);
begin
  FXValue := value;
end;

procedure TXMLEditorButton.SetValue(str: string);
begin
  FXValue := str;
  OnChange(Self);
end;

procedure TXMLEditorButton.SetXType(const value: XMLTypes);
begin
  FXType := value;
end;

procedure TXMLEditorButton.ShowEditor(Sender: TObject);
begin
  case FXType of
    xs_ENTITIES:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_ENTITY:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_ID:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_IDREF:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_IDREFSlanguage:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_Name:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_NCName:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_NMTOKEN:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_NMTOKENS:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_normalizedString:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_QName:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_string:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_token:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_date:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_time:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_dateTime:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_duration:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_byte:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_decimal:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_int:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_integer:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_long:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_negativeInteger:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_nonNegativeInteger:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_nonPositiveInteger:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_positiveInteger:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_short:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_unsignedLong:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_unsignedInt:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_unsignedShort:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_unsignedByte:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_anyURI:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_base64Binary:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_boolean:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_double:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_float:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_hexBinary:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_NOTATION:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xs_Class:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xml_Complex:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xml_ArrayCoordinates:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xml_Double1D:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xml_Integer1D:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xml_Double2D:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xml_Integer2D:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xml_Byte1D:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xml_Boolean1D:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xml_Byte2D:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xml_Boolean2D:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    W_Point2I:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    W_Point2D:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    W_Point3I:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    W_Point3D:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    W_Point2Is:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    W_Point2Ds:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    W_Point3Is:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    W_Point3Ds:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
    xml_Pointer:
      begin
        if Assigned(FormTextEdit) then
        begin
          FormTextEdit.Text := Self.FXValue;
          FormTextEdit.onSetString := Self.SetValue;
          FormTextEdit.ShowModal;
        end;
      end;
  else
    begin
      if Assigned(FormTextEdit) then
      begin
        FormTextEdit.Text := Self.FXValue;
        FormTextEdit.onSetString := Self.SetValue;
        FormTextEdit.ShowModal;
      end;
    end;
  end;
end;

end.
