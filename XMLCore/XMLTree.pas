unit XMLTree;

interface

uses
  SysUtils, Types, Classes, Variants, Generics.Collections,
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  Rtti, XMLLeafTypes, XMLCore, TeeTree;

type

  TXMLTree = class;

  TXMLTree = class(TTree)
  public
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('XML2Code', [TXMLTree]);
end;


end.
