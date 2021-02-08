unit XMLLeafTypes;

interface

uses
  SysUtils, Types, Classes, Variants, Generics.Collections;

type

  // 可扩展的基本类型枚举变量
  XMLTypes = (xs_ENTITIES, xs_ENTITY, xs_ID, xs_IDREF, xs_IDREFSlanguage, xs_Name, xs_NCName,
    xs_NMTOKEN, xs_NMTOKENS, xs_normalizedString, xs_QName, xs_string, xs_token, xs_date, xs_time,
    xs_dateTime, xs_duration, xs_byte, xs_decimal, xs_int, xs_integer, xs_long, xs_negativeInteger,
    xs_nonNegativeInteger, xs_nonPositiveInteger, xs_positiveInteger, xs_short, xs_unsignedLong,
    xs_unsignedInt, xs_unsignedShort, xs_unsignedByte, xs_anyURI, xs_base64Binary, xs_boolean,
    xs_double, xs_float, xs_hexBinary, xs_NOTATION, xs_Class, xml_XMLType, xml_Complex,
    xml_Coordinate, xml_ArrayCoordinates, xml_Double1D, xml_Integer1D, xml_String1D, xml_Double2D,
    xml_Double3D, xml_Integer2D, xml_Integer3D, xml_Byte1D, xml_Boolean1D, xml_Complex1D,
    xml_Byte2D, xml_Boolean2D, W_Point2I, W_Point2D, W_Point3I, W_Point3D, W_Point2Is,
    W_Point2Ds, W_Point3Is, W_Point3Ds, xml_Color, xml_Pointer);

const
  XMLTypeStrings: array [XMLTypes] of string = ('xs_ENTITIES', 'xs_ENTITY', 'xs_ID', 'xs_IDREF',
    'xs_IDREFSlanguage', 'xs_Name', 'xs_NCName', 'xs_NMTOKEN', 'xs_NMTOKENS',
    'xs_normalizedString', 'xs_QName', 'xs_string', 'xs_token', 'xs_date', 'xs_time',
    'xs_dateTime', 'xs_duration', 'xs_byte', 'xs_decimal', 'xs_int', 'xs_integer',
    'xs_long', 'xs_negativeInteger', 'xs_nonNegativeInteger', 'xs_nonPositiveInteger',
    'xs_positiveInteger', 'xs_short', 'xs_unsignedLong', 'xs_unsignedInt', 'xs_unsignedShort',
    'xs_unsignedByte', 'xs_anyURI', 'xs_base64Binary', 'xs_boolean', 'xs_double', 'xs_float',
    'xs_hexBinary', 'xs_NOTATION', 'xs_Class', 'xml_XMLType', 'xml_Complex', 'xml_Coordinate',
    'xml_ArrayCoordinates', 'xml_Double1D', 'xml_Integer1D', 'xml_String1D', 'xml_Double2D',
    'xml_Double3D', 'xml_Integer2D', 'xml_Integer3D', 'xml_Byte1D', 'xml_Boolean1D',
    'xml_Complex1D', 'xml_Byte2D', 'xml_Boolean2D', 'W_Point2I', 'W_Point2D',
    'W_Point3I', 'W_Point3D', 'W_Point2Is', 'W_Point2Ds', 'W_Point3Is', 'W_Point3Ds', 'xml_Color',
    'xml_Pointer');

type
  WPoint2D = Record
    x: Double;
    y: Double;
    constructor Create(const _x, _y: Double);
    constructor CreateFromString(const str: string);
    function ToString: string;
  End;

  WPoint2I = Record
    x: Integer;
    y: Integer;
    constructor Create(const _x, _y: Integer);
    constructor CreateFromString(const str: string);
    function ToString: string;
  End;

  WPoint3D = Record
    x: Double;
    y: Double;
    z: Double;
    constructor Create(const _x, _y, _z: Double);
    constructor CreateFromString(const str: string);
    function ToString: string;
  End;

  WPoint3I = Record
    x: Integer;
    y: Integer;
    z: Integer;
    constructor Create(const _x, _y, _z: Integer);
    constructor CreateFromString(const str: string);
    function ToString: string;
  End;

  TCoordinate = record
  Public
    Longitude: Double;
    Latitude: Double;
    Altitude: Double;
    constructor Create(const lon, lat, alt: Double);
    constructor CreateFromString(const str: string);
    function ToString: string;
  end;

  ArrayCoordinates = TArray<TCoordinate>;

  WPoint2Ds = TArray<WPoint2D>;
  WPoint2Is = TArray<WPoint2I>;
  WPoint3Ds = TArray<WPoint3D>;
  WPoint3Is = TArray<WPoint3I>;

  ShortInt1D = TArray<ShortInt>;
  Byte1D = TArray<Byte>;
  SmallInt1D = TArray<SmallInt>;
  Word1D = TArray<Word>;
  Integer1D = TArray<Integer>;
  Cardinal1D = TArray<Cardinal>;
  Int641D = TArray<Int64>;
  UInt641D = TArray<UInt64>;
  Double1D = TArray<Double>;
  Single1D = TArray<Single>;
  Boolean1D = TArray<Boolean>;
  String1D = TArray<String>;

  ShortInt2D = TArray<ShortInt1D>;
  Byte2D = TArray<Byte1D>;
  SmallInt2D = TArray<SmallInt1D>;
  Word2D = TArray<Word1D>;
  Integer2D = TArray<Integer1D>;
  Cardinal2D = TArray<Cardinal1D>;
  Int642D = TArray<Int641D>;
  UInt642D = TArray<UInt641D>;
  Double2D = TArray<Double1D>;
  Single2D = TArray<Single1D>;
  Boolean2D = TArray<Boolean1D>;
  String2D = TArray<String1D>;

  ShortInt3D = TArray<ShortInt2D>;
  Byte3D = TArray<Byte2D>;
  SmallInt3D = TArray<SmallInt2D>;
  Word3D = TArray<Word2D>;
  Integer3D = TArray<Integer2D>;
  Cardinal3D = TArray<Cardinal2D>;
  Int643D = TArray<Int642D>;
  UInt643D = TArray<UInt642D>;
  Double3D = TArray<Double2D>;
  Single3D = TArray<Single2D>;
  Boolean3D = TArray<Boolean2D>;
  String3D = TArray<String2D>;

  TComplex = record
    Real: Double;
    Imag: Double;
  Public
    constructor Create(rvalue, ivalue: Double);
    constructor CreateAP(Avalue, Pvalue: Double);
    constructor CreateFromString(str: String);
    constructor CreateFromXML(str: String);
    Class Operator Negative(z: TComplex): TComplex;
    OverLoad;
    Class Operator Add(Z1, Z2: TComplex): TComplex;
    OverLoad;
    Class Operator Add(z: TComplex; R: Double): TComplex;
    OverLoad;
    Class Operator Add(R: Double; z: TComplex): TComplex;
    OverLoad;
    Class Operator Subtract(Z1, Z2: TComplex): TComplex;
    OverLoad;
    Class Operator Subtract(z: TComplex; R: Double): TComplex;
    OverLoad;
    Class Operator Subtract(R: Double; z: TComplex): TComplex;
    OverLoad;
    Class Operator Multiply(Z1, Z2: TComplex): TComplex;
    OverLoad;
    Class Operator Multiply(z: TComplex; R: Double): TComplex;
    OverLoad;
    Class Operator Multiply(R: Double; z: TComplex): TComplex;
    OverLoad;
    Class Operator Divide(Z1, Z2: TComplex): TComplex;
    OverLoad;
    Class Operator Divide(z: TComplex; R: Double): TComplex;
    OverLoad;
    Class Operator Divide(R: Double; Z2: TComplex): TComplex;
    OverLoad;
    Function Conjugate: TComplex;
    Function Module: Double;
    Function Module2: Double;
    Function Phase: Double; { 以弧度为单位,-Pi~Pi之间 }
    Function Sin: TComplex;
    Function Cos: TComplex;
    Function Sqr: TComplex;
    Function Sqrt: TComplex;
    Function Ln: TComplex;
    Function Exp: TComplex;
    Function ToString: String;
    Function ToXML: String;
    Procedure FromString(str: String);
    Procedure FromXML(str: String);
  End;

const
  CMPLX_I: TComplex = (Real: 0; Imag: 1);
  CMPLX_0: TComplex = (Real: 0; Imag: 0);
  CMPLX_1: TComplex = (Real: 1; Imag: 0);
  CMPLX_2: TComplex = (Real: 2; Imag: 0);
  CMPLX_00: TComplex = (Real: 1E-20; Imag: 0);

type
  TComplex1D = TArray<TComplex>;
  TComplex2D = TArray<TComplex1D>;
  TComplex3D = TArray<TComplex2D>;

  StringsEvent = procedure(strs: TStrings) of object;
  StringEvent = procedure(str: string) of object;

  // 字符串转节点类型
function String2XMLType(const str: String): XMLTypes; inline;
// 节点类型转Pascal类型字符串
function XMLType2PascalType(const tp: XMLTypes): String; inline;
// 节点转化为String的函数名
function XMLConvertStr(const tp: XMLTypes; name: string): String; inline;
// String转化为节点的函数名
function StrConvertXML(const tp: XMLTypes; name: string): String; inline;

function Hex2String(const value: Cardinal): string;
function String2Hex(const value: string): Cardinal;
function Boolean2String(const value: Boolean): String; inline;
function String2Boolean(const value: String): Boolean; inline;
function Integer1D2String(const value: Integer1D): String; inline;
function String2Integer1D(const value: String): Integer1D; inline;
function String1D2String(const value: String1D): String; inline;
function String2String1D(const value: String): String1D; inline;
function Double1D2String(const value: Double1D): String; inline;
function String2Double1D(const value: String): Double1D; inline;
function Boolean1D2String(const value: Boolean1D): String; inline;
function String2Boolean1D(const value: String): Boolean1D; inline;
function Integer2D2String(const value: Integer2D): String; inline;
function String2Integer2D(const value: String): Integer2D; inline;
function Integer3D2String(const value: Integer3D): String; inline;
function String2Integer3D(const value: String): Integer3D; inline;
function Double2D2String(const value: Double2D): String; inline;
function String2Double2D(const value: String): Double2D; inline;
function Double3D2String(const value: Double3D): String; inline;
function String2Double3D(const value: String): Double3D; inline;
function Boolean2D2String(const value: Boolean2D): String; inline;
function String2Boolean2D(const value: String): Boolean2D; inline;

function StringToPoint2D(const str: string): WPoint2D; inline;
function StringToPoint2I(const str: string): WPoint2I; inline;
function StringToPoint3D(const str: string): WPoint3D; inline;
function StringToPoint3I(const str: string): WPoint3I; inline;
// function StringToPoint2Ds(const str: string): WPoint2Ds;inline;
// function StringToPoint2Is(const str: string): WPoint2Is;inline;
// function StringToPoint3Ds(const str: string): WPoint3Ds;inline;
// function StringToPoint3Is(const str: string): WPoint3Is;inline;

function Point2DToString(const p: WPoint2D): String; inline;
function Point2IToString(const p: WPoint2I): String; inline;
function Point3DToString(const p: WPoint3D): String; inline;
function Point3IToString(const p: WPoint3I): String; inline;
function Point2DsToString(const p: WPoint2Ds): String; inline;
function Point2IsToString(const p: WPoint2Is): String; inline;
function Point3DsToString(const p: WPoint3Ds): String; inline;
function Point3IsToString(const p: WPoint3Is): String; inline;

function Complex1D2String(const value: TComplex1D): String; inline;
function String2Complex1D(const value: String): TComplex1D; inline;

function StringToCoordinates(const str: string): ArrayCoordinates; inline;
function CoordinatesToString(const coors: ArrayCoordinates): String; inline;

function Double2D21D(value: Double2D): Double1D; inline;
function Double1D22D(value: Double1D; count: Integer): Double2D; inline;

const
  XMLDTFormat: string = 'yyyy-mm-dd"T"hh:nn:ss';
  XMLDFormat: string = 'yyyy-mm-dd';
  XMLTFormat: string = 'hh:nn:ss';

var
  XMLDateFormat: TFormatSettings;
  XMLTimeFormat: TFormatSettings;
  XMLDateTimeFormat: TFormatSettings;

implementation

// 字符串转节点类型
function String2XMLType(const str: String): XMLTypes;
begin
  if LowerCase(str) = 'xs:entities' then
    Result := xs_ENTITIES
  else if LowerCase(str) = 'xs:entity' then
    Result := xs_ENTITY
  else if LowerCase(str) = 'xs:id' then
    Result := xs_ID
  else if LowerCase(str) = 'xs:idref' then
    Result := xs_IDREF
  else if LowerCase(str) = 'xs:idrefslanguage' then
    Result := xs_IDREFSlanguage
  else if LowerCase(str) = 'xs:name' then
    Result := xs_Name
  else if LowerCase(str) = 'xs:ncname' then
    Result := xs_NCName
  else if LowerCase(str) = 'xs:nmtoken' then
    Result := xs_NMTOKEN
  else if LowerCase(str) = 'xs:nmtokens' then
    Result := xs_NMTOKENS
  else if LowerCase(str) = 'xs:normalizedstring' then
    Result := xs_normalizedString
  else if LowerCase(str) = 'xs:qname' then
    Result := xs_QName
  else if LowerCase(str) = 'xs:string' then
    Result := xs_string
  else if LowerCase(str) = 'xs:token' then
    Result := xs_token
  else if LowerCase(str) = 'xs:date' then
    Result := xs_date
  else if LowerCase(str) = 'xs:time' then
    Result := xs_time
  else if LowerCase(str) = 'xs:datetime' then
    Result := xs_dateTime
  else if LowerCase(str) = 'xs:duration' then
    Result := xs_duration
  else if LowerCase(str) = 'xs:byte' then
    Result := xs_byte
  else if LowerCase(str) = 'byte' then
    Result := xs_byte
  else if LowerCase(str) = 'xs:decimal' then
    Result := xs_decimal
  else if LowerCase(str) = 'xs:int' then
    Result := xs_int
  else if LowerCase(str) = 'xs:integer' then
    Result := xs_integer
  else if LowerCase(str) = 'xs:long' then
    Result := xs_long
  else if LowerCase(str) = 'xs:negativeinteger' then
    Result := xs_negativeInteger
  else if LowerCase(str) = 'xs:nonnegativeinteger' then
    Result := xs_nonNegativeInteger
  else if LowerCase(str) = 'xs:nonpositiveinteger' then
    Result := xs_nonPositiveInteger
  else if LowerCase(str) = 'xs:positiveinteger' then
    Result := xs_positiveInteger
  else if LowerCase(str) = 'xs:short' then
    Result := xs_short
  else if LowerCase(str) = 'xs:unsignedlong' then
    Result := xs_unsignedLong
  else if LowerCase(str) = 'xs:unsignedint' then
    Result := xs_unsignedInt
  else if LowerCase(str) = 'xs:unsignedshort' then
    Result := xs_unsignedShort
  else if LowerCase(str) = 'xs:unsignedbyte' then
    Result := xs_unsignedByte
  else if LowerCase(str) = 'xs:anyuri' then
    Result := xs_anyURI
  else if LowerCase(str) = 'xs:base64binary' then
    Result := xs_base64Binary
  else if LowerCase(str) = 'xs:boolean' then
    Result := xs_boolean
  else if LowerCase(str) = 'boolean' then
    Result := xs_boolean
  else if LowerCase(str) = 'bool' then
    Result := xs_boolean
  else if LowerCase(str) = 'xs:double' then
    Result := xs_double
  else if LowerCase(str) = 'xs:float' then
    Result := xs_float
  else if LowerCase(str) = 'xs:hexbinary' then
    Result := xs_hexBinary
  else if LowerCase(str) = 'hex' then
    Result := xs_hexBinary
  else if LowerCase(str) = 'xs:notation' then
    Result := xs_NOTATION
  else if LowerCase(str) = 'string' then
    Result := xs_string
  else if LowerCase(str) = 'integer' then
    Result := xs_integer
  else if LowerCase(str) = 'double' then
    Result := xs_double
  else if LowerCase(str) = 'float' then
    Result := xs_float
  else if LowerCase(str) = 'real' then
    Result := xs_double
  else if LowerCase(str) = 'tdatetime' then
    Result := xs_dateTime
  else if LowerCase(str) = 'datetime' then
    Result := xs_dateTime
  else if LowerCase(str) = 'tnewcomplex' then
    Result := xml_Complex
  else if LowerCase(str) = 'complex' then
    Result := xml_Complex
  else if LowerCase(str) = 'complex1d' then
    Result := xml_Complex1D
  else if LowerCase(str) = 'coordinate' then
    Result := xml_Coordinate
  else if LowerCase(str) = 'arraycoordinates' then
    Result := xml_ArrayCoordinates
  else if LowerCase(str) = 'coordinates' then
    Result := xml_ArrayCoordinates
  else if LowerCase(str) = 'tarray1d' then
    Result := xml_Double1D
  else if LowerCase(str) = 'double1d' then
    Result := xml_Double1D
  else if LowerCase(str) = 'tarray1i' then
    Result := xml_Integer1D
  else if LowerCase(str) = 'integer1d' then
    Result := xml_Integer1D
  else if LowerCase(str) = 'tarray1s' then
    Result := xml_String1D
  else if LowerCase(str) = 'string1d' then
    Result := xml_String1D
  else if LowerCase(str) = 'tmatrx2d' then
    Result := xml_Double2D
  else if LowerCase(str) = 'double2d' then
    Result := xml_Double2D
  else if LowerCase(str) = 'double3d' then
    Result := xml_Double3D
  else if LowerCase(str) = 'tmatrx2i' then
    Result := xml_Integer2D
  else if LowerCase(str) = 'integer2d' then
    Result := xml_Integer2D
  else if LowerCase(str) = 'integer3d' then
    Result := xml_Integer3D
  else if LowerCase(str) = 'tarray1b' then
    Result := xml_Byte1D
  else if LowerCase(str) = 'tarray1bl' then
    Result := xml_Boolean1D
  else if LowerCase(str) = 'tmatrx2b' then
    Result := xml_Byte2D
  else if LowerCase(str) = 'tmatrx2bl' then
    Result := xml_Boolean2D
  else if LowerCase(str) = 'wpoint2i' then
    Result := W_Point2I
  else if LowerCase(str) = 'wpoint2d' then
    Result := W_Point2D
  else if LowerCase(str) = 'wpoint3i' then
    Result := W_Point3I
  else if LowerCase(str) = 'wpoint3d' then
    Result := W_Point3D
  else if LowerCase(str) = 'wpoint2is' then
    Result := W_Point2Is
  else if LowerCase(str) = 'wpoint2ds' then
    Result := W_Point2Ds
  else if LowerCase(str) = 'wpoint3is' then
    Result := W_Point3Is
  else if LowerCase(str) = 'wpoint3ds' then
    Result := W_Point3Ds
  else if LowerCase(str) = 'xmltype' then
    Result := xml_XMLType
  else if LowerCase(str) = 'color' then
    Result := xml_Color
  else if LowerCase(str) = 'pointer' then
    Result := xml_Pointer
  else
    Result := xs_Class;
end;

// 节点类型转Pascal类型字符串
function XMLType2PascalType(const tp: XMLTypes): String;
begin
  case tp of
    xs_ENTITIES:
      Result := 'String';
    xs_ENTITY:
      Result := 'String';
    xs_ID:
      Result := 'String';
    xs_IDREF:
      Result := 'String';
    xs_IDREFSlanguage:
      Result := 'String';
    xs_Name:
      Result := 'String';
    xs_NCName:
      Result := 'String';
    xs_NMTOKEN:
      Result := 'String';
    xs_NMTOKENS:
      Result := 'String';
    xs_normalizedString:
      Result := 'String';
    xs_QName:
      Result := 'String';
    xs_string:
      Result := 'String';
    xs_token:
      Result := 'String';
    xs_date:
      Result := 'TDateTime';
    xs_time:
      Result := 'TDateTime';
    xs_dateTime:
      Result := 'TDateTime';
    xs_duration:
      Result := 'String';
    xs_byte:
      Result := 'Byte';
    xs_decimal:
      Result := 'Double';
    xs_int:
      Result := 'Integer';
    xs_integer:
      Result := 'Integer';
    xs_long:
      Result := 'Longint';
    xs_negativeInteger:
      Result := 'Integer';
    xs_nonNegativeInteger:
      Result := 'Integer';
    xs_nonPositiveInteger:
      Result := 'Integer';
    xs_positiveInteger:
      Result := 'Integer';
    xs_short:
      Result := 'Smallint';
    xs_unsignedLong:
      Result := 'Cardinal';
    xs_unsignedInt:
      Result := 'Longword';
    xs_unsignedShort:
      Result := 'Word';
    xs_unsignedByte:
      Result := 'Int8';
    xs_anyURI:
      Result := 'String';
    xs_base64Binary:
      Result := 'String';
    xs_boolean:
      Result := 'Boolean';
    xs_double:
      Result := 'Double';
    xs_float:
      Result := 'Single';
    xs_hexBinary:
      Result := 'String';
    xs_NOTATION:
      Result := 'String';
    xs_Class:
      Result := 'TXML';
    xml_Complex:
      Result := 'TComplex';
    xml_Complex1D:
      Result := 'TComplex1D';
    xml_Coordinate:
      Result := 'TCoordinate';
    xml_ArrayCoordinates:
      Result := 'ArrayCoordinates';
    xml_Double1D:
      Result := 'Double1D';
    xml_Integer1D:
      Result := 'Integer1D';
    xml_String1D:
      Result := 'String1D';
    xml_Double2D:
      Result := 'Double2D';
    xml_Double3D:
      Result := 'Double3D';
    xml_Integer2D:
      Result := 'Integer2D';
    xml_Integer3D:
      Result := 'Integer3D';
    xml_Byte1D:
      Result := 'Byte1D';
    xml_Boolean1D:
      Result := 'Boolean1D';
    xml_Byte2D:
      Result := 'Byte2D';
    xml_Boolean2D:
      Result := 'Boolean2D';
    W_Point2I:
      Result := 'WPoint2I';
    W_Point2D:
      Result := 'WPoint2D';
    W_Point3I:
      Result := 'WPoint3I';
    W_Point3D:
      Result := 'WPoint3d';
    W_Point2Is:
      Result := 'WPoint2Is';
    W_Point2Ds:
      Result := 'WPoint2Ds';
    W_Point3Is:
      Result := 'WPoint3Is';
    W_Point3Ds:
      Result := 'WPoint3ds';
    xml_Color:
      Result := 'TAlphaColor';
    xml_Pointer:
      Result := 'Pointer';
  end;
end;

// 节点转化为String的函数名
function XMLConvertStr(const tp: XMLTypes; name: string): String;
begin
  case tp of
    xs_ENTITIES:
      Result := name;
    xs_ENTITY:
      Result := name;
    xs_ID:
      Result := name;
    xs_IDREF:
      Result := name;
    xs_IDREFSlanguage:
      Result := name;
    xs_Name:
      Result := name;
    xs_NCName:
      Result := name;
    xs_NMTOKEN:
      Result := name;
    xs_NMTOKENS:
      Result := name;
    xs_normalizedString:
      Result := name;
    xs_QName:
      Result := name;
    xs_string:
      Result := name;
    xs_token:
      Result := name;
    xs_date:
      Result := 'FormatDateTime(XMLDFormat, ' + name + ')';
    xs_time:
      Result := 'FormatDateTime(XMLTFormat, ' + name + ')';
    xs_dateTime:
      Result := 'FormatDateTime(XMLDTFormat, ' + name + ')';
    xs_duration:
      Result := 'FormatDateTime(XMLDTFormat, ' + name + ')';
    xs_byte:
      Result := 'IntToStr(' + name + ')';
    xs_decimal:
      Result := 'FloatToStr(' + name + ')';
    xs_int:
      Result := 'IntToStr(' + name + ')';
    xs_integer:
      Result := 'IntToStr(' + name + ')';
    xs_long:
      Result := 'IntToStr(' + name + ')';
    xs_negativeInteger:
      Result := 'IntToStr(' + name + ')';
    xs_nonNegativeInteger:
      Result := 'IntToStr(' + name + ')';
    xs_nonPositiveInteger:
      Result := 'IntToStr(' + name + ')';
    xs_positiveInteger:
      Result := 'IntToStr(' + name + ')';
    xs_short:
      Result := 'IntToStr(' + name + ')';
    xs_unsignedLong:
      Result := 'IntToStr(' + name + ')';
    xs_unsignedInt:
      Result := 'IntToStr(' + name + ')';
    xs_unsignedShort:
      Result := 'IntToStr(' + name + ')';
    xs_unsignedByte:
      Result := 'IntToStr(' + name + ')';
    xs_anyURI:
      Result := name;
    xs_base64Binary:
      Result := name;
    xs_boolean:
      Result := 'Boolean2String(' + name + ')';
    xs_double:
      Result := 'FloatToStr(' + name + ')';
    xs_float:
      Result := 'FloatToStr(' + name + ')';
    xs_hexBinary:
      Result := 'Hex2String(' + name + ')';
    xs_NOTATION:
      Result := name;
    xs_Class:
      Result := name;
    xml_Complex:
      Result := name + '.toXML';
    xml_Complex1D:
      Result := 'Complex1D2String(' + name + ')';
    xml_Coordinate:
      Result := name + '.toString';
    xml_ArrayCoordinates:
      Result := 'CoordinatesToString(' + name + ')';
    xml_Double1D:
      Result := 'Double1D2String(' + name + ')';
    xml_Integer1D:
      Result := 'Integer1D2String(' + name + ')';
    xml_Integer2D:
      Result := 'Integer2D2String(' + name + ')';
    xml_Integer3D:
      Result := 'Integer3D2String(' + name + ')';
    xml_String1D:
      Result := 'String1D2String(' + name + ')';
    xml_Double2D:
      Result := 'Double2D2String(' + name + ')';
    xml_Double3D:
      Result := 'Double3D2String(' + name + ')';
    xml_Byte1D:
      Result := 'Byte1D2String(' + name + ')';
    xml_Boolean1D:
      Result := 'Boolean1D2String(' + name + ')';
    xml_Byte2D:
      Result := 'Byte2D2String(' + name + ')';
    xml_Boolean2D:
      Result := 'Boolean2D2String(' + name + ')';
    W_Point2I:
      Result := 'Point2IToString(' + name + ')';
    W_Point2D:
      Result := 'Point2DToString(' + name + ')';
    W_Point3I:
      Result := 'Point3IToString(' + name + ')';
    W_Point3D:
      Result := 'Point3dToString(' + name + ')';
    W_Point2Is:
      Result := 'Point2IsToString(' + name + ')';
    W_Point2Ds:
      Result := 'Point2DsToString(' + name + ')';
    W_Point3Is:
      Result := 'Point3IsToString(' + name + ')';
    W_Point3Ds:
      Result := 'Point3dsToString(' + name + ')';
    xml_Color:
      Result := 'Hex2String(' + name + ')';
    xml_Pointer:
      Result := 'PointerToString(' + name + ')';
  end;
end;

// String转化为节点的函数名
function StrConvertXML(const tp: XMLTypes; name: string): String;
begin
  case tp of
    xs_ENTITIES:
      Result := name;
    xs_ENTITY:
      Result := name;
    xs_ID:
      Result := name;
    xs_IDREF:
      Result := name;
    xs_IDREFSlanguage:
      Result := name;
    xs_Name:
      Result := name;
    xs_NCName:
      Result := name;
    xs_NMTOKEN:
      Result := name;
    xs_NMTOKENS:
      Result := name;
    xs_normalizedString:
      Result := name;
    xs_QName:
      Result := name;
    xs_string:
      Result := name;
    xs_token:
      Result := name;
    xs_date:
      Result := 'StrToDateDef(' + name + ', Now(), XMLDateFormat)';
    xs_time:
      Result := 'StrToTimeDef(' + name + ', Now(), XMLTimeFormat)';
    xs_dateTime:
      Result := 'StrToDateTimeDef(' + name + ', Now(), XMLDateTimeFormat)';
    xs_duration:
      Result := 'StrToDateTimeDef(' + name + ', Now(), XMLDateTimeFormat)';
    xs_byte:
      Result := 'StrToIntDef(' + name + ', 0);';
    xs_decimal:
      Result := 'StrToFloatDef(' + name + ', 0);';
    xs_int:
      Result := 'StrToIntDef(' + name + ', 0);';
    xs_integer:
      Result := 'StrToIntDef(' + name + ', 0);';
    xs_long:
      Result := 'StrToIntDef(' + name + ', 0);';
    xs_negativeInteger:
      Result := 'StrToIntDef(' + name + ', 0);';
    xs_nonNegativeInteger:
      Result := 'StrToIntDef(' + name + ', 0);';
    xs_nonPositiveInteger:
      Result := 'StrToIntDef(' + name + ', 0);';
    xs_positiveInteger:
      Result := 'StrToIntDef(' + name + ', 0);';
    xs_short:
      Result := 'StrToIntDef(' + name + ', 0);';
    xs_unsignedLong:
      Result := 'StrToIntDef(' + name + ', 0);';
    xs_unsignedInt:
      Result := 'StrToIntDef(' + name + ', 0);';
    xs_unsignedShort:
      Result := 'StrToIntDef(' + name + ', 0);';
    xs_unsignedByte:
      Result := 'StrToIntDef(' + name + ', 0);';
    xs_anyURI:
      Result := 'StrToIntDef(' + name + ', 0);';
    xs_base64Binary:
      Result := 'StrToIntDef(' + name + ', 0);';
    xs_boolean:
      Result := 'String2Boolean(' + name + ')';
    xs_double:
      Result := 'StrToFloatDef(' + name + ', 0);';
    xs_float:
      Result := 'StrToFloatDef(' + name + ', 0);';
    xs_hexBinary:
      Result := 'String2Hex(' + name + ')';
    xs_NOTATION:
      Result := name;
    xs_Class:
      Result := name;
    xml_Complex:
      Result := 'TComplex.CreateFromString(' + name + ')';
    xml_Complex1D:
      Result := 'String2Complex1D(' + name + ')';
    xml_Coordinate:
      Result := 'TCoordinate.CreateFromString(' + name + ')';
    xml_ArrayCoordinates:
      Result := 'StringToCoordinates(' + name + ')';
    xml_Double1D:
      Result := 'String2Double1D(' + name + ')';
    xml_Integer1D:
      Result := 'String2Integer1D(' + name + ')';
    xml_Integer2D:
      Result := 'String2Integer2D(' + name + ')';
    xml_Integer3D:
      Result := 'String2Integer3D(' + name + ')';
    xml_String1D:
      Result := 'String2String1D(' + name + ')';
    xml_Double2D:
      Result := 'String2Double2D(' + name + ')';
    xml_Double3D:
      Result := 'String2Double3D(' + name + ')';
    xml_Byte1D:
      Result := 'String2Byte1D(' + name + ')';
    xml_Boolean1D:
      Result := 'String2Boolean1D(' + name + ')';
    xml_Byte2D:
      Result := 'String2Byte2D(' + name + ')';
    xml_Boolean2D:
      Result := 'String2Boolean2D(' + name + ')';
    W_Point2I:
      Result := 'StringToPoint2I(' + name + ')';
    W_Point2D:
      Result := 'StringToPoint2D(' + name + ')';
    W_Point3I:
      Result := 'StringToPoint3I(' + name + ')';
    W_Point3D:
      Result := 'StringToPoint3d(' + name + ')';
    W_Point2Is:
      Result := 'StringToPoint2Is(' + name + ')';
    W_Point2Ds:
      Result := 'StringToPoint2Ds(' + name + ')';
    W_Point3Is:
      Result := 'StringToPoint3Is(' + name + ')';
    W_Point3Ds:
      Result := 'StringToPoint3ds(' + name + ')';
    xml_Color:
      Result := 'String2Hex(' + name + ')';
    xml_Pointer:
      Result := 'StringToPointer(' + name + ')';
  end;
end;

function Hex2String(const value: Cardinal): string;
const
  HexStr: Array [0 .. 15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B',
    'C', 'D', 'E', 'F');
var
  carvalue, modvalue: Cardinal;
begin
  carvalue := value;
  while carvalue > 15 do
  begin
    modvalue := carvalue mod 16;
    Result := HexStr[modvalue] + Result;
    carvalue := carvalue div 16;
  end;
  Result := HexStr[carvalue] + Result;
  Result := '$' + Result;
end;

function String2Hex(const value: string): Cardinal;
const
  HexStr: Array [0 .. 15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B',
    'C', 'D', 'E', 'F');
var
  I, J, count: Integer;
begin
  count := Length(value);
  Result := 0;
  for I := 1 to count - 1 do
  begin
    for J := 0 to 15 do
    begin
      if value[J + 1] = HexStr[I] then
      begin
        Result := Result * 16 + J;
        Break;
      end;
    end;
  end;
end;

function Boolean2String(const value: Boolean): String;
begin
  if value then
    Result := '1'
  else
    Result := '0';
end;

function String2Boolean(const value: String): Boolean;
begin
  if (value = '0') or (value = 'false') then
    Result := False
  else if (value = '1') or (value = 'true') then
    Result := True
  else
    Result := False;
end;

function Integer1D2String(const value: Integer1D): String;
var
  I, count: Integer;
begin
  count := Length(value);
  Result := '';
  for I := 0 to count - 1 do
  begin
    Result := Result + ' ' + IntToStr(value[I]);
  end;
  Result := Trim(Result);
end;

function String2Integer1D(const value: String): Integer1D;
var
  strlist: TStringList;
  str1: string;
  I, count: Integer;
begin
  try
    strlist := TStringList.Create;
    ExtractStrings([' '], ['#'], PChar(value), strlist);
    count := strlist.count;
    SetLength(Result, count);
    for I := 0 to count - 1 do
    begin
      Result[I] := StrToInt(strlist[I]);
    end;
    strlist.Free;
  except
    raise Exception.Create('String To Integer1D Error ! ' + value);
  end;
end;

function String1D2String(const value: String1D): String; inline;
var
  I, count: Integer;
begin
  count := Length(value);
  Result := '';
  for I := 0 to count - 1 do
  begin
    Result := Result + ' ' + value[I];
  end;
  Result := Trim(Result);
end;

function String2String1D(const value: String): String1D; inline;
var
  strlist: TStringList;
  str1: string;
  I, count: Integer;
begin
  try
    strlist := TStringList.Create;
    ExtractStrings([' '], ['#'], PChar(value), strlist);
    count := strlist.count;
    SetLength(Result, count);
    for I := 0 to count - 1 do
    begin
      Result[I] := strlist[I];
    end;
    strlist.Free;
  except
    raise Exception.Create('String To String1D Error ! ' + value);
  end;
end;

function Double1D2String(const value: Double1D): String;
var
  I, count: Integer;
begin
  count := Length(value);
  Result := '';
  for I := 0 to count - 1 do
  begin
    Result := Result + ' ' + FloatToStr(value[I]);
  end;
  Result := Trim(Result);
end;

function Complex1D2String(const value: TComplex1D): String;
var
  I, count: Integer;
begin
  count := Length(value);
  Result := '';
  for I := 0 to count - 1 do
  begin
    Result := Result + ' ' + value[I].ToXML;
  end;
  Result := Trim(Result);
end;

function String2Complex1D(const value: String): TComplex1D;
var
  strlist: TStringList;
  str1: string;
  I, count: Integer;
begin
  try
    strlist := TStringList.Create;
    ExtractStrings([' '], ['#'], PChar(value), strlist);
    count := strlist.count;
    SetLength(Result, count);
    for I := 0 to count - 1 do
    begin
      Result[I] := TComplex.CreateFromXML(strlist[I]);
    end;
    strlist.Free;
  except
    raise Exception.Create('String To Complex1D Error ! ' + value);
  end;
end;

function String2Double1D(const value: String): Double1D;
var
  strlist: TStringList;
  str1: string;
  I, count: Integer;
begin
  try
    strlist := TStringList.Create;
    ExtractStrings([' '], ['#'], PChar(value), strlist);
    count := strlist.count;
    SetLength(Result, count);
    for I := 0 to count - 1 do
    begin
      Result[I] := StrToFloat(strlist[I]);
    end;
    strlist.Free;
  except
    raise Exception.Create('String To Double1D Error ! ' + value);
  end;
end;

function Boolean1D2String(const value: Boolean1D): String;
var
  I, count: Integer;
begin
  count := Length(value);
  Result := '';
  for I := 0 to count - 1 do
  begin
    Result := Result + ' ' + Boolean2String(value[I]);
  end;
  Result := Trim(Result);
end;

function String2Boolean1D(const value: String): Boolean1D;
var
  strlist: TStringList;
  str1: string;
  I, count: Integer;
begin
  try
    strlist := TStringList.Create;
    ExtractStrings([' '], ['#'], PChar(value), strlist);
    count := strlist.count;
    SetLength(Result, count);
    for I := 0 to count - 1 do
    begin
      Result[I] := String2Boolean(strlist[I]);
    end;
    strlist.Free;
  except
    raise Exception.Create('String To Boolean1D Error ! ' + value);
  end;
end;

function Integer2D2String(const value: Integer2D): String;
var
  I, count: Integer;
begin
  count := Length(value);
  Result := '';
  for I := 0 to count - 1 do
  begin
    Result := Result + #13#10 + Integer1D2String(value[I]);
  end;
  Result := Trim(Result);
end;

function String2Integer2D(const value: String): Integer2D;
var
  strlist: TStringList;
  str1: string;
  I, count: Integer;
begin
  try
    strlist := TStringList.Create;
    ExtractStrings([#13, #10], ['#'], PChar(value), strlist);
    count := strlist.count;
    SetLength(Result, count);
    for I := 0 to count - 1 do
    begin
      Result[I] := String2Integer1D(strlist[I]);
    end;
    strlist.Free;
  except
    raise Exception.Create('String To Integer2D Error ! ' + value);
  end;
end;

function Integer3D2String(const value: Integer3D): String;
var
  I, count: Integer;
begin
  count := Length(value);
  Result := '';
  for I := 0 to count - 1 do
  begin
    Result := Result + '|' + Integer2D2String(value[I]);
  end;
  Result := Trim(Result);
end;

function String2Integer3D(const value: String): Integer3D;
var
  strlist: TStringList;
  str1: string;
  I, count: Integer;
begin
  try
    strlist := TStringList.Create;
    ExtractStrings(['|'], ['#'], PChar(value), strlist);
    count := strlist.count;
    SetLength(Result, count);
    for I := 0 to count - 1 do
    begin
      Result[I] := String2Integer2D(strlist[I]);
    end;
    strlist.Free;
  except
    raise Exception.Create('String To Integer3D Error ! ' + value);
  end;
end;

function Double2D2String(const value: Double2D): String;
var
  I, count: Integer;
begin
  count := Length(value);
  Result := '';
  for I := 0 to count - 1 do
  begin
    Result := Result + #13#10 + Double1D2String(value[I]);
  end;
  Result := Trim(Result);
end;

function String2Double2D(const value: String): Double2D;
var
  strlist: TStringList;
  str1: string;
  I, count: Integer;
begin
  try
    strlist := TStringList.Create;
    ExtractStrings([#13, #10], ['#'], PChar(value), strlist);
    count := strlist.count;
    SetLength(Result, count);
    for I := 0 to count - 1 do
    begin
      Result[I] := String2Double1D(strlist[I]);
    end;
    strlist.Free;
  except
    raise Exception.Create('String To Double2D Error ! ' + value);
  end;
end;

function Double3D2String(const value: Double3D): String;
var
  I, count: Integer;
begin
  count := Length(value);
  Result := '';
  for I := 0 to count - 1 do
  begin
    Result := Result + '|' + Double2D2String(value[I]);
  end;
  Result := Trim(Result);
end;

function String2Double3D(const value: String): Double3D;
var
  strlist: TStringList;
  str1: string;
  I, count: Integer;
begin
  try
    strlist := TStringList.Create;
    ExtractStrings(['|'], ['#'], PChar(value), strlist);
    count := strlist.count;
    SetLength(Result, count);
    for I := 0 to count - 1 do
    begin
      Result[I] := String2Double2D(strlist[I]);
    end;
    strlist.Free;
  except
    raise Exception.Create('String To Double3D Error ! ' + value);
  end;
end;

function Boolean2D2String(const value: Boolean2D): String;
var
  I, count: Integer;
begin
  count := Length(value);
  Result := '';
  for I := 0 to count - 1 do
  begin
    Result := Result + #13#10 + Boolean1D2String(value[I]);
  end;
  Result := Trim(Result);
end;

function String2Boolean2D(const value: String): Boolean2D;
var
  strlist: TStringList;
  str1: string;
  I, count: Integer;
begin
  try
    strlist := TStringList.Create;
    ExtractStrings([#13, #10], ['#'], PChar(value), strlist);
    count := strlist.count;
    SetLength(Result, count);
    for I := 0 to count - 1 do
    begin
      Result[I] := String2Boolean1D(strlist[I]);
    end;
    strlist.Free;
  except
    raise Exception.Create('String To Boolean2D Error ! ' + value);
  end;
end;

function CoordinatesToString(const coors: ArrayCoordinates): String;
var
  I, count: Integer;
  coor: TCoordinate;
begin
  count := Length(coors);
  Result := '';
  for I := 0 to count - 1 do
  begin
    coor := coors[I];
    Result := Result + coor.ToString + ' ';
  end;
end;

function StringToCoordinates(const str: string): ArrayCoordinates;
var
  strlist: TStringList;
  I, count: Integer;
begin
  try
    strlist := TStringList.Create;
    ExtractStrings([';', ':', ' ', #13], ['#'], PChar(str), strlist);
    count := strlist.count;
    for I := count - 1 downto 0 do
      if Trim(strlist[I]) = '' then
        strlist.Delete(I);
    count := strlist.count;
    SetLength(Result, count);
    for I := 0 to count - 1 do
    begin
      Result[I] := TCoordinate.CreateFromString(strlist[I]);
    end;
    strlist.Free;
  except
    raise Exception.Create('String To Coordinates Error ! ' + str);
  end;
end;

constructor TCoordinate.Create(const lon, lat, alt: Double);
begin
  Longitude := lon;
  Latitude := lat;
  Altitude := alt;
end;

constructor TCoordinate.CreateFromString(const str: string);
var
  strlist: TStringList;
  str1: string;
begin
  try
    str1 := Trim(str);
    strlist := TStringList.Create;
    ExtractStrings([','], [], PChar(str1), strlist);
    Longitude := StrToFloat(strlist[0]);
    Latitude := StrToFloat(strlist[1]);
    Altitude := StrToFloat(strlist[2]);
    strlist.Free;
  except
    raise Exception.Create('String To Coordinate Error:' + str);
  end;
end;

function TCoordinate.ToString: string;
begin
  Result := FloatToStr(Longitude) + ',' + FloatToStr(Latitude) + ',' + FloatToStr(Altitude);
end;

{ WPoint2D }

constructor WPoint2D.Create(const _x, _y: Double);
begin
  x := _x;
  y := _y;
end;

constructor WPoint2D.CreateFromString(const str: string);
var
  strlist: TStringList;
  str1: string;
begin
  try
    str1 := Trim(str);
    strlist := TStringList.Create;
    ExtractStrings([','], [], PChar(str1), strlist);
    x := StrToFloat(strlist[0]);
    y := StrToFloat(strlist[1]);
    strlist.Free;
  except
    raise Exception.Create('String To Point2d Error:' + str);
  end;
end;

function WPoint2D.ToString: string;
begin
  Result := FloatToStr(x) + ',' + FloatToStr(y);
end;

{ WPoint2I }

constructor WPoint2I.Create(const _x, _y: Integer);
begin
  x := _x;
  y := _y;
end;

constructor WPoint2I.CreateFromString(const str: string);
var
  strlist: TStringList;
  str1: string;
begin
  try
    str1 := Trim(str);
    strlist := TStringList.Create;
    ExtractStrings([','], [], PChar(str1), strlist);
    x := StrToInt(strlist[0]);
    y := StrToInt(strlist[1]);
    strlist.Free;
  except
    raise Exception.Create('String To Point2d Error:' + str);
  end;
end;

function WPoint2I.ToString: string;
begin
  Result := IntToStr(x) + ',' + IntToStr(y);
end;

{ WPoint3D }

constructor WPoint3D.Create(const _x, _y, _z: Double);
begin
  x := _x;
  y := _y;
  z := _z;
end;

constructor WPoint3D.CreateFromString(const str: string);
var
  strlist: TStringList;
  str1: string;
begin
  try
    str1 := Trim(str);
    strlist := TStringList.Create;
    ExtractStrings([','], [], PChar(str1), strlist);
    x := StrToFloat(strlist[0]);
    y := StrToFloat(strlist[1]);
    z := StrToFloat(strlist[2]);
    strlist.Free;
  except
    raise Exception.Create('String To Point2d Error:' + str);
  end;
end;

function WPoint3D.ToString: string;
begin
  Result := FloatToStr(x) + ',' + FloatToStr(y) + ',' + FloatToStr(z);
end;

{ WPoint3I }

constructor WPoint3I.Create(const _x, _y, _z: Integer);
begin
  x := _x;
  y := _y;
  z := _z;
end;

constructor WPoint3I.CreateFromString(const str: string);
var
  strlist: TStringList;
  str1: string;
begin
  try
    str1 := Trim(str);
    strlist := TStringList.Create;
    ExtractStrings([','], [], PChar(str1), strlist);
    x := StrToInt(strlist[0]);
    y := StrToInt(strlist[1]);
    z := StrToInt(strlist[2]);
    strlist.Free;
  except
    raise Exception.Create('String To Point2d Error:' + str);
  end;
end;

function WPoint3I.ToString: string;
begin
  Result := IntToStr(x) + ',' + IntToStr(y) + ',' + IntToStr(z);
end;

function StringToPoint2D(const str: string): WPoint2D;
begin
  Result := WPoint2D.CreateFromString(str);
end;

function StringToPoint2I(const str: string): WPoint2I;
begin
  Result := WPoint2I.CreateFromString(str);
end;

function StringToPoint3D(const str: string): WPoint3D;
begin
  Result := WPoint3D.CreateFromString(str);
end;

function StringToPoint3I(const str: string): WPoint3I;
begin
  Result := WPoint3I.CreateFromString(str);
end;
//
// function StringToPoint2Ds(const str: string): WPoint2Ds;
// var
// strlist: TStringList;
// str1: string;
// I, Count: Integer;
// reg: TPerlRegEx;
// begin
// try
// reg := TPerlRegEx.Create(nil);
// reg.Subject := Trim(str);
// reg.RegEx := ' ';
// strlist := TStringList.Create;
// reg.Split(strlist, MaxInt);
// Count := strlist.Count;
// for I := Count - 1 downto 0 do
// if Trim(strlist[I]) = '' then
// strlist.Delete(I);
// Count := strlist.Count;
// SetLength(Result, Count);
// for I := 0 to Count - 1 do
// begin
// Result[I] := WPoint2D.CreateFromString(strlist[I]);
// end;
// strlist.Free;
// reg.Free;
// except
// raise Exception.Create('String To Coordinates Error ! ' + str);
// end;
// end;

// function StringToPoint2Is(const str: string): WPoint2Is;
// var
// strlist: TStringList;
// str1: string;
// I, Count: Integer;
// reg: TPerlRegEx;
// begin
// try
// reg := TPerlRegEx.Create(nil);
// reg.Subject := Trim(str);
// reg.RegEx := ' ';
// strlist := TStringList.Create;
// reg.Split(strlist, MaxInt);
// Count := strlist.Count;
// for I := Count - 1 downto 0 do
// if Trim(strlist[I]) = '' then
// strlist.Delete(I);
// Count := strlist.Count;
// SetLength(Result, Count);
// for I := 0 to Count - 1 do
// begin
// Result[I] := WPoint2I.CreateFromString(strlist[I]);
// end;
// strlist.Free;
// reg.Free;
// except
// raise Exception.Create('String To Coordinates Error ! ' + str);
// end;
// end;

// function StringToPoint3Ds(const str: string): WPoint3Ds;
// var
// strlist: TStringList;
// str1: string;
// I, Count: Integer;
// reg: TPerlRegEx;
// begin
// try
// reg := TPerlRegEx.Create(nil);
// reg.Subject := Trim(str);
// reg.RegEx := ' ';
// strlist := TStringList.Create;
// reg.Split(strlist, MaxInt);
// Count := strlist.Count;
// for I := Count - 1 downto 0 do
// if Trim(strlist[I]) = '' then
// strlist.Delete(I);
// Count := strlist.Count;
// SetLength(Result, Count);
// for I := 0 to Count - 1 do
// begin
// Result[I] := WPoint3D.CreateFromString(strlist[I]);
// end;
// strlist.Free;
// reg.Free;
// except
// raise Exception.Create('String To Coordinates Error ! ' + str);
// end;
// end;

// function StringToPoint3Is(const str: string): WPoint3Is;
// var
// strlist: TStringList;
// str1: string;
// I, Count: Integer;
// reg: TPerlRegEx;
// begin
// try
// reg := TPerlRegEx.Create(nil);
// reg.Subject := Trim(str);
// reg.RegEx := ' ';
// strlist := TStringList.Create;
// reg.Split(strlist, MaxInt);
// Count := strlist.Count;
// for I := Count - 1 downto 0 do
// if Trim(strlist[I]) = '' then
// strlist.Delete(I);
// Count := strlist.Count;
// SetLength(Result, Count);
// for I := 0 to Count - 1 do
// begin
// Result[I] := WPoint3I.CreateFromString(strlist[I]);
// end;
// strlist.Free;
// reg.Free;
// except
// raise Exception.Create('String To Coordinates Error ! ' + str);
// end;
// end;

function Point2DToString(const p: WPoint2D): String;
begin
  Result := p.ToString;
end;

function Point2IToString(const p: WPoint2I): String;
begin
  Result := p.ToString;
end;

function Point3DToString(const p: WPoint3D): String;
begin
  Result := p.ToString;
end;

function Point3IToString(const p: WPoint3I): String;
begin
  Result := p.ToString;
end;

function Point2DsToString(const p: WPoint2Ds): String;
var
  I, count: Integer;
  wp: WPoint2D;
begin
  count := Length(p);
  Result := '';
  for I := 0 to count - 1 do
  begin
    wp := p[I];
    Result := Result + wp.ToString + ' ';
  end;
end;

function Point2IsToString(const p: WPoint2Is): String;
var
  I, count: Integer;
  wp: WPoint2I;
begin
  count := Length(p);
  Result := '';
  for I := 0 to count - 1 do
  begin
    wp := p[I];
    Result := Result + wp.ToString + ' ';
  end;
end;

function Point3DsToString(const p: WPoint3Ds): String;
var
  I, count: Integer;
  wp: WPoint3D;
begin
  count := Length(p);
  Result := '';
  for I := 0 to count - 1 do
  begin
    wp := p[I];
    Result := Result + wp.ToString + ' ';
  end;
end;

function Point3IsToString(const p: WPoint3Is): String;
var
  I, count: Integer;
  wp: WPoint3I;
begin
  count := Length(p);
  Result := '';
  for I := 0 to count - 1 do
  begin
    wp := p[I];
    Result := Result + wp.ToString + ' ';
  end;
end;

class Operator TComplex.Add(Z1, Z2: TComplex): TComplex;
begin
  Result.Real := Z1.Real + Z2.Real;
  Result.Imag := Z1.Imag + Z2.Imag;
end;

class Operator TComplex.Add(z: TComplex; R: Double): TComplex;
begin
  Result.Real := R + z.Real;
  Result.Imag := z.Imag;
end;

class Operator TComplex.Add(R: Double; z: TComplex): TComplex;
begin
  Result := z + R;
end;

function TComplex.Conjugate: TComplex;
begin
  Result.Real := Real;
  Result.Imag := -Imag;
end;

function TComplex.Cos: TComplex;
Var
  Z1, Z2: TComplex;
  Eip, Ein: TComplex;
Begin
  Eip := TComplex.Create(0, 1);
  Ein := TComplex.Create(0, -1);
  Z1 := Eip * Self;
  Z2 := Ein * Self;
  Result := Z1.Exp + Z2.Exp;
  With Result Do
  Begin
    Real := Real / 2;
    Imag := Imag / 2;
  end;
End;

constructor TComplex.Create(rvalue, ivalue: Double);
begin
  Real := rvalue;
  Imag := ivalue;
end;

constructor TComplex.CreateAP(Avalue, Pvalue: Double);
begin
  Real := Avalue * System.Cos(Pvalue);
  Imag := Avalue * System.Sin(Pvalue);
end;

constructor TComplex.CreateFromString(str: String);
Var
  strlist: TStringList;
begin
  Try
    strlist := TStringList.Create;
    ExtractStrings([' '], ['#'], PChar(str), strlist);
    Self.Real := StrToFloat(strlist[0]);
    Self.Imag := StrToFloat(strlist[1]);
    strlist.Free;
  Except
    Raise ;
  end;
end;

constructor TComplex.CreateFromXML(str: String);
Var
  strlist: TStringList;
begin
  Try
    strlist := TStringList.Create;
    ExtractStrings([','], ['#'], PChar(str), strlist);
    Self.Real := StrToFloat(strlist[0]);
    Self.Imag := StrToFloat(strlist[1]);
    strlist.Free;
  Except
    Raise ;
  end;
end;

class Operator TComplex.Divide(Z1, Z2: TComplex): TComplex;

Var
  z: TComplex;
  Mode_Z2: Double;
Begin
  Try
    Mode_Z2 := System.Sqr(Z2.Module);
    z := Z2.Conjugate;
    Result := Z1 * z;
    With Result Do
    Begin
      Real := Real / Mode_Z2;
      Imag := Imag / Mode_Z2;
    End;
  Except
    Raise ;
  End;
End;

class Operator TComplex.Divide(z: TComplex; R: Double): TComplex;
begin
  Try
    With Result Do
    Begin
      Real := z.Real / R;
      Imag := z.Imag / R;
    End;
  Except
    Raise ;
  End;
end;

class Operator TComplex.Divide(R: Double; Z2: TComplex): TComplex;

Var
  Z1: TComplex;
Begin
  Try
    Z1.Real := R;
    Z1.Imag := 0;
    Result := Z1 / Z2;
  Except
    Raise ;
  End;
End;

function TComplex.Exp: TComplex;
// var
// tmp: Double;
begin
  Result.Real := System.Exp(Real) * System.Cos(Imag);
  Result.Imag := System.Exp(Real) * System.Sin(Imag);
  // if (Real < 10) and (Imag < 10) then
  // Begin
  // Result.Real := System.Exp(Real) * System.Cos(Imag);
  // Result.Imag := System.Exp(Real) * System.Sin(Imag);
  // end
  // Else
  // Begin
  // Result.Real := System.Exp(Real + System.Ln(System.Cos(Imag)));
  // Result.Imag := System.Exp(Real + System.Ln(System.Sin(Imag)));
  // end;
end;

procedure TComplex.FromString(str: String);
Var
  strlist: TStringList;
begin
  Try
    strlist := TStringList.Create;
    ExtractStrings([' '], ['#'], PChar(str), strlist);
    Self.Real := StrToFloat(strlist[0]);
    Self.Imag := StrToFloat(strlist[1]);
    strlist.Free;
  Except
    Raise ;
  end;
end;

procedure TComplex.FromXML(str: String);
Var
  strlist: TStringList;
begin
  Try
    strlist := TStringList.Create;
    ExtractStrings([','], ['#'], PChar(str), strlist);
    Self.Real := StrToFloat(strlist[0]);
    Self.Imag := StrToFloat(strlist[1]);
    strlist.Free;
  Except
    Raise ;
  end;
end;

function TComplex.Ln: TComplex;
begin
  With Result Do
  Begin
    Real := System.Ln(Self.Module);
    Imag := Self.Phase;
  End;
end;

class Operator TComplex.Multiply(Z1, Z2: TComplex): TComplex;
begin
  Result.Real := Z1.Real * Z2.Real - Z1.Imag * Z2.Imag;
  Result.Imag := Z1.Imag * Z2.Real + Z1.Real * Z2.Imag;
end;

class Operator TComplex.Multiply(z: TComplex; R: Double): TComplex;
begin
  Result.Real := R * z.Real;
  Result.Imag := R * z.Imag;
end;

function TComplex.Module: Double;
begin
  Result := System.Sqrt(System.Sqr(Real) + System.Sqr(Imag));
end;

function TComplex.Module2: Double;
begin
  Result := System.Sqr(Real) + System.Sqr(Imag);
end;

class Operator TComplex.Multiply(R: Double; z: TComplex): TComplex;
begin
  Result.Real := R * z.Real;
  Result.Imag := R * z.Imag;
end;

class Operator TComplex.Negative(z: TComplex): TComplex;
begin
  With Result Do
  Begin
    Real := -z.Real;
    Imag := -z.Imag;
  end;
end;

function TComplex.Phase: Double; { -Pi~Pi之间 }
begin
  Try
    If Real <> 0 Then
      Result := Arctan(Imag / Real)
    Else
      Result := PI / 2.;
    if Real <= 0 Then
    Begin
      If Imag > 0 Then
        Result := PI + Result;
      If Imag <= 0 Then
        Result := Result - PI;
    End;
  Except
    Raise ;
  end;
end;

function TComplex.Sin: TComplex;
Var
  Z1, Z2: TComplex;
  Eip, Ein: TComplex;
Begin
  Eip := TComplex.Create(0, 1);
  Ein := TComplex.Create(0, -1);
  Z1 := Eip * Self;
  Z2 := Ein * Self;
  Result := Z1.Exp - Z2.Exp;
  With Result Do
  Begin
    Real := Real / 2;
    Imag := Imag / 2;
  end;
End;

function TComplex.Sqr: TComplex;
begin
  With Result Do
  Begin
    Real := System.Sqr(Real) - System.Sqr(Imag);
    Imag := 2 * Real * Imag;
  end;
end;

function TComplex.Sqrt: TComplex;
Var
  Mdl, Phs: Double;
Begin
  Mdl := Self.Module;
  Phs := Self.Phase / 2.0;
  If System.Cos(Phs) < 0 then
    Phs := PI + Phs;
  With Result Do
  Begin
    Real := System.Sqrt(Mdl) * System.Cos(Phs);
    Imag := System.Sqrt(Mdl) * System.Sin(Phs);
  end;
End;

class Operator TComplex.Subtract(R: Double; z: TComplex): TComplex;
begin
  Result.Real := R - z.Real;
  Result.Imag := z.Imag;
end;

function TComplex.ToString: String;
begin
  Result := '(' + FloatToStr(Real) + ',' + FloatToStr(Imag) + ')';
end;

function TComplex.ToXML: String;
begin
  Result := FloatToStr(Real) + ' ' + FloatToStr(Imag);
end;

class Operator TComplex.Subtract(Z1, Z2: TComplex): TComplex;
begin
  Result.Real := Z1.Real - Z2.Real;
  Result.Imag := Z1.Imag - Z2.Imag;
end;

class Operator TComplex.Subtract(z: TComplex; R: Double): TComplex;
begin
  Result.Real := z.Real - R;
  Result.Imag := z.Imag;
end;

function Double2D21D(value: Double2D): Double1D;
var
  I, J, count, len: Integer;
begin
  count := Length(value);
  len := Length(value[0]);
  SetLength(Result, count * len);
  for I := 0 to count - 1 do
  begin
    for J := 0 to len - 1 do
    begin
      Result[I * len + J] := value[I][J];
    end;
  end;
end;

function Double1D22D(value: Double1D; count: Integer): Double2D;
var
  I, J, len: Integer;
begin
  len := Length(value) div count;
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    SetLength(Result[I], len);
    for J := 0 to len - 1 do
    begin
      Result[I][J] := value[I * len + J];
    end;
  end;
end;

initialization

XMLDateFormat.ShortDateFormat := 'yyyy-MM-dd';
XMLTimeFormat.ShortTimeFormat := 'hh:mm:ss';
XMLDateTimeFormat.ShortDateFormat := 'yyyy-MM-dd';
XMLDateTimeFormat.ShortTimeFormat := 'hh:mm:ss';
XMLDateTimeFormat.DateSeparator := '-';
XMLDateTimeFormat.TimeSeparator := ':';

finalization

end.
