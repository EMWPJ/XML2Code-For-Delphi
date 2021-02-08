unit KML;

interface

uses
  Windows, SysUtils, Types, Classes, Variants, Generics.Collections, Dialogs, Controls, ExtCtrls,
  XMLCore, XMLDoc, XMLIntf, XMLLeafTypes,KMLFile;


type

  TKML = class;

  TKML = class(TXMLTree)
  private
    FTKMLFileObj: TKMLFile;
    procedure SetObj(const Value: TKMLFile);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    function ToGeoObj: TObject; override;
    procedure FromGeoObj(const Obj: TObject); override;
    { TKMLFile }
    procedure TKMLFile_FromXML(Obj: TKMLFile; node: IXMLNode);
    procedure TKMLFile_ToXML(Obj: TKMLFile; par: IXMLNode; pt: string = 'kml');
    { TKMLDocument }
    procedure TKMLDocument_FromXML(Obj: TKMLDocument; node: IXMLNode);
    procedure TKMLDocument_ToXML(Obj: TKMLDocument; par: IXMLNode; pt: string = 'Document');
    { TKMLFolder }
    procedure TKMLFolder_FromXML(Obj: TKMLFolder; node: IXMLNode);
    procedure TKMLFolder_ToXML(Obj: TKMLFolder; par: IXMLNode; pt: string = 'Folder');
    { TKMLPlacemark }
    procedure TKMLPlacemark_FromXML(Obj: TKMLPlacemark; node: IXMLNode);
    procedure TKMLPlacemark_ToXML(Obj: TKMLPlacemark; par: IXMLNode; pt: string = 'Placemark');
    { TKMLGeometry }
    procedure TKMLGeometry_FromXML(Obj: TKMLGeometry; node: IXMLNode); virtual;
    procedure TKMLGeometry_ToXML(Obj: TKMLGeometry; par: IXMLNode; pt: string = '#Optional'); virtual;
    { TKMLMultiGeometry }
    procedure TKMLMultiGeometry_FromXML(Obj: TKMLMultiGeometry; node: IXMLNode);
    procedure TKMLMultiGeometry_ToXML(Obj: TKMLMultiGeometry; par: IXMLNode; pt: string = 'MultiGeometry');
    { TKMLPoint }
    procedure TKMLPoint_FromXML(Obj: TKMLPoint; node: IXMLNode);
    procedure TKMLPoint_ToXML(Obj: TKMLPoint; par: IXMLNode; pt: string = 'Point');
    { TKMLPolygon }
    procedure TKMLPolygon_FromXML(Obj: TKMLPolygon; node: IXMLNode);
    procedure TKMLPolygon_ToXML(Obj: TKMLPolygon; par: IXMLNode; pt: string = 'Polygon');
    { TKMLBoundary }
    procedure TKMLBoundary_FromXML(Obj: TKMLBoundary; node: IXMLNode);
    procedure TKMLBoundary_ToXML(Obj: TKMLBoundary; par: IXMLNode; pt: string = 'Boundary');
    { TKMLLinearRing }
    procedure TKMLLinearRing_FromXML(Obj: TKMLLinearRing; node: IXMLNode);
    procedure TKMLLinearRing_ToXML(Obj: TKMLLinearRing; par: IXMLNode; pt: string = 'LinearRing');
    { TKMLLineString }
    procedure TKMLLineString_FromXML(Obj: TKMLLineString; node: IXMLNode);
    procedure TKMLLineString_ToXML(Obj: TKMLLineString; par: IXMLNode; pt: string = 'LineString');
    { TKMLLookAt }
    procedure TKMLLookAt_FromXML(Obj: TKMLLookAt; node: IXMLNode);
    procedure TKMLLookAt_ToXML(Obj: TKMLLookAt; par: IXMLNode; pt: string = 'LookAt');
    { TKMLLatLonBox }
    procedure TKMLLatLonBox_FromXML(Obj: TKMLLatLonBox; node: IXMLNode);
    procedure TKMLLatLonBox_ToXML(Obj: TKMLLatLonBox; par: IXMLNode; pt: string = 'LatLonBox');
    { TKMLGroundOverlay }
    procedure TKMLGroundOverlay_FromXML(Obj: TKMLGroundOverlay; node: IXMLNode);
    procedure TKMLGroundOverlay_ToXML(Obj: TKMLGroundOverlay; par: IXMLNode; pt: string = 'GroundOverlay');
    { TKMLExtendedData }
    procedure TKMLExtendedData_FromXML(Obj: TKMLExtendedData; node: IXMLNode);
    procedure TKMLExtendedData_ToXML(Obj: TKMLExtendedData; par: IXMLNode; pt: string = 'ExtendedData');
    { TKMLData }
    procedure TKMLData_FromXML(Obj: TKMLData; node: IXMLNode);
    procedure TKMLData_ToXML(Obj: TKMLData; par: IXMLNode; pt: string = 'Data');
    { TKMLSize }
    procedure TKMLSize_FromXML(Obj: TKMLSize; node: IXMLNode);
    procedure TKMLSize_ToXML(Obj: TKMLSize; par: IXMLNode; pt: string = 'Size');
    { TKMLStyle }
    procedure TKMLStyle_FromXML(Obj: TKMLStyle; node: IXMLNode);
    procedure TKMLStyle_ToXML(Obj: TKMLStyle; par: IXMLNode; pt: string = 'Style');
    { TKMLIconStyle }
    procedure TKMLIconStyle_FromXML(Obj: TKMLIconStyle; node: IXMLNode);
    procedure TKMLIconStyle_ToXML(Obj: TKMLIconStyle; par: IXMLNode; pt: string = 'IconStyle');
    { TKMLIcon }
    procedure TKMLIcon_FromXML(Obj: TKMLIcon; node: IXMLNode);
    procedure TKMLIcon_ToXML(Obj: TKMLIcon; par: IXMLNode; pt: string = 'Icon');
    { TKMLLineStyle }
    procedure TKMLLineStyle_FromXML(Obj: TKMLLineStyle; node: IXMLNode);
    procedure TKMLLineStyle_ToXML(Obj: TKMLLineStyle; par: IXMLNode; pt: string = 'LineStyle');
    { TKMLBalloonStyle }
    procedure TKMLBalloonStyle_FromXML(Obj: TKMLBalloonStyle; node: IXMLNode);
    procedure TKMLBalloonStyle_ToXML(Obj: TKMLBalloonStyle; par: IXMLNode; pt: string = 'BalloonStyle');
    { TKMLStyleMap }
    procedure TKMLStyleMap_FromXML(Obj: TKMLStyleMap; node: IXMLNode);
    procedure TKMLStyleMap_ToXML(Obj: TKMLStyleMap; par: IXMLNode; pt: string = 'StyleMap');
    { TKMLScreenOverlay }
    procedure TKMLScreenOverlay_FromXML(Obj: TKMLScreenOverlay; node: IXMLNode);
    procedure TKMLScreenOverlay_ToXML(Obj: TKMLScreenOverlay; par: IXMLNode; pt: string = 'ScreenOverlay');
    { TKMLPair }
    procedure TKMLPair_FromXML(Obj: TKMLPair; node: IXMLNode);
    procedure TKMLPair_ToXML(Obj: TKMLPair; par: IXMLNode; pt: string = 'Pair');
  public
    property TKMLFileObj: TKMLFile read FTKMLFileObj write SetObj;
  end;

implementation

procedure TKML.FromXML(node: IXMLNode);
begin
  inherited;
  if not Assigned(FTKMLFileObj) then
    FTKMLFileObj := TKMLFile.Create;
  TKMLFile_FromXML(FTKMLFileObj, node);
end;


function TKML.ToXML(par: IXMLNode; pt: string): IXMLNode;
begin
  if Assigned(FTKMLFileObj) then
    TKMLFile_ToXML(FTKMLFileObj, par, pt);
end;


procedure TKML.FromGeoObj(const Obj: TObject);
begin
  inherited;
  FTKMLFileObj := TKMLFile(Obj);
end;


function TKML.ToGeoObj: TObject;
begin
  Result := FTKMLFileObj;
end;


procedure TKML.SetObj(const Value: TKMLFile);
begin
  FTKMLFileObj := Value;
end;


procedure TKML.TKMLFile_FromXML(Obj: TKMLFile; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  FolderTmp: TKMLFolder;
begin
  try
    Obj.XmlnsExsit := False;
    Obj.FolderClear;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Document' then
      begin
        Obj.Document := TKMLDocument.Create;
        TKMLDocument_FromXML(Obj.Document, nodeTmp);
      end
      else if nodeTmp.NodeName = 'Folder' then
      begin
        FolderTmp := TKMLFolder.Create;
        TKMLFolder_FromXML(FolderTmp, nodeTmp);
        Obj.AddFolder(FolderTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'xmlns' then
      begin
        Obj.Xmlns := nodeTmp.Text;
        Obj.XmlnsExsit := True;
      end;
    end;
  except
    raise Exception.Create('KMLFile Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLFile_ToXML(Obj: TKMLFile; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  XmlnsTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'kml';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    TKMLDocument_ToXML(Obj.Document, node, 'Document');
    for I := 0 to Obj.Folders.Count - 1 do
       TKMLFolder_ToXML(Obj.Folder[I], node, 'Folder');
    if Obj.XmlnsExsit then 
    begin
      XmlnsTmp := doc.CreateNode('xmlns', ntAttribute);
      XmlnsTmp.NodeValue := Obj.Xmlns;
      node.AttributeNodes.Add(XmlnsTmp);
    end;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLDocument_FromXML(Obj: TKMLDocument; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  StyleMapTmp: TKMLStyleMap;
  StyleTmp: TKMLStyle;
  PlacemarkTmp: TKMLPlacemark;
  FolderTmp: TKMLFolder;
begin
  try
    Obj.NameExsit := False;
    Obj.OpenExsit := False;
    Obj.VisibilityExsit := False;
    Obj.DescriptionExsit := False;
    Obj.LookAtExsit := False;
    Obj.StyleMapClear;
    Obj.StyleClear;
    Obj.PlacemarkClear;
    Obj.FolderClear;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        Obj.Name := nodeTmp.Text;
        Obj.NameExsit := True;
      end
      else if nodeTmp.NodeName = 'open' then
      begin
        Obj.Open := StrToIntDef(nodeTmp.Text, 0);;
        Obj.OpenExsit := True;
      end
      else if nodeTmp.NodeName = 'visibility' then
      begin
        Obj.Visibility := StrToIntDef(nodeTmp.Text, 0);;
        Obj.VisibilityExsit := True;
      end
      else if nodeTmp.NodeName = 'description' then
      begin
        Obj.Description := nodeTmp.Text;
        Obj.DescriptionExsit := True;
      end
      else if nodeTmp.NodeName = 'LookAt' then
      begin
        Obj.LookAt := TKMLLookAt.Create;
        TKMLLookAt_FromXML(Obj.LookAt, nodeTmp);
        Obj.LookAtExsit := True;
      end
      else if nodeTmp.NodeName = 'StyleMap' then
      begin
        StyleMapTmp := TKMLStyleMap.Create;
        TKMLStyleMap_FromXML(StyleMapTmp, nodeTmp);
        Obj.AddStyleMap(StyleMapTmp);
      end
      else if nodeTmp.NodeName = 'Style' then
      begin
        StyleTmp := TKMLStyle.Create;
        TKMLStyle_FromXML(StyleTmp, nodeTmp);
        Obj.AddStyle(StyleTmp);
      end
      else if nodeTmp.NodeName = 'Placemark' then
      begin
        PlacemarkTmp := TKMLPlacemark.Create;
        TKMLPlacemark_FromXML(PlacemarkTmp, nodeTmp);
        Obj.AddPlacemark(PlacemarkTmp);
      end
      else if nodeTmp.NodeName = 'Folder' then
      begin
        FolderTmp := TKMLFolder.Create;
        TKMLFolder_FromXML(FolderTmp, nodeTmp);
        Obj.AddFolder(FolderTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Document Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLDocument_ToXML(Obj: TKMLDocument; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  OpenTmp: IXMLNode;
  VisibilityTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Document';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if Obj.NameExsit then
    begin
      NameTmp := doc.CreateNode('name', ntElement);
      NameTmp.NodeValue := Obj.Name;
      node.ChildNodes.Add(NameTmp);
    end;
    if Obj.OpenExsit then
    begin
      OpenTmp := doc.CreateNode('open', ntElement);
      OpenTmp.NodeValue := IntToStr(Obj.Open);
      node.ChildNodes.Add(OpenTmp);
    end;
    if Obj.VisibilityExsit then
    begin
      VisibilityTmp := doc.CreateNode('visibility', ntElement);
      VisibilityTmp.NodeValue := IntToStr(Obj.Visibility);
      node.ChildNodes.Add(VisibilityTmp);
    end;
    if Obj.DescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('description', ntElement);
      DescriptionTmp.NodeValue := Obj.Description;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    if Obj.LookAtExsit then
      TKMLLookAt_ToXML(Obj.LookAt, node, 'LookAt');
    for I := 0 to Obj.StyleMaps.Count - 1 do
       TKMLStyleMap_ToXML(Obj.StyleMap[I], node, 'StyleMap');
    for I := 0 to Obj.Styles.Count - 1 do
       TKMLStyle_ToXML(Obj.Style[I], node, 'Style');
    for I := 0 to Obj.Placemarks.Count - 1 do
       TKMLPlacemark_ToXML(Obj.Placemark[I], node, 'Placemark');
    for I := 0 to Obj.Folders.Count - 1 do
       TKMLFolder_ToXML(Obj.Folder[I], node, 'Folder');
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLFolder_FromXML(Obj: TKMLFolder; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  GroundOverlayTmp: TKMLGroundOverlay;
  PlacemarkTmp: TKMLPlacemark;
  ScreenOverlayTmp: TKMLScreenOverlay;
  FolderTmp: TKMLFolder;
begin
  try
    Obj.NameExsit := False;
    Obj.OpenExsit := False;
    Obj.VisibilityExsit := False;
    Obj.DescriptionExsit := False;
    Obj.LookAtExsit := False;
    Obj.DocumentExsit := False;
    Obj.GroundOverlayClear;
    Obj.PlacemarkClear;
    Obj.ScreenOverlayClear;
    Obj.FolderClear;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        Obj.Name := nodeTmp.Text;
        Obj.NameExsit := True;
      end
      else if nodeTmp.NodeName = 'open' then
      begin
        Obj.Open := StrToIntDef(nodeTmp.Text, 0);;
        Obj.OpenExsit := True;
      end
      else if nodeTmp.NodeName = 'visibility' then
      begin
        Obj.Visibility := StrToIntDef(nodeTmp.Text, 0);;
        Obj.VisibilityExsit := True;
      end
      else if nodeTmp.NodeName = 'description' then
      begin
        Obj.Description := nodeTmp.Text;
        Obj.DescriptionExsit := True;
      end
      else if nodeTmp.NodeName = 'LookAt' then
      begin
        Obj.LookAt := TKMLLookAt.Create;
        TKMLLookAt_FromXML(Obj.LookAt, nodeTmp);
        Obj.LookAtExsit := True;
      end
      else if nodeTmp.NodeName = 'Document' then
      begin
        Obj.Document := TKMLDocument.Create;
        TKMLDocument_FromXML(Obj.Document, nodeTmp);
        Obj.DocumentExsit := True;
      end
      else if nodeTmp.NodeName = 'GroundOverlay' then
      begin
        GroundOverlayTmp := TKMLGroundOverlay.Create;
        TKMLGroundOverlay_FromXML(GroundOverlayTmp, nodeTmp);
        Obj.AddGroundOverlay(GroundOverlayTmp);
      end
      else if nodeTmp.NodeName = 'Placemark' then
      begin
        PlacemarkTmp := TKMLPlacemark.Create;
        TKMLPlacemark_FromXML(PlacemarkTmp, nodeTmp);
        Obj.AddPlacemark(PlacemarkTmp);
      end
      else if nodeTmp.NodeName = 'ScreenOverlay' then
      begin
        ScreenOverlayTmp := TKMLScreenOverlay.Create;
        TKMLScreenOverlay_FromXML(ScreenOverlayTmp, nodeTmp);
        Obj.AddScreenOverlay(ScreenOverlayTmp);
      end
      else if nodeTmp.NodeName = 'Folder' then
      begin
        FolderTmp := TKMLFolder.Create;
        TKMLFolder_FromXML(FolderTmp, nodeTmp);
        Obj.AddFolder(FolderTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Folder Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLFolder_ToXML(Obj: TKMLFolder; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  OpenTmp: IXMLNode;
  VisibilityTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Folder';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if Obj.NameExsit then
    begin
      NameTmp := doc.CreateNode('name', ntElement);
      NameTmp.NodeValue := Obj.Name;
      node.ChildNodes.Add(NameTmp);
    end;
    if Obj.OpenExsit then
    begin
      OpenTmp := doc.CreateNode('open', ntElement);
      OpenTmp.NodeValue := IntToStr(Obj.Open);
      node.ChildNodes.Add(OpenTmp);
    end;
    if Obj.VisibilityExsit then
    begin
      VisibilityTmp := doc.CreateNode('visibility', ntElement);
      VisibilityTmp.NodeValue := IntToStr(Obj.Visibility);
      node.ChildNodes.Add(VisibilityTmp);
    end;
    if Obj.DescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('description', ntElement);
      DescriptionTmp.NodeValue := Obj.Description;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    if Obj.LookAtExsit then
      TKMLLookAt_ToXML(Obj.LookAt, node, 'LookAt');
    if Obj.DocumentExsit then
      TKMLDocument_ToXML(Obj.Document, node, 'Document');
    for I := 0 to Obj.GroundOverlays.Count - 1 do
       TKMLGroundOverlay_ToXML(Obj.GroundOverlay[I], node, 'GroundOverlay');
    for I := 0 to Obj.Placemarks.Count - 1 do
       TKMLPlacemark_ToXML(Obj.Placemark[I], node, 'Placemark');
    for I := 0 to Obj.ScreenOverlays.Count - 1 do
       TKMLScreenOverlay_ToXML(Obj.ScreenOverlay[I], node, 'ScreenOverlay');
    for I := 0 to Obj.Folders.Count - 1 do
       TKMLFolder_ToXML(Obj.Folder[I], node, 'Folder');
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLPlacemark_FromXML(Obj: TKMLPlacemark; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    Obj.NameExsit := False;
    Obj.VisibilityExsit := False;
    Obj.DescriptionExsit := False;
    Obj.LookAtExsit := False;
    Obj.StyleUrlExsit := False;
    Obj.GeometryExsit := False;
    Obj.ExtendedDataExsit := False;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        Obj.Name := nodeTmp.Text;
        Obj.NameExsit := True;
      end
      else if nodeTmp.NodeName = 'visibility' then
      begin
        Obj.Visibility := StrToIntDef(nodeTmp.Text, 0);;
        Obj.VisibilityExsit := True;
      end
      else if nodeTmp.NodeName = 'description' then
      begin
        Obj.Description := nodeTmp.Text;
        Obj.DescriptionExsit := True;
      end
      else if nodeTmp.NodeName = 'LookAt' then
      begin
        Obj.LookAt := TKMLLookAt.Create;
        TKMLLookAt_FromXML(Obj.LookAt, nodeTmp);
        Obj.LookAtExsit := True;
      end
      else if nodeTmp.NodeName = 'styleUrl' then
      begin
        Obj.StyleUrl := nodeTmp.Text;
        Obj.StyleUrlExsit := True;
      end
      else if nodeTmp.NodeName = 'Point' then
      begin
        Obj.Geometry := TKMLPoint.Create;
        TKMLPoint_FromXML(TKMLPoint(Obj.Geometry), nodeTmp);
        Obj.GeometryExsit := True;
      end
      else if nodeTmp.NodeName = 'Polygon' then
      begin
        Obj.Geometry := TKMLPolygon.Create;
        TKMLPolygon_FromXML(TKMLPolygon(Obj.Geometry), nodeTmp);
        Obj.GeometryExsit := True;
      end
      else if nodeTmp.NodeName = 'LinearRing' then
      begin
        Obj.Geometry := TKMLLinearRing.Create;
        TKMLLinearRing_FromXML(TKMLLinearRing(Obj.Geometry), nodeTmp);
        Obj.GeometryExsit := True;
      end
      else if nodeTmp.NodeName = 'LineString' then
      begin
        Obj.Geometry := TKMLLineString.Create;
        TKMLLineString_FromXML(TKMLLineString(Obj.Geometry), nodeTmp);
        Obj.GeometryExsit := True;
      end
      else if nodeTmp.NodeName = 'MultiGeometry' then
      begin
        Obj.Geometry := TKMLMultiGeometry.Create;
        TKMLMultiGeometry_FromXML(TKMLMultiGeometry(Obj.Geometry), nodeTmp);
        Obj.GeometryExsit := True;
      end
      else if nodeTmp.NodeName = 'ExtendedData' then
      begin
        Obj.ExtendedData := TKMLExtendedData.Create;
        TKMLExtendedData_FromXML(Obj.ExtendedData, nodeTmp);
        Obj.ExtendedDataExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Placemark Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLPlacemark_ToXML(Obj: TKMLPlacemark; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  VisibilityTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  StyleUrlTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Placemark';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if Obj.NameExsit then
    begin
      NameTmp := doc.CreateNode('name', ntElement);
      NameTmp.NodeValue := Obj.Name;
      node.ChildNodes.Add(NameTmp);
    end;
    if Obj.VisibilityExsit then
    begin
      VisibilityTmp := doc.CreateNode('visibility', ntElement);
      VisibilityTmp.NodeValue := IntToStr(Obj.Visibility);
      node.ChildNodes.Add(VisibilityTmp);
    end;
    if Obj.DescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('description', ntElement);
      DescriptionTmp.NodeValue := Obj.Description;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    if Obj.LookAtExsit then
      TKMLLookAt_ToXML(Obj.LookAt, node, 'LookAt');
    if Obj.StyleUrlExsit then
    begin
      StyleUrlTmp := doc.CreateNode('styleUrl', ntElement);
      StyleUrlTmp.NodeValue := Obj.StyleUrl;
      node.ChildNodes.Add(StyleUrlTmp);
    end;
    if Obj.GeometryExsit then
      TKMLGeometry_ToXML(Obj.Geometry, node, '#Optional');
    if Obj.ExtendedDataExsit then
      TKMLExtendedData_ToXML(Obj.ExtendedData, node, 'ExtendedData');
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLGeometry_FromXML(Obj: TKMLGeometry; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Geometry Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLGeometry_ToXML(Obj: TKMLGeometry; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := '#Optional';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLMultiGeometry_FromXML(Obj: TKMLMultiGeometry; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  GeometryTmp: TKMLGeometry;
begin
  try
    TKMLGeometry_FromXML(Obj, node);
    Obj.GeometryClear;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Point' then
      begin
        GeometryTmp := TKMLPoint.Create;
        TKMLPoint_FromXML(TKMLPoint(GeometryTmp), nodeTmp);
        Obj.AddGeometry(GeometryTmp);
      end
      else if nodeTmp.NodeName = 'Polygon' then
      begin
        GeometryTmp := TKMLPolygon.Create;
        TKMLPolygon_FromXML(TKMLPolygon(GeometryTmp), nodeTmp);
        Obj.AddGeometry(GeometryTmp);
      end
      else if nodeTmp.NodeName = 'LinearRing' then
      begin
        GeometryTmp := TKMLLinearRing.Create;
        TKMLLinearRing_FromXML(TKMLLinearRing(GeometryTmp), nodeTmp);
        Obj.AddGeometry(GeometryTmp);
      end
      else if nodeTmp.NodeName = 'LineString' then
      begin
        GeometryTmp := TKMLLineString.Create;
        TKMLLineString_FromXML(TKMLLineString(GeometryTmp), nodeTmp);
        Obj.AddGeometry(GeometryTmp);
      end
      else if nodeTmp.NodeName = 'MultiGeometry' then
      begin
        GeometryTmp := TKMLMultiGeometry.Create;
        TKMLMultiGeometry_FromXML(TKMLMultiGeometry(GeometryTmp), nodeTmp);
        Obj.AddGeometry(GeometryTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('MultiGeometry Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLMultiGeometry_ToXML(Obj: TKMLMultiGeometry; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'MultiGeometry';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    TKMLGeometry_ToXML(Obj, node);
    for I := 0 to Obj.Geometrys.Count - 1 do
       TKMLGeometry_ToXML(Obj.Geometry[I], node, '#Optional');
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLPoint_FromXML(Obj: TKMLPoint; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    TKMLGeometry_FromXML(Obj, node);
    Obj.ExtrudeExsit := False;
    Obj.AltitudeModeExsit := False;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'extrude' then
      begin
        Obj.Extrude := nodeTmp.Text;
        Obj.ExtrudeExsit := True;
      end
      else if nodeTmp.NodeName = 'altitudeMode' then
      begin
        Obj.AltitudeMode := nodeTmp.Text;
        Obj.AltitudeModeExsit := True;
      end
      else if nodeTmp.NodeName = 'coordinates' then
      begin
        Obj.Coordinates := StringToCoordinates(nodeTmp.Text);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Point Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLPoint_ToXML(Obj: TKMLPoint; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  ExtrudeTmp: IXMLNode;
  AltitudeModeTmp: IXMLNode;
  CoordinatesTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Point';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    TKMLGeometry_ToXML(Obj, node);
    if Obj.ExtrudeExsit then
    begin
      ExtrudeTmp := doc.CreateNode('extrude', ntElement);
      ExtrudeTmp.NodeValue := Obj.Extrude;
      node.ChildNodes.Add(ExtrudeTmp);
    end;
    if Obj.AltitudeModeExsit then
    begin
      AltitudeModeTmp := doc.CreateNode('altitudeMode', ntElement);
      AltitudeModeTmp.NodeValue := Obj.AltitudeMode;
      node.ChildNodes.Add(AltitudeModeTmp);
    end;
    CoordinatesTmp := doc.CreateNode('coordinates', ntElement);
    CoordinatesTmp.NodeValue := CoordinatesToString(Obj.Coordinates);
    node.ChildNodes.Add(CoordinatesTmp);
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLPolygon_FromXML(Obj: TKMLPolygon; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  OuterBoundaryIsTmp: TKMLBoundary;
  InnerBoundaryIsTmp: TKMLBoundary;
begin
  try
    TKMLGeometry_FromXML(Obj, node);
    Obj.ExtrudeExsit := False;
    Obj.AltitudeModeExsit := False;
    Obj.OuterBoundaryIsClear;
    Obj.InnerBoundaryIsClear;
    Obj.TessellateExsit := False;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'extrude' then
      begin
        Obj.Extrude := nodeTmp.Text;
        Obj.ExtrudeExsit := True;
      end
      else if nodeTmp.NodeName = 'altitudeMode' then
      begin
        Obj.AltitudeMode := nodeTmp.Text;
        Obj.AltitudeModeExsit := True;
      end
      else if nodeTmp.NodeName = 'outerBoundaryIs' then
      begin
        OuterBoundaryIsTmp := TKMLBoundary.Create;
        TKMLBoundary_FromXML(OuterBoundaryIsTmp, nodeTmp);
        Obj.AddOuterBoundaryIs(OuterBoundaryIsTmp);
      end
      else if nodeTmp.NodeName = 'innerBoundaryIs' then
      begin
        InnerBoundaryIsTmp := TKMLBoundary.Create;
        TKMLBoundary_FromXML(InnerBoundaryIsTmp, nodeTmp);
        Obj.AddInnerBoundaryIs(InnerBoundaryIsTmp);
      end
      else if nodeTmp.NodeName = 'tessellate' then
      begin
        Obj.Tessellate := nodeTmp.Text;
        Obj.TessellateExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Polygon Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLPolygon_ToXML(Obj: TKMLPolygon; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  ExtrudeTmp: IXMLNode;
  AltitudeModeTmp: IXMLNode;
  TessellateTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Polygon';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    TKMLGeometry_ToXML(Obj, node);
    if Obj.ExtrudeExsit then
    begin
      ExtrudeTmp := doc.CreateNode('extrude', ntElement);
      ExtrudeTmp.NodeValue := Obj.Extrude;
      node.ChildNodes.Add(ExtrudeTmp);
    end;
    if Obj.AltitudeModeExsit then
    begin
      AltitudeModeTmp := doc.CreateNode('altitudeMode', ntElement);
      AltitudeModeTmp.NodeValue := Obj.AltitudeMode;
      node.ChildNodes.Add(AltitudeModeTmp);
    end;
    for I := 0 to Obj.OuterBoundaryIss.Count - 1 do
       TKMLBoundary_ToXML(Obj.OuterBoundaryIs[I], node, 'outerBoundaryIs');
    for I := 0 to Obj.InnerBoundaryIss.Count - 1 do
       TKMLBoundary_ToXML(Obj.InnerBoundaryIs[I], node, 'innerBoundaryIs');
    if Obj.TessellateExsit then
    begin
      TessellateTmp := doc.CreateNode('tessellate', ntElement);
      TessellateTmp.NodeValue := Obj.Tessellate;
      node.ChildNodes.Add(TessellateTmp);
    end;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLBoundary_FromXML(Obj: TKMLBoundary; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'LinearRing' then
      begin
        Obj.LinearRing := TKMLLinearRing.Create;
        TKMLLinearRing_FromXML(Obj.LinearRing, nodeTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Boundary Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLBoundary_ToXML(Obj: TKMLBoundary; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Boundary';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    TKMLLinearRing_ToXML(Obj.LinearRing, node, 'LinearRing');
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLLinearRing_FromXML(Obj: TKMLLinearRing; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    TKMLGeometry_FromXML(Obj, node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'coordinates' then
      begin
        Obj.Coordinates := StringToCoordinates(nodeTmp.Text);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('LinearRing Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLLinearRing_ToXML(Obj: TKMLLinearRing; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  CoordinatesTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'LinearRing';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    TKMLGeometry_ToXML(Obj, node);
    CoordinatesTmp := doc.CreateNode('coordinates', ntElement);
    CoordinatesTmp.NodeValue := CoordinatesToString(Obj.Coordinates);
    node.ChildNodes.Add(CoordinatesTmp);
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLLineString_FromXML(Obj: TKMLLineString; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    TKMLGeometry_FromXML(Obj, node);
    Obj.TessellateExsit := False;
    Obj.AltitudeModeExsit := False;
    Obj.ExtrudeExsit := False;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'tessellate' then
      begin
        Obj.Tessellate := nodeTmp.Text;
        Obj.TessellateExsit := True;
      end
      else if nodeTmp.NodeName = 'altitudeMode' then
      begin
        Obj.AltitudeMode := nodeTmp.Text;
        Obj.AltitudeModeExsit := True;
      end
      else if nodeTmp.NodeName = 'extrude' then
      begin
        Obj.Extrude := nodeTmp.Text;
        Obj.ExtrudeExsit := True;
      end
      else if nodeTmp.NodeName = 'coordinates' then
      begin
        Obj.Coordinates := StringToCoordinates(nodeTmp.Text);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('LineString Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLLineString_ToXML(Obj: TKMLLineString; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  TessellateTmp: IXMLNode;
  AltitudeModeTmp: IXMLNode;
  ExtrudeTmp: IXMLNode;
  CoordinatesTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'LineString';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    TKMLGeometry_ToXML(Obj, node);
    if Obj.TessellateExsit then
    begin
      TessellateTmp := doc.CreateNode('tessellate', ntElement);
      TessellateTmp.NodeValue := Obj.Tessellate;
      node.ChildNodes.Add(TessellateTmp);
    end;
    if Obj.AltitudeModeExsit then
    begin
      AltitudeModeTmp := doc.CreateNode('altitudeMode', ntElement);
      AltitudeModeTmp.NodeValue := Obj.AltitudeMode;
      node.ChildNodes.Add(AltitudeModeTmp);
    end;
    if Obj.ExtrudeExsit then
    begin
      ExtrudeTmp := doc.CreateNode('extrude', ntElement);
      ExtrudeTmp.NodeValue := Obj.Extrude;
      node.ChildNodes.Add(ExtrudeTmp);
    end;
    CoordinatesTmp := doc.CreateNode('coordinates', ntElement);
    CoordinatesTmp.NodeValue := CoordinatesToString(Obj.Coordinates);
    node.ChildNodes.Add(CoordinatesTmp);
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLLookAt_FromXML(Obj: TKMLLookAt; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    Obj.LongitudeExsit := False;
    Obj.LatitudeExsit := False;
    Obj.AltitudeExsit := False;
    Obj.HeadingExsit := False;
    Obj.TiltExsit := False;
    Obj.RangeExsit := False;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'longitude' then
      begin
        Obj.Longitude := StrToFloatDef(nodeTmp.Text, 0);;
        Obj.LongitudeExsit := True;
      end
      else if nodeTmp.NodeName = 'latitude' then
      begin
        Obj.Latitude := StrToFloatDef(nodeTmp.Text, 0);;
        Obj.LatitudeExsit := True;
      end
      else if nodeTmp.NodeName = 'altitude' then
      begin
        Obj.Altitude := StrToFloatDef(nodeTmp.Text, 0);;
        Obj.AltitudeExsit := True;
      end
      else if nodeTmp.NodeName = 'heading' then
      begin
        Obj.Heading := StrToFloatDef(nodeTmp.Text, 0);;
        Obj.HeadingExsit := True;
      end
      else if nodeTmp.NodeName = 'tilt' then
      begin
        Obj.Tilt := StrToFloatDef(nodeTmp.Text, 0);;
        Obj.TiltExsit := True;
      end
      else if nodeTmp.NodeName = 'range' then
      begin
        Obj.Range := StrToFloatDef(nodeTmp.Text, 0);;
        Obj.RangeExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('LookAt Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLLookAt_ToXML(Obj: TKMLLookAt; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  LongitudeTmp: IXMLNode;
  LatitudeTmp: IXMLNode;
  AltitudeTmp: IXMLNode;
  HeadingTmp: IXMLNode;
  TiltTmp: IXMLNode;
  RangeTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'LookAt';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if Obj.LongitudeExsit then
    begin
      LongitudeTmp := doc.CreateNode('longitude', ntElement);
      LongitudeTmp.NodeValue := FloatToStr(Obj.Longitude);
      node.ChildNodes.Add(LongitudeTmp);
    end;
    if Obj.LatitudeExsit then
    begin
      LatitudeTmp := doc.CreateNode('latitude', ntElement);
      LatitudeTmp.NodeValue := FloatToStr(Obj.Latitude);
      node.ChildNodes.Add(LatitudeTmp);
    end;
    if Obj.AltitudeExsit then
    begin
      AltitudeTmp := doc.CreateNode('altitude', ntElement);
      AltitudeTmp.NodeValue := FloatToStr(Obj.Altitude);
      node.ChildNodes.Add(AltitudeTmp);
    end;
    if Obj.HeadingExsit then
    begin
      HeadingTmp := doc.CreateNode('heading', ntElement);
      HeadingTmp.NodeValue := FloatToStr(Obj.Heading);
      node.ChildNodes.Add(HeadingTmp);
    end;
    if Obj.TiltExsit then
    begin
      TiltTmp := doc.CreateNode('tilt', ntElement);
      TiltTmp.NodeValue := FloatToStr(Obj.Tilt);
      node.ChildNodes.Add(TiltTmp);
    end;
    if Obj.RangeExsit then
    begin
      RangeTmp := doc.CreateNode('range', ntElement);
      RangeTmp.NodeValue := FloatToStr(Obj.Range);
      node.ChildNodes.Add(RangeTmp);
    end;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLLatLonBox_FromXML(Obj: TKMLLatLonBox; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'north' then
      begin
        Obj.North := StrToFloatDef(nodeTmp.Text, 0);;
      end
      else if nodeTmp.NodeName = 'south' then
      begin
        Obj.South := StrToFloatDef(nodeTmp.Text, 0);;
      end
      else if nodeTmp.NodeName = 'east' then
      begin
        Obj.East := StrToFloatDef(nodeTmp.Text, 0);;
      end
      else if nodeTmp.NodeName = 'west' then
      begin
        Obj.West := StrToFloatDef(nodeTmp.Text, 0);;
      end
      else if nodeTmp.NodeName = 'rotation' then
      begin
        Obj.Rotation := StrToFloatDef(nodeTmp.Text, 0);;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('LatLonBox Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLLatLonBox_ToXML(Obj: TKMLLatLonBox; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  NorthTmp: IXMLNode;
  SouthTmp: IXMLNode;
  EastTmp: IXMLNode;
  WestTmp: IXMLNode;
  RotationTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'LatLonBox';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    NorthTmp := doc.CreateNode('north', ntElement);
    NorthTmp.NodeValue := FloatToStr(Obj.North);
    node.ChildNodes.Add(NorthTmp);
    SouthTmp := doc.CreateNode('south', ntElement);
    SouthTmp.NodeValue := FloatToStr(Obj.South);
    node.ChildNodes.Add(SouthTmp);
    EastTmp := doc.CreateNode('east', ntElement);
    EastTmp.NodeValue := FloatToStr(Obj.East);
    node.ChildNodes.Add(EastTmp);
    WestTmp := doc.CreateNode('west', ntElement);
    WestTmp.NodeValue := FloatToStr(Obj.West);
    node.ChildNodes.Add(WestTmp);
    RotationTmp := doc.CreateNode('rotation', ntElement);
    RotationTmp.NodeValue := FloatToStr(Obj.Rotation);
    node.ChildNodes.Add(RotationTmp);
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLGroundOverlay_FromXML(Obj: TKMLGroundOverlay; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    Obj.NameExsit := False;
    Obj.VisibilityExsit := False;
    Obj.DescriptionExsit := False;
    Obj.LookAtExsit := False;
    Obj.IconExsit := False;
    Obj.LatLonBoxExsit := False;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        Obj.Name := nodeTmp.Text;
        Obj.NameExsit := True;
      end
      else if nodeTmp.NodeName = 'visibility' then
      begin
        Obj.Visibility := StrToIntDef(nodeTmp.Text, 0);;
        Obj.VisibilityExsit := True;
      end
      else if nodeTmp.NodeName = 'description' then
      begin
        Obj.Description := nodeTmp.Text;
        Obj.DescriptionExsit := True;
      end
      else if nodeTmp.NodeName = 'LookAt' then
      begin
        Obj.LookAt := TKMLLookAt.Create;
        TKMLLookAt_FromXML(Obj.LookAt, nodeTmp);
        Obj.LookAtExsit := True;
      end
      else if nodeTmp.NodeName = 'Icon' then
      begin
        Obj.Icon := TKMLIcon.Create;
        TKMLIcon_FromXML(Obj.Icon, nodeTmp);
        Obj.IconExsit := True;
      end
      else if nodeTmp.NodeName = 'LatLonBox' then
      begin
        Obj.LatLonBox := TKMLLatLonBox.Create;
        TKMLLatLonBox_FromXML(Obj.LatLonBox, nodeTmp);
        Obj.LatLonBoxExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('GroundOverlay Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLGroundOverlay_ToXML(Obj: TKMLGroundOverlay; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  VisibilityTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'GroundOverlay';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if Obj.NameExsit then
    begin
      NameTmp := doc.CreateNode('name', ntElement);
      NameTmp.NodeValue := Obj.Name;
      node.ChildNodes.Add(NameTmp);
    end;
    if Obj.VisibilityExsit then
    begin
      VisibilityTmp := doc.CreateNode('visibility', ntElement);
      VisibilityTmp.NodeValue := IntToStr(Obj.Visibility);
      node.ChildNodes.Add(VisibilityTmp);
    end;
    if Obj.DescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('description', ntElement);
      DescriptionTmp.NodeValue := Obj.Description;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    if Obj.LookAtExsit then
      TKMLLookAt_ToXML(Obj.LookAt, node, 'LookAt');
    if Obj.IconExsit then
      TKMLIcon_ToXML(Obj.Icon, node, 'Icon');
    if Obj.LatLonBoxExsit then
      TKMLLatLonBox_ToXML(Obj.LatLonBox, node, 'LatLonBox');
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLExtendedData_FromXML(Obj: TKMLExtendedData; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  DataTmp: TKMLData;
begin
  try
    Obj.DataClear;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Data' then
      begin
        DataTmp := TKMLData.Create;
        TKMLData_FromXML(DataTmp, nodeTmp);
        Obj.AddData(DataTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('ExtendedData Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLExtendedData_ToXML(Obj: TKMLExtendedData; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'ExtendedData';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    for I := 0 to Obj.Datas.Count - 1 do
       TKMLData_ToXML(Obj.Data[I], node, 'Data');
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLData_FromXML(Obj: TKMLData; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'value' then
      begin
        Obj.Value := nodeTmp.Text;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        Obj.Name := nodeTmp.Text;
      end;
    end;
  except
    raise Exception.Create('Data Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLData_ToXML(Obj: TKMLData; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  ValueTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Data';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    ValueTmp := doc.CreateNode('value', ntElement);
    ValueTmp.NodeValue := Obj.Value;
    node.ChildNodes.Add(ValueTmp);
    NameTmp := doc.CreateNode('name', ntAttribute);
    NameTmp.NodeValue := Obj.Name;
    node.AttributeNodes.Add(NameTmp);
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLSize_FromXML(Obj: TKMLSize; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'x' then
      begin
        Obj.X := StrToFloatDef(nodeTmp.Text, 0);;
      end
      else if nodeTmp.NodeName = 'y' then
      begin
        Obj.Y := StrToFloatDef(nodeTmp.Text, 0);;
      end
      else if nodeTmp.NodeName = 'xunits' then
      begin
        Obj.XUnits := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'yunits' then
      begin
        Obj.YUnits := nodeTmp.Text;
      end;
    end;
  except
    raise Exception.Create('Size Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLSize_ToXML(Obj: TKMLSize; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  XTmp: IXMLNode;
  YTmp: IXMLNode;
  XUnitsTmp: IXMLNode;
  YUnitsTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Size';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    XTmp := doc.CreateNode('x', ntAttribute);
    XTmp.NodeValue := FloatToStr(Obj.X);
    node.AttributeNodes.Add(XTmp);
    YTmp := doc.CreateNode('y', ntAttribute);
    YTmp.NodeValue := FloatToStr(Obj.Y);
    node.AttributeNodes.Add(YTmp);
    XUnitsTmp := doc.CreateNode('xunits', ntAttribute);
    XUnitsTmp.NodeValue := Obj.XUnits;
    node.AttributeNodes.Add(XUnitsTmp);
    YUnitsTmp := doc.CreateNode('yunits', ntAttribute);
    YUnitsTmp.NodeValue := Obj.YUnits;
    node.AttributeNodes.Add(YUnitsTmp);
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLStyle_FromXML(Obj: TKMLStyle; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    Obj.IconStyleExsit := False;
    Obj.LineStyleExsit := False;
    Obj.PolyStyleExsit := False;
    Obj.BalloonStyleExsit := False;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'IconStyle' then
      begin
        Obj.IconStyle := TKMLIconStyle.Create;
        TKMLIconStyle_FromXML(Obj.IconStyle, nodeTmp);
        Obj.IconStyleExsit := True;
      end
      else if nodeTmp.NodeName = 'LineStyle' then
      begin
        Obj.LineStyle := TKMLLineStyle.Create;
        TKMLLineStyle_FromXML(Obj.LineStyle, nodeTmp);
        Obj.LineStyleExsit := True;
      end
      else if nodeTmp.NodeName = 'PolyStyle' then
      begin
        Obj.PolyStyle := TKMLLineStyle.Create;
        TKMLLineStyle_FromXML(Obj.PolyStyle, nodeTmp);
        Obj.PolyStyleExsit := True;
      end
      else if nodeTmp.NodeName = 'BalloonStyle' then
      begin
        Obj.BalloonStyle := TKMLBalloonStyle.Create;
        TKMLBalloonStyle_FromXML(Obj.BalloonStyle, nodeTmp);
        Obj.BalloonStyleExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'id' then
      begin
        Obj.ID := nodeTmp.Text;
      end;
    end;
  except
    raise Exception.Create('Style Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLStyle_ToXML(Obj: TKMLStyle; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  IDTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Style';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if Obj.IconStyleExsit then
      TKMLIconStyle_ToXML(Obj.IconStyle, node, 'IconStyle');
    if Obj.LineStyleExsit then
      TKMLLineStyle_ToXML(Obj.LineStyle, node, 'LineStyle');
    if Obj.PolyStyleExsit then
      TKMLLineStyle_ToXML(Obj.PolyStyle, node, 'PolyStyle');
    if Obj.BalloonStyleExsit then
      TKMLBalloonStyle_ToXML(Obj.BalloonStyle, node, 'BalloonStyle');
    IDTmp := doc.CreateNode('id', ntAttribute);
    IDTmp.NodeValue := Obj.ID;
    node.AttributeNodes.Add(IDTmp);
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLIconStyle_FromXML(Obj: TKMLIconStyle; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Icon' then
      begin
        Obj.Icon := TKMLIcon.Create;
        TKMLIcon_FromXML(Obj.Icon, nodeTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('IconStyle Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLIconStyle_ToXML(Obj: TKMLIconStyle; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'IconStyle';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    TKMLIcon_ToXML(Obj.Icon, node, 'Icon');
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLIcon_FromXML(Obj: TKMLIcon; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'href' then
      begin
        Obj.href := nodeTmp.Text;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Icon Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLIcon_ToXML(Obj: TKMLIcon; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  hrefTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Icon';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    hrefTmp := doc.CreateNode('href', ntElement);
    hrefTmp.NodeValue := Obj.href;
    node.ChildNodes.Add(hrefTmp);
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLLineStyle_FromXML(Obj: TKMLLineStyle; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    Obj.colorExsit := False;
    Obj.widthExsit := False;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'color' then
      begin
        Obj.color := nodeTmp.Text;
        Obj.colorExsit := True;
      end
      else if nodeTmp.NodeName = 'width' then
      begin
        Obj.width := StrToFloatDef(nodeTmp.Text, 0);;
        Obj.widthExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('LineStyle Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLLineStyle_ToXML(Obj: TKMLLineStyle; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  colorTmp: IXMLNode;
  widthTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'LineStyle';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if Obj.colorExsit then
    begin
      colorTmp := doc.CreateNode('color', ntElement);
      colorTmp.NodeValue := Obj.color;
      node.ChildNodes.Add(colorTmp);
    end;
    if Obj.widthExsit then
    begin
      widthTmp := doc.CreateNode('width', ntElement);
      widthTmp.NodeValue := FloatToStr(Obj.width);
      node.ChildNodes.Add(widthTmp);
    end;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLBalloonStyle_FromXML(Obj: TKMLBalloonStyle; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = '~text' then
      begin
        Obj.text := nodeTmp.Text;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('BalloonStyle Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLBalloonStyle_ToXML(Obj: TKMLBalloonStyle; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  textTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'BalloonStyle';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    textTmp := doc.CreateNode('text', ntCData);
    textTmp.NodeValue := Obj.text;
    node.ChildNodes.Add(textTmp);
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLStyleMap_FromXML(Obj: TKMLStyleMap; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  PairTmp: TKMLPair;
begin
  try
    Obj.PairClear;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Pair' then
      begin
        PairTmp := TKMLPair.Create;
        TKMLPair_FromXML(PairTmp, nodeTmp);
        Obj.AddPair(PairTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'id' then
      begin
        Obj.ID := nodeTmp.Text;
      end;
    end;
  except
    raise Exception.Create('StyleMap Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLStyleMap_ToXML(Obj: TKMLStyleMap; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  IDTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'StyleMap';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    for I := 0 to Obj.Pairs.Count - 1 do
       TKMLPair_ToXML(Obj.Pair[I], node, 'Pair');
    IDTmp := doc.CreateNode('id', ntAttribute);
    IDTmp.NodeValue := Obj.ID;
    node.AttributeNodes.Add(IDTmp);
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLScreenOverlay_FromXML(Obj: TKMLScreenOverlay; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    Obj.NameExsit := False;
    Obj.VisibilityExsit := False;
    Obj.DescriptionExsit := False;
    Obj.IconExsit := False;
    Obj.OverlayXYExsit := False;
    Obj.ScreenXYExsit := False;
    Obj.RotationXYExsit := False;
    Obj.SizeExsit := False;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        Obj.Name := nodeTmp.Text;
        Obj.NameExsit := True;
      end
      else if nodeTmp.NodeName = 'visibility' then
      begin
        Obj.Visibility := StrToIntDef(nodeTmp.Text, 0);;
        Obj.VisibilityExsit := True;
      end
      else if nodeTmp.NodeName = 'description' then
      begin
        Obj.Description := nodeTmp.Text;
        Obj.DescriptionExsit := True;
      end
      else if nodeTmp.NodeName = 'Icon' then
      begin
        Obj.Icon := TKMLIcon.Create;
        TKMLIcon_FromXML(Obj.Icon, nodeTmp);
        Obj.IconExsit := True;
      end
      else if nodeTmp.NodeName = 'overlayXY' then
      begin
        Obj.OverlayXY := TKMLSize.Create;
        TKMLSize_FromXML(Obj.OverlayXY, nodeTmp);
        Obj.OverlayXYExsit := True;
      end
      else if nodeTmp.NodeName = 'screenXY' then
      begin
        Obj.ScreenXY := TKMLSize.Create;
        TKMLSize_FromXML(Obj.ScreenXY, nodeTmp);
        Obj.ScreenXYExsit := True;
      end
      else if nodeTmp.NodeName = 'rotationXY' then
      begin
        Obj.RotationXY := TKMLSize.Create;
        TKMLSize_FromXML(Obj.RotationXY, nodeTmp);
        Obj.RotationXYExsit := True;
      end
      else if nodeTmp.NodeName = 'size' then
      begin
        Obj.Size := TKMLSize.Create;
        TKMLSize_FromXML(Obj.Size, nodeTmp);
        Obj.SizeExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('ScreenOverlay Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLScreenOverlay_ToXML(Obj: TKMLScreenOverlay; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  VisibilityTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'ScreenOverlay';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if Obj.NameExsit then
    begin
      NameTmp := doc.CreateNode('name', ntElement);
      NameTmp.NodeValue := Obj.Name;
      node.ChildNodes.Add(NameTmp);
    end;
    if Obj.VisibilityExsit then
    begin
      VisibilityTmp := doc.CreateNode('visibility', ntElement);
      VisibilityTmp.NodeValue := IntToStr(Obj.Visibility);
      node.ChildNodes.Add(VisibilityTmp);
    end;
    if Obj.DescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('description', ntElement);
      DescriptionTmp.NodeValue := Obj.Description;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    if Obj.IconExsit then
      TKMLIcon_ToXML(Obj.Icon, node, 'Icon');
    if Obj.OverlayXYExsit then
      TKMLSize_ToXML(Obj.OverlayXY, node, 'overlayXY');
    if Obj.ScreenXYExsit then
      TKMLSize_ToXML(Obj.ScreenXY, node, 'screenXY');
    if Obj.RotationXYExsit then
      TKMLSize_ToXML(Obj.RotationXY, node, 'rotationXY');
    if Obj.SizeExsit then
      TKMLSize_ToXML(Obj.Size, node, 'size');
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.TKMLPair_FromXML(Obj: TKMLPair; node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    Obj.KeyExsit := False;
    Obj.StyleUrlExsit := False;
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'key' then
      begin
        Obj.Key := nodeTmp.Text;
        Obj.KeyExsit := True;
      end
      else if nodeTmp.NodeName = 'styleUrl' then
      begin
        Obj.StyleUrl := nodeTmp.Text;
        Obj.StyleUrlExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Pair Read XML Error!' + node.Xml);
  end;
end;

procedure TKML.TKMLPair_ToXML(Obj: TKMLPair; par: IXMLNode; pt: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
  KeyTmp: IXMLNode;
  StyleUrlTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Pair';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if Obj.KeyExsit then
    begin
      KeyTmp := doc.CreateNode('key', ntElement);
      KeyTmp.NodeValue := Obj.Key;
      node.ChildNodes.Add(KeyTmp);
    end;
    if Obj.StyleUrlExsit then
    begin
      StyleUrlTmp := doc.CreateNode('styleUrl', ntElement);
      StyleUrlTmp.NodeValue := Obj.StyleUrl;
      node.ChildNodes.Add(StyleUrlTmp);
    end;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;


end.
