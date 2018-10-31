unit objInspector;

(* *****************************************************************************
Module:          objInspector
Description:     Object Inspector for Delphi Components
Version:         1.0
Date:            30-DEC-2006
Compiler:        Delphi 7
Author:          Angus Johnson,   angusj-AT-myrealbox-DOT-com
Copyright:       © 2006 Angus Johnson
********************************************************************************
The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
***************************************************************************** *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ValEdit, ExtCtrls, StdCtrls, System.UITypes;

const
  CM_FORCEPICKLISTUPDATE = WM_USER + 100;

type
  TObjInspectForm = class(TForm)
    pnlTop: TPanel;
    FontDialog1: TFontDialog;
    ColorDialog1: TColorDialog;
    eComponentInfo: TEdit;
    vle: TValueListEditor;
    pnlBottom: TPanel;
    btnClose: TButton;
    procedure vleEditButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure vleValidate(Sender: TObject; ACol, ARow: Integer;
      const KeyName, KeyValue: String);
    procedure vleKeyPress(Sender: TObject; var Key: Char);
    procedure vleSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fObj: TComponent;
    fPropList: TStringlist;
    fExcludList: TStringlist;
    fOnClosing: TNotifyEvent;
    //workaround (to improve cosmetics) and to know when PickList changes
    procedure CMForcePicklistUpdate(var Message: TMessage); message CM_FORCEPICKLISTUPDATE;
    procedure Initialize;
    procedure RefreshAllProperties;
  public
    procedure AssignObj(Obj: TComponent; IgnoreProperties: array of string);
    property OnClosing: TNotifyEvent read fOnClosing write fOnClosing;
    property Obj: TComponent read fObj;
  end;

var
  ObjInspectForm: TObjInspectForm;

implementation

uses TypInfo;

{$R *.dfm}

const

  MAX_CLIENT_HEIGHT = 540;
  boolStr: array [boolean] of string = ('False','True');

type
  THackedValueListEditor = class(TValueListEditor);


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function min(int1, int2: integer): integer;
begin
  if int1 < int2 then result := int1 else result := int2;
end;
//------------------------------------------------------------------------------

procedure split(const splitChars: string; var strL: string; out strR: string);
var
  i: integer;
begin
  i := pos(splitChars, strL);
  if i = 0 then
  begin
    strR := '';
  end else
  begin
    strR := copy(strL, i + length(splitChars), 255);
    strL := copy(strL, 1, i -1);
  end;
end;
//------------------------------------------------------------------------------

function RTrimNonPrintChars(const S: string): string;
var
  i: Integer;
begin
  i := Length(S);
  while (i > 0) and (S[i] < ' ') do Dec(i);
  Result := Copy(S, 1, i);
end;
//------------------------------------------------------------------------------

procedure CommaLineToStrings(const commaLine: string; strings: TStrings);
var
  l, r: string;
begin
  l := trim(commaLine);
  if not assigned(strings) then exit;
  while (l <> '') do
  begin
    split(',', l, r);
    strings.Add(l);
    l := trim(r);
  end;
end;
//------------------------------------------------------------------------------

function TColorToString(clr: TColor): string;
var
  rgb: longint;
begin
  rgb := ColorToRGB(clr);
  result := format('R:%d G:%d B:%d',
    [rgb and $FF, (rgb shr 8) and $FF, (rgb shr 16) and $FF]);
end;
//------------------------------------------------------------------------------

function FontStyleToString(font: TFont): string;
begin
  result := SetToString(GetPropInfo(font,'Style'), byte(font.Style), false);
  if result = '' then result := ' ';
end;
//------------------------------------------------------------------------------

function FontToString(font: TFont): string;
begin
  with font do
    result := format('%s, %d, [%s]',[name, size, FontStyleToString(font)]);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TObjInspectForm.FormCreate(Sender: TObject);
begin
  fPropList := TStringList.Create;
  fExcludList := TStringList.Create;
  fExcludList.Sorted := true;
end;
//------------------------------------------------------------------------------

procedure TObjInspectForm.FormDestroy(Sender: TObject);
begin
  fPropList.Free;
  fExcludList.Free;
end;
//------------------------------------------------------------------------------

procedure TObjInspectForm.btnCloseClick(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------

procedure TObjInspectForm.AssignObj(Obj: TComponent; IgnoreProperties: array of string);
var
  i: integer;
begin
  fObj := Obj;
  if not assigned(fObj) then exit;
  for i := low(IgnoreProperties) to high(IgnoreProperties) do
    fExcludList.Add(IgnoreProperties[i]);
  Initialize;
  with vle do
    self.ClientHeight := min(MAX_CLIENT_HEIGHT, RowCount *
      (DefaultRowHeight + GridLineWidth) + pnlTop.Height + pnlBottom.Height + 4);
end;
//------------------------------------------------------------------------------

procedure TObjInspectForm.Initialize;

  procedure AddString(const key, val: string);
  begin
    with vle do
      with ItemProps[InsertRow(key, val, true)] do
      begin
        EditStyle := esSimple;
      end;
  end;

  procedure AddMultiLines(const key, val: string);
  begin
    with vle do
      with ItemProps[InsertRow(key, val, true)] do
      begin
        EditStyle := esSimple;
      end;
  end;

  procedure AddComponent(const key, val: string; theClass: TClass);
  var
    i: integer;
  begin
    with vle do
      with ItemProps[InsertRow(key, val, true)] do
      begin
        EditStyle := esPickList;
        ReadOnly := true;
        PickList.Add('');
        //assuming a common 'Owner' ...
        //add any 'named' components of theClass to the Picklist
        with fObj.Owner do
          for i := 0 to ComponentCount -1 do
            if (Components[i].InheritsFrom(theClass)) and
              (Components[i] <> fObj) then
                if Components[i] is TControl and
                  not assigned(TControl(Components[i]).parent) then //do nothing
                else if (Components[i].name <> '') then
                  PickList.Add(Components[i].Name);
      end;
  end;

  procedure AddInteger(const key: string; val: integer);
  begin
    with vle do
      with ItemProps[InsertRow(key, inttostr(val), true)] do
      begin
        EditStyle := esSimple;
      end;
  end;

  procedure AddEnumeration(const key, val: string; list: TStrings);
  var
    i: integer;
  begin
    with vle do
      with ItemProps[InsertRow(key, val, true)] do
      begin
        EditStyle := esPickList;
        ReadOnly := true;
        for i := 0 to list.Count -1 do PickList.Add(list[i]);
      end;
  end;

  procedure GetEnumList(Instance: TObject; const PropName: string; list: TStrings);
  var
    PropInfo: PPropInfo;
    TypeInfo: PTypeInfo;
    T: PTypeData;
    i: integer;
  begin
    list.Clear;
    PropInfo := GetPropInfo(Instance, PropName);
    TypeInfo := PropInfo^.PropType^;
    //if TypeInfo^.Kind <> tkEnumeration then exit; //oops!!!
    T := GetTypeData(GetTypeData(TypeInfo)^.BaseType^);
    for i := T.MinValue to T.MaxValue do list.add( GetEnumName(TypeInfo, i) );
  end;

  procedure GetSetList(Instance: TObject; const PropName: string; list: TStrings);
  var
    PropInfo: PPropInfo;
    EnumInfo: PTypeInfo;
    T: PTypeData;
    i: integer;
  begin
    list.Clear;
    PropInfo := GetPropInfo(Instance, PropName);
    //if PropInfo^.PropType^.Kind <> tkSet then exit; //oops!!!
    EnumInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;
    T := GetTypeData(GetTypeData(EnumInfo)^.BaseType^);
    for i := T.MinValue to T.MaxValue do list.add( GetEnumName(EnumInfo, i) );
  end;

  procedure AddColor(const key: string; clr: TColor);
  begin
    with vle do
      with ItemProps[InsertRow(key, TColorToString(clr), true)] do
      begin
        EditStyle := esEllipsis;
        ReadOnly := true;
      end;
  end;

  procedure AddFont(const key: string; font: TFont);
  begin
    with vle.ItemProps[vle.InsertRow(key, FontToString(font), true)] do
    begin
      EditStyle := esEllipsis;
      ReadOnly := true;
    end;
  end;

  procedure AddPen(const key: string; pen: TPen);
  var
    tmpList: TStrings;
  begin
    AddColor(key +' : '+'Color', pen.Color);
    tmpList := TStringList.Create;
    try
      GetEnumList(pen, 'Style', tmpList);
      AddEnumeration(key +' : '+'Style', GetEnumProp(pen, 'Style'), tmpList);
    finally
      tmpList.Free;
    end;
    AddInteger(key +' : '+'Width', pen.Width);
  end;

  procedure AddSet(const key, val: string; list: TStrings);
  var
    i: integer;
    tmpList: TStringList;
  begin
    //list    == total set options
    //tmpList == current set selection
    tmpList := TStringList.Create;
    try
      CommaLineToStrings(val, tmpList);
      with vle do for i := 0 to list.Count -1 do
        with ItemProps[InsertRow(key +' : '+list[i],
          boolStr[tmpList.indexof(list[i])>= 0], true)] do
            begin
              EditStyle := esPickList;
              PickList.Add(boolStr[False]);
              PickList.Add(boolStr[True]);
              ReadOnly := true;
            end;
    finally
      tmpList.Free;
    end;
  end;

var
  pt : PTypeData;
  i  : longint;
  pp : PPropList;
  theClass: TClass;
  PersistObj: TPersistent;
  tmpList: TStringlist;
begin
  eComponentInfo.Text := fObj.ClassName + ' : '+ fObj.Name;

  with vle do while rowcount > 1 do deleterow(rowcount -1);
  vle.Cells[0,0] := ''; vle.Cells[1,0] := '';

  pt:=GetTypeData(fObj.ClassInfo);
  fPropList.Clear;

  tmpList := TStringlist.Create;
  try
    //get the list of published properties and then sort them ...
    GetMem (pp, pt^.PropCount*SizeOf(Pointer));
    try
      GetPropInfos(fObj.ClassInfo, pp);
      for i :=0 to pt^.PropCount-1 do
        if fExcludList.IndexOf(pp^[i].Name) < 0 then fPropList.Add(pp^[i].Name);
      fPropList.Sort;
    finally
      FreeMem(pp);
    end;

    //now, add each property and value into the ValueListEditor grid ...
    for i :=0 to fPropList.Count-1 do
    begin
      case typinfo.PropType(fObj, fPropList[i]) of
        tkClass:
          begin
            PersistObj := TPersistent(GetOrdProp(fObj, fPropList[i]));
            theClass := GetObjectPropClass(fObj, fPropList[i]);
            if theClass.InheritsFrom(TComponent) then
            begin
              if not assigned(PersistObj) then
                AddComponent(fPropList[i], '', theClass)
              else if TComponent(PersistObj).Name <> '' then
                AddComponent(fPropList[i], TComponent(PersistObj).Name, theClass)
              else
                AddComponent(fPropList[i], format('{%8.8x}',[cardinal(PersistObj)]), theClass);
            end
            else if PersistObj is TStrings then
              AddMultiLines(fPropList[i], RTrimNonPrintChars(TStrings(PersistObj).Text))
            else if PersistObj is TFont then
              AddFont(fPropList[i], TFont(PersistObj))
            else if PersistObj is TPen then
              AddPen(fPropList[i], TPen(PersistObj));

          end;
        tkEnumeration:
          begin
            GetEnumList(fObj, fPropList[i], tmpList);
            AddEnumeration(fPropList[i], GetEnumProp(fObj, fPropList[i]), tmpList);
          end;
        tkInteger:
          begin
            if GetPropInfo(fObj.ClassType, fPropList[i])^.PropType^^.Name = 'TColor' then
              AddColor(fPropList[i], TColor(GetOrdProp(fObj, fPropList[i]))) else
              AddInteger(fPropList[i], GetOrdProp(fObj, fPropList[i]));
          end;
        tkString, tkLString: AddString(fPropList[i], GetStrProp(fObj, fPropList[i]));
        tkSet:
          begin
            GetSetList(fObj, fPropList[i], tmpList);
            AddSet(fPropList[i], GetSetProp(fObj, fPropList[i], false), tmpList);
          end;
      end; //case
    end;
  finally
    tmpList.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TObjInspectForm.RefreshAllProperties;
var
  i, setVal: integer;
  PropInfo: PPropInfo;
  EnumInfo: PTypeInfo;
  Obj, PersistObj: TPersistent;
  key, subkey: string;
begin
  eComponentInfo.Text := fObj.ClassName + ' : '+ fObj.Name;

  for i :=0 to vle.RowCount -1 do
  begin
    key := vle.Keys[i];
    split(' : ', key, subkey);

    if subkey = '' then
      Obj := fObj
    else if typinfo.PropType(fObj, key) = tkSet then
    begin
      //nb: Although Set members look like enumeration properties they aren't !!
      Obj := fObj;
      PropInfo := GetPropInfo(Obj, key);
      EnumInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;
      setVal := GetEnumValue(EnumInfo, subkey);
      vle.Cells[1,i] := boolStr[(setVal and GetOrdProp(Obj, key)) > 0];
      exit;
    end else
    begin
      Obj := TPersistent(GetOrdProp(fObj, key));
      key := subkey;
    end;

    case typinfo.PropType(Obj, key) of
      tkClass:
        begin
          PersistObj := TPersistent(GetOrdProp(Obj, key));
          if GetObjectPropClass(Obj, key).InheritsFrom(TComponent) then
          begin
            if not assigned(PersistObj) then
              vle.Cells[1,i] := ''
            else if TComponent(PersistObj).Name <> '' then
              vle.Cells[1,i] := TComponent(PersistObj).Name
            else
              vle.Cells[1,i] := format('{%8.8x}',[cardinal(PersistObj)]);

          end
          else if PersistObj is TStrings then
            vle.Cells[1,i] := RTrimNonPrintChars(TStrings(PersistObj).Text)
          else if PersistObj is TFont then
            vle.Cells[1,i] := FontToString(TFont(PersistObj));
        end;
      tkInteger:
        if GetPropInfo(Obj.ClassType, key)^.PropType^^.Name = 'TColor' then
          vle.Cells[1,i] := TColorToString(TColor(GetOrdProp(Obj, key))) else
          vle.Cells[1,i] := inttostr(GetOrdProp(Obj, key));
      tkEnumeration:
        vle.Cells[1,i] := GetEnumProp(Obj, key);
      tkString, tkLString:
        vle.Cells[1,i] := GetStrProp(fObj, key);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TObjInspectForm.vleEditButtonClick(Sender: TObject);
var
  key, subkey: string;
  Obj, PersistObj: TPersistent;
begin
  key := vle.Keys[vle.Row];
  split(' : ', key, subkey);

  if (subkey <> '') and (typinfo.PropType(fObj, key) = tkClass) then
  begin
    //nb: Set property also have subkey <> '' but Set property values
    //    shouldn't call EditButtonClick() either.
    Obj := TPersistent(GetOrdProp(fObj, key));
    key := subkey;
  end else
    Obj := fObj;

  case typinfo.PropType(Obj, key) of
    tkClass:
      begin
        PersistObj := TPersistent(GetOrdProp(Obj, key));
        if PersistObj is TFont then
        begin
          FontDialog1.Font.Assign(TFont(PersistObj));
          if FontDialog1.Execute then
            TFont(PersistObj).Assign(FontDialog1.Font);
          RefreshAllProperties;
        end;
      end;
    tkInteger:
      if GetPropInfo(Obj.ClassType, key)^.PropType^^.Name = 'TColor' then
      begin
        ColorDialog1.Color := TColor(GetOrdProp(Obj, key));
        if ColorDialog1.Execute then
          SetOrdProp(Obj, key, ColorDialog1.Color);
        RefreshAllProperties;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TObjInspectForm.vleValidate(Sender: TObject; ACol, ARow: Integer;
  const KeyName, KeyValue: String);
var
  key, subkey: string;
  Obj, PersistObj: TPersistent;
  ownerComp, comp: TComponent;
  intVal, SetVal: integer;
  PropInfo: PPropInfo;
  EnumInfo: PTypeInfo;
  KeyValStr: string;
begin
  KeyValStr := RTrimNonPrintChars(KeyValue);
  key := KeyName;
  split(' : ', key, subkey);
  ownerComp := fObj.Owner;

  if subkey = '' then
    Obj := fObj
  else if typinfo.PropType(fObj, key) = tkSet then
  begin
    //nb: Although Set members look like enumeration properties they aren't !!
    Obj := fObj;
    PropInfo := GetPropInfo(Obj, key);
    EnumInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;
    SetVal := GetEnumValue(EnumInfo, subkey);
    if KeyValStr = boolStr[True] then
      intVal := GetOrdProp(Obj, key) or SetVal else
      intVal := GetOrdProp(Obj, key) and not SetVal;
    SetOrdProp(Obj, key, intVal);
    RefreshAllProperties;
    exit;
  end else
  begin
    Obj := TPersistent(GetOrdProp(fObj, key));
    key := subkey;
  end;

  case typinfo.PropType(Obj, key) of
    tkClass:
      begin
        PersistObj := TPersistent(GetOrdProp(Obj, key));
        if GetObjectPropClass(Obj, key).InheritsFrom(TComponent) then
        begin
          if KeyValStr = '' then
          begin
            SetOrdProp(Obj, key, 0);
            vle.Cells[1,vle.Row] := '';
            exit;
          end
          else if assigned(ownerComp) then
          begin
            comp := ownerComp.FindComponent(KeyValStr);
            if assigned(comp) and (comp is GetObjectPropClass(Obj, key)) then
            begin
              SetOrdProp(Obj, key, integer(comp));
              RefreshAllProperties;
              exit;
            end;
          end;
        end
        else if PersistObj is TStrings then
        begin
          TStrings(PersistObj).text := KeyValStr;
          RefreshAllProperties;
          exit;
        end;
      end;
    tkString, tkLString:
      try
        SetStrProp(Obj, key, KeyValStr);
        RefreshAllProperties;
        exit;
      except
        //restores old value below
      end;
    tkInteger:
      try
        intVal := StrToInt(KeyValStr);
        SetOrdProp(Obj, key, intVal);
        RefreshAllProperties;
        exit;
      except
        //restores old value below
      end;
    tkEnumeration:
      if KeyValStr <> '' then
      try
        SetEnumProp(Obj, key, KeyValStr);
        RefreshAllProperties;
        exit;
      except
        //restores old value below
      end;
  end;
  vle.RestoreCurrentRow; //ie: assume error
end;
//------------------------------------------------------------------------------

procedure TObjInspectForm.vleKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    with vle do if ItemProps[Row].EditStyle <> esEllipsis then
      vleValidate(nil, 1, Row, Keys[Row], Cells[1, Row]);
    Key := #0;
  end;
end;
//------------------------------------------------------------------------------

procedure TObjInspectForm.vleSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
var
  key, subkey, theName: string;
  Obj, PersistObj: TPersistent;
begin
  //Picklists are tricky beasts and don't call OnValidate() when a selection
  //changes until leaving the row. Therefore, we'll PostMessage() to notify the
  //change (and avoid recursion). It's not perfect, but good enough ...
  with vle do
    if THackedValueListEditor(vle).EditList.Focused and (ARow = Row) and
      (ItemProps[Row].EditStyle = esPickList) then
    begin
      key := keys[Row];
      split(' : ', key, subkey);
      if subkey = '' then
        Obj := fObj
      else
      begin
        Obj := TPersistent(GetOrdProp(fObj, key));
        key := subkey;
      end;
      //since components (as well as enumerations) are handled by a picklist
      //we need to handle each differently ...
      if typinfo.PropType(Obj, key) = tkClass then
      begin
        PersistObj := TPersistent(GetOrdProp(Obj, key));
        if assigned(PersistObj) then theName := TComponent(PersistObj).Name
        else theName := '';
        if theName <> Value then
          PostMessage(self.handle, CM_FORCEPICKLISTUPDATE, 0, 0);
      end else if GetEnumProp(Obj, key) <> Value then
        PostMessage(self.handle, CM_FORCEPICKLISTUPDATE, 0, 0);
    end;
end;
//------------------------------------------------------------------------------

procedure TObjInspectForm.CMForcePicklistUpdate(var Message: TMessage);
begin
  with vle do if ItemProps[Row].EditStyle = esPickList then
    vleValidate(nil, 1, Row, Keys[Row], Cells[1, Row]);
  THackedValueListEditor(vle).EditList.SelectAll;
end;
//------------------------------------------------------------------------------

procedure TObjInspectForm.FormDeactivate(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------

procedure TObjInspectForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if assigned(fOnClosing) then fOnClosing(self);
  Action := caFree;
end;
//------------------------------------------------------------------------------

end.
