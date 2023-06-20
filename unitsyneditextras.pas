unit unitsyneditextras;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Menus, SynEdit,
  unitcommon;

type

  { TSEPopupMenu }

  TSEPopupMenu = class(TPopupMenu)
  private
    fSynEdit: TSynEdit;
    procedure OnMenuItemClick(Sender: TObject);
    procedure OnSynEditPopUp(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    procedure AddReadonlyEntries;
    property SynEdit: TSynEdit read fSynEdit write fSynEdit;
  end;

  procedure AddPopupMenu(txt: TSynEdit);

implementation

const
  MENUITEM_COPY       = 1;
  MENUITEM_SELECTALL  = 2;

procedure AddPopupMenu(txt: TSynEdit);
var
  pop: TSEPopupMenu;
  mi: TMenuItem;
begin
  if txt.PopupMenu<>nil then
    exit;

  pop := TSEPopupMenu.Create(txt.Owner);
  pop.SynEdit := txt;
  pop.AddReadOnlyEntries;

  txt.PopupMenu := pop;
end;

{ TSEPopupMenu }

procedure TSEPopupMenu.OnMenuItemClick(Sender: TObject);
var
  mi: TMenuItem absolute Sender;
begin
  case mi.Tag of
    MENUITEM_COPY:      fSynEdit.CopyToClipboard;
    MENUITEM_SELECTALL: fSynEdit.SelectAll;
  end;
end;

procedure TSEPopupMenu.OnSynEditPopUp(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to Items.Count-1 do begin
    case items[i].Tag of
      MENUITEM_COPY:      items[i].enabled := fSynEdit.SelAvail;
      MENUITEM_SELECTALL: items[i].enabled := fSynEdit.Text<>'';
    end;
  end;
end;

constructor TSEPopupMenu.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  OnPopup := @OnSynEditPopUp;
end;

procedure TSEPopupMenu.AddReadonlyEntries;
var
  mi: TMenuItem;
begin

  Items.Clear;

  mi := TMenuItem.Create(Self);
  mi.Caption := rsCopy;
  mi.Tag := MENUITEM_COPY;
  mi.OnClick := @OnMenuItemClick;
  Items.Add(mi);

  mi := TMenuItem.Create(Self);
  mi.Caption := '-';
  Items.Add(mi);

  mi := TMenuItem.Create(Self);
  mi.Caption := rsSelectAll;
  mi.Tag := MENUITEM_SELECTALL;
  mi.OnClick := @OnMenuItemClick;
  Items.Add(mi);
end;

end.

