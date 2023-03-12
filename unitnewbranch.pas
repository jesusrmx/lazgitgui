unit unitnewbranch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ButtonPanel, ExtCtrls;

type

  { TfrmNewBranch }

  TfrmNewBranch = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Panel1: TPanel;
    tabSource: TTabControl;
    txtName: TEdit;
    ListBox1: TListBox;
    radNew: TRadioButton;
    radTracking: TRadioButton;
  private

  public

  end;

var
  frmNewBranch: TfrmNewBranch;

implementation

{$R *.lfm}

end.

