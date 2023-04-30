{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Epa;

{$warn 5023 off : no warning about unused units}
interface

uses
  NumEdit, OpenDlg, PgSetup, PSForm, VirtList, XPForm, Xprinter, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('NumEdit', @NumEdit.Register);
  RegisterUnit('OpenDlg', @OpenDlg.Register);
  RegisterUnit('PgSetup', @PgSetup.Register);
  RegisterUnit('VirtList', @VirtList.Register);
  RegisterUnit('Xprinter', @Xprinter.Register);
end;

initialization
  RegisterPackage('Epa', @Register);
end.
