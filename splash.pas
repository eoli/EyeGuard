unit Splash;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls;

type

  { TFormSplash }

  TFormSplash = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormSplash: TFormSplash;

implementation

{$R *.lfm}

{ TFormSplash }

procedure TFormSplash.FormCreate(Sender: TObject);
var
  sLeft, sTop, sWidth, sHeight: Integer;
  imWidth, imHeight, imWidthLimit, imHeightLimit: Integer;
begin
  imWidthLimit := Round(screen.Width * 0.8);
  imHeightLimit := Round(screen.Height * 0.8);

  try
     Image1.Picture.LoadFromFile('bg.png');
  except
     Image1.Picture.Bitmap.Width := imWidthLimit;
     Image1.Picture.Bitmap.Height := imHeightLimit;
     Image1.Picture.Bitmap.Canvas.Brush.Color:=clGreen;
     Image1.Picture.Bitmap.Canvas.FillRect(0,0,imWidthLimit,imHeightLimit);
  end;
  imWidth := Image1.Picture.Width;
  imHeight := Image1.Picture.Height;

  // limit picture width and height
  if imWidth > imWidthLimit then imWidth := imWidthLimit;
  if imHeight > imHeightLimit then imHeight := imHeightLimit;

  //sLeft := Round(screen.Width * 0.1);
  //sTop := Round(screen.Height * 0.1);
  //sWidth := screen.Width - 2*sLeft;
  //sHeight := screen.Height - 2*sTop;

  sLeft := (screen.Width - imWidth) div 2;
  sTop := (screen.Height - imHeight) div 2;
  sWidth := screen.Width - 2*sLeft;
  sHeight := screen.Height - 2*sTop;

  Width := sWidth;
  Height := sHeight;
  Left := sLeft;
  Top := sTop;

end;

end.

