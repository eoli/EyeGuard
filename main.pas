unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls
  , Menus
  , DateUtils
  , IniFiles
  , Splash;

type

  { TFormMain }

  TFormMain = class(TForm)
    BreakComboBox: TComboBox;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TrayMenuItemQuit: TMenuItem;
    TrayMenuItemBreakNow: TMenuItem;
    TrayMenuItemMain: TMenuItem;
    TrayMenuItemWork: TMenuItem;
    TrayPopupMenu: TPopupMenu;
    Separator1: TMenuItem;
    TrayIcon1: TTrayIcon;
    WorkTimer: TTimer;
    BreakTimer: TTimer;
    WorkComboBox: TComboBox;
    procedure BreakTimerTimer(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TrayMenuItemQuitClick(Sender: TObject);
    procedure TrayMenuItemBreakNowClick(Sender: TObject);
    procedure TrayMenuItemMainClick(Sender: TObject);
    procedure TrayMenuItemWorkClick(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure TrayIcon1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure WorkTimerTimer(Sender: TObject);
  private
    procedure stopWork();
    procedure startWork();
    procedure initAllTimers();
  public

  end;
const
  CONFIG_FILE = 'config.ini';

var
  FormMain: TFormMain;
  FormSplash: TFormSplash;
  nextBreakTime : TDateTime;

implementation

{$R *.lfm}

{ TFormMain }

{$define TRAY_MENU_BREAK 'break now !'}

procedure TFormMain.initAllTimers();
var
  workTime, breakTime: Integer;
begin
   WorkTimer.Enabled := False;
   BreakTimer.Enabled:=False;

   //load workTime and breakTime
   workTime := StrToInt(WorkComboBox.Text);
   breakTime := StrToInt(BreakComboBox.Text);

   // init timer
   WorkTimer.Interval:=workTime*60*1000;
   BreakTimer.Interval:=breakTime*60*1000;

end;

procedure TFormMain.startWork();
begin
  WorkTimer.Enabled := True;
  BreakTimer.Enabled := False;

  nextBreakTime := IncSecond(Now, WorkTimer.Interval div 1000);

  // hide splash
  FormSplash.Hide;
  TrayMenuItemBreakNow.Enabled:=True;
end;

procedure TFormMain.stopWork();
begin
  WorkTimer.Enabled := False;
  BreakTimer.Enabled := True;

  // show splash
  FormSplash.ShowOnTop;
  TrayMenuItemBreakNow.Enabled:=False;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  workTime, breakTime, workTimeItem, breakTimeItem: Integer;
  minWorkTime, maxWorkTime, minBreakTime, maxBreakTime: Integer;
  IniFile: TIniFile;
begin
   // load default value from config file
   try
     IniFile := TIniFile.Create(CONFIG_FILE);

     workTime := IniFile.ReadInteger('UserSettings', 'workTime', 25);
     breakTime := IniFile.ReadInteger('UserSettings', 'breakTime', 3);

     minWorkTime := IniFile.ReadInteger('SysSettings', 'minWorkTime', 5);
     maxWorkTime := IniFile.ReadInteger('SysSettings', 'maxWorkTime', 60);
     minBreakTime := IniFile.ReadInteger('SysSettings', 'minBreakTime', 1);
     maxBreakTime := IniFile.ReadInteger('SysSettings', 'maxBreakTime', 15);

     IniFile.Free;
   except
     on E: Exception do
       showMessage(E.Message);
   end;

   // setup UI
   for workTimeItem := minWorkTime to maxWorkTime do
   begin
        WorkComboBox.Items.Append(workTimeItem.ToString());
   end;

   for breakTimeItem := minBreakTime to maxBreakTime do
   begin
        BreakComboBox.Items.Append(breakTimeItem.ToString());
   end;

   BreakComboBox.Text:=breakTime.ToString;
   WorkComboBox.Text:=workTime.ToString;

   // create splash
   FormSplash := TFormSplash.Create(Self);

   // start timers
   initAllTimers();
   startWork();

end;

procedure TFormMain.TrayMenuItemMainClick(Sender: TObject);
begin
  FormMain.ShowOnTop;
end;

procedure TFormMain.TrayMenuItemWorkClick(Sender: TObject);
begin
  startWork;
end;

procedure TFormMain.TrayMenuItemBreakNowClick(Sender: TObject);
begin
   stopWork;
end;

procedure TFormMain.TrayMenuItemQuitClick(Sender: TObject);
begin
  Application.Terminate();
end;

procedure TFormMain.TrayIcon1DblClick(Sender: TObject);
begin
  FormMain.ShowOnTop;
end;


procedure TFormMain.TrayIcon1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  mins, secs: Integer;
begin
  mins := SecondsBetween(Now, nextBreakTime) div 60;
  secs := SecondsBetween(Now, nextBreakTime) mod 60;
  TrayIcon1.Hint:=  mins.ToString + ' minuts '+ secs.ToString +' seconds to next break time';
end;

procedure TFormMain.WorkTimerTimer(Sender: TObject);
begin
   stopWork;
end;

procedure TFormMain.BreakTimerTimer(Sender: TObject);
begin
   startWork;
end;

procedure TFormMain.ButtonOKClick(Sender: TObject);
var
   IniFile: TIniFile;
begin
  //save settings
   try
     IniFile := TIniFile.Create(CONFIG_FILE);
     IniFile.WriteInteger('UserSettings', 'workTime', StrToInt(WorkComboBox.Text));
     IniFile.WriteInteger('UserSettings', 'breakTime', StrToInt(BreakComboBox.Text));
     IniFile.Free;
   except
     on E: Exception do
       showMessage(E.Message);
   end;

  // init timers
  FormMain.Hide;

  initAllTimers();
  startWork;

end;

procedure TFormMain.ButtonCancelClick(Sender: TObject);
begin
  FormMain.Hide;
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FormMain.Hide;
  CloseAction := caNone;
end;



end.

