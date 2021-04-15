unit VLC_video_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  math, lclvlc, libvlc, vlc;

type

  { TVLC_Form }

  TVLC_Form = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  VLC_Form: TVLC_Form;
  VLC_player: TLCLVLCPlayer;

implementation

{$R *.lfm}

{ TVLC_Form }

procedure TVLC_Form.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  VLC_player.Stop;
  VLC_player.Free;
end;

procedure TVLC_Form.FormCreate(Sender: TObject);
begin
//  if VLC_Form.visible then
//  begin
    VLC_player:=TLCLVLCPlayer.create(self);
    VLC_player.ParentWindow:=VLC_Form;
    VLC_player.FitWindow:=true;
//  end;
end;

end.

