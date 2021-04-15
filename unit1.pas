//Copyright 2021 Andrey S. Ionisyan (anserion@gmail.com)
//
//Licensed under the Apache License, Version 2.0 (the "License");
//you may not use this file except in compliance with the License.
//You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
//Unless required by applicable law or agreed to in writing, software
//distributed under the License is distributed on an "AS IS" BASIS,
//WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//See the License for the specific language governing permissions and
//limitations under the License.

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ExtDlgs, FPCanvas, LCLintf, LCLType, VLC_video_form, math, lclvlc, libvlc, vlc;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel_Layer3: TBevel;
    Bevel_receptors: TBevel;
    BTN_nw_reset: TButton;
    BTN_s_clear: TButton;
    CB_timer: TCheckBox;
    CB_VLC_video: TCheckBox;
    CB_camera: TCheckBox;
    CB_noise: TCheckBox;
    CB_contrast: TCheckBox;
    CB_bitmapFile: TCheckBox;
    Edit_L1_inputs: TEdit;
    Edit_contrast: TEdit;
    Edit_noise: TEdit;
    Edit_timer: TEdit;
    Edit_N_L1: TEdit;
    Edit_N_L2: TEdit;
    Edit_N_L3: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label_Layer3: TLabel;
    Label2: TLabel;
    OpenPictureDialog: TOpenPictureDialog;
    PB_Layer3: TPaintBox;
    PB_receptors: TPaintBox;
    Timer1: TTimer;
    procedure BTN_nw_resetClick(Sender: TObject);
    procedure BTN_s_clearClick(Sender: TObject);
    procedure CB_bitmapFileChange(Sender: TObject);
    procedure CB_contrastChange(Sender: TObject);
    procedure CB_noiseChange(Sender: TObject);
    procedure CB_timerChange(Sender: TObject);
    procedure CB_VLC_videoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PB_Layer3Paint(Sender: TObject);
    procedure PB_receptorsPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure Forward_step;
    procedure BackTraceError_step;
    procedure BackTraceLearn_step;
  public

  end;

const
  s_width=256;
  s_height=256;
  alpha_BPA=0.5;

var
  Form1: TForm1;
  VideofileName:string;
  CameraBitmap: TBitmap;
  receptorsBitmap,L3_bitmap: TBitmap;
  img_buffer:array[0..1023,0..1023]of real;
  n_L1_inputs,n_L1,n_L2:integer;

  L1_w:array of array of real;
  L1_scalar:array of real;
  L1_out:array of real;
  L1_map_S_x:array of array of integer;
  L1_map_S_y:array of array of integer;

  L2_w:array of array of real;
  L2_scalar:array of real;
  L2_out:array of real;

  L3_w:array[1..s_width,1..s_height] of array of real;
  L3_scalar:array[1..s_width,1..s_height]of real;
  L3_out:array[1..s_width,1..s_height]of real;

  S_elements: array[1..s_width,1..s_height]of real;
  Target_elements: array[1..s_width,1..s_height]of real;

  sigma1:array of real;
  sigma2:array of real;
  sigma3:array[1..s_width,1..s_height] of real;

  error_target_to_L3:array[1..s_width,1..s_height]of real;
  error_L3_to_L2:array of real;
  error_L2_to_L1:array of real;

  BackTrace_flag:boolean;

implementation

{$R *.lfm}

function sigmoid(x:real):real;
begin sigmoid:=1/(1+exp(-x)); end;

function der_sigmoid(y:real):real;
begin der_sigmoid:=y*(1-y); end;

function tanh(x:real):real;
begin tanh:=(exp(x)-exp(-x))/(exp(x)+exp(-x)); end;

function der_tanh(y:real):real;
begin der_tanh:=1-y*y; end;

function ReLU(x:real):real;
begin if x<0 then ReLU:=0 else ReLU:=1; end;

function der_ReLU(x:real):real;
begin if x<0 then der_ReLU:=1 else der_ReLU:=1; end;

{ TForm1 }

procedure TForm1.Forward_step;
var i,k,cell_x,cell_y:integer;
begin
  BackTrace_flag:=false;

  for k:=1 to n_L1 do
  begin
    L1_scalar[k]:=0;
    for i:=1 to n_L1_inputs do
      L1_scalar[k]:=L1_scalar[k]+L1_w[k,i]*S_elements[L1_map_S_x[k,i],L1_map_S_y[k,i]];
    L1_out[k]:=sigmoid(L1_scalar[k]);
  end;

  for k:=1 to n_L2 do
  begin
    L2_scalar[k]:=0;
    for i:=1 to n_L1 do L2_scalar[k]:=L2_scalar[k]+L2_w[k,i]*L1_out[i];
    L2_out[k]:=sigmoid(L2_scalar[k]);
  end;

  for cell_x:=1 to s_width do
    for cell_y:=1 to s_height do
    begin
      L3_scalar[cell_x,cell_y]:=0;
      for i:=1 to n_L2 do
        L3_scalar[cell_x,cell_y]:=L3_scalar[cell_x,cell_y]+L3_w[cell_x,cell_y,i]*L2_out[i];
      L3_out[cell_x,cell_y]:=sigmoid(L3_scalar[cell_x,cell_y]);
    end;
end;

procedure TForm1.BackTraceError_step;
var cell_x,cell_y,i,k:integer;
begin
  for cell_x:=1 to s_width do
    for cell_y:=1 to s_height do
    begin
      error_target_to_L3[cell_x,cell_y]:=-(Target_elements[cell_x,cell_y]-L3_out[cell_x,cell_y]);
      sigma3[cell_x,cell_y]:=error_target_to_L3[cell_x,cell_y]*der_sigmoid(L3_out[cell_x,cell_y]);
    end;

  for i:=1 to n_L2 do
  begin
    error_L3_to_L2[i]:=0;
    for cell_x:=1 to s_width do
      for cell_y:=1 to s_height do
        error_L3_to_L2[i]:=error_L3_to_L2[i]+sigma3[cell_x,cell_y]*L3_w[cell_x,cell_y,i];
    sigma2[i]:=error_L3_to_L2[i]*der_sigmoid(L2_out[i]);
  end;

  for i:=1 to n_L1 do
  begin
    error_L2_to_L1[i]:=0;
    for k:=1 to n_L2 do
      error_L2_to_L1[i]:=error_L2_to_L1[i]+sigma2[k]*L2_w[k,i];
    sigma1[i]:=error_L2_to_L1[i]*der_sigmoid(L1_out[i]);
  end;
end;

procedure TForm1.BackTraceLearn_step;
var cell_x,cell_y,i,k:integer;
begin
  for i:=1 to n_L1 do
    for k:=1 to n_L1_inputs do
      L1_w[i,k]:=L1_w[i,k]-alpha_BPA*sigma1[i]*S_elements[L1_map_S_x[i,k],L1_map_S_y[i,k]];

  for i:=1 to n_L2 do
    for k:=1 to n_L1 do
      L2_w[i,k]:=L2_w[i,k]-alpha_BPA*sigma2[i]*L1_out[k];

  for cell_x:=1 to s_width do
    for cell_y:=1 to s_height do
      for k:=1 to n_L2 do
        L3_w[cell_x,cell_y,k]:=L3_w[cell_x,cell_y,k]-
                               alpha_BPA*sigma3[cell_x,cell_y]*L2_out[k];
end;

procedure TForm1.PB_Layer3Paint(Sender: TObject);
var x,y,sx,sy:integer; dx,dy:real; contrast_value:real; C:real;
var dst_bpp:integer; dst_ptr:PByte; R,G,B:byte;
begin
  dx:=PB_Layer3.width/s_width;
  dy:=PB_Layer3.Height/s_height;
  if CB_contrast.Checked
  then contrast_value:=StrToFloat(Edit_contrast.text)/100
  else contrast_value:=1;

  for x:=1 to s_width do
  for y:=1 to s_height do
  begin
    C:=(L3_out[x,y]-0.5)*contrast_value+0.5;
    if C<0 then C:=0;
    if C>1 then C:=1;
    for sx:=0 to trunc(dx) do
    for sy:=0 to trunc(dy) do
      img_buffer[trunc(sx+(x-1)*dx),trunc(sy+(y-1)*dy)]:=C;
  end;

  L3_Bitmap.BeginUpdate(false);
  dst_ptr:=L3_Bitmap.RawImage.Data;
  dst_bpp:=L3_Bitmap.RawImage.Description.BitsPerPixel div 8;
  for y:=0 to L3_Bitmap.height-1 do
  for x:=0 to L3_Bitmap.width-1 do
  begin
     R:=trunc(img_buffer[x,y]*255); G:=R; B:=R;
     dst_ptr^:=B; (dst_ptr+1)^:=G; (dst_ptr+2)^:=R; inc(dst_ptr,dst_bpp);
  end;
  L3_Bitmap.EndUpdate(false);
  PB_Layer3.Canvas.Draw(0,0,L3_Bitmap);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CameraBitmap:=TBitmap.Create;
  CameraBitmap.SetSize(256,256);
  //CameraBitmap.SetSize(VLC_Form.width,VLC_Form.height);
  receptorsBitmap:=TBitmap.Create;
  receptorsBitmap.SetSize(PB_receptors.width,PB_receptors.height);
  L3_Bitmap:=TBitmap.Create;
  L3_Bitmap.SetSize(PB_Layer3.width,PB_Layer3.height);
  BTN_nw_resetClick(self);
end;

procedure TForm1.PB_receptorsPaint(Sender: TObject);
var x,y,sx,sy:integer; dx,dy:real;
var dst_bpp:integer; dst_ptr:PByte; R,G,B:byte;
begin
  dx:=PB_receptors.width/s_width;
  dy:=PB_receptors.Height/s_height;
  for x:=1 to s_width do
  for y:=1 to s_height do
    for sx:=0 to trunc(dx) do
    for sy:=0 to trunc(dy) do
      img_buffer[trunc(sx+(x-1)*dx),trunc(sy+(y-1)*dy)]:=S_elements[x,y];

  receptorsBitmap.BeginUpdate(false);
  dst_ptr:=receptorsBitmap.RawImage.Data;
  dst_bpp:=receptorsBitmap.RawImage.Description.BitsPerPixel div 8;
  for y:=0 to receptorsBitmap.height-1 do
  for x:=0 to receptorsBitmap.width-1 do
  begin
     R:=trunc(img_buffer[x,y]*255); G:=R; B:=R;
     dst_ptr^:=B; (dst_ptr+1)^:=G; (dst_ptr+2)^:=R; inc(dst_ptr,dst_bpp);
  end;
  receptorsBitmap.EndUpdate(false);
  PB_receptors.Canvas.Draw(0,0,receptorsBitmap);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var cell_x,cell_y:integer; dx,dy:real; C:real; noise_value:real;
var CameraPanel_DC:HDC;
var x,y,src_bpp:integer; Src_ptr:PByte; R,G,B:word;
begin
  if VLC_Form.visible and (VideofileName<>'') then
  begin
    if VLC_Player.State=libvlc_Ended then VLC_player.PlayFile(VideofileName);
    CameraPanel_DC:=LCLIntf.GetDC(VLC_Form.Handle);
    CameraBitmap.LoadFromDevice(CameraPanel_DC);
    LCLIntF.ReleaseDC(CameraBitmap.Handle,CameraPanel_DC);
  end;

  src_ptr:=CameraBitmap.RawImage.Data;
  src_bpp:=CameraBitmap.RawImage.Description.BitsPerPixel div 8;
  for y:=0 to CameraBitmap.height-1 do
  for x:=0 to CameraBitmap.width-1 do
  begin
    R:=(src_ptr+2)^; G:=(src_ptr+1)^; B:=src_ptr^; inc(src_ptr,src_bpp);
    img_buffer[x,y]:=(R+G+B)/(256.0*3.0);
  end;

  dx:=CameraBitmap.Width/s_width;
  dy:=CameraBitmap.Height/s_height;
  for cell_x:=1 to s_width do
    for cell_y:=1 to s_height do
    begin
      C:=0;
      for x:=0 to trunc(dx) do
        for y:=0 to trunc(dy) do
          C:=C+img_buffer[trunc(x+(cell_x-1)*dx),trunc(y+(cell_y-1)*dy)];
      S_elements[cell_x,cell_y]:=C/((dx+1)*(dy+1));
    end;

  if CB_noise.Checked then
  begin
    noise_value:=StrToFloat(Edit_noise.text)/100.0;
    for cell_x:=1 to s_width do
    for cell_y:=1 to s_height do
      if random<=noise_value then S_elements[cell_x,cell_y]:=random;
  end;

  for cell_x:=1 to s_width do
    for cell_y:=1 to s_height do
      Target_elements[cell_x,cell_y]:=S_elements[cell_x,cell_y];

  Forward_step;
  BackTraceError_step;
  BackTraceLearn_step;
  PB_receptorsPaint(self);
  PB_Layer3Paint(PB_Layer3);
end;

procedure TForm1.BTN_nw_resetClick(Sender: TObject);
var i,k,cell_x,cell_y:integer;
begin
     randomize;
     n_L1_inputs:=StrToInt(Edit_L1_inputs.text);
     n_L1:=StrtoInt(Edit_N_L1.text);
     SetLength(L1_w,n_L1+1);
     SetLength(L1_scalar,n_L1+1);
     SetLength(L1_out,n_L1+1);
     SetLength(L1_map_S_x,n_L1+1);
     SetLength(L1_map_S_y,n_L1+1);
     for k:=1 to n_L1 do
     begin
       SetLength(L1_w[k],n_L1_inputs+1);
       SetLength(L1_map_S_x[k],n_L1_inputs+1);
       SetLength(L1_map_S_y[k],n_L1_inputs+1);
     end;

     n_L2:=StrToInt(Edit_N_L2.text);
     SetLength(L2_w,n_L2+1);
     for k:=1 to n_L2 do SetLength(L2_w[k],n_L1+1);
     SetLength(L2_scalar,n_L2+1);
     SetLength(L2_out,n_L2+1);

     SetLength(sigma1,n_L1+1);
     SetLength(sigma2,n_L2+1);
     SetLength(error_L3_to_L2,n_L2+1);
     SetLength(error_L2_to_L1,n_L1+1);

     for cell_x:=1 to s_width do
       for cell_y:=1 to s_height do
           SetLength(L3_W[cell_x,cell_y],n_L2+1);

     for k:=1 to n_L1 do
       for i:=1 to n_L1_inputs do
       begin
         L1_w[k,i]:=(random-0.5)/100;
         L1_map_S_x[k,i]:=random(s_width)+1;
         L1_map_S_y[k,i]:=random(s_height)+1;
       end;

     for k:=1 to n_L2 do
       for i:=1 to n_L1 do
         L2_w[k,i]:=(random-0.5)/100;

     for cell_x:=1 to s_width do
       for cell_y:=1 to s_height do
         for i:=1 to n_L2 do
           L3_w[cell_x,cell_y,i]:=(random-0.5)/100;

     Edit_N_L3.text:=IntToStr(s_width*s_height);
     BackTrace_flag:=false;
     Forward_step;
     PB_Layer3Paint(PB_Layer3);
end;

procedure TForm1.BTN_s_clearClick(Sender: TObject);
var cell_x,cell_y:integer;
begin
  CameraBitmap.clear;

  for cell_x:=1 to length(img_buffer[1]) do
  for cell_y:=1 to length(img_buffer) do
      img_buffer[cell_x,cell_y]:=0;

  for cell_x:=1 to s_width do
  for cell_y:=1 to s_height do
      S_elements[cell_x,cell_y]:=0;
  PB_receptorsPaint(self);
end;

procedure TForm1.CB_bitmapFileChange(Sender: TObject);
begin
  if CB_bitmapFile.Checked then
  begin
    VLC_player.stop;
    CB_VLC_video.Checked:=false;
    CB_camera.Checked:=false;
    if OpenPictureDialog.execute then
    begin
      VideofileName:=OpenPictureDialog.FileName;
      CameraBitmap.LoadFromFile(VideofileName);
      CB_timer.Checked:=true;
    end
    else VideofileName:='';
  end else CB_timer.Checked:=false;
end;

procedure TForm1.CB_contrastChange(Sender: TObject);
begin
  Edit_contrast.ReadOnly:=CB_contrast.Checked;
end;

procedure TForm1.CB_noiseChange(Sender: TObject);
begin
    Edit_noise.ReadOnly:=CB_noise.Checked;
end;

procedure TForm1.CB_timerChange(Sender: TObject);
begin
  Timer1.Interval:=StrToInt(Edit_timer.Text);
  Timer1.enabled:=CB_timer.Checked;
end;

procedure TForm1.CB_VLC_videoChange(Sender: TObject);
begin
  VLC_Form.Visible:=CB_VLC_video.Checked;
  if CB_VLC_video.Checked
  then
    begin
      VLC_Form.Left:=0; VLC_Form.Top:=0;
      VLC_player.stop;
      if CB_camera.Checked then
      begin
        VideofileName:='dshow://'; //worked OK under windows
        //VideofileName:='v4l2:///dev/video0'; //not worked!
        VLC_player.PlayFile(VideofileName);
      end
      else
        if OpenPictureDialog.Execute
        then
          begin
            VideofileName:=OpenPictureDialog.Filename;
            VLC_player.PlayFile(VideofileName);
        end else VideofileName:='';;
    end
  else begin VLC_player.Stop; VideofileName:=''; end;
  if VideofileName='' then
  begin
    VLC_Form.Visible:=false;
    CB_VLC_video.Checked:=false;
  end;
end;

end.

