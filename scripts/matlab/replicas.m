% Plotea las replicas para delinear la geometria de ruptura
% Cesar Jimenez 27 Dic 2014
% Update: 03 Ago 2015
  clear all; close all; clc
  
  xep = -76.04; yep=-15.30; zep =14; % coordenadas epicentro
  xo = -75.4; yo = -15.9; % extremo inferior (o ext sup para Japon)
  L = 150000; % Largo o longitud del pano de falla; (en metros)
  W = 80000; % Ancho del plano de falla; (metros)
  slip=1; % dislocacion de la falla (m)
  H = 5000;  % Profundidad de la parte superior de la falla
  Az= 310;     % strike, rumbo, azimuth
  echado = 19; % (dip), buzamiento
% *************************************************************
   W1 = W*cos(echado*pi/180); % *****************************************
   beta = atan(W1/L)*180/pi; % ******************************************
   alfa = Az - 270;% ****************************************************
   h = sqrt(L*L+W1*W1);% ************************************************
   a = 0.5*h*sin((alfa+beta)*pi/180)/1000;% *****************************
   b = 0.5*h*cos((alfa+beta)*pi/180)/1000;% *****************************
   % Epicentroide o centro de gravedad (xe, ye)
   xe = xo - km2deg(b); %xo = xe+km2deg(b);% ***************************
   ye = yo + km2deg(a); %yo = ye-km2deg(a);% ***************************
disp ('Cargando archivo, espere ...')
if xo < 0 xo = xo + 360; end
if xep < 0 xep = xep + 360; end

load perfil.txt;
lonp = perfil(:,2)+360;
latp = perfil(:,1);
load xya;
load grid_a.grd;
%I0=find(abs(xa-xo) == min(abs(xa-xo)) ); I0=I0(1);
%J0=find(abs(ya-yo) == min(abs(ya-yo)) ); J0=J0(1);

figure; hold on;
contour(xa,ya,grid_a',[0 0],'black');
text (282.384, -11.122,'Huacho');
text (282.852, -12.052,'Callao');
text (283.600, -13.090,'Canete');
axis equal; grid on; zoom on;

% Leer archivo de replicas
fname = 'replicas.txt';
fid = fopen(fname, 'r');
feof(fid) = 0;
lat = [];   lon = []; i = 1;
while feof(fid) == 0
   linea2 = fgetl(fid);
   if linea2 == -1
       break
   end
   if linea2(1) == '%'
       % disp ('Borrar primera linea de replicas.txt')
       i = i+1;
   else
     vector = str2num(linea2(26:42));
     lat = [lat, vector(1)];
     lon = [lon, vector(2)];
   end
end
if lon < 0 lon = lon + 360; end
hold on
plot (lon,lat,'o'), grid on
% Fin de leer Replicas

dip=echado*pi/180; 
a1=-(Az-90)*pi/180; a2=-(Az)*pi/180;
r1=L; r2=W*cos(dip);
r1=r1/(60*1853); r2=r2/(60*1853);
sx(1)=0;          sy(1)=0;
sx(2)=r1*cos(a1); sy(2)=r1*sin(a1);
sx(4)=r2*cos(a2); sy(4)=r2*sin(a2);
sx(3)=sx(4)+sx(2);sy(3)=sy(4)+sy(2);
sx(5)=sx(1)      ;sy(5)=sy(1);

sx=sx + xo; sy=sy + yo;
plot(sx,sy,'k','linewidth',2);
px=xo; py=yo; % origen del plano de falla de acuerdo al modelo de tsunamis
plot(px,py,'ro',xo,yo,'bo'), grid on
plot (xep,yep,'*'), zoom on
plot (lonp,latp);
