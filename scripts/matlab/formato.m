% cambio de formato de fort.46 a coordenadas geograficas
% Copyleft: Cesar Jimenez 31 May 2013
% Update: 12 Ene 2016

clear, close all, clc
dip = 16; dip_rad = dip*pi/180;
strike = 332;
theta = 360 - strike; theta_rad = theta*pi/180;
L = 20; % largo unitario km
W = 20; % ancho unitario km

load perfil.txt
latp = perfil(:,1);
lonp = perfil(:,2)+360;

% coordenadas del epicentro
load hypo; 
lat_e = hypo(1);
lon_e = hypo(2);
if lon_e < 0 lon_e = lon_e + 360; end
prof  = 20; % resultado de la inversion (km)

% leer datos del archivo fort.46
A = load ('fort.46');
[m n] = size(A);
y1 = A(:,1);
x2 = A(:,2); 
slip = A(:,3);
rake = A(:,4);
dip = ones(m,1)*dip;
strike = ones(m,1)*strike;
L = ones(m,1)*L;
W = ones(m,1)*W;

% Esquina inferior izquierda
%y1 = y1 - L/2;
%x2 = x2 - W/2;

% proyectar el plano de ruptura sobre la superficie
x1 = x2*cos(dip_rad);
z1 = x2*sin(dip_rad); 

% convertir distancias de km a deg
x1 = km2deg(x1);
y1 = km2deg(y1);

% rotacion de ejes
x = x1*cos(theta_rad) - y1*sin(theta_rad);
y = x1*sin(theta_rad) + y1*cos(theta_rad);

% traslacion de ejes con respecto al epicentro
x = x + lon_e;
y = y + lat_e;
if x < 0 
  x = x + 360;
end
z = z1 + prof;
% plot (x,y,'.',lonp,latp,lon_e,lat_e,'*') 
% grid on, zoom, axis equal

% Eliminar los slip nulos
fid = fopen('pfalla.kan','wt');
for k = 1:m
%   if rake(k)~=0 & slip(k)~=0
        fprintf(fid,'%5.1f %5.1f %5.1f %6.1f %6.1f',L(k),W(k),z(k),strike(k),dip(k));
        fprintf(fid,'%7.1f %6.2f %9.4f %9.4f',rake(k),slip(k),y(k),x(k));
        fprintf(fid,'\r\n');
%   end
end
fclose(fid);
type pfalla.kan    
disp ('Se creo el archivo: pfalla.kan')

% B = [L W z strike dip rake slip y x];

plot (x,y,'.',lonp,latp,lon_e,lat_e,'*') 
grid on, zoom, axis equal

