% Interpolar deformacion en la posicion de las estaciones geodesicas
% Copyleft: Cesar Jimenez 30 May 2013

clear, clc
load xya, load xyo
xa = xa(IDS:IDE);
ya = ya(JDS:JDE);
A = load ('deform_a.grd');

B = load('tidal.txt');
% formato: long   lat
[p q] = size(B);
lon_i = B(:,1);
if lon_i < 0
    lon_i = lon_i + 360;
end
lat_i = B(:,2);

% Interpolacion 2d
z = interp2 (xa, ya, A', lon_i, lat_i, 'cubic');
z = 100*z   % cm

%for k = 1:p
%  z(k) = interp2 (xa, ya, A', lon_i(k), lat_i(k), 'cubic');
%  if num2str(z(k)) == 'NaN'
%    z(k) = 0.0;
%  end
%end

C = [lon_i lat_i z]

