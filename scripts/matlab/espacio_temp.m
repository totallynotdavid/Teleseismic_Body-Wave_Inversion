% Distribucion espacio-temporal de la fuente sismica
% Archivo de entrada: fort.37 modificado !!!
% Copyleft: Cesar Jimenez 31 May 2013
% Update: 07 Nov 2014 

clear, close all, clc
help espacio_temp.m
disp ('Slip history of seismic source')
A = load('fort.37'); %anim.txt'); % fort.37
[m n] = size(A);
t = A(:,1);
x = A(:,2);
y = -A(:,3); % signo invertido
rake = A(:,4);
M0 = A(:,5); %/(1e19);
M_max = floor(max(M0));
B = [t x y rake M0];

[t_new i] = sort(t);
for k = 1:m
   C(k,:) = B(i(k),:);
end
t = C(:,1);
x = C(:,2);
y = C(:,3);
M0 = C(:,5);

% define a grid for x and y
delta = input('Longitud de grilla (km): ');%20;
[xgrid, ygrid] = meshgrid([min(x):delta:max(x)],[min(y):delta:max(y)]);
xlin = xgrid(1,:);
ylin = ygrid(:,1);
C = zeros(length(xlin),length(ylin));

% Crear colormap
D = 1:-0.13:0;
%D = [1.0000 0.8700 0.7400 0.6100 0.4000 0.2000 0.1000 0.0500];
E = [D' D' D'];
colormap(E);


for k = 1:m-1
     i = find(xlin == x(k));
     j = find(ylin == y(k));
     C(i,j) = M0(k);
     pcolor(xlin,ylin,C'), shading interp, grid on, axis equal
     colorbar; caxis([0 M_max]);
     xlim([min(xlin) max(xlin)])
     ylim([min(ylin) max(ylin)])
     delta = t(k+1)-t(k);
     xlabel ('Strike direction (km)')
     ylabel ('Dip direction (km)')
     text(-2,-2,'*','FontSize',20);
     tiempo = num2str(t(k));
     if t(k) < 10
         if length(tiempo)==1
             tiempo = ['0',tiempo];
         end
         if length(tiempo)==2
             tiempo = [tiempo,'.0'];
         end
     end
     if t(k) >= 10
         if length(tiempo)==2
             tiempo = [tiempo,'.0'];
         end
     end
     title (['Slip history, Time = ',tiempo,' s'],'FontSize',10);
     pause (delta/100);
end
