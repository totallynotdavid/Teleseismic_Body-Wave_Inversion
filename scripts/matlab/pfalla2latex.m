% Convertir formato de pfalla a latex
% 18 May 2023

A = load('pfalla.new');
[m n] = size(A);
lon = A(:,9)-360;
lat = A(:,8);
slip = A(:,7);
h = A(:,3);
str = A(:,4);
dip = A(:,5);
rak = A(:,6);

for k = 1:m
disp([num2str(k),' &',num2str(lat(k)),' & ',num2str(lon(k)),' & ',num2str(h(k)),' & ',num2str(str(k)),' & ',num2str(dip(k)),' & ',num2str(rake(k)),' & ',num2str(slip(k)),' \\'])
end