%----------------------------------------= Bento Caldeira 2004 =----%
%Programa Matlab:                                                   %
%fotogramas.m                                                       %
%-------------------------------------------------------------------%
% Programa que gerar fotogramas da ruptura a partir dos dados do    %
% ficheiro 'filme.dat' gerado pela versao do programa de inversao   %
% de slip MOM3, que se deu o nome de MOM3_filme.f. O ficheiro e     %
% constituido por cinco colunas de dados que correspondem a:        %
% T-tempo, X-posições ao longo do eixo strike, Y-posições eixo dip, %
% Rake-angulo de deslizamento, M0- momento sísmico.                 % 
% Cada imagem representa o deslizamento, que ocorreu sobre o plano  %
% de falha numa janela temporal cujo valor e representado por cima  %                                                               %
% da respectiva imagem. O deslizamento de cada sub-falha e dado,    %
% em metros numa escala de cor tambem representada. O numero de     %
% fotogramas a representar deve ser impar e e dado na linha deste   %
% programa com essa indicacao. O ultimo fotograma (inferior direito)%
% representa o deslizamento total produzido sobre a falha.          %
%-------------------------------------------------------------------%
%-LEITURA DE DADOS E ATRIBUIÇÃO DE NOMES ÀS VARIÀVEIS               %                                                         %
clear, close, clc
rad=pi/180;
load filme.dat
nx=filme(1,1);
nx0=filme(1,2);
dx=filme(1,3);
ny=filme(2,1);
ny0=filme(2,2);
dy=filme(2,3);
da=dx*dy*1e6;
rig=4.0e10;
T=filme(3:length(filme),1);
X=filme(3:length(filme),2);
Y=filme(3:length(filme),3);
RAKE=filme(3:length(filme),4);
M0=filme(3:length(filme),5);
tmax=max(T);
tmin=min(T);
%------------------------------------
%ATRIBUICAO DO NUMERO DE FOTOGRAMAS (IMPAR)
nfram=9;
%------------------------------------
dt=(tmax-tmin)/nfram;
t2=tmin;
MS(1:nx,1:ny,1:nfram)=0;
clear filme
for k=1:nfram
   t1=t2;
   t2=t1+dt;
   MSx(1:nx,1:ny)=0;
   MSy(1:nx,1:ny)=0;
   for j=1:length(T)
      if T(j)>t1 & T(j)<=t2
         M0x=M0(j)*cos(RAKE(j)*rad);
         M0y=M0(j)*sin(RAKE(j)*rad);
         l=fix(X(j)/dx+nx0+.5);
         c=fix(Y(j)/dy+ny0+.5);
         MSx(l,c)=MSx(l,c)+M0x;
         MSy(l,c)=MSy(l,c)+M0y;
      end
   end
   for l=1:nx
      for c=1:ny
         MS(l,c,k)=sqrt(MSx(l,c)^2+MSy(l,c)^2);
      end
   end           
end
%Nesta fase do programa estao criadas 9 vectores com o valor do momento de cada ponto da frame
%cria frame com o total (MStot)
MSxtot(1:nx,1:ny)=0;
MSytot(1:nx,1:ny)=0;
for j=1:length(T)
   M0x=M0(j)*cos(RAKE(j)*rad);
   M0y=M0(j)*sin(RAKE(j)*rad);
   l=fix(X(j)/dx+nx0+.5);
   c=fix(Y(j)/dy+ny0+.5);
   MSxtot(l,c)=MSxtot(l,c)+M0x;
   MSytot(l,c)=MSytot(l,c)+M0y;
end
MStot(1:nx,1:ny)=0;
for l=1:nx
   for c=1:ny
      MStot(l,c)=sqrt(MSxtot(l,c)^2+MSytot(l,c)^2);
   end
end  
MS=MS/(rig*da);
MStot=MStot/(rig*da);
% Preparativos da grelha das frames
xmin=-(nx0-1)*dx;
ymin=-(ny0-1)*dy;
xmax=(nx-nx0)*dx;
ymax=(ny-ny0)*dy;
x=[xmin:dx:xmax];
y=[ymin:dy:ymax];
xi=xmin:dx/6:xmax;
yi=(ymin:dy/6:ymax)';
caxis([min(min(MStot)) max(max(MStot))])
C=caxis
    t1=0;
for k=1:nfram
    t2=t1+dt;
   %escolha da posicao do grafico
   if k<=fix(nfram/2+.5);
      p=(2*k-1);
   else
      p=2*(k-fix(nfram/2+.5));
   end
   subplot((nfram+1)/2,2,p)
   zi=interp2(x,y,MS(:,:,k)',xi,yi,'cubic');
   pcolor(xi,yi,zi);
   text(0,0,'*','FontSize',15);
   % barra de colores
   D = 1:-0.01:0;
   E = [D' D' D'];
   colormap(E);
   %colormap(jet);%mudar o mapa de cores
   caxis(C);
   axis off;
   shading interp;
   set(gca,'ydir','reverse');
   a=num2str(t1,3);
   b=num2str(t2,3);
   c=[a,'-',b,'s'];
   text(xmin,ymin-(ymax-ymin)/8,c,'fontsize',8);
   t1=t2;
end
%grafico total
subplot((nfram+1)/2,2,nfram+1)
hold off
zi=interp2(x,y,MStot',xi,yi,'cubic');
pcolor(xi,yi,zi);
axis manual
set(gca,'tickdir','out')
shading interp
set(gca,'ydir','reverse')
%C=caxis;
xlabel('Direccion de azimut (km)','fontsize',8)
ylabel('Dir. dip','fontsize',8)
text(xmin,ymin-(ymax-ymin)/8,'TOTAL','fontsize',8);
set(gca,'fontsize',8)
axes('position',[-.3 .1 .37 .8],'visible','off')
pcolor(xi,yi,zi);
text(0,0,'*','FontSize',20);
h=pcolor(xi,yi,zi);
h1=gca;
colorbar
h2=colorbar;
format('bank')
set(h,'visible','off')
set(h1,'visible','off')
set(h2,'fontsize',8)
set(h2,'ytick',[min(min(MStot)):(max(max(MStot))-min(min(MStot)))/5:max(max(MStot))])
