        subroutine radpfc(f,phi,th,pw,svw,shw)
*  f=forcw (3)
*  phi=azimuth(degree)
*  th =take-off angle(degree)
*  pw,svw,shw =P,SV,SH-waves
	real f(3),dirp(3),dirsv(3),dirsh(3)
	rad=.01745329
* definition of vectors:
	sth=sin(th*rad)
	cth=cos(th*rad)
	cphi=cos(phi*rad)
	sphi=sin(phi*rad)
	dirp(1)=sth*cphi
	dirp(2)=sth*sphi
	dirp(3)=cth
	dirsv(1)=cth*cphi
	dirsv(2)=cth*sphi
	dirsv(3)=-sth
	dirsh(1)=-sphi
	dirsh(2)=cphi
	dirsh(3)=0
* radiation pattern:
	pw=0
	svw=0
	shw=0
	do 1 n=1,3
	  pw=pw+f(n)*dirp(n)
	  svw=svw+f(n)*dirsv(n)
1         shw=shw+f(n)*dirsh(n)
        end
