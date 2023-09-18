        subroutine radpmt(mom,phi,th,pw,svw,shw)
*  mom=moment-tensor (3x3)
*  phi=azimuth(degree)
*  th =take-off angle(degree)
*  pw,svw,shw =P,SV,SH-waves
	real mom(3,3),dirp(3),dirsv(3),dirsh(3)
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
	do 1 m=1,3
	do 1 n=1,3
	  pw=pw+mom(m,n)*dirp(m)*dirp(n)
	  svw=svw+mom(m,n)*dirp(m)*dirsv(n)
1         shw=shw+mom(m,n)*dirp(m)*dirsh(n)
        end
