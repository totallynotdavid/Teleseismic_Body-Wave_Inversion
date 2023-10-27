        subroutine mcplot1(mom,r0,nd,iphas)
       real mom(3,3)
	rad=.01745329
*  Conversion from an equal area to (azimuth,take-off)
       d=r0/nd
       do 1 x=-r0,r0,d
       do 1 y=-r0,r0,d
	  r=sqrt(x**2+y**2)
       if(r.gt.r0) goto 1
	  phi=atan2(x,y)/rad
	  th=2*asin(r/r0/1.41421356)/rad
        call radpmt(mom,phi,th,pw,svw,shw)
	if(iphas.eq.1.and.pw.le.0.) goto 1
	if(iphas.eq.2.and.svw.le.0.) goto 1
	if(iphas.eq.3.and.shw.le.0.) goto 1
          call plus(x,y)
* call cline(x-.01,y,x+.01,y)
* call cline(x,y-.01,x,y+.01)
1      continue
	call circ1(0.,0.,r0)
	end
