        subroutine mcplo(mom,ic)
        character*1 map(33,18),ic
       real mom(3,3)
	rad=.01745329
       do 2 ix=1,33
       do 2 iy=1,18
2      map(ix,iy)=' '
*  Conversion from an equal area to (azimuth,take-off)
       do 1 ix=1,33
       do 1 iy=1,18
         x=(ix-17)/16.0
         y=(iy-9.5)/9.0
	  r=sqrt(x**2+y**2)
       if(r.gt.1.0) goto 1
          map(ix,iy)='-'
	  phi=atan2(x,y)/rad
	  th=2*asin(r/1.41421356)/rad
        call radpmt(mom,phi,th,pw,svw,shw)
	if(pw.le.0.) goto 1
          map(ix,iy)=ic
1      continue
        do 3 iy=18,1,-1
3       write(6,10) (map(ix,iy),ix=1,33)
10      format('   ',33a1)
	end
