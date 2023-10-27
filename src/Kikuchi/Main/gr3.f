CC===================================================
CC  "gr3" displays
CC    Contour map of the distribution of moment-release
CC       (strike,dip)-plane 
CC    Source-time function and mechanism diagram.
CC
CC  Input from stdin (fort.5)
CC     spcm,norm,height,line,Nc,Amu:(f,i,f,i,i,f)
CC        spcm : tip-mark (sec per cm)
CC        norm : of the waveform amplitude
CC        Nc: number of contour lines
CC        Amu: rigidity [10GPa]
CC     npl,nps,fill: (3i)
CC        draw nodal-plane (if npl=1),
CC        plot stations (if nps=1),
CC        fill push-domain of mechanism diagram (if fill=1)
CC     narw,amp0,tip
CC        draw slip vector (if narw=1)
CC        amp0: maximum length [cm] of the slip vector
CC        tip: maximum length [cm] of the arrow
CC
CC============================ Ver.030705 ===============
       PARAMETER (ND0=4096,NL0=31,NJ0=120,nk0=31,NE0=1000,RAD=.01745329)
       CHARACTER ID*40,NAM(NJ0)*12,phas(4)*2,comp(3)*2,chrc*55
       INTEGER  IM(NJ0),IB(NJ0),IC(NJ0),Icont(nl0,nk0)
       REAL  MO(NE0),SO(ND0),amp(4),Ts(NE0),xs(NE0),ys(NE0)
     -,      STM(ND0),X(ND0,NJ0),Y(ND0,NJ0),AZ(NJ0),PV(NJ0)
     -,   CONT(nl0,nk0),vec(6),mij(3,3),sl(ne0)
     -,   slx(nl0,nk0),sly(nl0,nk0),rake(nl0,nk0)
       data amp/0,0,0,0/
       data comp/'UD','NS','EW'/
       data phas/'P ','SV','SH','PP'/
      OPEN(5,FILE='i_gr3')
      read(5,*) spcm,norm,height,line,Nc,Amu
      read(5,*) npl,nps,fill
      read(5,*) narw,amp0,tip
* < from unit 15 >
**==================================================**
        read(24) id
        READ(24) JS,NT,DT,NE,ms,T1,T2,str,dip,slip,h0
        READ(24) nx,nx0,dx,ny,ny0,dy
        Fc=100./(Amu*dx*dy)
       call stime(so,nt,dt,ms,t1,t2)
*-------------------------------------------
      DO 11 J=1,JS
        READ(24) NAM(J),IM(J),IB(J),IC(J)
        READ(24) AZ(J),PV(J),NT
        READ(24) (X(I,J),I=1,NT)
	 amp1=x(1,j)
	 amp2=x(1,j)
       do 1 i=1,nt
        amp1=max(amp1,x(i,j))
1       amp2=min(amp2,x(i,j))
11        amp(ib(j))=max(amp1-amp2,amp(ib(j)))
       smom1=0.
       smom2=0.
       Amo=0.
       amslp=0.
      DO 2 i=1,NE
      READ(24) Ts(i),xs(i),ys(i),mo(i),sl(i)
       l=xs(i)/dx+nx0+.5
       k=ys(i)/dy+ny0+.5
       slx(l,k)=slx(l,k)+mo(i)*cos(sl(i)*rad)
       sly(l,k)=sly(l,k)+mo(i)*sin(sl(i)*rad)
       smom1=smom1+mo(i)*cos(sl(i)*rad)
       smom2=smom2+mo(i)*sin(sl(i)*rad)
2      Amo=max(amo,mo(i))
       rake0=atan2(smom2,smom1)/rad
        Tmom=sqrt(smom1**2+smom2**2)
       do 3 l=1,nx
       do 3 k=1,ny
       slp=sqrt(slx(l,k)**2+sly(l,k)**2)
3      amslp=max(amslp,slp)
      do 22 j=1,js
22       READ(24) (Y(i,J),i=1,NT)
        READ(24) EROR
      DO 10 i=1,NE
10       GMO=MAX(GMO,ABS(MO(I)))
         XL=NT*DT/spcm
	 open(8,fiLe='plot3.ps')
	 call plots(8)
      CALL PLOT(3.,23.,-3)
      CALL SYMBOL(0.,4.,.45,ID,0.,40)
         Amw=(log10(Tmom)+18-9.1)/1.5
         write(chrc,'(a,g12.3,a,f7.2)')
     -      'Mo =',Tmom*1.e18,' Nm   Mw =',Amw
          call symbol(0.,3.0,.3,chrc,0.,35)
         write(chrc,'(a,f6.1,a,f6.2)')
     -     'H =',H0,' km     var =',eror
c    -     'H =',H0,' km  slip =',Fmax,' s   var =',eror
          call symbol(0.,2.0,.3,chrc,0.,55)
C
C < Source time function >
C
21        DO 30  I=1,NT
	  stm(i)=0.
        DO 30 II=1,NE
           I1=I-Ts(II)/DT-.5
30      IF(I1.GT.0.AND.I1.LT.NT) STM(I)=STM(I)+SO(I1)*MO(II)
          CALL PLOT(0.,-2.0,-3)
          CALL AXS(0,XL,1.,0.,spcm)
         call penw(1.5)
        CALL PLOTW(STM,NT,DT/spcm,smax,1,2,2.0)
         call penw(1.0)
         call plot(0.,0.,3)
         call plot(xl,0.,2)
C
C < Fault mechanism >
C
        CALL PLOT(2.,-3.,-3)
          RM=1.5
           CALL CIRC1(0.,0.,RM)
           CALL PLOT(0.,RM,3)
           CALL PLOT(0.,RM-.2,2)
         write(chrc,'(a,f4.0,a,f3.0,a,f5.0,a)')
     -      '(',str,',',dip,',',rake0,')'
          call symbol(-1.,-2.5,.25,chrc,0.,16)
C
C Nodal plane
C
	    if(npl.ne.0)
     -       CALL MCPLOT(STr,dip,rake0,RM,0)
C
C Station
C
	    if(nps.eq.0) goto 36
          DO 32 J=1,JS
32         CALL STNPLO(AZ(J)*RAD,PV(J),RM)
C
C Fill quadrants
C
36       if(fill.eq.0) goto 38
	   call dbas(str,dip,rake0,1.,vec)
	   call mtrx(vec,mij)
	    ndens=max(rm*10,1.)
	    call mcplot0(mij,rm,ndens)
C
C  < Spatial distribution of subevents >
C
38      CALL PLOT(-3.,-4.,-3)
       aspct=dy/dx
      if(aspct.lt.0.5) aspct=0.5
      if(ny.gt.1) then 
        CALL AXS(1,(Ny-1)*aspct,aspct,(1-ny0)*Dy,Dy)
        CALL SYMBOL(-1.,-(ny-1)*aspct/2+1.,.3,'dip,km',-90.,6)
      endif
        CALL PLOT(1.0,(1-ny)*aspct-1.0,-3)
      if(nx.gt.1) then
        CALL AXS(0,Nx-1.,1.,(1-nx0)*Dx,Dx)
        CALL SYMBOL(Nx*.5-1.,-1.,.3,'strike,km',0.,9)
      endif
        CALL PLOT(nx0-1.0,1.0+(ny-ny0)*aspct,-3)
       write(46,'(i5)') (nx+2)*(ny+2)
       write(46,'(2f9.3,i5)') str,dip
       do 402 l=1,nx
       do 402 k=1,ny
       rake(l+1,ny-k+2)=atan2(sly(l,k),slx(l,k))/rad
402    cont(l+1,ny-k+2)=sqrt(slx(l,k)**2+sly(l,k)**2)*fc
        Fm=0.
        do 404 l=1,nx+2
        do 404 k=1,ny+2
        write(46,'(5f8.2)') (l-nx0-1)*dx,(k+ny0-2-ny)*dy,
     -   cont(l,k),rake(l,k),0.
404     Fm=max(Fm,cont(l,k))
        call plot(-nx0+0.,-(ny-ny0+1)*aspct,-3)
          call penw(0.3)
         CALL contmap(CONT,0,FMAX,ICONT,NL0,nk0,nx+2,ny+2,1.,aspct,NC)
        call plot(nx0+0.,(ny-ny0+1)*aspct,-3)
          call penw(1.2)
         if(narw.ne.0) then
C
C Slip vector
C
        do 103 l=1,nx
          x1=l-nx0
        do 103 k=1,ny
          y1=-(k-ny0)*aspct
         if(slx(l,k).eq.0..and.sly(l,k).eq.0.) goto 103
          slp=sqrt(slx(l,k)**2+sly(l,k)**2)/amslp*amp0
          sla=atan2(sly(l,k),slx(l,k))/rad
          call arrow(x1,y1,slp,sla,tip)
103      continue
C
        endif
        call plot(-.1,0.,3)
        call plot(.1,0.,2)
        call plot(0.,.1,3)
        call plot(0.,-.1,2)
      print *, 'Maximum dislocation [m] = ',Fmax
      call newpage(8,ipage)
C
C < Obs. & Syn. >
C
      CALL PLOT(3.,25.,-3)
      CALL SYMBOL(0.,2.,.45,ID,0.,40)
          call number(0.,1.0,.3,eror,0.,4)
      xshift=0.
           CALL AXS(0,XL,1.,0.,spcm)
           CALL PLOT(0.,-2.,-3)
      DO 40 J=1,JS
         CALL SYMBOL(-2.3,0.,.25,NAM(J),.0,12)
	 call symbol(-1.1,-0.5,.25,phas(ib(j)),0.,2)
         if(ib(j).ne.3) call symbol(-.4,0.5,.20,comp(ic(j)),0.,2)
              if(az(j).lt.0.) az(j)=az(j)+360.
              CALL NUMBER(-1.7,-1.1,.25,az(j),.0,1)
	 if(norm.lt.2) then
           call penw(1.5)
            CALL PLOTW0(X(1,J),nt,DT/spcm,AS1,1,line,height)
            CALL NUMBER(-1.7,0.5,.20,AS1,.0,2)
            CALL PLOT(0.,-.7,-3)
           call penw(0.8)
              IF(NORM.EQ.1) THEN
                CALL PLOTW0(Y(1,J),nt,DT/spcm,AS2,1,line,height)
                CALL NUMBER(-1.7,.0,.20,AS2/AS1,.0,2)
              ELSE
                CALL PLOTW0(Y(1,J),nt,DT/spcm,AS1,0,line,height)
              ENDIF
	  else
           call penw(1.5)
        	call plotw0(x(1,j),nt,dt/spcm,amp(ib(j)),0,line,height)
		  xm1=x(1,j)
		  xm2=x(1,j)
               do 41 i=2,nt
		  xm1=max(xm1,x(i,j))
41                xm2=min(xm2,x(i,j))
		  xmax=xm1-xm2
		call number(-1.7,.5,.22,xmax,.0,2)
		  call plot(0.,-.7,-3)
           call penw(0.8)
		call plotw0(y(1,j),nt,dt/spcm,amp(ib(j)),0,line,height)
         endif
         CALL PLOT(0.,-1.5,-3)
      IF(MOD(J,8).NE.0.) GOTO 40
      IF(J.EQ.JS) GOTO 40
       xshift=xshift+xl+3.
       if(xshift+xl.gt.20.) then
         call newpage(8,ipage)
         xshift=0.
      CALL PLOT(3.,25.,-3)
      CALL SYMBOL(0.,2.,.45,ID,0.,40)
          call number(0.,1.0,.3,eror,0.,4)
        else
          call plot(xl+3.,19.6,-3)
        endif
           CALL PLOT(0.,-2.,-3)
40    CONTINUE
       call plote(8)
       close(8)
       END
C
        subroutine arrow(x,y,alen,angle,tip)
*   plots arrow
       atip=tip*28.1
       atip1=atip*.7
        scl=28.2
       call Ccirc1(x,y,.07)
       call plot(x,y,3)
       write(8,'(f7.1,a)') angle, '  rotate'
       write(8,'(2f8.1,a)') alen*scl,0.,' rL'
       write(8,'(2f8.1,a)') -atip,atip1,' rM'
       write(8,'(2f8.1,a)') atip,-atip1,' rL'
       write(8,'(2f8.1,a)') -atip,-atip1,' rL stroke'
       write(8,'(f7.1,a)') -angle, '  rotate'
       end
