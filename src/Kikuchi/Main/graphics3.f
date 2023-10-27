**===================================================**
**  Graphics for a fixed mechanism inversion         **
**    1.Correlation map                              **
**    2.Solution (Spatio-temporal distribution)      **
**    3.Obs. & Syn.                                  **
**  File:                                            **
**    34 = Waveforms                                 **
**============================ Ver.030712 ===========**
       PARAMETER (ND0=1024,NM=5,NL0=21,NJ0=40,nk0=11,NE0=50
     -            ,RAD=.01745329)
       CHARACTER ID*40,NAM(NJ0)*12,phas(4)*2,comp(3)*2,chrc*31
       INTEGER  IM(NJ0),IB(NJ0),IC(NJ0)
       REAL  MO(NE0),SO(ND0),amp(4)
     -,   TA(NE0),xax(NE0),YAX(NE0),STIME(ND0),vec(6),mij(3,3)
     -,   X(ND0,NJ0),Y(ND0,NJ0),AZ(NJ0),PV(NJ0),Mw
       data amp/0,0,0,0/
       data comp/'UD','NS','EW'/
       data phas/'P ','SV','SH','PP'/
      OPEN(5,FILE='i_graphics3')
      read(5,*) spcm,norm,height,line
      read(5,*) npl,nps,fill
* < from unit 15 >
        READ(34) ID
        READ(34) JS,NT,nte,DT,NE,ms,T1,T2
     -           ,stk,dip,slip,h0,dk,nk,k0,dl,nl,l0
        READ(34) (SO(I),I=1,NT)
      DO 11 J=1,JS
        READ(34) NAM(J),IM(J),IB(J),IC(J)
        READ(34) AZ(J),PV(J),NT
        READ(34) (X(I,J),I=1,NT)
	 amp1=x(1,j)
	 amp2=x(1,j)
       do 1 i=1,nt
        amp1=max(amp1,x(i,j))
1       amp2=min(amp2,x(i,j))
11        amp(ib(j))=max(amp1-amp2,amp(ib(j)))
       Tmom=0.
      DO 2 i=1,NE
      READ(34) NS,TA(i),xax(i),YAX(i),MO(i)
2     Tmom=Tmom+Mo(i)
        READ(34) EROR
      do 22 j=1,js
22       READ(34) (Y(i,J),i=1,NT)
      DO 10 i=1,NE
10       GMO=MAX(GMO,ABS(MO(I)))
      DO 20 J=1,JS
      DO 20 I=1,NT
20       Y(I,J)=X(I,J)-Y(I,J)
         XL=NT*DT/spcm
         XL1=(NTe*DT+t1+t2)/spcm
	 tu=dt*dx
	 open(8,fiLe='plot38.ps')
	 call plots(8)
        CALL INITG(ID,T1,T2,EROR)
* < Source time function >
21        DO 30  I=1,NT
	  stime(i)=0.
        DO 30 II=1,NE
           I1=I-TA(II)/DT-.5
30      IF(I1.GT.0.AND.I1.LT.NT) STIME(I)=STIME(I)+SO(I1)*MO(II)
          nte1=nte+(t1+t2)/dt+.5
          CALL PLOT(0.,-3.0,-3)
          CALL AXS(0,XL1,1.,0.,spcm)
            CALL SYMBOL(xl1/2-1.,-.9,.3,'Time,sec',0.,8)
         call penw(1.5)
        CALL PLOTW(STIME,NTe1,DT/spcm,smax,1,2,2.0)
         Mw = (log10(Tmom)+18-9.1)/1.5
         write(chrc,'(a,e12.3,a,f7.2)') 'Mo[Nm]=',Tmom*1.e18,'  Mw=', Mw
         call symbol(0.,3.3,.3,chrc,0.,31)
         call penw(1.0)

* < Fault mechanism >
        CALL PLOT(2.,-5.,-3)
          RM=1.5
           CALL CIRC1(0.,0.,RM)
           CALL PLOT(0.,RM,3)
           CALL PLOT(0.,RM-.2,2)

* Nodal plane
	    if(npl.ne.0)
     -       CALL MCPLOT(STK,dip,slip,RM,0)
* Station
	    if(nps.eq.0) goto 36
          DO 32 J=1,JS
32         CALL STNPLO(AZ(J)*RAD,PV(J),RM)

* Fill quadrants
36       if(fill.eq.0) goto 38
	   call dbas(stk,dip,slip,1.,vec)
	   call mtrx(vec,mij)
	    ndens=max(rm*10,1.)
	    call mcplot0(mij,rm,ndens)

*  < Spatial distribution of subevents >
38      CALL PLOT(-2.,-3.,-3)
      CALL PLOT(-1.0,0.,-3)
       aspct=dk/dl
      if(aspct.lt.0.5) aspct=0.5
      if(nk.gt.1) then 
        CALL AXS(1,(NK-1)*aspct,aspct,(1-K0)*DK,DK)
        CALL SYMBOL(-1.,-(nk-1)*aspct/2+1.,.3,'dip,km',-90.,6)
      endif
        CALL PLOT(1.0,(1-nk)*aspct-1.0,-3)
      if(nl.gt.1) then
        CALL AXS(0,NL-1.,1.,(1-L0)*DL,DL)
        CALL SYMBOL(NL*.5-1.,-1.,.3,'strike,km',0.,9)
      endif
        CALL PLOT(L0-1.0,1.0+(nk-k0)*aspct,-3)
      ANORM=.5/SQRT(MO(1))
      DO 402 I=1,NE
        if(dl.ne.0.) xx=xax(I)/DL
        if(dk.ne.0.) yy=-YAX(I)/DK*aspct
      rr=sqrt(abs(mo(i)))*anorm
402   CALL CIRC1(xx,yy,rr)
      call newpage(8)
* < Obs. & Syn. >
        CALL INITG(ID,T1,T2,EROR)
      xshift=0.
          CALL PLOT(0.,-1.,-3)
           CALL AXS(0,XL,1.,0.,spcm)
           CALL PLOT(0.,-2.,-3)
      DO 40 J=1,JS
         CALL SYMBOL(-2.3,0.,.25,NAM(J),.0,12)
	 call symbol(-1.1,-.4,.24,phas(ib(j)),0.,2)
         if(ib(j).ne.3) call symbol(-.4,-.4,.20,comp(ic(j)),0.,2)
              if(az(j).lt.0.) az(j)=az(j)+360.
              CALL NUMBER(-1.7,-.8,.25,az(j),.0,1)
	 if(norm.lt.2) then
           call penw(1.5)
            CALL PLOTW0(X(1,J),nt,DT/spcm,AS1,1,line,height)
            CALL NUMBER(-1.7,.5,.25,AS1,.0,2)
            CALL PLOT(0.,-.7,-3)
           call penw(0.8)
              IF(NORM.EQ.1) THEN
                CALL PLOTW0(Y(1,J),nt,DT/spcm,AS2,1,line,height)
                CALL NUMBER(-1.7,-.5,.22,AS2/AS1,.0,2)
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
        CALL INITG(ID,T1,T2,EROR)
        else
          call plot(xl+3.,20.6,-3)
        endif
          CALL PLOT(0.,-1.,-3)
           CALL PLOT(0.,-2.,-3)
40    CONTINUE
       call plote(8)
       close(8)
       END
