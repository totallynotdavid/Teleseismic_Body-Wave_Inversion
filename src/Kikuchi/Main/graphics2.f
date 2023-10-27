**===================================================**
**  Output of multiple event analysis                **
**    1.Solution (Temporal distribution)             **
**    2.Obs. & Syn.                                  **
**   option fill   see graphics                      **
**     plot mechanism on "stdout"                    **
**  File:                                            **
**   24 = Waveforms                                  **
**============================ Ver.030704 ===========**
       PARAMETER (ND0=1024,NJ0=50,NE0=100,RAD=.01745329)
       CHARACTER ID*40,NAM(50)*12,phas(4)*2,chrc*31
       INTEGER  IM(nj0),IB(nj0),IC(nj0),fill,gr,ms(ne0)
       REAL  MO(NE0),STK(NE0),DP(NE0),SL(NE0),SO(ND0)
     -,  TA(NE0),H(NE0),FR(NE0),FFI(NE0),STM(ND0,50),mij0(ne0,3,3)
     -,   X(ND0,NJ0),Y(ND0,NJ0),AZ(NJ0),PV(NJ0),amp(4),mij(3,3)
     -,   smom(50),gmo(50),t1(ne0),t2(ne0),Mw
	data phas/'P ','SV','SH','PP'/
        open(8,file='plot28')
	CALL PLOTS(8)
      CALL PLOT(3.,27.,-3)
	READ(5,*) spcm,NORM,height,line,hor
	read(5,*) npl,nps,fill
         FACT=hor/10.
* < from unit 24 >
        READ(24) ID
        READ(24) JS,nt,DT,NE
      DO 11 J=1,JS
        READ(24) NAM(J),IM(J),IB(J),IC(J)
        READ(24) AZ(J),PV(J),IX
        READ(24) (X(I,J),I=1,IX)
	 amp1=x(1,j)
	 amp2=x(1,j)
	do 1 i=1,ix
	 amp1=max(amp1,x(i,j))
1        amp2=min(amp2,x(i,j))
11       amp(ib(j))=max(amp1-amp2,amp(ib(j)))

* < Spatial distribution >
        CALL PLOT(1.,-5.,-3)
          call penw(1.0)
          call plot(.3,1.7,3)
          call plot(-.3,1.7,2)
          call plot(0.,2.2,2)
          call plot(0.,1.,2)
         CALL SYMBOL(-.1,2.5,.2,"N",0.,1)
          CALL cline(-.2,0.,.2,0.)
          CALL cline(0.,-.2,0.,.2)
	  ne1=ne+1
C
       DO 102 I=1,ne1
        if(i.le.ne) then
	 READ(24)
     -  gr,TA(I),FR(gr),FFI(gr),H(gr),STK(gr),DP(gr),SL(gr),MO(I)
     - ,ms(i),t1(i),t2(i)
        call stime(SO,Nt,dt,ms(i),t1(i),t2(i))
           smom(gr)=smom(gr)+mo(i)
         DO 101 ii=1,NT
          I1=ii-TA(I)/DT+.5
101        IF(I1.GT.0.AND.I1.LT.NT)
     -    STM(ii,gr)=STM(ii,gr)+SO(I1)*MO(i)
         if(igr.eq.gr) goto 312
          igr=gr
         XX=FR(gr)*SIN(FFI(gr)*RAD)*FACT
         YY=FR(gr)*COS(FFI(gr)*RAD)*FACT
           call circ1(xx,yy,0.3)
311           CALL NUMBER(xx,yy,.3,gr*1.,0.,-1)
312     continue
	else
          gr=gr+1
	   read(24) Tmom,stk(gr),dp(gr),sl(gr)
	endif
       	   read(24) mij
	  do 102 n=1,3
	  do 102 m=1,3
102     mij0(gr,m,n)=mij(m,n)
C
        READ(24) EROR
      DO 3 J=1,JS
3       READ(24) (Y(I,J),I=1,IX)
*
       xl=nt*dt/spcm
*=== Solution ===
* < Source time function >
      CALL PLOT(-3.,-22.,-3)
      call head(ID,ms,T1,T2,EROR)
        CALL PLOT(0.,-10.,-3)
          CALL AXS(0,XL,1.,0.,spcm)
          call penw(1.5)
	  smax=0.
          gmax=0.
	  do 10 ii=1,gr-1
	  smax=max(smax,abs(smom(ii)))
	  do 9 i=1,nt
9         gmo(ii)=max(gmo(ii),abs(STM(i,ii)))
10        gmax=max(gmax,gmo(ii))
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        do 331 ii=1,gr-1
        do 331 i=1,nt
331     STM(i,gr)=STM(i,gr)+STM(i,ii)
        CALL PLOTW(STM(1,gr),NT,DT/spcm,Gmax,0,2,2.0)
C
         Mw = (log10(Tmom)+18-9.1)/1.5
         write(chrc,'(a,e12.3,a,f7.2)') 'Mo[Nm]=',Tmom*1.e18,'  Mw=', Mw
         call symbol(0.,3.3,.3,chrc,0.,31)
        call penw(0.5)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	 do 31 m=1,gr-1
          ns=m
           ht=abs(gmo(ns)/gmax)*2.+.1
           xx=(ta(ns)+t1(1))/spcm
        CALL PLOTW(STM(1,ns),NT,DT/spcm,Gmax,0,1,2.0)
31      CALL number(xx,ht,.3,m*1.,0.,-1)
* < Fault mechanism >
        call penw(1.)
        CALL PLOT(0.,-5.,-3)
          DO 33 M=1,gr
	  if(m.eq.gr)  then
                rm=1.5
                ns=gr
*          CALL PLOT(-RM*2-.5,-RM*2-1.5,-3)
          else
                ns=m
                RM=SQRT(ABS(smom(ns))/smax)*1.5
          endif
           CALL PLOT(0.,RM,3)
           CALL PLOT(0.,RM-.2,2)

* Nodal plane
	    if(npl.ne.0)
     -       CALL MCPLOT(STK(ns),DP(ns),SL(ns),RM,0)
* Station
	    if(nps.eq.0) goto 36
          DO 32 J=1,JS
32         CALL STNPLO(AZ(J)*RAD,PV(J),RM)

* Fill quadrants
36       if(fill.eq.0) goto 38
	    do 35 n1=1,3
	    do 35 n2=1,3
35        mij(n1,n2)=mij0(ns,n1,n2)
	    ndens=max(rm*10,1.)
	    call mcplot0(mij,rm,ndens)
38       if(m.eq.gr) then
	    call symbol(-.5,1.6,.3,'Total',.0,5)
	      goto 33
*------------------------
	 endif
           CALL NUMBER(-.5,1.6,.3,m*1.,0.,-1)
           CALL PLOT(3.2,0.,-3)
         IF(MOD(M,5).NE.0) GOTO 33
           CALL PLOT(-16.,-4.,-3)
         IF(MOD(M,20).NE.0)GOTO 33
           call newpage(8)
      call head(ID,ms,T1,T2,EROR)
         CALL PLOT(0.,-5.,-3)
33       CONTINUE
         call newpage(8)
* < Obs. & Syn. >
        xshift=0.
      call head(ID,ms,T1,T2,EROR)
          CALL PLOT(0.,-1.,-3)
           CALL AXS(0,XL,1.,0.,spcm)
           CALL PLOT(0.,-2.,-3)
      DO 40 J=1,JS
         CALL SYMBOL(-2.4,0.,.25,NAM(J),.0,12)
	 call symbol(-1.2,-.4,.24,phas(ib(j)),0.,2)
                if(az(j).lt.0.) az(j)=az(j)+360.
                call number(-1.7,-.8,.25,az(j),.0,1)
        call penw(1.5)
	 if(norm.lt.2) then
            CALL PLOTW0(X(1,J),nt,DT/spcm,AS1,1,0,height)
            CALL NUMBER(-1.7,.5,.22,AS1,.0,1)
        call penw(0.8)
            CALL PLOT(0.,-0.7,-3)
              IF(NORM.EQ.1) THEN
                CALL PLOTW0(Y(1,J),nt,DT/spcm,AS2,1,line,height)
                CALL NUMBER(-1.7,-.5,.22,AS2/AS1,.0,2)
              ELSE
                CALL PLOTW0(Y(1,J),nt,DT/spcm,AS1,0,line,height)
              ENDIF
	  else
        	call plotw0(x(1,j),nt,dt/spcm,amp(ib(j)),0,0,height)
		  xm1=x(1,j)
		  xm2=x(1,j)
               do 41 i=2,nt
		  xm1=max(xm1,x(i,j))
41                xm2=min(xm2,x(i,j))
		  xmax=xm1-xm2
 	call number(-1.7,0.5,.22,xmax,.0,1)
        call penw(0.8)
		  call plot(0.,-0.7,-3)
		call plotw0(y(1,j),nt,dt/spcm,amp(ib(j)),0,line,height)
         endif
         CALL PLOT(0.,-1.5,-3)
      IF(MOD(J,8).NE.0.) GOTO 40
      IF(J.EQ.JS) GOTO 40
         xshift=xshift+xl+3.
       if(xshift+xl.gt.20.) then
         call newpage(8)
         xshift=0.
      call head(ID,ms,T1,T2,EROR)
       else
         call plot(xl+3.,20.6,-3)
       endif
          CALL PLOT(0.,-3.,-3)
40    CONTINUE
       write(8,*) 'showpage'
       close(8)
       END
      SUBROUTINE head(ID,ms,T1,T2,EROR)
      CHARACTER ID*40
      CALL PLOT(3.,27.,-3)
      CALL SYMBOL(0.,0.,.45,ID,0.,40)
      CALL NUMBER(0.,-.6,.3,ms*1.,0.,-1)
      CALL NUMBER(1.,-.6,.3,T1,0.,2)
      CALL NUMBER(2.3,-.6,.3,T2,0.,2)
      CALL NUMBER(4.,-.6,.3,EROR,0.,4)
      CALL PLOT(0.,-1.5,-3)
      END
