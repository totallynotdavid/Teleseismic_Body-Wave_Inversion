**============================ Ver.030630 ===========**
**  Output of multiple event analysis                **
**    1.Solution (Spatio-temporal distribution)      **
**    2.Obs. & Syn. wavefoirms                       **
**  File:                                            **
**     4 = Waveforms                                 **
**  Output options :                                 **
**      norm=0  scale is common for obs. & syn.      **
**           1  each waveform is normalized          **
**           2  scale is common for body wave types  **
**      line=0  draw time-axis, otherwise not        **
**      npl >0  draw nodal plane                     **
**          >1      with T-axis                      **
**      nps >0  plot stations                        **
**      fill>0  fill quadrants                       **
**===================================================**
       PARAMETER (ND0=1024,NL0=20,NJ0=40,NE0=50,RAD=.01745329)
       CHARACTER ID*40,NAM(NJ0)*12,phas(4)*2,chrc*31
       INTEGER  NODR(NE0),IB(NJ0),fill
       REAL  MO(NE0),STK(NE0),DP(NE0),SL(NE0),SO(ND0),STIME(ND0,Ne0)
     -,   TA(NE0),RA(NE0),FF(NE0),YAX(NE0),TAXS(3,NE0),FR(NL0),FFI(NL0)
     -,      X(ND0,NJ0),Y(ND0,NJ0),AZ(NJ0),PV(NJ0),amp(4),mxy(3,3)
     -,      mxy0(ne0,3,3),Mw
	data amp/0.,0.,0.,0./
	data phas/'P ','SV','SH','PP'/
C         data comp/'UD','NS','EW'/
* < Scale & Normalization >
      READ(5,*) spcm,norm,height,line,hor
      READ(5,*) npl,nps,fill
       open(8,file='plot8')
        call plots(8,ipage)
* < from unit 4 >
        READ(4) ID
        READ(4) JS,NT,DT,ITE,T1,T2,MS
        READ(4) (SO(I),I=1,NT)
        READ(4) NLEN
        READ(4) (FR(L),FFI(L),L=1,NLEN)
        ITE1=ITE+1
      DO 11 J=1,JS
        READ(4) NAM(J),Idum,IB(J),Idum
        READ(4) AZ(J),PV(J)
        READ(4) (X(I,J),I=1,NT)
	 amp1=x(1,j)
	 amp2=x(1,j)
	do 1 i=2,nt
	 amp1=max(amp1,x(i,j))
1        amp2=min(amp2,x(i,j))
11      amp(ib(j))=max(amp(ib(j)),amp1-amp2)
      DO 2 I=1,ITE
      READ(4) NS,TA(I),RA(I),FF(I),YAX(I),MO(I),Dum,STK(I),DP(I),SL(I)
        READ(4) mxy
	   do 21 n=1,3
	   do 21 m=1,3
21      mxy0(i,m,n)=mxy(m,n)
2       READ(4) (TAXS(K,I),K=1,3)
        READ(4) EROR
       print *, eror
       do 23 j=1,js
23     READ(4) (Y(I,J),I=1,NT)
      READ(4) MO(ITE1),Dum,STK(ITE1),DP(ITE1),SL(ITE1)
        Tmom=Mo(ite1)
        READ(4) mxy
	   do 22 n=1,3
	   do 22 m=1,3
22      mxy0(ite1,m,n)=mxy(m,n)
        READ(4) (TAXS(K,ITE1),K=1,3)
      DO 20 J=1,JS
      DO 20 I=1,NT
20       Y(I,J)=X(I,J)-Y(I,J)
         XL=NT*DT/spcm
*
*=== Solution ===
       CALL head(ID,ms,T1,T2,EROR)
* < Source time function >
        CALL PLOT(0.,-4.,-3)
          CALL AXS(0,XL,1.0,0.,spcm)
        call penw(1.5)
         smax=0.
      DO 10 I=1,ITE
10       smax=MAX(smax,ABS(MO(I)))
          AS=0
          DO 302 I=1,NT
302    AS=MAX(SO(I),AS)
	  gmo=smax*as
        DO 30 II=1,ITE
         DO 301 I=1,NT
          STIME(I,ii)=0
          I1=I-TA(II)/DT+.5
301        IF(I1.GT.0.AND.I1.LT.NT) STIME(I,ii)=SO(I1)*MO(II)
30      CALL PLOTW(STIME(1,ii),NT,DT/spcm,GMO,0,2,2.0)
         Mw = (log10(Tmom)+18-9.1)/1.5
         write(chrc,'(a,e12.3,a,f7.2)') 'Mo[Nm]=',Tmom*1.e18,'  Mw=', Mw
         call symbol(0.,3.3,.3,chrc,0.,31)
*  < Spatial distribution >
       if(nlen.eq.1) goto 39
         CALL PLOT(2.,-5.,-3)
         FACT=hor/20.
        DO 31 L=1,NLEN
         XX=FR(L)*SIN(FFI(L)*RAD)*FACT
         YY=FR(L)*COS(FFI(L)*RAD)*FACT
        if(l.ne.1) call plot(xx,yy,2)
           call ccirc1(xx,yy,0.2)
31         call plot(xx,yy,3)
          call penw(1.0)
          CALL cline(-.75,0.,.75,0.)
          CALL cline(0.,-.75,0.,1.5)
         CALL SYMBOL(-.1,1.7,.2,"N",0.,1)
        CALL PLOT(-2.,0.,-3)
* < Fault mechanism >
39      CALL PLOT(0.,-5.,-3)
          call penw(1.0)
          RM=1.5
       CALL ORDER(TA,ITE,NODR)
          DO 33 M=1,ITE1
           IF(M.EQ.ITE1) THEN
               if(ite1.eq.2) goto 33
                  NS=ITE1
                  RM=1.5
           CALL PLOT(-RM*2-.5,-RM*2-1.5,-3)
           ELSE
                 NS=NODR(M)
                 RM=SQRT(MO(NS)/smax)*1.5
           ENDIF
           CALL CIRC1(0.,0.,RM)
           CALL PLOT(0.,RM,3)
           CALL PLOT(0.,RM-.2,2)

* Nodal plane
          if(npl.ne.0)
     -       CALL MCPLOT(STK(NS),DP(NS),SL(NS),RM,0)
* T-axis
	  if(npl.lt.2)  goto 35
           TH=ACOS(TAXS(3,NS))
           FI=ATAN2(TAXS(2,NS),TAXS(1,NS))
             IF(TH/RAD.GT.90.) THEN
               TH=3.141593-TH
               FI=FI+3.141593
           END IF
             CALL STNPLX(FI,SIN(TH),RM)
* Station
35        if(nps.eq.0) goto 37
             DO 32 J=1,JS
32           CALL STNPLO(AZ(J)*RAD,PV(J),RM)

* Fill quadrants
37       if(fill.eq.0) goto 38
	    do 36 n1=1,3
	    do 36 n2=1,3
36                 mxy(n1,n2)=mxy0(ns,n1,n2)
	     ndens=max(rm*10,1.)
	    call mcplot0(mxy,rm,ndens)
38        IF(M.EQ.ITE1) then
	    call symbol(-.5,2.0,.3,'Total',0.,5)
	    GOTO 33
	  endif
           CALL NUMBER(-1.,2.3,.2,TA(NS),0.,1)
           CALL NUMBER( .2,2.3,.2,YAX(NS),0.,1)
           CALL NUMBER(-1.,1.8,.2,RA(NS),0.,1)
           CALL NUMBER( .1,1.8,.2,FF(NS),0.,1)
           CALL NUMBER(-1.5,1.8,.2,NS*1.,0.,-1)
           CALL PLOT(3.4,0.,-3)
         IF(MOD(M,5).NE.0) GOTO 33
           CALL PLOT(-13.6,-3.,-3)
33       CONTINUE
* < Obs. & Syn. >
        call newpage(8,ipage)
       xshift=0.
       CALL head(ID,ms,T1,T2,EROR)
        CALL PLOT(0.,-1.,-3)
           CALL AXS(0,XL,1.,0.,spcm)
           CALL PLOT(0.,-2.,-3)
      DO 40 J=1,JS
         CALL SYMBOL(-2.4,0.,.25,NAM(J),.0,12)
         call symbol(-1.2,-.4,.24,phas(ib(j)),0.,2)
              if(az(j).lt.0.) az(j)=az(j)+360.
              CALL NUMBER(-1.7,-.8,.25,az(j),.0,1)
	if(norm.lt.2) then
        call penw(1.5)
         CALL PLOTW0(X(1,J),NT,DT/spcm,AS1,1,line,height)
         CALL NUMBER(-1.7,.5,.22,AS1,.0,2)
         CALL PLOT(0.,-0.7,-3)
        call penw(0.8)
           IF(NORM.EQ.1) THEN
              CALL PLOTW0(Y(1,J),NT,DT/spcm,AS2,1,line,height)
              CALL NUMBER(-1.7,-.5,.22,AS2/AS1,.0,2)
           ELSE
              CALL PLOTW0(Y(1,J),NT,DT/spcm,AS1,0,line,height)
           ENDIF
         else
        call penw(1.5)
	   call plotw0(x(1,j),nt,dt/spcm,amp(ib(j)),0,line,height)
	     xm1=x(1,j)
	     xm2=x(1,j)
	    do 41 i=2,nt
	     xm1=max(xm1,x(i,j))
41           xm2=min(xm2,x(i,j))
            xmax=xm1-xm2
              CALL NUMBER(-1.7,.5,.22,xmax,.0,2)
              CALL PLOT(0.,-.7,-3)
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
        CALL head(ID,ms,T1,T2,EROR)
       else
         call plot(xl+3.,20.6,-3)
       endif
         CALL PLOT(0.,-3.,-3)
40    CONTINUE
      call plote(8)
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
