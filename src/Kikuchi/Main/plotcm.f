**==============================================**
**     plot correlation & mechanism-diagrams    **
**  Choice:                                     **
**     isr = 0/1/2/3/4 for base elements        **
**     ifp = 0/1       for grid scheme          **
**  Option:                                     **
**     thre=threshold for correlation           **
**    Files:                                    **
**       1 = Observed records                   **
**       3 = Green's function                   **
**       5 = Grid-point , weighting factor etc. **
**============ Ver.030704 ======================**

       PARAMETER (ND0=1024,NM0=6,nk0=11,NL0=20,NJ0=40,RAD=.01745329)
       CHARACTER ID*40,DSN*40,NAM(NJ0)*12,NAM1*12
       INTEGER   NDELY(NL0,NJ0),IX0(NJ0),NBASE(0:4)
     -,          IWK(NM0),IM(NJ0),IB(NJ0),IC(NJ0),ICONT(ND0,NL0)
       REAL  AZ(NJ0),P(NJ0),WT(NJ0),SO(ND0),TS(NL0)
     -,      RWW(NM0,NM0),ARWW(NM0,NM0),RWX(ND0,NM0,NJ0)
     -,      X(ND0,NJ0),Y(2048),W(ND0,NM0,NK0,NJ0)
     -,      RH(NM0),RH0(NM0),RH1(NM0),LH(NM0,NM0),FC(NJ0)
     -,      FR(NL0),FFI(NL0),CONT(ND0,NL0),V(NJ0),rhopt(nm0)
     -,      LH0(NM0,NM0),mom(3,3)
       DATA XPOWER,nbase/0,5,5,2,6,4/

* < from unit 5 >
      OPEN(5,FILE='i_plotcm')
      READ(5,'(A40)') ID
      READ(5,*) TL,TE,V1,dur,kk
C
C      Tl=time-window for waveforms
C      Te=sweeping range in time-domain
C      V1=maximum rupture velocity
C      dur=time-segment
C      kk=layer number
C
      READ(5,*) MS,T1,T2
      READ(5,*) NJS,(FC(J),J=1,NJS)
      READ(5,*) IDLY,ISR,IFP
      READ(5,*) NC,dx,dy,r,thre
         NM=NBASE(ISR)
* < Grid points >
      IF(IFP.EQ.0) THEN
           READ(5,*) NLEN
           READ(5,*) (FR(L),L=1,NLEN)
           READ(5,*) (FFI(L),L=1,NLEN)
           dl=abs(fr(1)-fr(2))
         do 19 l=1,nlen
19         if(fr(l).eq.0.) l0=l
      ELSE
           READ(5,*) SK0,DL,NLEN,L0
           DO 1 L=1,NLEN
           FR(L)=DL*(L-L0)
1          FFI(L)=SK0
      ENDIF

      open(8,file='plot_cm.ps')
      call plots(8)
      JS=1
      READ(1,'(A40)') DSN
      READ(3)DSN, VP,VS,DT,H0,DK,DIP,NK,K0,INTV
      h=h0+dk*(kk-k0)*sin(dip*rad)
      call table(id,h,ms*1.,t1,t2,idly*1.,isr*1.,ifp*1.,h0)
      call number(3.,16.,.3,thre,0.,3)
      call symbol(0.,16.,.3,'threshold=',0.,10)
      NT=TL/DT+0.5
        CALL STIME(SO,NT,DT,MS,T1,T2)
100   READ(1,'(A12)',END=99) NAM(JS)
      READ(1,*) AZ(JS),Dum,Dum,P(JS),Dum,IX0(JS)
      READ(1,*) IM(JS),IB(JS),IC(JS)
      IF(IM(JS).EQ.1) THEN
* < GDSN Instrument: DS converges micron to count >
           READ(1,*) IZP,IZZ,IP
           READ(1,*) DS
           DS=DS*1.E4
         DO 2 K=1,IZP+IZZ
2          READ(1,*)
      ELSE
* < WWSSN Instrument: DS is the magnification >
           READ(1,*) DS
      ENDIF
* < Equalize magnification >
       READ(1,*) XM,INTVL,DT0
      READ(1,*) (Y(I),I=1,INTVL)
      IF(IX0(JS).GT.INTVL) GOTO 100
* Remove record outside the time-window
      READ(3,END=99) NAM1
      do 11 k=1,nk
      do 11 n=1,nm0
11     READ(3) (W(I,N,K,JS),I=1,INTV)
      WT(JS)=1000/DS*FC(JS)
            IX0(JS)=IX0(JS)+IDLY
*
      DO 3 I=1,NT
           X(I,JS)=0.
           I1=IX0(JS)+(I-1)*DT/DT0+1.5
3     IF(I1.GT.0.AND.I1.LE.INTVL)  X(I,JS)=Y(I1)*XM*WT(JS)
            DO 4 N=1,NM
         CALL CONV(W(1,N,kk,JS),SO,Y,INTV,NT,NT)
            DO 4 I=1,NT
4        W(I,N,kk,JS)=Y(I)*WT(JS)*DT
       DO 5 L=1,NLEN
         IF(IB(JS).EQ.1.OR.IB(JS).EQ.4) V(JS)=VP
         IF(IB(JS).EQ.2.OR.IB(JS).EQ.3) V(JS)=VS
         DLY=-FR(L)*P(JS)*COS((AZ(JS)-FFI(L))*RAD)
     -     -DK*(kk-K0)*(SQRT(1/V(JS)**2-P(JS)**2)*SIN(DIP*RAD)
     -     +P(JS)*COS(DIP*RAD)*SIN((AZ(JS)-FFI(L))*RAD))
5        NDELY(L,JS)=NINT(DLY/DT)
       JS=JS+1
           GOTO 100
99     JS=JS-1
*---------------------------------------------------
      DO 10 J=1,JS
        DO 8 N=1,NM
          CALL CORR(W(1,N,kk,J),X(1,J),RWX(1,N,J),NT,NT,NT)
        DO 8 N2=1,NM
            DO 8 I=1,NT
8       RWW(N,N2)=RWW(N,N2)+W(I,N,kk,J)*W(I,N2,kk,J)
      DO 10 I=1,NT
10          XPOWER=XPOWER+X(I,J)**2
* << Inverse matrix ARWW >>
           DO 109 N1=1,NM
           DO 109 N2=1,NM
109   LH(N1,N2)=RWW(N1,N2)
         call ludcmp(lh,nm,nm0,iwk,ddd)
	 do 102 n1=1,nm
	 do 102 n2=1,nm
	 if(n1.eq.n2) arww(n1,n2)=1
102      if(n1.ne.n2) arww(n1,n2)=0
	 do 103 n=1,nm
103      call lubksb(lh,nm,nm0,iwk,arww(1,n))
C
        ndt=dur/dt+.5
        tu=dx/ndt
	optmax=0.
	Iend=te/dt+1.5

	DO 14 L=1,NLEN
*     time > distance/V1
*
       TS(L)=SQRT(FR(L)**2+(kk-K0)*(kk-K0)*DK**2)/V1
	opt=0.
*
       DO 14   I=1,Iend
       ii=mod(i,ndt)
*  < Correlation RH0(n) >
         DO 141 N=1,NM
             RH0(N)=0.
         DO 141 J=1,JS
141          RH0(N)=RH0(N)+RWX(I+NDELY(L,J),N,J)
*  < Inverse >
         DO 142 N=1,NM
             RH(N)=0.
         DO 142 N1=1,NM
            LH(N,N1)=ARWW(N,N1)
            LH0(N,N1)=RWW(N,N1)
142         RH(N)=RH(N)+ARWW(N,N1)*RH0(N1)
           OBJ=0.
        DO 143 N1=1,NM
143        OBJ=OBJ+RH0(N1)*RH(N1)
          DO 144 N=1,NM
144        RH1(N)=RH(N)
*
         IF(ISR.NE.1.AND.ISR.NE.4) GOTO 13
*  < Constraint of double-couple source >
*                RH(n) ==> RH1(n)
            CALL ZMOM(LH,LH0,RH,RH1,NM0,IL)
               IF(IL.NE.0) WRITE(6,*) ' ZMOM UNSTABLE'
            DOBJ=0.
           DO 146 N1=1,NM
           DO 146 N2=1,NM
146         DOBJ=DOBJ+RWW(N1,N2)*(RH1(N1)-RH(N1))*(RH1(N2)-RH(N2))
         OBJ=OBJ-DOBJ
            IF(OBJ.LT.0.) OBJ=0.
13         if((i-1)*dt.lt.ts(l)) obj=0.
         if((i-1)*dt.lt.ts(l)) goto14
	 if(obj.le.opt) goto 15
	  do 131 n=1,nm
131       rhopt(n)=rh1(n)
	   opt=obj
15       continue
	 optmax=max(optmax,opt)
	 if(ii.ne.0) goto 14
C
C  plot mechanism-diagram if the correlation >= thre
C
	   if(opt/xpower.lt.thre) goto 145

	   xx=(i-ndt/2)*tu
	   yy=(l-1)*dy
	   call plot(xx,yy,-3)
	 call mtrx(rhopt,mom)
C          rc=r*opt/XPOWER
           rc=r
           irc=20*rc+1.5
	 call mcplot0(mom,rc,irc)
         call plot(-xx,-yy,-3)
145       opt=0.
14        CONT(I,L)=OBJ/XPOWER
         corrmax=optmax/xpower
	 call plot(0.,-1.,-3)
         CALL AXS(0,Iend*tu,ndt*tu,0.,dur)
            CALL SYMBOL(Iend*tu/2-1.,-1.,.35,'Time',0.,4)
	 call plot(-1.,1.,-3)
         CALL AXS(-1,(NLEN-1)*dy+.01,dy,(1-l0)*dl,dl)
            CALL SYMBOL(-1.,(NLEN-1)*dy/2.-2.,.35,'Strike',90.,6)
            CALL number(-1.,(NLEN-1)*dy/2.,.3,Ffi(1),90.,1)
         CALL SYMBOL(Iend*tu/2-2.,(nlen-1)*dy+1.,.3,'Depth=',0.,6)
         CALL number(Iend*tu/2,(nlen-1)*dy+1.,.3,h,0.,1)
         CALL SYMBOL(Iend*tu/2+1.5,(nlen-1)*dy+1.,.3,'km',0.,2)

        call newpage(8)
      call table(id,h,ms*1.,t1,t2,idly*1.,isr*1.,ifp*1.,h0)
      call number(0.,16.,.3,corrmax,0.,4)
*  < Correlation map >
          call penw(0.3)
         CALL contmap(CONT,0,FMAX,ICONT,ND0,NL0,iend,NLEN,TU,dy,NC)
          call penw(2.0)
            CALL PLOT(TS(1)/DT*TU,dy*(-.5),3)
            CALL PLOT(TS(1)/DT*TU,dy*( .5),2)
          DO 18 L=2,NLEN
            CALL PLOT(TS(L)/DT*TU,dy*(L-1.5),2)
18          CALL PLOT(TS(L)/DT*TU,dy*(L-0.5),2)
	 call plot(0.,-1.,-3)
          call penw(1.0)
         CALL AXS(0,TU*Iend,ndt*tu,0.,dur)
            CALL SYMBOL(TU*Iend/2-1.,-1.,.35,'Time',0.,4)
	 call plot(-1.,1.,-3)
         CALL AXS(-1,(NLEN-1)*dy+.01,dy,(1-l0)*dl,dl)
            CALL SYMBOL(-1.,(NLEN-1)*dy/2.-2.,.35,'Strike',90.,6)
            CALL number(-1.,(NLEN-1)*dy/2.,.3,Ffi(1),90.,1)
         CALL SYMBOL(Iend*tu/2-2.,(nlen-1)*dy+1.,.3,'Depth=',0.,6)
         CALL number(Iend*tu/2,(nlen-1)*dy+1.,.3,h,0.,1)
         CALL SYMBOL(Iend*tu/2+1.5,(nlen-1)*dy+1.,.3,'km',0.,2)
       write(8,*) 'showpage'
      close(8)
      END

      subroutine table(id,h,pt,t1,t2,shift,type,grid,h0)
       CHARACTER ID*40
        call plot(3.,7.,-3)
        CALL SYMBOL(0.,20.,.45,ID,0.,40)
        CALL SYMBOL(0.,19.,.3,'h=',0.,2)
        CALL NUMBER(1.,19.,.3,h,0.,1)
        call number(3.,19.,.3,pt,0.,-1)
        call number(4.5,19.,.3,t1,0.,1)
        call number(6.,19.,.3,t2,0.,1)
        call symbol(0.,18.,.3,'h0',0.,2)
        call number(0.,17.,.3,h0,0.,1)
        call symbol(2.5,18.,.3,'delay',0.,5)
        call number(3.,17.,.3,shift,0.,-1)
        call symbol(4.,18.,.3,'type',0.,4)
        call number(4.5,17.,.3,type,0.,-1)
        call symbol(5.5,18.,.3,'grid',0.,4)
        call number(6.,17.,.3,grid,0.,-1)
        call plot(0.,5.,-3)
	end
