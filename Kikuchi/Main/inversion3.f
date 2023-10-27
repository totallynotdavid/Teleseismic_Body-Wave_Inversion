**============================ Ver.030704 ===========**
**   Inversion with a fixed fault plane              **
**   Files:                                          **
**       1 = Observed records                        **
**      33 = Green's function (output of "green3")   **
**      34 = Obs. & Syn. waveforms                   **
**      36 = List of source parameters               **
**       5 = Grid-point , weighting factor etc.      **
**       6 = output of source parameters             **
**===================================================**
       PARAMETER (ND0=1024,NL0=20,NK0=11,NJ0=50,RAD=.01745329)
       CHARACTER ID*40,DSN*40,NAM(NJ0)*12,NAM1*12
       INTEGER   NDELY(NL0,NK0,NJ0)
     -,          IM(NJ0),IB(NJ0),IC(NJ0),ix0(nj0)
       REAL      AZ(NJ0),P(NJ0),WT(NJ0)
     -,      RWW(NK0),RWX(ND0,NK0,NJ0),SO(ND0)
     -,      X(ND0,NJ0),Y(4800),W(ND0,NK0,NJ0)
     -,      FC(NJ0),az2(nj0),del(nj0),g(nj0)
     -,      EROR(0:50),V(NJ0)
       DATA XPOWER,EROR(0)/0,1/
* < from unit 5 >
      OPEN(5,FILE='i_inversion3')
      READ(5,'(A40)') ID
      READ(5,*) TL,V1,TE
      READ(5,*) MS,T1,T2
      READ(5,*) NE
      READ(5,*) DL,NLEN,L0
      READ(5,*) IDLY
      READ(5,*) NJS,(FC(J),J=1,NJS)
      JS=1
      jfc=1
      READ(1,'(A40)') DSN
      READ(33) DSN,VP,VS,INTV,DT,STK,DIP,SLIP,H0,DK,NK,K0
      NT=TL/DT+0.5
      NTE=TE/DT+0.5
       CALL STIME(SO,NT,DT,MS,T1,T2)
100   READ(1,'(A)',END=999) NAM(JS)
      READ(1,*) AZ(JS),AZ2(js),DEL(js),P(JS),G(js),IX0(js)
      READ(1,*) IM(JS),IB(JS),IC(JS)
      IF(IM(JS).EQ.1) THEN
* < GDSN Instrument: DS converges micron to count >
           READ(1,*) IZP,IZZ,IP
           READ(1,*) DS
           DS=DS*1.E4
         DO 1 K=1,IZP+IZZ
1          READ(1,*)
      ELSE
* < WWSSN Instrument: DS is the magnification >
           READ(1,*) DS
      ENDIF
* < Equalize magnification >
       READ(1,*) XM,INTVL,DT0
      READ(1,*) (Y(I),I=1,INTVL)

*  Remove station outside the time-window
      IF(IX0(js).GT.INTVL) GOTO 100
*  Remove station with zero weight
      fc(js)=fc(jfc)
       jfc=jfc+1
      READ(33,END=999) NAM1
      do 11 k=1,nk
      do 11 i=1,intv
11    read(33) W(I,K,JS)
       if(fc(js).eq.0.) goto 100

      WT(JS)=1.e4/DS*FC(JS)
            IX0(js)=IX0(js)+IDLY
*
      DO 2 I=1,NT
           X(I,JS)=0.
           I1=(I+IX0(js))*DT/DT0+0.5
2     IF(I1.GT.0.AND.I1.LE.INTVL)  X(I,JS)=Y(I1)*XM*WT(JS)
            DO 61 K=1,NK
          CALL CONV(W(1,K,JS),SO,Y,INTV,NT,NT)
            DO 61 I=1,NT
61       W(I,K,JS)=Y(I)*WT(JS)*DT
        DO 21 L=1,NLEN
        DO 21 K=1,NK
         IF(IB(JS).EQ.1.OR.IB(JS).EQ.4) V(JS)=VP
         IF(IB(JS).EQ.2.OR.IB(JS).EQ.3) V(JS)=VS
         DLY=-DL*(L-L0)*P(JS)*COS((AZ(JS)-STK)*RAD)
     -     -DK*(K-K0)*(SQRT(1/V(JS)**2-P(JS)**2)*SIN(DIP*RAD)
     -     +P(JS)*COS(DIP*RAD)*SIN((AZ(JS)-STK)*RAD))
21       NDELY(L,K,JS)=NINT(DLY/DT)
       JS=JS+1
           GOTO 100
999   JS=JS-1
*---------------------------------------------------
      DO 5 J=1,JS
        DO 6 K=1,NK
           CALL CORR(W(1,K,J),X(1,J),RWX(1,K,J),NT,NT,NT)
            DO 6 I=1,NT
6             RWW(K)=RWW(K)+W(I,K,J)*W(I,K,J)
        DO 5 I=1,NT
5          XPOWER=XPOWER+X(I,J)**2
*---------------------------------------------------
       WRITE(36,'(A40)') ID
       WRITE(36,'(A40)') DSN
       WRITE(36,201)NT,DT,stk,DIP,slip,h0,NE,idly
201    format('   nt   dt    strike  dip   slip   h0      Ne  Shift'
     - /I5,F6.2,4F7.1,2I5)
       WRITE(36,202)MS,T1,T2
202    format(' Time function :(Type T1 T2)=',I3,2F7.2)
*---------------------------------------------------
       WRITE(34) ID
       WRITE(34)JS,NT,nte,DT,NE,ms,T1,T2,
     -      stk,dip,slip,h0,dk,nk,k0,dl,nlen,l0
       WRITE(34) (SO(I),I=1,NT)
      DO 50 J=1,JS
       WRITE(34) NAM(J),IM(J),IB(J),IC(J)
       WRITE(34) AZ(J),P(J)*V(J),NT
50     WRITE(34) (X(I,J)/fc(j),I=1,NT)
****************************************************
**    Iteration to seek discrete subevents        **
****************************************************
      write(36,203)
203    format(' Step    Time   X    Y  Slip-angle  Moment')
*   < Iteration starts >
           LAST=MIN(NT,NTE)
           TSM=0
      DO 20 NS=1,NE
         OPT=0.
* << grid-search along dip-direction (K)
       DO 13 K=1,NK
* << grid-search along strike-direction (L)
       DO 13 L=1,NLEN
* < V1 =   Maximum rupture front velocity assumed >
         AL2=(DL*(L-L0))**2+((K-K0)*DK)**2
         IS=SQRT(AL2)/V1/DT
* << grid-search along time-axis (I)
       DO 13  I=IS,NT-1
*   == Correlation RH0(n) ==
         RH0=0.
        DO 131 J=1,JS
131          RH0=RH0+RWX(I+NDELY(L,K,J),K,J)
*   == Inverse ==
           OBJ=RH0/SQRT(RWW(K))
C     IF(abs(OBJ).LE.OPT.OR.I.GT.LAST) GOTO 13
      IF(OBJ.LE.OPT.OR.I.GT.LAST) GOTO 13
             IOPT=I
             LOPT=L
             KOPT=K
C            OPT=abs(OBJ)
             OPT=OBJ
             AOP=RH0/RWW(K)
13      EROR(NS)=EROR(NS-1)-OPT**2/XPOWER
*
* < Residual waveform X(i,j) & Correlation Rwx(i,k,j) >
      DO 16 J=1,JS
         ISF=IOPT-1+NDELY(LOPT,KOPT,J)
      DO 15 I=1,NT
         I1=I-ISF
         IF(I1.LE.0.OR.I1.GT.NT) GOTO 15
          X(I,J)=X(I,J)-AOP*W(I1,KOPT,J)
15       CONTINUE
      DO 16 K=1,NK
16     CALL CORR(W(1,K,J),X(1,J),RWX(1,K,J),NT,NT,NT)
*-------------------------------------------------
         TA=DT*(IOPT-1)
         xax=DL*(LOPT-L0)
         YAX=DK*(KOPT-K0)
        WRITE(34) NS,TA,xax,YAX,AOp
20    TSM=TSM+AOP
       WRITE(36,205) TSM
205    FORMAT('-- Total seismic moment -- ',F8.2)
        WRITE(34)    EROR(NE)
************ End of iteration ******************
* < Obs. - Syn. >
       do 22 j=1,js
22     WRITE(34) (X(I,J)/fc(j),I=1,NT)
      WRITE(36,206)
206   FORMAT(' < Convergence >')
      DO 30 NS=1,NE
30     WRITE(36,207) NS,EROR(NS),('=',LL=1,NINT(EROR(NS)*50.+1))
207   FORMAT(5X,I4,F8.4,2X,51A1)
*
       write(36,208)
208     format(' Stn          Inst Mode Comp  Az    B.Az    Delta',
     -   '     p      G      Fc   shift')
       DO 33 J=1,JS
33       WRITE(36,'(A,3I5,3F7.1,F8.3,2F7.2,i6)')
     -  NAM(J),IM(J),IB(J),IC(J),az(j),az2(j),del(j),p(j),
     -  g(j),fc(j),ix0(j)
      END
