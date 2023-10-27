**===================================================**
**            << inversion >>                        **
**  Choice:                                          **
**      isr=0/1/2/3/4/  for base elements            **
**          dev.M.T/D.C/S.S/M.T/D.C.vert.NP/         **
**      ifp=0/1/        for grid scheme              **
**          curving/straight fault plane/            **
**  MEMO:-------------------------------------       **
*      Unit of seismic moment                        **
*          = e18 Nm for waveforms in micron-meter    **
**  ------------------------------------------       **
*   Constraint on subevents:                         **
*      space-time region:TL,V1                       **
**  Files:                                           **
**       1 = Observed records                        ** 
**       3 = Green's function (binary)               **
**       4 = Data for graphics (binary)              **
**       5 = Grid-point , weighting factor etc.      **
**      16 = Results for source parameters           **
**============================ Ver.030630 ===========**
       PARAMETER (ND0=1024,NM0=6,NL0=11,NE0=50,NJ0=40,RAD=.01745329)
       CHARACTER ID*40,DSN*40,NAM(NJ0)*12,NAM1*12
       INTEGER   NDELY(NL0,NL0,NJ0),IX0(NJ0),NBASE(0:4)
     -,          im(nj0),ib(nj0),ic(nj0)
     -,          IWK(NM0),IOPT(NE0),LOPT(NE0),KOPT(NE0)
       REAL  AOP(NM0,NE0),MO,MO1,TAXS(3),AZ(NJ0),P(NJ0),WT(NJ0),SO(ND0)
     -,      RWW(NM0,NM0,NL0),ARWW(NM0,NM0,NL0),RWX(ND0,NM0,NL0,NJ0)
     -,      X(ND0,NJ0),Y(ND0),W(ND0,NM0,NL0,NJ0),az2(nj0)
     -,      SYN(ND0,NJ0,NE0),DEL(NJ0),g(nj0),total(nm0),mxy(3,3)
     -,      RH(NM0),RH0(NM0),RH1(NM0),LH(NM0,NM0),FC(NJ0)
     -,      FR(NL0),FFI(NL0),EROR(0:NE0),V(NJ0),total1(nm0)
     -,      LH0(NM0,NM0),TE(NE0)
     -,     Mw
       DATA XPOWER,EROR(0),nbase/0,1,5,5,2,6,4/
* < Input from unit 5 >
      OPEN(5,FILE='i_inversion')
      READ(5,'(A)') ID
      READ(5,*) TL,V1
      READ(5,*) MS,T1,T2
      READ(5,*) NE
      READ(5,*) (TE(I),I=1,NE)
      READ(5,*) NJS,(FC(J),J=1,NJS)
      READ(5,*) IDLY,ISR,IFP
      IF(IFP.EQ.0) THEN
           READ(5,*) NLEN
           READ(5,*) (FR(L),L=1,NLEN)
           READ(5,*) (FFI(L),L=1,NLEN)
      ELSE
           READ(5,*) SK0,DL,NLEN,L0
           DO 1 L=1,NLEN
           FR(L)=DL*(L-L0)
1          FFI(L)=SK0
      ENDIF
        NM=NBASE(ISR)
        JS=1
        jfc=1
      READ(1,'(A40)') DSN
      READ(3)DSN, VP,VS,DT,H0,DK,DIP,NK,K0,INTV
        NT=TL/DT+0.5
        CALL STIME(SO,NT,DT,MS,T1,T2)
100   READ(1,'(A)',END=99) NAM(JS)
      READ(1,*) AZ(JS),AZ2(js),DEL(JS),P(JS),G(js),IX0(JS)
      READ(1,*) IM(JS),IB(JS),IC(JS)
      IF(IM(JS).EQ.1) THEN
*  GDSN Instrument
           READ(1,*) IZP,IZZ
           READ(1,*) DS0
           DS=DS0*1.E4
         DO 2 K=1,IZP+IZZ
2          READ(1,*)
      ELSE
*  WWSSN Instrument: DS is the magnification
           READ(1,*) DS
      ENDIF
       READ(1,*) XM,INTVL,DT0
      READ(1,*) (Y(I),I=1,INTVL)

* Remove records outside the time-window
      IF(IX0(JS).GT.INTVL) GOTO 100

      READ(3,END=99) NAM1
      do 11 k=1,nk
      do 11 n=1,nm0
11    READ(3) (W(I,N,K,JS),I=1,INTV)

         fc(js)=fc(jfc)
         jfc=jfc+1
* Remove records with zero weight
      if(fc(js).eq.0.) goto 100

* Modify amplitudes
*           to an equal instrumental gain and then multiply
*                                         a station weight
      WT(JS)=       (1.e4/DS)   *          FC(JS)
      IX0(JS)=IX0(JS)+IDLY
*
      DO 3 I=1,NT
           X(I,JS)=0.
           I1=IX0(JS)+(I-1)*DT/DT0+1.5
3     IF(I1.GT.0.AND.I1.LE.INTVL)  X(I,JS)=Y(I1)*XM*WT(JS)
            DO 4 N=1,NM
            DO 4 K=1,NK
         CALL CONV(W(1,N,K,JS),SO,Y,INTV,NT,NT)
            DO 4 I=1,NT
4        W(I,N,K,JS)=Y(I)*WT(JS)*DT
       DO 5 L=1,NLEN
       DO 5 K=1,NK
         IF(IB(JS).EQ.1.OR.IB(JS).EQ.4) V(JS)=VP
         IF(IB(JS).EQ.2.OR.IB(JS).EQ.3) V(JS)=VS
         DLY=-FR(L)*P(JS)*COS((AZ(JS)-FFI(L))*RAD)
     -     -DK*(K-K0)*(SQRT(1/V(JS)**2-P(JS)**2)*SIN(DIP*RAD)
     -     +P(JS)*COS(DIP*RAD)*SIN((AZ(JS)-FFI(L))*RAD))
5        NDELY(L,K,JS)=NINT(DLY/DT)
       JS=JS+1
           GOTO 100
99     JS=JS-1
*---------------------------------------------------
        Nfree=Nt*Js-Nm*Ne
      DO 10 J=1,JS
        DO 8 K=1,NK
        DO 8 N1=1,NM
        DO 8 N2=1,NM
            DO 8 I=1,NT
8       RWW(N1,N2,K)=RWW(N1,N2,K)+W(I,N1,K,J)*W(I,N2,K,J)
      DO 10 I=1,NT
10          XPOWER=XPOWER+X(I,J)**2

*---------------------------------------------------
       WRITE(16,'(1X,A40)') ID
       WRITE(16,'(1X,A40)') DSN
       WRITE(16,201)NT,DT,H0,DIP,NE,isr,idly
       WRITE(16,202) MS,T1,T2
       WRITE(16,203) V1,(te(i),i=1,ne)
201   FORMAT('   NT    DT     H0    Dip    Ne Type Shift'
     -       /I6,F6.3,2F7.1,3I5)
202   FORMAT(' Time Function : (Type,T1,T2) = ',I1,2F6.1)
203   FORMAT(' Grid-search extent: V max = ',f6.1
     -      /' Time span at each iteration = ',12f6.1)
*---------------------------------------------------
       WRITE(4) ID
       WRITE(4)JS,NT,DT,NE,T1,T2,MS
       WRITE(4)(SO(I),I=1,NT)
       WRITE(4)NLEN,DK,NK,K0
       WRITE(4)(FR(L),FFI(L),L=1,NLEN)
      DO 20 J=1,JS
       WRITE(4) NAM(J),IM(J),IB(J),IC(J)
       WRITE(4) AZ(J),P(J)*V(J)
20     WRITE(4) (X(I,J)/fc(J),I=1,NT)
****************************************************
**    Iteration to seek discrete subevents        **
****************************************************
* < Inverse matrix ARWW >
      DO 110 K=1,NK
           DO 109 N1=1,NM
           DO 109 N2=1,NM
109   LH(N1,N2)=RWW(N1,N2,K)
       call ludcmp(lh,nm,nm0,iwk,ddd)
       do 102 n1=1,nm
       do 102 n2=1,nm
        if(n1.eq.n2) arww(n1,n2,k)=1
102     if(n1.ne.n2) arww(n1,n2,k)=0
       do 110 n=1,nm
110     call lubksb(lh,nm,nm0,iwk,arww(1,n,k))
           DO 111 N=1,NM
111     total(N)=0
       if(isr.ne.3) WRITE(16,210) 
       if(isr.eq.3) WRITE(16,211) 
210   format('  # Time Location(r,phi,Y-axis)  Moment non.DC ',
     -        ' Strike  Dip   Slip  Error')
211   format('  # Time Location(r,phi,Y-axis)  Moment Isotrpc',
     -        ' Strike  Dip   Slip  Error')
*
*  Iteration starts
*
       Nrep=1
       if(Ne.gt.1) Nrep=3
      DO 150 NNS=1,NREP
                 write(16,'(a7,i2)')'Step : ',NNS
      DO 150 NS=1,NE
         OPT=0.
         RWW0=0
         RWX0=0
      DO 130 J=1,JS
         IF(NNS.EQ.1) GOTO 121
*
*  Put back a subevent obtained at the previous round
*               to the current residual waveforms
      DO 120 I=1,NT
        X(I,J)=X(I,J)+SYN(I,J,NS)
        RWW0=RWW0+SYN(I,J,NS)**2
120     RWX0=RWX0+X(I,J)*SYN(I,J,NS)

121      DO 130 K=1,NK
         DO 130 N=1,NM
130       CALL CORR(W(1,N,K,J),X(1,J),RWX(1,N,K,J),NT,NT,NT)
        IF(NNS.LT.3) GOTO 132
          OPT=RWX0**2/RWW0
            AM=RWX0/RWW0
             DO 131 N=1,NM
131       AOP(N,NS)=AM*AOP(N,NS)
132     CONTINUE
*-------------------------------------------------
       DO 140 K=1,NK
       DO 140 L=1,NLEN
*
* Space-time region swept for subevents is given by V1 as:
*        distance/V1 < t < Tend
*
        IS1=SQRT(FR(L)**2+(K-K0)*(K-K0)*DK**2)/V1/DT+1.5
        IS2=TE(NS)/DT+1.5
        LAST=MIN(NT,IS2)
       DO 140  I=IS1,LAST
*  < Correlation RH0(n) >
         DO 141 N=1,NM
             RH0(N)=0.
         DO 141 J=1,JS
141          RH0(N)=RH0(N)+RWX(I+NDELY(L,K,J),N,K,J)
*  < Inverse >
         DO 142 N=1,NM
             RH(N)=0.
         DO 142 N1=1,NM
            LH(N,N1)=ARWW(N,N1,K)
            LH0(N,N1)=RWW(N,N1,K)
142         RH(N)=RH(N)+ARWW(N,N1,K)*RH0(N1)
           OBJ=0.
        DO 143 N1=1,NM
143        OBJ=OBJ+RH0(N1)*RH(N1)
          DO 144 N=1,NM
144        RH1(N)=RH(N)
*
         IF(ISR.NE.1.AND.ISR.NE.4) GOTO 145
*  < Constraint of 'double-couple' >
*                                         RH(n) ==> RH1(n)
            CALL ZMOM(LH,LH0,RH,RH1,NM0,IL)
               IF(IL.NE.0) GOTO 140
            DOBJ=0.
           DO 146 N1=1,NM
           DO 146 N2=1,NM
146         DOBJ=DOBJ+RWW(N1,N2,K)*(RH1(N1)-RH(N1))*(RH1(N2)-RH(N2))
         OBJ=OBJ-DOBJ
            IF(OBJ.LT.0.) OBJ=0.
145    IF(OBJ.LE.OPT) GOTO 140
      CALL DCP(RH1,STK,DP,SL,MO,DMO,TAXS)
148          IOPT(NS)=I
             LOPT(NS)=L
             KOPT(NS)=K
             OPT=OBJ
             DO 147 N=1,NM
147          AOP(N,NS)=RH1(N)
140     CONTINUE
* ---------------------------------
       IF(NNS.EQ.1)     EROR(NS)=EROR(NS-1)-OPT/XPOWER
*
* Conversion of moment-tensor to double-couple
      CALL DCP(AOP(1,NS),STK,DP,SL,MO,DMO,TAXS)
* Residual waveform X(i,j)
       XP=0.
      DO 155 J=1,JS
         ISF=IOPT(NS)+NDELY(LOPT(NS),KOPT(NS),J)-1
      DO 155 I=1,NT
         I1=I-ISF
         SYN(I,J,NS)=0
         IF(I1.LE.0.OR.I1.GT.NT) GOTO 155
           DO 151 N1=1,NM
151      SYN(I,J,NS)=SYN(I,J,NS)+AOP(N1,NS)*W(I1,N1,KOPT(NS),J)
         X(I,J)=X(I,J)-SYN(I,J,NS)
155     XP=XP+X(I,J)**2
*-------------------------------------------------
         TA=DT*(IOPT(NS)-1)
         RA=FR(LOPT(NS))
         FI=FFI(LOPT(NS))
         YAX=DK*(KOPT(NS)-K0)
         IF(ISR.EQ.3)  DMO=AOP(6,NS)
      WRITE(16,213)NS,TA,RA,FI,YAX,MO,DMO,STK,DP,SL,XP/XPOWER
213   FORMAT(I4,F6.1,3F7.1,2F9.3,3F7.1,F8.4)
      IF(NNS.NE.NREP) GOTO 150
        WRITE(4) NS,TA,RA,FI,YAX,MO,DMO,STK,DP,SL
        call mtrx(aop(1,ns),mxy)
        WRITE(4)  mxy
        WRITE(4)    (TAXS(N),N=1,3)
150   CONTINUE
************ End of iteration ******************
        WRITE(4)    XP/XPOWER
        DO 160 NS=1,NE
        DO 160 N=1,NM
160      total(N)=total(N)+AOP(N,NS)
*
* < Obs. - Syn. >
       do 161 j=1,js
161    WRITE(4) (X(I,J)/fc(J),I=1,NT)
      write(16,'(a16)') '< Convergence > '
      DO 170 NS=1,NE
170    WRITE(16,206) NS,EROR(NS),('=',LL=1,NINT(EROR(NS)*50.+1))
206   FORMAT(5X,I4,F8.4,2X,51A1)
         sx=xp/Nfree
         do 181 n=1,nm
181     total1(n)=total(n)+sign(sqrt(arww(n,n,k0)*sx),total(n))
      CALL DCP(total1,STK1,DP1,SL1,MO1,DMO1,TAXS)
      CALL DCP(total,STK,DP,SL,MO,DMO,TAXS)
         IF(ISR.EQ.3) then
                   DMO=total(6)
                   DMO1=total1(6)
         endif
        WRITE(16,214) MO,DMO,STK,DP,SL
     -   ,MO1-MO,DMO1-DMO,STK1-STK,DP1-DP,SL1-SL
       Mw=(log10(Mo)+8.9)/1.5
        write(6,'(a,f7.2,f8.4)') ' Mw, Var. = ',Mw, Xp/Xpower
        write(16,'(a,f7.2)') ' Mw = ',Mw
214   FORMAT(10x,'Total                ',2F8.2,3F8.1
     -      /10x,'standard-deviation   ',2f8.2,3f8.1)
        WRITE(4) MO,DMO,STK,DP,SL
        call mtrx(total,mxy)
        WRITE(4)  mxy
        WRITE(4) (TAXS(N),N=1,3)
*  < List ofstation parameters >
        write(16,208)
208    format(' Stn          Inst Mode Comp   Az     B.Az   Del',
     -        '    p      G      fc    shift')
       DO 175 J=1,JS
175    WRITE(16,'(A,3I5,3F7.1,F8.3,2F7.2,I6)')
     - NAM(J),IM(J),IB(J),IC(J),AZ(J)
     - ,az2(j),DEL(J),P(J),g(j),FC(J),IX0(J)
      END
