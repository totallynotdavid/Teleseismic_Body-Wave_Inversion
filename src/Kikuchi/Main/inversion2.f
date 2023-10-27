**==================================================**
**  Multiple Event Analysis (fixed base-elements)   **
**     Files:                                       **
**       1 = Observed records                       **
**       2 = structures                             **
**       5 = source parameters                      **
**      24 = Output for graphyics                   **
**      26 = Results                                **
**============================ Ver.030703 ==========**
      PARAMETER (ND0=1024,NE0=100,NJ0=40,NL0=10,
     -           PI=3.141593,RAD=.0174533)
      IMPLICIT COMPLEX*8 (Z)
       CHARACTER NAM(NJ0)*12,name*40,dsn*40
       dimension     ZI(ND0),ZQP(ND0),ZQS(ND0),ZZ(50),ZP0(50)
       INTEGER   IWK(NE0),IM(NJ0),IB(NJ0),IC(NJ0),LD(NE0,NJ0)
     -,          ix0(nj0),gr(ne0),ms(ne0)
       REAL  MO(NE0),AZ(NJ0),P(NJ0),V(NJ0)
     -,      RWW(NE0,NE0),WT(NJ0),FC(NJ0),SO(ND0),arww(ne0,ne0)
     -,      X(ND0,NJ0),Y(2048),W(ND0,NE0,NJ0)
     -,      RH(NE0),MXY(6),MXY0(6),AX(3),mij(3,3)
     -,      az2(nj0),del(nj0),g(nj0)
     -,      TM(NE0),YAX(NE0),FR(NE0),FFI(NE0),S1(NE0),D1(NE0),A1(NE0)
     -,      smom(50),t1(ne0),t2(ne0)
      COMMON /STR0/NL,VP(NL0),VS(NL0),DEN(NL0),DEP(NL0)
      COMMON /STR1/NL1,VP1(NL0),VS1(NL0),DEN1(NL0),DEP1(NL0)
      COMMON /STR2/NL2,VP2(NL0),VS2(NL0),DEN2(NL0),DEP2(NL0)
* < Structure >
      read(2,'(a40)') dsn
      READ(2,*) TQP,TQS,NL,(VP(L),VS(L),DEN(L),DEP(L),L=1,NL)
      READ(2,*) NL1,(VP1(L),VS1(L),DEN1(L),DEP1(L),L=1,NL1)
      READ(2,*) NL2,(VP2(L),VS2(L),DEN2(L),DEP2(L),L=1,NL2)
* 
      READ(5,'(a40)') name
      READ(5,*) NN,DT,H0,DIP,NE,idummy,ixa,nt
        DO 200 I=1,NE
200   READ(5,*)
     - gr(i),TM(I),FR(I),FFI(I),YAX(I),Dum,Dum,S1(I),D1(I),A1(I)
     - ,ms(i),t1(i),t2(i)
      READ(5,*) NJS,(FC(J),J=1,NJS)
C
C == Green's function W(i,n,j) & observed X(i,j) ======
C
         DF=1/(DT*NT)
         DW=DF*2*PI
* < Q-response >
      CALL QFm(ZQP,NT,TQP,DF)
      CALL QFm(ZQS,NT,TQS,DF)
         HL=0
         DEP(NL)=0
           DO 2 L=1,NL-1
          HL=HL+DEP(L)
         DH=H0-HL
2         IF(DH.LT.0.) GOTO 3
3      LL=L
* -----------------------------------------------
      READ(1,*)
      JS=1
      Jfc=1
100   READ(1,'(A)',END=999) NAM(JS)
      READ(1,*) AZ(JS),AZ2(js),DEL(js),P(JS),G(js),IX0(js)
      READ(1,*) IM(JS),IB(JS),IC(JS)
           IF(IB(JS).EQ.1.OR.IB(JS).EQ.4) V(JS)=VP(ll)
           IF(IB(JS).EQ.2.OR.IB(JS).EQ.3) V(JS)=VS(ll)
      IF(IM(js).EQ.1) THEN
*         = GDSN =
           READ(1,*) IZP,IZZ,IP
           READ(1,*) DS,A0
           DO 11 K=1,IZP
            READ(1,*) TEMP1,TEMP2
11          ZP0(K) = CMPLX(TEMP1,TEMP2)
           DO 12 K=1,IZZ
            READ(1,*) TEMP1,TEMP2
12          ZZ(K) = CMPLX(TEMP1,TEMP2)
           CALL INSTG(ZI,NT,DW,0,ZP0,ZZ,IZP,IZZ,A0,IP)
           DS=DS*1.E4
       ELSE
*         = WWSSN =
           READ(1,*) DS,TS,TG,HS,HG,S
           CALL INSTW(ZI,NT,DW,0,TS,TG,HS,HG,S)
       ENDIF
      READ(1,*) XM,INTVL,DT0
      READ(1,*) (Y(I),I=1,INTVL)

* Remove stations outside the time window and/or zero weight

      IF(IX0(js).GT.INTVL) GOTO 100
      fc(js)=fc(jfc)
	 jfc=jfc+1
      if(fc(js).eq.0.) goto 100

      WT(JS)=1.e4/DS*FC(JS)
      IX0(js)=IX0(js)+IXA
      DO 13 I=1,Nn
           X(I,JS)=0.
           I1=I+IX0(js)
13     IF(I1.GT.0.AND.I1.LE.INTVL)  X(I,JS)=Y(I1)*XM*WT(JS)

*  < Synthetics of Green's functions: w(i,n,j) >
           F=G(js)*DS
        IF(IB(js).EQ.1.OR.IB(js).EQ.4) THEN
            IF(IC(js).EQ.2) F=-F*COS(AZ2(js)*RAD)
            IF(IC(js).EQ.3) F=-F*SIN(AZ2(js)*RAD)
        ENDIF
      DO 20 N=1,NE
          H=H0+YAX(N)*SIN(DIP*RAD)
20    CALL BODYW(W(1,N,js),NT,DT,IB(js),IC(js),S1(N),D1(N),A1(N),
     -     H,AZ(js),P(js),F,ZQP,ZQS,ZI)
      DO 21 N=1,NE
       call stime(SO,Nn,dt,ms(n),t1(n),t2(n))
       CALL CONV(W(1,N,JS),SO,Y,NT,Nn,Nn)
      DO 21 I=1,Nn
21      W(I,N,JS)=Y(I)*WT(JS)*DT
         JS=JS+1
       GOTO 100
999   JS=JS-1
*---------------------------------------------------
       WRITE(24) name
       WRITE(24)JS,Nn,DT,NE
      DO 22 J=1,JS
       WRITE(24) NAM(J),IM(J),IB(J),IC(J)
       WRITE(24) AZ(J),P(J)*V(J),Nn
22     WRITE(24) (X(I,J)/FC(J),I=1,Nn)
*-------------------------------------------
      DO 61 J=1,JS
      DO 61 N1=1,NE
       DLY1=-FR(N1)*P(J)*COS((AZ(J)-FFI(N1))*RAD)
       DLY2=-YAX(N1)*( SQRT(1/V(J)**2-P(J)**2)*SIN(DIP*RAD)
     %    +P(J)*COS(DIP*RAD)*SIN((AZ(J)-FFI(N1))*RAD))
61    LD(N1,J)=NINT((TM(N1)+DLY1+DLY2)/DT)
      XP=0.
      DO 62 J=1,JS
      DO 62 I=1,Nn
          XP=XP+X(I,J)**2
      DO 62 N1=1,NE
          I1=I-LD(N1,J)
62    IF(I1.GT.0.AND.I1.LE.Nn) RH(N1)=RH(N1)+W(I1,N1,J)*X(I,J)
      DO 63 J=1,JS
      DO 63 N1=1,NE
      DO 63 N2=1,NE
          LD1=LD(N1,J)-LD(N2,J)
      DO 63 I=1,Nn
          I1=I-LD1
          IF(I1.LT.1.OR.I1.GT.Nn) GOTO 63
           RWW(N1,N2)=RWW(N1,N2)+W(I1,N1,J)*W(I,N2,J)
63      CONTINUE
        do 65 n=1,ne
 65     rww(n,n)=rww(n,n)*1.01
	call ludcmp(rww,ne,ne0,iwk,ddd)
	do 70 n1=1,ne
	do 70 n2=1,ne
	if(n1.eq.n2) arww(n1,n1)=1.
70      if(n1.ne.n2) arww(n1,n2)=0.
	do 71 n=1,ne
71      call lubksb(rww,ne,ne0,iwk,arww(1,n))

       DO 72 N=1,NE
         MO(N)=0
        DO 72 N1=1,NE
72       MO(N)=MO(N)+arww(N,N1)*RH(N1)

       DO 80 I=1,NE
        CALL DBAS(S1(I),D1(I),A1(I),MO(I),MXY0)
         DO 75 NB=1,6
75      MXY(NB)=MXY(NB)+MXY0(NB)
        WRITE(24)
     - gr(i),TM(I),FR(I),FFI(I),YAX(I),S1(I),D1(I),A1(I),MO(I)
     - ,ms(i),t1(i),t2(i)
        smom(gr(i))=smom(gr(i))+mo(i)
	call mtrx(mxy0,mij)
80       write(24) mij
        CALL DCP(MXY,FS1,DS1,AS1,SM1,DSM1,AX)
            WRITE(24)sm1,fS1,Ds1,As1
	call mtrx(mxy,mij)
            write(24) mij
           OBJ=0.
        DO 81 N1=1,NE
81         OBJ=OBJ+MO(N1)*RH(N1)
         WRITE(24) 1-OBJ/XP
         DO 90 J=1,JS
         DO 85 I=1,Nn
       Y(I)=0
         DO 85 N=1,NE
          I1=I-LD(N,J)
85     IF(I1.GT.0.AND.I1.LE.Nn) Y(I)=Y(I)+W(I1,N,J)*MO(N)
90     WRITE(24) (Y(I)/FC(J),I=1,Nn)
*-----------------------------------
           write(6,'(a,f8.4)')
     -    'Var. and Total moment-tensor = ',1-Obj/Xp
          write(6,'(3e12.3)') ((mij(n1,n2),n1=1,3),n2=1,3)
*-----------------------------------
       WRITE(26,201) name,dsn,nn,dt,h0,ne,dip
       WRITE(26,'(i2,f7.2,3F8.2,f9.3,f8.2,f7.2,f8.2,i2,2f6.2)')
     -(gr(i),TM(I),FR(I),FFI(I),YAX(I),MO(I),S1(I),D1(I),A1(I),
     -  ms(i),t1(i),t2(i),I=1,NE)
        write(26,*) ' group   Moment'
        write(26,'(i5,f8.2)') (i,smom(i),i=1,gr(ne))
       WRITE(26,202) SM1,DSM1,FS1,DS1,AS1
       WRITE(26,'(/A8,1X,F7.4)') 'Error = ',1-OBJ/XP
201    format(a40/a40//'   nt    dt   h0    Ne  dip   f0'
     -   /i5,2f7.2,i3,f6.1,f8.3//'< Subevent sequence >'/
     -' id   time     r       phi     depth',
     -'     Mo  strike     dip    slip')
202   FORMAT(/' Total moment'
     -     /' Mo+-Dm strike dip slip :',2F8.2,3(2X,F6.1))
*-----------------------------------
        write(26,206)
206    format(/' Stn          Inst Mode Comp   Az     B.Az   Del',
     -        '    p      G      fc    shift')
       DO 92 J=1,JS
92      WRITE(26,'(A,3I5,3F7.1,F8.3,2F7.2,I6)')
     - NAM(J),IM(J),IB(J),IC(J),AZ(J)
     - ,az2(j),DEL(J),P(J),g(j),FC(J),IX0(J)
      END
