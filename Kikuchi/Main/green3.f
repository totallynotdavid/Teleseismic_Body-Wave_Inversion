**========================= Ver.030712 ==============**
** Green's functions W(i;k) with a fixed mechanism   **
**        i:time step  k:depth#                      **
**                for  j-th station                  **
**   Files:                                          **
**     1 = observed records                          **
**     2 = structure                                 **
**     33 = Green's function                         **
**===================================================**
      PARAMETER (ND0=1024,LK0=11,NL0=10,PI=3.141593,RAD=.0174533)
      IMPLICIT COMPLEX*8 (Z)
      CHARACTER NAME*40,NAM*12,dsn*40
      DIMENSION W(ND0,LK0),ZI(ND0),ZQP(ND0),ZQS(ND0)
     -,     ZZ(50),ZP0(50)
      COMMON /STR0/NL,VP(NL0), VS(NL0), DEN(NL0), DEP(NL0)
      COMMON /STR1/NL1,VP1(NL0),VS1(NL0),DEN1(NL0),DEP1(NL0)
      COMMON /STR2/NL2,VP2(NL0),VS2(NL0),DEN2(NL0),DEP2(NL0)
      read(2,'(a40)') name 
      READ(2,*) TQP,TQS,NL,(VP(L),VS(L),DEN(L),DEP(L),L=1,NL)
      READ(2,*) NL1,(VP1(L),VS1(L),DEN1(L),DEP1(L),L=1,NL1)
      READ(2,*) NL2,(VP2(L),VS2(L),DEN2(L),DEP2(L),L=1,NL2)
* < from unit 5 >
*         NT should be powers of 2.
      OPEN(5,FILE='i_green3')
      READ(5,*) NT,DT,STK,DIP,SLIP,H0,DK,NK,K0
         DF=1/(DT*NT)
         DW=DF*2*PI
* < Q-response >
      CALL QFm(ZQP,NT,TQP,DF)
      CALL QFm(ZQS,NT,TQS,DF)
* < Source layer ll >
       HL=0
         DEP(NL)=0
           DO 2 L=1,NL-1
          HL=HL+DEP(L)
         DH=H0-HL
2         IF(DH.LT.0.) GOTO 3
3      LL=L
* -----------------------------------------------
      READ(1,'(A40)') dsn
        print *,' green3 for ',dsn
      WRITE(33)
     -  NAME,VP(LL),VS(LL),NT,DT,STK,DIP,SLIP,H0,DK,NK,K0
100   READ(1,'(A)',END=999)NAM
      READ(1,*) AZ,AZ2,Dum,P,G,IX0
      READ(1,*) IM,IB,IC
      IF(IM.EQ.1) THEN
*         = GDSN =
           READ(1,*) IZP,IZZ,IP
           READ(1,*) DS,A0
           DO 20 K=1,IZP
            READ(1,*) TEMP1,TEMP2
20          ZP0(K) = CMPLX(TEMP1,TEMP2)
           DO 22 K=1,IZZ
            READ(1,*) TEMP1,TEMP2
22          ZZ(K) = CMPLX(TEMP1,TEMP2)
           CALL INSTG(ZI,NT,DW,0,ZP0,ZZ,IZP,IZZ,A0,IP)
           DS=DS*1.E4
       ELSE
*         = WWSSN =
           READ(1,*) DS,TS,TG,HS,HG,S
           CALL INSTW(ZI,NT,DW,0,TS,TG,HS,HG,S)
       ENDIF
      READ(1,*) Dum,INTVL,Dum
      READ(1,*) (Dum,I=1,INTVL)
C**** skip stations where IX0>INTVL
         IF(IX0.GT.INTVL) GO TO 100
*       < Synthetics >
        FC=G*DS
       IF(IB.EQ.1.OR.IB.EQ.4) THEN
            IF(IC.EQ.2) FC=-FC*COS(AZ2*RAD)
            IF(IC.EQ.3) FC=-FC*SIN(AZ2*RAD)
        ENDIF
        DO 10 K=1,NK
         HH=H0+(K-K0)*DK*SIN(DIP*RAD)
10     CALL BODYW(W(1,K),NT,DT,IB,IC,STK,DIP,SLIP,
     -     HH,AZ,P,FC,ZQP,ZQS,ZI)
       WRITE(33) NAM
       do 11 k=1,nk
       do 11 i=1,nt
11     write(33) W(I,K)
       GOTO 100
999   CONTINUE
      END
