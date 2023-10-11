**===================================================**
**    Calculate  Green's functions W(i;n,k)          **
**     i:time step   n=6:#. base elements k:depth#   **
**        for  j-th station                          **
**   Files:                                          **
**     1 = station parameters                        **
**     2 = structure                                 **
**     3 = Green's function                          **
**========================= Ver.030630 ==============**
      PARAMETER (ND0=1024,NM0=6,LK0=11,NL0=10,PI=3.141593,RAD=.0174533)
      IMPLICIT COMPLEX*8 (Z)
      CHARACTER NAME*40,NAM*12
      DIMENSION W(ND0,NM0,LK0),ZI(ND0),ZQP(ND0),ZQS(ND0)
     -,     ZZ(50),ZP0(50),S1(NM0),A1(NM0),D1(NM0)
      COMMON /STR0/NL ,VP (NL0),VS (NL0),DEN (NL0),DEP (NL0)
      COMMON /STR1/NL1,VP1(NL0),VS1(NL0),DEN1(NL0),DEP1(NL0)
      COMMON /STR2/NL2,VP2(NL0),VS2(NL0),DEN2(NL0),DEP2(NL0)
      READ(2,'(a40)') name
      READ(2,*) TQP,TQS,NL ,(VP (L),VS (L),DEN (L),DEP (L),L=1,NL )
      READ(2,*)         NL1,(VP1(L),VS1(L),DEN1(L),DEP1(L),L=1,NL1)
      READ(2,*)         NL2,(VP2(L),VS2(L),DEN2(L),DEP2(L),L=1,NL2)

* < Base elements >
*   N=1,2,3,4,5,6 for Mxy,Mzz-Myy,Myz,Mzx,Mzz-Mxx,Mxx+Myy+Mzz
            DATA S1/ 0,135,180,90,90,0/
            DATA D1/90, 90, 90,90,45,500/
            DATA A1/ 0,  0, 90,90,90,0/

* < from unit 5 >
*         NT should be powers of 2.
      READ(5,*) NT,DT,H0,DK,NK,K0,DIP
         DF=1/(DT*NT)
         DW=DF*2*PI
* < Q-response >
      CALL QFm(ZQP,NT,TQP,DF)
      CALL QFm(ZQS,NT,TQS,DF)
* < Source layer LL for a reference point >
       HL=0
         DEP(NL)=0
           DO 2 L=1,NL-1
          HL=HL+DEP(L)
         DH=H0-HL
2         IF(DH.LT.0.) GOTO 3
3      LL=L
* -----------------------------------------------
      READ(1,*)
      WRITE(3) NAME,VP(LL),VS(LL),DT,H0,DK,DIP,NK,K0,NT
      JST=0
100   READ(1,'(A)',END=999)NAM
      READ(1,*) AZ,AZ2,Dum,P,G,IX0
      READ(1,*) IM,IB,IC
        write(6,'(a12)') nam
      IF(IM.EQ.1) THEN
*         = GDSN =
           READ(1,*) IZP,IZZ,IP
           READ(1,*) DS,A0
           DO 20 K=1,IZP
      READ(1,*) TEMP1,TEMP2
20    ZP0(K) = CMPLX(TEMP1,TEMP2)
           DO 22 K=1,IZZ
      READ(1,*) TEMP1,TEMP2
22    ZZ(K) = CMPLX(TEMP1,TEMP2)
           CALL INSTG(ZI,NT,DW,0,ZP0,ZZ,IZP,IZZ,A0,ip)
           DS=DS*1.E4
       ELSE
*         = WWSSN =
           READ(1,*) DS,TS,TG,HS,HG,S
           CALL INSTW(ZI,NT,DW,0,TS,TG,HS,HG,S)
       ENDIF
      READ(1,*) Dum,INTVL,Dt0
      READ(1,*) (Dum,I=1,INTVL)
C**** skip station if IX0>INTVL
      IF(IX0.GT.INTVL) GOTO 100
        JST=JST+1

* < Synthetics >
        FC=G*DS
      IF(IB.EQ.1.OR.IB.EQ.4) THEN
            IF(IC.EQ.2) FC=-FC*COS(AZ2*RAD)
            IF(IC.EQ.3) FC=-FC*SIN(AZ2*RAD)
        ENDIF
        DO 10 K=1,NK
         HH=H0+(K-K0)*DK*SIN(DIP*RAD)
        DO 10 N=1,NM0
10     CALL BODYW(W(1,N,K),NT,DT,IB,IC,S1(N),D1(N),A1(N),
     -     HH,AZ,P,FC,ZQP,ZQS,ZI)
       WRITE(3) NAM
       do 11 k=1,nk
       do 11 n=1,nm0
11     WRITE(3) (W(I,N,K),I=1,NT)
       GOTO 100
999   CONTINUE
      END
