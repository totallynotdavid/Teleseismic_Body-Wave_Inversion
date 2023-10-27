      SUBROUTINE BODYfc(X,N,DT,IB,IC,Azs,Dps,AZ,P,FC0,ZQP,ZQS,ZI)
*===================================================*
*  Synthetics for single-force at free surface
*    Type of body wave  IB= 1/2/3/4: P/SV/SH/PP     *
*    Component          IC= 1/2/3: UD/NS/EW (IB=1/4)*
*                       IC= 1/2  : UD/HR    (IB=2)  *
*                       IC= any  : SH       (IB=3)  *
*============================ Ver.980824 ===========*
      PARAMETER (ND=4096,NL0=10,RADIUS=6371,PI=3.141593)
      IMPLICIT COMPLEX*8 (Z)
      DIMENSION X(N),Z(ND),ZI(ND),ZQP(ND),ZQS(ND)
     -,  ZR0(ND),ZHR(ND),ZVR(ND),ZSH(ND),ZRPP(ND),ZDM(ND)
      COMMON /STR0/NL ,VP (NL0),VS (NL0),DEN (NL0),DEP (NL0)
      COMMON /STR1/NL1,VP1(NL0),VS1(NL0),DEN1(NL0),DEP1(NL0)
      COMMON /STR2/NL2,VP2(NL0),VS2(NL0),DEN2(NL0),DEP2(NL0)
         Rad=pi/180.
         DF=1/(DT*N)
         DW=DF*2*PI
         TL=DT*N
* < Radiation pattern >
       fr=cos((azs-az)*rad)*cos(dps*rad)
       ft=sin((azs-az)*rad)*cos(dps*rad)
       fv=sin(dps*rad)
       
* < Structure effects: Near-source & near-reciever >
       IF(IB.EQ.3) GOTO 1
       CALL CNVR(ZHR,ZVR,DW,N,P,VP,VS,DEN,DEP,NL,IB,TR1)
       IF(IC.EQ.1)
     - CALL CNVR(ZDM,ZR0,DW,N,P,VP1,VS1,DEN1,DEP1,NL1,IB,TR2)
       IF(IC.NE.1)
     - CALL CNVR(ZR0,ZDM,DW,N,P,VP1,VS1,DEN1,DEP1,NL1,IB,TR2)
*   PP-reflector
       CALL REFL(ZRPP,ZDM,ZDM,ZDM
     -              ,DW,N,P,VP2,VS2,DEN2,DEP2,NL2,NL2,IB,TR0)
              GOTO 2
1     CALL CNVRSH(ZSH,DW,N,P,VS,DEN,DEP,NL,TR1)
      CALL CNVRSH(ZR0,DW,N,P,VS1,DEN1,DEP1,NL1,TR2)
2      CONTINUE
* < Delay for Layers >
       DELY=TR1+TR2
*** Factor
      IF(IB.EQ.1.OR.IB.EQ.4) FC=FC0/(4*PI*DEN(NL)*VP(NL)**2)
      IF(IB.EQ.2.OR.IB.EQ.3) FC=FC0/(4*PI*DEN(NL)*VS(NL)**2)

      DO 3 I=1,N/2
           W=DW*(I-1)
*--------------------------------------------------------------
* P or PP wave
      IF(IB.EQ.1.OR.IB.EQ.4) THEN
       Z(I) =(Zhr(I)*Fr+Zvr(I)*Fv)*ZQP(I)
       IF(IB.EQ.1) GOTO 50
*  PP-reflector & additional Q & Hilbert-transform
         Z(I) = Z(I)*ZRPP(I)*ZQP(I)*CMPLX(0.,1.)
* SV-wave
      ELSEIF(IB.EQ.2) THEN
       Z(I) =(Zhr(I)*Fr+Zvr(I)*Fv)*ZQS(I)
* SH-wave
      ELSEIF(IB.EQ.3) THEN
        Z(I) =(Zsh(I)*Ft)*ZQS(I)
      ENDIF
*--------------------------------------------------------------
50      Z(I)=FC*Z(I)*ZR0(I)*ZI(I)*EXP(CMPLX(0.,W*DELY))
      IF(I.EQ.1) GOTO 3
       Z(N+2-I)=CONJG(Z(I))
3     CONTINUE
         Z(N/2+1)=0
      CALL CFFT(Z,N,1)
      DO 5 I=1,N
5     X(I)=Z(I)*DF
      END
