      SUBROUTINE INSTW(Z,M,DW,ID,TS,TG,HS,HG,S)
*===============================================
*   << Response of WWSSN instrument >>         *
*     ID = 0/1/2 for delta/step/ramp function  *
*===============================================
      IMPLICIT COMPLEX*8 (Z)
      DIMENSION Z(M)
       WS=2*3.141593/TS
       WG=2*3.141593/TG
       A0=(WS*WG)**2
       A1=2.*WS*WG*(HS*WG+HG*WS)
       A2=WS**2+WG**2+4.*(1.-S)*WS*WG*HS*HG
       A3=2.*(HS*WS+HG*WG)
* < Normalization >
      GM=0.
      DO 1 I=2,M/2
      ZW=CMPLX(0.,DW*(I-1))
      GM1=CABS(ZW**3/(A0+ZW*(A1+ZW*(A2+ZW*(A3+ZW)))))
1     GM=MAX(GM,GM1)
      G=1/GM
*
      DO 2 I=2,M/2
C     IM=M+2-I
      ZW=CMPLX(0.,DW*(I-1))
      Z(I)=ZW**(3-ID)*G/(A0+ZW*(A1+ZW*(A2+ZW*(A3+ZW))))
2     Z(M+2-I)=CONJG(Z(I))
      Z(1)=0.
      Z(M/2+1)=0.
      END
