      SUBROUTINE QFm(Z,N,TQ,DF)
*  < Q-filter >
C   modification is made for imaginary part
C    following Prof. Kanamori's suggestion  -01/06/29
      COMPLEX Z(N),Z1
C     FN=DF*N/2
      pi=3.141593
      S2=3.*(2.*pi/tq)
      DO 1 I=2,N/2
      F=DF*(I-1)
C     Z1=CMPLX(0.,2*F*TQ)*LOG(CMPLX(0.,F/FN))
      Z1=CMPLX(0.,2*F*TQ)*LOG(CMPLX(0.,F/s2))
      Z(I)=EXP(Z1)
1     Z(N+2-I)=CONJG(Z(I))
      Z(1)=1
      Z(N/2+1)=0
      END
