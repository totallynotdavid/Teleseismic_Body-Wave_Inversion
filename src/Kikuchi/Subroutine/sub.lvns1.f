      SUBROUTINE LVNS1(R,G,A,H,M,alfa,beta,gama)
*----------------------------------------
*  Levinson's algorithm to get an inverse filter
*       from auto- and cross- correlation functions:
*            R(i)  and  G(i)
*----------------------------------------
      DIMENSION R(M),G(M),A(M),H(M)
      A(1)=1.
      ALFA=R(1)
      BETA=0.
      H(1)=G(1)/ALFA
      GAMA=0.
      DO 35 I=2,M
      BETA=BETA+R(I)
      U=-BETA/ALFA
      L=(I+1)/2
      K=I
      A(I)=0.
      DO 31 J=1,L
      AJ=A(J)
      AK=A(K)
      A(J)=AJ+U*AK
      A(K)=AK+U*AJ
   31 K=K-1
      ALFA=ALFA+U*BETA
      V=(G(I)-GAMA-H(1)*R(I))/ALFA
      K=I
      H(I)=0.
      DO 32 J=1,I
      H(J)=H(J)+V*A(K)
   32 K=K-1
      BETA=0.
      GAMA=0.
      K=I
      DO 33 J=2,I
      BETA=BETA+A(J)*R(K)
      GAMA=GAMA+H(J)*R(K)
   33 K=K-1
   35 CONTINUE
      END
