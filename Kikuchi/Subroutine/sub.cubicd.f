      SUBROUTINE CUBICD(A,X,Y,ILL)
* < Cubic equation >
*  (copied from Computer center of Nagoya University,June 6,1989)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(4),X(3),Y(3),W(4)
      DO 10 I=1,4
   10 W(I)=A(I)
      IF(W(1).EQ.0.D0) GO TO 110
      Y(1)=0.D0
      IF(W(4).NE.0.D0) GO TO 20
      X(1)=0.D0
      GO TO 100
   20 C=W(1)*3.D0
      B=W(3)/C
      C=W(2)/C
      P=C*C-B
      Q=(0.5D0*B-P)*C-W(4)*0.5D0/W(1)
      IF(Q.EQ.0.D0) GO TO 80
      D=Q*Q-P*P*P
      IF(D) 70,60,30
*  30 U=DCBRT(DSIGN(DSQRT(D),Q)+Q)
   30  d1=dsign(dsqrt(d),q)+q
       u=dsign(abs(d1)**(1.d0/3),d1)
      V=P/U
      IF(P) 40,50,50
   40 Q=(Q+Q)/((U-V)*U+V*V)
      GO TO 80
   50 Q=U+V
      GO TO 80
   60 Q=DSIGN(DSQRT(P)*2.D0,Q)
      GO TO 80
   70 Q=DCOS(DATAN2(DSQRT(-D),Q)/3.D0)*DSQRT(P)*2.D0
   80 X(1)=Q-C
      IF(DABS(X(1)).LE.DABS(W(4))**1.d0/3) GO TO 90
      P=W(2)
      W(2)=W(4)/X(1)+W(3)
      W(1)=W(2)/X(1)+P
      W(3)=W(4)
      GO TO 100
   90 W(2)=X(1)*W(1)+W(2)
      W(3)=X(1)*W(2)+W(3)
  100 CALL QUADRD(W,X(2),Y(2),ILL)
      ILL=0
      RETURN
  110 ILL=30000
      RETURN
      END
