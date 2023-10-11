      SUBROUTINE QUADRD(A,X,Y,ILL)
* < Quadratic equation >
*  (copied from Computer center of Nagoya University,June 6,1989)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(3),X(2),Y(2)
      IF(A(1).EQ.0.D0) GO TO 30
      ILL=0
      IF(A(3).EQ.0.D0) GO TO 20
      P=-A(2)/A(1)*0.5D0
      Q=A(3)/A(1)
      D=P*P-Q
      IF(D.LT.0.D0) GO TO 10
      X(1)=DSIGN(DSQRT(D),P)+P
      X(2)=Q/X(1)
      Y(1)=0.D0
      Y(2)=0.D0
      RETURN
   10 Y(1)=DSQRT(-D)
      Y(2)=-Y(1)
      X(1)=P
      X(2)=P
      RETURN
   20 X(1)=-A(2)/A(1)
      X(2)=0.D0
      Y(1)=0.D0
      Y(2)=0.D0
      RETURN
   30 ILL=30000
      END
