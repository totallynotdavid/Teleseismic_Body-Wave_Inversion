      SUBROUTINE CONV(X,Y,Z,L,M,N)
**********************************
*       Z = X * Y                *
**********************************
      DIMENSION X(L),Y(M),Z(N)
      DO 1 I=1,N
      Z(I)=0.
      I0=MIN0(L,I)
      DO 1 J=1,I0
      I1=I-J+1
      IF(I1.LE.0) GOTO 1
      IF(I1.GT.M) GOTO 1
      Z(I)=Z(I)+X(J)*Y(I1)
    1 CONTINUE
      END
