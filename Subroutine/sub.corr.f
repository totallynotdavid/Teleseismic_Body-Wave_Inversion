      SUBROUTINE CORR(X,Y,Z,IX,IY,IZ)
*  < Correlation >
* === dt is not multiplied ===
      DIMENSION X(IX),Y(IY),Z(IZ)
      DO 1 I=1,IZ
      Z(I)=0.
      DO 1 J=1,IX
      I1=I+J-1
      IF(I1.GT.IY) GOTO 1
      Z(I)=Z(I)+X(J)*Y(I1)
    1 CONTINUE
      END
