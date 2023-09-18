      SUBROUTINE LVNS(R,A,N,al)
*----------------------------------------------------
*    LEVINSON'S ALGORITHM 
*       to get a minimum phase inverse filter A(i)
*                from auto-correlation function R(i)
*----------------------------------------------------
      DIMENSION R(N),A(N)
      DO 1 I=2,N
    1 A(I)=0.
      A(1)=1.
      AL=R(1)
      GM=0.
      DO 4 I=2,N
      AL=AL*(1.-GM*GM)
      IF(AL.LE.0.) print *,' Alpha < 0 for i=', i
      BT=0.
      I1=I-1
      DO 3 J=1,I1
      K=I+1-J
    3 BT=BT+A(J)*R(K)
      GM=-BT/AL
      L=(I+1)/2
      DO 4 J=1,L
      K=I+1-J
      AJ=A(J)
      AK=A(K)
      A(J)=AJ+GM*AK
    4 A(K)=AK+GM*AJ
      END
