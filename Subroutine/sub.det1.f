      SUBROUTINE DET1(U,V,NM,DET)
        REAL U(NM),V(NM),M(3,3),A(3,3)
        REAL*8 DET
*  Determinant (DET) of the moment-tensor with a modified column
      CALL MTRX(V,M)
      CALL MTRX(U,A)
      DET= A(1,1)*( M(2,2)*M(3,3)-M(2,3)*M(2,3) )
     -   - A(1,2)*( M(1,2)*M(3,3)-M(2,3)*M(1,3) )*2
     -   + A(1,3)*( M(1,2)*M(2,3)-M(2,2)*M(1,3) )*2
     -   + A(2,2)*( M(1,1)*M(3,3)-M(1,3)*M(1,3) )
     -   - A(2,3)*( M(1,1)*M(2,3)-M(1,2)*M(1,3) )*2
     -   + A(3,3)*( M(1,1)*M(2,2)-M(1,2)*M(1,2) )
      END
