      SUBROUTINE DET0(V,NM,DET)
        REAL V(NM),M(3,3)
        REAL*8 DET
*  Determinant (DET)
      CALL MTRX(V,M)
      DET=M(1,1)*( M(2,2)*M(3,3)-M(2,3)*M(2,3) )
     -   -M(1,2)*( M(1,2)*M(3,3)-M(2,3)*M(1,3) )
     -   +M(1,3)*( M(1,2)*M(2,3)-M(2,2)*M(1,3) )
      END
