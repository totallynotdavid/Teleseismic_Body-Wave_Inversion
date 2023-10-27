      SUBROUTINE DET00(V,NM,U)
        REAL V(NM),U(NM),M(3,3)
*  Derivatives of the determinant det(M)
      CALL MTRX(V,M)
         A11=M(2,2)*M(3,3)-M(2,3)*M(2,3)
         A22=M(1,1)*M(3,3)-M(1,3)*M(1,3)
         A12=M(1,2)*M(3,3)-M(2,3)*M(1,3)
         A13=M(1,2)*M(2,3)-M(2,2)*M(1,3)
         A23=M(1,1)*M(2,3)-M(1,2)*M(1,3)
         A33=M(1,1)*M(2,2)-M(1,2)*M(1,2)
       U(1)=-2*A12
       U(2)=A11-A22
       U(3)=-2*A23
       U(4)= 2*A13
       U(5)=-A11+A33
      END
