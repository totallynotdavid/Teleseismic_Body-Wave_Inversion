      SUBROUTINE MTRX(V,M)
* < Basis tensor:V(n) to moment-tensor:M(i,j)
      REAL V(6),M(3,3)
        M(1,1)=V(2)-V(5)+V(6)
        M(1,2)=V(1)
        M(2,1)=M(1,2)
        M(1,3)=V(4)
        M(3,1)=M(1,3)
        M(2,2)=-V(2)+V(6)
        M(2,3)=V(3)
        M(3,2)=M(2,3)
        M(3,3)=V(5)+V(6)
      END