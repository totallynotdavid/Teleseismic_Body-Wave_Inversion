      SUBROUTINE eigsrt(d,v,n,np) 
      INTEGER n,np 
      REAL  d(np), v(np,np) 
c Given the eigenvalues d and eigenvectors v as output from jacobi (§11.1) or tqli (§11.3), 
c this routine sorts the eigenvalues into descending order, and rearranges the columns of v 
c correspondingly. The method is straight insertion. 
      INTEGER i,j,k 
      REAL p 
      do 13  i=1,n-1 
      k=i 
      p=d(i) 
      do 11  j=i+1,n 
         if (d(j).ge.p) then 
            k=j 
            p=d(j) 
         endif 
11    enddo 
      if(k.ne.i)then 
         d(k)=d(i) 
         d(i)=p 
         do 12  j=1,n 
           p=v(j,i) 
           v(j,i)=v(j,k) 
           v(j,k)=p 
12       enddo
      endif 
13    enddo
      return 
      END 

