      SUBROUTINE ludcmp(a,n,np,indx,d) 
      INTEGER n,np,indx(n),NMAX 
      REAL d,a(np,np),TINY 
      PARAMETER (NMAX=1200,TINY=1.0e-20)  
c Largest expected n, and a small number. 
c Given a matrix a(1:n,1:n), with physical dimension np by np, this routine replaces it by 
c the LU decomposition of a rowwise permutation of itself. a and n are input. a is output, 
c arranged as in equation (2.3.14) above; indx(1:n) is an output vector that records the 
c row permutation effected by the partial pivoting; d is output as ±1 depending on whether 
c the number of row interchanges was even or odd, respectively. This routine is used in 
c combination with lubksb to solve linear equations or invert a matrix. 
      INTEGER i,imax,j,k 
      REAL aamax,dum,sum,vv(NMAX)         
c vv stores the implicit scaling of each row. 
      d=1.
c No row interchanges yet. 
      do 12  i=1,n               
c Loop over rows to get the implicit scaling information
      aamax=0.                                        
      do 11  j=1,n 
      if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j)) 
11    enddo
      if (aamax.eq.0.) pause 'singular matrix in ludcmp' 
c No nonzero largest element. 
      vv(i)=1./aamax                
c Save the scaling. 
12    enddo 
      do 19  j=1,n      
c This is the loop over columns of Crout’s method. 
c This is equation (2.3.12) except for i = j. 
      do 14  i=1,j-1                                  
      sum=a(i,j) 
      do 13  k=1,i-1 
      sum=sum-a(i,k)*a(k,j) 
13    enddo 
      a(i,j)=sum 
14    enddo 
      aamax=0.   
c Initialize for the search for largest pivot element. 
      do 16  i=j,n      
c This is i = j of equation (2.3.12) and i = j + 1 . . . N 
      sum=a(i,j)                                
      do 15  k=1,j-1 
      sum=sum-a(i,k)*a(k,j) 
15    enddo 
      a(i,j)=sum 
      dum=vv(i)*abs(sum)             
c Figure of merit for the pivot. 
      if (dum.ge.aamax) then       
c Is it better than the best so far? 
      imax=i 
      aamax=dum 
      endif 
16    enddo 
      if  (j.ne.imax)then    
c Do we need to interchange rows?      Yes, do so... 
      do 17  k=1,n                             
      dum=a(imax,k) 
      a(imax,k)=a(j,k) 
      a(j,k)=dum 
17    enddo 
      d=-d                                   
c ...and change the parity of d. 
      vv(imax)=vv(j)                         
c Also interchange the scale factor. 
      endif 
      indx(j)=imax 
      if(a(j,j).eq.0.)a(j,j)=TINY 
c If the pivot element is zero the matrix is singular (at least to the precision of the al- 
c gorithm). For some applications on singular matrices, it is desirable to substitute TINY 
c for zero. 
      if(j.ne.n)then                                 
c Now, finally, divide by the pivot element. 
      dum=1./a(j,j) 
      do 18  i=j+1,n 
      a(i,j)=a(i,j)*dum 
18    enddo 
      endif 
19    enddo
c Go back for the next column in the reduction. 
      return 
      END 
