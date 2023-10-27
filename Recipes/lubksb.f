      SUBROUTINE lubksb(a,n,np,indx,b) 
      INTEGER n,np,indx(n) 
      REAL  a(np,np),b(n) 
c Solves the set of n linear equations A · X = B. Here a is input, not as the matrix A but 
c rather as its LU decomposition, determined by the routine ludcmp. indx is input as the 
c permutation vector returned by ludcmp. b(1:n) is input as the right-hand side vector B, 
c and returns with the solution vector X. a, n, np, and indx are not modified by this routine 
c and can be left in place for successive calls with different right-hand sides b. This routine 
c takes into account the possibility that b will begin with many zero elements, so it is efficient 
c for use in matrix inversion. 
      INTEGER i,ii,j,ll 
      REAL sum 
      ii=0 
c When ii is set to a positive value, it will become the index of the first
c nonvanishing element of b. We now do the forward substitution, equation
c (2.3.6). The only new wrinkle is to unscramble the permutation as we go.
      do 12  i=1,n                                            
      ll=indx(i)                                      
      sum=b(ll)                                     
      b(ll)=b(i) 
      if (ii.ne.0)then 
      do 11  j=ii,i-1 
      sum=sum-a(i,j)*b(j) 
11    enddo
      else if (sum.ne.0.) then 
      ii=i                                  
c A nonzero element was encountered, so from now on we will 
c have to do the sums in the loop above. 
      endif                                          
      b(i)=sum 
12    enddo 
      do 14  i=n,1,-1                                
c Now we do the backsubstitution, equation (2.3.7). 
      sum=b(i) 
      do 13 j=i+1,n 
      sum=sum-a(i,j)*b(j) 
13    enddo 
      b(i)=sum/a(i,i)                       
c Store a component of the solution vector X. 
14    enddo 
      return                                              
      END 
