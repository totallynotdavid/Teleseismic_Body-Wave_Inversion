      SUBROUTINE jacobi(a,n,np,d,v,nrot) 
      INTEGER n,np,nrot,NMAX 
      REAL a(np,np),d(np),v(np,np) 
      PARAMETER (NMAX=1200) 
c Computes all eigenvalues and eigenvectors of a real symmetric matrix a, which is of size n 
c by n, stored in a physical np by np array. On output, elements of a above the diagonal are 
c destroyed. d returns the eigenvalues of a in its first n elements. v is a matrix with the same 
c logical and physical dimensions as a, whose columns contain, on output, the normalized 
c eigenvectors of a. nrot returns the number of Jacobi rotations that were required. 
      INTEGER i, ip, iq, j 
      REAL c, g, h, s, sm, t, tau, theta, tresh, b(NMAX), z(NMAX)
c  Initialize to the identity matrix. 
      do 12  ip=1, n                      
      do 11 iq=1, n 
      v(ip, iq)=0. 
11    enddo 
      v(ip, ip)=1. 
12    enddo 
      do 13  ip=1, n 
c  Initialize b and d to the diagonal of a. 
      b(ip)=a(ip, ip)            
      d(ip)=b(ip) 
      z(ip)=0.                   
c This vector will accumulate terms of the form tapq 
13    enddo                 
c as in equation (11.1.14). 
      nrot=0 
      do 24  i=1,50 
      sm=0. 
c Sum off-diagonal elements. 
      do 15  ip=1, n-1                               
      do 14  iq=ip+1, n 
      sm=sm+abs(a(ip, iq)) 
14    enddo 
15    enddo 
      if(sm. eq. 0.)return                          
      if(i. lt. 4)then                              
      tresh=0.2*sm/n**2                      
      else 
      tresh=0.                                       
      endif 
      do 22  ip=1, n-1 
      do 21  iq=ip+1, n 
      g=100.*abs(a(ip, iq)) 
c After four sweeps, skip the rotation if the off-diagonal element is small. 
      if((i.gt.4).and.(abs(d(ip))+g.eq.abs(d(ip)))
     & .and.(abs(d(iq))+g.eq.abs(d(iq))))then 
      a(ip,iq)=0. 
      else if(abs(a(ip,iq)).gt.tresh)then 
      h=d(iq)-d(ip) 
      if(abs(h)+g.eq.abs(h))then 
      t=a(ip,iq)/h             
      else 
      theta=0.5*h/a(ip,iq)             
      t=1./(abs(theta)+sqrt(1.+theta**2)) 
      if(theta.lt.0.)t=-t 
      endif 
      c=1./sqrt(1+t**2) 
      s=t*c 
      tau=s/(1.+c) 
      h=t*a(ip,iq) 
      z(ip)=z(ip)-h 
      z(iq)=z(iq)+h 
      d(ip)=d(ip)-h 
      d(iq)=d(iq)+h 
      a(ip,iq)=0. 
      do 16  j=1,ip-1                          
c Case of rotations 1 ≤ j < p. 
      g=a(j,ip) 
      h=a(j,iq) 
      a(j,ip)=g-s*(h+g*tau) 
      a(j,iq)=h+s*(g-h*tau) 
16    enddo
      do 17  j=ip+1,iq-1            
c Case of rotations p < j < q. 
      g=a(ip,j) 
      h=a(j,iq) 
      a(ip,j)=g-s*(h+g*tau) 
      a(j,iq)=h+s*(g-h*tau) 
17    enddo
      do 18  j=iq+1,n                  
c Case of rotations q < j ≤ n. 
      g=a(ip,j) 
      h=a(iq,j) 
      a(ip,j)=g-s*(h+g*tau) 
      a(iq,j)=h+s*(g-h*tau) 
18    enddo 
      do 19  j=1,n 
      g=v(j,ip) 
      h=v(j,iq) 
      v(j,ip)=g-s*(h+g*tau) 
      v(j,iq)=h+s*(g-h*tau) 
19    enddo 
      nrot=nrot+1 
      endif 
21    enddo
22    enddo
      do 23  ip=1,n 
      b(ip)=b(ip)+z(ip) 
      d(ip)=b(ip)                                 
      z(ip)=0.                                    
23    enddo 
24    enddo 
      pause 'too many iterations in Jacobi'
      return 
      END 

