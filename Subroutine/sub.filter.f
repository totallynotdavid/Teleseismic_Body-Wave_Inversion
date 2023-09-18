      subroutine recfil(x,y,n,h,nml)                                   
      dimension x(n),y(n),h(4)                                         
c                                                                      
c      recursive filtering                                             
c   x ; input time series                                              
c   y ; output time series (may be equivalent to x )                   
c   n ; length of x & y                                                
c   h ; filter coefficient                                             
c   nml ; >0 : for normal direction filtering                          
c         <0 : for reverse direction filtering                         
      if(n.le.0) go to 4                                               
      if(nml.ge.0) go to 1                                             
c*** reverse filtering                                                 
      j=n                                                              
      jd=-1                                                            
      go to 2                                                          
c*** normal filtering                                                  
    1 j=1                                                              
      jd=1                                                             
c                                                                      
    2 a=h(1)                                                           
      aa=h(2)                                                          
      b=h(3)                                                           
      bb=h(4)                                                          
      u1=0.                                                            
      u2=0.                                                            
      v1=0.                                                            
      v2=0.                                                            
c*** filtering                                                         
      do 3 i=1,n                                                       
      u3=u2                                                            
      u2=u1                                                            
      u1=x(j)                                                          
      v3=v2                                                            
      v2=v1                                                            
      v1=u1+a*u2+aa*u3-b*v2-bb*v3                                      
      y(j)=v1                                                          
      j=j+jd                                                           
    3 continue                                                         
c*** exit                                                              
      return                                                           
c*** error                                                             
    4 write(6,5) n                                                     
    5 format(//1x,5('?'),3x,'(recfil)',3x,'invalid input',3x,'n=',i5,  
     \ 3x,5('?')//)                                                    
      return                                                           
      end                                                              
      subroutine tandem(x,y,n,h,m,nml)                                 
      dimension x(n),y(n),h(4)                                         
c      recursive filtering in series                                   
c   x ; input time series                                              
c   y ; output time series (may be equivalent to x )                   
c   n ; length of x & y                                                
c   h ; filter coefficient                                             
c   m ; order of filter                                                
c   nml ; >0 : for normal direction filtering                          
c         <0 : for reverse direction filtering                         
      if(n.le.0.or.m.le.0) go to 2                                     
c*** 1-st call                                                         
      call recfil(x,y,n,h,nml)                                         
      if(m.le.1) return                                                
c*** 2-nd and after                                                    
      do 1 i=2,m                                                       
      call recfil(y,y,n,h(i*4-3),nml)                                  
    1 continue                                                         
c                                                                      
      return                                                           
c*** error                                                             
    2 write(6,3) n,m                                                   
    3 format(//1x,5('?'),3x,'(tandem)',3x,'invalid input',3x,'n=',i5,  
     \ 3x,'m=',i5,5('?')//)                                            
      return                                                           
      end                                                              
      subroutine chbhip(h,m,gn,n,eps,fp,fs,ap,as)                      
      dimension h(4)                                                   
      data pi/3.141593/, hp/1.570796/                                  
c*************************************************                     
      wp=amax1(abs(fp),abs(fs))*pi                                     
      ws=amin1(abs(fp),abs(fs))*pi                                     
      if(wp.eq.ws.or.ws.eq.0..or.wp.ge.hp) go to 100                   
c****** determine n , eps & c ********************                     
      c=tan(wp)                                                        
      os=c/tan(ws)                                                   
      ts=alog(os+sqrt((os+1.)*sin(wp-ws)/(cos(wp)*sin(ws))))           
      pa=amin1(abs(ap),abs(as))                                        
      sa=amax1(abs(ap),abs(as))                                        
      if(pa.eq.0.) pa=0.1                                              
      if(sa.eq.0.) sa=5.0                                              
      ps=pa/sa                                                         
      n=max0(2,ifix(abs(alog((1.+sqrt((1.-ps)*(1.+ps)))/ps)/ts)+0.5))  
      fn=n                                                             
      eps=sqrt(pa*sa/cosh(ts*fn))                                      
c*************************************************                     
      m=n/2                                                            
      k=m*4                                                            
      g=1./(eps*float(2**(n-1)))                                       
      ps=alog((1.+sqrt(1.+eps**2))/eps)/fn                             
      ch=cosh(ps)                                                      
      sh=sinh(ps)                                                      
      dp=hp/fn                                                         
      fj=1.                                                            
c*************************************************                     
      do 1 j=1,k,4                                                     
      sj=(cos(dp*fj)*ch)**2                                            
      tj=sin(dp*fj)*sh                                                 
      fj=fj+2.                                                         
      a=1./((c+tj)**2+sj)                                              
      g=g*a                                                            
      h(j)=-2.                                                         
      h(j+1)=1.                                                        
      h(j+2)=2.*((c-tj)*(c+tj)-sj)*a                                   
      h(j+3)=((c-tj)**2+sj)*a                                          
    1 continue                                                         
c****** exit *************************************                     
      gn=g                                                             
      if(mod(n,2).eq.0) return                                         
c****** for odd n ********************************                     
      m=m+1                                                            
      gn=g/(c+sh)                                                      
      h(k+1)=-1.                                                       
      h(k+2)=0.                                                        
      h(k+3)=(c-sh)/(c+sh)                                             
      h(k+4)=0.                                                        
      return                                                           
c****** error ************************************                     
  100 write(6,101) fp,fs                                               
  101 format(/1x,5('?'),'   (chbhip)   invalid input   fp=',e14.6,3x,  
     \ 'fs=',e14.6,3x,5('?')//)                                        
      return                                                           
      end                                                              
      subroutine chbpas (h,m,gn,n,eps,fl,fh,fs,ap,as)  
      complex r(2) , oj , cq                                           
      dimension h(4)                                                   
      data pi/3.141593/,hp/1.570796/                                   
c**********************************************************************
c      chebychev band-pass filter coefficient                          
c                                                                      
c     h    :filter coefficient                                         
c     m     :order of filter                                           
c     gn   :gain factor                                                
c     n     :degree of chebychev polynomial                            
c     eps  :ripple width                                               
c     fl   :low frequency cut off                                      
c     fh   :high frequency cut off                                     
c     fs   :stop band frequency (roll off )                            
c     ap   :max. attenuation in pass band                              
c     as   :min. attenuation in stop band                              
      wl=amin1( abs(fl), abs(fh) ) * pi                                
      wh=amax1( abs(fl), abs(fh) ) * pi                                
      ws = abs(fs)*pi                                                  
      if ( wl.eq.wh.or.wl.eq.0.0. or. wh.ge.hp .or. ws.eq.0.0 .or.     
     1             ws.ge.hp .or. (ws-wl)*(ws-wh).le.0.0 ) go to 100    
c     determination n, eps, and c                                      
      clh = cos(wl) * cos(wh )                                         
      c = clh/sin(wh-wl)                                               
      cc = c*c                                                         
      ww = tan(wl)*tan(wh)*cc                                          
      ws = tan(ws) * c                                                 
      os = abs(ws-(ww/ws) )                                            
      ts = alog( os + sqrt(( os-1.0) * (os+1.0) ) )                    
      pa = amin1( abs(ap), abs(as) )                                   
      sa =amax1( abs(ap), abs(as) )                                    
      if(pa.eq.0.0) pa = 0.1                                           
      if (sa.eq.0.0) sa = 5.0                                          
      ps=pa/ sa                                                        
      n=max0( 2, ifix(abs(alog((1.0+sqrt((1.0-ps)*(1.0+ps)))/ps)/ts)   
     1           +0.5))                                                
      fn = n                                                           
      eps= sqrt ( pa*sa/cosh(ts*fn) )                                  
c**********                                                            
      k=n/2                                                            
      m = k*2                                                          
      l=0                                                              
      fj=1.0                                                           
      g=1.0/(eps*float(2**(n-1)) )                                     
      ps= alog((1.0+sqrt(1.0+eps**2))/eps)/fn                          
      ch=cosh(ps)                                                      
      sh=sinh(ps)                                                      
      dp=hp/fn                                                         
c*******                                                               
      do 2 j = 1,k                                                     
      oj = cmplx( cos(dp*fj)*ch, sin(dp*fj)*sh ) * 0.5                 
      fj = fj+ 2.0                                                     
      cq = csqrt(oj**2+ww)                                             
      r(1) = oj + cq                                                   
      r(2) = oj- cq                                                    
      g = g *cc                                                        
      do 1 i =1,2                                                      
      re = real(r(i))**2                                               
      ri=aimag(r(i))                                                   
      a=1.0/((c+ri)**2+re)                                             
      g=g*a                                                            
      h(l+1)= 0.0                                                      
      h(l+2) = -1.0                                                    
      h(l+3) = 2.0*((ri-c)*(ri+c)+re)*a                                
      h(l+4) = ((ri-c)**2+re)*a                                        
      l=l+4                                                            
    1 continue                                                         
    2 continue                                                         
      gn = g                                                           
      if ( n.eq.m) return                                              
      m = m + 1                                                        
      wpc = cc*cos(wh-wl)/clh                                          
      wmc= -cc*cos(wh+wl)/clh                                          
      a=1.0/(wpc+c*sh)                                                 
      gn = g*c*a                                                       
      h(l+1) = 0.0                                                     
      h(l+2)= -1.0                                                     
      h(l+3) = 2.0*wmc*a                                               
      h(l+4) = (wpc-c*sh)*a                                            
      return                                                           
  100 write (6,101) fl , fh , fs                                       
  101 format(/,1x,5('?'),'   (chbpas)   invalid   input fl=',1pf14.6,  
     1        3x,'fh =',f14.6,3x,'fs =',f14.6,3x,5('?')//)             
      return                                                           
      end                                                              
      subroutine chblop(h,m,gn,n,eps,fp,fs,ap,as)                      
      dimension h(4)                                                   
      data pi/3.141593/,hp/1.570796/                                   
c*************************************************                     
      wp=amin1(abs(fp),abs(fs))*pi                                     
      ws=amax1(abs(fp),abs(fs))*pi                                     
      if(wp.eq.ws.or.wp.eq.0. .or.ws.ge.hp) go to 100                  
c*** determine n , eps & c ***********************                     
      c=1.0/tan(wp)                                                      
      os=c*tan(ws)                                                     
      ts=alog(os+sqrt((os+1.)*sin(ws-wp)/(cos(ws)*sin(wp))))           
      pa=amin1(abs(ap),abs(as))                                        
      sa=amax1(abs(ap),abs(as))                                        
      if(pa.eq.0.) pa=0.1                                              
      if(sa.eq.0.) sa=5.                                               
      ps=pa/sa                                                         
      n=max0(2,ifix(abs(alog((1.+sqrt((1.-ps)*(1.+ps)))/ps)/ts)+0.5))  
      fn=n                                                             
      eps=sqrt(pa*sa/cosh(ts*fn))                                      
c*************************************************                     
      m=n/2                                                            
      k=m*4                                                            
      g=1./(eps*float(2**(n-1)))                                       
      ps=alog((1.+sqrt(1.+eps**2))/eps)/fn                             
      ch=cosh(ps)                                                      
      sh=sinh(ps)                                                      
      dp=hp/fn                                                         
      fj=1.                                                            
c*************************************************                     
      do 1 j=1,k,4                                                     
      sj=(cos(dp*fj)*ch)**2                                            
      tj=sin(dp*fj)*sh                                                 
      fj=fj+2.                                                         
      a=1./((c+tj)**2+sj)                                              
      g=g*a                                                            
      h(j)=2.                                                          
      h(j+1)=1.                                                        
      h(j+2)=2.*((tj-c)*(tj+c)+sj)*a                                   
      h(j+3)=((tj-c)**2+sj)*a                                          
    1 continue                                                         
c*** exit ****************************************                     
      gn=g                                                             
      if(mod(n,2).eq.0) return                                         
c*** for odd n ***********************************                     
      m=m+1                                                            
      gn=g/(sh+c)                                                      
      h(k+1)=1.                                                        
      h(k+2)=0.                                                        
      h(k+3)=(sh-c)/(sh+c)                                             
      h(k+4)=0.                                                        
      return                                                           
c****** error ************************************                     
  100 write(6,101) fp,fs                                               
  101 format(/1x,5('?'),'   (chblop)   invalid input   fp=',e14.6,3x,  
     \ 'fs=',e14.6,3x,5('?')//)                                        
      return                                                           
      end                                                              
      subroutine  butpas(h,m,gn,n,fl,fh,fs,ap,as)                      
      complex  r(2),oj,cq                                              
      dimension  h(4)                                                  
      data  pi/3.141593/,hp/1.570796/                                  
c                                                                      
c      butterworth band pass filter coefficient                        
c                                                                      
c      arguments                                                       
c        h      : filter coefficients                                  
c        m      : order of filter                                      
c        gn     : gain factor                                          
c        n      : order of butterworth function                        
c        fl     : low  frequency cut-off  (non-dimensional)            
c        fh     : high frequency cut-off                               
c        fs     : stop band frequency                                  
c        ap     : max. attenuation in pass band                        
c        as     : min. attenuation in stop band                        
c                                                                      
c      m. saito  (7/i/76)                                              
c                                                                      
      wl = amin1(abs(fl),abs(fh))*pi                                   
      wh = amax1(abs(fl),abs(fh))*pi                                   
      ws = abs(fs)*pi                                                  
      if( wl.eq.0. .or. wl.eq.wh .or. wh.ge.hp .or. ws.eq.0. .or.      
     *    ws.ge.hp .or. (ws-wl)*(ws-wh).le.0. )  go to  100            
c****  determine n & c                                                 
      clh= 1./(cos(wl)*cos(wh))                                        
      op = sin(wh-wl)*clh                                              
      ww = tan(wl)*tan(wh)                                             
      ts = tan(ws)                                                     
      os = abs(ts-ww/ts)                                               
      pa = amin1(abs(ap),abs(as))                                      
      sa = amax1(abs(ap),abs(as))                                      
      if( pa.eq.0. )  pa = 0.5                                         
      if( sa.eq.0. )  sa = 5.                                          
      n  = max0(2,ifix(abs(alog(pa/sa)/alog(op/os))+0.5))              
      cc = exp(alog(pa*sa)/float(n))/(op*os)                           
      c  = sqrt(cc)                                                    
      ww = ww*cc                                                       
c                                                                      
      dp = hp/float(n)                                                 
      k  = n/2                                                         
      m  = k*2                                                         
      l  = 0                                                           
      g  = 1.                                                          
      fj = 1.                                                          
c                                                                      
      do  2  j=1,k                                                     
        oj = cmplx(cos(dp*fj),sin(dp*fj))*0.5                          
        fj = fj+2.                                                     
        cq = csqrt(oj**2+ww)                                           
        r(1) = oj+cq                                                   
        r(2) = oj-cq                                                   
        g  = g*cc                                                      
c                                                                      
        do  1  i=1,2                                                   
          re =  real(r(i))**2                                          
          ri = aimag(r(i))                                             
          a  = 1./((c+ri)**2+re)                                       
          g  = g*a                                                     
            h(l+1) = 0.                                                
            h(l+2) =-1.                                                
            h(l+3) = 2.*((ri-c)*(ri+c)+re)*a                           
            h(l+4) = ((ri-c)**2+re)*a                                  
            l = l+4                                                    
    1   continue                                                       
c                                                                      
    2 continue                                                         
c****  exit                                                            
      gn = g                                                           
      if( n.eq.m )  return                                             
c****  for odd n                                                       
      m = m+1                                                          
      wpc = cc*cos(wh-wl)*clh                                          
      wmc =-cc*cos(wh+wl)*clh                                          
          a  = 1./(wpc+c)                                              
          gn = g*c*a                                                   
            h(l+1) = 0.                                                
            h(l+2) =-1.                                                
            h(l+3) = 2.*wmc*a                                          
            h(l+4) = (wpc-c)*a                                         
      return                                                           
c****  error                                                           
  100 write(6,101)  fl,fh,fs                                           
  101  format(/1x,5('?'),'   (butpas)   invalid input   fl =',         
     *        1pe14.6,3x,'fh =',e14.6,3x,'fs =',e14.6,3x,5('?')//)     
      return                                                           
      end                                                              
      subroutine  butlop(h,m,gn,n,fp,fs,ap,as)                         
      dimension  h(4)                                                  
      data  pi/3.141593/,hp/1.570796/                                  
c                                                                      
c      butterworth low pass filter coefficient                         
c                                                                      
c      arguments                                                       
c        h      : filter coefficients                                  
c        m      : order of filter  (m=(n+1)/2)                         
c        gn     : gain factor                                          
c        n      : order of butterworth function                        
c        fp     : pass band frequency  (non-dimensional)               
c        fs     : stop band frequency                                  
c        ap     : max. attenuation in pass band                        
c        as     : min. attenuation in stop band                        
c                                                                      
c      m. saito  (17/xii/75)                                           
c                                                                      
      wp = amin1(abs(fp),abs(fs))*pi                                   
      ws = amax1(abs(fp),abs(fs))*pi                                   
      if( wp.eq.0. .or. wp.eq.ws .or. ws.ge.hp )  go to  100           
c****  determine n & c                                                 
      tp = tan(wp)                                                     
      ts = tan(ws)                                                     
      pa = amin1(abs(ap),abs(as))                                      
      sa = amax1(abs(ap),abs(as))                                      
      if( pa.eq.0. )  pa = 0.5                                         
      if( sa.eq.0. )  sa = 5.                                          
      n  = max0(2,ifix(abs(alog(pa/sa)/alog(tp/ts))+0.5))              
      cc = exp(alog(pa*sa)/float(n))/(tp*ts)                           
      c  = sqrt(cc)                                                    
c                                                                      
      dp = hp/float(n)                                                 
      m  = n/2                                                         
      k  = m*4                                                         
      g  = 1.                                                          
      fj = 1.                                                          
      c2 = 2.*(1.-c)*(1.+c)                                            
c                                                                      
      do  1  j=1,k,4                                                   
        sj = cos(dp*fj)**2                                             
        tj = sin(dp*fj)                                                
        fj = fj+2.                                                     
        a  = 1./((c+tj)**2+sj)                                         
        g  = g*a                                                       
          h( j ) = 2.                                                  
          h(j+1) = 1.                                                  
          h(j+2) = c2*a                                                
          h(j+3) = ((c-tj)**2+sj)*a                                    
    1 continue                                                         
c****  exit                                                            
      gn = g                                                           
      if( mod(n,2).eq.0 )  return                                      
c****  for odd n                                                       
      m  = m+1                                                         
        gn = g/(1.+c)                                                  
          h(k+1) = 1.                                                  
          h(k+2) = 0.                                                  
          h(k+3) = (1.-c)/(1.+c)                                       
          h(k+4) = 0.                                                  
      return                                                           
c****  error                                                           
  100 write(6,101)  fp,fs                                              
  101  format(/1x,5('?'),'   (butlop)   invalid input   fp =',         
     *        1pe14.6,3x,'fs =',e14.6,3x,5('?')//)                     
      return                                                           
      end                                                              
      subroutine  buthip(h,m,gn,n,fp,fs,ap,as)                         
      dimension  h(4)                                                  
      data  pi/3.141593/,hp/1.570796/                                  
c                                                                      
c      butterworth high pass filter coefficient                        
c                                                                      
c      arguments                                                       
c        h      : filter coefficients                                  
c        m      : order of filter  (m=(n+1)/2)                         
c        gn     : gain factor                                          
c        n      : order of butterworth function                        
c        fp     : pass band frequency  (non-dimensional)               
c        fs     : stop band frequency                                  
c        ap     : max. attenuation in pass band                        
c        as     : min. attenuation in stop band                        
c                                                                      
c      m. saito  (7/i/76)                                              
c                                                                      
      wp = amax1(abs(fp),abs(fs))*pi                                   
      ws = amin1(abs(fp),abs(fs))*pi                                   
      if( ws.eq.0. .or. ws.eq.wp .or. wp.ge.hp )  go to  100           
c****  determine n & c                                                 
      tp = tan(wp)                                                     
      ts = tan(ws)                                                     
      pa = amin1(abs(ap),abs(as))                                      
      sa = amax1(abs(ap),abs(as))                                      
      if( pa.eq.0. )  pa = 0.5                                         
      if( sa.eq.0. )  sa = 5.                                          
      n  = max0(2,ifix(abs(alog(sa/pa)/alog(tp/ts))+0.5))              
      cc = exp(alog(pa*sa)/float(n))*tp*ts                             
      c  = sqrt(cc)                                                    
c                                                                      
      dp = hp/float(n)                                                 
      m  = n/2                                                         
      k  = m*4                                                         
      g  = 1.                                                          
      fj = 1.                                                          
      c2 =-2.*(1.-c)*(1.+c)                                            
c                                                                      
      do  1  j=1,k,4                                                   
        sj = cos(dp*fj)**2                                             
        tj = sin(dp*fj)                                                
        fj = fj+2.                                                     
        a  = 1./((c+tj)**2+sj)                                         
        g  = g*a                                                       
          h( j ) =-2.                                                  
          h(j+1) = 1.                                                  
          h(j+2) = c2*a                                                
          h(j+3) = ((c-tj)**2+sj)*a                                    
    1 continue                                                         
c****  exit                                                            
      gn = g                                                           
      if( mod(n,2).eq.0 )  return                                      
c****  for odd n                                                       
      m = m+1                                                          
        gn = g/(c+1.)                                                  
          h(k+1) =-1.                                                  
          h(k+2) = 0.                                                  
          h(k+3) = (c-1.)/(c+1.)                                       
          h(k+4) = 0.                                                  
      return                                                           
c****  error                                                           
  100 write(6,101)  fp,fs                                              
  101  format(/1x,5('?'),'   (buthip)   invalid input   fp =',         
     *        1pe14.6,3x,'fs =',e14.6,3x,5('?')//)                     
      return                                                           
      end                                                              
