      SUBROUTINE quad3d(x1,x2,ss) 
      REAL ss,x1,x2,h 
      EXTERNAL h 
c     Uses h,qgausx 
c Returns as ss the integral of a user-supplied function func over a three-dimensional region 
c specified by the limits x1, x2, and by the user-supplied functions y1, y2, z1, and z2, as 
c defined in (4.6.2). 
      call qgausx(h,x1,x2,ss) 
      return 
      END 

      FUNCTION f(zz) 
      REAL f,zz,func,x,y,z 
      COMMON /xyz/ x,y,z 
c     USES func 
c Called by qgausz. Calls func. 
      z=zz 
      f=func(x,y,z) 
      return 
      END 

      FUNCTION g(yy) 
      REAL g,yy,f,z1,z2,x,y,z 
      EXTERNAL f 
      COMMON /xyz/ x,y,z 
c     USES f,qgausz,z1,z2 
c Called by qgausy. Calls qgausz. 
      REAL ss 
      y=yy 
      call qgausz(f,z1(x,y),z2(x,y),ss) 
      g=ss 
      return 
      END 

      FUNCTION h(xx) 
      REAL h,xx,g,y1,y2,x,y,z 
      EXTERNAL g 
      COMMON /xyz/ x,y,z 
c     USES g,qgausy,y1,y2 
c  Called by qgausx. Calls qgausy. 
      REAL ss 
      x=xx 
      call qgausy(g,y1(x),y2(x),ss) 
      h=ss 
      return 
      END 
c The necessary user-supplied functions have the following calling sequences: 
c      FUNCTION  func(x,y,z)          
c The 3-dimensional function to be integrated 
c      FUNCTION  y1(x) 
c      FUNCTION  y2(x) 
c      FUNCTION  z1(x,y) 
c      FUNCTION  z2(x,y) 

