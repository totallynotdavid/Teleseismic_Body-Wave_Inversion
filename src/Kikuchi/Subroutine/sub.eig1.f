      subroutine eig1(a,k,n,mode,e,dummy,u,wk,icon)
*  This replaces "EIG1" included in FACOM SSLII (subroutine package).
*  "mode" & "dummy" are used just for the consistency of the arguments.
      real a(k,n),u(k,n),e(n),dummy(n),wk(n)
      call jacobi(a,n,k,e,u,m)
      call eigsrt(e,u,n,k)
      icon=0
      end
