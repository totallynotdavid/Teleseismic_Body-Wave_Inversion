      SUBROUTINE qgausx(func, a, b, ss)
      REAL a, b, ss, func
      EXTERNAL func
      INTEGER j
      REAL dx, xm, xr, w(5), x(5)
      SAVE w, x
      DATA w/0.2955242247 0.2692667193 0.2190863625 0.1494513491 0.0666713443/
      DATA x/0.1488743389 0.4333953941 0.6794095682 0.8650633666 0.9739065285/
      xr=0.5*(b-a)
      xm=0.5*(b+a)
      ss=0
      DO j = 1, 5
          dx=xr*x(j)
          ss = ss + w(j)*(func(xm+dx) + func(xm-dx))
      ENDDO
      ss=xr*ss
      RETURN
      END
