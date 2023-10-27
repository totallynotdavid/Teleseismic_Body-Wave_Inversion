      SUBROUTINE STNPLX(F0,PA,R0)
* < Equal area projection >
      FACT=R0*1.4142
      THR=ASIN(PA)
      R=SIN(THR/2.)*FACT
      X=R*SIN(F0)
      Y=R*COS(F0)
      RR=R0/10
      call plot(x+rr,y+rr,3)
      call plot(x-rr,y-rr,2)
      call plot(x+rr,y-rr,3)
      call plot(x-rr,y+rr,2)
      END
