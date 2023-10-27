      SUBROUTINE STNPLO(F0,PA,R0)
* < Projection of station on an equal-area framework >
*
      FACT=R0*1.414214
      THR=ASIN(PA)
      R=SIN(THR/2.)*FACT
      X=R*SIN(F0)
      Y=R*COS(F0)
      CALL CIRC1(X,Y,0.08)
      END
