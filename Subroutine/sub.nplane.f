      SUBROUTINE NPLANE(F0,D0,R0)
* < To plot nodal plane >
*
      FACT=1.4142*R0
      DF=3.14159E-02
      X0=R0*SIN(F0)
      Y0=R0*COS(F0)
      CALL PLOT(X0,Y0,3)
      IF(ABS(D0-1.5708).LT.0.0005) GOTO 12
      TAND=TAN(D0)
      IF(ABS(D0).LT.0.0005) RETURN
      DO 10 I=1,99
      F1=DF*I
      F=F0+F1
      THR=ATAN(1./(TAND*SIN(F1)))
      IF(THR.LE.0.) GOTO 10
      R=SIN(THR/2.)*FACT
      X=R*SIN(F)
      Y=R*COS(F)
      CALL PLOT(X,Y,2)
   10 CONTINUE
   12 CALL PLOT(-X0,-Y0,2)
      END
