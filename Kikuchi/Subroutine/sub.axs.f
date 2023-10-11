      SUBROUTINE AXS(ID,XL,DX,X0,DX0)
* < x-axis (ID=0) or y-axis (ID.NE.0)
C     RAD=0.01745
      IF(ID.EQ.0) CALL PLOT(XL,0.,3)
      IF(ABS(ID).EQ.1) CALL PLOT(0.,-XL*ID,3)
      CALL PLOT(0.,0.,2)
      DO 1 X=0.,XL,DX
           XVAL=X0+X/DX*DX0
      IF(ID.EQ.0) THEN
            CALL PLOT(X,0.,3)
            CALL PLOT(X,-0.1,2)
            CALL NUMBER(X-0.2,-0.4,0.25,XVAL,0.,-1)
      ELSE
            CALL PLOT(0.,-X*ID,3)
            CALL PLOT(-0.1,-X*ID,2)
            CALL NUMBER(-0.4,-(X-.2)*ID,0.25,XVAL,-90.*ID,-1)
      END IF
1     CONTINUE
      END
