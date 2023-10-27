      SUBROUTINE INITG(ID,T1,T2,EROR)
      CHARACTER ID*40
      CALL PLOT(3.,27.,-3)
      CALL SYMBOL(0.,0.,.45,ID,0.,40)
      CALL NUMBER(0.,-.6,.3,T1,0.,1)
      CALL NUMBER(1.,-.6,.3,T2,0.,1)
      CALL NUMBER(3.,-.6,.3,EROR,0.,4)
      CALL PLOT(0.,-1.5,-3)
      END
