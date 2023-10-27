**=================================================**
**     << lists >>                                 **
**    lists the station parameters in fort.1       **
**============================ Ver.030705 =========**
       CHARACTER DSN*40,NAM*12,COMP(3)*2,PHAS(4)*2
       DATA COMP/'UD','NS','EW'/
       DATA PHAS/' P','SV','SH','PP'/
      JS=1
      READ(1,'(A40)') DSN
      WRITE(6,'(1X,A40)') DSN
100   READ(1,'(A12)',END=99) NAM
      READ(1,*) AZ,AZ2,DEL,P,G,IX0
      READ(1,*) IM,IB,IC
      IF(IM.EQ.1) THEN
* < GDSN Instrument: DS converges micron to count >
           READ(1,*) IZP,IZZ,IP
           READ(1,*) DS
           DS=DS*1.E4
         DO 2 K=1,IZP+IZZ
2          READ(1,*)
      ELSE
* < WWSSN Instrument: DS is the magnification >
           READ(1,*) DS
      ENDIF
* < Equalize magnification >
       READ(1,*) XM,INTVL,DT0
      READ(1,*) (Y,I=1,INTVL)
      IF(IX0.GT.INTVL) GOTO 100
      WRITE(6,'(A12,3F8.2,f7.3,f7.2,f9.1,i5,2X,A2,2X,A2)')
     - NAM,AZ,AZ2,del,p,g,ds,ix0,PHAS(IB),COMP(IC)
      GOTO 100
99    continue
      END
