**========================= Ver.030705 ==============**
**      << rotSH >>                                  **
**     rotates the co-ordinates to get SH component  **
**     from NS- and EW- components                   **
**        SV -component is excluded                  **
**===================================================**
      PARAMETER (ND=1024,RAD=.0174533)
      REAL X(ND),Y(ND),sh(nd),ZPR(50),ZPI(50),ZZR(50),ZZI(50)
      REAL Xx(ND),Yy(ND)
      CHARACTER NAME*60
       READ(22,'(a)') NAME
       WRITE(1,'(a)') NAME
10    READ(22,'(a)',END=99) NAME
      READ(22,*) AZ,AZ2,DEL,P,G,IX
      READ(22,*) IM,IB,IC
      READ(22,*) NP,NZ,IP
      READ(22,*) DS,A0R
      if(np.gt.0) READ(22,*) (ZPR(J),ZPI(J),J=1,NP)
      if(nz.gt.0) READ(22,*) (ZZR(J),ZZI(J),J=1,NZ)
      IF(Ic.EQ.3) THEN
         READ(22,*) XM,NT,DT
         READ(22,*) (Xx(I),I=1,NT)
         xm1=0.
         do 102 i=1,NT
          x(i)=xx(i)*xm 
102      xm1=max(abs(x(i)),xm1)
         xm=xm1
         goto 10
      ELSE
         READ(22,*) YM,NT,DT
         READ(22,*) (Yy(I),I=1,NT)
         ym1=0.
         do 101 i=1,NT
         y(i)=yy(i)*ym 
101      ym1=max(abs(y(i)),ym1)
         ym=ym1
      ENDIF
      IF(Ic.EQ.1) then
       WRITE(1,'(a)') NAME
       WRITE(1,'(2F7.1,F7.2,F7.3,F7.1,I5)') AZ,AZ2,DEL,P,G,IX
       WRITE(1,*) IM,IB,IC
       WRITE(1,*) NP,NZ,IP
       WRITE(1,'(2E12.3)') DS,A0R
       if(np.gt.0) WRITE(1,'(2E12.3)') (ZPR(J),ZPI(J),J=1,NP)
       if(nz.gt.0) WRITE(1,'(2E12.3)') (ZZR(J),ZZI(J),J=1,NZ)
       WRITE(1,'(E12.4,I5,F8.3)') YM,NT,DT
       WRITE(1,'(10F7.3)') (Y(I)/ym,I=1,NT)
      else
        P=P*1.732
        ST=SIN(RAD*AZ2)
        CT=COS(RAD*AZ2)
* SV(2) & SH(3) -component
C       vM=0.
        hM=0.
       DO 1 I=1,NT
       x(i)=x(i)
       y(i)=y(i)
      sh(I)=Y(I)*ST-X(I)*CT
1     hM=MAX(ABS(sh(I)),hM)
       WRITE(1,'(a)') NAME
       WRITE(1,'(2F7.1,F7.2,F7.3,F7.1,I5)') AZ,AZ2,DEL,P,G,IX
       WRITE(1,*) IM,3,IC
       WRITE(1,*) NP,NZ,IP
       WRITE(1,'(2E12.3)') DS,A0R
       if(np.gt.0) WRITE(1,'(2E12.3)') (ZPR(J),ZPI(J),J=1,NP)
       if(nz.gt.0) WRITE(1,'(2E12.3)') (ZZR(J),ZZI(J),J=1,NZ)
       WRITE(1,'(E12.4,I5,F8.3)') hM,NT,DT
       WRITE(1,'(10F7.3)') (sh(I)/hM,I=1,NT)
      endif
        GOTO 10
99      CLOSE(2)
      END
