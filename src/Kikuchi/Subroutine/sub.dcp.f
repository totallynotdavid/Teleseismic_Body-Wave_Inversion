      SUBROUTINE DCP(V,F1,D1,A1,SM,DSM,AX)
*  << To interpret a moment-tensor in terms of a double-couple. >>
      REAL V(6),AX(3),EG1(3),EG2(3),EV(3,3),WK1(3),MIJ(3,3)
* Output
*      Moment      SM +- DSM
*      Fault mechanism (F1,T1,A1)
*      Tension axis   AX()
* Input
*      Vector V(6)
* MIJ(*) = Moment tensor   (x:north, y:east, z:downward vertical)
* EG(i)  = Eigen values of moment-tensor (to be modified later)
* EV(*,i)= Eigen vector for i-th eigen-value
         PI=3.1415926
         RAD=PI/180
         ROOT2=1.41421356
         CALL MTRX(V,MIJ)
*******************************************************
           CALL EIG1(MIJ,3,3,2,EG1,EG2,EV,WK1,ICON)
            IF(ICON.NE.0) PRINT *,'  SEIG1.COND= ',ICON
*******************************************************
          E1=EG1(1)
          E3=EG1(1)
          I1=1
          I3=1
          DO 2 II=2,3
          IF(E1.GE.EG1(II)) GOTO 1
             E1=EG1(II)
             I1=II
1         IF(E3.LT.EG1(II)) GOTO 2
             E3=EG1(II)
             I3=II
2          CONTINUE
         SM=(E1-E3)/2
         DSM=SM-E1
           DO 3 II=1,3
           AX(II)=EV(II,I1)
           EG1(II)=(EV(II,I3)-EV(II,I1))/ROOT2
3          EG2(II)=(EV(II,I1)+EV(II,I3))/ROOT2
*   EG2 = normal vector for a fault plane
*   EG1 = slip vector
           D1=ACOS(EG2(3))
           F1=ATAN2(EG2(2),EG2(1))+PI/2
           CF=COS(F1)
           SF=SIN(F1)
                     C=EG1(1)*CF+EG1(2)*SF
        IF(D1.NE.0.) S=EG1(3)/SIN(D1)
        IF(D1.EQ.0.) S=EG1(1)*SF-EG1(2)*CF
              F1=F1/RAD
              D1=D1/RAD
              A1=-ATAN2(S,C)/RAD
            IF(D1.LT.90.) RETURN
              A1=-A1
              D1=180-D1
              F1=F1+180
              IF(F1.GT.360) F1=F1-360
        END
