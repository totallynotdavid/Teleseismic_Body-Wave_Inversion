      SUBROUTINE ZMOM(ARW,RWW,R1,R2,NM,IL)
*  << To seek a moment-tensor with zero-determinant so that
*      Norm(R2(n)-R1(n)) be minimum: R1(n) given >>
*
      REAL*8 DA(4),XR(3),XI(3),AL
      REAL ARW(NM,NM),RWW(NM,NM),R1(NM),R2(NM),RR(6),U(6),UI(6),UJ(6)
           DOB0=0
           AN=0
           DO 11 N1=1,NM
           DO 11 N2=1,NM
11         AN=AN+RWW(N1,N2)*R1(N1)*R1(N2)
           IF(AN.EQ.0.) RETURN
       CALL DET0(R1,NM,DA(4))
      DO 10 I=1,30
       CALL DET00(R2,NM,U)
* UI(n): Normal to the plane defined by det(M)=0
      DO 2 N=1,NM
       UI(N)=0
      DO 1 N1=1,NM
1      UI(N)=UI(N)-ARW(N,N1)*U(N1)
       IF(I.EQ.1) GOTO 2
       UI(N)=(UI(N)+UJ(N))/2
2      UJ(N)=UI(N)
*
      CALL DET1(UI,R1,NM,DA(3))
      CALL DET1(R1,UI,NM,DA(2))
      CALL DET0(UI,NM,DA(1))
*  Lagrange's multiplier, XR(1),is obtained by a cubic equation.
      CALL CUBICD(DA,XR,XI,IL)
            AL=XR(1)
        K0=1
        DO 4  K=2,3
        IF(XI(K).NE.0.D0) GOTO 4
         IF(ABS(XR(K)).GE.ABS(AL)) GOTO 4
          AL=XR(K)
          K0=K
4      CONTINUE
                IF(XI(K0).NE.0.D0) PRINT *,'  CUBIC-ERROR  '
        DO 5 N=1,NM
5       R2(N)=R1(N)+AL*UI(N)
           DOB=0
           DO 6 N1=1,NM
           DO 6 N2=1,NM
6          DOB=DOB+RWW(N1,N2)*(R2(N1)-R1(N1))*(R2(N2)-R1(N2))
        IF(I.GT.3.AND.DOB.GE.DOB0) GOTO 10
           DO 7 N=1,NM
7          RR(N)=R2(N)
        IF(ABS(DOB-DOB0)/AN.LT.1.E-4) GOTO 20
          DOB0=DOB
10       CONTINUE
20        DO 30 N=1,NM
30        R2(N)=RR(N)
       END
