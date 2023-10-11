      SUBROUTINE STIME(X,N,DT,MODE,T1,T2)
*   Source time function
      PARAMETER(PI=3.1415926)
      REAL X(N)
      DO 1 I=1,N
1     X(I)=0
           IT1=T1/DT+1.5
           IT2=T2/DT+1.5
           IT3=(T1+T2)/DT+1.5

* mode=0 : Impulsive time function
      IF(MODE.EQ.0) THEN
           X(1)=1/DT
           RETURN

* mode=1 : Ramp function       if T2 < T1
*          Symmetric trapezoid if T2 >= T1
      ELSEIF(MODE.EQ.1) THEN
           IF(T2.GE.T1) GOTO 23
             DO 21 I=2,IT1
21          X(I)=(I-1)*DT/T1
             DO 22 I=IT1+1,N
22          X(I)=1.0
          RETURN
23    CONTINUE
         DO 2 I=2,IT1
2          X(I)=(I-1)*DT/(T1*T2)
         DO 3 I=IT1+1,IT2
3          X(I)=1/T2
         DO 4 I=IT2+1,IT3
4          X(I)=(T1+T2-(I-1)*DT)/(T1*T2)
           RETURN

* mode=2 : Ramp function with cosine taper if T2 < T1
*          Symmetric trapezoid             if T2 >= T1
      ELSEIF(MODE.EQ.2) THEN
           IF(T2.GE.T1) GOTO 53
             DO 51 I=2,IT1
51         X(I)=(1-COS(PI*(I-1)*DT/T1))*.5
             DO 52 I=IT1+1,N
52          X(I)=1.0
          RETURN
53    CONTINUE
         DO 5 I=2,IT1
5          X(I)=(1-COS(PI*(I-1)*DT/T1))/(2*T2)
         DO 6 I=IT1+1,IT2
6          X(I)=1/T2
         DO 7 I=IT2+1,IT3
7          X(I)=(1-COS(PI*(T1+T2-(I-1)*DT)/T1))/(2*T2)
           RETURN

* mode=3 : Triangle
      ELSEIF(MODE.EQ.3) THEN
         DO 8 I=2,IT1
8          X(I)=2*(I-1)*DT/(T1*T2)
         DO 9 I=IT1+1,IT2
9          X(I)=2*(T2-(I-1)*DT)/(T2-T1)/T2
          RETURN
       ENDIF
        END
