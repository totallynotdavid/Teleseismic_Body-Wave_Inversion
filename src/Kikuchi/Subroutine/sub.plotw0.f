      subroutine plotw0(x,n,dx,am,id,lopt,amp)
* < To plot time-series xi >
*       am is modified as "peak-to-peak"      -900726
*
      parameter (nd0=50000)
      DIMENSION X(N),XP(nd0),TP(nd0)
*  option 
*   lopt=0          draw time-axis
*       =otherwise  not

      DO 1 I=1,nd0
        TP(I)=(I-1)*DX
1       XP(I)=0.
      IF(ID.EQ.0) GOTO 3
        am1=x(1)
	am2=x(1)
      DO 2 I=2,N
        AM1=MAX(AM1,X(I))
2       AM2=min(am2,x(i))
	am=am1-am2
3     IF(AM.EQ.0.) AM=2.
        AM1=AMP/AM*2
      DO 4 I=1,N
4       XP(I)=X(I)*AM1
      T1=(N-1)*DX
      if(lopt.eq.0) goto 5
	   CALL PLOT(T1,0.,3)
           CALL PLOT(0.,0.,2)
5     continue
      n500=(n-1)/500+1
       do 10 nn=1,n500
        is=(nn-1)*500
        if(is.eq.0) is=1
        ie=min(n,nn*500)
        call plot(tp(is),xp(is),3)
      DO 6 I=is,ie
6     CALL PLOT(TP(I),XP(I),2)
          call plot(tp(ie),xp(ie),3)
10     continue
      CALL PLOT(0.,0.,3)
      END
