      SUBROUTINE PLOTW(X,N,DX,AM,ID,LTP,AMP)
* < To plot time-series Xi >
*      if ltp > 1,  fill region below the line
*
      DIMENSION X(N),XP(5000),TP(5000)
       if(n.gt.5000) then
         print *,' The number of samples is larger than 5000 in plotw'
         return
      endif
      DO 1 I=1,5000
        TP(I)=(I-1)*DX
1       XP(I)=0.
      IF(ID.EQ.0) GOTO 3
        AM=0.
      DO 2 I=1,N
2       AM=AMAX1(AM,ABS(X(I)))
3     IF(AM.EQ.0.) AM=1.
        AM1=AMP/AM
      DO 4 I=1,N
4       XP(I)=X(I)*AM1
      n500=(n-1)/500+1
       do 10 nn=1,n500
        is=(nn-1)*500
        if(is.eq.0) is=1
        ie=min(n,nn*500)
        if(ltp.le.1) call plot(tp(is),xp(is),3)
        if(ltp.gt.1) call plot(tp(is),0.,3)
      DO 6 I=is,ie
6     CALL PLOT(TP(I),XP(I),2)
      if(ltp.gt.1) then
          call plot(tp(ie),0.,2)
          write(8,*) 'gsave 0.8 setgray fill grestore'
      else
          call plot(tp(ie),xp(ie),3)
      endif
10     continue
      CALL PLOT(0.,0.,3)
      if(ltp.le.1) return
        write(8,*) '0.8 setgray'
            do 11 nn=1,n500
               if(nn*500.ge.n) goto 12
            ie=nn*500
              call plot(tp(ie),xp(ie),3)
11            call plot(tp(ie),0.,2)
12            call plot(0.,0.,3)
            write(8,*) '0.0 setgray'
      END
