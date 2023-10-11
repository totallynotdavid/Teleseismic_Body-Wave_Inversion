**==============================================**
**       << gregra >>                           **
**       plots Green's function                 **
**    Files:                                    **
**       3 = Green's function (binary)          **
**============================ Ver.030705 ======**
       PARAMETER (ND0=1024,NM=6,NK0=10,NJ0=40,RAD=.01745329)
       CHARACTER DSN*40,NAM(NJ0)*12
       REAL      W(ND0,NM,NK0,NJ0),SO(ND0),X(ND0,NM,NK0)
       READ(5,*) TL,MS,T1,T2,spcm,height,line,Amag
      JS=0
      READ(3) DSN, VP,VS,DT,H0,DK,DIP,NK,K0,INTV
      NT=TL/DT+0.5
       CALL STIME(SO,NT,DT,MS,T1,T2)
        XL=TL/spcm
100    JS=JS+1
      READ(3,END=999) NAM(JS)
      do 1 k=1,nk
      do 1 n=1,nm
1     READ(3) (W(I,N,K,JS),I=1,INTV)
           GOTO 100
999     continue
	   open(8,file='plot.g')
	   call plots(8)
	   call initg(dsn,t1,t2,h0)
        DO 31 K=1,NK
         DEP=H0+(K-K0)*DK*SIN(DIP*RAD)
        CALL SYMBOL(1.+(K-1)*(xl+1),-1.,.3,'h=',0.,2)
31      CALL NUMBER(2.+(K-1)*(xl+1),-1.,.3,DEP,0.,1)
	   call plot(0.,-2.,-3)
        CALL AXS(0,XL,1.,0.,spcm)
        CALL PLOT(0.,-2.,-3)
      DO 50 J=1,JS-1
        W1=0.
        W2=0.
        DO 35 N=1,NM
        DO 35 K=1,NK
        CALL CONV(W(1,N,K,J),SO,X(1,N,K),INTV,NT,NT)
        DO 35 I=1,NT
         X(I,N,K)=X(I,N,K)*DT
         W1=MAX(W1,X(I,N,K))
35       W2=MIN(W2,X(I,N,K))
        WM=W1-W2
         CALL SYMBOL(-1.5,0.,.3,NAM(J),.0,12)
         CALL NUMBER(-1.5,.5,.3,WM/Amag,0.,2)
       print *, j,Nam(j)
      DO 40 K=1,NK
       IF(K.NE.1) CALL PLOT(xl+1.,0.,-3)
      DO 400 N=1,NM
         call plot(0.,-.3,-3)
         write(8,'(a,i2)') '% comp.= ',n
400      CALL PLOTW0(X(1,N,K),NT,DT/spcm,WM,0,line,height)
40       call plot(0.,.3*nm,-3)
         CALL PLOT(-(NK-1)*(xl+1),-(height+2),-3)

      IF(MOD(J,5).NE.0.) GOTO 50
        CALL newpage(8)
	   call initg(dsn,t1,t2,h0)
        DO 41 K=1,NK
         DEP=H0+(K-K0)*DK*SIN(DIP*RAD)
        CALL SYMBOL(1.+(K-1)*(xl+1),-1.,.3,'h=',0.,2)
41      CALL NUMBER(2.+(K-1)*(xl+1),-1.,.3,DEP,0.,1)
	   call plot(0.,-2.,-3)
        CALL AXS(0,XL,1.,0.,spcm)
        CALL PLOT(0.,-2.,-3)
50    continue
      call plote(8)
      close(8)
      END
