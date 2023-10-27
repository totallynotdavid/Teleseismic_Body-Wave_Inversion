CC===============================================================
CC  "mom3" inverts teleseismic body waves into
CC     moment-release distribution over a finite fault plane.
CC  Rupture front velocity and time-span specify the
CC     rupture time at every grid points.
CC  Slip direction can be restricted to within 90 degrees
CC  Smoothness constraint can also be imposed.
CC---------------------------------------------
CC   Files:
CC    fort.1 = Observed records
CC    fort.2 = structures
CC    fort.24 = Output for graphyics
CC    fort.37 = Results (Time & Space)
CC    fort.46 = Results (Space)
CC
CC Input from stdin (fort.5 or i_mom3):
CC    name: (a)  comment
CC    Inf: (a)  file-name for Observed records
CC    T0,TL,DT,H0,str,dip,slip0:(*)
CC       Start-time, Time-window, Sampling-time, Strike, Dip, Slip0
CC         (Slip is varied between Slip0 +- 45)
CC    Nx,Nx0,dx,Ny,Ny0,dy,Vr,Tst,t1,dlt,nw,beta0:(*)
CC       Grid scheme = Nx x Ny, Reference grid = (Nx0,Ny0)
CC       dx,dy = grid spacings for strike and dip directions
CC       Vr = Rupture front velocity
CC       Tst = Rupture start time
CC       t1,dlt,Nw specify a series of triangle time-functions
CC         t1: half of triangle base, dlt:time-shift
CC         nw: number of triangles
CC    Njs,(Fc(j),j=1,Njs)
CC       Number of wave-records, weight factors.   
CC    Njs must be less than Nj0
CC    Nt: Sample number of each Green's function (power of 2)
CC----------------------------------------------------
CC   Number of observation equations, Nijk=Nij+Ne
CC       Nij=Data points (Nj*Js)
CC        +Ne comes from smoothness constraint
CC   Nijk must be less than Nd0
CC   Number of unknowns, Ne=Nx*Ny*Nw*2
CC       '2' comes from two slip directions
CC   Ne must be less than Ne0
CC============================= Ver. 020101 ======================
      PARAMETER (Nd0=40000,Nd1=500,Ne0=2000,Nj0=150,Nl0=10
     -,  PI=3.141593,RAD=.0174533)
      IMPLICIT COMPLEX*8 (Z)
      character Nam(NJ0)*12,Name*40,Dsn*40,Inf*15
      dimension ZI(ND0),ZQP(ND0),ZQS(ND0),ZZ(50),ZP0(50)
      integer   Iwk(Nd0),Im(Nj0),Ib(Nj0),Ic(Nj0),Ld(Ne0,Nj0),Ix0(Nj0)
       REAL  Mo(Ne0),Az(Nj0),P(Nj0),V(Nj0)
     -,      Rw(Nd0,Ne0),Wt(Nj0),Fc(Nj0),So(Nd1)
     -,      X(Nd1,Nj0),Y(2048),W(Nd1,Ne0,Nj0)
     -,      RH(Nd0),WK(Nd0),Azz(Nd0)
     -,      Az2(Nj0),Del(Nj0),G(Nj0),Slp(Ne0)
     -,      Xs(Ne0),Ys(Ne0),Ts(Ne0),Slx(Nl0,Nl0),Sly(Nl0,Nl0)
      COMMON /STR0/NL,VP(NL0),VS(NL0),DEN(NL0),DEP(NL0)
      COMMON /STR1/NL1,VP1(NL0),VS1(NL0),DEN1(NL0),DEP1(NL0)
      COMMON /STR2/NL2,VP2(NL0),VS2(NL0),DEN2(NL0),DEP2(NL0)
* < Structure >
      read(2,'(a40)') dsn
      READ(2,*) TQP,TQS,NL,(VP(L),VS(L),DEN(L),DEP(L),L=1,NL)
      READ(2,*) NL1,(VP1(L),VS1(L),DEN1(L),DEP1(L),L=1,NL1)
      READ(2,*) NL2,(VP2(L),VS2(L),DEN2(L),DEP2(L),L=1,NL2)
* < Observed records>
      OPEN(5,FILE='i_mom3')
      READ(5,'(a40)') name
      READ(5,'(a)') Inf
        open(1,file=Inf)
C
      print *,' Observed record = ', Inf
C
      READ(5,*) T0,TL,DT,H0,str,dip,slip0
      read(5,*) nx,nx0,dx,ny,ny0,dy,Vr,Tst,t1,dlt,nw,beta0
      READ(5,*) NJS,(FC(J),J=1,NJS)
      READ(5,*) Nt
C
          Nn=TL/dt+.5
          ixa=T0/dt
C
C    Source time element = triangle time function
C          with a pulse width of 2*t1
C
          ms=1
          t2=t1
       call stime(So,Nn,dt,ms,t1,t2)
C
C Grid-scheme & Rupture start time at each grid
C
       Ne=nx*ny*nw*2
C
       Nwx=nw*nx
       Nwx2=nwx*2
        do 200 j=1,ny
        do 200 isd=1,2
        do 200 i=1,nx
        do 200 k=1,nw
C
C      j=co-ordinate along dip (1,..,ny)
C      isd=slip vector (1,2)
C      i=co-ordinate along strike (1,..,nx)
C      k=time step (1,..,nw)
C
C  n = sequential number for the model parameters
C
        n=k+(i-1)*nw+(isd-1)*nwx+(j-1)*nwx2
        slp(n)=slip0-135+90*isd
        xs(n)=dx*(i-nx0)
        ys(n)=dy*(j-ny0)
        ts(n)=sqrt(xs(n)**2+ys(n)**2)/vr+Tst+(k-1)*dlt
200    continue
C
      print *,' Green functions are now being calculated for '
      write(6,'(a,3f7.1)') '    Str, dip, rake = ', str, dip, slip0
* == Green's function W(i,n,j) & observed X(i,j) ======
         DF=1/(DT*NT)
         DW=DF*2*PI
* < Q-response >
***** QF is replaced by QFm   010623 *****
      CALL QFm(ZQP,NT,TQP,DF)
      CALL QFm(ZQS,NT,TQS,DF)
         HL=0
         DEP(NL)=0
           DO 2 L=1,NL-1
          HL=HL+DEP(L)
         DH=H0-HL
2         IF(DH.LT.0.) GOTO 3
3      LL=L
* -----------------------------------------------
      READ(1,*)
      JS=1
      Jfc=1
100   READ(1,'(A12)',END=999) NAM(JS)
      READ(1,*) AZ(JS),AZ2(js),DEL(js),P(JS),G(js),IX0(js)
      READ(1,*) IM(JS),IB(JS),IC(JS)
           IF(IB(JS).EQ.1.OR.IB(JS).EQ.4) V(JS)=VP(ll)
           IF(IB(JS).EQ.2.OR.IB(JS).EQ.3) V(JS)=VS(ll)
      IF(IM(js).EQ.1) THEN
*         = GDSN =
           READ(1,*) IZP,IZZ,IP
           READ(1,*) DS,A0
* -----------------------------------------------
           DO 11 K=1,IZP
            READ(1,*) TEMP1,TEMP2
11          ZP0(K) = CMPLX(TEMP1,TEMP2)
           DO 12 K=1,IZZ
            READ(1,*) TEMP1,TEMP2
12          ZZ(K) = CMPLX(TEMP1,TEMP2)
           CALL INSTG(ZI,NT,DW,0,ZP0,ZZ,IZP,IZZ,A0,IP)
           DS=DS*1.E4
       ELSE
*         = WWSSN =
           READ(1,*) DS,Tpn,TG,HS,HG,S
           CALL INSTW(ZI,NT,DW,0,Tpn,TG,HS,HG,S)
       ENDIF
      READ(1,*) XM,INTVL,DT0
            if(dt.ne.dt0) print *, ' Dt is not matched'
      READ(1,*) (Y(I),I=1,INTVL)
      fc(js)=fc(jfc)
	 jfc=jfc+1
      if(fc(js).eq.0.) goto 100
      WT(JS)=1.e4/DS*FC(JS)
      IX0(js)=IX0(js)+IXA
      DO 13 I=1,Nn
           X(I,JS)=0.
           I1=I+IX0(js)
13     IF(I1.GT.0.AND.I1.LE.INTVL)  X(I,JS)=Y(I1)*XM*WT(JS)

*  < Synthetics of Green's functions: w(i,n,j) >
           F=G(js)*DS
        IF(IB(js).EQ.1.OR.IB(js).EQ.4) THEN
            IF(IC(js).EQ.2) F=-F*COS(AZ2(js)*RAD)
            IF(IC(js).EQ.3) F=-F*SIN(AZ2(js)*RAD)
        ENDIF
      DO 20 n=1,Ne
          H=H0+ys(n)*SIN(DIP*RAD)
20    CALL BODYW(w(1,n,js),NT,DT,IB(js),IC(js),str,dip,slp(n),
     -     H,AZ(js),P(js),F,ZQP,ZQS,ZI)
      DO 21 N=1,NE
       CALL CONV(w(1,n,js),SO,Y,NT,Nn,Nn)
      DO 21 I=1,Nn
21      W(I,N,JS)=Y(I)*WT(JS)*DT
         JS=JS+1
       GOTO 100
999   JS=JS-1
C
      DO 61 J=1,JS
      DO 61 N=1,NE
       DLY1=-xs(N)*P(J)*COS((AZ(J)-str)*RAD)
       DLY2=-ys(N)*(SQRT(1/V(J)**2-P(J)**2)*SIN(DIP*RAD)
     %    +P(J)*COS(DIP*RAD)*SIN((AZ(J)-str)*RAD))
61    LD(N,J)=NINT((ts(N)+DLY1+DLY2)/DT)
C
220   continue
      XP=0.
         Smom1=0.
         Smom2=0.
      k=0
      rd0=0
      DO 62 J=1,JS
      DO 62 I=1,Nn
       k=k+1
          XP=XP+X(I,J)**2
      DO 63 n=1,Ne
          I1=I-LD(N,J)
        IF(I1.GT.0.AND.I1.LE.Nn) Rw(k,n)=w(i1,n,j)
63     rd0=rd0+Rw(k,n)**2
62    Rh(k)=x(i,j)
       Nij=k
       print *, ' Number of Data Nij & unknowns Ne = ', Nij, Ne
       if(Ne.gt.Ne0) stop
CCC
C  Smoothness constraint
CCC
C
C  Sum of Rw(k,n)**2
C
       nsm0=0.
       do 65 n=1,Ne
        ix=xs(n)/dx+.5+nx0
        iy=ys(n)/dy+.5+ny0
        nsm0=nsm0+16
        if(ix.ge.2)  nsm0=nsm0+1
        if(ix.lt.nx) nsm0=nsm0+1
        if(iy.ge.2)  nsm0=nsm0+1
65      if(iy.lt.ny) nsm0=nsm0+1
CCC
       beta=sqrt(rd0/nsm0*beta0)
       do 64 n=1,Ne
        k=n+Nij
       ix=xs(n)/dx+nx0+.5
       iy=ys(n)/dy+ny0+.5
                      Rw(k,n)     =4*beta
        if(ix.ge.2)   Rw(k,n-nw)  =-beta
        if(ix.lt.nx)  Rw(k,n+nw)  =-beta
        if(iy.ge.2)   Rw(k,n-nwx2)=-beta
64      if(iy.lt.ny)  Rw(k,n+nwx2)=-beta
C
       Nijk=Nij+Ne
       print *, ' Number of observation equations: Nijk =',Nijk
        if (Nijk.gt.Nd0) stop
C
c Changed by C Jimenez 07 Ene 2016
c      DO 26 I=1,Nijk
c26     WRITE (70,'(10000F10.4)')(Rw(I,J), J=1,Ne)
c
c      DO 24 J=1,Nijk
c24     write(71,'(1F10.4)') Rh(j)
c Fin cjimenez
c
C Non-Negative Least Squares
C
      call nnls(Rw,Nd0,Nijk,Ne,Rh,Mo,rnorm,wk,azz,iwk,mode)
C
       WRITE(24) name
       write(24) js,Nn,Dt,Ne,ms,t1,t2,str,dip,slip0,h0
      write(24) nx,nx0,dx,ny,ny0,dy
      DO 22 J=1,JS
       WRITE(24) NAM(J),IM(J),IB(J),IC(J)
       WRITE(24) AZ(J),P(J)*V(J),Nn
22     WRITE(24) (X(I,J)/FC(J),I=1,Nn)
*-------------------------------------------
       DO 80 I=1,NE
        WRITE(24) ts(i),xs(i),ys(i),mo(i),slp(i)
       smom1=smom1+mo(i)*cos(slp(i)*rad)
80     smom2=smom2+mo(i)*sin(slp(i)*rad)
       rake0=atan2(smom2,smom1)/rad
C
         err=0.
         DO 90 J=1,JS
         DO 85 I=1,Nn
       Y(I)=0
         DO 85 N=1,NE
          I1=I-LD(N,J)
85     IF(I1.GT.0.AND.I1.LE.Nn) Y(I)=Y(I)+W(I1,N,J)*MO(N)
         WRITE(24) (Y(I)/FC(J),I=1,Nn)
         do 86 i=1,nn
86      err=err+(x(i,j)-y(i))**2
90      continue
        write(24) err/xp
*-----------------------------------
C
CCC  ABIC
C
       ABIC=0.
       if(beta.ne.0.) ABIC=Nijk*alog(err)
     -     -Ne*(alog(beta**2*nsm0)-alog(rd0+beta**2*nsm0))
       write(6,'(a,1pe11.3,e11.3,2e12.4)')
     - 'beta0,var.,ABIC,rnorm**2  = ', beta0,err/XP,ABIC,rnorm**2
C
        Smom=sqrt(smom1**2+smom2**2)*1.e18
        aMw = (log10(smom)-9.1)/1.5
        write(6,'(a,e11.3,2f7.2,f8.4)')
     -    '  Mo, Mw, Rake, Variance = ',Smom,aMw,Rake0,err/Xp
*-----------------------------------
      do 225 n=1,Ne
       l=xs(n)/dx+nx0+.5
       k=ys(n)/dy+ny0+.5
       slx(l,k)=slx(l,k)+mo(n)*cos(slp(n)*rad)
225    sly(l,k)=sly(l,k)+mo(n)*sin(slp(n)*rad)
C
       WRITE(37,201) name,dsn,nn,dt,h0,ne,beta0
201    format(a40/a40//'   nt    dt   h0    Ne   beta0'
     -   /i5,2f7.2,i5,f7.2//'< Subevent sequence >'/
     -'     time     x       y  Slip-angle   Mo')
       WRITE(37,'(f8.1,3F8.2,e12.3)')
     -    (ts(i),xs(i),ys(i),slp(i),mo(i)*1.e18,i=1,Ne)
        write(37,'(a)') ' X   Y   Mo   Rake'
       do 226 l=1,nx
       do 226 k=1,ny
        xss=(l-nx0)*dx
        yss=(k-ny0)*dy
        sl=sqrt(slx(l,k)**2+sly(l,k)**2)
        sla=atan2(sly(l,k),slx(l,k))/rad
226     write(37,'(5f10.2)') xss,yss,sl,sla
       write(37,'(a,e12.4,2f7.2)')
     -    'Mo [Nm], Mw, rake = ',smom,aMw,rake0
       WRITE(37,'(/A8,1X,F7.4)') 'Error = ',err/XP
*-----------------------------------
        close(1)
        write(37,206)
206    format(/' Stn          Inst Mode Comp   Az     B.Az   Del',
     -        '    p      G      fc    shift')
       DO 92 J=1,JS
92      WRITE(37,'(A12,3I5,3F7.1,F8.3,2F7.2,I6)')
     - NAM(J),IM(J),IB(J),IC(J),AZ(J)
     - ,az2(j),DEL(J),P(J),g(j),FC(J),IX0(J)
       print *, ' ***** Inversion is completed *****'
      END
