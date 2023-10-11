*==============================================
**     << conv.sac.farm >>
**     converts SAC data to iterative decon. scheme.
*            original: "conv.sac.iris" written by M. Kikuchi
*            modified by Y. Yamanaka 
*
*      Instrumental response function is needed.
*      Zero-phase bandpass filter is applied.
*      Station parameters such as azimuth, G-factor,
*        ray-parameter are calculated.
*      Subroutine parm:
*           Utsu's formula for azimuth & distance
*      Out-file = fort.22
**============ Ver.030709 ======================
      PARAMETER (nd0=100000,pi=3.1415926)
      IMPLICIT COMPLEX*8 (Z)
      CHARACTER name*6,comp*6,fname*60,event*20
     -   ,cmp(0:3)*5,fname0*60,charac*15,scode*60
      REAL*8 GFACT(500),PP0(500),depth0
      integer hr0,mnu0,yr,month,day,hr,mnu
      REAL  x(nd0),y(nd0)
      DIMENSION Z(nd0),zpole(50),zero(50)
      data np,cmp/8,'disp.','vel. ','acc. ','orig.'/
      common /tbl/ip(110,14),is(110,14),secp(110,14),secs(110,14)
      read(5,'(a)') event
      read(5,*) alats,alons,depth,hr0,mnu0,sec0,id,delmin,delmax
         depth0=depth
      write(22,'(a,a5,3f7.2,2i3,f6.2)') 
     #            event,cmp(id),alats,alons,depth,hr0,mnu0,sec0
* << J-B travel time table >>
      OPEN(11,FILE='jb.ptime')
      OPEN(12,FILE='jb.stime')
1000   read(11,*,end=1001) n,(ip(n,i),secp(n,i),i=1,14)
       goto 1000
1001   read(12,*,end=1002) n,(is(n,i),secs(n,i),i=1,14)
       goto 1001
1002   continue
      close(11)
      close(12)
* << Geometrical factor >>
      OPEN(15,FILE='jb.table')
      CALL GEOM(GFACT,PP0,depth0)
      close(15)
      nstn=0
      print *,' #   stn  comp      Delta    Tr-time   Start in record'
5     read(5,'(a)') fname
      read(5,'(a)') scode
*     ta=advance of start-time relative the standard P/S arrival
*     du=duration
c
c CHANGE by sanchu
c    add iuni
      if(fname.eq.'dummy') goto 90
      read(5,*) ta,du,dt,f1,f2,iph,nr,iuni
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      open(1,file=fname)
      READ(1,'(g15.7)') dt0
      read(1,'(/////5g15.7)') dum, alat, alon, elev
      read(1,'(///////5i10)') yr, nday, hr,mnu, nsec
      read(1,'(5i10)') nmsec,ndum,ndum,ndum,nd
      read(1,'(/////)')
       read(1,'(a6,2x,a13)') name,charac
      read(1,'(////)')
C   format is changed on 00/03/29
       read(1,'(16x,a6)') comp
C      read(1,'(16x,a4)') comp
      read(1,*)
      read(1,*) (x(i),i=1,nd)
      call calen(yr,nday,month,day)
      sec=nsec+nmsec/1000.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       icomp=0
       if(index(comp,'LHZ')+index(comp,'BHZ').ne.0) icomp=1
       if(index(comp,'LHN')+index(comp,'BHN').ne.0) icomp=2
       if(index(comp,'LHE')+index(comp,'BHE').ne.0) icomp=3
        if(icomp.eq.0) then
       if(index(comp,'MHZ').ne.0) icomp=1
       if(index(comp,'MHN').ne.0) icomp=2
       if(index(comp,'MHE').ne.0) icomp=3
        endif
        if(icomp.eq.0) then
       if(index(fname,'lpz')+index(fname,'vbz').ne.0) icomp=1
       if(index(fname,'lpn')+index(fname,'vbn').ne.0) icomp=2
       if(index(fname,'lpe')+index(fname,'vbe').ne.0) icomp=3
        if(icomp.eq.0) print *, ' component is not defined for ', fname
       endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c modified by Y. Yamanaka
c
      open(13,file='response')
112   read(13,'(a)',end=199) fname0
      read(13,'(28x,f25.0)') a0
      read(13,'(28x,a)') charac
      read(charac,*) nzero1
      read(13,'(28x,a)') charac
      read(charac,*) npole1
      if(nzero1.ne.0) then
       do 113 n=1,nzero1
         read(13,*) idum,a1,a2,a3,a4
         zero(n)=cmplx(a1,a2)
113    continue
      endif
      if(npole1.ne.0) then
       do 114 n=1,npole1
         read(13,*) idum,a1,a2,a3,a4
         zpole(n)=cmplx(a1,a2)
114    continue
      endif
      read(13,'(28x,f25.0)') sens
      if(index(fname,fname0).eq.0) goto 112
      if(npole1.eq.0.or.nzero1.eq.0) goto 112
      nzero=nzero1
      npole=npole1
      sc=1.e+6/(a0*sens)
      close(13)
      goto 11
199   close(13)
        print *, name, ' No Instrumental Response'
        goto 5
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
11     continue
       ntf=dt/dt0+.1
       if(ntf.lt.1) intf=dt0/dt+.1
*  hr:mnu:sec     =start time
*  hr0:mnu0:sec0  =origin time
* Utsu's formula ==> delta1  (sphere model ==> delta)
      call parm(alat,alon,alats,alons,gfact,
     -        pp0,phi,phi2,delta,p,G,delta1)
      if(delta1.gt.delmax.or.delta1.lt.delmin) then
         print *, '    out of range : DELTA = ', delta1
         if(nr.eq.0) goto 90
         goto 5
      endif
      call jbtravel(delta1,Depth,Tp,Ts)
       if(iph.eq.1) Trav=Tp
       if(iph.eq.2) Trav=Ts
      if(delta1.lt.10.) Trav=0.
      if(delta1.lt.10.) p=0.
      stm=(hr0-hr)*3600+(mnu0-mnu)*60+sec0+Trav-sec-ta
      if(stm.lt.0.0) goto 5
      it1=stm/dt0+1.5
      nstn=nstn+1
      write(6,'(i3,2x,a5,a4,3x,3f9.1)')
     -     nstn,name,comp,delta1,Trav,stm
        if(it1.gt.nd) then
          print *, 'start ',it1,' Travel time ',Trav,'    ndata ',nd
         print *, hr0,hr,mnu0,mnu,sec0,sec,dt0
          if(nr.eq.0) goto  36
          goto 5
      endif
      it2=(stm+du)/dt0+1.5 
       it2=min(it2,nd)
      nd=it2-it1+1
       do 21 i=1,nd
21     y(i)=0.
      xm=0
      do 3 i=1,nd
       i1=i-1+it1
       if(i1.gt.0) y(i)=x(i1)
3      xm=xm+y(i)
       xm=xm/nd 
       do 4 i=1,nd
4      y(i)=y(i)-xm
       if(id.eq.3) goto 50
       nn=log(nd*1.)/log(2.)
       nn=2**nn
       if(nn.lt.nd) nn=nn*2
      do 31 i=1,nn
31    z(i)=0.
      do 32 i=1,nd
32     z(i)=y(i)
       CALL CFFT(Z,NN,-1)
       DF=1/(dt0*NN)
       dw=df*pi*2
c------------------------------
c modified by Y. Yamanaka
c
      idu=iuni-id
      if(idu.eq.1) then
        nzero=nzero+1
        zero(nzero)=0.
      endif
      if(idu.eq.2) then
        nzero=nzero+2
        zero(nzero)=0.
        zero(nzero-1)=0.
      endif
c------------------------------
      DO 33 I=2,NN/2+1
       F=DF*(I-1)
       zjw=cmplx(0.,dw*(i-1))
        zc1=1
        zc2=1
C
C 0-phase band-pass filter
C
       if(f1.ne.0.) ZC1=1-1/(1+(F/F1)**NP)
       if(f2.ne.0.) ZC2=1/(1+(F/F2)**NP)
*==========================================
        zt=1
       do 333 n=1,npole
333    zt=zt*(zjw-zpole(n))
       do 334 n=1,nzero
334    zt=zt/(zjw-zero(n))
       ZC=ZC1*ZC2*zt
       if(idu.eq.-1) zc=zc*cmplx(0,2*pi*f)
       if(idu.eq.-2) 
     \    zc=zc*cmplx(0,2*pi*f)*cmplx(0,2*pi*f)
       Z(I)=Z(I)*ZC
       if(i.eq.nn/2+1) z(i)=real(z(i))
33      Z(NN-I+2)=CONJG(Z(I))
       Z(1)=0.
       CALL CFFT(Z,NN,1)
       DO 34 I=1,Nd
34       y(I)=Z(i)/Nn*Sc
50     ym=0.
       DO 35 I=1,Nd
35      ym=max(abs(y(i)),ym)
36      if(ntf.ge.1) then
              nd1=nd/ntf
        else
              nd1=nd*intf
        endif
*  hr0:mnu0:sec0=origin time
        sec2=sec0+Trav
        mnu2=sec2/60
        sec2=sec2-mnu2*60
        mnu2=mnu0+mnu2
c
c modified by Y. Yamanaka
c
        write(22,201) scode,mnu2,sec2,-ta,f1,f2
201     format(a12,i4,f7.2,f8.2,3f7.2)
c
        isft=ta/dt+.5
        write(22,200) phi,phi2,delta1,P,G,isft
200    FORMAT(3F8.2,F8.3,F8.2,1x,i4)
        write(22,*) 1,iph,icomp
        if(id.ne.1) write(22,*) 0,0,0
        if(id.eq.1) write(22,*) 0,0,1
        write(22,'(2f5.1)') 1.0,1.0
        write(22,'(e12.3,i7,f7.3)') ym,nd1,dt
        if(ntf.ge.1) write(22,'(10f7.3)') (y((i-1)*ntf+1)/ym,i=1,nd1)
        if(ntf.lt.1) then
             do 210 j=1,nd
               do 210 k=1,intf
210           x((j-1)*intf+k) = y(j)+(y(j+1)-y(j))*(k-1.)/intf
              write(22,'(10f7.3)') (x(i)/ym,i=1,nd1)
        endif
          close(1)
        if(nr.eq.0) goto  90
          goto 5
90        continue
          end

      subroutine parm(alt,aln,alts,alns,gfact,pp0,phi,
     -    phi2,delta,p,G,del1)
       parameter (rad=0.017453292)
      real*8 GFACT(500),PP0(500)
      t0=alts*rad
      t1=alt*rad
      Dphi=(aln-alns)*rad
      C0=COS(T0)
      C1=COS(T1)
      S0=SIN(T0)
      S1=SIN(T1)
      C2=COS(Dphi)
      S2=SIN(Dphi)
      D1=C0*C1*C2+S0*S1
         delta=ACOS(D1)/RAD
* Utsu's formula
       t00=t0-11.55/60*rad*sin(2*t0)
       t11=t1-11.55/60*rad*sin(2*t1)
       C00=COS(T00)
       C11=COS(T11)
       S00=SIN(T00)
       S11=SIN(T11)
        d11=c00*c11*c2+s00*s11
        del1=acos(d11)/rad
*
      A1=C1*S2
      A2=S1*C0-S0*C1*C2
         phi=ATAN2(A1,A2)/RAD
      A1=-C0*S2
      A2=S0*C1-S1*C0*C2
         phi2=ATAN2(A1,A2)/RAD
      IDELT=DELTA
          G=GFACT(IDELT)+(DELTA-IDELT)*(GFACT(IDELT+1)-GFACT(IDELT))
         P=PP0(IDELT)+(DELTA-IDELT)*(PP0(IDELT+1)-PP0(IDELT))
      END
      SUBROUTINE GEOM(GFACT,PP0,H)
       PARAMETER (r0=6371,rad=0.017453292)
*  <<  DELTA VERSUS IH CURVE  >>
      IMPLICIT REAL*8 (A-H,O-Q,S-Z)
      Common V(1000),R(1000),dh,Ndep
      DIMENSION DELTA(500)
      DIMENSION VV(200),TIH(500),GFACT(500),PP0(500)
      READ(15,*)
      READ(15,*) DH1,NDEP1,DH,NDDH
      READ(15,*) (VV(I),I=1,NDEP1)
      NDEP=NDDH*(NDEP1-1)
      DO 11 I=2,NDEP1
      DO 11 J=1,NDDH
      IJ=NDDH*(I-2)+J
      V(IJ)=VV(I-1)+(VV(I)-VV(I-1))/DH1*DH*FLOAT(J)
   11 CONTINUE
C
      L=H/DH+1.
       ir=1
       RH=R0-H
      DO 9 I=1,NDEP
    9 R(I)=R0-DH*FLOAT(I)
C
      DO 100 J=1,180
      TH=RAD*FLOAT(J)*0.5
      P=RH*SIN(TH)/V(L)
      PA=P*V(NDEP)/R(NDEP)
      DEL=0.
      IF(PA.LE.1.) GOTO 90
      R1=RH
      R2=R(L)
      I=L
   10 PA=P*V(I)/R1
      IF(PA.GE.1.) GOTO 20
      DEL1=DASIN(PA)
      PA=P*V(I)/R2
      IF(PA.GE.1.) THEN
          DEL2=3.141593/2.
          DEL=DEL+DEL2-DEL1
          GOTO 20
      ELSE
         DEL2=DASIN(PA)
          DEL=DEL+DEL2-DEL1
          R1=R(I)
          I=I+1
          R2=R(I)
          GOTO 10
      END IF
   20 DEL=DEL*2.
      R2=RH
      I=L-1
      IF(I.EQ.ir-1) GOTO 30
      R1=R(I)
   25 DEL1=DASIN(P*V(I)/R1)
      DEL2=DASIN(P*V(I)/R2)
      DEL=DEL+DEL2-DEL1
      R2=R1
      I=I-1
      IF(I.EQ.ir-1) GOTO 30
      R1=R(I)
      GOTO 25
   30 DEL=DEL+DASIN(P*V(ir)/R0)-DASIN(P*V(ir)/R2)
      GOTO 99
  90   DEL=3.141593
   99 CONTINUE
  100 DELTA(J)=DEL
C
      DO 200 J= 5,125
      DEL=RAD*FLOAT(J)
      I=1
  110 CONTINUE
      IF(DEL.GT.DELTA(I)) THEN
          M=I-1
          GOTO 120
      ELSE
          I=I+1
          GOTO 110
      END IF
  120 TIH(J)=M+(DELTA(M)-DEL)/(DELTA(M)-DELTA(M+1))
      TIH(J)=TIH(J)*0.5
  200 CONTINUE
       DO 300 J=20,115
C     DTIH=(2.*TIH(J-4)+TIH(J-2)-TIH(J+2)-2.*TIH(J+4))/20.
      DTIH=0.
      DO 301 KK=-15,15

 301  DTIH=DTIH+KK*TIH(J-KK)
      DTIH=DTIH/2480
      SH=SIN(TIH(J)*RAD)
      PP0(J)=SH/V(L)
      PP=V(ir)*PP0(J)
      CS0=SQRT(1.-PP*PP)
      SDEL=SIN(FLOAT(J)*RAD)
      GG=V(L)*SH*DTIH/V(ir)/SDEL/CS0
* Ignore the density contrast
  300 GFACT(J)=SQRT(GG)/R0*1.E5
      END
      subroutine jbtravel(delta,Depth,Tp,Ts)
       real h0(14)
      common /tbl/ip(110,14),is(110,14),secp(110,14),secs(110,14)
       data h0/0,33,97,160,224,288,352,415,479,543,606,670,734,798/
       Tp=0.
       Ts=0.
       if(delta.lt.10..or.delta.gt.103..or.depth.gt.h0(14)) return
       idel=delta
       do 1 i=1,14
1      if(depth.lt.h0(i)) goto 2
2      idep=i-1
      Tp1=ip(idel,idep)*60+secp(idel,idep)
     #    +(depth-h0(idep))/(h0(idep+1)-h0(idep)) *((ip(idel,idep+1)
     #    -ip(idel,idep))*60+secp(idel,idep+1)-secp(idel,idep))
       Tp2=ip(idel+1,idep)*60+secp(idel+1,idep)
     #    +(depth-h0(idep))/(h0(idep+1)-h0(idep)) *((ip(idel+1,idep+1)
     #    -ip(idel+1,idep))*60+secp(idel+1,idep+1)-secp(idel+1,idep))
       Tp=Tp1+(delta-idel)*(Tp2-Tp1)
       Ts1=is(idel,idep)*60+secs(idel,idep)
     #    +(depth-h0(idep))/(h0(idep+1)-h0(idep)) *((is(idel,idep+1)
     #    -is(idel,idep))*60+secs(idel,idep+1)-secs(idel,idep))
       Ts2=is(idel+1,idep)*60+secs(idel+1,idep)
     #    +(depth-h0(idep))/(h0(idep+1)-h0(idep)) *((is(idel+1,idep+1)
     #    -is(idel+1,idep))*60+secs(idel+1,idep+1)-secs(idel+1,idep))
       Ts=Ts1+(delta-idel)*(Ts2-Ts1)
       end
       subroutine calen(nyr,nday,mon,nd)
       integer month(12)
       data month/31,28,31,30,31,30,31,31,30,31,30,31/
       month(2)=28
       i=nyr/4-nyr/100+nyr/400-((nyr-1)/4-(nyr-1)/100+(nyr-1)/400)
       month(2)=month(2)+i
       nd0=nday
       do 1 m=1,12
       nd=nd0-month(m)
       if(nd.le.0) goto 10
        nd0=nd
1      continue
         print *, ' Error'
10     continue
        mon=m
        nd=nd0
       end
