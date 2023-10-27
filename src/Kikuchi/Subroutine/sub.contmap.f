      subroutine contMap(a,IC,Amax,Ia,NX,NY,lX,lY,Wx,Wy,NC)
* < Contour map >
      DIMENSION a(NX,NY),Ia(NX,NY)
      IF(IC.NE.0) GOTO 112
      Amax=0.
      DO 111 L=1,lY
      DO 111  I=1,lX
111        Amax=MAX(a(I,L),Amax)
112     AMIN=AMAX/10
        DZ=(AMAX-AMIN)/NC
C     Check arguments
      IF(LX.GT.NX. or. Ly.gt.Ny.or.Dz.eq.0.) then
         WRITE(6,*)
     -   'ERROR: Lx.GT.Nx OR Ly.GT.Ny OR Dz.EQ.0'
       return
      endif

      MX=LX-1
      MY=LY-1
        Hori=Wx*Mx
        Vert=Wy*My
      PO=AMIN-DZ
   12 PO=PO+DZ

      IF(PO.GT.AMAX) GO TO 60

C     GENERATION OF TABLE -IA-
      DO 13 IY=1,LY
      DO 13 IX=1,LX
13       IA(IX,IY)=0
      DO 15 IY=1,LY
      LOWO=-1
      IF(A(1,IY).GE.PO) LOWO=1
      DO 15 IX=1,MX
      LOWN=-1
      IF(A(IX+1,IY).GE.PO) LOWN=1
      IF(LOWN.EQ.LOWO) GO TO 15
      IA(IX,IY)=IA(IX,IY)+1
      LOWO=LOWN
   15 CONTINUE
      DO 17 IX=1,LX
      LOWO=-1
      IF(A(IX,1).GE.PO) LOWO=1
      DO 17 IY=1,MY
      LOWN=-1
      IF(A(IX,IY+1).GE.PO) LOWN=1
      IF(LOWN.EQ.LOWO) GO TO 17
      IA(IX,IY)=IA(IX,IY)+2
      LOWO=LOWN
   17 CONTINUE

C     BEGIN PLOTTING
C     SEARCH STARTING POINT
C     CONTOUR LINE STARTING FROM EDGE LINE
      IST=1
      IYS=1
      IXS=0
  310 IXS=IXS+1
      IHV=IA(IXS,IYS)
      IF((IHV.NE.1).AND.(IHV.NE.3)) GO TO 315
      NIY=1
      MIY=0
      IHV=1
      IHM=1
      NIX=IXS
      MIX=IXS
      GO TO 360
  315 IF(IXS.LT.LX) GO TO 310
      IST=2
      IXS=LX
      IYS=0
  320 IYS=IYS+1
      IHV=IA(IXS,IYS)
      IF(IHV.LT.2) GO TO 325
      NIX=LX
      MIX=LX+1
      IHV=2
      IHM=2
      NIY=IYS
      MIY=IYS
      GO TO 360
  325 IF(IYS.LT.LY) GO TO 320
      IST=3
      IYS=LY
      IXS=LX+1
  330 IXS=IXS-1
      IHV=IA(IXS,IYS)
      IF((IHV.NE.1).AND.(IHV.NE.3)) GO TO 335
      NIY=LY
      MIY=LY+1
      IHV=1
      IHM=1
      NIX=IXS
      MIX=IXS
      GO TO 360
  335 IF(IXS.GT.1) GO TO 330
      IST=4
      IXS=1
      IYS=LY+1
  340 IYS=IYS-1
      IHV=IA(IXS,IYS)
      IF(IHV.LT.2) GO TO 345
      NIX=1
      MIX=0
      IHV=2
      IHM=2
      NIY=IYS
      MIY=IYS
      GO TO 360
  345 IF(IYS.GT.1) GO TO 340
C     CONTOUR LINE STARTING FROM INNER REGION
      IST=5
      IYS=0
  354 IYS=IYS+1
      IXS=0
  350 IXS=IXS+1
      IHV=IA(IXS,IYS)
      IF(IHV.LT.2) GO TO 352
      IHV=2
      IHM=2
      IX=IXS
      NIX=IXS
      MIX=IXS-1
      IY=IYS
      NIY=IYS
      MIY=IYS+1
      GO TO 20
  352 IF(IXS.LT.LX) GO TO 350
      IF(IYS.LT.LY) GO TO 354
C     CHANGE POTENTIAL
      GO TO 12
C     SET PEN TO STARTING POINT
  360 IX=IXS
      IY=IYS
      IA(IX,IY)=IA(IX,IY)-IHV
   20 GO TO (22,24),IHV
   22 DX=(PO-A(IX,IY))/(A(IX+1,IY)-A(IX,IY))
      DY=0.0
      GO TO 26
   24 DX=0.0
      DY=(PO-A(IX,IY))/(A(IX,IY+1)-A(IX,IY))
   26 XL=IX-1+DX
      YL=IY-1+DY

        num=1
        xl0=xl
        yl0=yl

      PX=WX*XL
      PY=WY*YL
      CALL PLOT(PX,PY,3)
C     SEARCH NEXT POINT
   30 GO TO (35,40),IHV
   35 IF(IY.LE.MIY) GO TO 100
      IF(IY.GE.LY) GO TO 1
      IF(IA(IX,IY).EQ.2) GO TO 105
      IF(MOD(IA(IX,IY+1),2).NE.0) GO TO 120
      GO TO 110
  105 IF(MOD(IA(IX,IY+1),2).EQ.0) GO TO 130
      AVA=A(IX,IY)+A(IX+1,IY)+A(IX,IY+1)+A(IX+1,IY+1)
      FUN=(0.25*AVA-PO)*(A(IX,IY)-PO)
      IF(FUN) 130,130,110
  110 IX=IX+1
      IHV=2
      GO TO 50
  120 IY=IY+1
      IHV=1
      GO TO 50
  130 IHV=2
      GO TO 50
  100 IF(IY.LE.1) GO TO 1
      IHV=IA(IX,IY-1)+1
      GO TO (160,150,140,135),IHV
  135 AVA=A(IX,IY)+A(IX+1,IY)+A(IX,IY-1)+A(IX+1,IY-1)
      FUN=(0.25*AVA-PO)*(A(IX,IY)-PO)
      IF(FUN) 140,140,160
  140 IY=IY-1
      IHV=2
      GO TO 50
  150 IY=IY-1
      IHV=1
      GO TO 50
  160 IX=IX+1
      IY=IY-1
      IHV=2
      GO TO 50
   40 IF(IX.LE.MIX) GO TO 200
      IF(IX.GE.LX) GO TO 1
      IF(IA(IX,IY).EQ.1) GO TO 205
      IF(IA(IX+1,IY).GE.2) GO TO 220
      GO TO 230
  205 IF(IA(IX+1,IY).LE.1) GO TO 210
      AVA=A(IX,IY)+A(IX+1,IY)+A(IX,IY+1)+A(IX+1,IY+1)
      FUN=(0.25*AVA-PO)*(A(IX,IY)-PO)
      IF(FUN) 210,210,230
  210 IHV=1
      GO TO 50
  220 IX=IX+1
      IHV=2
      GO TO 50
  230 IY=IY+1
      IHV=1
      GO TO 50
  200 IF(IX.LE.1) GO TO 1
      IHV=IA(IX-1,IY)+1
      GO TO (240,260,250,235),IHV
  235 AVA=A(IX,IY)+A(IX-1,IY)+A(IX,IY+1)+A(IX-1,IY+1)
      FUN=(0.25*AVA-PO)*(A(IX,IY)-PO)
      IF(FUN) 260,260,240
  240 IX=IX-1
      IY=IY+1
      IHV=1
      GO TO 50
  250 IX=IX-1
      IHV=2
      GO TO 50
  260 IX=IX-1
      IHV=1
C     DRAW CONTOUR LINE
   50 IA(IX,IY)=IA(IX,IY)-IHV
      MIX=NIX
      NIX=IX
      MIY=NIY
      NIY=IY
      GO TO (52,54),IHV
   52 DX=(PO-A(IX,IY))/(A(IX+1,IY)-A(IX,IY))
      DY=0.0
      GO TO 56
   54 DX=0.0
      DY=(PO-A(IX,IY))/(A(IX,IY+1)-A(IX,IY))
   56 XL=IX-1+DX
      YL=IY-1+DY
      PX=WX*XL
      PY=WY*YL

       num=num+1
       xl0=xl0+xl
       yl0=yl0+yl

      CALL PLOT(PX,PY,2)
      IF(IX.EQ.IXS.AND.IY.EQ.IYS.AND.IHV.EQ.IHM) GO TO 1
      GO TO 30
    1 CONTINUE
       iend=5
       if(ix.le.1)  iend=4
       if(iy.ge.ly) iend=3
       if(ix.ge.lx) iend=2
       if(iy.le.1)  iend=1
           ix0=xl0/num+1.5
           iy0=yl0/num+1.5
          hm=a(ix0,iy0)
C      if(Ist.eq.Iend.and.hm.ge.po) then
          Dens=(amax-po)/(amax-amin)*0.8+0.1
          write(8,'(a,f8.3,a)') 'gsave',Dens,
     -     ' setgray fill grestore stroke'
C      endif
C     CONTOUR LINE ENDED
C     SEARCH NEXT STARTING POINT
      GO TO (310,320,330,340,350),IST
   60 CONTINUE
      CALL PLOT(0.,0.,3)
      CALL PLOT(0.,Vert,2)
      CALL PLOT(Hori,Vert,2)
      CALL PLOT(Hori,0.,2)
      CALL PLOT(0.,0.,2)
      END
