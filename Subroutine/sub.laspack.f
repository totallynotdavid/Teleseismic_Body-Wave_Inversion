c     LASPAC    - low level plotting routines
c        written by Kohketsu originally
c     1) modified -  91/04/12  by Kikuchi
c     2) modified for write-format (f7.1,x) -  93/03/26  by Kikuchi

      subroutine ASPECT(RASP)
C        sets width to height ratio for characters to RASP
      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      COMMON/L00000/PSCA,xo,yo
      ASP = RASP
      return
      end

      subroutine PLUS(x,y)
      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage
      COMMON/L00000/PSCA,xo,yo
      xp=PSCA*x+xorig
      yp=PSCA*y+yorig
      write(LPLOT,'(f7.1,x,f7.1,a)') xp,yp,' PLUS'
      end

      subroutine TRI(x,y)
      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage
      COMMON/L00000/PSCA,xo,yo
      xp=PSCA*x+xorig
      yp=PSCA*y+yorig
      write(LPLOT,'(f7.1,x,f7.1,a)') xp,yp,' TRI'
      end

      subroutine SQR(x,y)
      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage
      COMMON/L00000/PSCA,xo,yo
      xp=PSCA*x+xorig
      yp=PSCA*y+yorig
      write(LPLOT,'(f7.1,x,f7.1,a)') xp,yp,' SQR'
      end

      subroutine ARC(x,y,RADIUS,ang1,ang2)
      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage
      COMMON/L00000/PSCA,xo,yo
      xp=PSCA*x+xorig
      yp=PSCA*y+yorig
      rp=PSCA*radius
      write(LPLOT,'(5(f7.1,x),a)') xp,yp,rp,ang1,ang2,' arc'
      end
      subroutine ARCN(x,y,RADIUS,ang1,ang2)
      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage
      COMMON/L00000/PSCA,xo,yo
      xp=PSCA*x+xorig
      yp=PSCA*y+yorig
      rp=PSCA*radius
      write(LPLOT,'(5(f7.1,x),a)') xp,yp,rp,ang1,ang2,' arcn'
      end
      subroutine CIRC1(x,y,RADIUS)
      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage
      COMMON/L00000/PSCA,xo,yo
      xp=PSCA*x+xorig
      yp=PSCA*y+yorig
      rp=PSCA*radius
      write(LPLOT,'(3(f7.1,x),a)') xp,yp,rp,' AR'
      end
      subroutine cCIRC1(x,y,RADIUS)
      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage
      COMMON/L00000/PSCA,xo,yo
      xp=PSCA*x+xorig
      yp=PSCA*y+yorig
      rp=PSCA*radius
      write(LPLOT,'(3(f7.1,x),a)') xp,yp,rp,' cAR'
      end

      subroutine NUMBER(X,Y,SIZE,RN,ANGL,NSF)
      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      COMMON/L00000/PSCA,xo,yo
       call xnumber(x,y,size,rn,angl,nsf,5)
      END

      subroutine xNUMBER(X,Y,SIZE,RN,ANGL,NSF, ifont)
C       modified on 90/10/20 by Kikuchi

      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      COMMON/L00000/PSCA,xo,yo
      CHARACTER*8 IFORM
      CHARACTER*40 IWORD
      if(rn.eq.0.) then
        call xsymbol(x,y,size,'0',angl,1,ifont)
         return
      endif
        iorder=log10(abs(rn))+1.
         if(iorder.le.0) iorder=1
        if(nsf.lt.0) goto 20
        idl=iorder+nsf+2
      Write(IFORM,55) idl,NSF
   55 FORMAT('(f',i2,'.',I1,')')
      Write(IWORD,IFORM)RN
      GO TO 30

C    for integer format

   20  if(rn.lt.0.) iorder=iorder+1
       write(iform,56) iorder
   56  format('(i',i2,')')
       Write(IWORD,iform)IFIX(RN)
       idl=iorder

   30 CALL xSYMBOL(X,Y,SIZE,IWORD,ANGL,idl, ifont)
      END

      subroutine xPLOT(X,Y,I)
C
C     Raises (I=3) or lowers (I=2) pen and moves to coordinates
C      (X,Y) if I>0 or to current position plus (X,Y) if I<0
C
      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage
      COMMON/L00000/PSCA,xo,yo
      DATA IUP/1/
      II=IABS(I)
C
C     Rotate plot by 90 degrees if necessary
C
      XP=X
      YP=Y
      XV= PSCA*XP+xorig
      YV= PSCA*YP+yorig
C
C     plot
C
      if(I.eq.2)  write(LPLOT,10) xv,yv,' pL'
      if(I.eq.3)  write(LPLOT,10) xv,yv,' pM'
      if(I.eq.-2) write(LPLOT,10) xv,yv,' rL'
      if(I.eq.-3) write(LPLOT,10) xv,yv,' rM'
10    format(f7.1,1x,f7.1,a)
c 
c      if(I.gt.0) then
       xo = x   
       yo = y
c      endif
      RETURN
      END
C-----------------------------------------------------------gh--bk
       subroutine ORIGIN(x,y,iorig)
c
      common/p00001/ xorig,yorig,ipage
      common/l00000/psca,xo,yo
c
      if(iorig.eq.0) then
         xorig=x*psca
         yorig=y*psca
      else if(iorig.gt.0) then
         xorig=xorig+x*psca
         yorig=yorig+y*psca
      else if(iorig.lt.0) then
         if(psca.eq.0) stop 'ORIGIN error: zero scale'
         x=xorig/psca
         y=yorig/psca
      endif
      return
      end

      subroutine PLOTS(LPL)
      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage
      COMMON/L00000/PSCA,xo,yo
C
      A=1.0
      B=0.0
      C=1.0
      D=0.0
      asp = 0.6666
      LPLOT=LPL
      xorig=0.
      yorig=0.
C
c     Postcript initialisation
      write(LPLOT,'(a)') '%!'
      write(LPLOT,'(a)') '%%Page:1'
        ipage=1
      write(LPLOT,'(a)') 'initmatrix'
      write(LPLOT,'(a)') '/pM {stroke newpath moveto} def'
      write(LPLOT,'(a)') '/pL {lineto} def'
      write(LPLOT,'(a)') '/rM {rmoveto} def'
      write(LPLOT,'(a)') '/rL {rlineto} def'
      write(LPLOT,'(a)') '/AR {stroke newpath',
     -               ' 0 360 arc closepath stroke} def'
      write(LPLOT,'(a)') '/cAR {stroke newpath',
     -               ' 0 360 arc closepath fill} def'
      write(LPLOT,'(a)') '/PLUS {stroke newpath moveto'
      write(LPLOT,'(a)') '   -0.5 0 rlineto 1 0 rlineto'
      write(LPLOT,'(a)') '   -0.5 -0.5 rmoveto 0 1 rlineto} def'
      write(LPLOT,'(a)') '/SQR {stroke newpath moveto'
      write(LPLOT,'(a)') '   -4 -4 rmoveto 0 8 rlineto 8 0 rlineto'
      write(LPLOT,'(a)') '0 -8 rlineto -8 0 rlineto closepath fill} def'
      write(LPLOT,'(a)') '/TRI {stroke newpath moveto'
      write(LPLOT,'(a)') '   -4 -4 rmoveto 4 8 rlineto'
      write(LPLOT,'(a)') '4 -8 rlineto -8 0 rlineto closepath fill} def'
      write(LPLOT,'(a)') '1 setlinejoin'
*     write(LPLOT,'(a)') '/pR {/Palatino-Roman findfont '
*     write(LPLOT,'(a)') ' exch scalefont setfont} def'
*     write(LPLOT,'(a)') '/pI {/Palatino-Italic findfont '
*     write(LPLOT,'(a)') ' exch scalefont setfont} def'
*     write(LPLOT,'(a)') '/pB {/Palatino-Bold findfont '
*     write(LPLOT,'(a)') ' exch scalefont setfont} def'
*     write(LPLOT,'(a)') '/pBI {/Palatino-BoldItalic findfont '
*     write(LPLOT,'(a)') ' exch scalefont setfont} def'
      write(LPLOT,'(a)') '/hV {/Helvetica-Bold findfont '
      write(LPLOT,'(a)') ' exch scalefont setfont} def'
      write(LPLOT,'(a)') 'newpath '
      write(LPLOT,'(a)') '0.8 setlinewidth'
c
        PSCA = 72.0/2.54
      END

      subroutine PENW(pw)
      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      write(LPLOT,'(a,f7.2,a)') 'stroke',pw,' setlinewidth'
      end
 
      subroutine NEWPAGE(LPL)
 
      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage
C
      A=1.0
      B=0.0
      C=1.0
      D=0.0
      asp = 0.6666
      LPLOT=LPL
      xorig=0.
      yorig=0.
      write(LPLOT,'(a)') 'stroke'
      write(LPLOT,'(a)') 'showpage'
      ipage=ipage+1
      write(LPLOT,'(a,i5)') '%%Page:',ipage
      write(LPLOT,'(a)') '0.8 setlinewidth'
      write(LPLOT,'(a)') '1 setlinejoin'
      end

      subroutine PLOTE(LPL)
      common/p00001/ xorig,yorig,ipage
C
      write(LPL,'(a)') 'stroke'
      write(LPL,'(a)') 'showpage'
      end

      subroutine SYMBOL(X,Y,SIZE,IWORD,ANGL,NCHAR)
      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      COMMON/L00000/PSCA,xo,yo
      CHARACTER*80 IWORD
       call xsymbol(x,y,size,iword,angl,nchar,5)
      end

      subroutine xSYMBOL(X,Y,SIZE,IWORD,ANGL,NCHAR, ifont)
C
C     writes a Hollerith string on the plot--plotter unit
C     ifont = 1(Palatino Roman), 2(Bold), 3(Italic), 4(BoldItalic)
C
      COMMON/P00000/LPLOT,A,B,C,D,ASP,THET
      COMMON/L00000/PSCA,xo,yo
      CHARACTER*80 IWORD
      CHARACTER*80 CWORD
      nch = nchar
      IF(NCHAR.GT.80)NCH=80
C
C     select character orientation
C
      ab=0.0
C 04.08.07  
C      if(irot.ne.0) ab=90.0

C
C     select character size
C
   40 SZ=ABS(SIZE)*PSCA*1.5
      do 50 k=1,80
        cword(k:k) = iword(k:k)
   50 continue
      do 51 k=nch+1,80
        cword(k:k) = ' '
   51 continue
C
C      move pen to symbol location
C
      IP=3
      IF(SIZE.LT.0.0)IP=-3
      CALL PLOT(X,Y,IP)
      ang=angl+ab
      write(LPLOT,fmt='(f7.1,a)') ang,' rotate'
C
c     write character string
C
	if(ifont .eq. 1) then
		write(LPLOT,fmt='(f7.1,x,a)') sz,' pR'
	else if(ifont .eq. 2) then
		write(LPLOT,fmt='(f7.1,x,a)') sz,' pB'
	else if(ifont .eq. 3) then
		write(LPLOT,fmt='(f7.1,x,a)') sz,' pI'
	else if(ifont .eq. 4) then
		write(LPLOT,fmt='(f7.1,x,a)') sz,' pBI'
	else
		write(LPLOT,fmt='(f7.1,x,a)') sz,' hV'
	endif
      write(LPLOT,fmt='(x,a,a,a,/,a)') '(',cword,')',' show'
C
C     reset character orientation if necessary
C
      bng = -ang 
      write(LPLOT,fmt='(f7.1,a)') bng,' rotate'
10    RETURN
      END

      subroutine plot(x,y,ipen)
c
      common/p00001/ xorig,yorig,ipage
      common/a00000/ psca,ixo,iyo,iox,ioy
c
      if(ipen.eq.999) then
         call plots(8)
      else if(ipen.lt.0) then
         call origin(x,y,1)
         iqen=-ipen
         call xplot(0.,0.,iqen)
      else 
         iqen=ipen
         call xplot(x,y,iqen)
         endif
      return
      end
