**==============================================
**     << plotw >>
**     plots observed waveforms
**  Options:
**   ishift= 0 <== neglect the specified delay
**   norm  = 1 <== common scale for amplitude
**============ Ver.030705 ======================
       PARAMETER (ND0=1024)
       CHARACTER DSN*40,name*12,phas(4)*2,comp(3)*2,Inf*30
       REAL  x(nd0),y(nd0)
       data phas/'P ','SV','SH','PP'/
       data comp/'UD','NS','EW'/
      OPEN(5,FILE='i_plotw')
      READ(5,'(a)') Inf
      READ(5,*) spcm,amp,ishift,tlen,line,pw,dy,ny,norm
      if(norm.eq.1) read(5,*) camp
      close(5)
      open(1,file=Inf)
      READ(1,'(A40)') DSN
      open(8,file='plot_w.ps')
      call plots(8)
          xshift=3.
          xlen=tlen/spcm
          call penw(1.0)
	  call symbol(3.,25.,.5,dsn,0.,40)
	  call plot(4.,23.,-3)
       call axs(0,xlen,1.,0.,spcm)
C      call symbol(xlen*.5-1.,.2,.3,'Time,sec',0.,8)
       call plot(0.,-dy,-3)
          call penw(pw)
	   j=0
100   READ(1,'(A12)',END=99) name
      READ(1,*) AZ,Dum,Dum,Dum,Dum,IX0
      READ(1,*) IM,IB,IC
      IF(IM.EQ.1) THEN
* < GDSN Instrument: DS converges micron to count >
           READ(1,*) IZP,IZZ,IP
           READ(1,*) DS
         DO 2 K=1,IZP+IZZ
2          READ(1,*)
      ELSE
* < WWSSN Instrument: DS is the magnification >
           READ(1,*) DS
           ds=ds/1.e+4
      ENDIF
* < Equalize magnification >
       READ(1,*) XM,INTVL,DT
      READ(1,*) (Y(I),I=1,INTVL)
* Remove record outside the time-window
      IF(IX0.GT.INTVL) GOTO 100
	j=j+1
      if(ishift.eq.0) ix0=0
       num=intvl-ix0
       nn=tlen/dt+.5
*      if(ishift.ne.0) num=min(num,nn)
       num=min(num,nn)
       as1=0.
       as2=0.
      DO 3 I=1,num
           X(I)=0.
           I1=IX0+I
      IF(I1.GT.0.AND.I1.LE.INTVL)  X(I)=Y(I1)*XM/ds
        as1=max(x(i),as1)
3       as2=min(x(i),as2)
        as=as1-as2

	call symbol(-2.5,.5,.28,name,.0,12)
	call symbol(-1.7,.0,.28,phas(ib),0.,2)
	if(ib.ne.3) call symbol(-0.9,.0,.28,comp(ic),0.,2)
          if(norm.eq.1) then
                    amp0=amp*as/camp*2
          else
                    amp0=amp*2
          endif
	  call plotw0(x,num,dt/spcm,as,0,0,amp0)
           if(line.ne.0) then
             call penw(0.8)
             call plot(0.,0.,3)
             call plot(num*dt/spcm,0.,2)
             call penw(pw)
           endif
         if(norm.ne.2) call number(-1.7,1.0,.25,as,0.,2)
         if(az.lt.0.) az=az+360.
          call number(-1.7,-.5,.28,az,.0,1)
           call plot(0.,-dy,-3)
      if(mod(j,ny).ne.0) goto 100
         xshift=xshift+xlen+3.
          if(xshift+xlen.gt.20.0) then
                call newpage(8)
                xshift=3.
          call penw(1.)
	  call symbol(3.,25.,.5,dsn,0.,40)
           call plot(3.,23.,-3)
          else
           call plot(xlen+3.,dy*(ny+1),-3)
          endif
          call penw(1.)
       call axs(0,xlen,1.,0.,spcm)
       call plot(0.,-dy,-3)
          call penw(pw)
	   goto 100
99	  continue
          call plote(8)
	  close(8)
	  close(1)
	  end
