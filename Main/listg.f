**================================
**     << listg >>
**      lists the content of Green's functions
**      in fort.3 (binary)
**================================
      PARAMETER (ND0=1024,NM0=6)
      CHARACTER NAME*40,NAM*12
      DIMENSION W(ND0,NM0,10)
      read(3) NAME,VP,VS,DT,H0,DK,DIP,NK,K0,NT
      write(6,'(a40/6f7.2,3i5)')  NAME,VP,VS,DT,H0,DK,DIP,NK,K0,NT
100       read(3,end=99) NAM
       do 1 k=1,nk
       do 1 n=1,nm0
1       read(3)(W(I,N,K),I=1,NT)
       WRITE(*,'(a12)') NAM
       GOTO 100
99    CONTINUE
      END
