       SUBROUTINE ORDER(SEQ,N,NODR)
       DIMENSION SEQ(N),NODR(N),NN(1000)
       SM=SEQ(1)
      DO 1 I=1,N
        SM=MAX(SM,SEQ(I))
1       NN(I)=I
      DO 10 IJ=1,N
        AM=SM
        I0=N-IJ+1
       DO 2 I=1,I0
         IF(SEQ(NN(I)).GT.AM) GOTO 2
          AM=SEQ(NN(I))
          IS=I
2      CONTINUE
         NS=NN(IS)
         NN(IS)=NN(I0)
10        NODR(IJ)=NS
        END
