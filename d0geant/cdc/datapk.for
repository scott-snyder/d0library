      SUBROUTINE DATAPK(NDATA,IDATA,NBIT,IOUT)
C      ================================================
C
C      Dynamic data packing routine
C      Put data (or + address) into 32 bit word
C       Input :
C                NDATA ---  Number of data
C                IDATA ---  Array of data
C                NBIT  ---  Array of bits to pack data
C       Output : IOUT
C
C       K. Ng,  S. Linn,      Aug. 9, 1986
C      ================================================
      INCLUDE 'D0$INC:GCUNIT.INC'
      INTEGER NDATA,IDATA(*),NBIT(*),IOUT,NTOT,MAX(10)
      INTEGER I
C
      DO 100 I=1,NDATA
        MAX(I)=2**NBIT(I)-1
        IF (IDATA(I) .GT. MAX(I)) THEN
          WRITE(LOUT,*)' PACKING PROBLEM: MAX=',MAX(I),'VALUE=',IDATA(I)
          IOUT=0
          RETURN
        ENDIF
  100 CONTINUE
      NTOT=NBIT(1)+1
      IOUT=IDATA(1)
      DO 200 I=2,NDATA
        CALL CBYT(IDATA(I),1,IOUT,NTOT,NBIT(I))
        NTOT=NTOT+NBIT(I)
  200 CONTINUE
      END
