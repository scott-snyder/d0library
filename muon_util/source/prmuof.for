      SUBROUTINE PRMUOF(PRUNIT,LMUOF,NMUOF,CFL,IFL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    PRINT OUT MUOF - MUON HIT FLAGS
CC
CC    PRUNIT - UNIT NUMBER FOR PRINTOUT
CC    LMUOF - BANK ADDRESS
CC    NMUOF - BANK NUMBER
CC    CFL - FLAG TO CONTROL PRINT OUT
CC    IFL - HOW MUCH TO PRINT
CC
CC    THERE WILL ONLY BE A SINGLE RAW MUON BANK
CC    SO IGNORE CFL
CC
CC    DH MARCH 1986 
CC    DH 4/87 PLANES COUNT 0-3
CC    DH 7/89 NEW FORMAT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,LMUOF,NMUOF,IFL,LFLAG,LHIT
      CHARACTER CFL*(*)
      INTEGER LZLOC,NMOD,I,J,K,GZMUOF
CCCCCCCCCkACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      LFLAG=LMUOF
      IF(LFLAG.EQ.0) THEN
 100      FORMAT(' NO MUOF BANK')
      LFLAG=GZMUOF(0)   
        IF(LFLAG.EQ.0) THEN
          WRITE(PRUNIT,100)
          RETURN
        ENDIF
      ENDIF
C
      NMOD=IQ(LFLAG-1)/10       ! 10 WORDS/MODULE
      WRITE(PRUNIT,101)
101   FORMAT('0',10X,' BANK MUOF: MUON HIT FLAGS '//
     A ' MODULE  NO. RAW  NO. PROCESSED  PLANE 0   1   2   3'/)
      DO 10 I=1,NMOD
      K=10*(I-1)+LFLAG
      WRITE(PRUNIT,102) (IQ(K+J),J=1,9)
 102  FORMAT(I5,4I5,9X,4I5)
 10   CONTINUE
      RETURN
      END
