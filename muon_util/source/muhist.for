      SUBROUTINE MUHIST
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    HISTOGRAMS FROM MUD1 - RAW MUON DATA
CC
CC    DH 4/87
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUD1.LINK'
      INTEGER LRAW,NMUONH,I,J,K     
      REAL FLOAT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        LRAW=LQ(LHEAD-IZMUD1)
        IF(LRAW.EQ.0) THEN
          WRITE(6,100)
 100      FORMAT(' NO MUON RAW DATA BANK')
          RETURN
        ENDIF
C
      NMUONH=IQ(LRAW-1)/9       ! 9 WORDS/HIT
      CALL HFILL(401,FLOAT(NMUONH),0.,1.)
      DO 10 I=1,NMUONH
      K=9*(I-1)+LRAW
      CALL HFILL(402,FLOAT(IQ(K+2)),0.,1.)         ! RAW PADS
      CALL HFILL(402,FLOAT(IQ(K+3)),0.,1.)
      CALL HFILL(402,FLOAT(IQ(K+6)),0.,1.)
      CALL HFILL(402,FLOAT(IQ(K+7)),0.,1.)
      CALL HFILL(403,FLOAT(IQ(K+4)),0.,1.)         ! RAW TIMES
      CALL HFILL(403,FLOAT(IQ(K+5)),0.,1.)
      CALL HFILL(404,FLOAT(IQ(K+8)),0.,1.)         ! RAW DELTA TIMES
      CALL HFILL(404,FLOAT(IQ(K+9)),0.,1.)
 10   CONTINUE
      RETURN
      END