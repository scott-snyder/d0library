      SUBROUTINE BKVRHS(LAYER,LVRHS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK WORK BANK FOR VRHT -- STORE HITS IN SECTOR
C-               BOOK WITH DEFAULT SIZE = 10 HITS/WIRE
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-JUN-1994   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C I/O:
      INTEGER LVRHS,LAYER
C Locals:
      LOGICAL FIRST
      INTEGER LVRHT,NIO,WORDS_PER_HIT,DEFAULT_HITS,ND,NHEAD,LINK
C Externals:
      INTEGER GZVRHT
c Data:
      DATA FIRST/.TRUE./
      DATA NHEAD/21/
      DATA DEFAULT_HITS/10/
C----------------------------------------------------------------------
      LVRHT = GZVRHT()
      IF (LVRHT .EQ. 0) CALL ERRMSG('No VRHT bank!','BKVRHS',' ','F')
      IF (LQ(LVRHT-2) .GT. 0) CALL ERRMSG(
     &  'Two VRHS banks already exist','BKVRHS','Check read width','F')
      WORDS_PER_HIT = IQ(LVRHT+6)
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL MZFORM('VRHS','21I / 5F 1B',NIO)
      ENDIF
      ND = NHEAD + 8*WORDS_PER_HIT*DEFAULT_HITS
      LINK = 1
      IF (LQ(LVRHT-LINK) .GT. 0) LINK = 2
      CALL MZBOOK(IXMAIN,LVRHS,LVRHT,-LINK,'VRHS',0,0,ND,NIO,0)
      IQ(LVRHS-5) = LAYER
      IQ(LVRHS+1) = 0
      IQ(LVRHS+2) = NHEAD
      IQ(LVRHS+3) = WORDS_PER_HIT
  999 RETURN
      END
