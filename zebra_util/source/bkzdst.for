      SUBROUTINE BKZDST(ND, LZDST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank ZDST.
C-
C-   Inputs  : ND    [I] Number of Data words
C-   Outputs : LZDST [I] Address of booked ZDST bank.
C-   Controls: None
C-
C-   Created  28-Sep-1994 Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ND,NL,NS,IXIO
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZZDST.LINK'
      INTEGER LZDST
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LZDST = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('ZDST','-I',IXIO)
      ENDIF
      IF(LHEAD.EQ.0)GO TO 999
C
C--   BOOK BANK
C
      NL = 0
      NS = 0
      CALL MZBOOK(IXMAIN,LZDST,LHEAD,-IZZDST,'ZDST',NL,NS,ND,IXIO,-1)
C
  999 RETURN
      END
