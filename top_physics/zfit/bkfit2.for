      SUBROUTINE BKFIT2(LPROC,LFIT2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank FIT2.
C-
C-   Inputs  : LPROC  [I] Address of the parent bank.
C-                          = 0, will find it for you.
C-   Outputs : LFIT2  [I] Address of booked FIT2 bank.
C-   Controls: None
C-
C-   Created   3-SEP-1993 23:29:14.64  Pushpa C. Bhat
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LPROC
      INTEGER LFIT2
C----------------------------------------------------------------------
      INTEGER ND,NL,NS,IXIO
      INTEGER GZPROC
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFIT2.LINK'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LFIT2 = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('FIT2','3I 47F',IXIO)        ! Describe Bank format
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LPROC .LE. 0 ) THEN
        LPROC = GZPROC()
      ENDIF
      IF ( LPROC .LE. 0 ) THEN
        GOTO 999
      ENDIF
C
      NL = 3
      NS = 1
      ND = 50
      CALL MZBOOK(IXMAIN,LFIT2,LPROC,-IZFIT2,'FIT2',NL,NS,ND,IXIO,0)
C
C ****  Book a stand-alone bank
C
C      CALL MZBOOK(IXMAIN,LFIT2,0,2,'FIT2',NL,NS,ND,IXIO,0)
  999 RETURN
      END
