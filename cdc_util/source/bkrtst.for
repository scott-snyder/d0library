      SUBROUTINE BKRTST(NTDATA, LRTST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank RTST.
C-
C-   Inputs  : NTDATA [I] No. of data words in the bank
C-   
C-   Outputs : LRTST  [I] Address of booked RTST bank.
C-   Controls: None
C-
C-   Created  23-OCT-1995 09:20:30.95  Jadwiga Warchol
C-   Modified 15-NOV-1995 Jadwiga Warchol, RTST hangs now from UPGD to
C-            accomodate preshower test
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUPGD
      INTEGER LRTST
C----------------------------------------------------------------------
      INTEGER ND,NTDATA,NL,NS,IXIO
      INTEGER GZUPGD
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZRTST.LINK'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LRTST = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('RTST','3I-B',IXIO)        ! Describe Bank format
      ENDIF
C
C --  Book UPGD bank, if needed
C
      LUPGD = GZUPGD()
      IF (LUPGD.LE.0) CALL BKUPGD(LUPGD)
      IF (LUPGD.LE.0) GOTO 999
C
      NL = 0
      NS = 0
      ND = NTDATA
      CALL MZBOOK(IXMAIN,LRTST,LUPGD,-IZRTST,'RTST',NL,NS,ND,IXIO,0)
C
  999 RETURN
      END
