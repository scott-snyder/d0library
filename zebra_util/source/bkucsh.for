      SUBROUTINE BKUCSH(NCELLS,LUCSH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank UCSH.
C-
C-   Inputs  : NCELLS [I] Number of cells (*NOT* number of data entries)
C-   Outputs : LUCSH  [I] Address of booked UCSH bank.
C-   Controls: None
C-
C-   Created   3-DEC-1993  Ian Adam 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NCELLS,ND,NL,NS,IXIO
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUCSH.LINK'
      INTEGER LUCSH
      INTEGER LANLS,GZANLS
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LUCSH = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('UCSH','2I/1I1F',IXIO)        ! Describe Bank format
      ENDIF
C
      LANLS = GZANLS()
      IF ( LANLS .LE. 0 ) THEN
        CALL BKANLS(LANLS)
      ENDIF
C
C--   BOOK BANK
C
      NL = 0
      NS = 0
      ND=2*NCELLS+2
      CALL MZBOOK(IXMAIN,LUCSH,LANLS,-IZUCSH,'UCSH',NL,NS,ND,IXIO,0)
C
  999 RETURN
      END
