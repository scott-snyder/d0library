      SUBROUTINE BKUCWX(NCELLS,LUCWX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank UCWX.
C-
C-   Inputs  : NCELLS [I] Number of cells (*NOT* number of data entries)
C-   Outputs : LUCWX  [I] Address of booked UCWX bank.
C-   Controls: None
C-
C-   Created   3-DEC-1993  Ian Adam 
C-   Updated  14-OCT-1995   Ian Adam UCSH replaced by UCWX  
C---------------------------------------------------------------------- 
      IMPLICIT NONE
      INTEGER  NCELLS,ND,NL,NS,IXIO
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUCWX.LINK'
      INTEGER LUCWX
      INTEGER LANLS,GZANLS
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LUCWX = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('UCWX','2I/1I1F',IXIO)        ! Describe Bank format
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
      CALL MZBOOK(IXMAIN,LUCWX,LANLS,-IZUCWX,'UCWX',NL,NS,ND,IXIO,0)
C
  999 RETURN
      END
