      SUBROUTINE BKUDST(ND,LUDST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank UDST.
C-
C-   Inputs  : ND     [I] Number of Data words
C-   Outputs : LUDST  [I] Address of booked UDST bank.
C-   Controls: None
C-
C-   Created  14-MAR-1993 00:09:38.33  Balamurali V.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ND,NL,NS,IXIO
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUDST.LINK'
      INTEGER LUDST
      INTEGER LANLS,GZANLS
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LUDST = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('UDST','1I *I -F',IXIO)        ! Describe Bank format
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
      CALL MZBOOK(IXMAIN,LUDST,LANLS,-IZUDST,'UDST',NL,NS,ND,IXIO,0)
C
  999 RETURN
      END
