      SUBROUTINE BKUTAG(ND,LUTAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank UTAG.
C-
C-   Inputs  : ND     [I] No. of Data words.
C-   Outputs : LUTAG  [I] Address of booked UTAG bank.
C-   Controls: None
C-
C-   Created  14-MAR-1993 00:20:40.59  Balamurali V.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUTAG,LANLS,GZANLS
      INTEGER ND,NL,NS,IXIO
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUTAG.LINK'
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LUTAG = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('UTAG','1I *I -H',IXIO)        ! Describe Bank format
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
      CALL MZBOOK(IXMAIN,LUTAG,LANLS,-IZUTAG,'UTAG',NL,NS,ND,IXIO,0)
C
  999 RETURN
      END
