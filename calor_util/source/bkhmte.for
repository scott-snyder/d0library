      SUBROUTINE BKHMTE(LPELC,LHMTE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank HMTE
C-
C-   Inputs  : LPELC = Link of parent bank.
C-                      = 0, will find it for you.
C-   Outputs : Link of Booked HMTE Bank
C-   Controls: None
C-
C-   Created  25-SEP-1990 12:17:52.73  Norman A. Graf
C-   Updated  20-FEB-1994   Meenakshi Narain  
C-                          declare word 16 and 17 to be integers
C-                          until now 16-20 were real
C-   Updated  27-JAN-1995   Steven M. Glenn  declared word 18 integer
C-   Updated  28-FEB-1995   Meenakshi Narain  increase bank size by 4 words
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LHMTE
      INTEGER LPELC
      INTEGER IXIO
      INTEGER GZPELC
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZHMTE.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LHMTE = 0
      IF(FIRST)THEN
C
        CALL MZFORM('HMTE','4I 11F 3I 5F 1I',IXIO)    ! Describe Bank format
C
        FIRST = .FALSE.
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LPELC.EQ.0 ) THEN
        LPELC = GZPELC()
      ENDIF
C
      CALL MZBOOK
     &  (IXMAIN,LHMTE,LPELC,-IZHMTE,'HMTE',1,1,24,IXIO,0)
C
  999 RETURN
      END
