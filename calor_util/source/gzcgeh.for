      INTEGER FUNCTION GZCGEH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find link to Zebra Bank CGEH
C-
C-   Returned value  : Link to bank
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   2-MAY-1989   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGEH.LINK'
      INTEGER GZSCAL
C----------------------------------------------------------------------
      GZCGEH = 0
      LSCAL = GZSCAL('STPC')
      IF ( LSCAL.GT.0 ) THEN
        GZCGEH = LC(LSCAL-IZCGEH)
      ENDIF
  999 RETURN
      END
