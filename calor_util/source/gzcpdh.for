      INTEGER FUNCTION GZCPDH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find link to Zebra Bank CPDH
C-
C-   Returned value  : Link to bank
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1990   W.G.D. DHARMARATNA
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPDH.LINK'
      INTEGER GZSCAL
C----------------------------------------------------------------------
      GZCPDH = 0
      LSCAL = GZSCAL('STPC')
      IF ( LSCAL.GT.0 ) THEN
        GZCPDH = LC(LSCAL-IZCPDH)
      ENDIF
  999 RETURN
      END
