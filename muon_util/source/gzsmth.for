C+
      INTEGER FUNCTION GZSMTH ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get link to bank SMTH (hanging under SSAM)
C-
C-   Returned value  : Link to bank SMTH
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None.
C-
C-   Created  3-AUG-1992   Oleg Eroshin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSMTH.LINK'
      INTEGER LSSAM,GZSSAM
C
      GZSMTH = 0
      LSSAM = GZSSAM()
      IF (LSSAM .GT. 0) THEN
        GZSMTH = LC(LSSAM-IZSMTH)
      END IF
C
  999 RETURN
      END
