      INTEGER FUNCTION GZSSTH ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get link to bank SSTH (hanging under SSAM)
C-
C-   Returned value  : Link to bank SSTH
C-   Inputs  : None
C-   Outputs : None
C-   Controls: 
C-
C-   Created  30-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSSTH.LINK'
      INTEGER LSSAM,GZSSAM
C----------------------------------------------------------------------
C
      GZSSTH = 0
C
      LSSAM = GZSSAM()
      IF ( LSSAM.GT.0 ) THEN
        GZSSTH=LC(LSSAM-IZSSTH)
      ENDIF
C
  999 RETURN
      END
