      INTEGER FUNCTION GZSBPH ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get link to bank SBPH (hanging under SSAM)
C-
C-   Returned value  : Link to bank SBPH
C-   Inputs  : None
C-   Outputs : None
C-   Controls: 
C-
C-   Created  21-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSBPH.LINK'
      INTEGER LSSAM,GZSSAM
C----------------------------------------------------------------------
C
      GZSBPH = 0
C
      LSSAM = GZSSAM()
      IF ( LSSAM.GT.0 ) THEN
        GZSBPH=LC(LSSAM-IZSBPH)
      ENDIF
C
  999 RETURN
      END
