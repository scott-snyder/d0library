      INTEGER FUNCTION GZSMAH ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get link to bank SMAH (hanging under SSAM)
C-
C-   Returned value  : Link to bank SMAH
C-   Inputs  : None
C-   Outputs : None
C-   Controls: 
C-
C-   Created  22-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSMAH.LINK'
      INTEGER LSSAM,GZSSAM
C----------------------------------------------------------------------
C
      GZSMAH = 0
C
      LSSAM = GZSSAM()
      IF ( LSSAM.GT.0 ) THEN
        GZSMAH=LC(LSSAM-IZSMAH)
      ENDIF
C
  999 RETURN
      END
