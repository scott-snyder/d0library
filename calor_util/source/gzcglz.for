      INTEGER FUNCTION GZCGLZ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CGLZ
C-                            ICD Laser channels - gains
C-   Returned value  : Link to bank
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   5-MAR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGLZ.LINK'
      INTEGER GZCGNH
C----------------------------------------------------------------------
      GZCGLZ = 0
      LCGNH = GZCGNH()
      IF ( LCGNH.GT.0 ) THEN
        GZCGLZ = LC(LCGNH-IZCGLZ)
      ENDIF
  999 RETURN
      END
