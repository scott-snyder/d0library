      INTEGER FUNCTION GZCGL0()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CGL0
C-                            Level-0 channels - gains
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
      INCLUDE 'D0$LINKS:IZCGL0.LINK'
      INTEGER GZCGNH
C----------------------------------------------------------------------
      GZCGL0 = 0
      LCGNH = GZCGNH()
      IF ( LCGNH.GT.0 ) THEN
        GZCGL0 = LC(LCGNH-IZCGL0)
      ENDIF
  999 RETURN
      END
