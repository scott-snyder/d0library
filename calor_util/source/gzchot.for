      FUNCTION GZCHOT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the link to the CHOT bank.
C-                            Calorimeter hot channel bank.
C-
C-   Returned value  : Link to CHOT bank
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  26-JAN-1993   Jan Guida   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCHOT.LINK'
      INTEGER LCHOT,GZCHOT,GZCGEV,LCGEV
C----------------------------------------------------------------------
C
      GZCHOT = 0
      LCGEV = GZCGEV()
      IF (LCGEV.GT.0) THEN
        GZCHOT = LC(LCGEV-IZCHOT)
      ELSE
        CALL ERRMSG('CALHOT','GZCHOT',' Error finding CHOT bank','W')
      END IF
C
  999 RETURN
      END
