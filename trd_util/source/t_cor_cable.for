      SUBROUTINE T_COR_CABLE(ILAYER,IWIRE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correct cabling errors
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-APR-1991   Saclay Group
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ILAYER,IWIRE,IW2
      IF (ILAYER.EQ.3) THEN
       IF (IWIRE.GE.48.AND.IWIRE.LE.55) IW2 = IWIRE + 8
       IF (IWIRE.GE.56.AND.IWIRE.LE.63) IW2 = IWIRE - 8
      ENDIF
      IWIRE = IW2
  999 RETURN
      END
