      LOGICAL FUNCTION INTCAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialises user commands for CAL.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-JUN-1989   Harrison B. Prosper   
C-   Moved code from GUINTI
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:D0COM.INC/LIST'
C
      INTEGER I,K
C----------------------------------------------------------------------
      INTCAL = .TRUE.
C
      I = IDCAL
      IF ( DCAL.GE.2) THEN
C        CALL ZCMENU(I,'CAL$')
C        CALL GUICAL(I,1,K)
      ELSE
C        CALL ZCMENU(I,'NO-CAL$')
      ENDIF
C
  999 RETURN
      END
