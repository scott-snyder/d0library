      LOGICAL FUNCTION INTCDC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialises user commands for CDC.
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
      INTCDC = .TRUE.
C
      I = IDCDC
      IF ( DCDC.GE.2) THEN
C        CALL ZCMENU(I,'CDC$')
C        CALL GUICDC(I,1,K)
      ELSE
C        CALL ZCMENU(I,'NO-CDC$')
      ENDIF
C
  999 RETURN
      END
