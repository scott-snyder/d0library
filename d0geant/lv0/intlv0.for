      LOGICAL FUNCTION INTLV0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialises user commands for LV0.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   6-JUN-1989   Harrison B. Prosper
C-   Code taken from GUINTI
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:D0COM.INC/LIST'
C
      INTEGER I,K
C----------------------------------------------------------------------
      INTLV0 = .TRUE.
C
      I = IDLV0
      IF ( DLV0.GE.2) THEN
C        CALL ZCMENU(I,'LV0$')
C        CALL GUILV0(I,1,K)
      ELSE
C        CALL ZCMENU(I,'NO-LV0$')
      ENDIF
C
  999 RETURN
      END
