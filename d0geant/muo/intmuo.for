      LOGICAL FUNCTION INTMUO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialises user commands for MUO.
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
      INTMUO = .TRUE.
C
      I = IDMUO
      IF ( DMUO.GE.2) THEN
C        CALL ZCMENU(I,'MUO$')
C        CALL GUIMUO(I,1,K)
      ELSE
C        CALL ZCMENU(I,'NO-MUO$')
      ENDIF
C
  999 RETURN
      END
