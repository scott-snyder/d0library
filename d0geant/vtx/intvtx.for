      LOGICAL FUNCTION INTVTX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialises user commands for VTX.
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
      INTVTX = .TRUE.
C
      I = IDVTX
      IF ( DVTX.GE.2) THEN
C        CALL ZCMENU(I,'VTX$')
C        CALL GUIVTX(I,1,K)
      ELSE
C        CALL ZCMENU(I,'NO-VTX$')
      ENDIF
C
  999 RETURN
      END
