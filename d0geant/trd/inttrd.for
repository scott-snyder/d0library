      LOGICAL FUNCTION INTTRD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialises user commands for TRD.
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
      INTTRD = .TRUE.
C
      I = IDTRD
      IF ( DTRD.GE.2) THEN
C        CALL ZCMENU(I,'TRD$')
C        CALL GUITRD(I,1,K)
      ELSE
C        CALL ZCMENU(I,'NO-TRD$')
      ENDIF
C
  999 RETURN
      END
