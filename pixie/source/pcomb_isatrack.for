      SUBROUTINE PCOMB_ISATRACK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays ISAJET tracks with PISATRACK. This
C-   is an action routine of the COMBDIS package.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   4-DEC-1990   Sharon Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL EZERROR
      INTEGER IER
C----------------------------------------------------------------------
C
C ****  Select correct bank
C
      CALL EZPICK('PX_COMBDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCOMB_ISATRACK',
     &    'Unable to pick bank PX_COMBDIS_RCP','W')
        GOTO 999
      ELSE
        CALL PISATRACK
        CALL EZRSET
      ENDIF
  999 RETURN
      END
