      FUNCTION PCISATRACK ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays ISAJET tracks with PISATRACK.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   4-DEC-1990   Sharon Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PCISATRACK
      LOGICAL EZERROR
      INTEGER IER
C----------------------------------------------------------------------
      PCISATRACK = .TRUE.
C
C ****  Select correct bank
C
      CALL EZPICK('PX_CALDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCISATRACK',
     &    'Unable to pick bank PX_CALDIS_RCP','W')
        GOTO 999
      ELSE
        CALL PISATRACK
        CALL EZRSET
      ENDIF
  999 RETURN
      END
