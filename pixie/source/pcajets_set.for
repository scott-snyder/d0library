      SUBROUTINE PCAJETS_SET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set some parameters for PCAJETS.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-NOV-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      LOGICAL EZERROR
      INTEGER IER
      INTEGER OLD_JET_TYPE,NEW_JET_TYPE
      SAVE OLD_JET_TYPE
      DATA NEW_JET_TYPE /2/
C----------------------------------------------------------------------
C
C ****  Setting parameters for PCAJETS
C
      CALL EZPICK('PX_CALDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCAJETS_SET',
     &    'Unable to find bank PX_CALDIS_RCP','W')
        GOTO 999
      ENDIF
C
      CALL PUGETV('JET TYPE',OLD_JET_TYPE)
      CALL PUSETV('JET TYPE',NEW_JET_TYPE)
      CALL EZRSET
      RETURN
C
      ENTRY PCAJETS_RESET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reset Jet type
C-
C-   Created  28-NOV-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
C
C ****  Reset parameters for PCAJETS
C
      CALL EZPICK('PX_CALDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCAJETS_RESET',
     &    'Unable to find bank PX_CALDIS_RCP','W')
        GOTO 999
      ENDIF
C
      CALL PUSETV('JET TYPE',OLD_JET_TYPE)
      CALL EZRSET
  999 RETURN
      END
