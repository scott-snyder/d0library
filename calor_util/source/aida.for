      SUBROUTINE AIDA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : The Anomalous Isolated Deposit Algorithm looks at a
C-   completed CAEH bank, and "removes" (actually reduces the magnitude of) hits
C-   identified as anomalous isolated deposits.  Please refer to the additional
C-   documentation for detailed reference.
C-
C-   This routine requires the RCP file CAHITS_RCP, and modifies the CAEH bank.
C-   Note that the lack of parameters for AIDA control causes a fatal error.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  12-MAR-1993   Marc Paterno
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE  'D0$INC:ZEBCOM.INC'
      REAL     ET_THRESH, RATIO
      INTEGER  IER, LIST(AIDA_LIST_LENGTH), NAID, LCAID, I
      INTEGER  GZCAID, NFOUND
      EXTERNAL GZCAID
      LOGICAL  FIRST, OK, OVERFLOW
      DATA     FIRST / .TRUE./
      SAVE     FIRST, ET_THRESH, RATIO
      CHARACTER*1 BLANK
      PARAMETER (BLANK =' ')
      REAL     RATIO_LIST (AIDA_LIST_LENGTH)
C----------------------------------------------------------------------
C
C ****  Initialize
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.

        CALL EZPICK ('CAHITS_RCP')
        CALL EZGET ('AIDA_ET_THRESH', ET_THRESH, IER)
        IF ( IER .NE. 0 ) GOTO 100      ! FATAL ERROR
        CALL EZGET ('AIDA_RATIO', RATIO, IER)
        IF ( IER .NE. 0 ) GOTO 100      ! FATAL ERROR
        CALL EZRSET
      ENDIF                             ! if first
C
C ****  Remove any previously existing CAID bank
C
      LCAID = GZCAID()
      IF ( LCAID .GT. 0 ) CALL MZDROP (IXCOM, LCAID, BLANK)
C
C ****  Get list of candidates
C
      CALL AIDA_MAKE_LIST (ET_THRESH, LIST, OK, NFOUND)
      IF ( .NOT. OK ) THEN
        CALL ERRMSG ('AIDA_MAKE_LIST failed', 'AIDA',
     &    'AIDA cannot proceed without list - aborting', 'W')
        RETURN
      ENDIF                             ! if .not. ok
C
C ****  Cull list of candidates
C
      CALL AIDA_CULL_LIST (LIST, ET_THRESH, RATIO, NAID, RATIO_LIST)

      IF ( NAID .EQ. 0 ) THEN
        OK = .TRUE.
        RETURN
      ENDIF                             ! if naid .eq. 0
C
C ****  Modify CAEH
C
      CALL AIDA_FIX_CAEH (LIST, OK)

      IF ( .NOT. OK ) THEN
        CALL ERRMSG ('AIDA_FIX_BANKS failed', 'AIDA',
     &    'AIDA cannot proceed from this error -- aborting', 'W')
        RETURN
      ENDIF                             ! if .not. ok
C
C ****  Make the CAID bank
C
      LCAID = 0
      CALL CAIDFL( NAID, NFOUND, ET_THRESH, RATIO,
     &             LIST, RATIO_LIST, LCAID, OK)

      RETURN
C
C ****  Jump here on error reading RCP file
C
  100 CONTINUE
      CALL ERRMSG ('Missing parameter(s) in CAHITS_RCP', 'AIDA',
     &  'AIDA cannot proceed without control parameters - crashing',
     &  'F' )
      END
