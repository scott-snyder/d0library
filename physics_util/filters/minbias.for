      FUNCTION MINBIAS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return TRUE if this event passed a minbias
C-                         filter
C-
C-   Returned value  : TRUE if it did
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-FEB-1994   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL MINBIAS
      INTEGER NTRIG, NFILT, TRIGBIT(32), FILTBIT(128), I
      CHARACTER*64 TRIGNAME(32), FILTNAME(128)
C----------------------------------------------------------------------
      MINBIAS = .FALSE.
C
C: GET LIST OF PASSED FILTERS
C
      CALL GTTSUM( NTRIG, TRIGBIT, TRIGNAME, NFILT, FILTBIT, FILTNAME
     &    )
C
C: Check that at least one passed...
C
      IF ( NFILT .LE. 0  ) THEN
        CALL ERRMSG('Nothing passed','MINBIAS',
     &    'No filters passed this event!!?','W')
      ENDIF
      IF ( NTRIG .LE. 0  ) THEN
        CALL ERRMSG('Nothing passed','MINBIAS',
     &    'No triggers passed this event!!?','W')
      ENDIF
C
C: Loop over passed filters and see if any are "of interest"
C
      DO I = 1, NFILT
        IF ( INDEX(FILTNAME(I), 'ZERO_BIAS' ) .NE. 0 ) MINBIAS = .TRUE.
        IF ( INDEX(FILTNAME(I), 'MIN_BIAS') .NE. 0 ) MINBIAS = .TRUE.
      ENDDO
C
C: Loop over passed L1 triggers and see if any are "of interest"
C
      DO I = 1, NTRIG
        IF ( INDEX(TRIGNAME(I), 'ZERO_BIAS' ) .NE. 0 ) MINBIAS = .TRUE.
      ENDDO

  999 RETURN
      END
