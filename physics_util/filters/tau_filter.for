      FUNCTION TAU_FILTER()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return TRUE if this event passed a tau_filter
C-
C-   Returned value  : TRUE if it did
C-
C-   Created   16-MAY-1994   Qizhong Li-Demarteau   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TAU_FILTER
      INTEGER NTRIG, NFILT, TRIGBIT(32), FILTBIT(128), I, L
      INTEGER TRULEN
      CHARACTER*32 TRIGNAME(32), FILTNAME(128)
C----------------------------------------------------------------------
      TAU_FILTER = .FALSE.
C
C: GET LIST OF PASSED FILTERS
C
      CALL GTTSUM(NTRIG, TRIGBIT, TRIGNAME, NFILT, FILTBIT, FILTNAME)
C
C: Check that at least one passed...
C
      IF ( NFILT .LE. 0  ) THEN
        CALL ERRMSG('Nothing passed','TAU_FILTER',
     &    'No filters passed this event!!?','W')
      ENDIF
      IF ( NTRIG .LE. 0  ) THEN
        CALL ERRMSG('Nothing passed','TAU_FILTER',
     &    'No triggers passed this event!!?','W')
      ENDIF
C
C: Loop over passed filters and see if any are "of interest"
C  Require exact matches.
C
      DO I = 1, NFILT
	L=TRULEN(FILTNAME(I))
        IF ( FILTNAME(I)(1:L).EQ.'JET_1_VETO'  ) TAU_FILTER = .TRUE.
        IF ( FILTNAME(I)(1:L).EQ.'JET_1_MISS'  ) TAU_FILTER = .TRUE.
      ENDDO
C
  999 RETURN
      END
