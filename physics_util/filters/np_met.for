      FUNCTION NP_MET()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return TRUE if this event passed a MET
C-                         filter
C-
C-   Returned value  : TRUE if it did
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-FEB-1994   Richard V. Astur
C-   Adapted   29-MAR-1994  Lee Lueking from Rich's MINBIAS filter
C-   Modified  1-MAY-1994   Lee Lueking Added TRULEN to make it work properly.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL NP_MET
      INTEGER NTRIG, NFILT, TRIGBIT(32), FILTBIT(128), I, L
      INTEGER TRULEN
      CHARACTER*32 TRIGNAME(32), FILTNAME(128)
C----------------------------------------------------------------------
      NP_MET = .FALSE.
C
C: GET LIST OF PASSED FILTERS
C
      CALL GTTSUM( NTRIG, TRIGBIT, TRIGNAME, NFILT, FILTBIT, FILTNAME
     &    )
C
C: Check that at least one passed...
C
      IF ( NFILT .LE. 0  ) THEN
        CALL ERRMSG('Nothing passed','NP_MET',
     &    'No filters passed this event!!?','W')
      ENDIF
      IF ( NTRIG .LE. 0  ) THEN
        CALL ERRMSG('Nothing passed','NP_MET',
     &    'No triggers passed this event!!?','W')
      ENDIF
C
C: Loop over passed filters and see if any are "of interest"
C  Require exact matches.
C
      DO I = 1, NFILT
	L=TRULEN(FILTNAME(I))
        IF ( FILTNAME(I)(1:L).EQ.'SCALAR_ET') NP_MET = .TRUE.
        IF ( FILTNAME(I)(1:L).EQ.'SCALAR_ET_JT') NP_MET = .TRUE.
        IF ( FILTNAME(I)(1:L).EQ.'SCALAR_ET_HT') NP_MET = .TRUE.
        IF ( FILTNAME(I)(1:L).EQ.'MISSING_ET'  ) NP_MET = .TRUE.
        IF ( FILTNAME(I)(1:L).EQ.'JET_3_MISS'  ) NP_MET = .TRUE.
        IF ( FILTNAME(I)(1:L).EQ.'JET_2_MISS'  ) NP_MET = .TRUE.
      ENDDO
C
C: Loop over passed L1 triggers and see if any are "of interest"
C
C      DO I = 1, NTRIG
C        IF ( INDEX(TRIGNAME(I), 'ZERO_BIAS' ) .NE. 0 ) MINBIAS = .TRUE.
C      ENDDO

  999 RETURN
      END
