      FUNCTION MU1_FILT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return TRUE if this event passed a filter
C-                         in the MU1 set
C-
C-   Returned value  : TRUE if it did
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-Feb-1994       D. Wood
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL MU1_FILT
      INTEGER NTRIG, NFILT, TRIGBIT(32), FILTBIT(128), I
      CHARACTER*64 TRIGNAME(32), FILTNAME(128)
C----------------------------------------------------------------------
      MU1_FILT = .FALSE.
C
C: GET LIST OF PASSED FILTERS
C
      CALL GTTSUM( NTRIG, TRIGBIT, TRIGNAME, NFILT, FILTBIT, FILTNAME
     &    )
C
C: Check that at least one passed...
C
      IF ( NFILT .LE. 0  ) THEN
        CALL ERRMSG('Nothing passed','MU1_FILT',
     &    'No filters passed this event!!?','W')
        RETURN
      ENDIF
C
C: Loop over passed filters and see if any are "of interest"
C
      DO I = 1, NFILT
        IF ( INDEX(FILTNAME(I), 'MU_1_MAX' ) .NE. 0 ) MU1_FILT = .TRUE.
        IF ( INDEX(FILTNAME(I), 'MU_2_HIGH') .NE. 0 ) MU1_FILT = .TRUE.
        IF ( INDEX(FILTNAME(I), 'MU_2_MAX') .NE. 0 ) MU1_FILT = .TRUE.
        IF ( INDEX(FILTNAME(I), 'MU_JET_MON') .NE. 0 ) MU1_FILT = .TRUE.
        IF ( INDEX(FILTNAME(I), 'MU_GIS') .NE. 0 ) MU1_FILT = .TRUE.
      ENDDO

  999 RETURN
      END
