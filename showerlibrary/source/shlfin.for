      LOGICAL FUNCTION SHLFIN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finish stuff in Shower library Process.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  27-DEC-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
C----------------------------------------------------------------------
        CALL ERRMSG('SHOWERLIBRARY','SHLFIN',
     &    ' FINISHING UP SHOWER LIBRARY WRITE + HBOOK ','W')
C
      CALL SHLEND                       ! TIDY UP.
      SHLFIN = .TRUE.
C
      CALL DHDIR('SHOWERLIBRARY_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Print HBOOK ID's
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('SHOWERLIBRARY','SHLFIN',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
        SHLFIN = .FALSE.
      ENDIF
C
      CALL HLDIR(' ',' ')               ! LIST DIRECTORY
C      CALL HISTDO                       ! With index
      CALL STAT_MAKE_LIB                ! PRINT STATISTICS
C
  999 RETURN
      END
