      LOGICAL FUNCTION SHLEVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : steering routine for showerlibrary
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-JAN-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL DO_MAKE_LIB,DO_ISANL,DO_SHLANL,DO_SHSTAT
      INTEGER IER
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('SHOWERLIBRARY_RCP')
        CALL EZGET('DO_MAKE_LIB',DO_MAKE_LIB,IER)
        CALL EZGET('DO_ISANL',DO_ISANL,IER)
        CALL EZGET('DO_SHLANL',DO_SHLANL,IER)
        CALL EZGET('DO_SHSTAT',DO_SHSTAT,IER)
        IF(IER.NE.0)THEN
          CALL ERRMSG('SHOWERLIBRARY','SHLEVT',
     &      'ERROR READING SHOWERLIBRARY_RCP','W')
          SHLEVT = .FALSE.
          RETURN
        ENDIF
        CALL EZRSET
      ENDIF
C
      SHLEVT = .TRUE.
      CALL TIMER
      IF ( DO_MAKE_LIB ) THEN
        CALL SHLEVT_MAKE_LIB            ! MAKE SHOWERLIBRARY
      ELSEIF ( DO_ISANL ) THEN
        CALL SHLEVT_ISANL               ! ANALYZE ISAJET TRACKS
      ELSEIF ( DO_SHLANL ) THEN
        CALL SHLEVT_SHLANL              ! ANALYZE SHOWER LIBRARY
      ENDIF
      CALL TIMER
C
  999 RETURN
      END
