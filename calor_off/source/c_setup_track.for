      SUBROUTINE C_SETUP_TRACK(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SETUP TRACK FROM ISAJET, OR TEST BEAM
C-   DATA
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  30-MAR-1992   Rajendran Raja
C-   Updated  30-MAR-1992   Meenakshi Narain  get PWC track info for TB90 data
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      LOGICAL FIRST, LMONTE_CARLO, LTEST_BEAM ,LZTRAKS
      INTEGER GZISV1,GZTBD0
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        IER = 0
        LMONTE_CARLO = .FALSE.
        LTEST_BEAM = .FALSE.
        IF(GZISV1().NE.0)LMONTE_CARLO = .TRUE.
        LTEST_BEAM = .FALSE.
        IF(GZTBD0().NE.0)LTEST_BEAM = .TRUE.
        IF(LMONTE_CARLO)THEN
          CALL ERRMSG('CALORIMETER','C_SETUP_TRACK',
     &      'MONTE CARLO DATA  BEING USED','W')
        ELSEIF ( LTEST_BEAM ) THEN
          CALL ERRMSG('CALORIMETER','C_SETUP_TRACK',
     &      ' TESTBEAM DATA IS BEING USED FOR INPUT TRACKS','W')
        ELSE
          LZTRAKS = .TRUE.
          CALL ERRMSG('CALORIMETER','C_SETUP_TRACK',
     &      'ZTRAKS BEING USED FOR INPUT TRACKS','W')
        ENDIF
        FIRST=.FALSE.
      END IF
      IF (LMONTE_CARLO) THEN
        CALL C_SETUP_ISAJET
      ELSE IF (LTEST_BEAM) THEN
        CALL C_SETUP_PWCPAR(IER)
      ELSEIF ( LZTRAKS) THEN
        CALL C_SETUP_ZTRAKS(IER)
      ELSE
        IER = IER + 1
      END IF
  999 RETURN
      END
