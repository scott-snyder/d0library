      FUNCTION TTEE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main routine for Top into Lepton + jets
C-
C-   Returned value  : True if OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-OCT-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TTEE
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL DO_TTEE_ANAL
      INTEGER IER
C----------------------------------------------------------------------
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('TTEE_RCP')
        CALL EZGET('DO_TTEE_ANAL',DO_TTEE_ANAL,IER)
        IF(IER.NE.0)THEN
          CALL ERRMSG('TTEE','TTEE',
     &      'ERROR READING TTEE_RCP','W')
        ENDIF
        CALL EZRSET
      ENDIF
C
      IF(DO_TTEE_ANAL)CALL TTEE_ANAL        ! ANALYZE
C
      TTEE = .TRUE.
  999 RETURN
      END
