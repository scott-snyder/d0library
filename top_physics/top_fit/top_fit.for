      FUNCTION TOP_FIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main routine for Top into Lepton + jets
C-
C-   Returned value  : True if OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created 10-JAN-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TOP_FIT
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL DO_TOP_FIT_ANAL
      LOGICAL MAKE_EVENTS
      INTEGER IER
C----------------------------------------------------------------------
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('TOP_FIT_RCP')
        CALL EZGET('DO_TOP_FIT_ANAL',DO_TOP_FIT_ANAL,IER)
        CALL EZGET('MAKE_RCP_EVENTS',MAKE_EVENTS,IER)
        CALL EZRSET
        IF ( MAKE_EVENTS ) THEN
          CALL TOP_FIT_MAKE_RCP
        ENDIF
      ENDIF
C
      IF ( IER.EQ.0 ) THEN
        IF(DO_TOP_FIT_ANAL)CALL TOP_FIT_ANAL        ! ANALYZE
C
        TOP_FIT = .TRUE.
      ELSE
        TOP_FIT = .FALSE.
      ENDIF
  999 RETURN
      END
