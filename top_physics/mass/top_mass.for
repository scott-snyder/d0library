      FUNCTION TOP_MASS()
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
      LOGICAL TOP_MASS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL DO_TOP_MASS_ANAL
      LOGICAL DO_TOP_MASS_ANAL1
      INTEGER IER
C----------------------------------------------------------------------
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET('DO_TOP_MASS_ANAL',DO_TOP_MASS_ANAL,IER)
        CALL EZGET('DO_TOP_MASS_ANAL1',DO_TOP_MASS_ANAL1,IER)
        CALL EZRSET
      ENDIF
C
      IF ( IER.EQ.0 ) THEN
        IF(DO_TOP_MASS_ANAL)CALL TOP_MASS_ANAL        ! ANALYZE
        IF(DO_TOP_MASS_ANAL1)CALL TOP_MASS_ANAL1        ! ANALYZE
C
        TOP_MASS = .TRUE.
      ELSE
        TOP_MASS = .FALSE.
      ENDIF
  999 RETURN
      END
