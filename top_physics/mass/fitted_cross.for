      REAL FUNCTION FITTED_CROSS(TYPE,TMASS)

C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns fitted top cross section
C-
C-   Inputs  : QUARK IF TRUE WILL GIVE QQBAR INDUCED CROSS SECTION
C-             IF FALSE WILL RETURN GLUON INDUCED PART.
C-   Outputs : 
C-   Controls: 
C-
C-   Created   4-APR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    TMASS
      LOGICAL first
      SAVE first
      DATA first / .true. /
      REAL    GLUON(10),QQB(10),TOTALS(10)
      REAL    POLY_CROSS
      INTEGER IER
      CHARACTER*(*) TYPE
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET('GLUON_FIT',GLUON,IER)
        CALL EZGET('QUARK_FIT',QQB,IER)
        CALL EZGET('TOTAL_FIT',TOTALS,IER)
        CALL EZRSET
      ENDIF
      IF ( TYPE.EQ.'QUARK' ) THEN
        FITTED_CROSS = POLY_CROSS(TMASS,QQB)
      ELSEIF ( TYPE.EQ.'GLUON' ) THEN
        FITTED_CROSS = POLY_CROSS(TMASS,GLUON)
      ELSEIF ( TYPE.EQ.'TOTAL' ) THEN
        FITTED_CROSS = POLY_CROSS(TMASS,TOTALS)
      ENDIF
  999 RETURN
      END
