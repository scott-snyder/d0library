      LOGICAL FUNCTION PMEVT_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Muon Initialize within the Event Loop.
C-
C-   Returned value  : TRUE or FALSE
C-
C-   Created  14-MAR-1994   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER,USE_DBL
      LOGICAL OK, MUOPAR,MCONST
C----------------------------------------------------------------------
      OK = .FALSE.
C-
      CALL EZPICK('MURECO_RCP')
      CALL EZGET('USE_DBL',USE_DBL,IER)
      IF (USE_DBL .GT. 0) THEN
        USE_DBL = 0
        CALL EZSET('USE_DBL',USE_DBL,IER)
      ENDIF
      CALL EZRSET
C-
      CALL EZPICK('SAMRECO_RCP')
      CALL EZGET('USE_DBL',USE_DBL,IER)
      IF (USE_DBL .GT. 0) THEN
        USE_DBL = 0
        CALL EZSET('USE_DBL',USE_DBL,IER)
      ENDIF
      CALL EZRSET
C-
      OK = MUOPAR()
      IF ( OK ) THEN
        CALL EZPICK('MURECO_RCP')
        OK = MCONST()
        CALL EZRSET
      ENDIF
      PMEVT_INI = OK
C-
  999 RETURN
      END
