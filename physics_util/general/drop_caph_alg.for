      LOGICAL FUNCTION DROP_CAPH_ALG()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Drop unwanted CAPH banks.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Entry points:  DROP_CAPH_ALG_INI - Initialization.
C-
C-   Created  10-Dec-1994   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DROP_CAPH_ALG_INI
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAPH.DEF'
      INCLUDE 'D0$LINKS:IZCAPH.LINK'
C-
C- Variables from DROP_CAPH_ALG_RCP.
C-
      LOGICAL DO_DROP_CAPH_ALG
      LOGICAL DROP_SELECTED_CAPHS, KEEP_SELECTED_CAPHS
      INTEGER NUM_ALG, MAX_ALG, LEN_TEMPLATE, NUM_TEMPLATE
      PARAMETER(MAX_ALG=20, LEN_TEMPLATE=7)
      CHARACTER*12 ALGORITHM_LIST(MAX_ALG)
      REAL TEMPLATE_LIST(LEN_TEMPLATE, MAX_ALG)
C-
C- Other variables and functions.
C-
      INTEGER LPROC, LCAPH
      INTEGER GZPROC, GZCAPH
      INTEGER NUM_LCAPH, LCAPH_LIST(MAX_ALG)
      LOGICAL SELECTED, CAPHEL
      INTEGER I, IER
      CHARACTER*80 MSG
      LOGICAL FIRST
C-
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      DROP_CAPH_ALG = .TRUE.
C-
C- Drop CAPH banks.
C-
      IF(DO_DROP_CAPH_ALG)THEN
C-
C- Get link of PROC bank.
C-
        LPROC = GZPROC()
        IF(LPROC.EQ.0)GO TO 999
C-
C- First get a list of the links of all CAPH banks corresponding to the
C- TEMPLATE_LIST by calling SET_CAPH.
C-
        NUM_LCAPH = 0
        DO I = 1, NUM_ALG
          CALL SET_CAPH(ALGORITHM_LIST(I), TEMPLATE_LIST(1,I), IER)
          IF(IER.EQ.0)THEN
            LCAPH = GZCAPH()
            IF(LCAPH.NE.0)THEN
              NUM_LCAPH = NUM_LCAPH + 1
              LCAPH_LIST(NUM_LCAPH) = LCAPH
            ENDIF
            CALL RESET_CAPH
          ENDIF
        ENDDO
C-
C- Now loop over all CAPH banks and drop the ones we don't want.
C-
        LCAPH = LQ(LPROC-IZCAPH)
        DO WHILE(LCAPH.NE.0)
          SELECTED = .FALSE.
          DO I = 1, NUM_LCAPH
            SELECTED = SELECTED .OR. LCAPH.EQ.LCAPH_LIST(I)
          ENDDO
          CAPHEL = IQ(LCAPH+4).EQ.A_ELECTRON
C-
C- Here is where we drop unwanted CAPH banks.
C-
          IF(SELECTED.AND.DROP_SELECTED_CAPHS .OR.
     &       .NOT.CAPHEL.AND..NOT.SELECTED.AND.KEEP_SELECTED_CAPHS)THEN
            CALL MZDROP(IXCOM, LCAPH, ' ')
          ENDIF
          LCAPH = LQ(LCAPH)
        ENDDO
      ENDIF
      GO TO 999
 
      ENTRY DROP_CAPH_ALG_INI()
C-
C- Initialization entry point
C-
      DROP_CAPH_ALG_INI = .TRUE.
      IF(FIRST) THEN
C-
C- Read RCP parameters.  First read from DROP_CAPH_ALG_RCP.
C-
        CALL EZPICK_NOMSG('DROP_CAPH_ALG_RCP', IER)
        IF(IER.NE.0)THEN
          CALL INRCP('DROP_CAPH_ALG_RCP', IER)
          CALL EZPICK_NOMSG('DROP_CAPH_ALG_RCP', IER)
        ENDIF
        IF(IER.EQ.0)
     &    CALL EZGET('DO_DROP_CAPH_ALG', DO_DROP_CAPH_ALG, IER)
        IF(IER.EQ.0. .AND. DO_DROP_CAPH_ALG) THEN
          IF(IER.EQ.0)CALL EZGET('DROP_SELECTED_CAPHS',
     &      DROP_SELECTED_CAPHS, IER)
          IF(IER.EQ.0)CALL EZGET('KEEP_SELECTED_CAPHS',
     &      KEEP_SELECTED_CAPHS, IER)
          IF(IER.EQ.0)CALL EZ_GET_CHARS('ALGORITHM_LIST', NUM_ALG, 
     &      ALGORITHM_LIST, IER)
          IF(IER.EQ.0)CALL EZGETA('TEMPLATE_LIST', 0, 0, 0, 
     &      NUM_TEMPLATE, IER)
          IF(NUM_TEMPLATE.NE.LEN_TEMPLATE*NUM_ALG)IER = -1
          IF(IER.EQ.0)THEN
            IF(NUM_ALG.GT.MAX_ALG)THEN
              CALL ERRMSG('Too many templates', 'DROP_CAPH_ALG_INI',
     &          ' ', 'F')
            ENDIF
            CALL EZGET('TEMPLATE_LIST', TEMPLATE_LIST, IER)
          ENDIF
        ENDIF
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in DROP_CAPH_ALG_RCP',
     &    'DROP_CAPH_ALG_INI','Error getting RCP parameters','F')
        FIRST=.FALSE.
      ENDIF
 999  RETURN
      END
