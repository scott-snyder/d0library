      LOGICAL FUNCTION NP_SCALAR_TIGHT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filter for high scalar Et events.
C-
C-   Returned value: .TRUE. to keep the event, .FALSE. on error.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   ENTRY  NP_SCALAR_TIGHT_EOJ  performs end-of-job summary for this filter.
C-   Created  14-DEC-1992   Marc Paterno
C-   Updated  16-FEB-1993   Jay A. Wightman  Modify to cut on total E
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      LOGICAL  NP_SCALAR_TIGHT_EOJ
C----------------------------------------------------------------------
      INTEGER  LCAEP, GZCAEP, LGLOB, GZGLOB, LPNUT, GZPNUT, IER
      INTEGER  LCAPH,GZCAPH,LJETS,GZJETS
      REAL     SUMMARY(20),TEMPLATE(5),CONE_RADIUS,EMFRJET1,EMFRJET1_CUT
      REAL     RSUMMARY(20)
      INTEGER  I, NCH,QCD_BAD_WORD_FLAG,NUM_BAD
      EXTERNAL GZCAEP, GZGLOB, GZPNUT
      REAL     ETOT, ETOT_CUT, SCALAR_ET, SCALAR_ET_CUT
      REAL     HARD_SCALAR_ET_CUT
      LOGICAL  FIRST,ANALYZING_STA,QCD_BAD_JET_CUT
      CHARACTER*8 ALGORITHM
      DATA     FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP ('NP_SCALAR_TIGHT_RCP', IER)

        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ( 'Could not find NP_SCALAR_TIGHT_RCP',
     &      'NP_SCALAR_TIGHT', ' ', 'F')
        ENDIF                           ! if ier .eq. 0

        CALL EZGET ('ETOT_CUT', ETOT_CUT, IER)
        CALL EZGET ('SCALAR_ET_CUT', SCALAR_ET_CUT, IER)
        CALL EZGET ('HARD_SCALAR_ET_CUT', HARD_SCALAR_ET_CUT, IER)
        CALL EZGET ('CONE_RADIUS', CONE_RADIUS, IER)
        CALL EZGET ('EMFRJET1_CUT', EMFRJET1_CUT, IER)
        CALL EZGET ('QCD_BAD_JET_CUT', QCD_BAD_JET_CUT, IER)
        CALL EZRSET

        CALL VZERO(RSUMMARY,20)

      ENDIF                             ! if first
C----------------------------------------------------------------------
      NP_SCALAR_TIGHT = .FALSE.               ! reject by default

      LGLOB=GZGLOB()
      LCAEP=GZCAEP()
      IF(LCAEP.GT.0) ANALYZING_STA=.TRUE.
      LPNUT=GZPNUT(2)

      IF(LGLOB.GT.0) THEN
        ETOT=Q(LGLOB+8)
        IF(ETOT.GT.ETOT_CUT) RETURN
      ELSEIF(LCAEP.GT.0) THEN
        NCH=IQ(LCAEP+3)
        ETOT=0.0
        DO I=1,NCH
          ETOT=ETOT+Q(LCAEP+3+I*2)
        ENDDO
        IF(ETOT.GT.ETOT_CUT) RETURN
      ENDIF
      RSUMMARY(1) = RSUMMARY(1) + 1
      IF(LPNUT.LE.0) RETURN
C
C     CHECK EACH CAPH BANK FOR ANY ASSOCIATED JETS BANKS
C
      IF(CONE_RADIUS.GT.0.0) THEN
        ALGORITHM='CONE_JET'
        TEMPLATE(1)=1.0
        TEMPLATE(2)=6.0
        TEMPLATE(3)=CONE_RADIUS
      ELSE
        ALGORITHM='NN_JET'
C
C       FOLLOWING QCD GROUP'S CHOICE OF TEMPLATE FOR NN ALGORITHM
C
        TEMPLATE(1)=0.0
      ENDIF
      CALL SET_CAPH(ALGORITHM,TEMPLATE,IER)
      LCAPH=GZCAPH()
      LJETS=GZJETS()
      IF(LJETS.GT.0) THEN
C
C       SORT THE JETS BY INCREASING ET (WORD 6)
C
        CALL ZSORT(IXMAIN,LJETS,6)
        LJETS=GZJETS()
C
C       HIGHEST ET FIRST
C
        CALL ZTOPSY(IXMAIN,LJETS)
        LJETS=GZJETS()
      ENDIF
C
      EMFRJET1=Q(LJETS+14)
      IF(EMFRJET1.LT.EMFRJET1_CUT) THEN
        CALL RESET_CAPH
        RETURN
      ENDIF
      RSUMMARY(2) = RSUMMARY(2) + 1
      CALL RESET_CAPH
      IF(ANALYZING_STA) THEN
C       CALL QCD_BAD_JET_EVENT(QCD_BAD_WORD_FLAG,NUM_BAD)
      ELSE
        CALL QCD_BAD_DST_EVENT(QCD_BAD_WORD_FLAG,NUM_BAD)
      ENDIF
      IF(QCD_BAD_JET_CUT) THEN
        IF(QCD_BAD_WORD_FLAG.NE.0) RETURN
      ENDIF
      RSUMMARY(3) = RSUMMARY(3) + 1
      SCALAR_ET=Q(LPNUT+14)
      IF ( LGLOB.GT.0 ) THEN
        IF(SCALAR_ET.LT.SCALAR_ET_CUT) RETURN
      ELSE
        IF(SCALAR_ET.LT.HARD_SCALAR_ET_CUT) RETURN
      ENDIF

      NP_SCALAR_TIGHT = .TRUE.
      RETURN
C#######################################################################
      ENTRY NP_SCALAR_TIGHT_EOJ (SUMMARY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End of job summary for this filter.  Dummy for now.
C-
C-   Inputs  : none
C-   Outputs : SUMMARY   [R(20)] array of 20 integers
C-   Controls: none
C-
C-   Created  18-DEC-1992   Marc Paterno
C-
C----------------------------------------------------------------------
      NP_SCALAR_TIGHT_EOJ = .TRUE.
      DO I = 1,20
        SUMMARY(I) = RSUMMARY(I)
      ENDDO
      RETURN
      END
