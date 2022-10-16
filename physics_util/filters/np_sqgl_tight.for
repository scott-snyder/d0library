      LOGICAL FUNCTION NP_SQGL_TIGHT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filtering for Squark and Gluino events.  This
C-   function expects to see DST format data.  The requirements are:
C-
C-     MISSING ET greater than the threshold MET_CUT
C-     NJETS or more jets with EM fraction greater than JET_EMF_MIN and less
C-     than JET_EMF_MAX, with abs(eta) less than JET_ETA_CUT.
C-
C-   We use the full calorimeter, but not the muon system, in the missing Et
C-   calculation (PNUT(2) bank).
C-
C-   Returned value:  .TRUE. if the event is to be kept; .FALSE. if the event is
C-                    not wanted by this filter.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: control parameters in NP_SQGL_TIGHT_RCP
C-
C-
C-   Created   11-JAN-1993   Marc Paterno
C-   Updated   6-APR-1993   Marc Paterno  Made to check ALL of given algorithm's
C-   jets for a "hot cell" jet.
C-   Updated  13-APR-1993   Marc Paterno  Updated to include cuts on jet-missing
C-                                        Et correlations.
C-   Updated  31-AUG-1993   K. Wyatt Merritt  Fix extra RESET_CAPH bug 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INCLUDE  'D0$INC:PI.DEF'
      INCLUDE  'D0$LINKS:IZHMTE.LINK'
      INCLUDE  'D0$LINKS:IZHMTP.LINK'
C----------------------------------------------------------------------
      LOGICAL NP_SQGL_TIGHT_EOJ
      INTEGER RSUM(20)
      INTEGER  NUM, IER, ET_OFFSET, LJETS, GZJETS, SORTBANKS, NGOOD
      INTEGER  NFOUND, I
      PARAMETER ( NUM = 2 )             ! PNUT bank number to use
      PARAMETER ( ET_OFFSET = 7 )       ! offset of ET word in JETS bank
      EXTERNAL GZJETS, SORTBANKS
      INTEGER  NUM_JETS, N_DELTA, LPELC, GZPELC, LPPHO, GZPPHO
      INTEGER  LHMTE, LHMTP
      SAVE     NUM_JETS
      REAL     ENUT(4), MET, THETA, ETA, PHI, SIG(3)
      REAL     MET_CUT, JET_EMF_MIN, JET_EMF_MAX, JET_ET_MIN
      REAL     JET_ETA_MAX, JET_RADIUS, TEMPLATE(3), EMF
      REAL     EMFJET, ETJET, ABSETAJET, DPHI1, DPHI2, DPHI3, PSI
      REAL     PSI_CUT, ELECTRON_ET_CUT, ELECTRON_CHISQ_CUT
      SAVE     MET_CUT, JET_EMF_MIN, JET_EMF_MAX, JET_ET_MIN, PSI_CUT
      SAVE     N_DELTA, ELECTRON_CHISQ_CUT
      SAVE     JET_ETA_MAX, JET_RADIUS, TEMPLATE, ELECTRON_ET_CUT
      REAL     JETINFO(10, 5), ET, CHISQ
      LOGICAL  FIRST, OK
      SAVE     FIRST
      DATA     FIRST /.TRUE./
C----------------------------------------------------------------------
C
C ****  Initialization.  Failure to read RCP file correctly causes FATAL error.
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP ('NP_SQGL_TIGHT_RCP', IER)
C
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ('No NP_SQGL_TIGHT_RCP', 'NP_SQGL_TIGHT',
     &      'Could not find NP_SQGL_TIGHT_RCP', 'F')
        ENDIF                           ! if ier .ne. 0
C
        CALL EZPICK ('NP_SQGL_TIGHT_RCP')
C
        CALL EZGET ('MET_CUT', MET_CUT, IER)
        CALL EZGET_i ('NUM_JETS', NUM_JETS, IER)
        CALL EZGET ('JET_RADIUS', JET_RADIUS, IER)
        CALL EZGET ('JET_EMF_MIN', JET_EMF_MIN, IER)
        CALL EZGET ('JET_EMF_MAX', JET_EMF_MAX, IER)
        CALL EZGET ('JET_ET_MIN', JET_ET_MIN, IER)
        CALL EZGET ('JET_ETA_MAX', JET_ETA_MAX, IER)
        CALL EZGET_i ('N_DELTA', N_DELTA, IER)
        CALL EZGET ('PSI_CUT', PSI_CUT, IER)
        CALL EZGET ('ELECTRON_ET_CUT', ELECTRON_ET_CUT, IER)
        CALL EZGET ('ELECTRON_CHISQ_CUT', ELECTRON_CHISQ_CUT, IER)
C
        TEMPLATE(1) = 1.0
        TEMPLATE(2) = 6.0
        TEMPLATE(3) = JET_RADIUS
C
        CALL EZRSET
      ENDIF                             ! if first
C
C ****  Beginning of event processing
C
      NP_SQGL_TIGHT = .FALSE.                 ! default action -- reject event
C
C ****  Require missing Et greater than MET_CUT, or event is rejected.  If no
C ****  PNUT(2) bank is found the event is kept and a warning is issued.
C
      CALL GTPNUT ( NUM, ENUT, MET, THETA, ETA, PHI, SIG, IER)
C
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG ('No PNUT(2) bank', 'NP_SQGL_TIGHT',
     &      'GTPNUT error for this event', 'W')
        GOTO 100
      ENDIF                                ! if ier .ne. 0
C
      IF ( MET .LT. MET_CUT ) RETURN       ! reject the event
C
C ****  We require that NONE of the R = 0.5 cone jets have a "bad" EM fraction.
C
  100 CONTINUE
      CALL SET_CAPH ('CONE', TEMPLATE, IER)
      LJETS = GZJETS()
      DO WHILE (LJETS .GT. 0)
        EMFJET = Q(LJETS+14)
        ETJET = Q(LJETS+6)
        ABSETAJET = ABS(Q(LJETS+9))
        IF (ABSETAJET .LT. JET_ETA_MAX) THEN
          IF (ETJET .GT. JET_ET_MIN ) THEN
            IF (EMFJET .LT. JET_EMF_MIN) GOTO 999     ! reject
          ENDIF
        ENDIF
        LJETS = LQ(LJETS)
      ENDDO                 ! while ljets .gt. 0
C
C ****  We require at least NUM_JETS "good" jets.  We use the R = JET_RADIUS
C ****  cone jet finder.  A "good" jet passes the following requirements:
C ****          ET > (JET_ET_MIN) GeV
C ****          (JET_EMF_MIN) < EM fraction < (JET_EMF_MAX)
C ****          |eta of jet| < JET_ETA_MAX
C
      CALL JET_INFO ('CONE', TEMPLATE, NUM_JETS, NFOUND, JETINFO, OK)
      IF ( .NOT. OK ) GOTO 999             ! reject the event
C
      IF ( NFOUND .LT. NUM_JETS ) GOTO 999 ! too few jets
      NGOOD = 0
      DO I = 1, MIN(NFOUND, NUM_JETS)
        IF ( JETINFO(1, I) .GE. JET_ET_MIN ) THEN         ! sufficient Et
          IF ( ABS(JETINFO(3, I)) .LE. JET_ETA_MAX ) THEN ! in eta range
            IF ((JETINFO(7, I) .GE. JET_EMF_MIN) .AND. 
     &          (JETINFO(7,I) .LE. JET_EMF_MAX)) THEN      ! good EMF
              NGOOD = NGOOD + 1
            ENDIF                          ! if jetinfo(7, i) .ge. ...
          ENDIF                            ! if abs(jetinfo(3, i) .ge. ...
        ENDIF                              ! if jetinfo(1, i) .gt. ...
      ENDDO
C
      IF ( NGOOD .LT. NUM_JETS ) GOTO 999
C
C ****  Look at correlations between missing Et and jet directions.
C
      DPHI1 = JETINFO(10, 1)
      DPHI2 = JETINFO(10, 2)
      DPHI3 = JETINFO(10, 3)

      IF ( ABS(DPHI1 - SNGL(PI)) .LT. N_DELTA*SNGL(PI)/64.0 ) GOTO 999
      IF ( ABS(DPHI2 - SNGL(PI)) .LT. N_DELTA*SNGL(PI)/64.0 ) GOTO 999
      IF ( ABS(DPHI3 - SNGL(PI)) .LT. N_DELTA*SNGL(PI)/64.0 ) GOTO 999

      PSI = SQRT ( (DPHI1-SNGL(PI))**2 + DPHI2**2)
      IF ( PSI .LT. PSI_CUT ) GOTO 999
C
C ****  Reject events with "good" electron or photon: ET above threshold and
C ****  chisquared below threshold.  If chisquared is missing from bank, or is
C ****  EXACTLY zero (sign of an error) then we do NOT reject the event.
C
      LPELC = GZPELC()
      DO WHILE (LPELC .GT. 0)
        ET = Q(LPELC+7)
        IF ( ET .GT. ELECTRON_ET_CUT ) THEN
          LHMTE = LQ(LPELC-IZHMTE)
          IF (LHMTE .GT. 0) THEN
            CHISQ = Q(LHMTE+7)
            IF ( (CHISQ .GT. 0) .AND.
     &         (CHISQ .LT. ELECTRON_CHISQ_CUT)) GOTO 999  ! Reject event
          ENDIF                              ! if lhmte .gt. 0
        ENDIF                              ! if et .gt. electron_et_cut
        LPELC = LQ(LPELC)
      ENDDO                                ! while lpelc .gt. 0

      LPPHO = GZPPHO()
      DO WHILE (LPPHO .GT. 0)
        ET = Q(LPPHO+7)
        IF ( ET .GT. ELECTRON_ET_CUT ) THEN
          LHMTP = LQ(LPPHO-IZHMTP)
          IF (LHMTP .GT. 0) THEN
            CHISQ = Q(LHMTP+7)
            IF ( (CHISQ .GT. 0) .AND.
     &         (CHISQ .LT. ELECTRON_CHISQ_CUT)) GOTO 999  ! Reject event
          ENDIF                              ! if lhmtp .gt. 0
        ENDIF                              ! if et .gt. electron_et_cut
        LPPHO = LQ(LPPHO)
      ENDDO                                ! while lppho .gt. 0

      NP_SQGL_TIGHT = .TRUE.
  999 CONTINUE
      CALL RESET_CAPH
      RETURN
C
      ENTRY NP_SQGL_TIGHT_EOJ(RSUM)
C
      CALL VZERO(RSUM,20)
      END
