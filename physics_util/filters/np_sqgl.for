      LOGICAL FUNCTION NP_SQGL ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filtering for Squark and Gluino events.  This
C-   function expects to see DST format data.  The requirements are:
C-
C-     MISSING ET greater than the threshold MET_CUT
C-     ONE OR MORE jet(s)
C-     LEADING ET JET EM FRACTION greater than threshold J1_EMF_CUT
C-
C-   We use the full calorimeter, but not the muon system, in the missing Et
C-   calculation (PNUT(2) bank).
C-
C-   Returned value:  .TRUE. if the event is to be kept; .FALSE. if the event is
C-                    not wanted by this filter.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: control parameters in NP_SQGL_RCP
C-
C-   ENTRY NP_SQGL_EOJ  peforms end-of-run summary
C-
C-   Created  11-DEC-1992   Marc Paterno
C-   Updated  12-DEC-1992   Marc Paterno  Made to require at least on jet
C-   Updated  18-DEC-1992   Marc Paterno  Added summary statistics and entry
C-                                        NP_SQGL_EOJ
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      LOGICAL  NP_SQGL_EOJ
C----------------------------------------------------------------------
      INTEGER  NUM, IER, ET_OFFSET, LJETS, GZJETS, SORTBANKS
      REAL     SUMMARY(20)
      INTEGER  NSEEN, NPASS_MET, NPASS_JET, NPASS_EMF
      PARAMETER ( NUM = 2 )             ! PNUT bank number to use
      PARAMETER ( ET_OFFSET = 7 )       ! offset of ET word in JETS bank
      EXTERNAL GZJETS, SORTBANKS
      REAL     ENUT(4), ET, THETA, ETA, PHI, SIG(3)
      REAL     MET_CUT, J1_EMF_CUT, JET_RADIUS, TEMPLATE(3), J1EMF
      SAVE     MET_CUT, J1_EMF_CUT, TEMPLATE
      LOGICAL  FIRST
      SAVE     FIRST
      DATA     FIRST /.TRUE./
C----------------------------------------------------------------------
C
C ****  Initialization.  Failure to read RCP file correctly causes FATAL error.
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP ('NP_SQGL_RCP', IER)

        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ('FILTERING', 'NP_SQGL',
     &      'Could not find NP_SQGL_RCP', 'F')
        ENDIF                           ! if ier .ne. 0

        CALL EZPICK ('NP_SQGL_RCP')

        CALL EZGET ('MET_CUT', MET_CUT, IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ('FILTERING', 'NP_SQGL',
     &      'Could not find parameter MET_CUT', 'F')
        ENDIF                           ! if ier .ne. 0

        CALL EZGET ('J1_EMF_CUT', J1_EMF_CUT, IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ('FILTERING', 'NP_SQGL',
     &      'Could not find parameter J1_EMF_CUT', 'F')
        ENDIF                           ! if ier .ne. 0

        CALL EZGET ('JET_RADIUS', JET_RADIUS, IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ('FILTERING', 'NP_SQGL',
     &      'Could not find parameter JET_RADIUS', 'F')
        ENDIF                           ! if ier .ne. 0

        TEMPLATE(1) = 1
        TEMPLATE(2) = 6
        TEMPLATE(3) = JET_RADIUS

        NSEEN = 0
        NPASS_MET = 0
        NPASS_JET = 0
        NPASS_EMF = 0
      ENDIF                             ! if first
C
C ****  Beginning of event processing
C
      NP_SQGL = .FALSE.                 ! default action -- reject event
      NSEEN = NSEEN + 1
C
C ****  Require missing Et greater than MET_CUT, or event is rejected.  If no
C ****  PNUT(2) bank is found, the event is kept, and a warning is issued.
C
      CALL GTPNUT ( NUM, ENUT, ET, THETA, ETA, PHI, SIG, IER)

      IF ( IER .EQ. 0 ) THEN
        IF ( ET .LT. MET_CUT ) RETURN   ! reject the event
      ELSE
        CALL ERRMSG ('FILTERING', 'NP_SQGL',
     &      'GTPNUT error for this event', 'W')
      ENDIF                             ! if ier .eq. 0
      NPASS_MET = NPASS_MET + 1
C
C ****  Require that the leading ET jet have electromagnetic ET fraction greater
C ****  than J1_EMF_CUT; we are also requiring the existence of at least one
C ****  jet.  We use CONE jet with radius JET_RADIUS.  If no CAPH for this
C ****  radius is found, we keep the event and issue a warning.
C
      CALL SET_CAPH ('CONE_JET', TEMPLATE, IER)
      IF ( IER .EQ. 0 ) THEN
        LJETS = GZJETS()

        IF ( LJETS .GT. 0 ) THEN
          NPASS_JET = NPASS_JET + 1
          LJETS = SORTBANKS(LJETS, ET_OFFSET, 'DESCENDING', 'FLOAT')
          J1EMF = Q(LJETS+14)                ! em fraction

          IF ( J1EMF .LT. J1_EMF_CUT) THEN
            CALL RESET_CAPH
            RETURN                           ! reject the event
          ENDIF                              ! if j1emf .lt. j1_emf_cut
        ELSE
          CALL RESET_CAPH
          RETURN
        ENDIF                                ! if ljets .gt. 0

      ELSE
        CALL ERRMSG ('FILTERING', 'NP_SQGL',
     &      'Could not find CAPH for correct jets for this event', 'W')
      ENDIF
      CALL RESET_CAPH
      NPASS_EMF = NPASS_EMF + 1
C
C ****  If we have made it here, the event is to be kept
C
      NP_SQGL = .TRUE.
      RETURN
C#######################################################################
      ENTRY NP_SQGL_EOJ (SUMMARY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End of job summary for NP_SQGL filter.
C-
C-   Inputs  : none
C-   Outputs : SUMMARY   [R(20)]   SUMMARY (1) = number of events seen
C-                                 SUMMARY (2) = number of events passing the
C-                                                missing Et cut.
C-                                 SUMMARY (3) = number of events passing 1 jet
C-                                                cut.
C-                                 SUMMARY (4) = number of events passing EMF
C-                                                cut.
C-   Controls: none
C-
C-   Created  18-DEC-1992   Marc Paterno
C-
C----------------------------------------------------------------------
      NP_SQGL_EOJ  = .TRUE.

      CALL VZERO (SUMMARY, 20)
      SUMMARY(1)  = FLOAT( NSEEN )
      SUMMARY(2)  = FLOAT( NPASS_MET )
      SUMMARY(3)  = FLOAT( NPASS_JET )
      SUMMARY(4)  = FLOAT( NPASS_EMF )

      RETURN
      END
