      SUBROUTINE QCD_JET_TRIGGER_EFF( ETMIN, ETMAX, ETAMIN, ETAMAX,
     &  FILTER_NAME, IMASK, EFFIC, EFFIC_ERR, IER, IVERS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the trigger efficiency for jets in the
C-              specified kinematic range for the desired trigger. This
C-              routine uses a .hst4 file in the QCD area.
C-
C-   Inputs  : ETMIN  [R] : Minimum Jet ET
C-             MAXET  [R] : Maximum Jet ET
C-             ETMINA [R] : Minimum Jet Eta
C-             MAXETA [R] : Maximum Jet Eta
C-        FILTER_NAME [C*]: Name of L2 filter
C-             IMASK  [I] : Not used for now
C-
C-   Outputs : EFFIC  [R] : Jet Trigger efficiency for this FILTER for
C-                          jets in this kinematic region.
C-          EFFIC_ERR [R] : Error on Jet trigger efficiency
C-              IER   [I] : Error code- 0=OK
C-                                     -1=Illegal trigger name
C-                                     -2=Bad data-you are asking for a
c-                                        trigger efficiency in a kinematic
C-                                        region which is low in statistics.
C-             IVERS  [I] : Version number
C-   Controls:
C-
C-   Created  18-FEB-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:QCD_XS.PARAMS'
      REAL ETMIN, ETMAX, ETAMIN, ETAMAX, EFFIC, EFFIC_ERR
      REAL ETMIN1, ETMAX1, ETAMIN1, ETAMAX1, RAT, ERR
      INTEGER IETMIN, IETMAX, IETAMIN, IETAMAX, IVERS
      INTEGER IMASK, IER, IQCD_BIT, IHIST, IOFF, NDUM,NID
      INTEGER I,J,K, L, IUNIT, IERGT, LREC, ISTAT, KK, II
      CHARACTER*(*) FILTER_NAME
      CHARACTER*12 L2EFFDIR
      CHARACTER*4 TOPDIR
      INTEGER ID_ALL(5), ID_FOR(2), ID(5)
      LOGICAL FIRST, FORWARD, ABOVE_THRESH, CLEAN
      REAL EFFCUR
      REAL TRIG_ALL( 80, 15, 10 )         ! For ALL triggers
      REAL TRIG_FOR( 80, 5, 4  )         ! For FORWARD triggers
      REAL COUNT_TRIG( 5, 3 )
      REAL COUNT_ERR( 5, 3 )
      DATA ID_ALL / 1, 4, 5, 6, 7 /
      DATA ID_FOR / 2, 3 /
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      EFFIC = 0.
      EFFIC_ERR = -1.
      IER   = 0
      IVERS = 1                   ! First version
C
C: Make sure fiducial limits make sense. ET range is from 10. to 200. GeV
C: Eta goes from 0. to 3.
C
      ETMIN1 = MAX( 10., MIN( 200., ETMIN ) )
      ETMAX1 = MAX( 10., MIN( 200., ETMAX ) )
      ETAMIN1= MAX(  0., MIN( 3.00, ETAMIN))
      ETAMAX1= MAX(  0., MIN( 3.00, ETAMAX))
C                                     ! Find appropriate bin numbers
      IETMAX = (ETMAX1/1.001)/5 + 1 
      IETMIN = MIN( IETMAX, INT( ETMIN1/5 + 1) )          
      IETAMAX= (ETAMAX1/1.001)/.2 + 1
      IETAMIN= MIN( IETAMAX, INT(ETAMIN1/.2 + 1 ) )
C
C: Find out which trigger they want
C
      CALL QCD_NAME_TO_BIT( FILTER_NAME, IQCD_BIT )
      IF ( (IQCD_BIT .LT. 1 .OR. IQCD_BIT .GT. 6) .AND. IQCD_BIT .NE. 8
     &  ) THEN
        IER = -1                      ! Cant do this trigger
        GOTO 900
      ENDIF
      IF ( IQCD_BIT .EQ. 8 ) IQCD_BIT = 7
C
C: Setup depending whether we will run on ALL or FORWARD triggers
C
      IF ( IQCD_BIT .EQ. 2 .OR. IQCD_BIT .EQ. 3 ) THEN
        NID = 2
        FORWARD = .TRUE.
      ELSE
        NID= 5
        FORWARD = .FALSE.
      ENDIF
      DO I = 1, NID
        IF ( FORWARD ) ID(I) = ID_FOR(I)
        IF ( .NOT. FORWARD ) ID(I) = ID_ALL(I)
      ENDDO
C: Dont do anything more than we have to
      IF ( FORWARD ) THEN
        NID = IQCD_BIT - 1
      ELSE
        IF ( IQCD_BIT .EQ. 1 ) NID = 1
        IF ( IQCD_BIT .GE. 4 ) NID = IQCD_BIT - 2
      ENDIF

C
C: Read in Jet efficiency histograms
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL GTUNIT( IQCD_USER, IUNIT, IERGT )
        IF ( IERGT .NE. 0 ) STOP
     &    'QCD_JET_TRIGGER_EFF cant get unit number'
        LREC = 0
        TOPDIR = 'QCDT'
        CALL HROPEN( IUNIT, TOPDIR, 'D0$BETA:[QCD]QCD_L2EFF.HST4',' ',
     &    LREC, ISTAT)
        IF ( ISTAT .NE. 0 ) THEN
          PRINT *, ' Open of efficiency file failed ', ISTAT
          STOP
        ENDIF
        IOFF = 0
        IHIST =0
        L2EFFDIR = '//QCDT/LEFF'
        CALL HCDIR(L2EFFDIR, ' ')
        CALL HRIN( IHIST, 9999, IOFF )
        NDUM = 0
        DO I = 1, 5
          CALL HUNPAK(3000 + ID_ALL(I), TRIG_ALL(1,1,2*I-1), 'HIST',
     &      NDUM )
          CALL HUNPAK(4000 + ID_ALL(I), TRIG_ALL(1,1,2*I), 'HIST', NDUM
     &      )
        ENDDO
        DO I = 1, 2
          CALL HUNPAK(3000 + ID_FOR(I), TRIG_FOR(1,1,2*I-1), 'HIST',
     &      NDUM )
          CALL HUNPAK(4000 + ID_FOR(I), TRIG_FOR(1,1,2*I), 'HIST', NDUM
     &      )
        ENDDO
C: Special
        CALL HUNPAK(4991, TRIG_ALL(1,1,2), 'HIST', NDUM )
        CALL HCDIR('//PAWC',' ')
        CALL HREND(TOPDIR)
C
C: Cleanup the data
C
        CLEAN = ( .NOT. BTEST( IMASK, 0 ) )
        IF ( CLEAN ) THEN
          DO I = 1, 5                   ! For each trigger set
            DO K = 1, 15
              EFFCUR = 1.0
              ABOVE_THRESH = .TRUE.
              DO J = 80, 1, -1
                IF ( TRIG_ALL( J, K, 2*I-1 ) .LE. 0. ) TRIG_ALL( J, K,
     &            2*I-1) = 1.
                TRIG_ALL( J, K , 2*I ) = MIN( TRIG_ALL(J,K,2*I),
     &            TRIG_ALL(J,K,2*I-1) )
                IF ( TRIG_ALL( J, K, 2*I-1) .LE. 20. ) THEN
                  TRIG_ALL(J,K,2*I) = EFFCUR*TRIG_ALL(J,K,2*I-1)
                ELSE
                  EFFCUR = MIN(TRIG_ALL(J,K,2*I)/TRIG_ALL(J,K,2*I-1),
     &              EFFCUR)
                ENDIF
                IF ( EFFCUR .LT. .6 ) ABOVE_THRESH = .FALSE.
              ENDDO
            ENDDO
          ENDDO
          DO I = 1, 2                   ! For each trigger set
            KK = 5
            DO K = 1, KK
              EFFCUR = 1.0
              ABOVE_THRESH = .TRUE.
              DO J = 80, 1, -1
                IF ( TRIG_FOR( J, K, 2*I-1 ) .LE. 0. ) TRIG_FOR( J, K,
     &            2*I-1) = 1.
                TRIG_FOR( J, K , 2*I ) = MIN( TRIG_FOR(J,K,2*I),
     &            TRIG_FOR(J,K,2*I-1) )
                IF ( TRIG_FOR( J, K, 2*I-1) .LE. 20. ) THEN
                  TRIG_FOR(J,K,2*I) = EFFCUR*TRIG_FOR(J,K,2*I-1)
                ELSE
                  EFFCUR = MIN(TRIG_FOR(J,K,2*I)/TRIG_FOR(J,K,2*I-1),
     &              EFFCUR)
                ENDIF
                IF ( EFFCUR .LT. .6 ) ABOVE_THRESH = .FALSE.
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDIF
C
C: Sum up number of triggers in fiducial region
C
      CALL VZERO( COUNT_TRIG, 10 )
      CALL VZERO( COUNT_ERR, 10 )
      DO K = 1, NID
        DO I = IETMIN, IETMAX
          DO J = IETAMIN, IETAMAX
            IF ( .NOT. FORWARD ) THEN
              COUNT_TRIG( K,1 ) = COUNT_TRIG( K,1 ) + TRIG_ALL( I, J,
     &            2*K-1 )
              COUNT_TRIG(K,2) = COUNT_TRIG(K, 2) + TRIG_ALL( I, J, 2*K
     &            )
            ELSE
              COUNT_TRIG( K,1 ) = COUNT_TRIG( K,1 ) + TRIG_FOR( I, J,
     &            2*K-1 )
              COUNT_TRIG(K,2) = COUNT_TRIG(K, 2) + TRIG_FOR( I, J, 2*K
     &            )
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C: Determine efficiencies
C
      EFFIC = 1.
      EFFIC_ERR = 0.
      DO I = 1, NID
        RAT = COUNT_TRIG(I,2)/COUNT_TRIG(I,1)
        COUNT_TRIG(I,3) = RAT
        IF ( COUNT_TRIG(I,1) .GT. 1. ) THEN
          ERR = SQRT( RAT*(1.-RAT)/(COUNT_TRIG(I,1)-1.) )
        ELSE
          ERR = 0.
        ENDIF
        COUNT_ERR(I,3) = ERR
        EFFIC_ERR = SQRT( (EFFIC*ERR)**2 + (RAT*EFFIC_ERR)**2)
        EFFIC = EFFIC*RAT
      ENDDO


  900 CONTINUE
  999 RETURN
      END
