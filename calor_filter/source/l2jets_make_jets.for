      SUBROUTINE L2JETS_MAKE_JETS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make a RECO style set of JETS banks from the
C-      Level 2 tool L2JETS result bank:JAUX.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  22-JUL-1991   Richard Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:L2LINK.INC'               ! zebra link area
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$PARAMS:JAUX.PARAMS'
      INTEGER GZJAUX,GZJPAR,LCAPH,LJPAR,NUM_PAR,IPAR
      INTEGER NUM_JETS,NUM_IND_PARAMS,NUM_PARAMS,I,IP,IJ
      INTEGER NREPJAUX, NREP_JPAR, I_IND_PARAM
      CHARACTER*4 OLD_PATH
      REAL ETCUT, CONE
      REAL THETA_FROM_ETA, RETA                 ! Statement function
      LOGICAL BTEST
C&IF VAXVMS,VAXELN
C&ELSE
C&      EXTERNAL BTEST
C&ENDIF
C----------------------------------------------------------------------
      THETA_FROM_ETA(RETA ) = 2*ATAN(EXP(-(RETA)))
C
      CALL PATHGT(OLD_PATH)
      CALL PATHST('FILT')               ! Set to FILT

C: Init temp link area.
      IF (DUM(1) .NE. 0) THEN
        CALL ERRMSG('LINK AREA SET','L2JETS_MAKE_JETS',
     &    'Error: Link area already used','F')
        GOTO 900
      END IF
      CALL MZLINT(IXCOM,'/L2LINK/',DUM,L2LINK(NLNK),DUM)

C: Get JAUX bank address.
      LJAUX = GZJAUX()
      IF (LJAUX .LE. 0) THEN
        CALL ERRMSG('No JAUX bank','L2JETS_MAKE_JETS',
     &    'Cant make JETS without JAUX bank','W')
        GOTO 900
      END IF

C: Define some values from JAUX
      NREPJAUX = IQ(LJAUX + 2)        ! Repetition number for JAUX
      NUM_JETS  = IQ(LJAUX+3)         ! Number of jet candidates
      NUM_IND_PARAMS = IQ(LJAUX+4)    ! Number of INDEPENDENT sets

C: Get JPAR bank address
      LJPAR = GZJPAR()
      IF (LJPAR .LE. 0) THEN
        CALL ERRMSG('No JPAR bank','L2JETS_MAKE_JETS',
     &    'Cant make JETS without JPAR bank','W')
        GOTO 900
      END IF
      NREP_JPAR= IC(LJPAR+ 2) + 1     ! Repetition number for JPAR
      NUM_PAR =  IC(LJPAR + 2)        ! # of parameter cuts
      NUM_PARAMS = IC(LJPAR + 3)      ! # of parameter sets

C: Loop over PARAMETER sets. For each one, make a CAPH bank and
C: include all the jets which passed this set. Note, that this does
C: NOT include the jet count cut.
      DO WHILE (NUM_PARAMS .GT. 0)
C: Find which individual PARAMETER set this corresponds to
C: Get its ETCUTS and cone size as well.
        I_IND_PARAM = NINT( C( LJPAR + (NUM_PARAMS-1)*NREP_JPAR + 12) )
        ETCUT       = C( LJPAR + (NUM_PARAMS-1)*NREP_JPAR + 5)
        CONE        = C( LJPAR + (NUM_PARAMS-1)*NREP_JPAR + 7)
C: Book CAPH
        CALL BKCAPH(LCAPH)
        IF (LCAPH .LE. 0) THEN
          CALL ERRMSG('Book Failure',
     &        'L2JETS_MAKE_JETS','CAPH book failure','F')
          RETURN
        END IF
        IQ(LCAPH + 2) = 0             ! No CACL banks used
        IQ(LCAPH + 3) = 0             ! Set at 0 and increment
        IQ(LCAPH + 4) = 2             ! Cone algorithm
        IQ(LCAPH + 5) = 3             ! Version Number
        Q(LCAPH  + 6) = CONE          ! Cone size
        Q(LCAPH  + 7) = ETCUT         ! Et min
        Q(LCAPH  + 8) = 0.0           ! Et split fraction
        Q(LCAPH  + 9) = 0.0           ! Min cluster separation
        Q(LCAPH  +10) = 0.0           ! seed min et
        Q(LCAPH  +11) = 0.0           ! Cand min et
        Q(LCAPH  +12) = 0.0           ! Precluster min et
C: Find which jets passed these cuts
        DO I = 1,NUM_JETS
          IP = LJAUX + (I-1)*NREPJAUX +
     &      NUM_JETS*NREPJAUX*(I_IND_PARAM-1)

C: Make Jets bank for qualifying jets.
          IF (IQ(IP+PJEVT) .EQ. L2J_DONE .AND.
     &      BTEST( IQ(IP + PJMASK + (NUM_PARAMS-1)/32 ),
     &      MOD( NUM_PARAMS-1,32))) THEN
            IQ(LCAPH + 3 ) = IQ(LCAPH + 3) + 1        ! Incr. Jet counter
            CALL BKJETS(LJETS)
            IF (LJETS .LE. 0) THEN
              CALL ERRMSG('Book Failure',
     &          'L2JETS_MAKE_JETS','JETS book failure','F')
              RETURN
            END IF
            Q(LJETS + 2 ) = 0.        ! PX
            Q(LJETS + 3 ) = 0.        ! PY
            Q(LJETS + 4 ) = 0.        ! PZ
            Q(LJETS + 6 ) = Q(IP + PJET)  ! ET
            Q(LJETS + 7 ) =           THETA_FROM_ETA(Q(IP+PJETA)) !theta
            IF (SIN(Q(LJETS+7)) .GT. .001)
     &          Q(LJETS+5) = Q(LJETS+6)/SIN(Q(LJETS+7))             ! E
            Q(LJETS + 8 ) =           Q(IP + PJPHI) ! PHI
            Q(LJETS + 9 ) =           Q(IP + PJETA) ! ETA
            Q(LJETS + 10) =           0.            ! SIG**2(EX)
            Q(LJETS + 11) =           0.            ! SIG**2(EY)
            Q(LJETS + 12) = Q(IP + PJETASIZ)/( Q(IP + PJET) + ZEERO)
            Q(LJETS + 13) = Q(IP + PJPHISIZ)/( Q(IP + PJET) + ZEERO)
            Q(LJETS + 14) = Q(IP + PJEMFR)/ ( Q(IP+PJET) + ZEERO)
            IQ(LJETS + 15) = 0
          END IF
        END DO                        ! Next jet
        NUM_PARAMS = NUM_PARAMS - 1
      END DO
  900 CONTINUE
      DUM(1) = 0                        ! Deactivate link area
      CALL PATHST(OLD_PATH)             ! Reset path
  999 RETURN
      END
