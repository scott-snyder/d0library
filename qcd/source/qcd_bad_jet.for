      SUBROUTINE QCD_BAD_JET( IJET, BAD_FLAG )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the BAD_FLAG for a jet.
C
C-   Inputs  : IJET     [I] Ith jet in loop over jets
C-   Outputs : BAD_FLAG [I] Bit mask flag
C-                          bit 1 = Failed CH fraction cut
C-                          bit 2 = Failed Hot cell cut
C-                          bit 3 = Failed L1-L2 cut
C-                          bit 4 = Failed EM fraction cut
C-   Controls:
C-
C-   Created  17-DEC-1992   Richard V. Astur
C-   Modified  5-JAN-1993   Andrew G. Brandt use GTJETS_ALL
C-   Modified 13-FEB-1993   R. Astur (Disable if JETS is version 2 or less)
C-   Modified 21-MAR-1993   R. Astur (generalize for STA's using L1-L2 cut)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$LINKS:IZJPTS.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IJET,BAD_FLAG
C
      INTEGER IJ,IVERS,ISPL,IER
      REAL    E(5),SIG(4),HOT(3)
      REAL    THETA,PHI,ETA,EMFRAC
C
      REAL CH_FRACTION_MAX, EM_FRACTION_MIN, EM_FRACTION_MAX
      REAL HOT_FRACTION_MIN
      PARAMETER( CH_FRACTION_MAX = .4 )
      PARAMETER( EM_FRACTION_MIN = .1 )
      PARAMETER( EM_FRACTION_MAX = 1.01)
      PARAMETER( HOT_FRACTION_MIN= .1 )
c: Use K. Streets' program
      REAL OFF_EM(NPHIL1, -NETAL1:NETAL1)
      REAL OFF_HAD(NPHIL1,-NETAL1:NETAL1)
      REAL CAL_ET_CHAN(NPHIL1,-NETAL1:NETAL1,56)
      REAL L1_SATURATE(20,2)                        ! 1=EM 2=HD
      DATA L1_SATURATE /
     &   60., 60., 60., 60., 55., 48., 38., 29., 23.,
     &   19., 16., 12., 10.,  8.,  7.,  6.,  2., 2., 2., 2.,
     &   60., 60., 60., 60., 60., 60., 46., 40., 30.,
     &   28., 22., 18., 13., 11.,  9.,   7., 2., 2., 2., 2. /
      REAL L1_SLOPE(20,2), L1_INTER(20,2)
      DATA L1_SLOPE /
     &  .97, .96, 1.08, 1.09, .96, 1.05, .90, 1., 1., 1., .80, .97, .94,
     &  .75, .94, .92, 1., 1., 1., 1.,
     &  1.3, 1.13, 1.23, 1.17, .94, 1., .77, 1.19, 1.06, 1.25, 1.19,
     &  1.38, 1.27, 1.1, 1.0, 1.5, 1., 1., 1., 1. /
      DATA L1_INTER /
     &  -5.8, -5., -8.5, -7.8, -6., -6., -3.5, -2.0, -3., -2., 0., -2.2,
     &  -2.67, 0., -1.5, -.8, 0., 0., 0., 0.,
     &  -13., -9.2, -8., -7.3, -3.5, -7., -8.7, -2.8, -5.4, -2., -2.4,
     &  -3., -1.7, -1., 0., -2.0, 0., 0., 0., 0./
      INTEGER NEM_BAD, NHD_BAD
      PARAMETER( NEM_BAD=3)
      PARAMETER( NHD_BAD=2)
      INTEGER L1_BAD_HD(2,NHD_BAD), L1_BAD_EM(2,NEM_BAD), IEHBAD
      DATA L1_BAD_EM / -6, 12,
     &                  9, 24,
     &                  -14, 27 /
      DATA L1_BAD_HD / -4, 21,
     &                 -3, 17 /
      LOGICAL BAD_TOWER
      INTEGER RORANGE(2,2), II, JJ,LJETS, GZJETS, LJPTS, GZCAEH, LCAEH
      INTEGER NR,NCELL, POINT, I, IETA, IPHI,K, J
      REAL EMET, TOTET
      CHARACTER*4 OLD_PATH
C----------------------------------------------------------------------
C
C Get jet information necessary to use routine
C
      IJ=IJET
      CALL GTJETS_ALL(IJ,IVERS,E,THETA,PHI,ETA,SIG,EMFRAC,ISPL,
     +                NCELL,HOT,IER)
C
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('No jets bank','QCD_BAD_JET',
     &          'No jets bank found','W')
        BAD_FLAG = -1
        GOTO 999
      ELSE IF ( IVERS .LT. 3 ) THEN
        BAD_FLAG = -1
        CALL ERRMSG('Old version', 'QCD_BAD_JET',
     &    'JETS bank uses version 2 or less',
     &    'W')
        GOTO 999
      END IF
C
C: Set to OK
      BAD_FLAG = 0
C
C: Apply most of these cuts by using info that is available in the DST.
C
      CALL QCD_BAD_DST( IJ, BAD_FLAG )

C
C: Now apply L1-L2 cut
C
C
      CALL PATHGT(OLD_PATH)
      IF (OLD_PATH .NE. 'RECO' ) GOTO 999
C: Kall Kathy's routine
      CALL TRIG_OFF(OFF_EM,OFF_HAD,CAL_ET_CHAN)
C: Get pointer to JETS bank for this guy
      I = IJ-1
      LJETS = GZJETS()
      DO WHILE (I .GT. 0 )
        I = I - 1
        IF ( LJETS .GT. 0 ) LJETS = LQ(LJETS)
      ENDDO
      IF ( LJETS .GT. 0 ) LJPTS = LQ( LJETS - IZJPTS )
      LCAEH = GZCAEH()
      IF ( LJETS .LE. 0 .OR. LJPTS .LE. 0 .OR. LCAEH .LE. 0) THEN
        CALL ERRMSG('Not enough info','QCD_BAD_JET',
     &    'Cant find JETS or JPTS or CAEH','W')
        GOTO 998
      ENDIF
C
C: Loop over all trigger towers in this jet
C
C: Init
      RORANGE(1,1) =  NETAL + 1
      RORANGE(2,1) = -NETAL - 1
      RORANGE(1,2) =  NPHIL + 1
      RORANGE(2,2) =  0
C
      NR = IQ( LCAEH + 2)
      NCELL = IQ( LJPTS + 2)
      DO I = 1,NCELL
        POINT = LCAEH + IQ(LJPTS+ 2 + I)*NR - NR
        IETA  = IQ( POINT + 12)
        IPHI  = IQ( POINT + 13)
        RORANGE(1,1) = MIN( RORANGE(1,1), IETA )
        RORANGE(2,1) = MAX( RORANGE(2,1), IETA )
        RORANGE(1,2) = MIN( RORANGE(1,2), IPHI )
        RORANGE(2,2) = MAX( RORANGE(2,2), IPHI )
      ENDDO
C
      DO II = RORANGE(1,1), RORANGE(2,1)
        DO JJ = RORANGE(1,2), RORANGE(2,2)
C: Convert from RO tower to TT
          IF ( II .NE. 0 .AND. ABS(II) .LT. 37) THEN
            I = (II-SIGN(1,II))/2 + SIGN( 1, II )
            IF ( II .GE. 33 ) I = (II-33) + 17      ! Coarsening
            J = (JJ-1)/2 + 1
            CALL CL1PHET(J, I, EMET, TOTET )
C: Dont look at certain bad towers
            DO K = 1, NEM_BAD
              IF (J .EQ. L1_BAD_EM(2,K) .AND. I .EQ. L1_BAD_EM(1,K) )
     &          GOTO 330
            ENDDO
            IF ( EMET .GT. 3. .AND. OFF_EM(J,I) .GT. 3. .AND. EMET 
     &            .LT. L1_SATURATE(ABS(I),1) ) THEN
              BAD_TOWER = ( EMET .LT. L1_SLOPE(ABS(I),1)*OFF_EM(J,I)
     &              +L1_INTER(ABS(I),1) )
              IF ( BAD_TOWER) BAD_FLAG = IBSET(BAD_FLAG, 2 )
            ENDIF
  330       CONTINUE
            DO K = 1, NHD_BAD
              IF (J .EQ. L1_BAD_HD(2,K) .AND. I .EQ. L1_BAD_HD(1,K) )
     &          GOTO 340
            ENDDO
            IF ( TOTET-EMET .GT. 3. .AND. OFF_HAD(J,I) .GT. 3. .AND.
     &            TOTET-EMET .LT. L1_SATURATE(ABS(I),2) ) THEN
              BAD_TOWER = ( TOTET-EMET .LT. L1_SLOPE(ABS(I),2)
     &              *OFF_HAD(J,I) + L1_INTER(ABS(I),1))
              IF ( BAD_TOWER) BAD_FLAG = IBSET(BAD_FLAG, 2 )
            ENDIF
  340       CONTINUE

          ENDIF
        ENDDO
      ENDDO
  998 CONTINUE
  999 RETURN
      END
