      SUBROUTINE GTJAUX(START, CONE_SIZE, PAR_SET, ET, ETA, PHI, L1ETA,
     &  L1PHI, ETASIZE, PHISIZE, EMET, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return L2JETS found in the JAUX bank. Set START
C-    to .TRUE. and keep calling while IER = 0. The other output values are
C-    not valid when IER is not 0.
C-
C-
C-   Inputs  :      [L]   START     : Set to .TRUE. to get first jet
C-                  [R]   CONE_SIZE : Cone size desired (.1 resolution)
C-                                  : if CONE_SIZE <0. then do all
C-                  [I]   PAR_SET   : parameter set required (1:128) 0=all
C-
C-   Outputs :      [R]   ET        : ET of L2 jet
C-                  [R]   ETA       : Eta of L2 jet
C-                  [R]   PHI       : Phi of L2 jet
C-                  [I]   L1ETA     : l1 eta index of L1 candidate tower
C-                  [I]   L1PHI     : L1 phi index of L1 candidate tower
C-                  [R]   ETASIZE   : Eta width of jet
C-                  [R]   PHISIZE   : Phi width of jet
C-                  [R]   EMET      : Em ET of L2 jet
C-                  [I]   IER       : 0 = Okay
C-                                   -1 = No additional jets found.
C-                                   -2 = No JAUX bank
C-                                   -3 = cone size not found
C-   Controls:
C-
C-   Created  13-SEP-1992   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:JAUX.PARAMS'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL START, START_JPAR
      INTEGER IER, L1ETA, L1PHI
      REAL CONE_SIZE, ET, EMET, PHI, ETA, ETASIZE, PHISIZE
      INTEGER I, LJAUX, GZJAUX, NR, NPAR_MAX, NJT_HOT, PAR_SET, ICOUNT
      INTEGER NCOUNT, IND_PAR_SET, IER_JPAR, IP, PAR_SET1, IOFF
      INTEGER ESUM_CONESIZE_TO_JET, PAR_SET_LOW, PAR_SET_HIGH
      REAL ET_CUT, CORE_RADIUS, TOTAL_RADIUS, MAXRAD, MINRAD
      REAL  EMFRACT_MIN, EMFRACT_MAX
      LOGICAL VETO_THIS
      SAVE ICOUNT, IND_PAR_SET
C----------------------------------------------------------------------
C
C: Is there even a JAUX bank present?
C
      IER = -2              ! No JAUX bank
      LJAUX = GZJAUX()
      IF ( LJAUX .LE. 0 ) RETURN
C
C: Find out what they want
C
      IF ( START ) THEN     
C: Get the word and the bit where this parameter set is stored
        PAR_SET1 = MIN( MAX( PAR_SET, 0 ), 128 )    ! Legal is from 0-128
        IOFF = PAR_SET1/32
        PAR_SET1 = PAR_SET1 - 32*IOFF
C: Which independant parameter sets do we want. Default = all. 
        PAR_SET_LOW = 1
        PAR_SET_HIGH= IQ( LJAUX + 4 )
C
C: Choose the independant parameter set which corresponds to this conesize
C
        IF ( CONE_SIZE .GT. 0. ) THEN     ! They want to specify this
          START_JPAR = .TRUE.
  100     CALL GTJPAR(START_JPAR, NCOUNT, ET_CUT, CORE_RADIUS,
     &      TOTAL_RADIUS, MAXRAD, MINRAD, EMFRACT_MIN, EMFRACT_MAX,
     &      IND_PAR_SET, IER_JPAR, VETO_THIS )
          IF ( IER_JPAR .EQ. 0 ) THEN
            IF ( ABS( TOTAL_RADIUS - CONE_SIZE ) .LE. .1 ) THEN
              PAR_SET_LOW = IND_PAR_SET       ! Only this set
              PAR_SET_HIGH= IND_PAR_SET
              GOTO 200
            ENDIF
            GOTO 100
          ENDIF
C
C: Oops. Could not determine independent parameter set from JPAR. Maybe we
C: can get this information from ESUM
C
          IND_PAR_SET = ESUM_CONESIZE_TO_JET( 'FILT', CONE_SIZE,
     &      IER_JPAR)
          IF ( IER_JPAR .EQ. 0 ) THEN
            IND_PAR_SET = MAX( 1, MIN( IQ(LJAUX+4),IND_PAR_SET -
     &        ID_JET_1
     &        + 1))
            PAR_SET_LOW = IND_PAR_SET
            PAR_SET_HIGH= IND_PAR_SET
            GOTO 200
          ENDIF
          IER = -3
          RETURN
        ENDIF
  200   CONTINUE
        START = .FALSE.
        IND_PAR_SET = PAR_SET_LOW
        ICOUNT      = 0
      ENDIF
C
C: Get all those JAUX entries that match the specified L2 parameter
C: set (PAR_SET1) while looking at those cone algorithms they specified
C: Choose the ICOUNT jet of the IND_PAR_SET set
C
      NR = IQ( LJAUX + 2)
      NJT_HOT = IQ( LJAUX + 3 )
  300 ICOUNT = ICOUNT + 1
      IF ( ICOUNT .GT. NJT_HOT ) THEN
        IND_PAR_SET = IND_PAR_SET + 1
        ICOUNT = 1
      ENDIF
      IF ( IND_PAR_SET .GT. PAR_SET_HIGH ) THEN
        IER = -1
        RETURN
      ENDIF
      IP = LJAUX + (ICOUNT-1)*NR + ( IND_PAR_SET - 1 )*NR*NJT_HOT
      IF ( IQ( IP + PJEVT ) .EQ. L2J_DONE ) THEN
        IF ( PAR_SET1 .EQ. 0 .OR. BTEST( IQ(IP+14+IOFF), PAR_SET1-1) )
     &      THEN
          IER = 0
          ET = Q( IP + PJET )
          ETA= Q( IP + PJETA)
          PHI= Q( IP + PJPHI)
          L1ETA = IQ( IP + PJIETA )
          L1PHI = IQ( IP + PJIPHI )
          ETASIZE= Q( IP + PJETASIZ )
          PHISIZE= Q( IP + PJPHISIZ )
          EMET   = Q( IP + PJEMFR )
          RETURN
        ENDIF
      ENDIF
      GOTO 300
      END
