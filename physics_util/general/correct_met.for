      FUNCTION CORRECT_MET()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correct the missing Et (PNUT) banks in a standard
C-   DST.
C-
C-   Returned value  : .TRUE. if successful, .FALSE. on error
C-   Inputs  : none
C-   Outputs : none
C-   Controls: parameters in CORRECT_MET_RCP
C-
C-   ENTRY CORRECT_MET_SET_REPEAT(control)
C-
C-
C-   Created   3-OCT-1993   Marc Paterno
C-   Updated   7-OCT-1993   Marc Paterno  Indicate failure if correct jet
C-                                        template not found.
C-   Updated  15-OCT-1993   Marc Paterno  Added CORRECT_MET_SET_REPEAT
C-   Updated   9-MAY-1995   R. Astur      New soft correction, add MRFN
C-                                        to VCOR's considered, new noise
C-                                        routine.
C-   Updated  13-AUG-1996   Bob Hirosky -- update for cafix51
C-                                      make template choose unique 0.7
C-                                      cone jet algoritm for d0fix
C-   Updated   7-AUG-1997   Bob Hirosky -- drop SOFT correction
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'd0$links:izreco.link'
      INCLUDE 'D0$LINKS:IZJNEP.LINK'
      LOGICAL  CORRECT_MET, CORRECT_MET_SET_REPEAT, YES
C----------------------------------------------------------------------
      INTEGER  GZPELC, GZRECO, LVCOR
      EXTERNAL GZRECO, GZPELC
      LOGICAL  USE_JET_CORRECTION
      LOGICAL  WANT_SOFT_CORRECTION
      SAVE     WANT_SOFT_CORRECTION
      LOGICAL  FIRST, DO_MET_CORRECTION
      LOGICAL  REPEAT_CORRECTION, CONTROL, EM_IN_JET
      EXTERNAL EM_IN_JET
      INTEGER  ERRSUM, IER
C: functions
      INTEGER  GZPNUT, GZJETS
      LOGICAL  EM_FITS_MASK
      EXTERNAL GZPNUT, GZJETS, EM_FITS_MASK
      EXTERNAL CORRECT_MET_NUM_INTERACTIONS
      REAL     CORRECT_MET_NUM_INTERACTIONS
C: other
      LOGICAL OK
      INTEGER I, NZV, NVWANTED
      PARAMETER (NVWANTED = 1)
      REAL VINFO(3, NVWANTED), Z_VERTEX
      REAL WIDTH_FACTOR, DRETA, DREMF, DPHIMIN
      REAL DPHI, DRMIN, DUM1, DUM2
      INTEGER SOFT_MATCH
      CHARACTER*80 MSG
C: ELECTRON/PHOTON
      REAL EEL(4), ETEL, SIGEL(4), CONE_NRG(5), DIST
      REAL THETA, PHI, ETA
      INTEGER NUM_TRACKS, NELEC, NPHOT
      INTEGER ISOLATION_MASK

C: JETS
      INTEGER LBANK, LJETS, NJETS, ISYS
      SAVE    ISYS
      REAL EJ(7), TEMPLATE(9), UNDER_E, UNDER_ET, ZSP_E, ZSP_ET
      DATA TEMPLATE / 4., 6., .7, 10., 1.0, 11., 1.0, 12., 1.0 /
      SAVE TEMPLATE
      LOGICAL MET_CORRECT
      LOGICAL DO_ZSP, DO_UNDER, DO_OUT_OF_CONE    ! For jet correction
      PARAMETER( DO_ZSP = .TRUE. )                ! Do do noise subtraction
      PARAMETER( DO_UNDER = .TRUE. )              ! Do subtract under. event
      PARAMETER( DO_OUT_OF_CONE = .FALSE. )       ! Dont correct for out of
C: PNUT
      INTEGER LPNUT, IVERSION
      REAL ENUT(4), SIGNUT(3), ETNUT
      REAL RECOIL_ET, RECOIL_PHI, ETX, ETY, METX, METY
      REAL OLD_SOFT_ETX, OLD_SOFT_ETY, OLD_RECOIL_ET
      REAL VCOR_DELTA(5), VCOR_SIGMA(5)
C
      SAVE     FIRST, DO_MET_CORRECTION
      SAVE     REPEAT_CORRECTION
      DATA MSG /' MET CORRECTION V3.0 - Em, jet, soft jet corrections'/
      DATA     FIRST /.TRUE./
C----------------------------------------------------------------------
C    Initialization
C----------------------------------------------------------------------
      CORRECT_MET       = .TRUE.
      DO_MET_CORRECTION = .TRUE.

      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL INTMSG(MSG)

        CALL INRCP ('CAFIX_RCP', IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ('NO RCP', 'CORRECT_MET',
     &  'Could not find CAFIX_RCP, will not do MET correction',
     &  'W')
          DO_MET_CORRECTION = .FALSE.
        ELSE
          CALL EZPICK ('CAFIX_RCP')
          ERRSUM = 0
c          CALL EZGET ('DO_SOFT_CORRECTION', WANT_SOFT_CORRECTION, IER)
c          ERRSUM = ERRSUM + ABS(IER)
          CALL EZGET ('REPEAT_CORRECTION', REPEAT_CORRECTION, IER)
          ERRSUM = ERRSUM + ABS(IER)
C
          USE_JET_CORRECTION = .TRUE.       ! Default
          CALL EZGET ('USE_JET_CORRECTION', USE_JET_CORRECTION, IER)
C
          CALL EZGET( 'ISYS',ISYS, IER )
          ERRSUM = ERRSUM + ABS(IER)
C
          CALL EZRSET

          IF (ERRSUM .NE. 0) THEN
            CALL ERRMSG ('CORRECT_MET', 'CORRECT_MET',
     & 'Missing params in CAFIX_RCP: will not do MET correction',
     & 'W' )
            CORRECT_MET = .FALSE.
            DO_MET_CORRECTION  = .FALSE.
          ENDIF                         ! if errsum .ne. 0
        ENDIF                           ! if ier .ne. 0

        CALL INRCP ('CORRECTEM_RCP', IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ('NO RCP', 'CORRECT_MET',
     & 'Could not find CORRECTEM_RCP, will not do MET correction',
     & 'W')
          DO_MET_CORRECTION = .FALSE.
        ELSE
          CALL EZPICK ('CORRECTEM_RCP')
          ERRSUM = 0
          CALL EZGET ('ECORRECT_MASK', ISOLATION_MASK, IER)
          ERRSUM = ERRSUM + ABS(IER)
          CALL EZRSET

          IF (ERRSUM .NE. 0) THEN
            CALL ERRMSG ('CORRECT_MET', 'CORRECT_MET',
     & 'Missing params in CORRECTEM_RCP: will not do MET correction',
     & 'W' )
            CORRECT_MET = .FALSE.
            DO_MET_CORRECTION  = .FALSE.
          ENDIF                         ! if errsum .ne. 0
        ENDIF                           ! if ier .ne. 0

        IF (.NOT. DO_MET_CORRECTION ) THEN
          CALL ERRMSG ('CORRECT_MET', 'CORRECT_MET',
     &        'Missing ET corrections turned off', 'I')
        ENDIF                           ! if .not. do_correction

      ENDIF                             ! if first
C----------------------------------------------------------------------
      IF ( .NOT. DO_MET_CORRECTION ) RETURN
C
C ****  Make sure we have PNUT(2).  Check on existence of PNUT(3).
C
      IF (GZPNUT(2) .LE. 0) THEN
        CALL ERRMSG ('CORRECT_MET', 'CORRECT_MET',
     &    'No PNUT(2) in this event', 'W')
        CORRECT_MET = .FALSE.
        RETURN
      ENDIF
C
C ****  Check to see if correction is already done, and if it should be
C ****  repeated.
C
      DO I = 4, 5
        IF (GZPNUT(I) .GT. 0) THEN
          IF (REPEAT_CORRECTION) THEN
            LPNUT = GZPNUT(I)
            IF (LPNUT .GT. 0) CALL MZDROP (IXCOM, LPNUT, '.')
          ELSE
            CALL ERRMSG ('CORRECT_MET', 'CORRECT_MET',
     &        'Missing ET corrections already done', 'I')
            RETURN
          ENDIF
        ENDIF
      ENDDO
C
C **** Drop any old  JETS VCOR banks
C
      IER = 0
      DO WHILE ( IER .EQ. 0 )
        CALL GTVCOR('JETS', 1, IVERSION, VCOR_DELTA, VCOR_SIGMA, IER )
        IF ( IER .EQ. 0 ) THEN
          CALL GTVCOR_POINTER( LVCOR )
          CALL MZDROP(IXMAIN, LVCOR, ' ')
        ENDIF
      ENDDO

C
C ****  Get the z of the vertex
C
      Z_VERTEX = 0.0
      CALL VERTEX_INFO (NVWANTED, NZV, VINFO, OK)
      IF (OK) THEN
        Z_VERTEX = VINFO(1, 1)             ! first vertex, 0 if no vertex
      ELSE
        Z_VERTEX = 0.0
      ENDIF
C
C ****  Get PNUT 2 missing Et
C
      CALL GTPNUT(2, ENUT, ETNUT, THETA, ETA, PHI, SIGNUT, IER )
      IF (IER .NE. 0) THEN
        CALL ERRMSG ('CORRECT_MET', 'CORRECT_MET',
     &    'GTPNUT returns error condition for PNUT(2)', 'W')
        CORRECT_MET = .FALSE.
        RETURN
      ENDIF
      METX = ENUT(1)
      METY = ENUT(2)
C
C ****  Init the jet algorithm we will use; reset counters
C
      NELEC = 0
      NPHOT = 0
      NJETS = 0
C
C ****  Find recoil vector by subtracting out all PELC/PPHO (electrons/photons)
C ****  and then subtract out all the jets (using JNEP which is equal to the
C ****  JETS with any associated PELC/PPHOs subtracted out.
C
      ETX = -METX
      ETY = -METY
C
C ****  Loop over Electron
C
      CALL GTPELC_TOTAL( NELEC, IER )
      IF ( IER .NE. 0 ) NELEC = 0
C
      DO I = 1, NELEC
        CALL GTPELC_UNCORRECTED                       ! Get uncorrected values
        CALL GTPELC(I, EEL, ETEL, SIGEL, THETA, ETA, PHI,
     &          CONE_NRG, DIST, NUM_TRACKS, IER )
        IF ( IER .EQ. 0 ) THEN
          ETX = ETX - ETEL*COS(PHI)
          ETY = ETY - ETEL*SIN(PHI)
        ENDIF                                       ! if ier .eq. 0
      ENDDO                                           ! i = 1, nelec
C
C ****  Loop over Photons
C
      CALL GTPPHO_TOTAL( NPHOT, IER )
      IF ( IER .NE. 0 )  NPHOT = 0
C
      DO I = 1, NPHOT
        CALL GTPPHO_UNCORRECTED
        CALL GTPPHO(I, EEL, ETEL, SIGEL, THETA, ETA, PHI,
     &          CONE_NRG, IER )
        IF ( IER .EQ. 0 ) THEN
          ETX = ETX - ETEL*COS(PHI)
          ETY = ETY - ETEL*SIN(PHI)
        ENDIF                                       ! if ier .eq. 0
      ENDDO                                           ! i = 1, nphot

C
C ****  Loop over all the jets. Subtract from the MET the jet minus
C ****  its noise contribution.
C
      CALL SET_CAPH ('CONE_JET', TEMPLATE, IER)
      IF (IER .NE. 0) THEN
        CALL ERRMSG ('CORRECT_MET', 'CORRECT_MET',
     &    'SET_CAPH could not find right jet template, cannot correct',
     &    'W')
        CORRECT_MET = .FALSE.
        CALL RESET_CAPH
        RETURN
      ENDIF

C
C ****  Ensure that JETS have NOT been corrected yet. If they have,
C ****  then we will UNCORRECT them
C
      YES   = .FALSE.
      LJETS = GZJETS()
      DO WHILE ( LJETS .GT. 0 )
        IF ( LJETS .GT. 0 ) CALL JETS_BANK_CORRECTED( LJETS, 'COR', YES,
     &    IER )
        IF ( YES ) THEN
          CALL ERRMSG('Already corrected','CORRECT_MET',
     &      'JETS bank has been corrected, will uncorrect','W')
          CALL UNCORRECT_JETS_BANK( LJETS, .TRUE., IER )
        ENDIF
C
C
C: Subtract out the noise so that we only get the physics recoil.
C: Use JETS or JNEP for position of jet.
C
        LBANK   = LJETS
        IF ( LQ( LJETS - IZJNEP ) .GT. 0 ) LBANK = LQ( LJETS - IZJNEP )
C
C: Skip if dont want any jet corrections...
C
        IF ( USE_JET_CORRECTION ) THEN
          CALL CORRECT_JETS_NOISEU( LJETS, DUM1, DUM2, UNDER_E,
     &      UNDER_ET, ZSP_E, ZSP_ET )
        ELSE
          UNDER_ET = 0.0
          ZSP_ET   = 0.0
        ENDIF
C
C: Subtract jet (minus noise/ue) out
C
        EJ(1)   = Q( LBANK + 2 )
        EJ(2)   = Q( LBANK + 3 )
        EJ(5)   = Q( LBANK + 6 )
        PHI     = Q( LBANK + 8 )
C
        ETX = ETX - (EJ(1)-(UNDER_ET+ZSP_ET)*COS(PHI))
        ETY = ETY - (EJ(2)-(UNDER_ET+ZSP_ET)*SIN(PHI))
C
        LJETS = LQ( LJETS )
      ENDDO
C
C ****  See if there are Main Ring VCOR banks to be dealt with; if so, deal...
C
      CALL GTVCOR ('MRFX', 1, IVERSION, VCOR_DELTA, VCOR_SIGMA, IER)
      IF (IER .EQ. 0) THEN
        ETX = ETX + VCOR_DELTA(1)
        ETY = ETY + VCOR_DELTA(2)
      ENDIF
      CALL GTVCOR ('MRFN', 1, IVERSION, VCOR_DELTA, VCOR_SIGMA, IER)
      IF (IER .EQ. 0) THEN
        ETX = ETX + VCOR_DELTA(1)
        ETY = ETY + VCOR_DELTA(2)
      ENDIF
C
C **** Drop old SOFT if there is one
C
      IER = 0
      DO WHILE ( IER .EQ. 0 )
        CALL GTVCOR ('SOFT', 1, IVERSION, VCOR_DELTA, VCOR_SIGMA, IER)
        IF ( IER .EQ. 0 ) THEN
          CALL GTVCOR_POINTER( LVCOR )
          CALL MZDROP(IXMAIN,LVCOR,' ')
        ENDIF
      ENDDO
c
C
C **** Calculate and correct soft recoil vector
C
c      IF (WANT_SOFT_CORRECTION) THEN
c        IF ( ETX*ETY .EQ. 0.0 ) THEN
c          RECOIL_PHI = 0.
c        ELSE
c          RECOIL_PHI= ATAN2( ETY, ETX )
c        ENDIF
c        IF ( RECOIL_PHI .LT. 0.0 ) RECOIL_PHI = RECOIL_PHI + 2*SNGL(PI)
c        OLD_SOFT_ETX = ETX
c        OLD_SOFT_ETY = ETY
c
C:  Try to find a jet which matches to this
C
c        SOFT_MATCH    = 0
c        LJETS  = GZJETS()
c        DO WHILE ( LJETS .GT. 0 )
c          LBANK = LQ( LJETS - IZJNEP )
c          IF ( LBANK .LE. 0 ) LBANK = LJETS
C
c          DPHI  = MIN( ABS(Q(LBANK+8)-RECOIL_PHI), ABS( SNGL(TWOPI)
c     &      -ABS(Q(LBANK+8)-RECOIL_PHI)) )
c          IF ( (DPHI .LT. DPHIMIN .OR. SOFT_MATCH .EQ. 0 )  .AND. DPHI .
c     &      LT. .7 ) THEN
c            DPHIMIN     = DPHI
c            SOFT_MATCH  = 1
c            DRETA = Q( LBANK + 9 )
c            DRMIN = SQRT( Q(LBANK+12)**2 + Q(LBANK+13)**2 )
c            DREMF = Q( LBANK + 14 )
c          ENDIF
c          LJETS = LQ( LJETS )
c        ENDDO
C
C: Make soft correction
C
c        WIDTH_FACTOR  = 1.0
c        RECOIL_ET     = SQRT( ETX**2 + ETY**2 )
c        OLD_RECOIL_ET = RECOIL_ET
c        IF ( SOFT_MATCH .EQ. 1 ) THEN
c          CALL CORRECT_JETS_WIDTH( .7, RECOIL_ET, DRETA, DRMIN, DREMF,
c     &      WIDTH_FACTOR)
c        ENDIF
c        RECOIL_ET = RECOIL_ET*WIDTH_FACTOR
c        ETX = RECOIL_ET * COS(RECOIL_PHI)
c        ETY = RECOIL_ET * SIN(RECOIL_PHI)
C
C **** Book and fill a VCOR for this SOFT correction
C
c        CALL BKVCOR( LVCOR )
c        IF ( LVCOR .GT. 0 ) THEN
c          IQ( LVCOR + 2 ) = 4HSOFT
c          Q ( LVCOR + 3 ) = ETX - OLD_SOFT_ETX
c          Q ( LVCOR + 4 ) = ETY - OLD_SOFT_ETY
c        ELSE
c          CALL ERRMSG('Book failure','CORRECT_MET',
c     &      'Failed to book VCOR','F')
c          GOTO 900
c        ENDIF
c      ENDIF
C
C ****   Correct jets with special MET flag set. This tells CORRECT_JETS to
C ****   first remove ISOLATED PELC/PPHOs from JETS. Then correct what is
C ****   left. Also make VCOR's and not to put the isolated electrons back
C ****   into the JETS.
C
      MET_CORRECT = .TRUE.
      IF ( USE_JET_CORRECTION ) THEN
        CALL CORRECT_JETS( DO_ZSP, DO_UNDER, DO_OUT_OF_CONE, ISYS,
     &    MET_CORRECT, OK )
      ENDIF

C
C ***  One final loop over jets. Uncorrect the jets we corrected with the
C ***  MET algorithm
C
      LJETS = GZJETS()
      DO WHILE ( LJETS .GT. 0 )
        CALL UNCORRECT_JETS_BANK( LJETS, .TRUE., IER )
        LJETS = LQ( LJETS )
      ENDDO
  900 CALL RESET_CAPH


      RETURN
C#######################################################################
      ENTRY CORRECT_MET_SET_REPEAT ( CONTROL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This entry points allows the user to override the
C-   control REPEAT_CORRECTION in the CORRECT_MET_RCP file.
C-
C-   Inputs  : CONTROL  [L]   .TRUE. to repeat correction (old PNUT(4) and
C-                            PNUT(5) removed, and newly calculated ones replace
C-                            them) or .FALSE. to *not* repeat correction (if
C-                            old banks exist, CORRECT_MET then does nothing).
C-   Outputs : none
C-   Controls: none
C-
C-   Created  15-OCT-1993   Marc Paterno
C-
C----------------------------------------------------------------------
      REPEAT_CORRECTION = CONTROL
      CORRECT_MET_SET_REPEAT = .TRUE.
      RETURN
      END
