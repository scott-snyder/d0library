      SUBROUTINE CORRECT_MET_JET_FIXUP ( MASK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Remove electrons/photons from JETS.  Only those
C-   electrons/photons which satisfy MASK are removed.  See CHECK_EM_QUALITY for
C-   the meaning of MASK.  It does its work under the CURRENT setting of the
C-   CAPH bank.
C-
C-   The ENERGY, ET, EX, EY, EZ, ETA, THETA, PHI, and ETA are recalculated.
C-   The EM fraction, CH fraction, and ICD fraction are recalculated.
C-
C-   ** NB ** THIS ROUTINE OVERWRITES SOME QUANTITIES IN THE JETS BANK.
C-
C-   Inputs  : MASK   [I]  the mask used to specify which electrons/photons
C-                         should be removed.
C-   Outputs : none
C-
C-   Controls: none
C-
C-   Created   8-OCT-1993   Marc Paterno
C-   Updated   1-NOV-1993   Marc Paterno  Recalculate EM fraction for the jet,
C-                                        required because jet correction is now
C-                                        EMF dependent.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INTEGER  MASK
C----------------------------------------------------------------------
      LOGICAL  OK, EMCLUSTER_FOUND
      INTEGER  GZJETS, LCAPH, LJETS, LCACL, LBANK, I, NR, LVCOR
      EXTERNAL GZJETS
      REAL     JET_THRESHOLD
      REAL     EX, EY, EZ, ENERGY, ET, ETA, THETA, PHI, EVECTOR(4)
      REAL     JET_EX, JET_EY, JET_EZ, JET_ENERGY, JET_ET, EMF
      REAL     EMET, JET_EMET, CHF, ICDF, JET_ICDET, JET_CHET
      REAL     DX, DY, DZ, DE
C----------------------------------------------------------------------
      LJETS = GZJETS()
C
C ****  Loop over all jets
C
      DO WHILE (LJETS .GT. 0)           ! loop over jets
        EMCLUSTER_FOUND = .FALSE.
        EX     = 0.0
        EY     = 0.0
        EZ     = 0.0
        ENERGY = 0.0
        ET     = 0.0
        EMET   = 0.0
C
C ****  Add up energy sums for all electrons/photons which satisfy the caller's
C ****  mask within this jet.
C
        DO I = 1, 4
          LBANK = LQ(LJETS -2 -I)
          DX = 0.0
          DY = 0.0
          DZ = 0.0
          DE = 0.0
          IF ( LBANK .GT. 0 ) THEN
            EMCLUSTER_FOUND = .TRUE.
            LCACL = LQ(LBANK -2 )
            NR = IQ(LBANK - 3)
            IF (NR .GE. 4) THEN
              LVCOR = LQ(LBANK - 4)
              IF (LVCOR .GT. 0) THEN
                DX = Q(LVCOR+3)
                DY = Q(LVCOR+4)
                DZ = Q(LVCOR+5)
                DE = Q(LVCOR+6)
              ENDIF
            ENDIF
          ENDIF
          IF (LBANK .GT. 0 .AND. LCACL .GT. 0 ) THEN
            CALL CHECK_EM_QUALITY (LBANK, MASK, OK) ! see if it satisfies mask
            IF (OK) THEN
              EX     = EX     + Q(LBANK + 3) - DX
              EY     = EY     + Q(LBANK + 4) - DY
              EZ     = EZ     + Q(LBANK + 5) - DZ
              ENERGY = ENERGY + Q(LBANK + 6) - DE
              ET     = SQRT (EX**2 + EY**2)
              EMF    = (Q( LBANK + 6 ) - Q( LCACL + 19 ))/Q( LBANK + 6)
              EMET   = EMET   + Q(LBANK + 7)*EMF
            ENDIF                       ! if ok
          ENDIF                         ! if lbank .gt. 0
        ENDDO                           ! i = 1, 4
        IF ( EMCLUSTER_FOUND ) THEN
C
C ****  Get the original jet's energy quantities.
C
          JET_EX     = Q(LJETS + 2)
          JET_EY     = Q(LJETS + 3)
          JET_EZ     = Q(LJETS + 4)
          JET_ENERGY = Q(LJETS + 5)
          JET_ET     = Q(LJETS + 6)
          EMF        = Q(LJETS + 14)
          ICDF       = Q(LJETS + 17)
          CHF        = Q(LJETS + 18)
          JET_EMET   = JET_ET * EMF
          JET_ICDET  = JET_ET * ICDF
          JET_CHET   = JET_ET * CHF
C
C ****  Subtract the electron contributions
C
          JET_EX     = JET_EX - EX
          JET_EY     = JET_EY - EY
          JET_EZ     = JET_EZ - EZ
          JET_ENERGY = JET_ENERGY - ENERGY ! energy is SCALAR sum
          JET_ET     = JET_ET - ET         ! Et is SCALAR sum
          JET_EMET   = JET_EMET - EMET
          EVECTOR(1) = JET_EX
          EVECTOR(2) = JET_EY
          EVECTOR(3) = JET_EZ
          EVECTOR(4) = JET_ENERGY
          CALL ETOETA (EVECTOR, PHI, THETA, ETA)
          IF ( JET_ET .GT. 1. ) THEN
            EMF  =  JET_EMET / JET_ET          ! new EM fraction
            CHF  =  JET_CHET / JET_ET          ! new CH fraction
            ICDF = JET_ICDET / JET_ET          ! new ICD fraction
          ELSE
            EMF  = -1.
            CHF  = -1.
            ICDF = -1.
          ENDIF
C
C ****  Now reset the jet's quantities.
C
          Q(LJETS + 2)  = JET_EX
          Q(LJETS + 3)  = JET_EY
          Q(LJETS + 4)  = JET_EZ
          Q(LJETS + 5)  = JET_ENERGY
          Q(LJETS + 6)  = JET_ET
          Q(LJETS + 7)  = THETA
          Q(LJETS + 8)  = PHI
          Q(LJETS + 9)  = ETA
          Q(LJETS + 14) = EMF
          Q(LJETS + 17) = ICDF
          Q(LJETS + 18) = CHF
        ENDIF
C
C ****  On to the next jet...
C
        LJETS = LQ(LJETS)
      ENDDO                             ! while ljets .gt. 0
C
C ****  Some of these jets may now be below threshold. Drop them.
C
      LJETS = GZJETS()
      IF ( LJETS .LE. 0 ) RETURN
      LCAPH = LQ( LJETS + 1 )
      IF ( LCAPH .GT. 0 ) THEN
        JET_THRESHOLD = Q( LCAPH + 7 )
      ELSE
        JET_THRESHOLD = 0.0
        CALL ERRMSG('No CAPH?','CORRECT_MET_JET_FIXUP',
     &    'No CAPH for this Jet!?','E')
      ENDIF
C
      DO WHILE ( LJETS .GT. 0 ) 
        IF ( Q(LJETS+6) .LT. JET_THRESHOLD ) THEN
          CALL MZDROP(IXCOM, LJETS, ' ')
          LJETS = GZJETS()
        ELSE
          LJETS = LQ(LJETS)
        ENDIF
      ENDDO
      RETURN
      END
