C DEC/CMS REPLACEMENT HISTORY, Element CORRECT_JETS_EM_REMOVE.FOR
C *3    19-JUL-1994 21:44:49 MEENA "Richard V. Astur: Add entry point"
C *2    13-MAR-1994 20:31:11 MEENA "Richard V. Astur: Bug fixes to correction -routines"
C *1    20-FEB-1994 19:46:26 MEENA "Richard V. Astur: For CAFIX and Jet Correc-tion Version 4.0"
C DEC/CMS REPLACEMENT HISTORY, Element CORRECT_JETS_EM_REMOVE.FOR
      SUBROUTINE CORRECT_JETS_EM_REMOVE ( LJETS, REMOVE, MASK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Remove/Add electrons/photons from JETS.  Only those
C-   electrons/photons which satisfy MASK are removed.  See CHECK_EM_QUALITY
C-   for the meaning of MASK.  It does its work under the CURRENT setting of
C-   the CAPH bank.
C-
C-   The ENERGY, ET, EX, EY, EZ, ETA, THETA, PHI, and ETA are recalculated.
C-   The EM fraction, CH fraction, and ICD fraction are recalculated.
C-
C-   ** NB ** THIS ROUTINE OVERWRITES SOME QUANTITIES IN THE JETS BANK.
C-
C-   Inputs  : LJETS    [I]   Pointer to JETS bank to fix
C-             REMOVE   [L]   If TRUE, remove uncorrected isolated PELC/PPHO
C-                            from JETS, otherwise add corrected values back
C-                            in.
C-             MASK     [I]   the mask used to specify which electrons/photons
C-                            should be removed.
C-   Outputs : none
C-
C-   Controls: none
C-
C-   Created   8-OCT-1993   Marc Paterno
C-   Updated   1-NOV-1993   Marc Paterno  Recalculate EM fraction for the jet,
C-                                        required because jet correction is
C-                                        now EMF dependent.
C-   Modified 12-DEC-1993   Rich Astur    Changed to add/subtract separately
C-   Updated   8-MAR-1994   Rich Astur    Watch out for em objects wrongly
C-                                        matched to jets
C-   Updated  11-OCT-1995   sss
C-     Fix corrected/uncorrected confusion in EMF calculation.
C-     Make pelc/ppho et sum be a scalar sum instead of a vector sum
C-      so that it is consistent with the EMET calculation.
C-     Remove cutoff in EMF calculation.
C-   Updated  Oct-12-1995   Bob Kehoe  -- Store new ICD and CH fractions
C-                                        in JETS bank
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INTEGER  MASK
C----------------------------------------------------------------------
      LOGICAL  OK, EMCLUSTER_FOUND, REMOVE, WE_REMOVED_EM_OBJECT
      LOGICAL  EM_REMOVED
      INTEGER  GZJETS, LJETS, LCACL, LBANK, I, NR, LVCOR
      EXTERNAL GZJETS
      INTEGER ISIGN, IER
      REAL     PTOT,EX, EY, EZ, ENERGY, ET, ETA, THETA, PHI, EVECTOR(4)
      REAL     JET_EX, JET_EY, JET_EZ, JET_ENERGY, JET_ET, EMF
      REAL     EMET, JET_EMET, CHF, ICDF, JET_ICDET, JET_CHET
      REAL     DX, DY, DZ, DE, this_clustet
      save     chf, icdf, we_removed_em_object

C----------------------------------------------------------------------
C: Decide whether we are adding or subtracting
C
      WE_REMOVED_EM_OBJECT  = .FALSE.
      IF ( REMOVE ) THEN
        ISIGN = 1
      ELSE
        ISIGN = -1
      ENDIF

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
C
C: If we are removing, we want to remove the uncorrected 4-vectors
C
            IF ( REMOVE .AND. LVCOR .GT. 0) THEN
              DX = Q(LVCOR+3)
              DY = Q(LVCOR+4)
              DZ = Q(LVCOR+5)
              DE = Q(LVCOR+6)
            ELSE
              DX = 0.0
              DY = 0.0
              DZ = 0.0
              DE = 0.0
            ENDIF
          ELSE
            CALL ERRMSG('OLD BANK','CORRECT_JETS_EM_REMOVE',
     &        'PELC/PPHO bank has too few links','W')
            LVCOR = 0
          ENDIF
        ENDIF
        IF (LBANK .GT. 0 .AND. LCACL .GT. 0 ) THEN
          CALL CHECK_EM_QUALITY (LBANK, MASK, OK) ! see if it satisfies mask
          IF (OK) THEN
            WE_REMOVED_EM_OBJECT  = .TRUE.
            EX     = EX     + Q(LBANK + 3) - DX
            EY     = EY     + Q(LBANK + 4) - DY
            EZ     = EZ     + Q(LBANK + 5) - DZ
            ENERGY = ENERGY + Q(LBANK + 6) - DE
            this_clustet = SQRT ((Q(LBANK + 3) - DX)**2 +
     &                           (Q(LBANK + 4) - DY)**2)
            et     = et + this_clustet
            EMF    = ((Q(LBANK + 6) - DE) - Q( LCACL + 19 )) /
     &                  (Q(LBANK + 6) - DE)
            EMET   = EMET   + this_clustet * EMF
          ENDIF                       ! if ok
        ENDIF                         ! if lbank .gt. 0
      ENDDO                           ! i = 1, 4
C
C ****  Get the original jet's energy quantities.
C
      EMF        = Q(LJETS + 14)
      ICDF       = Q(LJETS + 17)
      CHF        = Q(LJETS + 18)
      IF ( EMCLUSTER_FOUND ) THEN
        JET_EX     = Q(LJETS + 2)
        JET_EY     = Q(LJETS + 3)
        JET_EZ     = Q(LJETS + 4)
        JET_ENERGY = Q(LJETS + 5)
        JET_ET     = Q(LJETS + 6)
        JET_EMET   = JET_ET * EMF
        JET_ICDET  = JET_ET * ICDF
        JET_CHET   = JET_ET * CHF
C
C ****  Subtract the electron contributions
C
        JET_EX     = JET_EX - ISIGN * EX
        JET_EY     = JET_EY - ISIGN * EY
        JET_EZ     = JET_EZ - ISIGN * EZ
        JET_ENERGY = JET_ENERGY - ISIGN * ENERGY ! energy is SCALAR sum
        JET_ET     = JET_ET - ISIGN * ET         ! Et is SCALAR sum
        JET_EMET   = JET_EMET - ISIGN * EMET
        EVECTOR(1) = JET_EX
        EVECTOR(2) = JET_EY
        EVECTOR(3) = JET_EZ
        EVECTOR(4) = ABS(JET_ENERGY)   ! In case of negative energy, want
C                                      ! to treat this 3-vector as if it really
C                                      ! does point in this direction.
C                                      !(ETOETA will invert the direction of
C                                      ! negative energy cells to get the
C                                      ! true 'direction')
        CALL ETOETA (EVECTOR, PHI, THETA, ETA)
        IF ( ABS (JET_ET) .GT. 1.E-6 ) THEN
          EMF  =  JET_EMET / JET_ET          ! new EM fraction
          CHF  =  JET_CHET / JET_ET          ! new CH fraction
          ICDF = JET_ICDET / JET_ET          ! new ICD fraction
        endif
C
C ****  Calculate the changes made and overwrite the jets bank
C
        PTOT   = SQRT( JET_EX**2 + JET_EY**2 + JET_EZ**2 )
        CALL CORRECT_JETS_BANK( LJETS, 'E  ', JET_ENERGY, IER )
        CALL CORRECT_JETS_BANK( LJETS, 'P  ', PTOT, IER )
        CALL CORRECT_JETS_BANK( LJETS, 'ET ', JET_ET, IER )
        CALL CORRECT_JETS_BANK( LJETS, 'ETA', ETA, IER )
        CALL CORRECT_JETS_BANK( LJETS, 'PHI', PHI, IER )
        CALL CORRECT_JETS_BANK( LJETS, 'EMF', EMF, IER )
        CALL CORRECT_JETS_BANK( LJETS, 'ICF', ICDF, IER )
        CALL CORRECT_JETS_BANK( LJETS, 'CHF', CHF, IER )
      ENDIF
      RETURN

      ENTRY CORRECT_JETS_EM_REMOVED( EM_REMOVED )
      EM_REMOVED  = WE_REMOVED_EM_OBJECT
      RETURN
      END
