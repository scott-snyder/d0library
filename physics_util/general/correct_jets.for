C DEC/CMS REPLACEMENT HISTORY, Element CORRECT_JETS.FOR
C *6     5-OCT-1994 17:17:50 FRAME "Richard V. Astur: small changes/bug fixes"
C *5    19-JUL-1994 21:44:34 MEENA "Richard V. Astur: small changes/bug fixes"
C *4    17-MAY-1994 10:45:13 MEENA "Richard V. Astur: small changes/bug fixes"
C *3     3-APR-1994 16:57:51 MEENA "Richard V. Astur: Han
C *2    13-MAR-1994 20:31:19 MEENA "Richard V. Astur: Bug fixes to
C *1    20-FEB-1994 19:47:00 MEENA "Richard V. Astur: For CORRECT_MET"
C DEC/CMS REPLACEMENT HISTORY, Element CORRECT_JETS.FOR
      SUBROUTINE CORRECT_JETS (DO_ZSP_CORR, DO_UNDEVT_CORR,
     &                         DO_OUTOFCONE_CORR,ISYS,MET_CORRECT,ok)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This subroutine uses the standard routine
C-   QCD_JET_CORRECTION to overwrite JETS banks.  It loops over all CAPH
C-   headers, and corrects all the jets under each CONE_JET algorithm.  Other
C-   algorithms are untouched.
C-
C-   Note: JETS which contain isolated PELC/PPHO (defined in CORRECTEM_RCP)
C-   are corrected by subtracting the isolated PELC/PPHO and correcting
C-   the rest if it is above the reconstruction ET threshold.  The
C-   corrected (by CORRECTEM) PELC/PPHO is then added back.
C-
C-
C-
C-
C-   This routine does not work on microDSTs, which have no JETS banks.
C-
C-   Inputs  : DO_ZSP_CORR         [L]   same as QCD_JET_CORRECTION
C-             DO_UNDEVT_CORR      [L]   same as QCD_JET_CORRECTION
C-             DO_OUTOFCONE_CORR   [L]   same as QCD_JET_CORRECTION
C-             MET_CORRECT         [L]   Correct Jets for the MET
C-                                       correction. This means that
C-                                       the caller has already chosen
C-                                       the algorithm for us using
C-                                       SET_CAPH and wants us to leave
C-                                       isolated electrons out of the
C-                                       JETS. Also we will make VCOR banks
C-             ISYS                [I]   same as QCD_JET_CORRECTION
C-
C-   Outputs : OK   [L]   .TRUE. if successful,  (all jets OK)
C-                        .FALSE. on error (one or more jets not OK)
C-
C-   Controls: see QCD_JET_CORRECTION
C-
C-   Created  13-OCT-1993   Marc Paterno
C-   Updated  21-OCT-1993   Marc Paterno  MZPUSH the JETS bank to assure they
C-                                        match version 4 of the JETS bank
C-                                      (include the ENERGY CORRECTION STATUS
C-                                      WORD)
C-   Updated  23-OCT-1993   Rich Astur    Make and fill VCOR banks
C-                                        & use MZPUSH to match version 5 or
C-                                        higher.
C-
C-   Updated  13-DEC-1993   Rich Astur    Fill JETS correction words/
C-                                        implement formal energy correction
C-                                        procedure
C-
C-   Modified  4-MAR-1994   Rich Astur    Handle negative mass jets - rescale
C-                                        P to E (make zero mass)
C-   Modified  6-APR-1994   Rich Astur    VCOR for JETS should be 0.0 when
C-                                        the jet is
C-                                        below threshold. This is usually
C-                                        the case, except when NEW ET
C-                                        is negative. Do OLD ET the
C-                                        same way.
C-   Updated   9-MAY-1995   R. Astur      Update for CAFIX v5.0 release
C-   Updated  Oct-12-1995   Bob Kehoe --  put new energy fractions, overall
C-                                        scale, and errors into JETS/JETX bank
C-   Updated  Oct-18-1995   Bob Kehoe --  fix bug in writing em_fraction
C-   Updated  Oct-25-1995   Bob Kehoe --  replace get_uncorrect_jets_bank with
C-                                        get_uncorrect_jets_bank_2
C-   Updated  30-OCT-1995   Dhiman Chakraborty   
C-                          Replaced the direct call to MZPUSH by BKJETS_UPDATE
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INCLUDE  'D0$LINKS:IZCAPH.LINK'
      INCLUDE  'D0$LINKS:IZJETS.LINK'
      INCLUDE  'D0$PARAMS:CAPH.DEF'
      LOGICAL  DO_ZSP_CORR, DO_UNDEVT_CORR, DO_OUTOFCONE_CORR, OK
      LOGICAL  MET_CORRECT, EM_REMOVED
      LOGICAL  RET_COND
      SAVE     RET_COND
      INTEGER  ISYS
C----------------------------------------------------------------------
      LOGICAL  DO_JET_CORRECTION, CORRECT_EM_JETS
      SAVE     DO_JET_CORRECTION, CORRECT_EM_JETS
      LOGICAL  REPEAT_CORRECTION
      SAVE     REPEAT_CORRECTION
      LOGICAL  FIRST, YES, REMOVE
      REAL     DUMMY(12), DUME(12), DUMDIF(12)
      LOGICAL  DUMFIRST
      DATA     DUMDIF/ 5*.1, 3*.01, 4*.05/
      INTEGER  ALGORITHM, GZJETS, IER, I
      INTEGER  DNDATA, DNLINKS, IVER
      INTEGER  ERRSUM
      INTEGER  VWANTED, VFOUND, LPROC, GZPROC, GZCAPH
      INTEGER  IERR
      INTEGER  ISOLATION_MASK
      SAVE     ISOLATION_MASK
      PARAMETER (VWANTED = 1)
      INTEGER  GZPNUT
      EXTERNAL GZPNUT
      EXTERNAL GZJETS, GZPROC, GZCAPH
      REAL     NOISE(8), NOISE_ET
      REAL     NEW_ETA, OLD_E(5), NEW_E(5), PTOT, z_vertex
      REAL new_e_fract(3),jet_quans(50),error(2,2)
      REAL     VINFO(3,VWANTED)
      CHARACTER*4 PATH
      DATA     FIRST /.TRUE./
      SAVE     FIRST
C---------------------------------------------------------------------
C ****  An evil COMMON block is required by ZEBRA to make a link area.
      INTEGER  NLINKS_TO_SAVE
      PARAMETER (NLINKS_TO_SAVE=4)
      INTEGER  LINKAREA(NLINKS_TO_SAVE+2)
      INTEGER  DUM(2), LCAPH, LJETS, LJETS_NEXT, LVCOR
      EQUIVALENCE (LINKAREA(1), DUM(1))
      EQUIVALENCE (LINKAREA(3), LCAPH)
      EQUIVALENCE (LINKAREA(4), LJETS)
      EQUIVALENCE (LINKAREA(5), LJETS_NEXT)
      EQUIVALENCE (LINKAREA(6), LVCOR)
      COMMON    /CORRJETS/ LINKAREA
C----------------------------------------------------------------------
      IF (FIRST) THEN
        OK = .TRUE.                       ! default return condition
        FIRST = .FALSE.
        CALL ERRMSG ( 'CORRECT_JETS', 'CORRECT_JETS',
     &    'CORRECT_JETS is being used to overwrite JETS banks', 'I' )

        CALL INRCP ('CAFIX_RCP', IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ('NO RCP', 'CORRECT_JETS',
     &'Could not find CAFIX_RCP, will not do JETS correction',
     &'W')
          OK = .FALSE.
        ELSE
          CALL EZPICK ('CAFIX_RCP')
          ERRSUM = 0
          CALL EZGET ('DO_JET_CORRECTION', DO_JET_CORRECTION, IER)
          ERRSUM = ERRSUM + ABS(IER)
          CALL EZGET ('REPEAT_CORRECTION', REPEAT_CORRECTION, IER)
          ERRSUM = ERRSUM + ABS(IER)
          CALL EZGET ('CORRECT_EM_JETS', CORRECT_EM_JETS, IER)
          ERRSUM = ERRSUM + ABS(IER)
          CALL EZRSET

          IF (ERRSUM .NE. 0) THEN
            CALL ERRMSG ('CORRECT_JETS', 'CORRECT_JETS',
     &'Missing params in CAFIX_RCP: will not do JETS correction',
     &'W' )
            OK = .FALSE.
          ENDIF                         ! if errsum .ne. 0
        ENDIF                           ! if ier .ne. 0
        CALL INRCP ('CORRECTEM_RCP', IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ('NO RCP', 'CORRECT_JETS',
     &'Could not find CORRECTEM_RCP, will not do JETS correction',
     &'W')
          OK = .FALSE.
        ELSE
          CALL EZPICK ('CORRECTEM_RCP')
          ERRSUM = 0
          CALL EZGET ('ECORRECT_MASK', ISOLATION_MASK, IER)
          ERRSUM = ERRSUM + ABS(IER)
          CALL EZRSET

          IF (ERRSUM .NE. 0) THEN
            CALL ERRMSG ('CORRECT_JETS', 'CORRECT_JETS',
     &'Missing params in CORRECTEM_RCP: will not do JETS correction',
     &'W' )
            OK = .FALSE.
          ENDIF                         ! if errsum .ne. 0
        ENDIF                           ! if ier .ne. 0
        RET_COND = OK
      ELSE
        OK       = RET_COND
      ENDIF                             ! if first
C----------------------------------------------------------------------
      IF ( .NOT. OK ) RETURN
C
C ****  Check that the current path is not MDST.
C
      CALL PATHGT (PATH)
      IF (PATH .EQ. 'MDST') THEN
        CALL ERRMSG  ('CORRECT_JETS', 'CORRECT_JETS',
     &    'CORRECT_JETS cannot be used on microDSTs', 'W')
        OK = .FALSE.
        RETURN
      ENDIF
C
C: Skip if done MET before
C
      DO I = 4, 5
        IF (GZPNUT(I) .GT. 0) THEN
          IF (.NOT.REPEAT_CORRECTION) THEN
            CALL ERRMSG ('Skip correction', 'CORRECT_JETS',
     &        ' REPEAT_CORRECTION is FALSE', 'I')
            RETURN
          ENDIF
        ENDIF
      ENDDO
C
C ****  Get the Z location of the primary interaction vertex.
C
      CALL VERTEX_INFO (VWANTED, VFOUND, VINFO, OK)
      IF (.NOT. OK) THEN
        CALL ERRMSG ('CORRECT_JETS', 'CORRECT_JETS',
     &    'Could not get location of primary interaction, assuming z=0',
     &    'W' )
        Z_VERTEX = 0.0
      ELSE
        Z_VERTEX = VINFO(1,1)
      ENDIF
C
C ****  Set up to save the LCAPH and LJETS links for ZEBRA, since they
C ****  might get trashed by garbage collection in the following
C ****  manipulations.
C
      CALL MZLINT (IXCOM, 'CORRJETS', LINKAREA(1),
     &  LINKAREA(NLINKS_TO_SAVE+2), LINKAREA(1))
C
C ****  Loop over all CAPH.  This can't be done with GZCAPH, which only
C ****  looks at
C ****  the "current" CAPH.  We do it by hand...
C
      LPROC = GZPROC()
      IF (LPROC .LE. 0) THEN
        CALL ERRMSG('CORRECT_JETS', 'CORRECT_JETS',
     &    'Cannot find PROC bank', 'W')
        OK = .FALSE.
        CALL VZERO (LINKAREA(3), NLINKS_TO_SAVE)  ! Zero inactive links
        LINKAREA(1) = 0                           ! Deactivate link area
        RETURN
      ENDIF

C: Normally loop over all CAPH banks, but only do one if we are correcting
C: jets for the MET correction.

      LCAPH = LQ(LPROC-IZCAPH)
      IF ( MET_CORRECT ) LCAPH = GZCAPH()         ! CAPH path has been set
      DO WHILE (LCAPH .GT. 0)
C
C ****  Make sure this is a CONE,NN,KT or other (not em) algorithm
C
        ALGORITHM = IQ(LCAPH + 4)
        IF ( ALGORITHM .GT. 1 ) THEN
C
C ****  Make sure we have JETS banks of version 4 or higher, so that they
C ****  contain the ENERGY CORRECTION STATUS word.
C
          LJETS = LQ( LCAPH - IZJETS )
          DO WHILE (LJETS .GT. 0)
            LJETS_NEXT = LQ(LJETS)
            CALL BKJETS_UPDATE(LJETS,IVER,DNLINKS,DNDATA,IERR)
C
C ****  Loop over dependent JETS, figure out corrected values,
C ****  and overwrite the bank.  If bank has already been corrected, 
C ****  issue error message and uncorrect it and do again
C
C: Was it corrected?
C
            CALL JETS_BANK_CORRECTED( LJETS, 'COR', YES, IER )
            IF ( YES ) THEN
              CALL ERRMSG('ALREADY CORRECTED','CORRECT_JETS',
     &          'Will recorrect','W')
              CALL UNCORRECT_JETS_BANK( LJETS, .TRUE., IER )
            else
            ENDIF
C
C: Hold old numbers
C
            DO I = 1, 8
              DUMMY(I) = Q( LJETS + 1 + I )
            ENDDO
            DUMMY(9) =  Q( LJETS + 14 )
            DUMMY(10) =  Q( LJETS + 17 )
            DUMMY(11) =  Q( LJETS + 18 )
C
C: Remove Isolated PELC/PPHO's first
C
            REMOVE = .TRUE.
            CALL CORRECT_JETS_EM_REMOVE( LJETS, REMOVE, ISOLATION_MASK )
C
C: Store the old values and set the new values equal to old values.
C: The VCOR we make will be the difference between them when
C: we are done.
C
            DO I = 2, 6
              OLD_E( I-1 ) = Q( LJETS + I )
              NEW_E( I-1 ) = Q( LJETS + I )
            ENDDO
C
C: At this point the jet is either an uncorrected RECO jet
C: (with ET above some threshold) or it has an electron subtracted from it.
C: In some cases this subtraction does not work perfectly and the remainder of
C: the jet is negative. We refuse to correct the jet if this occurs.
C
            IF ( Q(LJETS+6) .GT. 0.0 .AND. Q(LJETS+5) .GT. 0.0 ) THEN
C
C: Now correct what is left. No correction will be made if the JET
C: is consistent with noise/underlying-event.  Between the noise/uevent and the
C: reconstruction threshold we interpolate, except for the noise subtraction.
C
C: Dont do if JET CORRECTIONS are turned off. Also, dont do if
C: we are calculating missing ET and this jet contains a
C: PELC/PPHO and the user doesn't want additional corrections
C: (beyond CORRECTEM) done to this jet.
C
              CALL CORRECT_JETS_EM_REMOVED( EM_REMOVED )
              IF ( DO_JET_CORRECTION .AND. .NOT. ( MET_CORRECT .AND.
     &          EM_REMOVED .AND. .NOT. CORRECT_EM_JETS)) THEN
                CALL QCD_JET_CORRECTION (LJETS, DO_ZSP_CORR,
     &                                 DO_UNDEVT_CORR,
     &                                 DO_OUTOFCONE_CORR,
     &                                 Z_VERTEX, ISYS, NEW_E(4),
     &                                 NEW_E(5), NEW_ETA, IER)
                IF (IER .NE. 0) THEN
                  CALL ERRMSG ('CORRECT_JETS', 'CORRECT_JETS',
     &              'QCD_JET_CORRECTION returned error condition', 'W')
                  OK = .FALSE.
                  CALL UNCORRECT_JETS_BANK( LJETS, .TRUE., IER )
                  NEW_E(5) = OLD_E(5)
                  NEW_E(4) = OLD_E(4)
                  NEW_ETA  = Q( LJETS + 9 )
                ELSE
C
C: obtain energy fractions, energy_scale and errors computed
C
                  call qcd_jet_correction_quans(jet_quans)
                  new_e_fract(1) = jet_quans(1)
                  new_e_fract(2) = jet_quans(2)
                  new_e_fract(3) = jet_quans(3)
                  error(1,1) = jet_quans(6)
                  error(2,1) = jet_quans(7)
                  error(1,2) = jet_quans(8)
                  error(2,2) = jet_quans(9)
C
C: Retrieve the noise numbers used and adjust our original 4-vector to
C: exclude noise
C
                  CALL QCD_JET_CORRECTION_GET_UNDZSP( NOISE )
                  NOISE_ET = NOISE(3) + NOISE(7)
                  OLD_E(1) = OLD_E(1) - NOISE_ET*COS(
     &              Q(LJETS+8 ) )
                  OLD_E(2) = OLD_E(2) - NOISE_ET*SIN(
     &              Q(LJETS+8 ) )
                  OLD_E(3) = OLD_E(3) - NOISE_ET*COS( Q(LJETS+7) )/SIN(
     &              Q(LJETS+7))
                  OLD_E(4) = OLD_E(4) - NOISE_ET/SIN(Q(LJETS+7))
                  OLD_E(5) = OLD_E(5) - NOISE_ET
C
C: At this point, it is possible for either the energy or the transverse
C: energy of the jet to go negative. If so, it was due to the noise
C: subtraction. If the energy is negative, we set it to 0.0.  If the
C: transverse energy is negative, we set it to E*sin(theta)
C
                  IF ( NEW_E(4) .LT. 0. ) THEN
                    NEW_E(4)  = 0.0
                    NEW_E(5)  = 0.0
                    OLD_E(4)  = 0.0
                    OLD_E(5)  = 0.0
                  ELSEIF ( NEW_E(5) .LT. 0. ) THEN
                    NEW_E(5)  = NEW_E(4)*SIN( Q(LJETS+7) )
                    OLD_E(5)  = NEW_E(5)  ! No VCOR contribution
                  ENDIF
C
C: If Mass of jet is negative, rescale the momentum to the energy
C: of the jet. The end result is that the corrected momentum is
C: scaled to the corrected energy.
C
                  PTOT = SQRT( OLD_E(1)**2 + OLD_E(2)**2 + OLD_E(3)**2 )
                  IF ( PTOT .GT. ABS(OLD_E(4)) ) THEN
                    PTOT  = ABS( OLD_E(4) )
                    CALL CORRECT_JETS_BANK( LJETS, 'P  ',PTOT, IER )
                  ELSE
                    PTOT  =   PTOT * NEW_E(4)/OLD_E(4)
                    CALL CORRECT_JETS_BANK( LJETS, 'P  ', PTOT, IER )
                  ENDIF
C
C: Update the JETS bank. There is no PHI correction here.
C
                  CALL CORRECT_JETS_BANK(LJETS,'ET ',NEW_E(5),ier)
                  CALL CORRECT_JETS_BANK(LJETS,'E  ',NEW_E(4),ier)
                  CALL CORRECT_JETS_BANK(LJETS,'ETA',NEW_ETA,IER)
                  CALL CORRECT_JETS_BANK(LJETS,'ZSP',NOISE(7),IER)
                  CALL CORRECT_JETS_BANK(LJETS,'UND',NOISE(3),IER)
                  CALL CORRECT_JETS_BANK(LJETS,'EMF',NEW_E_FRACT(1),IER)
                  CALL CORRECT_JETS_BANK(LJETS,'ICF',NEW_E_FRACT(2),IER)
                  CALL CORRECT_JETS_BANK(LJETS,'CHF',NEW_E_FRACT(3),IER)
                  CALL CORRECT_JETS_BANK(LJETS,'STH',error(1,1),IER)
                  CALL CORRECT_JETS_BANK(LJETS,'STL',error(2,1),IER)
                  CALL CORRECT_JETS_BANK(LJETS,'SYH',error(1,2),IER)
                  CALL CORRECT_JETS_BANK(LJETS,'SYL',error(2,2),IER)
C
C: Update NEW_E
C
                  DO I = 2, 4
                    NEW_E( I - 1 )  = Q( LJETS + I )
                  ENDDO
C
C: Set the correction word so that we know which corrections were applied
C
                  IF ( DO_ZSP_CORR ) CALL SET_JETS_BANK_CORRECTED(
     &              LJETS,'ZSP', IER )

                  IF ( DO_UNDEVT_CORR ) CALL SET_JETS_BANK_CORRECTED(
     &              LJETS, 'UND', IER )

                  IF ( DO_OUTOFCONE_CORR ) CALL SET_JETS_BANK_CORRECTED(
     &              LJETS, 'OOC', IER )

                ENDIF

              ENDIF

            ENDIF
C
C: Now add corrected PELC/PPHO back in, if there are any, BUT
C: Dont do this if this is for MET correction
C
            IF ( .NOT. MET_CORRECT ) THEN
              REMOVE = .FALSE.
              CALL CORRECT_JETS_EM_REMOVE( LJETS, REMOVE,
     &            ISOLATION_MASK)
            ENDIF
C
C: If we are correcting for MET. Then we should Book and Fill VCOR and
C: uncorrect the JETS bank when we are done
C
            IF ( MET_CORRECT ) THEN
              CALL BKVCOR( LVCOR )
              CALL CATALOGUE_JET_CORR( LJETS, LVCOR, OLD_E, NEW_E )
            ENDIF
C
C: Debug, verify uncorrection routine
C
            CALL UNCORRECT_JETS_BANK( LJETS, .FALSE., IER )
            call get_uncorrect_jets_bank_2(dume)
            DUMFIRST=.TRUE.
            DO I = 1, 11
              IF ( ABS( DUME(I)-DUMMY(I) ) .GT. DUMDIF(I) ) THEN
                IF ( DUMFIRST) THEN
                  CALL ERRMSG('Uncorrect problem','CORRECT_JETS',
     &                ' Discrepency in uncorrecting jets ?!','W')
C                  write(33,*) IQ(LHEAD+6),IQ(LHEAD+9), I
C                  write(33,*) DUMMY(i),dume(i)
                  DUMFIRST = .FALSE.
                ENDIF
C                  PRINT *, IQ(LHEAD+6),IQ(LHEAD+9), I, DUME(I),
C     &                DUMMY(I)
              ENDIF
            ENDDO
            LJETS = LJETS_NEXT
          ENDDO                             ! do while (ljets .gt. 0)
        ENDIF                               ! if algorithm .eq. cone_jet
        IF ( MET_CORRECT ) THEN
          LCAPH = 0                         ! Dont do any more algorithms
        ELSE
          LCAPH = LQ(LCAPH)
        ENDIF
      ENDDO                                 ! while lcaph .gt. 0

      CALL VZERO (LINKAREA(3), NLINKS_TO_SAVE)  ! Zero inactive links
      LINKAREA(1) = 0                           ! Deactivate link area
      RETURN
      END
