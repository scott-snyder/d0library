      SUBROUTINE SM_JET_UNDZSP_FACTOR( CONE_USED, N_INTERACTIONS,
     &  NUM_CONES, OLDJET, CONE_ARRY, UNDER_E, UNDER_ET,
     &  ZSP_E, ZSP_ET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the amount (in GeV) of underlying event
C-      and zero suppression noise both in energy and transverse energy
C-      as a function of detector eta and the number of interactions
C-      for shared and disjoint areas of two overlapping jets.  Areas
C-      common to 3rd jets are ignored
C-
C-   Inputs  : CONE_USED  [R]  : Conesize of jets
C-             N_INTERACTIONS [I] : # of interactions
C-                N_INTERACTIONS < 0 flags area calculation only
C-             NUM_CONES  [I]  : NUMBER OF JET CONES ABOVE THRESHOLD
C-             OLDJET     [I]  : Previously found jet shaing Et w/ NEWJET
C-             CONE_ARRY(200,3) [R] : ARRAY OF FOUND JET CONES
C-                   CONE_ARRY(N,1) : ETA FOR CONE N
C-                   CONE_ARRY(N,2) : PHI FOR CONE N
C-                   CONE_ARRY(N,3) : JET CONTAINING CONE N
C-   Outputs : UNDER_E(3) [R]  : Underlying event energy (minbias) (GeV)
C-             UNDER_ET(3)[R]  : Underlying event transverse energy
C-             ZSP_E(3)   [R]  : Noise from zero suppression energy (GeV)
C-             ZSP_ET(3)  [R]  : Noise from zero suppression trans energy
C-                    1 : OVERLAP AREA CORRECTION NEAREST NEWJET
C-                    2 : OVERLAP AREA CORRECTION NEAREST OLDJET
C-                    3 : NON OVERLAP AREA CORRECTION FOR NEWJET
C-
C- ENTRY SM_JET_UNDZSP_ERROR( UNDER_E_ERR, UNDER_ET_ERR, ZSP_E_ERR, ZSP_ET_ERR)
C-      To return the errors on the previous call to JET_UNDZSP_FACTOR.
C-
C- ENTRY SM_JET_ICD_ADD( ICD_UNDE, ICD_UNDET, ICD_ZSPE, ICD_ZSPET )
C-      To return ICD Contributions which are included in the call to
C-      SM_JET_UNDZSP_FACTOR
C-
C-   Created 13-APR-1994   Bob Hirosky "modified from JET_UNDZSP_FACTOR"
C-   Updated  18-SEP-1995  Bob Hirosky "turn off density factors
C-                         by setting N_INTERACTIONS<0", also fix
C-                         area calc (.099 x .099 unit slices)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      REAL SMALL
      PARAMETER( SMALL = .00001 )
      INTEGER N_INTERACTIONS, NUM_CONES, LRCP
      REAL CONE_USED, CONE_ARRY(200,3)
      REAL ZSP_E(3), ZSP_ET(3), UNDER_E(3), UNDER_ET(3)
      REAL ZSP_ERR(3), ZSP_ETERR(3), UNDER_ERR(3), UNDER_ETERR(3)
      SAVE ZSP_ERR, ZSP_ETERR, UNDER_ERR, UNDER_ETERR
      REAL ICD_ADD_UNDE(3),ICD_ADD_UND(3),ICD_ADD_ZSPE(3),ICD_ADD_ZSP(3)
      SAVE
      REAL E1(3), E2(3), E3(3), E4(3), E5(3), E6(3), E7(3), E8(3)
      REAL ETA_SLICE, PHI_SLICE, PHI_PIECE, SINTHETA
      REAL FACTOR, FACTOR_N
      PARAMETER( FACTOR = .0098 )
      INTEGER IER, IER1, IERTOT
      REAL ETA_NEW, PHI_NEW, ETA_OTHER, PHI_OTHER
      REAL DISTONW, DISTOLD, TMPDIST
      INTEGER OLDJET, OTHER_JET, ICONE, IAREA
      LOGICAL IN_NEW, IN_OLD, IN_OTHER
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      REAL ZEROZSP_DENSITY, ZEROZSP_ERROR, UNDER_DENSITY, UNDER_ERROR
      REAL UNDER_ICD_DENSITY, ZEROZSP_ICD_DENSITY
      SAVE UNDER_ICD_DENSITY, ZEROZSP_ICD_DENSITY
      SAVE ZEROZSP_DENSITY, ZEROZSP_ERROR, UNDER_DENSITY, UNDER_ERROR
      REAL RETA1, RETA2, RPHI1, RPHI2, DELTA_R, PIE
      PARAMETER( PIE = PI )
C---------------------------------------------------------------------
C: Statement function to determine delta r in eta-phi(X-Y)
      DELTA_R(RETA1,RETA2,RPHI1,RPHI2) = SQRT( ((RETA1)-(RETA2))**2 +
     &  MIN( MOD(ABS((RPHI1)-(RPHI2)),2*PIE) ,
     &  2*PIE-ABS(MOD(ABS((RPHI1)-(RPHI2)),2*PIE)) )**2 )
C----------------------------------------------------------------------
C
C: Initialize
C
      DO IAREA = 1,3
        ZSP_E(IAREA)      = 0.0
        ZSP_ET(IAREA)     = 0.0
        UNDER_E(IAREA)    = 0.0
        UNDER_ET(IAREA)   = 0.0
        ZSP_ERR(IAREA)    = 0.0
        ZSP_ETERR(IAREA)  = 0.0
        UNDER_ERR(IAREA)  = 0.0
        UNDER_ETERR(IAREA)= 0.0
        ICD_ADD_ZSP(IAREA) = 0.0
        ICD_ADD_UND(IAREA) = 0.0
        ICD_ADD_ZSPE(IAREA)= 0.0
        ICD_ADD_UNDE(IAREA)= 0.0
      ENDDO
C
C: Read values for RCP
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        IF (N_INTERACTIONS.LT.0) THEN  ! Just calculate areas
          N_INTERACTIONS = -1
          UNDER_ICD_DENSITY = 1.0
          ZEROZSP_ICD_DENSITY = 1.0
          UNDER_DENSITY = 1.0
          ZEROZSP_DENSITY = 1.0
          UNDER_ERROR = 0.0
          ZEROZSP_ERROR = 0.0
        ELSE
          CALL EZLOC('QCD_JET_CORRECTION_RCP',LRCP)
          IF ( LRCP .LE. 0 ) THEN
            CALL INRCP('QCD_JET_CORRECTION_RCP', IER1 )
            IF ( IER1 .NE. 0 ) THEN
              CALL ERRMSG('RCP error','JET_UNDZSP_FACTOR',
     &          'Cant read in RCP file','F')
              IER = -1
              GOTO 900
            ENDIF
          ENDIF
          CALL EZPICK('QCD_JET_CORRECTION_RCP')
          CALL EZERR( IER1 )
          IF ( IER1 .NE. 0 ) THEN
            CALL ERRMSG('RCP error','JET_UNDZSP_FACTOR',
     &        'Cant find bank ','F')
            IER = -1
            GOTO 900
          ENDIF
          IERTOT = 0
          CALL EZGET('UNDER_DENSITY',UNDER_DENSITY, IER )
          IERTOT = IERTOT + ABS(IER)
          CALL EZGET('UNDER_ICD_DENSITY',UNDER_ICD_DENSITY, IER )
          IERTOT = IERTOT + ABS(IER)
          CALL EZGET('ZEROZSP_DENSITY',ZEROZSP_DENSITY, IER )
          IERTOT = IERTOT + ABS(IER)
          CALL EZGET('ZEROZSP_ICD_DENSITY',ZEROZSP_ICD_DENSITY, IER )
          IERTOT = IERTOT + ABS(IER)
          CALL EZGET('UNDER_ERROR',UNDER_ERROR, IER )
          IERTOT = IERTOT + ABS(IER)
          CALL EZGET('ZEROZSP_ERROR',ZEROZSP_ERROR, IER )
          IERTOT = IERTOT + ABS(IER)
          CALL EZRSET
          IF ( IERTOT .NE. 0 ) THEN
            CALL ERRMSG('RCP error','JET_UNDZSP_FACTOR',
     &        'Read error:abort ','F')
            IER = -1
            GOTO 900
          ENDIF
        ENDIF
      ENDIF
C
      ETA_NEW = CONE_ARRY(NUM_CONES,1)       !NEW JET COORD
      PHI_NEW = CONE_ARRY(NUM_CONES,2)
C
C: Use densities for energies and transverse energies in cone
C: Break the calculation up into eta,phi squares and assign each square
C: to a region 1-4 depending on which jet(s) contain the square.
C: We adjust the UNDERLYING event numbers by the number of interactions
C
C- Also split area into phi slices - then do calc. for overlap region
C: Use .099 instead of .1 to avoid round off problems that would
C: stop the loop prematurely
C
      DO ETA_SLICE = ETA_NEW - CONE_USED, ETA_NEW + CONE_USED -.05, .099
C
C
        DO PHI_SLICE = PHI_NEW - CONE_USED, PHI_NEW + CONE_USED-.05,.099
C
          PHI_PIECE = PHI_SLICE
          IF (PHI_PIECE .LT. 0.0) THEN
            PHI_PIECE = 2.0*PIE + PHI_PIECE
          ELSEIF (PHI_PIECE .GT. 2.0*PIE) THEN
            PHI_PIECE = PHI_PIECE - 2.0*PIE
          ENDIF
C
          DISTONW = DELTA_R(ETA_SLICE,ETA_NEW,PHI_PIECE,PHI_NEW)
          IN_NEW = ( DISTONW .LE.  CONE_USED )
C
          IF (IN_NEW) THEN                    ! IS POINT INSIDE NEW JET?
            IN_OLD = .FALSE.
            IN_OTHER = .FALSE.
C
C ****  LOOK THROUGH ALL CONES TO FIND OVERLAPPING AREAS W/ NEWJET
C
            DISTOLD = 999.9
            DO ICONE = 1,NUM_CONES-1          ! DONT LOOK FOR OVERLAP W/ ITSELF
              ETA_OTHER = CONE_ARRY(ICONE,1)        !LAST CONE IS NEWJET
              PHI_OTHER = CONE_ARRY(ICONE,2)
              TMPDIST = DELTA_R(ETA_OTHER,ETA_SLICE,PHI_OTHER,PHI_PIECE)
C
              IF (TMPDIST.LE.CONE_USED) THEN
                OTHER_JET = NINT(CONE_ARRY(ICONE,3))
                IF (OTHER_JET .EQ. OLDJET) THEN
                  IN_OLD = .TRUE.
                  DISTOLD = MIN(TMPDIST,DISTOLD)
                ELSE
                  IN_OTHER = .TRUE.
                  GOTO 666
C                  ICONE = NUM_CONES-1       ! GET OUT OF LOOP, POINT IN 3RD JET
                ENDIF
              ENDIF
C
            ENDDO
  666       CONTINUE
C
            IAREA = 0
            IF ( (.NOT. IN_OTHER) .AND. (.NOT. IN_OLD) )THEN  ! AREA in NEWJET
              IAREA = 3
            ELSEIF ((IN_OLD) .AND. (.NOT. IN_OTHER)) THEN     ! OVERLAP AREA
              IAREA = 1                                        ! AREA ->NEWJET
              IF ( DISTOLD .LT. DISTONW ) IAREA = 2            ! AREA ->OLDJET
            ELSE
              IAREA = 4                                       ! Overlap w/ 3rd
            ENDIF                                             ! jet, ignore
C
            IF ((IAREA.GT.0).AND.(IAREA.LT.4)) THEN      !AREA NOT IN A 3RD JET
              SINTHETA = 1./COSH(ETA_SLICE)
              FACTOR_N = FACTOR*ABS(N_INTERACTIONS)
              IF ( ABS(ETA_SLICE + SMALL) .GE. 1.2  .AND.
     &                          ABS(ETA_SLICE - SMALL) .LE. 1.5 ) THEN
                ICD_ADD_UNDE(IAREA) = ICD_ADD_UNDE(IAREA) +
     &                              FACTOR_N*UNDER_ICD_DENSITY/SINTHETA
                ICD_ADD_UND(IAREA)= ICD_ADD_UND(IAREA)+
     &                                       FACTOR_N*UNDER_ICD_DENSITY
                ICD_ADD_ZSPE(IAREA)  = ICD_ADD_ZSPE(IAREA) +
     &                                       FACTOR*ZEROZSP_ICD_DENSITY
                ICD_ADD_ZSP(IAREA)   = ICD_ADD_ZSP(IAREA) +
     &                              FACTOR*ZEROZSP_ICD_DENSITY*SINTHETA

              ENDIF
c
              UNDER_E(IAREA) = UNDER_E(IAREA) +
     &                                  FACTOR_N*UNDER_DENSITY/SINTHETA
              UNDER_ET(IAREA)= UNDER_ET(IAREA)+ FACTOR_N*UNDER_DENSITY
              ZSP_E(IAREA)   = ZSP_E(IAREA)   + FACTOR*ZEROZSP_DENSITY
              ZSP_ET(IAREA)  = ZSP_ET(IAREA)  +
     &                                  FACTOR*ZEROZSP_DENSITY*SINTHETA
            ENDIF
C
          ENDIF   !in newjet
        ENDDO     !phi_slice
C
      ENDDO       !eta_slice
C
C: Add ICD contributions in
C
      DO IAREA = 1,3
        UNDER_E(IAREA)  = UNDER_E(IAREA) + ICD_ADD_UNDE(IAREA)
        UNDER_ET(IAREA) = UNDER_ET(IAREA) + ICD_ADD_UND(IAREA)
        ZSP_E(IAREA)    = ZSP_E(IAREA) + ICD_ADD_ZSPE(IAREA)
        ZSP_ET(IAREA)   = ZSP_ET(IAREA) + ICD_ADD_ZSP(IAREA)
      ENDDO
C
C: Determine errors for entry point
C
      DO IAREA = 1,3
        UNDER_ERR(IAREA)   = UNDER_E(IAREA)*(UNDER_ERROR/UNDER_DENSITY)
        UNDER_ETERR(IAREA) = UNDER_ET(IAREA)*(UNDER_ERROR/UNDER_DENSITY)
        ZSP_ERR(IAREA)     = ZSP_E(IAREA)*
     &                                   (ZEROZSP_ERROR/ZEROZSP_DENSITY)
        ZSP_ETERR(IAREA)   = ZSP_ET(IAREA)*
     &                                   (ZEROZSP_ERROR/ZEROZSP_DENSITY)
      ENDDO
      GOTO 999
C
C: RCP error
C
  900 CONTINUE
      DO IAREA = 1,3
        ZSP_ET(IAREA) = -999.
        ZSP_E(IAREA)  = -999.
        UNDER_E(IAREA)= -999.
        UNDER_ET(IAREA)= -999.
      ENDDO

  999 RETURN

C*********************************************************
C ENTRY JET_UNDZSP_ERROR to report errors of the above
C WARNING: This entry is not valid unless preceded by a call to
C          JET_UNDZSP_FACTOR!
C*********************************************************
C
      ENTRY SM_JET_UNDZSP_ERROR( E1, E2, E3, E4 )
      DO IAREA = 1,3
        E1(IAREA) = UNDER_ERR(IAREA)
        E2(IAREA) = UNDER_ETERR(IAREA)
        E3(IAREA) = ZSP_ERR(IAREA)
        E4(IAREA) = ZSP_ETERR(IAREA)
      ENDDO
      RETURN

C*********************************************************
C ENTRY JET_ICD_ADD to report contributions of ICD region.
C WARNING: This entry is not valid unless preceded by a call to
C          JET_UNDZSP_FACTOR!
C*********************************************************

      ENTRY SM_JET_ICD_ADD( E5, E6, E7, E8 )
      DO IAREA = 1, 3
        E5(IAREA)  =   ICD_ADD_UNDE(IAREA)
        E6(IAREA)  =   ICD_ADD_UND(IAREA)
        E7(IAREA)  =   ICD_ADD_ZSPE(IAREA)
        E8(IAREA)  =   ICD_ADD_ZSP(IAREA)
      ENDDO
      RETURN

      END
