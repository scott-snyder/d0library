C VAX/DEC CMS REPLACEMENT HISTORY, Element JET_UNDZSP_FACTOR.FOR
      SUBROUTINE JET_UNDZSP_FACTOR( CONE_USED, N_INTERACTIONS,
     &  ETA,UNDER_E, UNDER_ET, ZSP_E, ZSP_ET )

C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the amount (in GeV) of underlying event
C-      and zerosuppression noise both in energy and transverse energy.
C-      as a function of detector eta and the number of interactions.
C-
C-   Inputs  : CONE_USED  [R]  : Conesize of jet
C-             N_INTERACTIONS [I] : # of interactions
C-             ETA        [R]  : Detector eta of jet
C-   Outputs : UNDER_E    [R]  : Underlying event energy (minbias) (GeV)
C-             UNDER_ET   [R]  : Underlying event transverse energy
C-             ZSP_E      [R]  : Noise from zero suppression energy (GeV)
C-             ZSP_ET     [R]  : Noise from zero suppression trans energy
C-
C-
C- ENTRY JET_UNDZSP_FACTOR( UNDER_E_ERR, UNDER_ET_ERR, ZSP_E_ERR, ZSP_ET_ERR)
C-      To return the errors on the previous call to JET_UNDZSP_FACTOR.
C-
C- ENTRY JET_ICD_ADD( ICD_UNDE, ICD_UNDET, ICD_ZSPE, ICD_ZSPET )
C-      To return ICD Contributions which are included in the call to
C-      JET_UNDZSP_FACTOR
C-
C-   Created  16-AUG-1993   Richard V. Astur
C-
C-   Modified 30-NOV-1993   R. Astur " Add error reporting and ICD "
C-   Modified 10-NOV-1994   R. Astur " Fix bug in loop (from R. Hirosky)
C-   Modified 10-NOV-1994   R. Astur " Faster sin theta (synder/greenlee)
C-   Modified  9-MAY-1995   R. Astur " Modify parametrizations of noise/ue"
C-   Updated  18-SEP-1995   Bob Hirosky "turn off density factors
C-                          by setting N_INTERACTIONS<0"
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL SMALL
      PARAMETER( SMALL = .0001)
      INTEGER N_INTERACTIONS
      REAL CONE_USED, ETA
      REAL ZSP_E, ZSP_ET, UNDER_E, UNDER_ET
      REAL ZSP_ERR, ZSP_ETERR, UNDER_ERR, UNDER_ETERR
      SAVE ZSP_ERR, ZSP_ETERR, UNDER_ERR, UNDER_ETERR
      REAL E1, E2, E3, E4
      REAL E5, E6, E7, E8
      REAL ETA_SLICE, DPHI, SINTHETA
      REAL FACTOR, FACTOR_N
      INTEGER IER, IER1, IERTOT
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      REAL UNDER_A0_DENSITY, UNDER_A1_DENSITY, UNDER_ICD_DENSITY
      REAL UNDER_ERROR, ZSP_ERROR
      SAVE UNDER_A0_DENSITY, UNDER_A1_DENSITY, UNDER_ICD_DENSITY
      SAVE UNDER_ERROR, ZSP_ERROR
      REAL ZSP_A0_DENSITY, ZSP_A1_DENSITY, ZSP_ICD_DENSITY
      SAVE ZSP_A0_DENSITY, ZSP_A1_DENSITY, ZSP_ICD_DENSITY
      REAL AREA, ZSP_DENSITY, UNDER_DENSITY
      REAL ICD_ADD_ZSP, ICD_ADD_UND
      REAL ICD_ADD_ZSPE, ICD_ADD_UNDE
      SAVE ICD_ADD_ZSP, ICD_ADD_UND
      SAVE ICD_ADD_ZSPE, ICD_ADD_UNDE
C---------------------------------------------------------------------
C
C: Initialize
C
      ZSP_ET     = 0.0
      ZSP_ETERR  = 0.0
      ZSP_E      = 0.0
      ZSP_ERR    = 0.0
      UNDER_E    = 0.0
      UNDER_ERR  = 0.0
      UNDER_ET   = 0.0
      UNDER_ETERR= 0.0
      AREA       = 0.0
      ICD_ADD_ZSP = 0.0
      ICD_ADD_UND = 0.0
      ICD_ADD_ZSPE= 0.0
      ICD_ADD_UNDE= 0.0
C
C: Read values for RCP
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        IF (N_INTERACTIONS.LT.0) THEN  ! Just calculate areas
          N_INTERACTIONS = -1
          UNDER_ICD_DENSITY = 1.0
          UNDER_A0_DENSITY = 1.0
          UNDER_A1_DENSITY = 0.0
          ZSP_A0_DENSITY = 1.0
          ZSP_A1_DENSITY = 0.0
          UNDER_ERROR = 0.0
          ZSP_ERROR = 0.0
        ELSE
          CALL INRCP('QCD_JET_CORRECTION_RCP', IER1 )
          IF ( IER1 .NE. 0 ) THEN
            CALL ERRMSG('RCP error','JET_UNDZSP_FACTOR',
     &        'Cant read in RCP file','F')
            IER = -1
            GOTO 900
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
          CALL EZGET('UNDER_A0_DENSITY',UNDER_A0_DENSITY, IER )
          IERTOT = IERTOT + ABS(IER)
          CALL EZGET('UNDER_A1_DENSITY',UNDER_A1_DENSITY, IER )
          IERTOT = IERTOT + ABS(IER)
          CALL EZGET('UNDER_ICD_DENSITY',UNDER_ICD_DENSITY, IER )
          IERTOT = IERTOT + ABS(IER)
          CALL EZGET('UNDER_ERROR',UNDER_ERROR, IER )
          IERTOT = IERTOT + ABS(IER)
          CALL EZGET('ZEROZSP_A0_DENSITY',ZSP_A0_DENSITY, IER )
          IERTOT = IERTOT + ABS(IER)
          CALL EZGET('ZEROZSP_A1_DENSITY',ZSP_A1_DENSITY, IER )
          IERTOT = IERTOT + ABS(IER)
          CALL EZGET('ZEROZSP_ICD_DENSITY',ZSP_ICD_DENSITY, IER )
          IERTOT = IERTOT + ABS(IER)
          CALL EZGET('ZEROZSP_ERROR',ZSP_ERROR, IER )
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
C: Use densities to energies and transverse energies in cone
C: Since cone can be large, break the calculation up into eta slices.
C: We adjust the UNDERLYING event numbers by the number of interactions
C
C: Use .099 instead of .1 to avoid round off problems that would
C: stop the loop prematurely
      DO ETA_SLICE = ETA - CONE_USED +.05, ETA + CONE_USED -.05, .099
c        SINTHETA = SIN( THETA_FROM_ETA( ETA_SLICE ) )
        SINTHETA  = 1./COSH(ETA_SLICE)        ! Faster
        DPHI     = 2*SQRT( CONE_USED**2 -
     &    ABS(ETA_SLICE - ETA)**2 )
        AREA     = AREA + DPHI*.1
        FACTOR   = DPHI*.1
        FACTOR_N = DPHI*.1*ABS(N_INTERACTIONS)
C
C: ICR contributions
C
        IF ( ABS(ETA_SLICE + SMALL) .GE. 1.1 .AND. ABS(ETA_SLICE -
     &    SMALL) .LE. 1.5 ) THEN
          ICD_ADD_UND   = ICD_ADD_UND + FACTOR_N*UNDER_ICD_DENSITY
          ICD_ADD_ZSP   = ICD_ADD_ZSP + FACTOR*ZSP_ICD_DENSITY
          ICD_ADD_UNDE  = ICD_ADD_UNDE +
     &      FACTOR_N*UNDER_ICD_DENSITY/SINTHETA
          ICD_ADD_ZSPE  = ICD_ADD_ZSPE +
     &      FACTOR*ZSP_ICD_DENSITY/SINTHETA

        ENDIF
C
C: Rest
C
        UNDER_DENSITY = UNDER_A0_DENSITY +
     &    UNDER_A1_DENSITY*ABS(ETA_SLICE)

        ZSP_DENSITY   = ZSP_A0_DENSITY + ZSP_A1_DENSITY*SINTHETA

        UNDER_E = UNDER_E + FACTOR_N*UNDER_DENSITY/SINTHETA
        UNDER_ET= UNDER_ET+ FACTOR_N*UNDER_DENSITY
        ZSP_E   = ZSP_E   + FACTOR*ZSP_DENSITY/SINTHETA
        ZSP_ET  = ZSP_ET  + FACTOR*ZSP_DENSITY
      ENDDO
c
c: Add ICD contributions in
C
      UNDER_E       = UNDER_E + ICD_ADD_UNDE
      UNDER_ET      = UNDER_ET + ICD_ADD_UND
      ZSP_E         = ZSP_E + ICD_ADD_ZSPE
      ZSP_ET        = ZSP_ET+ ICD_ADD_ZSP
C
C: Determine errors for entry point
C
      UNDER_ETERR   = UNDER_ERROR*AREA
      UNDER_ERR     = UNDER_ERROR*AREA*COSH(ETA)
      ZSP_ETERR     = ZSP_ERROR*AREA
      ZSP_ERR       = ZSP_ERROR*AREA*COSH(ETA)

      GOTO 999
C
C: RCP error
C

  900 ZSP_ET = -999.
      ZSP_E  = -999.
      UNDER_E= -999.
      UNDER_ET= -999.

  999 RETURN

C*********************************************************
C ENTRY JET_UNDZSP_ERROR to report errors of the above
C WARNING: This entry is not valid unless preceded by a call to
C          JET_UNDZSP_FACTOR!
C*********************************************************

      ENTRY JET_UNDZSP_ERROR( E1, E2, E3, E4 )
      E1 = UNDER_ERR
      E2 = UNDER_ETERR
      E3 = ZSP_ERR
      E4 = ZSP_ETERR
      RETURN

C*********************************************************
C ENTRY JET_ICD_ADD to report contributions of ICD region.
C WARNING: This entry is not valid unless preceded by a call to
C          JET_UNDZSP_FACTOR!
C*********************************************************

      ENTRY JET_ICD_ADD( E5, E6, E7, E8 )
      E5  =   ICD_ADD_UNDE
      E6  =   ICD_ADD_UND
      E7  =   ICD_ADD_ZSPE
      E8  =   ICD_ADD_ZSP
      RETURN


      END
