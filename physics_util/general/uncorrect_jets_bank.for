      SUBROUTINE UNCORRECT_JETS_BANK( LJETS, OVERWRITE, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the uncorrected values of this JETS
C-                         bank. These values are then available for the
C-                         ENTRY point GET_UNCORRECT_JETS_BANK. The bank
C-                         is overwritten with these values if OVERWRITE
C-                         is TRUE.
C-                         No error is returned if bank has
C-                         never been corrected.
C-
C-   Inputs  :    LJETS   [I]   : Pointer to JETS bank
C-              OVERWRITE [L]   : If TRUE, the JETS bank is overwritten with
C-                                these values and the energy correction
C-                                word is cleared
C-   Outputs :    IER     [I]   : Error condition
C-                                0= ok
C-                               -1= error ( LJETS is invalid )
C                                -2= bank isnt corrected
C-   Controls:
C-
C-   Created  13-DEC-1993   Richard V. Astur
C-   Updated  Oct-12-1995   Bob Kehoe   --  add calculation of ICD and CH
C-                                          energy fractions independent of
C-                                          EM fraction, write into JETS bank
C-   Updated  13-OCT-1995   Bob Hirosky - add AREA_BIT to correction word
C-   Updated  25-Oct-1995   Bob Kehoe   --  implement JETX storage, make more
C-                                          flexible entry point called
C-                                          get_uncorrect_jets_bank_2
C-   Updated  30-OCT-1995   Dhiman Chakraborty
C-                          Replace JETX storage by extended JETS bank
C-   Updated   7-OCT-1996   Bob Hirosky   now keep a copy of uncor jet's bank
C-                                        for cleaner recovery
C-   Updated  18-JUL-1997   Bob Hirosky   dump JQAN on uncorrect
C-   Updated  28-OCT-1997   BOB HIROSKY   JQAN(5) == JETS(29)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZUJET.LINK'
      INCLUDE 'D0$LINKS:IZJQAN.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL PIE, SMALL
      PARAMETER( SMALL = .01 )
      PARAMETER( PIE = 1.0*PI )
      INTEGER CORRECTED_BIT, OUT_OF_CONE_BIT, NOISE_BIT, UNDER_EVT_BIT
      INTEGER AREA_BIT
      PARAMETER( CORRECTED_BIT = 0 )    ! Was correction done
      PARAMETER( OUT_OF_CONE_BIT = 1 )  ! Was out of cone correction done
      PARAMETER( NOISE_BIT = 2 )        ! Was zero suppres. correction done
      PARAMETER( UNDER_EVT_BIT = 3 )    ! Was underlying event correction done
      PARAMETER( AREA_BIT = 4 )         ! Was area & sin(theta) wgtd area found
      INTEGER LJETS, IER, I,nd_jets
      LOGICAL OVERWRITE
      CHARACTER*3 FLAG
      REAL NEW_VALUE, THETA, PTOT, PT, PHI
      LOGICAL IS_CORRECTED
      REAL E_SCALE_FACTOR, ET_SCALE_FACTOR, DETA, P_SCALE_FACTOR
      REAL ZSP_ET, UND_ET, DEMF, DPHI,dchf,dicdf
      REAL UET, UE, UEX, UEY, UEZ, UETA, UPHI, UEMF, UTHETA
      SAVE UET, UE, UEX, UEY, UEZ, UETA, UPHI, UEMF, UTHETA
      REAL uchf,uicdf,farray(12)
      SAVE uchf,uicdf
      INTEGER VERSION, LUJET, LJQAN, NDATA
      REAL AE(5), APHI, ATHETA, AETA, AEMF
      REAL MAXABS, X1
C----------------------------------------------------------------------
C
C: Statement function : MAXABS(x1) return x1 if abs(x1) > small else
C:                      return small (with the sign of x1)
C
      MAXABS( X1 ) = MAX( SMALL, ABS((X1)) ) * SIGN( 1., (X1) )
C----------------------------------------------------------------------
      VERSION = IQ(LJETS+1)
      IF (VERSION.GE.10) THEN
        LUJET = LQ( LJETS - IZUJET )
        IF (LUJET.GT.0) THEN   !old jet is hanging off of new one
          UEX       = Q( LUJET + 2 )
          UEY       = Q( LUJET + 3 )
          UEZ       = Q( LUJET + 4 )
          UE        = Q( LUJET + 5 )
          UET       = Q( LUJET + 6 )
          UPHI      = Q( LUJET + 8 )
          UTHETA    = Q( LUJET + 7 )
          UETA      = Q( LUJET + 9 )
          UEMF      = Q( LUJET + 14)
          UICDF     = Q(LUJET+17)
          UCHF      = Q(LUJET+18)
          IF (OVERWRITE) THEN   ! get info from hanging bank
            NDATA = IQ(LJETS-1)
            DO i = 1,NDATA
              Q(LJETS+i) = Q(LUJET+i)
            ENDDO
            CALL mzdrop(IXMAIN,LUJET,'L')
            LQ( LJETS - IZUJET ) = 0
            LJQAN = LQ( LJETS - IZJQAN )
            CALL mzdrop(IXMAIN,LJQAN,'L')
            LQ( LJETS - IZJQAN ) = 0
          ENDIF
        ENDIF
        RETURN
      ENDIF

      IER = -1
      UEX             = -999.
      UEY             = -999.
      UEZ             = -999.
      UE              = -999.
      UET             = -999.
      UEMF            = -999.
      uchf = -999.
      uicdf = -999.
      UETA            = -999.
      UPHI            = -999.
      UTHETA          = -999.
      IF ( LJETS .LE. 0 ) RETURN
      IER = -2
      IF ( IQ( LJETS - 1) .LT. 35 .OR. (.NOT. BTEST( IQ(LJETS+26),
     &      CORRECTED_BIT ) ) ) THEN
        UEX             = Q( LJETS + 2 )
        UEY             = Q( LJETS + 3 )
        UEZ             = Q( LJETS + 4 )
        UE              = Q( LJETS + 5 )
        UET             = Q( LJETS + 6 )
        UEMF            = Q( LJETS + 14)
        uicdf = q(ljets+17)
        uchf = q(ljets+18)
        UETA            = Q( LJETS + 9 )
        UPHI            = Q( LJETS + 8 )
        UTHETA          = Q( LJETS + 7 )
        RETURN
      ENDIF
      IER = 0
C
C: Get correction factors
C
      E_SCALE_FACTOR  = Q( LJETS + 28 )
      ET_SCALE_FACTOR = Q( LJETS + 29 )
      DETA            = Q( LJETS + 32 )
      DEMF            = Q( LJETS + 33 )
      P_SCALE_FACTOR  = Q( LJETS + 34 )
      DPHI            = Q( LJETS + 35 )
      DCHF = Q(LJETS+46)
      DICDF = Q(LJETS+47)
C
C: Correct kinematic variables
C
      UEX             = Q( LJETS + 2 )/P_SCALE_FACTOR
      UEY             = Q( LJETS + 3 )/P_SCALE_FACTOR
      UEZ             = Q( LJETS + 4 )/P_SCALE_FACTOR
      UE              = Q( LJETS + 5 )/E_SCALE_FACTOR
      UET             = Q( LJETS + 6 )/ET_SCALE_FACTOR
      UEMF            = Q( LJETS + 14) - DEMF
      uicdf = q(ljets+17) - dicdf
      uchf = q(ljets+18) - dchf
      UETA            = Q( LJETS + 9 ) - DETA
      UPHI            = Q( LJETS + 8 ) - DPHI
      UTHETA          = 2*ATAN( EXP(-UETA) )
      UTHETA          = MOD( 4*PIE + UTHETA, PIE )
C
C: Rescale 3 vector from eta,phi change
C
      PTOT  = SQRT( UEX**2 + UEY**2 + UEZ**2 )
      PT    = PTOT * SIN( UTHETA )
      UEZ   = PTOT * COS( UTHETA )
      UEX   = PT   * COS( UPHI   )
      UEY   = PT   * SIN( UPHI   )

      IF ( OVERWRITE ) THEN
        Q( LJETS + 2 )    =   UEX
        Q( LJETS + 3 )    =   UEY
        Q( LJETS + 4 )    =   UEZ
        Q( LJETS + 5 )    =   UE
        Q( LJETS + 6 )    =   UET
        Q( LJETS + 7 )    =   UTHETA
        Q( LJETS + 8 )    =   UPHI
        Q( LJETS + 9 )    =   UETA
        Q( LJETS + 14)    =   UEMF
        q(ljets+17) = uicdf
        q(ljets+18) = uchf
C
C: Wipe record of correction
C
        DO I = 28, 35
          IQ( LJETS + I)= 0             ! Energy calibration words
        ENDDO

        IQ( LJETS + 26 )= 0             ! Energy correction status word

      ENDIF

  999 RETURN
C**********************************************************
C Entry GET_UNCORRECT_JETS_BANK - original entry pt to Return uncorrected values
C**********************************************************
      ENTRY GET_UNCORRECT_JETS_BANK( AE, APHI, ATHETA, AETA, AEMF )
      AE(1) = UEX
      AE(2) = UEY
      AE(3) = UEZ
      AE(4) = UE
      AE(5) = UET
      APHI  = UPHI
      ATHETA= UTHETA
      AETA  = UETA
      AEMF  = UEMF
      RETURN
C**********************************************************
C Entry GET_UNCORRECT_JETS_BANK_2 - Return uncorrected values
C**********************************************************
      ENTRY GET_UNCORRECT_JETS_BANK_2(farray)
      farray(1) = UEX
      farray(2) = UEY
      farray(3) = UEZ
      farray(4) = UE
      farray(5) = UET
      farray(6) = UTHETA
      farray(7) = UPHI
      farray(8) = UETA
      farray(9) = UEMF
      farray(10) = uicdf
      farray(11) = uchf
      RETURN
C********************************************************
C Entry CORRECT_JETS_BANK - Overwrite JETS bank with appropriate corrections
C*******************************************************

      ENTRY CORRECT_JETS_BANK( LJETS, FLAG, NEW_VALUE, IER )

      IER = -1
      IF ( LJETS .LE. 0 ) RETURN
      IF ( IQ( LJETS - 1) .LT. 35 ) RETURN
      IER = 0
      ND_JETS = IQ(LJETS-1)
C
C: If this bank hasnt been corrected before, setup for this and future
C: corrections
C
      IF ( .NOT. BTEST( IQ( LJETS+26), CORRECTED_BIT ) ) THEN
        VERSION = IQ(LJETS+1)
        IF (VERSION.GE.10) THEN ! copy old jet for later recovery
          CALL MZCOPY(IXMAIN,LJETS,IXMAIN,LUJET,2,'S')
          LQ(LJETS-IZUJET) = LUJET
        ENDIF
        IQ( LJETS + 26 ) = IBSET( IQ( LJETS + 26), CORRECTED_BIT )
        Q( LJETS + 28 ) = 1.0            ! Muliplicative scales
        Q( LJETS + 29 ) = 1.0
        Q( LJETS + 34 ) = 1.0
        Q( LJETS + 32 ) = 0.0            ! Additive offsets
        Q( LJETS + 33 ) = 0.0
        Q( LJETS + 35 ) = 0.0
        q(ljets+46) = 0.0
        q(ljets+47) = 0.0
      ENDIF
C
      IF ( FLAG .EQ. 'ET ') THEN
        ET_SCALE_FACTOR = MAXABS(NEW_VALUE)/MAXABS(Q(LJETS+6))
        Q( LJETS + 29 ) = Q( LJETS + 29) * ET_SCALE_FACTOR
        LJQAN = LQ( LJETS - IZJQAN ) 
        Q( LJQAN + 5 ) = Q( LJETS + 29 )  ! keep JQAN cor. factor current
        Q( LJETS + 6  ) = MAXABS( NEW_VALUE )
C
      ELSEIF ( FLAG .EQ. 'E  ') THEN
        E_SCALE_FACTOR  = MAXABS( NEW_VALUE )/MAXABS( Q(LJETS+5) )
        Q( LJETS + 28 ) = Q( LJETS + 28 ) * E_SCALE_FACTOR
        Q( LJETS + 5  ) = MAXABS( NEW_VALUE )
C
      ELSEIF ( FLAG .EQ. 'P  ') THEN
        PTOT            = SQRT( Q(LJETS+2)**2 + Q(LJETS+3)**2 + Q(
     &        LJETS+4)**2)
        THETA           = Q( LJETS + 7 )
        PHI             = Q( LJETS + 8 )
        P_SCALE_FACTOR  = MAXABS( NEW_VALUE )/MAXABS( PTOT )
        Q( LJETS + 2 ) = MAXABS( NEW_VALUE ) * SIN( THETA ) * COS( PHI )
        Q( LJETS + 3 ) = MAXABS( NEW_VALUE ) * SIN( THETA ) * SIN( PHI )
        Q( LJETS + 4 ) = MAXABS( NEW_VALUE ) * COS( THETA )
        Q( LJETS + 34) = Q( LJETS + 34 ) * P_SCALE_FACTOR
C
      ELSEIF ( FLAG .EQ. 'ETA') THEN
        DETA            = NEW_VALUE - Q( LJETS + 9)
        Q( LJETS + 9)   = NEW_VALUE
        Q( LJETS + 32)  = Q( LJETS + 32) + DETA
C
C: Rescale 3 vector from eta change
C
        THETA           = 2*ATAN( EXP(-Q( LJETS+9) ) )
        THETA           = MOD( THETA +4*PIE, PIE )
        Q( LJETS + 7)   = THETA
        PTOT  = SQRT( Q( LJETS + 2)**2 + Q( LJETS + 3)**2 + Q( LJETS +4)
     &        **2 )
        PT    = SQRT( Q( LJETS +2)**2 + Q( LJETS +3)**2 )
        Q( LJETS + 4 )  = PTOT * COS( THETA )
        Q( LJETS + 2 )  = Q( LJETS + 2) * PTOT*SIN( THETA )/PT
        Q( LJETS + 3 )  = Q( LJETS + 3) * PTOT*SIN( THETA )/PT
C
      ELSEIF ( FLAG .EQ. 'PHI') THEN
        DPHI            = NEW_VALUE - Q( LJETS + 8)
        Q( LJETS + 8)   = NEW_VALUE
        Q( LJETS + 35)  = Q( LJETS + 35) + DPHI
C
C: Rescale 3 vector from eta change
C
        PT    = SQRT( Q( LJETS +2)**2 + Q( LJETS +3)**2 )
        Q( LJETS + 2 )  = PT * COS( Q( LJETS + 8 ) )
        Q( LJETS + 3 )  = PT * SIN( Q( LJETS + 8 ) )

      ELSEIF ( FLAG .EQ. 'EMF') THEN        ! save em-fraction and changes
        DEMF            = NEW_VALUE - Q( LJETS + 14)
        Q( LJETS + 14)  = NEW_VALUE
        Q( LJETS + 33)  = Q( LJETS + 33 ) + DEMF
      ELSEIF ( FLAG .EQ. 'ICF') THEN        ! save ICD-fraction and changes
        dicdf           = NEW_VALUE - Q( LJETS + 17)
        Q( LJETS + 17)  = NEW_VALUE
        q(ljets+47) = q(ljets+47) + dicdf
      ELSEIF ( FLAG .EQ. 'CHF') THEN        ! save CH-fraction and changes
        dchf            = NEW_VALUE - Q( LJETS + 18)
        Q( LJETS + 18)  = NEW_VALUE
        q(ljets+46) = q(ljets+46) + dchf
      ELSEIF ( FLAG .EQ. 'UND') THEN
        UND_ET          = NEW_VALUE
        Q( LJETS + 31 ) = NEW_VALUE
      ELSEIF ( FLAG .EQ. 'ZSP') THEN
        ZSP_ET          = NEW_VALUE
        Q( LJETS + 30 ) = NEW_VALUE
      ELSEIF ( FLAG .EQ. 'STL') THEN        ! save low statistical error
        q(ljets+48) = new_value
      ELSEIF ( FLAG .EQ. 'STH') THEN        ! save high statistical error
        q(ljets+49) = new_value
      ELSEIF ( FLAG .EQ. 'SYL') THEN        ! save low systematic error
        q(ljets+50) = new_value
      ELSEIF ( FLAG .EQ. 'SYH') THEN        ! save high systematic error
        q(ljets+51) = new_value
      ELSE
        IER = -2
        CALL ERRMSG('INVALID FLAG','CORRECT_JETS_BANK',
     &        'INVALID FLAG PASSED', 'W')
        RETURN
      ENDIF

      RETURN

C********************************************
C Entry JETS_BANK_CORRECTED - IS_CORRECTED is TRUE if this JETS bank has been
C                             corrected in the manner specified by flag
C********************************************
      ENTRY JETS_BANK_CORRECTED( LJETS, FLAG, IS_CORRECTED, IER )
      IER = -1
      IF ( LJETS .LE. 0 ) RETURN
      IER = 0
      IS_CORRECTED = .FALSE.
      IF ( IQ( LJETS - 1) .LT. 35 ) RETURN

C
C: Check the various correction flags
C
      IF ( FLAG .EQ. 'COR' ) THEN
        IS_CORRECTED = BTEST( IQ( LJETS + 26), CORRECTED_BIT )
      ELSEIF ( FLAG .EQ. 'OOC' ) THEN
        IS_CORRECTED = BTEST( IQ( LJETS + 26), OUT_OF_CONE_BIT )
      ELSEIF ( FLAG .EQ. 'ZSP' ) THEN
        IS_CORRECTED = BTEST( IQ( LJETS + 26), NOISE_BIT )
      ELSEIF ( FLAG .EQ. 'UND' ) THEN
        IS_CORRECTED = BTEST( IQ( LJETS + 26), UNDER_EVT_BIT )
      ELSE
        IER = -2
      ENDIF

      RETURN

C********************************************
C Entry SET_JETS_BANK_CORRECTED - Sets the energy correction status word in the
C                                 jets bank to indicated it has been
C                                 corrected in the manner specified by flag
C********************************************
      ENTRY SET_JETS_BANK_CORRECTED( LJETS, FLAG, IER )
      IER = -1
      IF ( LJETS .LE. 0 ) RETURN
      IF ( IQ( LJETS - 1) .LT. 35 ) RETURN
      IER = 0

C
C: Check the various correction flags
C
      IF ( FLAG .EQ. 'COR' ) THEN
        IQ(LJETS + 26) = IBSET( IQ( LJETS + 26), CORRECTED_BIT )
      ELSEIF ( FLAG .EQ. 'OOC' ) THEN
        IQ(LJETS + 26) = IBSET( IQ( LJETS + 26), OUT_OF_CONE_BIT )
      ELSEIF ( FLAG .EQ. 'ZSP' ) THEN
        IQ(LJETS + 26) = IBSET( IQ( LJETS + 26), NOISE_BIT )
      ELSEIF ( FLAG .EQ. 'UND' ) THEN
        IQ(LJETS + 26) = IBSET( IQ( LJETS + 26), UNDER_EVT_BIT )
      ELSEIF ( FLAG .EQ. 'ARA' ) THEN
        IQ(LJETS + 26) = IBSET( IQ( LJETS + 26), AREA_BIT )
      ELSE
        IER = -2
      ENDIF

      RETURN

      END
