      SUBROUTINE   PROM_RESPONSES ( SIGN_ETA, MAGN_ETA, PHI, 
     &                              L0_BIN, 
     &                              EM_ADC_BYTE, HD_ADC_BYTE, 
     &                              LUQ_BYTE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reconstruct the set of prom responses for the 
C-                         specified set of ADC bytes, and the specified 
C-                         Level 0 Trigger information.
C-
C-   Inputs  : 
C-     SIGN_ETA            is the sign of the eta index of the trigger
C-                         tower for which the PROM response is reconstructed.
C-                         Use 0 to specify a negative eta index, 1 to
C-                         specify a positive eta index.
C-                         It is recommended to use one of the compilation
C-                         constants defined along with the common block. 
C-                         POS_ETA=0 or NEG_ETA=1.
C-
C-     MAGN_ETA            is the magnitutde of the eta index of the
C-                         trigger tower for which the PROM response 
C-                         is reconstructed.
C-                         Range : 1 to 24
C-
C-     PHI                 is the phi index of the trigger tower for
C-                         which the PROM response is reconstructed.
C-                         Range : 1 to 32
C-
C-     EM_ADC_BYTE         is the ADC output byte (simulated or from First
C-                         Level Trigger Data Block information) of the
C-                         ELectro-Magnetic Trigger Tower for which the
C-                         PROM response is reconstructed.
C-
C-     HD_ADC_BYTE         is the ADC output byte (simulated or from First
C-                         Level Trigger Data Block information) of the
C-                         Hadronic Trigger Tower for which the
C-                         PROM response is reconstructed.
C-
C-   Outputs : 
C-     LUQ_BYTE            is the expected set of PROM ouptuts for all of the
C-                         constructed quantities.
C-                         The PROMs will correct the first estimate of the EM,
C-                         HD or TOT=EM+HD transverse energy using the
C-                         information about the position of the interaction
C-                         vertex made available by the Level 0 Trigger. 
C-                         
C-                         See the parameter file for the ordering of the
C-                         elements in this one dimension array. It is highly
C-                         recommended to always access an element by the
C-                         parameter pointing to its rank, not by the parameter
C-                         value. 
C-
C-   Comments :
C-                         A call to INIT_LOOKUP_ROUTINES must have been made
C-                         before this routine can be called.
C-
C-                         No verification is made of the integrity of the
C-                         arguments specifying the indices of the Trigger
C-                         Tower or the Level 0 bin.
C-
C-                         The subroutine will return a zero in any value for
C-                         which the lookup (or the tower) is not implemented.
C-
C-                         This simulation will take into account any scale
C-                         shift performed by the PROM.
C-
C-                         This simulation will take into account any low
C-                         energy cutoff performed by the PROM.
C-
C-                         This simulation will take into account any input or
C-                         output offset handled by the PROM.
C-
C-                         This simulation will take into account the fine
C-                         tuning "Tweak" of each lookup page.
C-
C-   Defined 29-JAN-1990 MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-   Updated  2-AUG-1990 MICHIGAN STATE UNIVERSTITY, TRIGGER CONTROL SOFTWARE
C-   Updated  Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                            - Made IMPLICIT NONE statement recognizable by
C-                              D0FLAVOR
C----------------------------------------------------------------------
      IMPLICIT NONE 
C      
      INCLUDE   'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE   'D0$INC:LEVEL1_LOOKUP.INC'
C
      INTEGER    SIGN_ETA, MAGN_ETA, PHI
      INTEGER    L0_BIN
      INTEGER    EM_ADC_BYTE, HD_ADC_BYTE 
      INTEGER    LUQ_BYTE ( EM_ET_QUANT:PY_QUANT )
C
C
      INTEGER    SATURATION
      PARAMETER (SATURATION=255)
C
      INTEGER    PAGE, DAC_EM, DAC_HD
      INTEGER    EM_ADC_ZERESP, HD_ADC_ZERESP
      REAL       ADC_CORR(EM_TOWER:TOT_TOWER)
C
C----------------------------------------------------------------------
C
      IF (LSM_SANITY_CHECKS .EQV. .TRUE.) THEN
        IF (((SIGN_ETA .NE. POS_ETA) .AND.
     &       (SIGN_ETA .NE. NEG_ETA))     .OR.
     &      ((MAGN_ETA .LT. ETA_MIN) .OR.
     &       (MAGN_ETA .GT. ETA_MAX))     .OR.
     &      ((PHI .LT. PHI_MIN) .OR.
     &       (PHI .GT. PHI_MAX))          .OR.
     &      ((L0_BIN .LT. L0_BIN_MIN) .OR.
     &       (L0_BIN .GT. L0_BIN_MAX))) THEN
          CALL ABORT( 'ERROR: Parameter out of range in PROM_RESPONSES')
        ENDIF
        IF ((EM_ADC_BYTE .GT. 255) .OR. 
     &      (EM_ADC_BYTE .LT. 0) .OR.
     &      (HD_ADC_BYTE .GT. 255) .OR.
     &      (HD_ADC_BYTE .LT. 0)) THEN
          CALL ABORT( 'ERROR: Byte out of range in PROM_RESPONSES')
        ENDIF
      ENDIF
C
      EM_ADC_ZERESP       = ADC_ZERESP(SIGN_ETA,MAGN_ETA,PHI,EM_TOWER)
      HD_ADC_ZERESP       = ADC_ZERESP(SIGN_ETA,MAGN_ETA,PHI,HD_TOWER)
      ADC_CORR(EM_TOWER)  = FLOAT( EM_ADC_BYTE - EM_ADC_ZERESP ) 
      ADC_CORR(HD_TOWER)  = FLOAT( HD_ADC_BYTE - HD_ADC_ZERESP ) 
C       Px and Py PROMs receive the highest 8 bits of the 9 bit (EM+HD)
      ADC_CORR(TOT_TOWER) = FLOAT( (EM_ADC_BYTE + HD_ADC_BYTE) / 2 )
     &                    - FLOAT( EM_ADC_ZERESP + HD_ADC_ZERESP ) / 2.0
     &                    + 0.25
      DAC_EM = DAC_BYTE(SIGN_ETA, MAGN_ETA, PHI, EM_TOWER)
      DAC_HD = DAC_BYTE(SIGN_ETA, MAGN_ETA, PHI, HD_TOWER)
C
C     EM Et lookup, fully commented
C
      PAGE = LUQ_PAGE_INDEX ( EM_ET_QUANT,
     &                        LUQ_PAGE_NUMBER(EM_ET_QUANT,L0_BIN) )
C
      IF (( PAGE .EQ. 0 ) .OR. (DAC_EM .EQ. 0)) THEN 
C
C       Lookup or tower not implemented
C
        LUQ_BYTE(EM_ET_QUANT) = 0
C
      ELSE
C
C       Apply Multiplicative Vertex Correction
C
        LUQ_BYTE(EM_ET_QUANT) = NINT( ADC_CORR(EM_TOWER)
     &                * PROM_SLOPE(SIGN_ETA,MAGN_ETA,PHI,EM_PROM,PAGE) )
C
C       Two Sided Cut
C
        IF ( ABS( LUQ_BYTE(EM_ET_QUANT) )
     &     .LT. PROM_CUT (SIGN_ETA,MAGN_ETA,PHI,EM_PROM,PAGE) ) 
     &   LUQ_BYTE(EM_ET_QUANT) = 0
C
C       Offset
C
        LUQ_BYTE(EM_ET_QUANT) = LUQ_BYTE(EM_ET_QUANT) 
     &                + LOOKUP_ZERESP(SIGN_ETA,MAGN_ETA,PHI,EM_ET_QUANT)
C
C       Output Must Stay Within Range
C
        IF ( LUQ_BYTE(EM_ET_QUANT) .LT. 0 ) 
     &    LUQ_BYTE(EM_ET_QUANT) = 0  
        IF ( LUQ_BYTE(EM_ET_QUANT) .GT. SATURATION ) 
     &    LUQ_BYTE(EM_ET_QUANT) = SATURATION 
C
      END IF
C
C     Same for EM L2 lookup
C
      PAGE = LUQ_PAGE_INDEX ( EM_L2_QUANT,
     &                        LUQ_PAGE_NUMBER(EM_L2_QUANT,L0_BIN) )
C
      IF (( PAGE .EQ. 0 ) .OR. (DAC_EM .EQ. 0)) THEN 
        LUQ_BYTE(EM_L2_QUANT) = 0
      ELSE
        LUQ_BYTE(EM_L2_QUANT) = NINT( ADC_CORR(EM_TOWER) 
     &                * PROM_SLOPE(SIGN_ETA,MAGN_ETA,PHI,EM_PROM,PAGE) )
        IF ( ABS( LUQ_BYTE(EM_L2_QUANT) )
     &      .LT. PROM_CUT (SIGN_ETA, MAGN_ETA,PHI, EM_PROM, PAGE) ) 
     &    LUQ_BYTE(EM_L2_QUANT) = 0
        LUQ_BYTE(EM_L2_QUANT) = LUQ_BYTE(EM_L2_QUANT) 
     &                + LOOKUP_ZERESP(SIGN_ETA,MAGN_ETA,PHI,EM_L2_QUANT)
        IF ( LUQ_BYTE(EM_L2_QUANT) .LT. 0 ) 
     &    LUQ_BYTE(EM_L2_QUANT) = 0
        IF ( LUQ_BYTE(EM_L2_QUANT) .GT. SATURATION ) 
     &    LUQ_BYTE(EM_L2_QUANT) = SATURATION 
      END IF
C
C     Same for HD Et lookup
C
      PAGE = LUQ_PAGE_INDEX ( HD_ET_QUANT,
     &                        LUQ_PAGE_NUMBER(HD_ET_QUANT,L0_BIN) )
C
      IF (( PAGE .EQ. 0 ) .OR. (DAC_HD .EQ. 0)) THEN 
        LUQ_BYTE(HD_ET_QUANT) = 0
      ELSE
        LUQ_BYTE(HD_ET_QUANT) = NINT( ADC_CORR(HD_TOWER) 
     &                * PROM_SLOPE(SIGN_ETA,MAGN_ETA,PHI,HD_PROM,PAGE) )
        IF ( ABS( LUQ_BYTE(HD_ET_QUANT) )
     &      .LT. PROM_CUT (SIGN_ETA, MAGN_ETA,PHI, HD_PROM, PAGE) ) 
     &    LUQ_BYTE(HD_ET_QUANT) = 0
        LUQ_BYTE(HD_ET_QUANT) = LUQ_BYTE(HD_ET_QUANT) 
     &                + LOOKUP_ZERESP(SIGN_ETA,MAGN_ETA,PHI,HD_ET_QUANT)
        IF ( LUQ_BYTE(HD_ET_QUANT) .LT. 0 ) 
     &    LUQ_BYTE(HD_ET_QUANT) = 0
        IF ( LUQ_BYTE(HD_ET_QUANT) .GT. SATURATION ) 
     &    LUQ_BYTE(HD_ET_QUANT) = SATURATION 
      END IF
C
C     Same for HD L2 lookup
C
      PAGE = LUQ_PAGE_INDEX ( HD_L2_QUANT,
     &                        LUQ_PAGE_NUMBER(HD_L2_QUANT,L0_BIN) )
C
      IF (( PAGE .EQ. 0 ) .OR. (DAC_HD .EQ. 0)) THEN 
        LUQ_BYTE(HD_L2_QUANT) = 0
      ELSE
        LUQ_BYTE(HD_L2_QUANT) = NINT( ADC_CORR(HD_TOWER) 
     &                * PROM_SLOPE(SIGN_ETA,MAGN_ETA,PHI,HD_PROM,PAGE) )
        IF ( ABS( LUQ_BYTE(HD_L2_QUANT) )
     &      .LT. PROM_CUT (SIGN_ETA, MAGN_ETA,PHI, HD_PROM, PAGE) ) 
     &    LUQ_BYTE(HD_L2_QUANT) = 0
        LUQ_BYTE(HD_L2_QUANT) = LUQ_BYTE(HD_L2_QUANT) 
     &                + LOOKUP_ZERESP(SIGN_ETA,MAGN_ETA,PHI,HD_L2_QUANT)
        IF ( LUQ_BYTE(HD_L2_QUANT) .LT. 0 ) 
     &    LUQ_BYTE(HD_L2_QUANT) = 0
        IF ( LUQ_BYTE(HD_L2_QUANT) .GT. SATURATION ) 
     &    LUQ_BYTE(HD_L2_QUANT) = SATURATION 
      END IF
C
C     Same for Px lookup
C
      PAGE = LUQ_PAGE_INDEX ( PX_QUANT,
     &                        LUQ_PAGE_NUMBER(PX_QUANT,L0_BIN) )
      IF (( PAGE .EQ. 0 ) .OR. 
     &    ((DAC_EM .EQ. 0) .AND. (DAC_HD .EQ. 0))) THEN 
        LUQ_BYTE(PX_QUANT) = 0
      ELSE
        LUQ_BYTE(PX_QUANT) = NINT( ADC_CORR(TOT_TOWER)  
     &                * PROM_SLOPE(SIGN_ETA,MAGN_ETA,PHI,PX_PROM,PAGE) )
        IF ( ABS( LUQ_BYTE(PX_QUANT) ) 
     &      .LT. PROM_CUT(SIGN_ETA, MAGN_ETA, PHI, PX_PROM, PAGE) ) 
     &    LUQ_BYTE(PX_QUANT) = 0
        LUQ_BYTE(PX_QUANT) = LUQ_BYTE(PX_QUANT) 
     &                   + LOOKUP_ZERESP(SIGN_ETA,MAGN_ETA,PHI,PX_QUANT)
        IF ( LUQ_BYTE(PX_QUANT) .LT. 0 ) 
     &    LUQ_BYTE(PX_QUANT) = 0
        IF ( LUQ_BYTE(PX_QUANT) .GT. SATURATION ) 
     &    LUQ_BYTE(PX_QUANT) = SATURATION 
      END IF
C
C     Same for Py lookup
C
      PAGE = LUQ_PAGE_INDEX ( PY_QUANT,
     &                        LUQ_PAGE_NUMBER(PY_QUANT,L0_BIN) )
      IF (( PAGE .EQ. 0 ) .OR. 
     &    ((DAC_EM .EQ. 0) .AND. (DAC_HD .EQ. 0))) THEN 
        LUQ_BYTE(PY_QUANT) = 0
      ELSE
        LUQ_BYTE(PY_QUANT) = NINT( ADC_CORR(TOT_TOWER)  
     &                * PROM_SLOPE(SIGN_ETA,MAGN_ETA,PHI,PY_PROM,PAGE) )
        IF ( ABS( LUQ_BYTE(PY_QUANT) ) 
     &      .LT. PROM_CUT(SIGN_ETA, MAGN_ETA, PHI, PY_PROM, PAGE) ) 
     &    LUQ_BYTE(PY_QUANT) = 0
        LUQ_BYTE(PY_QUANT) = LUQ_BYTE(PY_QUANT) 
     &                   + LOOKUP_ZERESP(SIGN_ETA,MAGN_ETA,PHI,PY_QUANT)
        IF ( LUQ_BYTE(PY_QUANT) .LT. 0 ) 
     &    LUQ_BYTE(PY_QUANT) = 0
        IF ( LUQ_BYTE(PY_QUANT) .GT. SATURATION ) 
     &    LUQ_BYTE(PY_QUANT) = SATURATION 
      END IF
C
C----------------------------------------------------------------------
  999 RETURN
      END
