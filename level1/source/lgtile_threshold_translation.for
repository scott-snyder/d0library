      SUBROUTINE   LGTILE_THRESHOLD_TRANSLATION ( 
     &                          SIGN_ETA, MAGN_ETA, PHI, 
     &                          DESIRED_THRSH, 
     &                          HARDWARE_THRSH )  
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates the appropriate reference value to load
C-                         into the Tier #1 CAT2 card (Px or Py card used for
C-                         Large Tiles) for the specified Large Tile in order to
C-                         achieve the specified energy threshold comparison. 
C-                         The threshold translation is inclusive, meaning that
C-                         an energy deposit of the amount specified or greater
C-                         will clear the threshold.  
C-   Inputs  : 
C-     
C-     SIGN_ETA            is the sign of the eta index of the Large Tile 
C-                         for which the threshold is requested.
C-                         Use 0 to specify a negative eta index, 1 to
C-                         specify a positive eta index.
C-                         It is recommended to use one of the compilation
C-                         constants defined along with the common block. 
C-                         POS_ETA=0 or NEG_ETA=1.
C-
C-     MAGN_ETA            is the magnitutde of the eta index of the
C-                         Large Tile for which the threshold is 
C-                         requested. 
C-                         The Large Tile is identified by the lowest Magnitude
C-                         Eta of the Trigger Towers it covers
C-                         Allowed: 1, 5, 9, 13, or 17
C-
C-     PHI                 is the phi index of the Large Tile for
C-                         which the threshold is requested.
C-                         The Large Tile is identified by the lowest Phi 
C-                         of the Trigger Towers it covers
C-                         Allowed: 1, 9, 17, or 25
C-
C-     DESIRED_THRSH       is the desired energy threshold to be applied to the
C-                         specified tower.
C-                         Units: GeV.
C-     
C-   Outputs : 
C-     HARDWARE_THRSH      is the byte of quantified threshold to be loaded in
C-                         the CTFE cluster threshold comparator of the
C-                         specified Trigger Tower to achieve the specified
C-                         threshold comparison. 
C-
C-   Controls: None
C-
C-   Comments :
C-                         A call to INIT_LOOKUP_ROUTINES must have been made
C-                         before this routine can be called.
C-
C-                         This translation will take into account any 
C-                         offset present at the output of the lookup PROMs.
C-                         
C-                         The threshold comparator in the hardware of the CAT
C-                         card will perform an inclusive comparison. This
C-                         means that a quantified global energy sum must be
C-                         greater than or equal to the reference number
C-                         programmed for that quantity to be considered as
C-                         passing the threshold. 
C-                         
C-   Created 30-JUN-1993 Philippe Laurens
C-                      Derived from TOWER_THRESHOLD_TRANSLATION.FOR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE 
C      
      INCLUDE     'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE     'D0$INC:LEVEL1_LOOKUP.INC'
C
      INTEGER      SIGN_ETA, MAGN_ETA, PHI
      REAL         DESIRED_THRSH
      INTEGER      HARDWARE_THRSH 
C
      INTEGER      LT_LOOKUP_ZERESP, TT_MAGN_ETA, TT_PHI
C
C----------------------------------------------------------------------
C
C
      IF (LSM_SANITY_CHECKS .EQV. .TRUE.) THEN
        IF (    (     (SIGN_ETA .NE. POS_ETA) 
     &          .AND. (SIGN_ETA .NE. NEG_ETA) )     
     &     .OR. (     (MAGN_ETA .LT. ETA_MIN) 
     &          .OR.  (MAGN_ETA .GT. ETA_MAX) )     
     &     .OR. (     (MAGN_ETA - 1) .NE. 
     &                (TT_ETA_PER_LT * ((MAGN_ETA-1) / TT_ETA_PER_LT)) )
     &     .OR. (     (PHI .LT. PHI_MIN) 
     &          .OR.  (PHI .GT. PHI_MAX) ) 
     &     .OR. (     (PHI - 1) .NE. 
     &                (TT_PHI_PER_LT * ((PHI-1) / TT_PHI_PER_LT)) ))THEN
          CALL ABORT( 
     &  'ERROR: Parameter out of range in LGITLE_THRESHOLD_TRANSLATION')
        ENDIF
      ENDIF
C
      LT_LOOKUP_ZERESP = 0
      DO TT_MAGN_ETA = MAGN_ETA , MAGN_ETA + TT_ETA_PER_LT - 1
        DO TT_PHI = PHI , PHI + TT_PHI_PER_LT - 1
          LT_LOOKUP_ZERESP = LT_LOOKUP_ZERESP
     &                     + ( ADC_ZERESP( SIGN_ETA,
     &                                     TT_MAGN_ETA,
     &                                     TT_PHI, 
     &                                     EM_TOWER )
     &                       + ADC_ZERESP( SIGN_ETA,
     &                                     TT_MAGN_ETA,
     &                                     TT_PHI,
     &                                     HD_TOWER)  ) / 2
        ENDDO
      ENDDO
C
      IF ( GLOBAL_ENERGY_SCALE( PX_QUANT ) .EQ. 0) THEN
        HARDWARE_THRSH = 0
      ELSE
        HARDWARE_THRSH = NINT ( DESIRED_THRSH
     &                      / ( GLOBAL_ENERGY_SCALE ( PX_QUANT ) 
     &                        * 2.0 ** LUQ_LOCAL_RESCALING
     &                               (SIGN_ETA,MAGN_ETA,PX_QUANT) ) ) 
     &                 + LT_LOOKUP_ZERESP
      ENDIF
      IF ( HARDWARE_THRSH .GT. 4*8*255 ) HARDWARE_THRSH = 4*8*255 + 1
C
C----------------------------------------------------------------------
  999 RETURN
      END
