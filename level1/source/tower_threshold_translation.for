      SUBROUTINE   TOWER_THRESHOLD_TRANSLATION ( 
     &                          SIGN_ETA, MAGN_ETA, PHI, 
     &                          THRSH_TYPE,
     &                          DESIRED_THRSH, 
     &                          HARDWARE_THRSH )  
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates the appropriate reference byte to load
C-                         into the CTFE card cluster threshold comparator  
C-                         for the specified Trigger Tower and the specified  
C-                         quantity type in order to achieve the specified
C-                         energy threshold comparison. 
C-                         The threshold translation is inclusive, meaning that
C-                         an energy deposit of the amount specified or greater
C-                         will clear the threshold.  
C-                         In the case of the Hadronic Veto, an hadronic
C-                         energy deposit of the amount specified or greater
C-                         will prevent an EM tower to clear its threshold.
C-
C-   Inputs  : 
C-     THRSH_TYPE          specifies the quantity type for which the threshold
C-                         translation is requested. 
C-                         It is recommended to use one of the compilation
C-                         constants defined along with the common block. 
C-                         See TT_..._THRTYP in common block.
C-     
C-     SIGN_ETA            is the sign of the eta index of the trigger
C-                         tower for which the threshold is requested.
C-                         Use 0 to specify a negative eta index, 1 to
C-                         specify a positive eta index.
C-                         It is recommended to use one of the compilation
C-                         constants defined along with the common block. 
C-                         POS_ETA=0 or NEG_ETA=1.
C-
C-     MAGN_ETA            is the magnitutde of the eta index of the
C-                         trigger tower for which the threshold is 
C-                         requested. 
C-                         Range : 1 to 24
C-
C-     PHI                 is the phi index of the trigger tower for
C-                         which the threshold is requested.
C-                         Range : 1 to 32
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
C-                         No verification is made of the integrity of the
C-                         arguments specifying the indices of the Trigger
C-                         Tower.
C-
C-                         This translation will take into account any 
C-                         offset present at the output of the lookup PROMs.
C-                         
C-                         The Threshold comparator (in the hardware) will
C-                         perform a strict comparison. This means that a
C-                         Trigger Tower must have a quantified transverse
C-                         energy at the output from the Lookup PROMS strictly
C-                         greater than the reference byte programmed for that
C-                         Trigger Tower to contribute to the global count
C-                         carried over the whole detector. 
C-                         
C-                         When the desired threshold exactly falls on an
C-                         allowed quantified value, the byte will be chosen to
C-                         implement an inclusve comparison by returning the
C-                         next smaller byte when necessary.
C-                        
C-   Defined  6-FEB-1990 MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-   Updated  2-AUG-1990 MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-   Updated  22-JUL-1991 MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-   Updated  Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                      - Made IMPLICIT NONE statement recognizable by D0FLAVOR
C-   Updated  26-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Fixed bug in TOT Et threshold calculation. Now uses
C-                      scales for HD_ET_QUANT rather than TOT_ET_QUANT and
C-                      EM_ET_QUANT.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE 
C      
      INCLUDE     'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE     'D0$INC:LEVEL1_LOOKUP.INC'
C
      INTEGER      THRSH_TYPE
      INTEGER      SIGN_ETA, MAGN_ETA, PHI
      REAL         DESIRED_THRSH
      INTEGER      HARDWARE_THRSH 
C
C----------------------------------------------------------------------
C
C
      IF (LSM_SANITY_CHECKS .EQV. .TRUE.) THEN
        IF (((SIGN_ETA .NE. POS_ETA) .AND.
     &       (SIGN_ETA .NE. NEG_ETA))     .OR.
     &      ((MAGN_ETA .LT. ETA_MIN) .OR.
     &       (MAGN_ETA .GT. ETA_MAX))     .OR.
     &      ((PHI .LT. PHI_MIN) .OR.
     &       (PHI .GT. PHI_MAX))          .OR.
     &      ((THRSH_TYPE .LT. TT_EMET_THRTYP) .OR.
     &       (THRSH_TYPE .GT. TT_TOTET_THRTYP))) THEN
          CALL ABORT( 
     &   'ERROR: Parameter out of range in TOWER_THRESHOLD_TRANSLATION')
        ENDIF
      ENDIF
C
      IF ( THRSH_TYPE .EQ. TT_EMET_THRTYP ) THEN
        IF (GLOBAL_ENERGY_SCALE(EM_ET_QUANT) .EQ. 0) THEN
          HARDWARE_THRSH = 0
        ELSE
          HARDWARE_THRSH = NINT ( DESIRED_THRSH 
     &                        / ( GLOBAL_ENERGY_SCALE ( EM_ET_QUANT ) 
     &                        * 2.0 ** LUQ_LOCAL_RESCALING
     &                               (SIGN_ETA,MAGN_ETA,EM_ET_QUANT) ) )
     &                 + LOOKUP_ZERESP(SIGN_ETA,MAGN_ETA,PHI,
     &                                 EM_ET_QUANT)
     &                 - 1
        ENDIF
C
      ELSE IF ( THRSH_TYPE .EQ. TT_HDVETO_THRTYP ) THEN
        IF (GLOBAL_ENERGY_SCALE( HD_ET_QUANT ) .EQ. 0) THEN
          HARDWARE_THRSH = 0
        ELSE
          HARDWARE_THRSH = NINT ( DESIRED_THRSH 
     &                        / ( GLOBAL_ENERGY_SCALE ( HD_ET_QUANT ) 
     &                        * 2.0 ** LUQ_LOCAL_RESCALING
     &                               (SIGN_ETA,MAGN_ETA,HD_ET_QUANT) ) )
     &                 + LOOKUP_ZERESP(SIGN_ETA,MAGN_ETA,PHI,
     &                                 HD_ET_QUANT)
     &                 - 1
        ENDIF
C
      ELSE IF ( THRSH_TYPE .EQ. TT_TOTET_THRTYP  ) THEN
        IF (GLOBAL_ENERGY_SCALE( TOT_ET_QUANT) .EQ. 0) THEN
          HARDWARE_THRSH = 0
        ELSE
          HARDWARE_THRSH = (NINT ( DESIRED_THRSH
     &                      / ( GLOBAL_ENERGY_SCALE ( HD_ET_QUANT ) 
     &                        * 2.0 ** LUQ_LOCAL_RESCALING
     &                               (SIGN_ETA,MAGN_ETA,HD_ET_QUANT) ) )
     &                 + LOOKUP_ZERESP(SIGN_ETA,MAGN_ETA,PHI,
     &                                 EM_ET_QUANT)
     &                 + LOOKUP_ZERESP(SIGN_ETA,MAGN_ETA,PHI,
     &                                 HD_ET_QUANT)  ) / 2
     &                 - 1
        ENDIF
C
      ELSE !ERROR
      ENDIF
C
      IF ( HARDWARE_THRSH .LT.   0 ) HARDWARE_THRSH = 0
      IF ( HARDWARE_THRSH .GT. 255 ) HARDWARE_THRSH = 255
C----------------------------------------------------------------------
  999 RETURN
      END
