      SUBROUTINE   GLOBAL_THRESHOLD_TRANSLATION ( 
     &                           THRSH_TYPE, 
     &                           DESIRED_THRSH, 
     &                           HARDWARE_THRSH )  
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Translates the specified global energy threshold to 
C-                         the appropriate reference number to load into a
C-                         comparator of one of the last summing CAT card of
C-                         the specified Energy tree. 
C-                         The threshold translation is inclusive, meaning that
C-                         an energy deposit of the amount specified or greater
C-                         will clear the threshold.  
C-
C-   Inputs  : 
C-     THRSH_TYPE          specifies the quantity type for which the threshold
C-                         translation is requested. 
C-                         It is recommended to use one of the compilation
C-                         constants defined along with the common block. 
C-                         See GL_..._THRTYP in common block.
C-
C-     DESIRED_THRSH       is the desired energy threshold to be applied to the
C-                         specified tower.
C-                         Units: GeV.
C-     
C-   Outputs : 
C-     HARDWARE_THRSH      is the byte of quantified threshold to be loaded in
C-                         the appropriate Tier #3 (or final stage) CAT Card 
C-                         to achieve the specified threshold comparison. 
C-
C-   Controls: None
C-
C-   Comments :
C-                         A call to INIT_LOOKUP_ROUTINES must have been made
C-                         before this routine can be called.
C-
C-                         This translation will take into account the global
C-                         sum of any offset present at the output of the 
C-                         lookup PROMs for the specified quantity.
C-                         
C-                         The threshold comparator in the hardware of the CAT
C-                         card will perform an inclusive comparison. This
C-                         means that a quantified global energy sum must be
C-                         greater than or equal to the reference number
C-                         programmed for that quantity to be considered as
C-                         passing the threshold. 
C-                         
C-   Defined  6-FEB-1990 MICHIGAN STATE UNIVERSTITY, TRIGGER CONTROL SOFTWARE
C-   Updated  2-AUG-1990 MICHIGAN STATE UNIVERSTITY, TRIGGER CONTROL SOFTWARE
C-   Updated  31-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                     - Made IMPLICIT NONE statement recognizable by D0FLAVOR
C----------------------------------------------------------------------
      IMPLICIT NONE 
C      
      INCLUDE     'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE     'D0$INC:LEVEL1_LOOKUP.INC'
C
      INTEGER      THRSH_TYPE
      REAL         DESIRED_THRSH 
      INTEGER      LOOKUP_TYPE
      INTEGER      HARDWARE_THRSH 
      INTEGER      THRTYPE_TO_LOOKUP(GL_EMET_THRTYP:GL_TOTL2_THRTYP)
      LOGICAL      FIRST
      SAVE         THRTYPE_TO_LOOKUP,FIRST
      DATA         FIRST / .TRUE. /
C
C----------------------------------------------------------------------
C
      IF (LSM_SANITY_CHECKS .EQV. .TRUE.) THEN
        IF ((THRSH_TYPE .LT. EM_ET_QUANT) .OR. 
     &    (THRSH_TYPE .GT. GL_TOTL2_THRTYP) .OR. 
     &    ((THRSH_TYPE .LT. GL_EMET_THRTYP) .AND. 
     &     (THRSH_TYPE .GT. TOT_L2_QUANT))) THEN
          CALL ABORT(
     &'ERROR: Invalid threshold type in GLOBAL_THRESHOLD_TRANSLATION')
        ENDIF
      ENDIF
C        
      IF (FIRST .EQV. .TRUE.) THEN
        THRTYPE_TO_LOOKUP(GL_EMET_THRTYP) = EM_ET_QUANT
        THRTYPE_TO_LOOKUP(GL_EML2_THRTYP) = EM_L2_QUANT
        THRTYPE_TO_LOOKUP(GL_HDET_THRTYP) = HD_ET_QUANT
        THRTYPE_TO_LOOKUP(GL_HDL2_THRTYP) = HD_L2_QUANT
        THRTYPE_TO_LOOKUP(GL_TOTET_THRTYP) = TOT_ET_QUANT
        THRTYPE_TO_LOOKUP(GL_TOTL2_THRTYP) = TOT_L2_QUANT
        FIRST = .FALSE.
      ENDIF
C
      IF (THRSH_TYPE .LT. GL_EMET_THRTYP) THEN
        LOOKUP_TYPE = THRSH_TYPE
      ELSE
        LOOKUP_TYPE = THRTYPE_TO_LOOKUP(THRSH_TYPE)
      ENDIF
C        
      IF (GLOBAL_ENERGY_SCALE(LOOKUP_TYPE) .EQ. 0) THEN
        HARDWARE_THRSH = 0
      ELSE
        HARDWARE_THRSH = NINT ( DESIRED_THRSH 
     &                      / GLOBAL_ENERGY_SCALE(LOOKUP_TYPE) )
     &                      + TREE_OFFSET(LOOKUP_TYPE)
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
