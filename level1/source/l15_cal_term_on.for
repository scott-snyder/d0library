      LOGICAL FUNCTION L15_CAL_TERM_ON(ITERM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns true if ITERM is on for this event
C-                         (i.e. if there is a L15 trigger that needs
C-                         this term evaluated), false otherwise.
C-
C-   Returned value  : 
C-   Inputs  :        ITERM  -- INTEGER L15CAL term number
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-MAR-1994   sFahey
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
      INCLUDE 'D0$INC:L15_FRAMEWORK.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
C
      INTEGER ITERM,TRIGGER
C----------------------------------------------------------------------
      L15_CAL_TERM_ON = .FALSE.
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
C
        IF (FIRED_TRIGGER(TRIGGER)) THEN
C
          IF (ST_IS_L15(TRIGGER)) THEN
            IF (L15_TERM_ALLOCATE(ITERM,TRIGGER)) THEN
              L15_CAL_TERM_ON = .TRUE.      
            ENDIF                           
          ENDIF        
        ENDIF   
      ENDDO 
C
  999 RETURN
      END
