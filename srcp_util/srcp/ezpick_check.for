      FUNCTION EZPICK_CHECK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Package interface to EZPICK_CHECK_STACK
C-      to check whether EZPICK stack is cleared correctly
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-DEC-1994   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL EZPICK_CHECK
C----------------------------------------------------------------------
      CALL EZPICK_CHECK_STACK
      EZPICK_CHECK = .TRUE. !always returns true
  999 RETURN
      END
