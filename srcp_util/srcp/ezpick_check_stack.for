      SUBROUTINE EZPICK_CHECK_STACK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : spew the stack if it has any banks still open
C-      
C-   Inputs  : none
C-   Outputs : possibly an ERRMSG, and a dump in SSUNIT
C-   Controls: 
C-
C-   Created   8-DEC-1994   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUNIT,SSUNIT,LEVEL
C----------------------------------------------------------------------
C
      LEVEL = 3
      CALL EZPICK_ERRORS(LEVEL) !scream if stack under or overflows
      LUNIT = SSUNIT()      
      IF (LUNIT.EQ.0) LUNIT = 3 !sent to zebra log if summary not open yet
      CALL EZPICK_DUMP_STACK(LUNIT)
  999 RETURN
      END
