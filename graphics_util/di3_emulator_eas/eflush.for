      SUBROUTINE EFLUSH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used in escape function to flush buffer to screen
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  13-MAY-1992   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL ERRHND
C
      CALL PPURGE(ERRHND)
C
C----------------------------------------------------------------------
  999 RETURN
      END
