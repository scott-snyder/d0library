      SUBROUTINE STADIS(TIMAST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : AST routine which actually gets called at the
C-                         end of timer interval (for TIMDIS facility).
C-                         Calls user AST and then the restart of the timer.
C-
C-   Inputs  : TIMAST: User AST to call
C-   Outputs : None
C-   Controls: None
C-
C-   Created  22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL TIMAST
C----------------------------------------------------------------------
      CALL TIMAST
      CALL RESTAT(TIMAST)
      RETURN
      END
