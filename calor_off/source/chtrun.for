      FUNCTION CHTRUN ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reset run dependent parameter and switches 
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: CAHITS_RCP
C-
C-   Created   3-FEB-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CHTRUN
      INTEGER IER,GZCAD1
C----------------------------------------------------------------------
      CHTRUN =.TRUE.
      CALL CAEPFL_RESET
  999 RETURN
      END
