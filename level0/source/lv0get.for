      SUBROUTINE LV0GET(PRTLVL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Utility to set a printout level based on 
C-                         error encountered
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-SEP-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRINT_LEVEL, PRTLVL
      LOGICAL LV0OUT, PRINT_EVENT, PRTEVT
      SAVE PRINT_EVENT
C----------------------------------------------------------------------
      PRTLVL = PRINT_LEVEL
      RETURN
C----------------------------------------------------------------------
      ENTRY LV0SET(PRTLVL)
      PRINT_EVENT = .TRUE.
      IF ( PRTLVL.GT.PRINT_LEVEL ) PRINT_LEVEL = PRTLVL
      RETURN
C----------------------------------------------------------------------
      ENTRY LV0RST()
      PRINT_EVENT = .FALSE.
      PRINT_LEVEL = 0
C----------------------------------------------------------------------
  999 RETURN
      END
