      PROGRAM CRYDRW
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simple program to draw cryostat.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  21-MAR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL MZEBRA (0)
      CALL INZSTP
      CALL CRYPLT (80)
      END
