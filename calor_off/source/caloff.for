      PROGRAM CALOFF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main program for calorimeter offline code
C-                         development.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-JAN-1989   Harrison B. Prosper, John Womersley
C-
C----------------------------------------------------------------------
      IMPLICIT NONE 
C----------------------------------------------------------------------
C
      CALL CBEGIN       ! General initialization
      CALL CRUNCH       ! Do processing
      CALL CEND         ! General summary
C
      END
