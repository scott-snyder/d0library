      SUBROUTINE GUDIGI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Digitisation  for ALL detectors called here
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   6-DEC-1988   A.M.Jonckheere
C-   Updated   6-DEC-1988   A.M.Jonckheere  Added LV0 and made into standard
C-                              form.
C-   Updated   5-JUN-1989   Harrison B. Prosper  
C-   Added hook LUDIGI
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL LUDIGI
  999 RETURN
      END
