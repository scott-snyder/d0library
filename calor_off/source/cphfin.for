      FUNCTION CPHFIN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FINISH UP CPHEL
C-
C-   Returned value  : TRUE if all OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-APR-1989   Rajendran Raja
C-   Updated  14-SEP-1990   Harrison B. Prosper  
C-      Remove printout of histograms; done by frame 
C-   Updated  24-AUG-1992   Rajendran Raja  removed summing up old hmatrix 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      LOGICAL CPHFIN,HMREAD
C----------------------------------------------------------------------
      CPHFIN = .TRUE.
C
  999 RETURN
      END
