      SUBROUTINE VGNL_UPDATE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read gain callibration numbers for real data from
C-                         ASCII files into the VGNL banks
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  25-SEP-1992   Peter M. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL OK
C----------------------------------------------------------------------
C
      CALL VGNL_FROM_ASCII(OK)
C
  999 RETURN
      END
