      SUBROUTINE VGNCHK(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check the gain values produced by VTXCOFF processing
C-                         and decide whether they are fit to be DBL3ized.
C-
C-   Inputs  : none
C-   Outputs : OK if gains should go to the database
C-   Controls: 
C-
C-   Created   8-OCT-1992   Peter M. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL OK
C----------------------------------------------------------------------
      OK = .TRUE.
  999 RETURN
      END
