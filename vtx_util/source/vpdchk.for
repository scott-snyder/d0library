      SUBROUTINE VPDCHK(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check to see if the VTX pedestal values in the VPDL
C-                         banks are fit to be DBL3ized.
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
