      SUBROUTINE MOVINT(SRC, DST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Copy an integer from SRC to DST.
C-
C-   Inputs  : SRC
C-   Outputs : DST
C-   Controls: None
C-
C-   Created   2-OCT-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER SRC, DST
C----------------------------------------------------------------------
      DST = SRC
      RETURN
      END
