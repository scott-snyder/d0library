      SUBROUTINE CLOFIL ( LUNIN )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Close the file LUNIN
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created  23-MAY-1988   Ghita Rahal-Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUNIN
C----------------------------------------------------------------------
C      INCLUDE 'D0$MCH:CLOFI1.INC'
C **** CLOFI1.INC - VAX780 version
      CLOSE ( LUNIN )
  999 RETURN
      END
