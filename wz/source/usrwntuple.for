      SUBROUTINE USRWNTUPLE(PNT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Hook provided for users to fill the spares PNT(181)
C-   to PNT(210) Called from routine ANLWMUNU
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  15-OCT-1992   Cecilia E. Gerber
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NTDIM
      PARAMETER(NTDIM=210)
      REAL PNT(NTDIM)
C----------------------------------------------------------------------
  999 RETURN
      END
