      LOGICAL FUNCTION CLNVTX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Edit D0GEANT run-time switches for VTX package.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  14-JUL-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      CLNVTX = .TRUE.
C
      DVTX = MIN(DVTX,DCEN)
      IF ( DVTX .LE. 0 ) GOTO 999
      PVTX = MIN(PVTX,PCEN)
C
  999 RETURN
      END
