      LOGICAL FUNCTION CLNTRD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Edit D0GEANT run-time switches for TRD package.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   9-JUN-1989   Harrison B. Prosper
C-   Updated  14-JUL-1989   Harrison B. Prosper   
C-   Added code from CLNCEN
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      CLNTRD = .TRUE.
C
      DTRD = MIN(DTRD,DCEN)
      IF ( DTRD .LE. 0 ) GOTO 999
      PTRD = MIN(PTRD,PCEN)
C
  999 RETURN
      END
