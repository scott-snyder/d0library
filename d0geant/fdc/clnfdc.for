      LOGICAL FUNCTION CLNFDC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-   
C-   Edit D0GEANT run-time switches for the FDC package.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   9-JUN-1989   Harrison B. Prosper
C-   Updated  14-JUL-1989   Harrison B. Prosper  
C-   Added code from CLNCEN 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      CLNFDC = .TRUE.
C
      DFDC = MIN(DFDC,DCEN)
      IF ( DFDC .LE. 0 ) GOTO 999
      PFDC = MIN(PFDC,PCEN)
C
  999 RETURN
      END
