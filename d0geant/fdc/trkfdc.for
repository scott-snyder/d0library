      LOGICAL FUNCTION TRKFDC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FDC - Handle any procedures needed after a
C-   full track is finished - DUMMY for now.
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   Created  13-JUL-1987   A.M.Jonckheere
C-   Updated  17-JUL-1989   Harrison B. Prosper  
C-   Made into pbd interface function. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      TRKFDC = .TRUE.
      IF ( DFDC .LT. 2 ) GOTO 999
C
  999 RETURN
      END
