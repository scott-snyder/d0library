      LOGICAL FUNCTION LSTVTX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Vertex Detector rap up.
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Calledby: LULAST
C-   
C-   Created   9-JUL-1987   A.M.Jonckheere
C-   Updated  17-JUL-1989   Harrison B. Prosper  
C-   Made into pbd interface function 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      LSTVTX = .TRUE.
      IF ( DVTX .LT. 2 ) GOTO 999
C
  999 RETURN
      END
