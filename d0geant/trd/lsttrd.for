      LOGICAL FUNCTION LSTTRD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Transition Radiation Detector rap up
C-
C-   Inputs  : NONE
C-   Outputs : NONE as of now
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
      LSTTRD = .TRUE.
      IF ( DTRD .LT. 2 ) GOTO 999
C
  999 RETURN
      END
