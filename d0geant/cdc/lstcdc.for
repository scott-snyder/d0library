      LOGICAL FUNCTION LSTCDC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Central Drift Detector rap up
C-
C-   Inputs  : NONE
C-   Outputs : NONE as of now
C-   Calledby: LULAST
C-
C-   Created   9-JUL-1987   A.M.Jonckheere
C-
C-   Updated   9-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      LSTCDC = .TRUE.
      IF ( DCDC .LT. 2 ) GOTO 999
C
  999 RETURN
      END
