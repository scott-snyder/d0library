      LOGICAL FUNCTION ANLFDC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyse results - called after each event
C-
C-   Inputs  : None
C-   Outputs : None
C-   Calledby: LUOUT
C-
C-   Created  24-JUL-1987   A.M.Jonckheere
C-
C-   Updated   9-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C----------------------------------------------------------------------
      ANLFDC = .TRUE.
      IF ( DFDC .LT. 4 ) GOTO 999
C
  999 RETURN
      END
