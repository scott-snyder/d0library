      LOGICAL FUNCTION ANLVTX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyse results - called after each event.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Calledby: LUOUT
C-
C-   Created  24-JUL-1987   A.M.Jonckheere
C-   Updated  14-JUL-1989   Harrison B. Prosper  
C-   Made into pbd logical function 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C----------------------------------------------------------------------
      ANLVTX = .TRUE.
      IF ( DVTX .LT. 4 ) GOTO 999
C
  999 RETURN
      END
