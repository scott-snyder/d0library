      LOGICAL FUNCTION ANLLV0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyse results - called after each event
C-                                called by ===> GUOUT
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created   6-DEC-1988   A.M.Jonckheere
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C----------------------------------------------------------------------
      ANLLV0 = .TRUE.
      IF ( DLV0 .LT. 4 ) GOTO 999
  999 RETURN
      END
