      LOGICAL FUNCTION TEVLV0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called before each event by ====> GUTREV
C-
C-   Inputs  : Logical flags
C-   Outputs : None
C-
C-   Created  6-DEC-1988   A.M.Jonckheere
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C----------------------------------------------------------------------
      TEVLV0 = .TRUE.
      IF ( DLV0 .LT. 2 ) GOTO 999
  999 RETURN
      END
