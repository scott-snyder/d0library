      LOGICAL FUNCTION CLNLV0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set flags/values that depend on FFREAD cards
C-                         read in parameter input
C-
C-   Inputs  : DLV0
C-   Outputs :
C-
C-   Created  6-DEC-1988   A.M.Jonckheere
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C----------------------------------------------------------------------
      CLNLV0 = .TRUE.
      IF ( DLV0 .LE. 0 ) GOTO 999
  999 RETURN
      END
