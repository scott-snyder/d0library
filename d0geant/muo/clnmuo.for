      LOGICAL FUNCTION CLNMUO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set flags/values that depend on FFREAD cards
C-                         read in parameter input
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created  10-MAR-1988   A.M.Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C----------------------------------------------------------------------
      CLNMUO = .TRUE.
      IF ( DMUO.LE.0 ) GOTO 999
  999 RETURN
      END
