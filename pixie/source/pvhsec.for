      SUBROUTINE PVHSEC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Head routine that calls PVTSEC to display the
C-   chamber between the sectors 0 and 31.
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  26-APR-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL PVTSEC(0,31)
  999 RETURN
      END
