      SUBROUTINE MU_TK_BLOWUP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Head routine that displays the Muon Trak Blow up
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  26-SEP-1990   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IVIEW
C----------------------------------------------------------------------
      CALL PMBLOW(IVIEW)
  999 RETURN
      END
