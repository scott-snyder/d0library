      SUBROUTINE ENPROG (PROGID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return program identifier. Use this number
C-                         with GTUNIT.
C-
C-   Inputs  : None
C-   Outputs : PROGID      Program identifier
C-   Controls: None
C-
C-   Created   7-MAR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PROGID
C----------------------------------------------------------------------
      CALL EZPICK ('SCPH')
      CALL EZGSET ('IDENTIFIER',PROGID,1)
      CALL EZRSET
  999 RETURN
      END
