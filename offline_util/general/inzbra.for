      SUBROUTINE INZBRA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize ZEBRA once only, by calling
C-                         MZEBRA(0).
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  13-SEP-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZEBRA (0)
      ENDIF
  999 RETURN
      END
