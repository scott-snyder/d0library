      INTEGER FUNCTION PRUNIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    provide a unit number for printout
C-
C-   Created  22-JUL-1987   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER UNIT,ERR
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST) THEN
        CALL GTUNIT(1,UNIT,ERR)
        FIRST=.FALSE.
      ENDIF
      PRUNIT=UNIT              
C----------------------------------------------------------------------
  999 RETURN
      END
