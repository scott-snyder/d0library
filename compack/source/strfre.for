      SUBROUTINE STRFRE(COOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Free the string identified by the cookie COOK.  The cookie should
C-    have been returned from STRSTO or STRAPP.
C-
C-    This version stores strings in dynamic memory allocated by
C-    LIB$GET_VM.
C-
C-   Inputs  :
C-    COOK : The cookie which identifies the string to freed.
C-
C-   Created  16-MAY-1991   Scott Snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER COOK
C&IF VAXVMS
      INCLUDE '($DSCDEF)'
      RECORD /DSCDEF1/ DSC
      INTEGER ISTAT
      INTEGER  LIB$FREE_VM
      EXTERNAL LIB$FREE_VM
C----------------------------------------------------------------------
      IF (COOK .NE. 0) THEN
        CALL STRDCP(%VAL(COOK), DSC)
        ISTAT = LIB$FREE_VM(DSC.DSC$W_MAXSTRLEN + DSC$K_S_BLN, COOK)
        IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      ENDIF
C&ENDIF
  999 RETURN
      END
