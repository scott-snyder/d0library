      SUBROUTINE STRFET(COOK, STR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Fetch the string identified by COOK and copy it to STR.  COOK
C-    should be a cookie returned by STRSTO or STRAPP.
C-
C-    This version stores strings in dymanic memory allocated by
C-    LIB$GET_VM.
C-
C-   Inputs  :
C-    COOK : A cookie which identifies the string to be fetched.
C-   Outputs :
C-    STR : Where the string should be copied.
C-
C-   Created  16-MAY-1991   Scott Snyder
C-   Updated  30-SEP-1991   Herbert Greenlee
C-   Modified 14-AUG-1992   sss - fix unix version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER COOK
      CHARACTER*(*) STR
C&IF VAXVMS
C&ELSE
C&      INTEGER DSC(2)
C&ENDIF
C----------------------------------------------------------------------
      IF (COOK .EQ. 0) THEN
        STR = ' '
      ELSE
C&IF VAXVMS
        CALL STRCOP(%VAL(COOK), STR)
C&ELSE
C&        call strdcp(%val(cook), dsc)
C&        call strcop(%val(dsc(2)),str,%val(dsc(1)),%val(len(str)))
C&ENDIF
      ENDIF
  999 RETURN
      END


