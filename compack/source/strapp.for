      FUNCTION STRAPP(COOK, STR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Append string STR to the end of the string identified by COOK, and
C-    return a cookie for the new string.  The old cookie is freed.
C-    COOK should be a cookie returned by STRSTO or STRAPP.
C-
C-    This version stores strings in dynamic memory allocated by
C-    LIB$GET_VM.
C-
C-   Returned value  :
C-    A cookie for the string which is the concatenation of COOK and
C-    STR.
C-
C-   Inputs  :
C-    COOK : A cookie which identifies the first string to be
C-           concatenated.
C-    STR  : The second string to be concatenated.
C-
C-   Created  16-MAY-1991   Scott Snyder
C-   Updated  12-DEC-1991   Herbert Greenlee
C-     UNIX compatible version
C-   Modified 14-AUG-1992   sss - fix unix version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER STRAPP
      INTEGER*4 COOK
      CHARACTER*(*) STR
C&IF VAXVMS
C&ELSE
C&      integer dsc(2)
C&ENDIF
C
      INTEGER*4 STRAP1, STRSTO
      EXTERNAL  STRAP1, STRSTO
C----------------------------------------------------------------------
      IF (COOK .EQ. 0) THEN
        STRAPP = STRSTO(STR)
      ELSE
C&IF VAXVMS
        STRAPP = STRAP1(%VAL(COOK), STR)
C&ELSE
C&        call strdcp (%val(cook), dsc)
C&        strapp = strap1 (%val(dsc(2)), %ref(str),
C&     &                   %val(dsc(1)), %val(len (str)))
C&ENDIF
        CALL STRFRE(COOK)
      ENDIF
  999 RETURN
      END
