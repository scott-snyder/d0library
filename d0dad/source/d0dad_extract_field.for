      SUBROUTINE D0DAD_EXTRACT_FIELD(KEY,INSTRING,FIELD)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Extract a field started by keyword KEY from 
C-     the comma separated list of fields in INSTRING and copy it into
C-     FIELD.   If the keyword is not found, FIELD is left unchanged
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-OCT-1996   John Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) KEY,INSTRING,FIELD
C- External functions
      INTEGER ICFIND,LENOCC,ICLOCU
C- Temporary local storage
      INTEGER J,K,KLEN
C-----------------------------------------------------------------------
C- Look for the key word KEY in the input string.

      KLEN = LENOCC(KEY)
      J = ICLOCU(KEY,KLEN,INSTRING,1,LENOCC(INSTRING))

C- If it's found, copy it up to comma or end-of-line

      IF( J.NE.0 ) THEN
        K = ICFIND(INSTRING,',',J,LENOCC(INSTRING))
        J = J + LENOCC(KEY)
        IF( LEN(FIELD).GE.(K-J+1) ) FIELD=INSTRING(J:K)
      ENDIF
C
  999 RETURN
      END
