      SUBROUTINE D0DAD_EXTRACT_IFIELD(KEY,INSTRING,IVAL)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Extract an integer value from INSTRING 
C-     corresponding to the value flagged by KEY
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-OCT-1996   John Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) KEY,INSTRING
      INTEGER IVAL
      CHARACTER*128 CSTRING
C-----------------------------------------------------------------------
C
      CSTRING=' '
      CALL D0DAD_EXTRACT_FIELD(KEY,INSTRING,CSTRING)
      IF( CSTRING.NE.' ' ) READ(CSTRING,*) IVAL
C
  999 RETURN
      END
