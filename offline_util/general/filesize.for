      INTEGER FUNCTION FILESIZE(FILENAME)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Get the size in bytes of the file FILENAME
C-     Use the POSIX stat routine interface
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-OCT-1996   John Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FILENAME
C- External functions
      INTEGER LENOCC,POSIX_FILESIZE
C-----------------------------------------------------------------------
      FILESIZE = POSIX_FILESIZE(%REF(FILENAME),%VAL(LENOCC(FILENAME)))
 999  RETURN
      END
