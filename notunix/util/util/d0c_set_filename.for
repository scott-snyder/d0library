      SUBROUTINE D0C_SET_FILENAME (FILENAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store current filename in a local variable.
C-
C-   Use D0C_GET_FILENAME to retrieve it.
C-
C-   Inputs  : FILENAME [C*]
C-   Outputs : None
C-   Controls: None
C-
C-   Created  11-AUG-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*(*) FILENAME
      CHARACTER*132 FILE_NAME,FILE
C----------------------------------------------------------------------
      FILE_NAME = FILENAME
      RETURN
      ENTRY D0C_GET_FILENAME (FILE)
      FILE = FILE_NAME
  999 RETURN
      END
