      FUNCTION GZHDIR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return address of current sub-directory.
C-
C-   Returned value  : Address
C-
C-   Created  11-JAN-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      INTEGER GZHDIR
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:HCBOOK.INC'
C----------------------------------------------------------------------
      GZHDIR = LCDIR
 999  RETURN
      END
