      INTEGER FUNCTION GZMSAU_N(MOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DUMMY
C-
C-   Returned value  : Zebra address of MSAU bank for module MOD
C-   Inputs  : MOD                      ! module number
C-
C-   Created  19-MAY-1989   J.Green
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MOD
C----------------------------------------------------------------------
      CALL INTMSG(' Call made to dummy GZMSAU_N ')
      GZMSAU_N = 0
  999 RETURN
      END
