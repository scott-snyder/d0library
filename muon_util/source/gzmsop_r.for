      INTEGER FUNCTION GZMSOP_R(MOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DUMMY
C-
C-   Returned value  : Zebra address of MSOP bank for module MOD
C-   Inputs  : MOD                      ! module number
C-
C-   Created  19-MAY-1989   J.Green
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MOD
C----------------------------------------------------------------------
      CALL INTMSG(' Call made to dummy GZMSOP_R ')
      GZMSOP_R = 0
  999 RETURN
      END
