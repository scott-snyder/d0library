      FUNCTION L2_VERT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine for returning vertex used in level 2
C-       dummy version, the working version is in LEVEL2 library
C-
C-   Returned value: 0.0
C-
C-   Created  25-JAN-1993 Serban Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    L2_VERT
C----------------------------------------------------------------------
      L2_VERT = 0.0
      CALL ERRMSG('Dummy version of L2_VERT called','L2_VERT',' ','W')
  999 RETURN
      END
