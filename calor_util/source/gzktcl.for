      INTEGER FUNCTION GZKTCL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :   Find pointer to KTCL 
C-
C-   Created  4-OCT-1995 Dhiman Chakraborty
C-   Updated  6-NOV-1995 Dhiman Chakraborty   Get rid of an ERRMSG
C-   Updated 30-JAN-1996 Dhiman Chakraborty   Assign value to GZKTCL before
C-                                            "IF(...) RETURN" to pacify the 
C-                                            ALPHA compiler
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZKTCL.LINK'
      INTEGER  GZANLS,LANLS,LKTCL
      EXTERNAL GZANLS
C----------------------------------------------------------------------
      GZKTCL = 0
      LKTCL = 0
      LANLS = GZANLS()
      IF(LANLS.LE.0) RETURN
      LKTCL = LQ(LANLS-IZKTCL)
      GZKTCL = LKTCL
C
  999 RETURN
      END
