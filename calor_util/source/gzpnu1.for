      INTEGER FUNCTION GZPNU1
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Find pointer to PNU1 
C-
C-   Created  14-SEP-1995 Dhiman Chakraborty
C-   Updated  17-NOV-1995 Dhiman Chakraborty   
C-                        bug-fix to prevent returning uninitialized 
C-                        value if PNUT2 doesn't exist
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPNU1.LINK'
      INTEGER GZPNUT,LPNUT
      EXTERNAL GZPNUT
C----------------------------------------------------------------------
      GZPNU1 = 0
      LPNUT = GZPNUT(2)
      IF(LPNUT.GT.0) GZPNU1 = LQ(LPNUT-IZPNU1)
C
  999 RETURN
      END
