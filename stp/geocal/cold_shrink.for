      SUBROUTINE COLD_SHRINK(X, Y, Z, DL, XF )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TRANSFERS WARM MEASUREMENTS FROM SURVEY
C-                 TO COLD LIQUID ARGON TEMPERATURE COORDINATES
C-
C-   Inputs  :    X, Y, Z COORDINATES OF THE WARM POSITION
C-   Outputs :    X, Y, Z COORDINATES OF THE COLD TRANSFORMED POSITION
C-   Controls: 
C-
C-   Created  11-AUG-1992   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  I, IERR, JERR
      REAL     DL, XF(3), XW(3), XC(3), X, Y, Z
      LOGICAL  FIRST
      SAVE     FIRST
      DATA     FIRST  / .TRUE. /
C
C
      XW(1) = X
      XW(2) = Y
      XW(3) = Z
C
      DO 100 I=1,3
  100 XC(I) = XF(I) + DL*(XW(I)-XF(I))
C
      X = XC(1)
      Y = XC(2)
      Z = XC(3)
C----------------------------------------------------------------------
  999 RETURN
      END
