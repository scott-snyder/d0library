      SUBROUTINE JDJUST(HORIZ, VERTIC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets the default text justification.
C-
C-   Inputs  :
C      HORIZ --> The default horizontal justification value.  This parameter is
C                defined as follows:
C                   1 = Left justify.
C                   2 = Center justify.
C                   3 = Right justify.
C      VERTIC --> The default vertical justification value.  This parameter is
C                 defined as follows:
C                   1 = Bottom justify.
C                   2 = Center justify.
C                   3 = Top justify.
C-   Outputs :
C-   Controls:
C-
C-   Created   8-JUL-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER HORIZ, VERTIC
      INCLUDE 'D0$INC:TEXATT.INC'
C
      IF (HORIZ .LT. 1 .OR. HORIZ .GT. 3) THEN
        CALL ERROR('DEFAULT HORIZONTAL JUST. OUT OF RANGE')
      ELSE
        DHJUST = HORIZ
      ENDIF
      IF (VERTIC .LT. 1 .OR. VERTIC .GT. 3) THEN
        CALL ERROR('DEFAULT VERTICAL JUST. OUT OF RANGE')
      ELSE
        DVJUST = VERTIC
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
