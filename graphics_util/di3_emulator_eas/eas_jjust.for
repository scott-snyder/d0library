      SUBROUTINE JJUST(HORIZ, VERTIC)
C
C    Purpose:
CD   This subroutine sets the current text justification. The parameters
CD   passed are:
CD      HORIZ --> The horizontal justification value.  This parameter is
CD                defined as follows:
CD                   1 = Left justify.
CD                   2 = Center justify.
CD                   3 = Right justify.
CD      VERTIC --> The vertical justification value.  This parameter is
CD                 defined as follows:
CD                   1 = Bottom justify.
CD                   2 = Center justify.
CD                   3 = Top justify.
C
C    A. Virgo, R. Heckelsberg
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 09-Jul-1988
CH   History:
CH      09-JUL-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB        SEGINF-R, TEXATT-W
C
C    Calls:
CC        ERROR
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER HORIZ, VERTIC
C
C    Then local declarations of variables (non-common variables).
C

C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:TEXATT.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         IF (HORIZ .LT. 1 .OR. HORIZ .GT. 3) THEN
            CALL ERROR('HORIZONTAL JUST. OUT OF RANGE')
         ELSE
            CHJUST = HORIZ
            IF (VERTIC .LT. 1 .OR. VERTIC .GT. 3) THEN
               CALL ERROR('VERTICAL JUST. OUT OF RANGE')
            ENDIF
            CVJUST = VERTIC
         ENDIF
      ELSE
         CALL ERROR('NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END
