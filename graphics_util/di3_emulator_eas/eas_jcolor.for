        SUBROUTINE JCOLOR(ICOLOR)
C
C    Purpose:
CD   This module selects a color to be used as the current color with
CD   all drawing primitives.
C
C    A. Virgo, R. Heckelsberg
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 12-Jul-1988
CH   History:
CH      12-AUG-88  ATV  Corrected erroneous error message.
CH      06-JUL-88  ATV  Added the modulo 40 to the color specification.
CH      29-JUN-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB        SEGINF-R, LINATT-W
C
C    Calls:
CC        ERROR, KUPDV
      EXTERNAL ERRHND
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER ICOLOR 
C
C    Then local declarations of variables (non-common variables).
C
 
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         CALL KUPDV
         IF (ICOLOR .LT. 0 .OR. ICOLOR .GT. 32767) THEN
            CALL ERROR('COLOR CVALUE IS NOT IN RANGE')
         ENDIF
         CURCOL = MOD(ICOLOR,40)
      ELSE
         CALL ERROR('NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END
