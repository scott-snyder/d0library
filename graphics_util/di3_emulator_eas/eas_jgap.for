      SUBROUTINE JGAP(GAPSIZ)
C
C    Purpose:
CD   This routines sets the intercharacter text spacing value. The
CD   parameter GAPSIZ is the gap value based on an integral of the
CD   character size. Valid values are from -1.0 to +infinity.
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
CB      SEGINF-R, TEXATT-W
C
C    Calls:
CC      ERROR
C
C    Next is the declaration of parameters passed to the subroutine/function.
      REAL GAPSIZ
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
         IF (GAPSIZ .LT. -1.0) THEN
            CALL ERROR('THE INTERCHARACTER SPACING IS < -1.0')
         ELSE
            CGAP = GAPSIZ
         ENDIF
      ELSE
         CALL ERROR('NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END
