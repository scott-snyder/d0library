      SUBROUTINE JDSIZE(XSIZE, YSIZE)
C
C    Purpose:
CD   This routine sets the default size of the character box for text
CD   output primitives.  The arguments are XSIZE and YSIZE and the
CD   value represents a character size in world coordinates.
C
C    A. Virgo, R. Heckelsberg
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 09-JUL-1988
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
      REAL XSIZE, YSIZE
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
      IF (.NOT. SEGSOP) THEN
         IF (XSIZE .LE. 0.0 .OR. YSIZE .LE. 0.0) THEN
            CALL ERROR('CHAR. SIZE PARAMETER LESS THAN 0.0')
         ELSE
            DXSIZE = XSIZE
            DYSIZE = YSIZE
         ENDIF
      ELSE
         CALL ERROR('DEFAULT CHAR. SIZE MAY NOT CHANGE')
      ENDIF
      RETURN
      END
