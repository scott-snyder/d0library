      SUBROUTINE JSIZE(XSIZE, YSIZE)
C
C    Purpose:
CD   This routine sets the current size of the character box for text
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
CH      16-JUN-92  Nobu Oshima - Make *.9 scale 
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
      IF (SEGOPN) THEN
         IF (XSIZE .LE. 0.0 .OR. YSIZE .LE. 0.0) THEN
            CALL ERROR('CHAR. SIZE PARAMETER LESS THAN 0.0')
         ELSE
            CXSIZE = XSIZE *.94
            CYSIZE = YSIZE *.94
         ENDIF
      ELSE
         CALL ERROR('NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END
