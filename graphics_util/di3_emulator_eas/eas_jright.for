      SUBROUTINE JRIGHT(LVALUE)
C
C    Purpose:
CD   This module defines the handedness of the coordinate systems. The 
CD   parameter passed is a logical value. A true value designates a
CD   right handed coordinate; a false value sets a left handed system. 
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 26-Oct-1988
CH   History:
CH      26-OCT-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-W, SEGINF-R
C
C    Calls:
CC      ERROR.
C
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      LOGICAL LVALUE
C
C    Then local declarations of variables (non-common variables).
C
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         CALL ERROR('JRIGHT: A SEGMENT IS OPEN')
      ENDIF
      IF (LVALUE) THEN
         RIGHT = -1.0
      ELSE
         RIGHT = 1.0
      ENDIF
      RETURN
      END
