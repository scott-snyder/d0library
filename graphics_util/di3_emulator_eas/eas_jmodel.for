      SUBROUTINE JMODEL(MATRIX)
C
C    Purpose:
CD   This module defines the current modeling transformation matrix to 
CD   be applied if modeling transformation is enabled.
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
      REAL MATRIX(4,4)
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I, J
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         CALL ERROR('JMODEL: A SEGMENT IS OPEN')
      ENDIF
      DO 10 I=1,4
         DO 10 J=1,4
            MODMAT(I,J) = MATRIX(I,J)
   10 CONTINUE
      RETURN
      END
