      SUBROUTINE JVUPNT(XPNT, YPNT, ZPNT)
C
C    Purpose:
CD   This module sets the the view point in 3D world coordinates.
CD   All three parameters passed are real values representing the
CD   view point in world coordinates.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 24-Oct-1988
CH   History:
CH      24-OCT-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-W, SEGINF-R
C
C    Calls:
CC      ERROR
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      REAL XPNT, YPNT, ZPNT
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
         CALL ERROR('JVUPNT: A SEGMENT IS OPEN')
      ENDIF
      VUPNT(1) = XPNT
      VUPNT(2) = YPNT
      VUPNT(3) = ZPNT
      RETURN
      END
