      SUBROUTINE JYON(YDIST)
C
C    Purpose:
CD   This module sets the yon clipping plane distance. The parameter
CD   passed is a real designating the yon plane distance.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 21-Oct-1988
CH   History:
CH      21-OCT-88  ATV  Initial entry.
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
      REAL YDIST
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
         CALL ERROR('JYON: A SEGMENT IS OPEN')
      ENDIF
      UWIND(6) = YDIST
      RETURN
      END
