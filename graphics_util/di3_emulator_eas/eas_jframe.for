      SUBROUTINE JFRAME
C
C    Purpose:
CD   This module clears all non-retained segments.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 26-Aug-1988
CH   History:
CH      26-AUG-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-R
C
C    Calls:
CC      PPURGE, KFRAM, ERROR
C
      EXTERNAL ERRHND
C
C    Next is the declaration of parameters passed to the subroutine/function.

C
C    Then local declarations of variables (non-common variables).
C

C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         CALL ERROR('JFRAME: A SEGMENT IS OPEN')
      ENDIF
      CALL KFRAM
      CALL PPURGE(ERRHND)
      RETURN
      END
