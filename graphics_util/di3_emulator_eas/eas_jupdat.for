      SUBROUTINE JUPDAT
C
C    Purpose:
CD   This module simply updates the screen by sending any un-flushed
CD   vectors to the buffers and flush the buffers to the screen.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 17-Aug-1988
CH   History:
CH      17-AUG-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      None.
C
C    Calls:
CC      KUPDV, PPURGE
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

C
C    Then executable code follows
C
      CALL KUPDV
      CALL PPURGE(ERRHND)
      RETURN
      END
