      SUBROUTINE JIWIND(XV, YV, ZV)
C
C    Purpose:
CD   This module returns the current viewplane clipping boundaries.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 5-Dec-1988
CH   History:
CH      05-DEC-88  ATV  Corrected definitions of passed parameters.
CH      07-NOV-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-R
C
C    Calls:
CC      NONE.
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      REAL XV(4), YV(4), ZV(4)
C
C    Then local declarations of variables (non-common variables).
C
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
C
C    Then executable code follows
C
      XV(1) = UWIND(1)
      XV(2) = UWIND(1)
      XV(3) = UWIND(2)
      XV(4) = UWIND(2)
      YV(1) = UWIND(3)
      YV(2) = UWIND(4)
      YV(3) = UWIND(4)
      YV(4) = UWIND(3)
      ZV(1) = 0.0
      ZV(2) = 0.0
      ZV(3) = 0.0
      ZV(4) = 0.0
      RETURN
      END
