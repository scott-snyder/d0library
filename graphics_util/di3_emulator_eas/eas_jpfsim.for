      SUBROUTINE JPFSIM(DSPDV, SIMMOD)
C
C    Purpose:
CD   The purpose of this module is to set the polygon fill simulation
CD   mode. DSPDV is the display device (not tested) and SIMMOD is the
CD   simulation mode where zero is always, one is conditional, and two
CD   is never.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 08-Nov-1988
CH   History:
CH      08-NOV-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      PLGATT-W
C
C    Calls:
CC      NONE.
C
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER DSPDV, SIMMOD
C
C    Then local declarations of variables (non-common variables).
C
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:PLGATT.INC/LIST'
C
C    Then executable code follows
C
      PFSIM = MOD(SIMMOD, 3)
      RETURN
      END
