        SUBROUTINE JVPORT (UMIN, UMAX, VMIN, VMAX)
C
C    Purpose:
CD   This module defines user specified viewport boundaries of the
CD   virtual coordinate system. The parameters passed are:
CD      UMIN  --  Mininum x boundary.
CD      UMAX  --  Maximum x boundary.
CD      VMIN  --  Mininum y boundary.
CD      VMAX  --  Maximum y boundary.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 30-Sep-1988
CH   History:
CH      30-SEP-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-W
C
C    Calls:
CC      None.
C
C
C    Next is the declaration of parameters passed to the subroutine/function.
      REAL UMIN, UMAX, VMIN, VMAX
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
      UVIEW(1) = UMIN
      UVIEW(2) = UMAX
      UVIEW(3) = VMIN
      UVIEW(4) = VMAX
      RETURN
      END
