        SUBROUTINE JWINDO(UMIN, UMAX, VMIN, VMAX)
C
C    Purpose:
CD   This subroutine defines the boundaries and user coordinate
CD   system the default user window is (-1.0,1.0,-1.0,1.0). The
CD   parameters passed in are as follows:
CD       UMIN  -  Minimum window boundary along the horizontal.
CD       UMAX  -  Maximum window boundary along the horizontal.
CD       VMIN  -  Minimum window boundary along the vertical.
CD       VMAX  -  Maximum window boundary along the vertical.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 30-Sep-1988
CH   History:
CH      30-Sep-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-W
C
C    Calls:
CC      None.
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
      UWIND(1) = UMIN
      UWIND(2) = UMAX
      UWIND(3) = VMIN
      UWIND(4) = VMAX
      RETURN
      END
