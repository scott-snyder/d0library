      SUBROUTINE PURECT(XL,YL,Z,XR,YR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw a rectangle in world coordinates with
C-   UPPER-LEFT-HAND point at (XL,YL,Z) and LOWER-RIGHT-HAND point at
C-   (XR,YR,Z). A segment must be opened before calling PURECT.
C-
C-   Inputs  : XL,XL    [R]     First Point
C-             Z        [R]     Z-coordinate of points
C-             XR,YR    [R]     Second point
C-   Outputs : None
C-   Controls: None
C-
C-   Created  23-MAY-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    XL,YL,Z,XR,YR
      REAL    XX(4),YY(4),ZZ(4)
C----------------------------------------------------------------------
      XX(1) = XL
      YY(1) = YL
      ZZ(1) = Z
      XX(2) = XR
      YY(2) = YL
      ZZ(2) = Z
      XX(3) = XR
      YY(3) = YR
      ZZ(3) = Z
      XX(4) = XL
      YY(4) = YR
      ZZ(4) = Z
      CALL J3PLGN(XX,YY,ZZ,4)
  999 RETURN
      END
