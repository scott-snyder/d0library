      SUBROUTINE PU_SET_3D_POINT(XVS,YVS,WXS,WYS,WZS,ICHARS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set picked point in virtual and world, and
C-                         button ID.
C-
C-   Inputs  : XVS,YVS     [F] - virtual coordinate in X and Y
C-             WXS,WYS,WZS [F] - world coordinate in X, Y and Z
C-             ICHARS      [I] - button ID
C-   Controls:
C-
C-   Created  28-AUG-1992   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    XVS,YVS, WXS,WYS,WZS
      INTEGER ICHARS
      REAL    XVG,YVG, WXG,WYG,WZG
      INTEGER ICHARG
      REAL    XVIRT,YVIRT,XWORLD,YWORLD,ZWORLD
      INTEGER ICHAR
      SAVE    XVIRT,YVIRT,XWORLD,YWORLD,ZWORLD, ICHAR
C----------------------------------------------------------------------
C
      XVIRT  = XVS
      YVIRT  = YVS
      XWORLD = WXS
      YWORLD = WYS
      ZWORLD = WZS
      ICHAR  = ICHARS
      RETURN
C
C **** To get picked point in virtual and world, and button ID.
C
      ENTRY PU_GET_3D_POINT(XVG,YVG,WXG,WYG,WZG,ICHARG)
C
      XVG    = XVIRT
      YVG    = YVIRT
      WXG    = XWORLD
      WYG    = YWORLD
      WZG    = ZWORLD
      ICHARG = ICHAR
C
  999 RETURN
      END
