      SUBROUTINE PXBOX(XC,YC,ZC,DX,DY,DZ)
C======================================================================
C
C  Description:  Draws a box at the position specified, with the 
C  ============  specified dimensions
C
C  Parameter Description:
C  =======================
C
C  XC,YC,ZC - The world coordinate position of the center of the box
C
C  DX,DY,DZ - Half-lengths of the width, heigth and depth, respectively,
C             of the box
C
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - March 19,1986
C
C======================================================================
C
      IMPLICIT NONE
C
C  Parameter Declarations:
C  =======================
C
      INTEGER I
      REAL XC,YC,ZC,DX,DY,DZ
      REAL XARR(4),YARR(4),ZARR1(4),ZARR2(4)
C
C  Executable Code:
C  ================
C
      XARR(1) = XC - DX
      YARR(1) = YC - DY
      XARR(2) = XC - DX
      YARR(2) = YC + DY
      XARR(3) = XC + DX
      YARR(3) = YC + DY
      XARR(4) = XC + DX
      YARR(4) = YC - DY
      DO 34 I = 1,4
         ZARR1(I) = ZC - DZ
         ZARR2(I) = ZC + DZ
         CALL J3MOVE(XARR(I),YARR(I),ZARR1(I))
         CALL J3DRAW(XARR(I),YARR(I),ZARR2(I))

   34 CONTINUE
      CALL J3PLGN(XARR,YARR,ZARR1,4)
      CALL J3PLGN(XARR,YARR,ZARR2,4)
      RETURN
      END
