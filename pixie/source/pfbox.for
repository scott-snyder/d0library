      SUBROUTINE PFBOX(XC,YC,ZC,DX,DY,DZ,ANG)
C======================================================================
C
C  Description:  Draws a box at the position specified, with the 
C  ============  specified dimensions rotated by ANGle in degrees in X,Y
C
C  Parameter Description:
C  =======================
C
C  XC,YC,ZC - The world coordinate position of the center of the box
C
C  DX,DY,DZ - Half-lengths of the width, heigth and depth, respectively,
C             of the box
C  ANG      - Angle to rotated about in X,Y plane
C
C   Created  29-MAY-1990   Jeffrey Bantly   based on T.Kramer pxbox.for
C-   Updated  23-MAR-2004   compile with g77.
C
C======================================================================
C
      IMPLICIT NONE
      include 'd0$inc:pi.def'
      INTEGER I
      REAL XC,YC,ZC,DX,DY,DZ,ANG
      REAL XARR(4),YARR(4),ZARR1(4),ZARR2(4),X(4),Y(4)
C-----------------------------------------------------------------------
C
      X(1) = XC - DX
      Y(1) = YC - DY
      X(2) = XC - DX
      Y(2) = YC + DY
      X(3) = XC + DX
      Y(3) = YC + DY
      X(4) = XC + DX
      Y(4) = YC - DY
C
C  rotate the X,Y coordinates by ANG around Z axis
C
        DO 10 I=1,4
          XARR(I)=X(I)*COS(ANG*pi/180)-Y(I)*SIN(ANG*pi/180)
          YARR(I)=X(I)*SIN(ANG*pi/180)+Y(I)*COS(ANG*pi/180)
   10   CONTINUE
C
      DO 20 I = 1,4
         ZARR1(I) = ZC - DZ
         ZARR2(I) = ZC + DZ
         CALL J3MOVE(XARR(I),YARR(I),ZARR1(I))
         CALL J3DRAW(XARR(I),YARR(I),ZARR2(I))
   20 CONTINUE
C
      CALL J3MOVE( XARR(4),YARR(4),ZARR1(4) )
      DO 30 I=1,4
        CALL J3DRAW( XARR(I),YARR(I),ZARR1(I) )
   30 CONTINUE
C
      CALL J3MOVE( XARR(4),YARR(4),ZARR2(4) )
      DO 40 I=1,4
        CALL J3DRAW( XARR(I),YARR(I),ZARR2(I) )
   40 CONTINUE
C--------------------------------------------------------------------------
      RETURN
      END
