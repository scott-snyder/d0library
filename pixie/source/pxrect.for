      SUBROUTINE PXRECT(RCTCOL,XC,YC,ZC,DX,DY)
C======================================================================
C
C  Description:  Draws a rectangle with center at XC,YC,ZC with
C  ============  sides of dimensions 2*DX and 2*DY
C
C  Author:
C  =======
C  Tami Kramer
C
C  Input Arguements:
C  =================
C  XC,YC,ZC - world coordinates of the center of the rectangle
C  DX,DY - Half-lengths of the sides of the rectangle
C
C  Conditions necessary before call:
C  =================================
C  Graphics initialized
C
C  Revision History:
C  =================
C  Original Creation - December 5,1986
C  Updated 10-JAN-1990 Lupe Howell Implementing color table
C
C======================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      CHARACTER*(*) RCTCOL
      REAL XC,YC,ZC,DX,DY
      REAL XLB,YLB
C
C  Executable Code:
C  ================
C
      CALL PXCOLR(RCTCOL)
      XLB = XC - DX
      YLB = YC - DY
C
      CALL JMOVE(XLB,YLB)
      CALL JRRECT(2*DX,2*DY)
C
      RETURN
      END
