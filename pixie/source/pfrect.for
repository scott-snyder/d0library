      SUBROUTINE PFRECT(ICOLOR,XC,YC,ZC,DX,DY,ANG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws rectangle with center at XC,YC,Z
C-                         and sides of dimension 2*DX, 2*DY
C-                         rotated by an angle ANG wrt X-axis  
C-
C-   Inputs  : icolor - color code for rectangle
C-             XC,YC,ZC - center of retangle in world coord.
C-             DX,DY - 1/2 size of rectangle
C-             ANG - angle of rotation of rectangle wrt X-axis
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-JAN-1989   Sharon Hagopian
C-   Updated   2-MAY-1989   Jeffrey Bantly  altered for upgraded FDC 
C-                                          display 
C-   Updated  10-JAN-1990   Lupe Howell   Color table implemented. 
C-                          ICOLOR parameter converted to a [C*3]
C-   Updated   7-FEB-1990   Jeffrey Bantly  remove temp fixes 
C-   Updated   6-AUG-1991   Robert E. Avery  Change definition of rotation,
C-                              simply rotates by ANG. 
C-   Updated  23-MAR-2004   compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      include 'd0$inc:pi.def'
      CHARACTER*(*) ICOLOR
      REAL XC,YC,ZC,DX,DY,ANG
      real angr
C- local variables:
      REAL U(4),V(4),UU(4),VV(4),TMPFIX
      INTEGER IP
C----------------------------------------------------------------------
      CALL PXCOLR(ICOLOR)
      U(1)=XC-DX
      V(1)=YC-DY
      U(2)=XC+DX
      V(2)=YC-DY
      U(3)=XC+DX
      V(3)=YC+DY
      U(4)=XC-DX
      V(4)=YC+DY
      IF(ANG.EQ.0.)THEN
C
        CALL JPOLGN(U,V,4)
      ELSE
C
C  rotate the coordinate system by ANG
C
        angr = ang*pi/180
        DO IP=1,4
          UU(IP)=U(IP)*COS(ANGr)-V(IP)*SIN(ANGr)
          VV(IP)=U(IP)*SIN(ANGr)+V(IP)*COS(ANGr)
        ENDDO
        CALL JPOLGN(UU,VV,4)
      ENDIF 
C----------------------------------------------------------------------
  999 RETURN
      END
