C======================================================================
      SUBROUTINE PTSECT(R,A,SCALE)
C======================================================================
C
C  Description: Draw conic section between 2 radii 
C  ============
C
C- Input:   R Inner and outer radii of conic section
C  ======
c           A Inner and outer arc lengths 
C  Author:
C  =======
C  SHARON HAGOPIAN
C
C  Revision History:
C  =================
C  Jun. 6, 1986 - Original creation
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
C  Argument Declarations:
C  ======================
      REAL R(2)  ! inner and outer radii of conic section
      REAL A(2)  ! inner and outer arc lenths
      REAL SCALE ! Scale to determine the size of sector
C  Local Declarations:
C  ===================
      REAL DEGRAD
      REAL A1,A2,Y1,Y2,X1,X2
      INTEGER IP
      REAL ANG
      REAL RSCALE   ! Radius scaled
      REAL R0        ! Radius of the sector
C======================================================================
C  Executable Code:
C  ================
      DEGRAD=PI/180.
      R0=R(2)-R(1)
      RSCALE=(R0*SCALE)+R(1)
C DRAW 2 ARCS OF A CIRCLE
      A1=A(1)
      A2=A(2)
      CALL JARC(0.,0.,0.,R(1),0,A1,A2)
      CALL JARC(0.,0.,0.,RSCALE,0,A1,A2)
C DRAW SIDES OF CELL RADIALLY
      DO 10 IP=1,2
      ANG=DEGRAD*A(IP)
      X1=R(1)*COS(ANG)
      Y1=R(1)*SIN(ANG)
      Y2=RSCALE*SIN(ANG)
      X2=RSCALE*COS(ANG)
      CALL JMOVE(X1,Y1)
      CALL JDRAW(X2,Y2)
   10 CONTINUE
      RETURN
      END
