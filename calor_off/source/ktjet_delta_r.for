C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_DELTA_R.FOR
C *1     3-FEB-1994 14:36:28 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_DELTA_R.FOR
      REAL FUNCTION KTJET_DELTA_R(I,J)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-JUN-1993   Kate Frame
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$INC:KTJET_LINK.INC'
      INTEGER I, J
      REAL X,Y
      INCLUDE 'D0$INC:KTJET_UTIL.DEF'
C----------------------------------------------------------------------
      X=ABS(Q(KTPHI(I)) - Q(KTPHI(J)))
      Y=ABS(2*PI - X)
      KTJET_DELTA_R = MIN(X,Y)**2 + (Q(KTETA(I)) - Q(KTETA(J)))**2
  999 RETURN
      END
