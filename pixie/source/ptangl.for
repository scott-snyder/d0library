      FUNCTION PTANGL(X1,Y1,X2,Y2,DIS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates the angle between two points
C-               
C-   Inputs  : The X's AND Y's coordinate of ponit the two points 
C-   Outputs : Angle between the points (in radians)
C-
C-   Created   6-JAN-1989   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL PTANGL,X1,Y1,X2,Y2,DIS,ANG,PI
      DATA PI/3.1415927/
C----------------------------------------------------------------------
      ANG=ABS(ASIN((Y2-Y1)/DIS))
      IF(X2.LT.0) THEN
        IF(Y2.LT.0) THEN
          ANG=ANG+PI        ! in 3rd quadrant
        ELSE
          ANG=PI-ANG        ! in 2nd quadrant
        ENDIF
      ELSE
        IF (Y2.LT.0) 
     X    ANG=(2.*PI)-ANG   ! in the 4th quadrant
      ENDIF
      PTANGL=ANG
  999 RETURN
      END
