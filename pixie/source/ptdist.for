      FUNCTION PTDIST(X1,Y1,X2,Y2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gives the distance between two points 
C-
C-   Inputs  : X1,Y1,X2,Y2 - Two set of points
C-   Outputs : Distance betw points 
C-
C-   Created   6-JAN-1989   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL PTDIST,X1,Y1,X2,Y2,DIS
C----------------------------------------------------------------------
      DIS=SQRT((X2-X1)**2+(Y2-Y1)**2)
      PTDIST=DIS
  999 RETURN
      END
