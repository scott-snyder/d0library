

      SUBROUTINE CECPTS(TH,PH,ZED,X,Y,Z)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  12-DEC-1988   Michael W. Peters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL TH,PH,ZED,X,Y,Z
      REAL TTH
      TTH=TAN(TH)
      X=ZED*TTH*COS(PH)
      Y=ZED*TTH*SIN(PH)
      Z=ZED
  999 RETURN
      END
