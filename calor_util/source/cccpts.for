

      SUBROUTINE CCCPTS(TH,PH,RAD,X,Y,Z)
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
      REAL TH,PH,RAD,X,Y,Z
C
      X=RAD*COS(PH)
      Y=RAD*SIN(PH)
      Z=RAD/TAN(TH)
  999 RETURN
      END
