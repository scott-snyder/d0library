C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_4P.FOR
C *1     3-FEB-1994 14:33:19 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_4P.FOR
      SUBROUTINE KTJET_4P(ETA,PHI,ETT,KTP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :CALCULATES 4 MOMENTA FROM ETA,PHI AND ET
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   3-JUN-1993   Kate Frame
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ETA, PHI, ETT, KTP(4), THETA
C----------------------------------------------------------------------
      KTP(1)=ETT*COS(PHI)
      KTP(2)=ETT*SIN(PHI)
      THETA=2*ATAN(EXP(-ETA))
      KTP(4)=ETT/SIN(THETA)
      KTP(3)=KTP(4)*COS(THETA)
  999 RETURN
      END
