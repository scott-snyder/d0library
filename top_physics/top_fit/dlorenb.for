      SUBROUTINE DLORENB (U,PS,PI,PF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Backward Lorentz Transforms a 4 vector PI
C-                         in rest frame specified by PS(4) of mass U
C-                         to the lab.
C-
C-   Inputs  : U=mass of rest frame 4 vector
C-             PS(1:4) = 4 vector of rest frame particle in lab
C-             PI(1:4) = 4 vector of particle in rest frame PS
C-   Outputs : PF(1:4) = 4 vector of particle Pi in lab
C-   Controls: 
C-
C-   Created  26-FEB-1994   Rajendran Raja
C-                          Double precision version of CERNLIB
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C CERN PROGLIB# U102    LORENB          .VERSION KERNFOR  4.04  821124
C ORIG. 20/08/75 L.PAPE
C
      DOUBLE PRECISION PF4, FN ,U
      DOUBLE PRECISION      PS(4),PI(4),PF(4)
C----------------------------------------------------------------------
C
      IF (PS(4).EQ.U) GO TO 17
      PF4  = (PI(4)*PS(4)+PI(3)*PS(3)+PI(2)*PS(2)+PI(1)*PS(1)) / U
      FN   = (PF4+PI(4)) / (PS(4)+U)
      PF(1)= PI(1) + FN*PS(1)
      PF(2)= PI(2) + FN*PS(2)
      PF(3)= PI(3) + FN*PS(3)
      PF(4)= PF4
      GO TO 18
C
   17 PF(1)= PI(1)
      PF(2)= PI(2)
      PF(3)= PI(3)
      PF(4)= PI(4)
C
   18 CONTINUE
C
  999 RETURN
      END
