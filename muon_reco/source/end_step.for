      FUNCTION END_STEP(ORENT,VOUT,DIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : LAST STEP IN THE TOROID
C-
C-   Returned value  : END_STEP
C-   Inputs  : ORENT,VOUT,DIST-DISTANCE FROM SURFACE
C-   Outputs : END_STEP
C-   Controls: 
C-
C-   Created   9-MAY-1991   AKL
C-   Updated  26-JUN-1991   AKL 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ORENT
      REAL END_STEP,VOUT(7),DIST,TG,CS,SI
C----------------------------------------------------------------------
      IF(ORENT.EQ.1)
     &  TG = VOUT(4)*VOUT(4)/(VOUT(5)*VOUT(5) + VOUT(6)*VOUT(6))
      IF(ORENT.EQ.2)
     &  TG = VOUT(5)*VOUT(5)/(VOUT(4)*VOUT(4) + VOUT(6)*VOUT(6))
      IF(ORENT.EQ.3 .OR. ORENT .EQ.4)
     &  TG = VOUT(6)*VOUT(6)/(VOUT(4)*VOUT(4) + VOUT(5)*VOUT(5))
       CS = 1./(1. + TG)
       SI = SQRT(1. - CS)
       END_STEP = ABS(DIST/SI)
  999 RETURN
      END
