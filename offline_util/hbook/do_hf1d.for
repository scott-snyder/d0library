      SUBROUTINE DO_HF1D(ID,Q,W)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DO_HF1 IN DOUBLE PRECISION
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   7-APR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ID
      DOUBLE PRECISION Q,W
      REAL    QS,WS
C----------------------------------------------------------------------
      QS=Q
      WS=W
      CALL DO_HF1(ID,QS,WS)
  999 RETURN
      END
