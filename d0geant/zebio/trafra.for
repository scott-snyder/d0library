      SUBROUTINE TRAFRA(X1,Y1,Z1,CTTHE,STTHE,CTPHI,STPHI,
     &  X0,Y0,Z0,X2,Y2,Z2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates coordinates in a new frame.  
C-
C-   Inputs  : X1,Y1,Z1,CTTHE,STTHE,CTPHI,STPHI
C-   Outputs : X2,Y2,Z2
C-   Controls: 
C-
C-   Created  22-JUL-1993   J.P. Cussonneau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL X1,Y1,Z1,X2,Y2,Z2,X0,Y0,Z0
      REAL CTPHI,STPHI,CTTHE,STTHE
C --
      X2=X0+CTPHI*(CTTHE*X1+STTHE*Z1)-STPHI*Y1
      Y2=Y0+STPHI*(CTTHE*X1+STTHE*Z1)+CTPHI*Y1
      Z2=Z0-STTHE*X1+CTTHE*Z1
C----------------------------------------------------------------------
  999 RETURN
      END
