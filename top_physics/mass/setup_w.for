      SUBROUTINE SETUP_W
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SET UP W 4 VECTORS
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-JAN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KINEQ.INC'
      DOUBLE PRECISION  WPZ2
C----------------------------------------------------------------------
      CALL FIND_WLNUD(WMASS,LEPTON1,PNUT1,W1(1,1),WPZ2,SOL(1))
      IF ( SOL(1) ) THEN
        CALL UCOPYDD(W1(1,1),W1(1,2),4)
C THE OTHER SOLUTION
        W1(3,2) = WPZ2
        W1(4,2) = DSQRT(WMASS**2 + W1(1,2)**2 + W1(2,2)**2 + W1(3,2)**2)
      ENDIF
C
C NOW FOR THE SECOND W
C
      CALL FIND_WLNUD(WMASS,LEPTON2,PNUT2,W2(1,1),WPZ2,SOL(2))
      IF ( SOL(2) ) THEN
        CALL UCOPYDD(W2(1,1),W2(1,2),4)
C THE OTHER SOLUTION
        W2(3,2) = WPZ2
        W2(4,2) = DSQRT(WMASS**2 + W2(1,2)**2 + W2(2,2)**2 + W2(3,2)**2)
      ENDIF
C
  999 RETURN
      END
