      SUBROUTINE MULOCLS(DX,DY,POINT,SLOPE,CHISQ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Determine Least-Squares fit for 1 hit/plane
C-                          in A-layer segment
C-
C-   Inputs  :
C-      DX(4)   position of hits
C-      DY(4)
C-
C-   Outputs :
C-      POINT   one point describing best fit line
C-      SLOPE   slope of best fit line
C-      CHISQ   chi squared of best fit line
C-
C-   Created  28-JUL-1994   Elizabeth Brillhart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER I

      REAL    DX(4),DY(4),POINT,SLOPE,CHISQ
      REAL    DXSQR,DXSUM,DYSUM,DXYSM
      REAL    DELTA,SIGMA(4)
C----------------------------------------------------------------------
      DXSQR = 0.
      DXSUM = 0.
      DYSUM = 0.
      DXYSM = 0.
      CHISQ = 0.
      DELTA = 0.
      CALL VZERO(SIGMA,4)

      DO I=1,4
        DXSQR    = DXSQR + DX(I)**2
        DXSUM    = DXSUM + DX(I)
        DYSUM    = DYSUM + DY(I)
        DXYSM    = DXYSM + DX(I)*DY(I)
        SIGMA(I) = 0.1
      END DO

      DELTA = 4*DXSQR - DXSUM**2

      IF (DELTA .EQ. 0.) THEN
        POINT = 9999.
        SLOPE = 9999.
        CHISQ = 9999.
        GO TO 999 
      END IF

      POINT = (1/DELTA) * (DXSQR*DYSUM - DXSUM*DXYSM)
      SLOPE = (1/DELTA) * (4*DXYSM - DXSUM*DYSUM)

      DO I=1,4
        CHISQ = CHISQ + ( (1/(SIGMA(I)))*(DY(I)-POINT-SLOPE*DX(I)) )**2
      END DO

  999 RETURN
      END
