      SUBROUTINE GET_E_VARIANCE(P4,VARP3,DPIDPJ,VARE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate Energy Variance, given an error matrix
C-                         in (px,py,pz)
C-
C-   Returned value  : VAR(E)
C-
C-   Inputs  :  P4(4)     [R]  4-vector (px,py,pz,e)
C-              VARP3(3)  [R]  Variances: sig**2(px), sig**2(py), sig**2(pz)
C-              DPIDPJ(3) [R]  Covariances: DpxDpy, DpxDpz, DpyDpz
C-   Outputs :  VARE      [R]  Energy Variance
C-   Controls: 
C-
C-   Created   1-JUL-1993   Stan M. Krzywdzinski
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL     VARE,P4(4),VARP3(3),DPIDPJ(3)
C----------------------------------------------------------------------
      INTEGER  I
      REAL     P3N(3),TEMP
C----------------------------------------------------------------------
C
      VARE = 0.
      IF (P4(4) .LE. 0.)      GOTO 999
      DO I = 1,3
        IF (VARP3(I) .LT. 0.) GOTO 999
      ENDDO
C
      TEMP = 0.
      DO I = 1,3
        P3N(I) = P4(I)/P4(4)
        TEMP   = TEMP + VARP3(I)*P3N(I)**2
      ENDDO
      VARE = TEMP +
     &  2.*DPIDPJ(1)*P3N(1)*P3N(2) +
     &  2.*DPIDPJ(2)*P3N(1)*P3N(3) +
     &  2.*DPIDPJ(3)*P3N(2)*P3N(3)
      IF (VARE .LE. 0.) THEN
        VARE = TEMP
      ENDIF
C   
  999 RETURN
      END
