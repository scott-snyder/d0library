      SUBROUTINE VALS_ROTATE(DELPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Rotate VALS bank for layer 0 and layer 1 relative 
C-                         to layer 2
C-
C-   Inputs  : DELPHI(0:2)
C-   Outputs : VALS bank
C-   Controls: 
C-
C-   Created   7-JUL-1992   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      REAL DELPHI(0:2)
c Locals:
      REAL PHI,DPHI,X,Y,PI
      INTEGER LAYER,SECTOR,NSEC(0:2),LVALS,WIRE,J
      DATA NSEC/15,31,31/
c Externals:
      INTEGER GZVALS
C----------------------------------------------------------------------
      PI = 4.*ATAN(1.)
      DO LAYER = 0,2
        DPHI = DELPHI(LAYER)
        DO SECTOR = 0,NSEC(LAYER)
          LVALS = GZVALS(LAYER,SECTOR)
          PHI = ATAN2( C(LVALS+4) , C(LVALS+3) )
          IF ( PHI .LT. 0. ) PHI = PHI + 2.*PI
          PHI = PHI + DPHI
          C(LVALS+3) = COS(PHI)
          C(LVALS+4) = SIN(PHI)
          DO WIRE = 0,7
            J = 6 + WIRE*IC(LVALS+6)
            X = C(LVALS+J+1)*COS(DPHI) - C(LVALS+J+2)*SIN(DPHI)
            Y = C(LVALS+J+1)*SIN(DPHI) + C(LVALS+J+2)*COS(DPHI)
            C(LVALS+J+1) = X
            C(LVALS+J+2) = Y
          ENDDO
        ENDDO
      ENDDO
  999 RETURN
      END
