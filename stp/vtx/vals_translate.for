      SUBROUTINE VALS_TRANSLATE(X0,Y0)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : translate each layer by (X0(LAY),Y0(LAY))
C-                        
C-
C-   Inputs  : X0(0:2), Y0(0:2)
C-   Outputs : VALS bank
C-   Controls: 
C-
C-   Created   7-JUL-1992   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      REAL X0(0:2),Y0(0:2)
c Locals:
      INTEGER LAYER
      INTEGER SECTOR,NSEC(0:2),LVALS,WIRE,J
      DATA NSEC/15,31,31/
c Externals:
      INTEGER GZVALS
      REAL    ATAN3
C----------------------------------------------------------------------
      DO LAYER = 0, 2
        DO SECTOR = 0,NSEC(LAYER)
          LVALS = GZVALS(LAYER,SECTOR)
          DO WIRE = 0,7
            J = 6 + WIRE*IC(LVALS+6)
            C(LVALS+J+1) = C(LVALS+J+1) + X0(LAYER)
            C(LVALS+J+2) = C(LVALS+J+2) + Y0(LAYER)
          ENDDO
        ENDDO
      ENDDO
  999 RETURN
      END
