      SUBROUTINE SALOOP (NV, XY, X0, Y0, CX, CY,
     +                       NCR, XCR, YCR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find coordinates of crossing points
C-                         between loop and line.
C-
C-   Inputs  : NV - number of loop vertex
C-             XY(2,NV) - coordinates of loop vertex
C-             X0,Y0 - coordinate of point on line
C-             CX,CY - cos of X and Y angle of line (CX**2+CY**2=1)
C-   Outputs : NCR - number of crossing points
C-             XCR,YCR - coordinates of crossing points
C-   Controls:
C-
C-   Created  20-SEP-1990   A. Efimov.
C-   Updated  30-APR-1991   Andrei Kiryunin: use array XY 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NV, NCR
      REAL XY(2,NV), X0, Y0, CX, CY
      REAL XCR(*), YCR(*)
      INTEGER I, J, K1, K2
      REAL DX, DY, XX, YY, U, D, T
C
      NCR = 0
      DO I = 1, NV
        J = I + 1
        IF (I .EQ. NV) J = 1
        DX = XY(1,J) - XY(1,I)
        DY = XY(2,J) - XY(2,I)
        U = (Y0 - XY(2,I)) * DX - (X0 - XY(1,I)) * DY
        D = CX * DY - CY * DX
        IF (ABS(D) .GT. 1.0E-13) THEN
          T = U / D
          XX = X0 + T * CX
          YY = Y0 + T * CY
          U = (XX-XY(1,I))*(XX-XY(1,J)) + (YY-XY(2,I))*(YY-XY(2,J))
          IF (U .LT. 0.0) THEN
            NCR = NCR + 1
            XCR(NCR) = XX
            YCR(NCR) = YY
          END IF
        END IF
      END DO
C
      RETURN
      END
