      SUBROUTINE EXTCYL ( VIN, VOUT, RNEW, IREJ )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Intersection of a track with a cylinder defined by
C-                         the radius of its section.
C-                         The cylinder is a tube parallel to Oz, with its
C-                         section in the plane xOy.
C-
C-   Inputs  :  VIN(6) = X, Y, Z, DX/DS, DY/DS, DZ/DS for the track
C-              RNEW   = Radius of the section of the cylinder
C-   Outputs :  VOUT(6)= new components of the track
C-              IREJ   =  0 if the calculation is OK ; else 1
C-
C-   Created  18-SEP-1987   Ghita Rahal-Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IREJ
      REAL VIN(6), VOUT(6), RNEW
      DOUBLE  PRECISION A, B, C, DELTA, S
C----------------------------------------------------------------------
      IREJ = 0
      CALL UCOPY ( VIN(4), VOUT(4), 3)
C
      A = VIN(4)*VIN(4) + VIN(5)*VIN(5)
      IF ( A .EQ. 0. ) THEN
C
C ****  The track cannot reach the cylinder
C
        IREJ = 1
        GO TO 999
      ENDIF
      B = VIN(1)*VIN(4) + VIN(2)*VIN(5) 
      C = VIN(1)*VIN(1) + VIN(2)*VIN(2) - RNEW*RNEW
      DELTA = B*B - A*C
      IF ( DELTA .LT. 0. ) THEN
        IREJ = 1
        GO TO 999
      ENDIF
      S = ( -B  + SQRT(DELTA) ) / A
      VOUT(1) = VIN(1) + S * VIN(4)
      VOUT(2) = VIN(2) + S * VIN(5)
      VOUT(3) = VIN(3) + S * VIN(6)
  999 RETURN
      END
