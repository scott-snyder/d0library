      SUBROUTINE SLOPAN ( SLOPE, TETA, PHI )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts slopes to angles
C-                          ****  0 < PHI  < TWOPI
C-                          ****  0 < TETA < PI
C-   Inputs  : slope(1) = dx/ds
C-             slope(2) = dy/ds
C-             slope(3) = dz/ds
C-   Outputs : teta, phi
C-
C-   Created  12-OCT-1987   Ghita Rahal-Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCONST.INC'
      REAL SLOPE(3), TETA, PHI, SPH
C----------------------------------------------------------------------
C
      PHI  = 0.
      TETA = 0.
      IF ( SLOPE(1) .EQ. 0. .AND. SLOPE(2).EQ. 0.) GO TO 999
      PHI = ATAN2 ( SLOPE(2), SLOPE(1) )
      TETA = SQRT ( SLOPE(1)**2 + SLOPE(2)**2 )
      TETA = ATAN2 ( TETA, SLOPE(3) ) 
      IF ( PHI .LT. 0. ) PHI = PHI + TWOPI
  999 RETURN
      END
