      SUBROUTINE MTC_COSETAPHI(DIR, ETA,PHI)
C----------------------------------------------------------------------
C- MTC_COSETAPHI: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Turn the input direction cosines DIR(3) of a line
C-      into an equivalent ETA and PHI
C-
C-   Created  17-AUG-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ETA,PHI, DIR(3)

      REAL THETA, X1,Y1,Z1
C- need some constants ...
      REAL PI, TWOPI, HALFPI
      PARAMETER (PI=3.141593,TWOPI=6.283185,HALFPI=1.570796)
C----------------------------------------------------------------------
      THETA = ACOS(DIR(3))
      IF(THETA.EQ.HALFPI) THEN
        ETA = 0.
      ELSE IF(THETA.LE.HALFPI) THEN
        ETA = -LOG(TAN( THETA/2. ))
      ELSE
        ETA = LOG(TAN( (PI-THETA)/2. ))
      END IF

      IF(DIR(2).EQ.0. .AND. DIR(1).EQ.0.) THEN
        PHI = 0.
      ELSE
        PHI = ATAN2(DIR(2),DIR(1))
        IF(PHI.LT.0.) PHI = PHI + TWOPI
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
