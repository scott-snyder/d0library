      SUBROUTINE MTC_ETAPHICOS(ETA,PHI,DIR)
C----------------------------------------------------------------------
C- MTC_ETAPHICOS: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Find the direction cosines DIR(3) of a line
C-      specified by the input ETA and PHI
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
      IF(ETA.EQ.0.) THEN
        THETA = HALFPI
      ELSE IF(ETA.GE.0.) THEN
        THETA =      2. * ATAN(EXP( -ETA ))
      ELSE
        THETA = PI - 2. * ATAN( EXP(ETA) )
      END IF

      X1 = SIN(THETA) * COS(PHI)
      Y1 = SIN(THETA) * SIN(PHI)
      Z1 = COS(THETA)

      DIR(1) = X1
      DIR(2) = Y1
      DIR(3) = Z1

C----------------------------------------------------------------------
  999 RETURN
      END
