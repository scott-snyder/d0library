      SUBROUTINE CAL_EM_IMPACT2(DELTA_PHI,DELTA_Z,DELTA_R)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : this routine assumes that the arrays in 
C-                         'CIMPACT.INC' have been setup by "cal_em_impact"
C-
C-   Inputs  : 
C-   Outputs : calculate the differences delta_phi, delta_Z and delta_r
C              between the track impact point and the cluster centroid.
C-   Controls: 
C-
C-   Created  10-DEC-1991   Meenakshi Narain
C-   Updated   4-APR-1992   Rajendran Raja  re-written in true cylinderical 
C-   co-ordinates 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CIMPACT.INC'
      INCLUDE 'D0$INC:PI.DEF'
      REAL SIGN
      REAL DELTA_PHI,DELTA_Z,DELTA_R
      REAL PHI_CENTROID, PHI_ISAJET
      REAL Z_CENTROID, Z_ISAJET
      REAL R_CENTROID, R_ISAJET
      INTEGER I
C----------------------------------------------------------------------
      PHI_CENTROID = ATAN2(EM3AV(2),EM3AV(1))
      IF(PHI_CENTROID.LT.0.0)PHI_CENTROID = PHI_CENTROID + TWOPI
      PHI_ISAJET   = ATAN2(RIMPACT(2),RIMPACT(1))
      IF(PHI_ISAJET.LT.0.0)PHI_ISAJET = PHI_ISAJET + TWOPI
C
      DELTA_PHI    = PHI_ISAJET - PHI_CENTROID
C
      Z_CENTROID = EM3AV(3)
      Z_ISAJET = RIMPACT(3)
      DELTA_Z    = Z_ISAJET - Z_CENTROID
C    
      R_ISAJET = SQRT(RIMPACT(1)**2 + RIMPACT(2)**2)
      R_CENTROID = SQRT(EM3AV(1)**2 + EM3AV(2)**2)
      DELTA_R = R_ISAJET - R_CENTROID
C
  999 RETURN
      END
