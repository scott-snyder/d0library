      SUBROUTINE JET_CON_AREA(CONE_USED,ETA,AREA,AREA_STHW,
     &  AREA_ICR,AREA_ICR_STHW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the area of jet for later use
C-      in underlying event zero suppression noise calculations.
C-
C-   Inputs  : CONE_USED     [R]  : Conesize of jet
C-             ETA           [R]  : Detector eta of jet
C-   Outputs : AREA          [R]  : eta x phi area of jet
C-             AREA_STHW     [R]  :   '' sin(theta) weighted
C-             AREA_ICR      [R]  :   '' w/in ICR
C-             AREA_ICR_STHW [R]  :   '' w/in ICR sin(theta) weighted
C-
C-   Created  17-OCT-1995   Bob Hirosky
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL CONE_USED, ETA
      REAL AREA, AREA_STHW, AREA_ICR, AREA_ICR_STHW
      REAL ETA_SLICE, SINTHETA, DPHI, FACTOR
      REAL SMALL
      PARAMETER( SMALL = .0001)
C----------------------------------------------------------------------
C
      AREA = 0.0
      AREA_STHW = 0.0
      AREA_ICR = 0.0
      AREA_ICR_STHW = 0.0
C
C: Since cone can be large, break the calculation up into eta slices.
C
C: Use .099 instead of .1 to avoid round off problems that would
C: stop the loop prematurely
C
      DO ETA_SLICE = ETA - CONE_USED +.05, ETA + CONE_USED -.05, .099
        SINTHETA  = 1./COSH(ETA_SLICE)
        DPHI     = 2*SQRT( CONE_USED**2 - ABS(ETA_SLICE - ETA)**2 )
        FACTOR = DPHI*.1
        AREA     = AREA + FACTOR
        AREA_STHW = AREA_STHW + FACTOR * SINTHETA
C
C: ICR contributions
C
        IF ( ABS(ETA_SLICE + SMALL) .GE. 1.1 .AND. ABS(ETA_SLICE -
     &    SMALL) .LE. 1.5 ) THEN
          AREA_ICR = AREA_ICR + FACTOR
          AREA_ICR_STHW = AREA_ICR_STHW + FACTOR * SINTHETA
        ENDIF
      ENDDO
C
  999 RETURN
      END
