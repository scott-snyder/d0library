      FUNCTION FDC_DRIFT_SLOPE(SLOPE,LR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correction to hit position based on slope 
C-     of segment.
C-
C-   Returned value  : FDC_DRIFT_SLOPE, Amount of shift in cm
C-   Inputs  : SLOPE, DY/DZ of segment
C-             LR, Side of wire plane (0 = plus side, 1= minus side)
C-
C-   Created   4-JUN-1991   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    FDC_DRIFT_SLOPE
C INPUT:
      INTEGER LR
      REAL    SLOPE
C LOCAL:
      INTEGER IER
      REAL    FACTOR,OFFSET
      LOGICAL FIRST
      SAVE FIRST,FACTOR,OFFSET
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('T0_OFFSET',OFFSET,IER)
        CALL EZGET('T0_FACTOR',FACTOR,IER)
        CALL EZRSET
      ENDIF
C      
      FDC_DRIFT_SLOPE = (-1)**LR * ( OFFSET + FACTOR * SLOPE**2 )
C
  999 RETURN
      END
