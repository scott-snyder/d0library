      FUNCTION FDC_IN_ROAD(HALF,QTRAK,ZVTX,PMIN,PMAX,TMIN,TMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if a particular FDC track is in the road
C-
C-   Returned value  : .TRUE. if in road
C-                     .FALSE. if not
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-APR-1993   Susan K. Blessing
C-   Updated  24-MAY-1993   Susan K. Blessing  Subtract 0.00001 from
C-    PI and TWOPI before making comparison for full tracking case.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'                             
C
      LOGICAL FDC_IN_ROAD
C
      INTEGER LFTRH,GZFTRH
      INTEGER HALF
C
      REAL QTRAK(26)
      REAL ZVTX
      REAL PMIN,PMAX,TMIN,TMAX
      REAL Z0(0:1)
      REAL XPOS,YPOS,ZPOS,RPOS
      REAL PHI,PHI1,PHI2,PHICEN
      REAL THETA,THECEN
      REAL SINGLEPI,SINGLETWOPI
C
      LOGICAL FIRST
      LOGICAL PHIRD
C
      SAVE FIRST,Z0,SINGLEPI,SINGLETWOPI
C
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        LFTRH = GZFTRH()
        Z0(0)= Q(LFTRH+3)
        Z0(1)= Q(LFTRH+4)
        SINGLEPI = PI
        SINGLETWOPI = TWOPI
        FIRST = .FALSE.
      ENDIF
C
C If road is completely open, track must be in it
      IF (PMIN.LE.0.AND.PMAX.GT.SINGLETWOPI-0.00001.AND.
     &  TMIN.LE.0.AND.TMAX.GE.SINGLEPI-0.00001) THEN
        FDC_IN_ROAD = .TRUE.
        GO TO 999
      ELSE 
        FDC_IN_ROAD = .FALSE.
      END IF
C
      XPOS = QTRAK(4)
      YPOS = QTRAK(5)
C
C  check the track's direction in PHI
C
      PHI = QTRAK(6)
      PHIRD = .FALSE.
      IF (PMIN .GE. 0.0 .AND. PMAX .LE. TWOPI) THEN
        IF (PHI .GE. PMIN .AND. PHI .LE. PMAX) PHIRD = .TRUE.
      ELSE
        IF (PMIN .LT. 0.0) THEN
          PHI1 = PHI - TWOPI
          IF (PHI .GE. 0.0 .AND. PHI .LE. PMAX
     &      .OR. PHI1 .GE. PMIN .AND. PHI1 .LE. 0.0) PHIRD = .TRUE.
        ENDIF
        IF (PMAX .GT. TWOPI) THEN
          PHI2 = PHI + TWOPI
          IF (PHI .GE. PMIN .AND. PHI .LE. TWOPI
     &      .OR. PHI2 .GE. TWOPI .AND. PHI2 .LE. PMAX) PHIRD = .TRUE.
        ENDIF
      ENDIF
C
C  check the track's position too
C
      IF (PHIRD) THEN
        PHICEN = ATAN2(YPOS, XPOS)
        PHIRD = .FALSE.
        IF (PHICEN .LE. 0.0) PHICEN = PHICEN + TWOPI
        IF (PMIN .GE. 0.0 .AND. PMAX .LE. TWOPI) THEN
          IF (PHICEN .GE. PMIN .AND. PHICEN .LE. PMAX) 
     &      PHIRD = .TRUE.
        ELSE
          IF (PMIN .LT. 0.0) THEN
            PHI1 = PHICEN - TWOPI
            IF (PHICEN .GE. 0.0 .AND. PHICEN .LE. PMAX
     &      .OR. PHI1 .GE. PMIN .AND. PHI1 .LE. 0.0) PHIRD = .TRUE.
          ENDIF
          IF (PMAX .GT. TWOPI) THEN
            PHI2 = PHICEN + TWOPI
            IF (PHICEN .GE. PMIN .AND. PHICEN .LE. TWOPI
     &      .OR. PHI2 .GE. TWOPI .AND. PHI2 .LE. PMAX) PHIRD = .TRUE.
          ENDIF
        ENDIF
      ENDIF
C
      IF (PHIRD) THEN
        THETA = QTRAK(22)
        IF (THETA.LT.0.) THETA=THETA+PI
C
C  check the track's direction in THETA
C
        IF (THETA .GE. TMIN .AND. THETA .LE. TMAX) THEN
          RPOS = SQRT(XPOS**2 + YPOS**2)
          ZPOS = Z0(HALF) - ZVTX
          THECEN = ATAN2(RPOS, ZPOS)
          IF (THECEN .LT. 0.0) THECEN = THECEN + PI
          IF (THECEN .GE. TMIN .AND. THECEN .LE. TMAX) THEN
            FDC_IN_ROAD = .TRUE.
          ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
