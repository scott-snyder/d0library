      SUBROUTINE NVROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NV,IDV)      
C------------------------------------------------------------------
C
C  Find the number of VTX tracks in a road and their ID's 
C-
C-   Inputs  : 
C-        ZVTX : vertex position in Z
C-        PHIMIN: minimum phi of the road
C-        PHIMAX: maximum phi of the road
C-        THEMIN: minimum theta of the road
C-        THEMAX: maximum theta of the road
C-   Outputs : NV:     number of VTX tracks in the road
C-             IDV(I): VTX track ID
C-
C  Daria Zieminska Feb.,  1989
C-   Updated  20-JAN-1991   Qizhong Li-Demarteau   added a choice to
C-                             including non-theta tracks for the road
C-   Updated   6-FEB-1991   Qizhong Li-Demarteau added phi road definition 
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  30-DEC-1991   Qizhong Li-Demarteau  rewritten to speed up
C-   Updated   6-FEB-1992   Qizhong Li-Demarteau  added position check and
C-                                         speed up for full_tracking case
C-   Updated  20-JUL-1992   Qizhong Li-Demarteau  added an input ZVTX for
C-                                         Z position check 
C-   Updated  21-MAY-1993   Ed Oltman  Replace THEMIN,THEMAX with road 
C-                                      center +- VTX theta resolutions
C                            
C------------------------------------------------------------------
      IMPLICIT NONE 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:PI.DEF/LIST'                             
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
      INTEGER LVTRH, PLVTXT, GZVTRH, NV, IDV(*)
      INTEGER NV2, IDV2(MAXTRK), IER
      INTEGER NLEFT
      REAL    PHIMIN, PHIMAX, THEMIN, THEMAX, PHI, THETA, ERRTHE, ZVTX
      REAL    PHI1, PHI2
      REAL    PHICEN, XPOS, YPOS
      REAL    THECEN, RPOS, ZPOS
      LOGICAL EZERROR
      LOGICAL FIRST, MATCH2D, PHIRD, FULL_TRACKING
      REAL    THERD
      LOGICAL VTXTHR
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','NVROAD',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('MATCH2D',MATCH2D,IER)
        CALL EZGET('FULL_TRACKING',FULL_TRACKING,IER)
        CALL EZRSET
      END IF
      PHIRD = .FALSE.
      IF (FULL_TRACKING) PHIRD = .TRUE.
C
      THERD = 0.5*(THEMIN + THEMAX)
      NV = 0
      NV2 = 0
      LVTRH = GZVTRH()
      IF (LVTRH .LE. 0) GO TO 999
      PLVTXT = LQ(LVTRH - 1)      
 100  IF (PLVTXT .LE. 0) GOTO 999         ! no track
      XPOS = Q(PLVTXT + 7)
      YPOS = Q(PLVTXT + 8)
      IF (FULL_TRACKING) GOTO 200
C
C  check the track's direction in PHI
C
      PHI = Q(PLVTXT + 6)
      PHIRD = .FALSE.
      IF (PHIMIN .GE. 0.0 .AND. PHIMAX .LE. TWOPI) THEN
        IF (PHI .GE. PHIMIN .AND. PHI .LE. PHIMAX) PHIRD = .TRUE.
      ELSE
        IF (PHIMIN .LT. 0.0) THEN
          PHI1 = PHI - TWOPI
          IF (PHI .GE. 0.0 .AND. PHI .LE. PHIMAX
     &      .OR. PHI1 .GE. PHIMIN .AND. PHI1 .LE. 0.0) PHIRD = .TRUE.
        ENDIF
        IF (PHIMAX .GT. TWOPI) THEN
          PHI2 = PHI + TWOPI
          IF (PHI .GE. PHIMIN .AND. PHI .LE. TWOPI
     &      .OR. PHI2 .GE. TWOPI .AND. PHI2 .LE. PHIMAX) PHIRD = .TRUE.
        ENDIF
      ENDIF
C
C  check the track's position too
C
      IF (PHIRD) THEN
        PHICEN = ATAN2(YPOS, XPOS)
        PHIRD = .FALSE.
        IF (PHICEN .LE. 0.0) PHICEN = PHICEN + TWOPI
        IF (PHIMIN .GE. 0.0 .AND. PHIMAX .LE. TWOPI) THEN
          IF (PHICEN .GE. PHIMIN .AND. PHICEN .LE. PHIMAX) 
     &      PHIRD = .TRUE.
        ELSE
          IF (PHIMIN .LT. 0.0) THEN
            PHI1 = PHICEN - TWOPI
            IF (PHICEN .GE. 0.0 .AND. PHICEN .LE. PHIMAX
     &      .OR. PHI1 .GE. PHIMIN .AND. PHI1 .LE. 0.0) PHIRD = .TRUE.
          ENDIF
          IF (PHIMAX .GT. TWOPI) THEN
            PHI2 = PHICEN + TWOPI
            IF (PHICEN .GE. PHIMIN .AND. PHICEN .LE. TWOPI
     &      .OR. PHI2 .GE. TWOPI .AND. PHI2 .LE. PHIMAX) PHIRD = .TRUE.
          ENDIF
        ENDIF
      ENDIF
C
 200  IF (PHIRD) THEN
        ERRTHE = Q(PLVTXT + 18)
        IF (ERRTHE .LT. 9.99) THEN
C
C  check the track's direction in THETA
C
          THETA = Q(PLVTXT + 9)
          IF ( VTXTHR(1,THERD,ABS(THETA-THERD)) .OR.
     &        (THETA .GE. THEMIN .AND. THETA .LE. THEMAX)) THEN
            RPOS = SQRT(XPOS**2 + YPOS**2)
            ZPOS = Q(PLVTXT + 11) - ZVTX
            THECEN = ATAN2(RPOS, ZPOS)
            IF (THECEN .LT. 0.0) THECEN = THECEN + PI
            IF (VTXTHR(2,THERD,ABS(THECEN-THERD)) .OR.
     &         (THECEN .GE. THEMIN .AND. THECEN .LE. THEMAX)) THEN
              NV = NV + 1
              IF (NV .LT. MAXTRK) THEN
                IDV(NV) = IQ(PLVTXT - 5)
              ELSE
                NV = NV - 1
              ENDIF
            ENDIF
          ENDIF
        ELSE
C
C  assume non-theta tracks are belong to this theta road, if 2_D match is
C  requested
C
          IF (MATCH2D) THEN
            NV2 = NV2 + 1
            IF (NV2 .LT. MAXTRK) IDV2(NV2) = IQ(PLVTXT - 5)
          ENDIF
        ENDIF
      ENDIF
      PLVTXT = LQ(PLVTXT)
      IF (PLVTXT .LE. 0) GOTO 101
      GOTO 100
C
C   in the array IDV: 
C             IDV(1:NV) -- VTX track IDs belong to the (phi, theta) road
C             IDV(NV+1) = -1  to separate the IDs
C             IDV(NV+2:NV+NV2+1) -- VTX non-theta track IDs belong to the
C                                   phi road
C
 101  IF (MATCH2D) THEN
        IDV(NV+1) = -1
        NLEFT = MAXTRK - NV - 1
        NLEFT = MAX(NLEFT,0)
        NV2 = MIN(NV2,NLEFT)
        CALL UCOPY(IDV2(1),IDV(NV+2),NV2)
        NV = NV + NV2 + 1
      ENDIF
C
  999 RETURN
      END
