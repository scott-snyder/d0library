      SUBROUTINE NFROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NF,IDF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find FDC tracks in a road
C-
C-   Inputs  : 
C-        ZVTX : vertex position in Z
C-        PHIMIN: minimum phi of the road
C-        PHIMAX: maximum phi of the road
C-        THEMIN: minimum theta of the road
C-        THEMAX: maximum theta of the road
C-   Outputs : NF:     number of FDC tracks in the road
C-             IDF(I): FDC track ID
C-
C  Daria Zieminska Feb.,  1989
C-   Updated  05-APR-1989   Qizhong Li-Demarteau   rewrite to speed up
C-   Updated  20-JAN-1991   Qizhong Li-Demarteau   added a choice to
C-                             including non-theta tracks for the road
C-   Updated   6-FEB-1991   Qizhong Li-Demarteau added phi road definition 
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated   6-FEB-1992   Qizhong Li-Demarteau  added position check and
C-                                         speed up for full_tracking case
C-   Updated  20-JUL-1992   Qizhong Li-Demarteau  added an input ZVTX for
C-                                         Z position check 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:PI.DEF'                             
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
      INTEGER LFTRH, PLFDCT, GZFTRH, NF, IDF(*), NF2, IDF2(MAXTRK), IER
      INTEGER LRCP
      REAL    PHIMIN, PHIMAX, THEMIN, THEMAX, PHI, THETA, ZVTX
      REAL    PHI1, PHI2
      REAL    PHICEN, XPOS, YPOS
      REAL    THECEN, RPOS, ZPOS
      LOGICAL FIRST, MATCH2D, PHIRD, FULL_TRACKING, INROAD
      SAVE FIRST, MATCH2D, FULL_TRACKING
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZLOC('ZTRAKS_RCP',LRCP)                        
        IF(LRCP.GT.0) THEN                                   
          CALL EZPICK('ZTRAKS_RCP')
          CALL EZGET('MATCH2D',MATCH2D,IER)
          CALL EZGET('FULL_TRACKING',FULL_TRACKING,IER)
          CALL EZRSET
        ELSE                            ! Called from FTRAKS
          MATCH2D = .FALSE.
          FULL_TRACKING = .TRUE.
        ENDIF
      END IF
      INROAD = .FALSE.
      IF (FULL_TRACKING) INROAD = .TRUE.
C
      NF = 0
      NF2 = 0
      LFTRH = GZFTRH()
      IF (LFTRH.EQ.0) GO TO 999
      PLFDCT = LQ(LFTRH - 1)      
 100  IF (PLFDCT .LE. 0) GOTO 999         ! no full track
      XPOS = Q(PLFDCT + 4)
      YPOS = Q(PLFDCT + 5)
      IF (FULL_TRACKING) GOTO 200
      INROAD = .FALSE.
C
C  check the track's direction in PHI
C
      PHI = Q(PLFDCT + 6)
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
      IF (PHIRD) THEN
        THETA = Q(PLFDCT + 22)
        IF (THETA.LT.0.) THETA=THETA+PI
C
C  check the track's direction in THETA
C
        IF (THETA .GE. THEMIN .AND. THETA .LE. THEMAX) THEN
          RPOS = SQRT(XPOS**2 + YPOS**2)
          CALL FGETZ0(IQ(PLFDCT-5),ZPOS)
          ZPOS = ZPOS - ZVTX
          THECEN = ATAN2(RPOS, ZPOS)
          IF (THECEN .LT. 0.0) THECEN = THECEN + PI
          IF (THECEN .GE. THEMIN .AND. THECEN .LE. THEMAX) THEN
            INROAD = .TRUE.
          ENDIF
        ENDIF
      ENDIF
 200  IF (INROAD) THEN
        NF = NF + 1
        IF (NF .LT. MAXTRK) THEN
          IDF(NF) = IQ(PLFDCT - 5)
        ELSE
          NF = NF -1
        ENDIF
      ENDIF
      PLFDCT = LQ(PLFDCT)
      IF (PLFDCT .LE. 0) GOTO 101
      GOTO 100
C
C   in the array IDF: 
C        IDF(1:NF) -- FDC track IDs belong to the (phi, theta) road
C        IDF(NF+1) = -1  to separate the IDs (to be used in 2D match only)
C
 101  IF (MATCH2D) THEN
        IDF(NF+1) = -1
      ENDIF
C
  999 RETURN
      END
