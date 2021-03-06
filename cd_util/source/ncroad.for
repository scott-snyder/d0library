      SUBROUTINE NCROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NC,IDC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find CDC tracks in a road
C-
C-   Inputs  : 
C-        ZVTX : vertex position in Z
C-        PHIMIN: minimum phi of the road
C-        PHIMAX: maximum phi of the road
C-        THEMIN: minimum theta of the road
C-        THEMAX: maximum theta of the road
C-   Outputs : NC:     number of CDC tracks in the road
C-             IDC(I): CDC track ID
C-
C  Daria Zieminska Feb.,  1989
C-   Updated  15-DEC-1989   Qizhong Li-Demarteau   rewrite to speed up
C-   Updated  20-JAN-1991   Qizhong Li-Demarteau   added a choice to
C-                             including non-theta tracks for the road
C-   Updated  1-FEB-1991   S. Abachi     Limits on phi modified
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
      INCLUDE 'D0$INC:PI.DEF/LIST'                             
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
      INTEGER LDTRH, PLDTRK, GZDTRH, NC, IDC(*)
      INTEGER NC2, IDC2(MAXTRK), IER
      INTEGER NLEFT
      REAL    PHIMIN, PHIMAX, THEMIN, THEMAX, PHI, THETA, ERRTHE, ZVTX
      REAL    PHI1, PHI2
      REAL    PHICEN, XPOS, YPOS
      REAL    THECEN, RPOS, ZPOS
      LOGICAL EZERROR
      LOGICAL FIRST, MATCH2D, PHIRD, FULL_TRACKING
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C 
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','NCROAD',
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
      NC = 0
      NC2 = 0
      LDTRH = GZDTRH()
      IF(LDTRH.LE.0)GO TO 999
      PLDTRK = LQ(LDTRH - 1)      
 100  IF (PLDTRK .LE. 0) GOTO 999         ! no full track
      XPOS = Q(PLDTRK + 7)
      YPOS = Q(PLDTRK + 8)
      IF (FULL_TRACKING) GOTO 200
C
C  check the track's direction in PHI
C
      PHI = Q(PLDTRK + 6)
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
        ERRTHE = Q(PLDTRK + 18)
        IF (ERRTHE .LT. 9.99) THEN
C
C  check the track's direction in THETA
C
          THETA = Q(PLDTRK + 9)
          IF (THETA .GE. THEMIN .AND. THETA .LE. THEMAX) THEN
            RPOS = Q(PLDTRK + 10)
            ZPOS = Q(PLDTRK + 11) - ZVTX
            THECEN = ATAN2(RPOS, ZPOS)
            IF (THECEN .LT. 0.0) THECEN = THECEN + PI
            IF (THECEN .GE. THEMIN .AND. THECEN .LE. THEMAX) THEN
              NC = NC + 1
              IF (NC .LT. MAXTRK) THEN
                IDC(NC) = IQ(PLDTRK - 5)
              ELSE
                NC = NC - 1
              ENDIF
            ENDIF
          ENDIF
        ELSE
C
C  assume non-theta tracks are belong to this theta road, if 2_D match is
C  requested
C
          IF (MATCH2D) THEN
            NC2 = NC2 + 1
            IF (NC2 .LT. MAXTRK) IDC2(NC2) = IQ(PLDTRK - 5)
          ENDIF
        ENDIF
      ENDIF
      PLDTRK = LQ(PLDTRK)
      IF (PLDTRK .LE. 0) GOTO 101
      GOTO 100
C
C   in the array IDC: 
C             IDC(1:NC) -- CDC track IDs belong to the (phi, theta) road
C             IDC(NC+1) = -1  to separate the IDs
C             IDC(NC+2:NC+NC2+1) -- CDC non-theta track IDs belong to the
C                                   phi road
C
 101  IF (MATCH2D) THEN
        IDC(NC+1) = -1
        NLEFT = MAXTRK - NC - 1
        NLEFT = MAX(NLEFT,0)
        NC2 = MIN(NC2,NLEFT)
        CALL UCOPY(IDC2(1),IDC(NC+2),NC2)
        NC = NC + NC2 + 1
      ENDIF
C
  999 RETURN
      END
