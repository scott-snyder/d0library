      SUBROUTINE ZTR_CONE(IV, ZVTX, ETA, PHI, DR, PT, NZTR, ZLINKR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To find the number of CD tracks in a cone. Could
C-                         be used for isolation determination.
C-
C-   Inputs  : IV         If IV=1 then ZVTX is given by user, IV=0 then
C-                        ZVTX is calculated (Using ZVERTE).
C-           : ZVTX       Coordinates of event vertex.
C-           : ETA, PHI   Eta & Phi of the cone axis.
C-           : DR         Cone size (Delta-R of the cone).
C-           : PT         Pt of the track.
C-
C-   Outputs : NZTR       Number of ZTRAKS found
C-             ZLINKR     Link to ZTRK for the tracks found
C-   Controls:
C-
C-   Created  20-MAR-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IV, NZTR, ZLINKR(250)
      REAL ETA, PHI, DR, PT
C
      INTEGER I,NVER
      REAL ZVER(10),DZVER(10),ZVTX(3),DZV
      REAL PHIMIN,PHIMAX,THEMIN,THEMAX,ETAMIN,ETAMAX
C
      IF(IV .EQ. 0) THEN
        CALL ZVERTE(NVER,ZVER,DZVER)
        ZVTX(3) = ZVER(1)
        DZV = DZVER(1)
        IF(NVER.GE.1) THEN
          DO I=2,NVER
            IF(DZVER(I) .LT. DZV) THEN
              ZVTX(3) = ZVER(I)
              DZV = DZVER(I)
            ENDIF
          ENDDO
        ENDIF
        DO I=2,3
          ZVTX(I) = 0.0
        ENDDO
      ENDIF
C
      PHIMIN = PHI - DR
      PHIMAX = PHI + DR
      ETAMIN = ETA - DR
      ETAMAX = ETA + DR
      THEMIN = 2.* ATAN(EXP(-ETAMIN))
      THEMAX = 2.* ATAN(EXP(-ETAMAX))
C
      CALL ZTRAKS(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,PT,NZTR,ZLINKR)
C
C----------------------------------------------------------------------
  999 RETURN
      END
