      SUBROUTINE ZSEPRT(ZVERTX,THETCV,THETFV,THETFF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to calculate the theta boundaries for CV
C-                         road, FV road and FF road from the vertex
C-                         position
C-
C-   Inputs  : ZVERTX: vertex position in Z
C-   Outputs : THETCV: CV road is between THETCV(1) and THETCV(2)
C-             THETFV: FV road is between THETFV(1) and THETCV(1)
C-                                        THETCV(2) and THETFV(2)
C-             THETFF: when THETFF(1) < theta < THETFV(1) or 
C-                          THETFV(2) < theta < THETFF(2), use FDC only
C-
C-   Created  20-NOV-1989   Qizhong Li-Demarteau
C-   Updated  13-JAN-1990   Qizhong Li-Demarteau  use ZVERTX from argument
C-   Updated   9-JUN-1992   Qizhong Li-Demarteau  use ATAN2 to avoid problem
C-                                                at PI/2 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZVGEH.LINK'
      INTEGER LVERH, LVERT, LDRFT
      INTEGER GZVERH, GZVERT, GZDGEH, GZVGEH, GZFGEH
      REAL    THETCV(2), THETFV(2), THETFF(2)
      REAL    ZVERTX, YCDC, ZCDC, YVTX, ZVTX, YFDC, ZFDC, DIM(6)
      LOGICAL VERTEX
C----------------------------------------------------------------------
C
C  calculate the boundaries between CV road and the FV road
C
  101 LDGEH = GZDGEH()
      IF (LDGEH .GT. 0) THEN
        ZCDC = C(LDGEH + 19)
        LDRFT = LC(LDGEH - 3)
        IF (LDRFT .GT. 0) THEN
          YCDC = C(LDRFT + 13) + C(LDRFT + 8)
          THETCV(1) = ATAN2(YCDC, ABS(-ZCDC + ZVERTX))
          THETCV(2) = PI - ATAN2(YCDC, ABS(ZCDC + ZVERTX))
        ELSE
          THETCV(1) = 0.625
          THETCV(2) = 2.516 
        ENDIF
      ELSE
        THETCV(1) = 0.625
        THETCV(2) = 2.516 
      ENDIF
C
C  calculate the boundaries between FV road and the FF road
C
C      LVGEH = GZVGEH()
      IF (LSVTX .GT. 0 .AND. LVGEH .LE. 0) LVGEH = LQ(LSVTX - IZVGEH)
      IF (LSVTX .LE. 0 .OR. LVGEH .LE. 0) THEN
        THETFV(1) = 0.111
        THETFV(2) = 3.030
      ELSE
        YVTX = C(LVGEH + 11) + (C(LVGEH + 12) - C(LVGEH + 11)) / 2.0
        ZVTX = C(LVGEH + 17)
        THETFV(1) = ATAN2(YVTX, ABS(-ZVTX + ZVERTX))
        THETFV(2) = PI - ATAN2(YVTX, ABS(ZVTX + ZVERTX))
      ENDIF
C
C  calculate the boundaries for the FF road
C
      LFGEH = GZFGEH()
      IF (LFGEH .LE. 0) THEN
        THETFF(1) = 0.086
        THETFF(2) = 3.055
      ELSE
        ZFDC = C(LFGEH + 8) + C(LFGEH + 5)
        CALL GTFWAL(2,DIM)
        YFDC = DIM(1)
        THETFF(1) = ATAN2(YFDC, ABS(-ZFDC + ZVERTX))
        THETFF(2) = PI - ATAN2(YFDC, ABS(ZFDC + ZVERTX))
      ENDIF
C
  999 RETURN
      END
