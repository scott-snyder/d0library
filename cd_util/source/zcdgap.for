      SUBROUTINE ZCDGAP(ZVTX,THEMIN,THEMAX,THE1,THE2,THE3,THE4)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to determine if the road is in the gap region
C-                         between CDC and FDC.
C-                         if yes, return the gap road theta limits
C-
C-   Inputs  : ZVTX: vertex position in Z
C-             THEMIN, THEMAX: theta road
C-   Outputs : 
C-             THE1: the theta minimum for the gap road (+Z)
C-             THE2: the theta maxmum for the gap road (+Z)
C-             THE3: the theta minimum for the gap road (-Z)
C-             THE4: the theta maxmum for the gap road (-Z)
C-
C-   Created  30-SEP-1991   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZDRFT.LINK'
      INCLUDE 'D0$LINKS:IZFDRT.LINK'
      INCLUDE 'D0$LINKS:IZFDPH.LINK'
      INTEGER LDRFT, LFDRT, LFDPH, GZFDPH, ERR, IER
      REAL    ZVTX, THEMIN, THEMAX
      REAL    THE1, THE2, THE3, THE4, DELTHE
      REAL    RCDC, ZCDC, THECDC1, THECDC2
      REAL    RFDC, XFDCP, YFDCP, ZFDCP, THEFDC1, THEFDC2
      REAL    XFDCN, YFDCN, ZFDCN
      REAL    RFDC1, ZFDC1
      LOGICAL GAP1, GAP2, FIRST, EZERROR
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZTRAKS',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('DELTHE_GAP',DELTHE,ERR)
        IF (ERR .NE. 0) DELTHE = 0.05
        CALL EZRSET
        IF (LDGEH .LE. 0) GOTO 999
        LDRFT = LC(LDGEH - IZDRFT)
        IF (LDRFT .LE. 0) GOTO 999
        ZCDC = C(LDGEH + 14)
        RCDC = C(LDRFT +13) + C(LDRFT + 24)
        CALL GTFALH(1,0,4,5,0,XFDCP,YFDCP,ZFDCP) 
C       returns Theta chamber Quad 4 Sec 5 SW 0 at positive side of Z
        CALL GTFALH(0,0,4,5,0,XFDCN,YFDCN,ZFDCN) 
C       returns Theta chamber Quad 4 Sec 5 SW 0 at negative side of Z
      ENDIF
C
      THE1 = 0.0
      THE2 = 0.0
      THE3 = 0.0
      THE4 = 0.0
      GAP1 = .FALSE.
      GAP2 = .FALSE.
C
      THECDC1 = ATAN(RCDC/(ZCDC-ZVTX)) + DELTHE
      THECDC2 = ATAN(RCDC/(-ZCDC-ZVTX)) + PI - DELTHE
C
      RFDC = SQRT(XFDCP**2 + YFDCP**2)
      THEFDC1 = ATAN(RFDC/(ZFDCP-ZVTX)) - DELTHE
      RFDC = SQRT(XFDCN**2 + YFDCN**2)
      THEFDC2 = ATAN(RFDC/(ZFDCN-ZVTX)) + PI + DELTHE
C
      IF (THEMAX .GT. THECDC1) THEN
        IF (THEMIN .LT. THECDC1) THEN
          GAP1 = .TRUE.
        ENDIF
      ELSE
        IF (THEMAX .GT. THEFDC1) THEN
          GAP1 = .TRUE.
        ENDIF
      ENDIF
      IF (THEMAX .GT. THEFDC2) THEN
        IF (THEMIN .LT. THEFDC2) THEN
          GAP2 = .TRUE.
        ENDIF
      ELSE
        IF (THEMAX .GT. THECDC2) THEN
          GAP2 = .TRUE.
        ENDIF
      ENDIF
C
      IF (GAP1) THEN
        THE1 = MAX(THEFDC1, THEMIN)
        THE2 = MIN(THECDC1, THEMAX)
      ENDIF
      IF (GAP2) THEN
        THE3 = MAX(THECDC2, THEMIN)
        THE4 = MIN(THEFDC2, THEMAX)
      ENDIF
C
  999 RETURN
      END
