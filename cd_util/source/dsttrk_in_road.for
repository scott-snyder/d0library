      SUBROUTINE DSTTRK_IN_ROAD
     &  (ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NTRK,TRKLNK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find CD tracks in a road from a DST file
C-                         First build DTRKs in the road to ZTRKs if they
C-                         were not built to ZTRK before, then, search all 
C-                         ZTRKs in the road.
C-
C-   Inputs  :
C-           ZVTX     = vertex Z position used for this road
C-           PHIMIN   = minimum phi of the road
C-           PHIMAX   = maximum phi of the road
C-           THEMIN   = minimum theta of the road
C-           THEMAX   = maximum theta of the road
C-   Outputs : NTRK:  # of previours ZTRKs belong to this road
C-             TRKLNK(I): link of ZTRKs in the road
C-
C-   Created  16-OCT-1995   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
      INCLUDE 'D0$INC:ZTRLNK.INC/LIST'
      INTEGER NTRK, TRKLNK(ZMAX)
      INTEGER LDTRK, LDTRH, GZDTRH, ITRK, NC, IDC(MAXTRK)
      INTEGER LZFIND
      REAL    ZVTX, PHIMIN, PHIMAX, THEMIN, THEMAX
C
C----------------------------------------------------------------------
C
      CALL NCROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NC,IDC)
      IF (NC .GT. 0) THEN
        LDTRH = GZDTRH()
        IF (LDTRH .LE. 0) GOTO 100
        LDTRK = LQ(LDTRH-1)
        IF (LDTRK .LE. 0) GOTO 100
        DO 200 ITRK = 1, NC
          LDTRK = LZFIND(IXCOM,LDTRK,IDC(ITRK),-5)
          IF (LQ(LDTRK-2) .LE. 0) THEN  ! no privious ZTRK, rebuild ZTRK
            CALL DTRK_TO_ZTRK(ZVTX,0,LDTRK)
          ENDIF
  200   CONTINUE
      ENDIF
C
C search all ZTRKs for this road
C
  100 CALL ZTRK_IN_ROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NTRK,TRKLNK)
C
  999 RETURN
      END
