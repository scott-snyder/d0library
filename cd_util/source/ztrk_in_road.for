      SUBROUTINE ZTRK_IN_ROAD
     &  (ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NZ,ZLINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : check the previours built ZTRKs if any ZTRK
C-                         belong to this road.
C-
C-   Inputs  :
C-           ZVTX     = vertex Z position used for this road
C-           PHIMIN   = minimum phi of the road
C-           PHIMAX   = maximum phi of the road
C-           THEMIN   = minimum theta of the road
C-           THEMAX   = maximum theta of the road
C-   Outputs : NZ:  # of previours ZTRKs belong to this road
C-             ZLINK(I): link of ZTRKs in the road
C-
C-   Created  18-OCT-1993   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZTRLNK.INC/LIST'
      INCLUDE 'D0$LINKS:IZZTRK.LINK/LIST'
      INCLUDE 'D0$LINKS:IZZFIT.LINK/LIST'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER  NZTRK
      INTEGER  NZ, ZLINK(ZMAX)
      INTEGER  LZTRH, LZTRK, LZFIT, LVTXT, LDTRK, LFDCT, GZZTRH
      REAL     ZVTX, PHIMIN, PHIMAX, THEMIN, THEMAX, PHI, THETA, CHISQ
      REAL     XPOS, YPOS, RPOS, ZPOS, PHICEN, THECEN, TANTHE, ZIMP
      REAL     PHI1, PHI2
      LOGICAL  PHIRD
C----------------------------------------------------------------------
C
      NZ = 0
      CALL VZERO(ZLINK,ZMAX)
      LZTRH = GZZTRH()
      IF (LZTRH .LE. 0) GOTO 999
      NZTRK = IQ(LZTRH + 2)
      IF (NZTRK .LE. 0) GOTO 999
      LZTRK = LQ(LZTRH - IZZTRK)
  101 IF (LZTRK .LE. 0) GOTO 999
      LZFIT = LQ(LZTRK - IZZFIT)
      IF (LZFIT .GT. 0) THEN
        PHI = Q(LZFIT + 10)
        THETA = Q(LZFIT + 13)
        XPOS = Q(LZFIT + 11)
        YPOS = Q(LZFIT + 12)
        RPOS = Q(LZFIT + 14)
        ZPOS = Q(LZFIT + 15) 
        CHISQ = Q(LZFIT + 8) / IQ(LZFIT+6)
      ENDIF
      IF (LZFIT .LE. 0 .OR. CHISQ .GT. 50.0) THEN
C
C    in case of the matching to VTX is too bad, use CDC/FDC information only
C   (in case of no ZFIT, use subdetector track information only)
C
        LDTRK = LQ(LZTRK - 7)
        IF (LDTRK .GT. 0) THEN
          PHI = Q(LDTRK + 6)
          THETA = Q(LDTRK + 9)
          XPOS = Q(LDTRK + 7)
          YPOS = Q(LDTRK + 8)
          ZPOS = Q(LDTRK + 11)
        ELSE
          LFDCT = LQ(LZTRK - 8)
          IF (LFDCT .GT. 0) THEN
            PHI = Q(LFDCT + 6)
            THETA = Q(LFDCT + 22)
            XPOS = Q(LFDCT + 4)
            YPOS = Q(LFDCT + 5)
            CALL FGETZ0(IQ(LFDCT-5),ZPOS)
          ELSE
            LVTXT = LQ(LZTRK - 6)
            IF (LVTXT .GT. 0) THEN
              PHI = Q(LVTXT + 6)
              THETA = Q(LVTXT + 9)
              XPOS = Q(LVTXT +7)
              YPOS = Q(LVTXT +8)
              ZPOS = Q(LVTXT +11)
            END IF
          ENDIF
        ENDIF
      ENDIF
C
C   check if it belongs to this PHI road
C
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
C    check the THETA road
C
      IF (PHIRD) THEN
        IF (THETA .GT. 0.0) THEN
          IF (THETA .GE. THEMIN .AND. THETA .LE. THEMAX) THEN
            RPOS = SQRT(XPOS**2 + YPOS**2)
            ZPOS = ZPOS - ZVTX
            THECEN = ATAN2(RPOS, ZPOS)
            IF (THECEN .LT. 0.0) THECEN = THECEN + PI
            IF (THECEN .LT. THEMIN .OR. THECEN .GT. THEMAX) THEN
              TANTHE = TAN(THETA)
              ZIMP = ZPOS - (RPOS / TANTHE)
              IF (ZIMP .GT. 10.0) GOTO 201
            ENDIF
            NZ = NZ + 1
            ZLINK(NZ) = LZTRK
          ENDIF
        ENDIF
      ENDIF
  201 LZTRK = LQ(LZTRK)
      GOTO 101
C
  999 RETURN
      END
