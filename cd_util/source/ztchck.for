      SUBROUTINE ZTCHCK(PHIMIN,PHIMAX,THEMIN,THEMAX,NZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : check the previours built ZTRKs if any ZTRK
C-                         belong to this road.
C-
C-   Inputs  : 
C-           PHIMIN   = minimum phi of the road
C-           PHIMAX   = maximum phi of the road
C-           THEMIN   = minimum theta of the road
C-           THEMAX   = maximum theta of the road
C-   Outputs : NZ:  # of previours ZTRKs belong to this road
C-
C-   Created  18-AUG-1990   Qizhong Li-Demarteau
C-   Updated   7-FEB-1991   Daria Zieminska: fix to work if PHIMIN>PHIMAX 
C-                          use theta from DTRK rather than VTXT   
C-   Updated  12-FEB-1991   Qizhong Li-Demarteau added phi road definition 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZTRLNK.INC/LIST'
      INCLUDE 'D0$LINKS:IZZTRK.LINK/LIST'
      INCLUDE 'D0$LINKS:IZZFIT.LINK/LIST'
      INTEGER  LZTRH, LZTRK, LZFIT, LVTXT, LDTRK, LFDCT, GZZTRH
      INTEGER  NZ, NZTRK
      INCLUDE 'D0$INC:PI.DEF'                             
      REAL     PHIMIN, PHIMAX, THEMIN, THEMAX, PHI, THETA, CHISQ
      REAL     PHI1, PHI2
      LOGICAL  PHIRD
C----------------------------------------------------------------------
C
      LZTRH = GZZTRH()
      IF (LZTRH .LE. 0) GOTO 999
      NZTRK = IQ(LZTRH + 2)
      IF (NZTRK .LE. 0) GOTO 999
      LZTRK = LQ(LZTRH - IZZTRK)
  101 IF (LZTRK .LE. 0) GOTO 999
      LZFIT = LQ(LZTRK - IZZFIT)
      IF (LZFIT .GT. 0) THEN
        LFDCT = LQ(LZTRK - 8)                          ! temporarily use 
        CHISQ = Q(LZFIT + 8) / IQ(LZFIT+6)             ! FDCT phi & theta
        IF (LFDCT .GT. 0 .AND. CHISQ .GT. 20.0) THEN   ! for large Chi case
          PHI = Q(LFDCT + 6)                           ! until FTRKHT is
          THETA = Q(LFDCT + 22)                        ! improved
        ELSE                                           ! 
          PHI = Q(LZFIT + 10) 
          THETA = Q(LZFIT + 13)
        ENDIF
      ELSE
        LVTXT = LQ(LZTRK - 6)
        IF (LVTXT .GT. 0) THEN
          PHI = Q(LVTXT + 6)
          THETA = Q(LVTXT + 9)
C
C  Use theta from DTRK
C
          LDTRK = LQ(LZTRK - 7)
          IF (LDTRK .GT. 0) THEN
            THETA = Q(LDTRK + 9)
          END IF
C
        ELSE
          LDTRK = LQ(LZTRK - 7)
          IF (LDTRK .GT. 0) THEN
            PHI = Q(LDTRK + 6)
            THETA = Q(LDTRK + 9)
          ELSE
            LFDCT = LQ(LZTRK - 8)
            IF (LFDCT .GT. 0) THEN
              PHI = Q(LFDCT + 6)
              THETA = Q(LFDCT + 22)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C   check if it belongs to this road
C
      IF (PHIMIN.GT.PHIMAX) PHIMIN=PHIMIN-TWOPI
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
      IF (PHIRD) THEN
        IF (THETA .GT. 0.0) THEN
          IF (THETA .GE. THEMIN .AND. THETA .LE. THEMAX) THEN
            NZ = NZ + 1
            ZLINKS(NZ) = LZTRK
          ENDIF
        ENDIF
      ENDIF
      LZTRK = LQ(LZTRK)
      GOTO 101
C
  999 RETURN
      END
