      SUBROUTINE L2_ZCDCGZ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  get the Z pozition along the bean line from
C-                          CDC hits and fill it into the histogram
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  27-FEB-1990   Qizhong Li-Demarteau
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  11-AUG-1992   Qizhong Li-Demarteau  changed the way of using
C-                                      DALS bank for capatibility with L2 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LAYER, SECTOR, MAXSEC, WIRE, IHIT, ISIDE, LAY, WIR
      INTEGER LDRFT, LDALS, LDSEC
      INTEGER GZDSEC, GZDRFT, GZDALS
      INTEGER NBHITS(0:31), NHTSEC, BIGDST, ONEZ, IP, ERR, IPAL, JHIT
      INTEGER IER
C
      REAL    BIGSGM, CDCRES, ITRLMT, SGMFCT, SGMFC2, ZCERMX, ZSIGMA
      REAL    STAG, CPHI, SPHI, X0SEC, Y0SEC, SLOP, INTC, YR, ZCDC
      REAL    RWIRE0, RWIRE6, ERRZ(0:2,10), DIST(2), TOLDST, ZPOSIT
      REAL    XABS(0:2,10,2), YABS(0:2,10,2), ZABS(0:2,10)
      REAL    PHI, PHI1, RAY, CPHI1
      LOGICAL FIRST, MORETK
      LOGICAL EZERROR
C
      COMMON/VERTEX_CUTS/ BIGSGM, CDCRES, ITRLMT, MORETK,
     &  SGMFCT, SGMFC2, TOLDST, ZCERMX, ZSIGMA
C
      SAVE FIRST
      DATA    FIRST/.TRUE./
C
      DATA MAXSEC/31/
C*DC* Hardwire MAXSEC at 31 or pass from L2_VERTEX_DCD_PARAMETERS
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C Need to be passed VERTEX_RCP's TOLDST,MORETK
        ZCDC = C(LDGEH + 19)  ! Limit of CDC coverage?  Hardwire?
      ENDIF
C
C   try to match hitz for each sector
C
      DO 10 SECTOR = 0, MAXSEC
        LAY = 0
        WIR = 0
        LDSEC = GZDSEC(SECTOR, LAY)
        IF (LDSEC .LE. 0) GOTO 10
        NBHITS(SECTOR) = IQ(LDSEC + 4)
        IF (NBHITS(SECTOR) .LE. 0) GOTO 10
        LDRFT = GZDRFT()
        STAG = C(LDRFT + 26 + WIR)     ! Staggering
        RWIRE0 = C(LDRFT + 11) + C(LDRFT + 19)
        RWIRE6 = C(LDRFT + 15) + C(LDRFT + 25)
        NHTSEC = MIN(10,NBHITS(SECTOR))
        BIGDST = 0
        ONEZ = 0
        CALL VZERO(XABS(0,1,1),60)
        CALL VZERO(YABS(0,1,1),60)
        CALL VZERO(ZABS(0,1),30)
        DO 20 IHIT = 1, NHTSEC
          DO 40 LAYER = 0, 2, 2
            LDALS = GZDALS(LAYER,0)
            CPHI1 = C(LDALS+3)
            PHI1 = ACOS(CPHI1)
            PHI = PHI1 + (ACOS(-1.) / 16.) * SECTOR
            CPHI = COS(PHI)
            SPHI = SIN(PHI)
            WIRE = LAYER * 3
            RAY = C(LDRFT+11+2*LAYER) + C(LDRFT+19+WIRE)
            X0SEC =   RAY * SPHI + STAG * CPHI
            Y0SEC = - RAY * CPHI + STAG * SPHI
            LDSEC = GZDSEC(SECTOR, LAYER)
            IF (LDSEC .LE. 0) GOTO 40
            IP = LDSEC + IQ(LDSEC + IQ(LDSEC + 2) + WIRE + 4)
            IP = IP + IQ(LDSEC + 3) * (IHIT - 1)
            ERRZ(LAYER,IHIT) = Q(IP + 6)
            IF (ERRZ(LAYER,IHIT) .GE. 999.9) GOTO 40
            ZABS(LAYER,IHIT) = Q(IP + 4)
            DO 30 ISIDE = 1, 2
              YR = Q(IP + ISIDE + 1) - STAG
              XABS(LAYER,IHIT,ISIDE) = X0SEC + YR * CPHI
              YABS(LAYER,IHIT,ISIDE) = Y0SEC + YR * SPHI
   30       CONTINUE
   40     CONTINUE
C  
C  calculate the impack parameter in the X-Y plane, and apply a cut to
C  make sure that it is a right track for Z position calculation
C  
          DIST(1) = 9999.0
          DIST(2) = 9999.0
          IF (ERRZ(0,IHIT).LT.999.9 .AND. ERRZ(2,IHIT).LT.999.9) THEN
            DO 31 ISIDE = 1, 2
              IF (XABS(0,IHIT,ISIDE)-XABS(2,IHIT,ISIDE) .EQ. 0) GOTO 31
              SLOP = (YABS(0,IHIT,ISIDE) - YABS(2,IHIT,ISIDE)) 
     &        / (XABS(0,IHIT,ISIDE) - XABS(2,IHIT,ISIDE))
              INTC = YABS(0,IHIT,ISIDE) - SLOP * XABS(0,IHIT,ISIDE)
              DIST(ISIDE) = ABS(INTC / SQRT(1 + SLOP**2))
   31       CONTINUE              
            IF (DIST(1) .LE. TOLDST .OR. DIST(2) .LE. TOLDST) THEN
              ZPOSIT = (ZABS(2,IHIT) * RWIRE0 - ZABS(0,IHIT) * RWIRE6) 
     &             / (RWIRE0 - RWIRE6)
              IF (ABS(ZPOSIT) .LT. ZCDC) then
C*DC*                CALL HFILL(1099,ZPOSIT,0.,1.)
                CALL L2_HFILL(1,ZPOSIT,0.,1.)
              ENDIF
              GOTO 20
            ELSE
              BIGDST = BIGDST + 1
            ENDIF
          ELSE
            IF (ERRZ(0,IHIT) .LT. 999.9 .OR. ERRZ(2,IHIT) .LT. 999.9) 
     &          ONEZ = ONEZ + 1
          ENDIF
   20   CONTINUE
        IF (MORETK .AND. (BIGDST+ONEZ) .EQ. NHTSEC) THEN
          DO 200 IHIT = 1, NHTSEC
            IF (ERRZ(0,IHIT) .LT. 999.9) THEN
              DO 300 JHIT = 1, NHTSEC
                IF (IHIT .NE. JHIT .AND. ERRZ(2,JHIT) .LT. 999.9) THEN
                  DO 301 ISIDE = 1, 2
                    DIST(ISIDE) = 9999.0
                    IF (XABS(0,IHIT,ISIDE) - XABS(2,JHIT,ISIDE) .EQ. 0) 
     &              GOTO 301
                    SLOP = (YABS(0,IHIT,ISIDE) - YABS(2,JHIT,ISIDE)) 
     &                 / (XABS(0,IHIT,ISIDE) - XABS(2,JHIT,ISIDE))
                    INTC = YABS(0,IHIT,ISIDE) - SLOP*XABS(0,IHIT,ISIDE)
                    DIST(ISIDE) = ABS(INTC / SQRT(1 + SLOP**2))
  301             CONTINUE
                  IF (DIST(1) .LE. TOLDST .OR. DIST(2) .LE. TOLDST) THEN
                    ZPOSIT = (ZABS(2,JHIT)*RWIRE0-ZABS(0,IHIT)*RWIRE6) 
     &                   / (RWIRE0 - RWIRE6)
                    IF (ABS(ZPOSIT) .LT. ZCDC) 
     &                CALL L2_HFILL(1,ZPOSIT,0.,1.)
C*DC*     &                CALL HFILL(1099,ZPOSIT,0.,1.)
                  ENDIF
                ENDIF
 300          CONTINUE
            ENDIF
 200      CONTINUE
        ENDIF
C
   10 CONTINUE
C
  999 RETURN
      END
