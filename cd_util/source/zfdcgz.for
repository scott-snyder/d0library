      SUBROUTINE ZFDCGZ(ONEZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  get the Z pozition along the beam line from
C-                          FDC hits and fill it into the histogram
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  13-SEP-1990   Jeffrey Bantly
C-   Updated  30-JUL-1991   Jeffrey Bantly  moved MXHALF,MXQUAD,MXSECT to 
C-                                          FDPARA.PARAMS
C-   Updated  14-AUG-1991   Susan K. Blessing  Add EZRSET, remove reference
C-    to FDSPEC.INC and pass ONEZ in call.
C-   Updated  20-MAR-1992   Susan K. Blessing  Remove machine block.  Replace
C-    SIND and COSD functions with SIN and COS (degrees to radians).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER HALF, QUAD, SECTOR, MAXSEC, WIRE, IHIT, ISIDE
      INTEGER LKFTSC, GZFTSC
      INTEGER NBHITS(0:7,0:5), BIGDST,MISSZ,IP,ERR,JHIT,JSIDE
      INTEGER QUADTB(0:7),QUADTE(0:7),SECTT(2,0:5,2),SECMIN,SECMAX
      INTEGER QUADO, SECTORO, NHTSEC, NHIT
      INTEGER QUAD_TYPE, MINSEC, FSECMIN, FSECMAX, NHITS
      INTEGER ONEZ(0:1)
C
      REAL    STAG, SLOP, INTC, DRIFTD, ZFDC
      REAL    RWIREI, RWIREO, DIST, TOLDST, ZPOSIT
      REAL    XABS(0:7,0:5,10,0:1), YABS(0:7,0:5,10,0:1)
      REAL    DEL(0:7,0:5,10), ERRDL, DELAY
      REAL    THETA, PHI, CDRIFT, SDRIFT, SECDIR
      REAL    FSTAGR, XC, YC, ZC, ZIN, ZOUT, XI, XO, YI, YO
C
      LOGICAL FIRST,FHALF
C
      DATA FIRST/.TRUE./
      DATA QUADTB / 4,5,6,7,3,0,1,2/
      DATA QUADTE / 5,6,7,4,0,1,2,3/
      DATA SECTT  / 0,1,0,3,1,3,1,5,3,5,3,5,    ! A Quads
     &              0,1,0,2,1,3,2,4,3,5,3,5/    ! B Quads
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')       ! Get FDC vertex-finding params
        CALL EZGET('FTOLDST',TOLDST,ERR)
        CALL EZGET('FSECMIN',FSECMIN,ERR)
        CALL EZGET('FSECMAX',FSECMAX,ERR)
        CALL EZGET('ZFDCLM',ZFDC,ERR)
C        ZFDC = C(LFGEH + 8) - C(LFGEH + 5)
        CALL EZGET('FHALF',FHALF,ERR)
        CALL EZRSET
      ENDIF
C
      WIRE = 0
      DO 2 HALF = 0, MXHALF             ! Do one FDC Half at a time
C
        STAG = FSTAGR(HALF,0,0,0,WIRE)          ! Staggering
        CALL GTFALH(HALF,0,0,0,0,XC,YC,ZIN)     ! Locate Z of inner SW0
        CALL GTFALH(HALF,0,4,0,0,XC,YC,ZOUT)    ! Locate Z of outer SW0
        CALL VZERO(NBHITS(0,0),48)
        CALL VZERO(XABS(0,0,1,0),960)     ! is there time to do this?
        CALL VZERO(YABS(0,0,1,0),960)     ! is there time to do this?
        ONEZ(HALF) = 0
        MISSZ = 0
        BIGDST = 0
C
C
C   Accumulate hits for each sector
C
        DO 5 QUAD = 0, MXQUAD
          MINSEC = 0
          MAXSEC = MXSECT
          IF(QUAD.LE.3) THEN
            MAXSEC = FSECMAX
            MINSEC = FSECMIN
          ENDIF
          DO 10 SECTOR = MINSEC, MAXSEC
            SECDIR=1.
            IF(SECTOR.EQ.1) SECDIR=-1.
            LKFTSC = GZFTSC(HALF,QUAD,SECTOR)
            IF(LKFTSC.LE.5) GOTO 10
            NHTSEC = IQ(LKFTSC + 4) ! Same as number hits on wire 0
            IF (NHTSEC .LE. 0) GOTO 10
            CALL GTFALH(HALF,0,QUAD,SECTOR,WIRE,XC,YC,ZC)
            NHTSEC = MIN(10,NHTSEC)
            IF(QUAD.LE.3) THEN
              PHI=45.+90.*FLOAT(QUAD)
            ELSE
              PHI=90.*(QUAD-4)
            ENDIF
            IF(SECTOR.EQ.1) PHI=PHI+180.
            CDRIFT=COS(PHI*RADIAN)
            SDRIFT=SIN(PHI*RADIAN)
            NHIT = 0
            NHITS = 0
            DO 20 IHIT = 1, NHTSEC
              IP = LKFTSC + IQ(LKFTSC + IQ(LKFTSC + 2) + WIRE + 4)
              IP = IP + IQ(LKFTSC + 3) * (IHIT - 1) - 1
              ERRDL = Q(IP+6)
              NHITS = NHITS + 1
              IF(ERRDL .GT. 10.) GOTO 20
              NHIT = NHIT + 1
              DELAY = Q(IP+4)
              DEL(QUAD,SECTOR,IHIT)=DELAY
              DO 30 ISIDE = 0, 1
                DRIFTD = Q(IP + ISIDE + 2) - STAG
                IF(SECTOR.LE.2 .AND. ISIDE.EQ.1) DRIFTD=9999.
                XABS(QUAD,SECTOR,IHIT,ISIDE) = XC + DRIFTD * CDRIFT +
     &                           SECDIR*(DELAY*(-SDRIFT))
                YABS(QUAD,SECTOR,IHIT,ISIDE) = YC + DRIFTD * SDRIFT +
     &                           SECDIR*(DELAY*CDRIFT)
   30         CONTINUE                  ! End of LR side loop.
   20       CONTINUE                    ! End of IHIT loop.
            NBHITS(QUAD,SECTOR) = NHIT
   10     CONTINUE                      ! End of SECTOR loop.
    5   CONTINUE                        ! End of QUAD loop.
C
C  calculate the impact parameter in the X-Y plane, and apply a cut to
C  make sure that it is a right track for Z position calculation
C
        DIST = 9999.0
        DO 105 QUAD=0,3
          QUAD_TYPE=2
          IF(QUAD.EQ.1 .OR. QUAD.EQ.3) QUAD_TYPE=1
          DO 110 SECTOR=1,4             ! Only look where it makes sense.
            SECMIN=SECTT(1,SECTOR,QUAD_TYPE)
            SECMAX=SECTT(2,SECTOR,QUAD_TYPE)
            DO 120 IHIT=1,NBHITS(QUAD,SECTOR)
              QUADO=QUADTB(QUAD)
              IF(DEL(QUAD,SECTOR,IHIT) .GT. 0.) QUADO=QUADTE(QUAD)
              DO 130 ISIDE = 0, 1
                XI=XABS(QUAD,SECTOR,IHIT,ISIDE)
                YI=YABS(QUAD,SECTOR,IHIT,ISIDE)
                DO 135 SECTORO=SECMIN,SECMAX
                  DO 137 JHIT=1,NBHITS(QUADO,SECTORO)
C                    IF(DEL(QUAD,SECTOR,IHIT)*DEL(QUADO,SECTORO,JHIT)
C     &                .GE. 0.0) GOTO 137        ! want opposite sign for
C     &                                   ! overlap of rotated quadrants.
                    DO 140 JSIDE = 0, 1
                      XO=XABS(QUADO,SECTORO,JHIT,JSIDE)
                      YO=YABS(QUADO,SECTORO,JHIT,JSIDE)
                      IF( (XO-XI) .EQ. 0.0) GOTO 140
                      SLOP = ( YO - YI ) / ( XO - XI )
                      INTC = YI - SLOP * XI
                      DIST = ABS(INTC / SQRT(1 + SLOP**2))
                      IF (DIST .LE. TOLDST) THEN
                        RWIREI = SQRT(XI**2. + YI**2.)
                        RWIREO = SQRT(XO**2. + YO**2.)
                        IF ((RWIREO-RWIREI).LE. 0.0) GOTO 140
                        ZPOSIT = (ZOUT * RWIREI - ZIN * RWIREO) /
     &                                            (RWIREI - RWIREO)
                        IF(ABS(ZPOSIT).LT.ZFDC) THEN
                          IF(FHALF) THEN        ! Seperate FDC Halves
                            CALL HF1(1096+HALF,ZPOSIT,1.)
                          ELSE
                            CALL HF1(1096,ZPOSIT,1.)
                          ENDIF
                          ONEZ(HALF) = ONEZ(HALF) + 1
                        ELSE
                          MISSZ = MISSZ + 1
                        ENDIF
                        GOTO 120
                      ELSE
                        BIGDST = BIGDST + 1
                      ENDIF
  140               CONTINUE            ! End of JSIDE loop.
  137             CONTINUE              ! End of JHIT loop.
  135           CONTINUE                ! End of SECTORO loop.
  130         CONTINUE                  ! End of ISIDE loop.
  120       CONTINUE                    ! End of IHIT loop.
  110     CONTINUE                      ! End of SECTOR loop.
  105   CONTINUE                        ! End of QUAD loop.
C
    2 CONTINUE                          ! End of HALF loop.
C---------------------------------------------------------------------------
  999 RETURN
      END
