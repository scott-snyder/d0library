      SUBROUTINE FXTSEG(HALF,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Form roads with remaining hits across
C-                         Theta sector boundaries
C-
C-   Inputs  : HALF,QUAD,SECTOR - Identify which Theta sector being checked
C-
C-   Created   7-NOV-1989   Jeffrey Bantly
C-   Updated  20-MAR-1990   Jeffrey Bantly  general cleanup
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS files 
C-   Updated  16-MAY-1991   Susan K. Blessing  Add NDELAY for number of 
C-    delay line hits and pass to LDTSEG.
C-   Updated  12-JUN-1991   Susan K. Blessing  Add MAX_XT_SLOPE from RCP file
C-    to cut out cross sector segments with very high slope (junk).
C-   Updated  17-JUN-1991   Susan K. Blessing  Add SLOPE,INTER,CHISQ from
C-    segment fit.  Add AVE_ION for average ionization/hit.
C-   Updated  25-JUN-1991   Susan K. Blessing  Make MAXCHI an RCP parameter.
C-   Updated  17-OCT-1991   Robert E. Avery  Add Position Error,
C-    Delay Line Position, and Delay Line Error to segement bank.
C-    Include slope dependent corrections to position and error.
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated  21-FEB-1992   Susan K. Blessing  Change call to THITAC 
C-    to include beginning and ending wires of interest (0 to NBTSEN-1 
C-    here). 
C-   Updated  21-APR-1992   Susan K. Blessing  Set NDELAY=0 when first
C-    hit isn't on wire 0.  DELAY, EDELAY and NDELAY should come from 
C-    the hit being used, not the first hit on wire 0.
C-   Updated  30-JUN-1992   Tacy M. Joffe-Minor  Change to HITS_PER_WIRE
C-    instead of MX_HIT_WIRE to allow for more hits 
C-   Updated   7-JUL-1992   Robert E. Avery  Include dependence of
C-    error on number hits in sector.
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C-   Updated  18-OCT-1993   Robert E. Avery  Eliminate unnecessary 
C-    QHIT array (~3000 words).
C-   Updated   9-NOV-1993   Robert E. Avery  Do not use pointers to 
C-    DA banks to test for delay lines. Use status bits instead. 
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER HALF,QUAD,SECTOR,SECTDR
      INTEGER STAT1 
      INTEGER NHITS(0:NBTSEN-1),IPTR(0:NBTSEN-1),DIR
      INTEGER IIWIRE(NBTSEN),LRWIR(NBTSEN),IIHIT(NBTSEN)
      INTEGER HITLOC(NBTSEN),NHIT,NHITS1,SECTD1
      INTEGER I1,I2,J1,J2,IWIRE,IHIT,JWIRE,JHIT,LR,LR1,NEL,NWORDS
      INTEGER NN,ICALL,II,JJ,TMPI(NBTSEN),JH,IH,ID
      INTEGER LOC(0:NBTSEN-1,MX_HIT_WIRE*2,3),IHITD,JWIRED,JWIREH
      INTEGER DLOC(0:NBTSEN-1,MX_HIT_WIRE*2,3)
      INTEGER TLOC(NBTSEN,MX_HIT_WIRE*2,3)
      INTEGER TIMAX(NBTSEN),TIHMAX(NBTSEN),TIDMAX(NBTSEN)
      INTEGER HOHITS(0:NBTSEN-1),D1HITS(0:NBTSEN-1)
      INTEGER INEFF,IER,NOFF,MIN,WIRBEG,WIREND,IHITST,IHITEN,DUM
      INTEGER NLVL,MAXLVL,I(NBTSEN),IMAX(NBTSEN)
      INTEGER LBANK,LFACE,LWIRE,LBANKD1,LBANKP1
      INTEGER LFDTA,LOGCHA,LOGCHAD1
      INTEGER GZFDTA,GZFACE,GZFTSC
      INTEGER NDELAY
      INTEGER NMISS
      INTEGER HITS_PER_WIRE
      INTEGER STATUS 
C
      REAL    MAX_XT_SLOPE
      REAL    CONT(18),RESID(16),TMPRES(16)
      REAL    A,B,CHISQ,BESCHI,MAXCHI,DELAY,EDELAY
      REAL    SLOPE,INTER,AVE_ION
      REAL    YL(0:NBTSEN-1,MX_HIT_WIRE*2), ZL(0:NBTSEN-1,MX_HIT_WIRE*2)
      REAL    DYL(0:NBTSEN-1,MX_HIT_WIRE*2)
      REAL    DZL(0:NBTSEN-1,MX_HIT_WIRE*2)
      REAL    TYL(NBTSEN,MX_HIT_WIRE*2),TZL(NBTSEN,MX_HIT_WIRE*2)
      REAL    X(16),Y(16)
      REAL    XC,YC,ZC,RC,RHIT,ENDY,ENDZ,SLOPEM,INTERB,ANGLE
      REAL    PHI1,PHI2
      REAL    PHI,THETA
      REAL    DISTANCE
      REAL    LINEPOS,TOLXDI,RADIUS,STAGGER
      REAL    ERROR, SHIFT(0:1)
      REAL    FDC_DRIFT_SLOPE
      REAL    FDC_ERROR_SLOPE
      REAL    ERROR_FACTOR, ERRF_XSECT 
      REAL    DL_DIST, DL_ERROR 
      REAL    AVE_HIT_WIRE 
      REAL    FDC_ERROR_NHIT
C
      LOGICAL ENOUGH
C
      SAVE ICALL,INEFF,TOLXDI,MAX_XT_SLOPE,MAXCHI,ERRF_XSECT 
C
      DATA ICALL/0/
      DATA MAX_XT_SLOPE/10./
      DATA MAXCHI/100000./
      DATA ERRF_XSECT /1.0/
C
C----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        ICALL = 1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('TINEFF',INEFF,IER)
        CALL EZGET('TOLXDI',TOLXDI,IER)
        CALL EZGET('MAX_XT_SLOPE',MAX_XT_SLOPE,IER)
        CALL EZGET('MAX_XTSEG_CHI',MAXCHI,IER)
        CALL EZGET('ERRF_XSECT_THETA',ERRF_XSECT ,IER)
        CALL EZGET('HITS_PER_WIRE',HITS_PER_WIRE,IER)
        CALL EZRSET
      END IF
C
C   Loop over the two possiblities, matching with the inner sector(-1)
C   and matching with the outer sector(+1).
C
      DO 10 DIR = -1,1,2
        SECTD1 = SECTOR + DIR
C
C   Check to see if the sector near the current one has hits
C   for cross-sector segments.
C
        IF (SECTD1.LT.0 .OR. SECTD1.GT.5) THEN
          GOTO 10
        ELSE
          LBANK = GZFTSC(HALF,QUAD,SECTD1)
          STAT1 = IQ(LBANK)
          IF (.NOT.BTEST(STAT1,ION)) GOTO 10
          NHITS1 = IQ(LBANK+1)
          IF (NHITS1.LT.1) GOTO 10
        ENDIF
C
C   Check to see if at least one hit available in current sector to
C   start road.
C
        CALL GTFTSC(HALF,QUAD,SECTOR,'WIR',0,NEL,NWORDS,CONT)
        CALL UCOPY(CONT,NHITS,NEL)
        CALL UCOPY(CONT(NEL+1),IPTR,NEL)
        NHITS1 = 0
        DO 5 II = 0,INEFF
          NHITS1 = NHITS1 + NHITS(II)
    5   CONTINUE
        IF (NHITS1 .LE. 0) GOTO 999      ! no hits to start road
  500   CONTINUE
C
C  Accumulate unused hits from beginning 'home' sector.
C
        CALL THITAC(HALF,QUAD,SECTOR,0,NBTSEN-1,YL,ZL,LOC,HOHITS,ENOUGH)
C
C  Accumulate all unused hits from neighboring sector
C
        CALL GTFTSC(HALF,QUAD,SECTD1,'WIR',0,NEL,NWORDS,CONT)
        CALL UCOPY(CONT,NHITS,NEL)
        CALL UCOPY(CONT(NEL+1),IPTR,NEL)
        NHITS1 = 0
        DO 205 II = NBTSEN-1,NBTSEN-1-INEFF,-1
          NHITS1 = NHITS1 + NHITS(II)
  205   CONTINUE
        IF (NHITS1 .LE. 0) GOTO 10      ! no hits to end road
C
C  Accumulate unused hits from 'minus 1' sector.
C
        CALL THITAC(HALF,QUAD,SECTD1,0,NBTSEN-1,
     &    DYL,DZL,DLOC,D1HITS,ENOUGH)
C
C  Form segments across sector boundaries by choosing beginning wire and
C  ending wire in different, adjacent sectors and making a road.
C
        JWIREH = 0
        JWIRED = 0
        WIRBEG = NBTSEN
        WIREND = -1
        DO 110 IWIRE = NBTSEN-1,0,-1
          IF (HOHITS(IWIRE) .GT. 0) THEN
            JWIREH = JWIREH + 1
            IF (IWIRE .LE. INEFF) WIRBEG = IWIRE
          ENDIF
  110   CONTINUE
        DO 120 IWIRE = 0,NBTSEN-1
          IF (D1HITS(IWIRE) .GT. 0) THEN
            JWIRED = JWIRED + 1
            IF (IWIRE.GE.NBTSEN-1-INEFF) WIREND = IWIRE
          ENDIF
  120   CONTINUE
        IF (WIREND-WIRBEG+1 .LE. NBTSEN-1-INEFF) GOTO 10
        IF (JWIREH+JWIRED .LT. NBTSEN-1-INEFF) GOTO 10
C
C  Loop over every unused hit in beginning wire, allowing only one possible
C  hit at this level.
C
        DO 300 IHITST = 1,HOHITS(WIRBEG)
C
C  Loop over every unused hit in ending wire, allowing only one possible
C  hit at this level.
C
          DO 400 IHITEN = 1,D1HITS(WIREND)
            CALL VZERO(TIMAX,NBTSEN)
            CALL VZERO(TIHMAX,NBTSEN)
            CALL VZERO(TIDMAX,NBTSEN)
            JWIRE = 1
            JHIT = 1
            TYL(JWIRE,JHIT) = YL(WIRBEG,IHITST)
            TZL(JWIRE,JHIT) = ZL(WIRBEG,IHITST)
            TLOC(JWIRE,JHIT,1) = LOC(WIRBEG,IHITST,1)
            TLOC(JWIRE,JHIT,2) = LOC(WIRBEG,IHITST,2)
            TLOC(JWIRE,JHIT,3) = LOC(WIRBEG,IHITST,3)
            TIHMAX(JWIRE) = JHIT
            TIMAX(JWIRE) = JHIT
            ENDY = DYL(WIREND,IHITEN)
            ENDZ = DZL(WIREND,IHITEN)
            IF ((ENDZ-TZL(1,1)) .EQ. 0.) GOTO 400
            SLOPEM = (ENDY-TYL(1,1))/(ENDZ-TZL(1,1))
C
            IF (ABS(SLOPEM).GT.MAX_XT_SLOPE) GO TO 400
C
            INTERB = ENDY-(SLOPEM*ENDZ)
            ANGLE = ATAN2((ENDY-TYL(1,1)),(ENDZ-TZL(1,1)))
            SHIFT(0) = FDC_DRIFT_SLOPE(SLOPEM,0)
            SHIFT(1) = FDC_DRIFT_SLOPE(SLOPEM,1)
C
C  Loop over intervening wires
C
C Count number of missed/unused wires
            NMISS = NBTSEN - (WIREND-WIRBEG+1)
            DO 450 IWIRE = WIRBEG+1,WIREND-1
              IF (HOHITS(IWIRE) .GT. 0)
     &                            LINEPOS = (SLOPEM*ZL(IWIRE,1))+INTERB
C
C  Loop over all unused hits in the intervening wires in the home sector,
C  allowing as many hits as available at each level that are within
C  TOLXDI of the road center.
C
              JHIT = 0
              DO 475 IHIT = 1,MIN(HITS_PER_WIRE,HOHITS(IWIRE))
                DISTANCE = ABS((LINEPOS-YL(IWIRE,IHIT))*COS(ANGLE))
                IF (DISTANCE .LE. TOLXDI) THEN
                  IF (JHIT .EQ. 0) JWIRE = JWIRE+1
                  JHIT = JHIT+1
                  TYL(JWIRE,JHIT) = YL(IWIRE,IHIT)
                  TZL(JWIRE,JHIT) = ZL(IWIRE,IHIT)
                  TLOC(JWIRE,JHIT,1) = LOC(IWIRE,IHIT,1)
                  TLOC(JWIRE,JHIT,2) = LOC(IWIRE,IHIT,2)
                  TLOC(JWIRE,JHIT,3) = LOC(IWIRE,IHIT,3)
                ENDIF
  475         CONTINUE                    ! End of loop over hits intervening
              IF (JHIT.GT.0) TIHMAX(JWIRE) = JHIT ! Set max num home hits per level
C
C  Loop over all unused hits in the intervening wires, allowing as many hits
C  a as available at each level that are within TOLXDI of the road center.
C
              IF (D1HITS(IWIRE) .GT. 0)
     &                            LINEPOS = (SLOPEM*DZL(IWIRE,1))+INTERB
              DO 485 IHIT = 1,MIN(HITS_PER_WIRE,D1HITS(IWIRE))
                DISTANCE = ABS((LINEPOS-DYL(IWIRE,IHIT))*COS(ANGLE))
                IF (DISTANCE .LE. TOLXDI) THEN
                  IF (JHIT .EQ. 0) JWIRE = JWIRE+1
                  JHIT = JHIT+1
                  TYL(JWIRE,JHIT) = DYL(IWIRE,IHIT)
                  TZL(JWIRE,JHIT) = DZL(IWIRE,IHIT)
                  TLOC(JWIRE,JHIT,1) = DLOC(IWIRE,IHIT,1)
                  TLOC(JWIRE,JHIT,2) = DLOC(IWIRE,IHIT,2)
                  TLOC(JWIRE,JHIT,3) = DLOC(IWIRE,IHIT,3)
                ENDIF
  485         CONTINUE                    ! End of loop over hits intervening
              IF (JHIT.GT.0) TIDMAX(JWIRE) = JHIT-TIHMAX(JWIRE) ! Num home hits/lvl
              IF (JHIT.GT.0) TIMAX(JWIRE) = JHIT ! Set max num all hits per level
C
C Check if have missed too many wires
              IF (JHIT.EQ.0) NMISS = NMISS + 1
              IF (NMISS.GT.INEFF) GO TO 400
C
  450       CONTINUE                      ! End of loop over intervening wires
C
            JWIRE = JWIRE+1                 ! Load end wire hit for this loop.
            JHIT = 1
            TYL(JWIRE,JHIT) = DYL(WIREND,IHITEN)
            TZL(JWIRE,JHIT) = DZL(WIREND,IHITEN)
            TLOC(JWIRE,JHIT,1) = DLOC(WIREND,IHITEN,1)
            TLOC(JWIRE,JHIT,2) = DLOC(WIREND,IHITEN,2)
            TLOC(JWIRE,JHIT,3) = DLOC(WIREND,IHITEN,3)
            TIHMAX(JWIRE) = 0
            TIDMAX(JWIRE) = JHIT
            TIMAX(JWIRE) = JHIT
C
            BESCHI = 999999.0
            MAXLVL = JWIRE
C
            DO 11 II = 2,JWIRE
              I(II) = 1
              TMPI(II) = 0
   11       CONTINUE
            I(1) = 0
            TMPI(1) = 0
            NLVL = 1
  100       CONTINUE                          ! Begin loop over possible roads
            I(NLVL) = I(NLVL) + 1
            IF (I(NLVL) .LE. TIMAX(NLVL)) THEN
              NLVL = 1
              DO 12 JJ = 1,JWIRE
                Y(JJ) = TYL(JJ,I(JJ)) + SHIFT(TLOC(JJ,I(JJ),3))
                X(JJ) = TZL(JJ,I(JJ))
   12         CONTINUE
              CALL SEGFIT(X,Y,MAXLVL,A,B,CHISQ,RESID)
              IF (CHISQ .LT. BESCHI .AND. CHISQ .GT. 0.0) THEN
                BESCHI = CHISQ
                SLOPE = A
                INTER = B
                DO 13 JJ = 1,JWIRE
                  TMPI(JJ) = I(JJ)
                  TMPRES(JJ) = RESID(JJ)
   13           CONTINUE
              ENDIF
            ELSE
              NLVL = NLVL + 1
              I(NLVL-1) = 1
              IF (NLVL .GT. MAXLVL) GOTO 200
            ENDIF
            GOTO 100
C
  200       CONTINUE                          ! Store a segment if chisq<cut
C
            IF ((BESCHI .GT. MAXCHI) .OR. (BESCHI.LE.0.)) GOTO 400
C
C  Record as a segment.
C
            NHIT = 0
            LBANK = GZFTSC(HALF,QUAD,SECTOR)
            IHITD = 0
            DO 201 JJ = 1,JWIRE
              IF (TMPI(JJ) .GT. TIMAX(JJ)) GOTO 201
              NHIT = NHIT+1
              IF (TMPI(JJ) .GT. TIHMAX(JJ) .AND. IHITD.EQ.0) THEN
                LBANK = GZFTSC(HALF,QUAD,SECTD1)
                IHITD = NHIT                ! First hit in other sector
              ENDIF
              IF (IHITD.GT.0 .AND. TMPI(JJ).LE.TIHMAX(JJ)) THEN
C               !  Cross-back at hit JJ
                GOTO 400
              ENDIF
              IIWIRE(NHIT) = TLOC(JJ,TMPI(JJ),1)
              LRWIR(NHIT)  = TLOC(JJ,TMPI(JJ),1)*2+TLOC(JJ,TMPI(JJ),3)
              IIHIT(NHIT)  = TLOC(JJ,TMPI(JJ),2)
              HITLOC(NHIT) = IQ(LBANK+4+IQ(LBANK+2)+IIWIRE(NHIT))
              RESID(NHIT)  = TMPRES(JJ)
              IF (NHIT .EQ. 1) LR1 = TLOC(JJ,TMPI(JJ),3)
  201       CONTINUE
C
            IF (NHIT .LT. NBTSEN-INEFF) GOTO 400
            LBANK  = GZFTSC(HALF,QUAD,SECTOR)
C
            IF (IIWIRE(1) .EQ. 0) THEN
              JH = LBANK+HITLOC(1)-1+(IIHIT(1)-1)*IQ(LBANK+3)
              DELAY = Q(JH+4)
              EDELAY = Q(JH+6)
              STATUS = IQ(JH+9)
C Find number of delay hits used
              IF (BTEST(STATUS,0).AND.BTEST(STATUS,1)) THEN
                NDELAY = 2
              ELSE IF (BTEST(STATUS,0).OR.BTEST(STATUS,1)) THEN
                NDELAY = 1
              ELSE
                NDELAY = 0
              END IF
            ELSE
              DELAY = 0.
              EDELAY = 0.
              NDELAY = 0
            ENDIF
C
            IWIRE = TLOC(1,TMPI(1),1)
            CALL GTFALH(HALF,0,QUAD,SECTOR,IWIRE,XC,YC,ZC)
            RC = SQRT(XC**2+YC**2)
            RHIT = SQRT(XC**2+YC**2+DELAY**2)
            PHI1 = ATAN2(YC,XC)
            PHI2 = ATAN2(DELAY,RC)
            PHI = PHI1 + PHI2
            IF (YC.LT.0.) PHI = PHI+TWOPI
            IF (PHI.LT.0.) PHI = PHI+TWOPI
            THETA = ATAN2(RHIT,ZC)
C
C  Good track segment.
C  Flag hits on track segment and calculate AVE_ION,
C  and do angle correction.
C
            AVE_ION = 0.
            LBANK  = GZFTSC(HALF,QUAD,SECTOR)
            LBANKD1 = GZFTSC(HALF,QUAD,SECTD1)
            IF (LBANK.LE.0 .OR. LBANKD1.LE.0) THEN
              GOTO 400
            ENDIF
            AVE_HIT_WIRE = (IQ(LBANK+1)+IQ(LBANKD1+1))/16.
            ERROR_FACTOR = ERRF_XSECT * FDC_ERROR_NHIT(AVE_HIT_WIRE,0)
            ERROR = FDC_ERROR_SLOPE(SLOPE,0) * ERROR_FACTOR
            BESCHI = BESCHI / ERROR_FACTOR**2.
            SHIFT(0) = FDC_DRIFT_SLOPE(SLOPE,0) 
            SHIFT(1) = FDC_DRIFT_SLOPE(SLOPE,1) 
            DL_DIST = 0.0
            DL_ERROR = 9999.
            DO 401 ID = 1,NHIT
              JH = HITLOC(ID)-1
              IF (ID .GE. IHITD) THEN
                JH = LBANKD1+JH+(IIHIT(ID)-1)*IQ(LBANKD1+3)
              ELSE
                JH = LBANK+JH+(IIHIT(ID)-1)*IQ(LBANK+3)
              ENDIF
              IH = IQ(JH+9)
              AVE_ION = AVE_ION + Q(JH+7)
              IF (.NOT.BTEST(IH,2)) THEN
                Q(JH+2) = Q(JH+2) + SHIFT(0) 
                IF ( Q(JH+3) .NE. 0 ) THEN
                  Q(JH+3) = Q(JH+3) + SHIFT(1) 
                ENDIF
                Q(JH+5) = ERROR
                IQ(JH+9) = IBSET(IH,2)             ! Flag hit ID
              END IF
              IF (BTEST(IH,0) .OR. BTEST(IH,1)) THEN
                DL_DIST = Q(JH+4) 
                DL_ERROR = Q(JH+6) 
              ENDIF
  401       CONTINUE
            AVE_ION = AVE_ION/FLOAT(NHIT)
C
C  Store this segment in Zebra bank
C
            SECTDR = (SECTOR + 1000*IHITD)*DIR
            DO 505 ID = 1,IHITD-1
              IIHIT(ID) = IQ(LBANK+4+IQ(LBANK+2)+(IIWIRE(ID)))+
     &                         ((IIHIT(ID)-1)*IQ(LBANK+3))
  505       CONTINUE
            DO 510 ID = IHITD,NHIT
              IIHIT(ID) = IQ(LBANKD1+4+IQ(LBANKD1+2)+(IIWIRE(ID)))+
     &                         ((IIHIT(ID)-1)*IQ(LBANKD1+3))
  510       CONTINUE
C
            CALL LDTSEG(HALF,QUAD,SECTDR,NHIT,LRWIR,IIHIT,
     &        PHI,THETA,RESID,SLOPE,INTER,BESCHI,AVE_ION,NDELAY,
     &        ERROR, DL_DIST, DL_ERROR)

C
            GOTO 500                      ! Start over for next segment
C
  400     CONTINUE                        ! End loop over end wires
  300   CONTINUE                          ! End loop over beginning wires
   10 CONTINUE                          ! End loop over neighboring sectors
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
