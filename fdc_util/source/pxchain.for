      SUBROUTINE PXCHAIN(HALF,SECTOR,SECTD1,
     &                           BCHAIN,HYL,HZL,HLOC,DYL,DZL,DLOC)
C--------------------------------------------------------------------
C-
C-   Purpose and Methods : Store chain from a  PHI sector as a
C-                         track segment in Zebra bank FSGx.
C-
C-   Input : HALF,SECTOR = Logical address of home sector
C-           SECTD1      = Sector adjacent to home sector
C-           BCHAIN      = last hit on home sector chain
C-           HYL,HZL     = drift dists and z locations of home sec hits
C-           HLOC        = wire,hit,lr info of home sector hits
C-           DYL,DZL     = drift dists and z locations of adjac sec hits
C-           DLOC        = wire,hit,lr info of adjac sector hits
C-
C-   Created   6-AUG-1990   Jeffrey Bantly
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS files
C-   Updated  13-JUN-1991   Jeffrey Bantly  cleanup and choose best chisq fit
C-   Updated  17-JUN-1991   Susan K. Blessing  Add SLOPE,INTER,CHISQ for
C-    segment fit.  Add AVE_ION for average ionization/hit for segment.
C-   Updated  30-JUN-1991   Robert E. Avery  Bug fix, if too many chains
C-                              set GCHAIN to 20 (not 21).
C-   Updated  17-OCT-1991   Robert E. Avery  Add Position Error
C-                              to segement bank.Include slope dependent
C-                              corrections to position and error.
C-   Updated  25-NOV-1991   Robert E. Avery  Insure that segment uses at least
C-                              three hits from second chain.
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block.  Replace
C-    SIND and COSD functions with SIN and COS (degrees to radians).
C-   Updated   7-JUL-1992   Robert E. Avery  Include dependence of
C-    error on number hits in sector.
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C-
C--------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER HALF,SECTOR               ! logical address of home sector
      INTEGER SECTD1                    ! address of adjacent sector
      INTEGER BCHAIN                    ! last hit on home sector chain
      INTEGER HLOC(0:NBPSEN-1,MX_HIT_WIRE*2,3) ! wire,hit,lr info of home sec
                                               ! hits
C
      REAL    HYL(0:NBPSEN-1,MX_HIT_WIRE*2) ! drift distances of home sector
                                            ! hits
      REAL    HZL(0:NBPSEN-1,MX_HIT_WIRE*2) ! wire Z locat'ns of home sector
                                            ! hits
      REAL    DYL(0:NBPSEN-1,MX_HIT_WIRE*2) ! drift distances of adjac sec hits
      REAL    DZL(0:NBPSEN-1,MX_HIT_WIRE*2) ! wire Z locat'ns of adjac sec hits
      INTEGER DLOC(0:NBPSEN-1,MX_HIT_WIRE*2,3) ! wire,hit,lr info of adjac sec
                                               ! hits
C
      INTEGER DEPTH1,DEPTH2             ! depth of home,adjac chains
      INTEGER DIR                       ! direction of adjacent sector
      INTEGER GCHAIN                    ! number of stored chain pairs
      INTEGER MXCHAIN                   ! max num of chain pairs
      PARAMETER( MXCHAIN = 20 )
      INTEGER GHIT(NBPSEN,MXCHAIN)      ! hits of stored chain pairs
      INTEGER GLRWIR(NBPSEN,MXCHAIN)    ! LR of stored chain pairs
      INTEGER GNHIT1(MXCHAIN)           ! last hit of 1st chain in stored pair
      INTEGER GTOT(MXCHAIN)             ! total hits of stored chain pairs
      INTEGER GWIRE(NBPSEN,MXCHAIN)     ! wires of stored chain pairs
      INTEGER ICHAIN                    ! chain pair loop counter
      INTEGER JCHAIN                    ! chain with smallest variance
      INTEGER ICHAIN1,ICHAIN2           ! chain loop counters
      INTEGER ID                        ! hit loop counter
      INTEGER IHIT(NBPSEN)              ! hits on wires on segment
      INTEGER IHITD                     ! cross-over hit for seg storage
      INTEGER ILINK                     ! current link
      INTEGER INEFF                     ! max allowed missing wires in segment
      INTEGER ITOT                      ! current count of total hits on chains
      INTEGER ITOTUSD                   ! total number of used hits in segment
      INTEGER IWIRE(NBPSEN)             ! wires on segment
      INTEGER J1,JH(NBPSEN),IH          ! locations in banks
      INTEGER LRWIR(NBPSEN)             ! LR value of hits on segment
      INTEGER LCHAI                     ! CHAI bank link
      INTEGER LKBANK,LKBANKD1           ! FPSC bank links of home,adjac sec
      INTEGER LLINKH,LLINKD             ! LINK bank links of home,adjac sec
      INTEGER LOC                       ! location of link in LINK bank
      INTEGER LR1                       ! LR of 1st hit on segment
      INTEGER LSTART1,LSTART2           ! bank locations of home,adjac chains
      INTEGER MAXWIR1,MAXWIR2           ! first,last wires of two chains
      INTEGER MINDEP                    ! minimum allowed depth of chain pair
      INTEGER NCHAIN                    ! total number of chains in CHAI
      INTEGER NHIT                      ! number of hits in total segment
      INTEGER NHIT1                     ! number of hits in first chain
      INTEGER NHUSED                    ! number of newly used hits in sector
      INTEGER SECTDR                    ! adjusted sector value for seg storage
      INTEGER WIRE,HIT,LR               ! info of current hit
      INTEGER ICALL                     ! initialization check
      INTEGER IER                       ! error check
      INTEGER GZFPSC                    ! function fetches FPSC bank link
C
      REAL    AVE_ION                   ! average ionization/hit on segment
      REAL    DDRAD                     ! delta drift radius
      REAL    DELDRPR                   ! delta drift dist between pair hits
      REAL    DRIFTD(NBPSEN)            ! drift distances of hits on segment
      REAL    DRIFTD_OLD                ! previous adjusted drift distance
      REAL    MAXCHI(2)                 ! variance cut-offs of segment fits
      REAL    GRESID(NBPSEN,MXCHAIN)    ! residuals of stored chain pairs
      REAL    GVAR(MXCHAIN)             ! variance of stored chain pairs
      REAL    GSLOPE(MXCHAIN)           ! slope of stored chains
      REAL    GINTER(MXCHAIN)           ! intercept of stored chains
      REAL    GZLOC(MXCHAIN)            ! 1st wire Z loc of stored chains
      REAL    HSLOPE                    ! drift/z slope of home sector hits
      REAL    MINVAR                    ! current minimum variance of seg fit
      REAL    ORIG_DRIFTD(NBPSEN)       ! drift distances before adjustment
      REAL    PHI,DR1                   ! phi, 1st wire drift dist for seg
      REAL    PREV_DRIFTD               ! drift distance before adjustment
      REAL    RADIUS                    ! distance of sector wall from wires
      REAL    RESID(NBPSEN)             ! residuals of segment fit of hits
      REAL    RPRIME                    ! radius of segment cross-over
      REAL    SLOPE,INTER,VARIANCE      ! slope,intercept,variance of seg fit
      REAL    SUMDEL                    ! sum of delta drift distances
      REAL    SUMDEL_OLD                ! old sum of delta drift distances
      REAL    VARIANCE1                 ! variance of plane fit segment
      REAL    WIRDIF                    ! distance between rotated wire planes
      REAL    ZLOC(NBPSEN)              ! wire Z locations of hits on segment
      REAL    ZPOS                      ! z position of 1st in segment fit
      REAL    ERROR, SHIFT(0:1)
      REAL    FDC_DRIFT_SLOPE
      REAL    FDC_ERROR_SLOPE
      REAL    ERROR_FACTOR, ERRF_XSECT 
      REAL    AVE_HIT_WIRE 
      REAL    FDC_ERROR_NHIT
C
      LOGICAL RADIUS_DONE               ! TRUE if radius of seg calculated
C
      SAVE MAXCHI,INEFF,MINDEP,ERROR_FACTOR
C
      DATA RESID/16*0.0/
      DATA MAXCHI/100000.,100000./
      DATA ERRF_XSECT /1.0/
C
C----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('PINEFF',INEFF,IER)
        CALL EZGET('MAX_XPSEG_CHI',MAXCHI,IER)
        CALL EZGET('ERRF_XSECT_PHI',ERRF_XSECT ,IER)
        MINDEP = NBPSEN-INEFF-2
        CALL EZRSET
        ICALL = 1
      END IF
      GCHAIN = 0
C
C  Get and check link values.
C
      IF (LFLOC.LE.5) GOTO 999
      LLINKH = LQ(LFLOC-1)
      IF (LLINKH.LE.5) GOTO 999
      LLINKD = LQ(LFLOC-5)
      IF (LLINKD.LE.5) GOTO 999
      LCHAI = LQ(LFLOC-2)
      IF (LCHAI.LE.5) GOTO 999
      NCHAIN = IQ(LCHAI+1)
C
C  Loop over pairs of chains, one from each of the two sectors.
C
      DO 100 ICHAIN1 = 1,BCHAIN
        DO 150 ICHAIN2 = BCHAIN+1,NCHAIN
C
          LSTART1 = LCHAI + 1 + (ICHAIN1-1)*NBPSEN
          LSTART2 = LCHAI + 1 + (ICHAIN2-1)*NBPSEN
          DEPTH1 = IQ(LSTART1+1)-1
          DEPTH2 = IQ(LSTART2+1)-1
          IF (DEPTH1+DEPTH2 .LT. MINDEP) GOTO 150
C
          LOC = LLINKH + (IQ(LSTART1+1+1)-1)*10
          MAXWIR1 = IQ(LOC+3)
          LOC = LLINKD + (IQ(LSTART2+1+1)-1)*10
          MAXWIR2 = IQ(LOC+3)
          IF ((MAXWIR2-MAXWIR1).LE. (NBPSEN-1-INEFF)) GOTO 150
C
C  Loop over hits on home chain and find their coordinates in the home
C  sector drift coordinates.
C
          NHIT1 = DEPTH1 + 1
          NHIT = NHIT1 + DEPTH2 + 1
          ITOT = 0
          DO 200 ID = 1,NHIT1
            IF (ID.LT.NHIT1) THEN
              ILINK = IQ(LCHAI+1+(ICHAIN1-1)*NBPSEN+1+ID)
              LOC = LLINKH + (ILINK-1)*10
              IF (IQ(LOC+6).LT.0) GO TO 100         ! link deleted
              J1 = 0
            ELSEIF (ID.EQ.NHIT1) THEN
              J1 = 1
            ENDIF
            ITOT = ITOT+1
            WIRE = IQ(LOC+3+J1)
            HIT = IQ(LOC+1+J1)
            IWIRE(ITOT) = HLOC(WIRE,HIT,1)
            LR = HLOC(WIRE,HIT,3)
            LRWIR(ITOT) = IWIRE(ITOT)*2+LR
            IF (ITOT.EQ.1) LR1 = LR
            IHIT(ITOT) = HLOC(WIRE,HIT,2)
            DRIFTD(ITOT) = HYL(WIRE,HIT)
            ORIG_DRIFTD(ITOT) = DRIFTD(ITOT)
            ZLOC(ITOT)  = HZL(WIRE,HIT)
            IF (ID.EQ.NHIT1) THEN
              DIR = SECTD1-SECTOR
              IF (LR.EQ.0 .AND. DIR.NE.1) THEN   ! LR = 0 is Positive drift
                GOTO 150
              ELSEIF (LR.EQ.1 .AND. DIR.NE.-1) THEN
                GOTO 150
              ENDIF
            ENDIF
  200     CONTINUE
C
          HSLOPE = (DRIFTD(ITOT)-DRIFTD(1)) / (IWIRE(ITOT)-IWIRE(1))
C
C  Loop over the hits on the adjacent sector's chain and convert the
C  drift distances to coordinates of the home sector.  Also, calculate
C  the radius of the cross-over point between sectors.
C  Conversion takes the first point across the boundary and places it at
C  the expected location projected from the home sector.  Each subsequent
C  hit is placed based on the difference in drift distance between the hit
C  and the previous one.  The more accurate the cross-over radius, the
C  better the conversion works in general.
C
          RADIUS_DONE = .FALSE.
          DO 201 ID = NHIT,NHIT1+1,-1
            IF (ID.LT.NHIT) THEN
              ILINK = IQ(LCHAI+1+(ICHAIN2-1)*NBPSEN+1+(ID-NHIT1))
              LOC = LLINKD + (ILINK-1)*10
              IF (IQ(LOC+6).LT.0) GO TO 150         ! link deleted
              J1 = 0
            ELSEIF (ID.EQ.NHIT) THEN
              ILINK = IQ(LCHAI+1+(ICHAIN2-1)*NBPSEN+1+((ID-1)-NHIT1))
              LOC = LLINKD + (ILINK-1)*10
              IF (IQ(LOC+6).LT.0) GO TO 150         ! link deleted
              J1 = 1
            ENDIF
            WIRE = IQ(LOC+3+J1)
            HIT = IQ(LOC+1+J1)
            IF (DLOC(WIRE,HIT,1).LE.IWIRE(NHIT1)) THEN
              IF (DLOC(WIRE,HIT,1).EQ.IWIRE(NHIT1)) THEN
                DIR = SECTD1-SECTOR
C 
                RADIUS = (ABS(DYL(WIRE,HIT))+ABS(DRIFTD(ITOT))) /
     &                           (2.*SIN(5.*RADIAN))
                RPRIME = RADIUS*COS(5.*RADIAN)/COS(10.*RADIAN)
                WIRDIF = RPRIME*SIN(10.*RADIAN)*DIR
                RADIUS_DONE = .TRUE.
              ENDIF
              GOTO 201
            ENDIF
            ITOT = ITOT+1
            IWIRE(ITOT) = DLOC(WIRE,HIT,1)
            LR = DLOC(WIRE,HIT,3)
            LRWIR(ITOT) = IWIRE(ITOT)*2+LR
            IF (ITOT.EQ.1) LR1 = LR
            IHIT(ITOT) = DLOC(WIRE,HIT,2)
            DRIFTD(ITOT) = DYL(WIRE,HIT)
            ZLOC(ITOT)  = DZL(WIRE,HIT)
            IF (ITOT.EQ.NHIT1+1) THEN    ! first hit across boundary
              DIR = SECTD1-SECTOR
              IF (LR.EQ.1 .AND. DIR.NE.1) THEN   ! LR = 1 is Negative drift
                GOTO 150
              ELSEIF (LR.EQ.0 .AND. DIR.NE.-1) THEN
                GOTO 150
              ENDIF
            ENDIF
            IF (ITOT.EQ.NHIT1+1) THEN
              IF (.NOT.RADIUS_DONE) THEN
                DIR = SECTD1-SECTOR
                RADIUS = (ABS(DRIFTD(ITOT))+ABS(DRIFTD(ITOT-1))) /
     &                           (2.*SIN(5.*RADIAN))
                RPRIME = RADIUS*COS(5.*RADIAN)/COS(10.*RADIAN)
                WIRDIF = RPRIME*SIN(10.*RADIAN)*DIR
              ENDIF
              DELDRPR = 0
              DDRAD = (DRIFTD(1)+HSLOPE*(IWIRE(ITOT)-IWIRE(1)))-WIRDIF
              ORIG_DRIFTD(ITOT) = DRIFTD(ITOT)
              PREV_DRIFTD = DRIFTD(ITOT)
              SUMDEL_OLD = 0
              SUMDEL = 0
            ELSE
C 1.396 radians is about 80 degrees
              DELDRPR = (DRIFTD(ITOT)-PREV_DRIFTD)/SIN(1.396)
              SUMDEL_OLD = SUMDEL_OLD+DELDRPR
              SUMDEL = SUMDEL+DELDRPR
              ORIG_DRIFTD(ITOT) = DRIFTD(ITOT)
              PREV_DRIFTD = DRIFTD(ITOT)
            ENDIF
            DRIFTD_OLD = WIRDIF+DDRAD+SUMDEL_OLD
            DRIFTD(ITOT) = WIRDIF+DDRAD+SUMDEL
  201     CONTINUE
C
C  If the combined chains have enough hits to make a segment, then fit
C  them for a segment.  Make a cut on the variance of the fit.
C  (Future : If the variance is large but not TOO large, then retry the
C   cross-sector fit using intersecting planes and refitting.  Refit must
C   still pass the tighter cut.)
C
          IF (ITOT.LT.NBPSEN-INEFF) GOTO 150
          IF (ITOT-NHIT1.LE.2) GOTO 150
C
          CALL SEGFIT(ZLOC,DRIFTD,ITOT,SLOPE,INTER,VARIANCE,RESID)
C
          IF (VARIANCE.GT. MAXCHI(2)) GOTO 150
          IF (VARIANCE.GT. MAXCHI(1)) THEN
            CALL FXPCHK_PLANES(HALF,SECTOR,SECTD1,IWIRE,ORIG_DRIFTD,
     &                         NHIT1,ITOT,VARIANCE1)
            IF (VARIANCE1.GT. MAXCHI(1)) GOTO 150
          ENDIF
C
C  Store the information for each successful fitted pair of chains.
C
          GCHAIN = GCHAIN+1
          IF (GCHAIN.GT.MXCHAIN) THEN
            CALL ERRMSG('FDC-too-many-phi-chains','PXCHAIN',
     &            ' Too many Phi cross-segment hit chains in sector',
     &            'W')
            GCHAIN = MXCHAIN
            GOTO 50
          ENDIF
          DO ID = 1,ITOT
            GWIRE(ID,GCHAIN) = IWIRE(ID)
            GHIT(ID,GCHAIN)  = IHIT(ID)
            GLRWIR(ID,GCHAIN) = LRWIR(ID)
            GRESID(ID,GCHAIN) = RESID(ID)
          ENDDO
          GNHIT1(GCHAIN)   = NHIT1
          GTOT(GCHAIN)     = ITOT
          GZLOC(GCHAIN)    = ZLOC(1)
          GVAR(GCHAIN)     = VARIANCE
          GSLOPE(GCHAIN)  = SLOPE
          GINTER(GCHAIN)  = INTER
C
  150   CONTINUE                        ! End ICHAIN2 loop
  100 CONTINUE                          ! End ICHAIN1 loop
C
   50 CONTINUE
      IF (GCHAIN.LE.0) GOTO 999
C
C  Select the best segment candidate first and record that one.
C
      MINVAR = 500.
      JCHAIN = 0
      DO ICHAIN = 1,GCHAIN
        IF (GVAR(ICHAIN).LT.MINVAR) THEN
          JCHAIN = ICHAIN
          MINVAR = GVAR(ICHAIN)
        ENDIF
      ENDDO
      IF (JCHAIN.LE.0) GOTO 999
C
C  Check and flag hits on track segment.
C  Calculate average ionization/hit on segment,
C  and do angle corrections.
C
  300 CONTINUE
      NHUSED = 0
      ITOTUSD = 0
      ITOT = GTOT(JCHAIN)
      NHIT1 = GNHIT1(JCHAIN)
      LKBANK = GZFPSC(HALF,SECTOR)
      IF (LKBANK.LE.5) GOTO 999
      LKBANKD1 = GZFPSC(HALF,SECTD1)
      IF (LKBANKD1.LE.5) GOTO 999
      CALL VZERO(JH,NBPSEN)
      AVE_ION = 0.
      AVE_HIT_WIRE = (IQ(LKBANK+1)+IQ(LKBANKD1+1))/32.
      DO 400 ID = 1,ITOT
        IF (ID .LE. NHIT1) THEN
          JH(ID) = IQ(LKBANK+4+IQ(LKBANK+2)+GWIRE(ID,JCHAIN))
          JH(ID) = LKBANK+JH(ID)+(GHIT(ID,JCHAIN)-1)*IQ(LKBANK+3)-1
          IHITD = ID+1
        ELSE
          JH(ID) = IQ(LKBANKD1+4+IQ(LKBANKD1+2)+GWIRE(ID,JCHAIN))
          JH(ID) = LKBANKD1+JH(ID)+(GHIT(ID,JCHAIN)-1)*IQ(LKBANKD1+3)-1
        ENDIF
        IH = IQ(JH(ID)+9)
        IF (.NOT.BTEST(IH,2)) THEN
          NHUSED = NHUSED+1
        ELSE
          ITOTUSD = ITOTUSD + 1
          IF (ITOTUSD.GT.1) GOTO 350
        END IF
  400 CONTINUE
      ERROR_FACTOR = ERRF_XSECT * FDC_ERROR_NHIT(AVE_HIT_WIRE,1)
      ERROR = FDC_ERROR_SLOPE(SLOPE,1) * ERROR_FACTOR
      SHIFT(0) = FDC_DRIFT_SLOPE(SLOPE,0)
      SHIFT(1) = FDC_DRIFT_SLOPE(SLOPE,1)
      DO 401 ID = 1,ITOT
        IH = IQ(JH(ID)+9)
        IF (.NOT.BTEST(IH,2)) THEN
          Q(JH(ID)+2) = Q(JH(ID)+2) + SHIFT(0)
          Q(JH(ID)+3) = Q(JH(ID)+3) + SHIFT(1)
          IQ(JH(ID)+9) = IBSET(IH,2)             ! Flag hit ID
        END IF
        Q(JH(ID)+5) = ERROR
        AVE_ION = AVE_ION + Q(JH(ID)+7)
        IF (ID.EQ.1) THEN
          LR1 = GLRWIR(1,JCHAIN)-GWIRE(1,JCHAIN)*2
          DR1 = Q(JH(ID)+2+LR1)
        ENDIF
  401 CONTINUE
      AVE_ION = AVE_ION/FLOAT(ITOT)
C
C  Store this segment in Zebra bank
C
C      PHI = 0.08727+0.17453*FLOAT(SECTOR)
      PHI = 5.*RADIAN + 10.*RADIAN*FLOAT(SECTOR)
      ZPOS = GZLOC(JCHAIN)
      SLOPE = GSLOPE(JCHAIN)
      INTER = GINTER(JCHAIN)
      VARIANCE = GVAR(JCHAIN) / ERROR_FACTOR**2.
      DIR = SECTD1-SECTOR
      SECTDR = (SECTOR + 1000*(IHITD))*DIR
      DO 500 ID = 1,NHIT1
        IHIT(ID) = IQ(LKBANK+4+IQ(LKBANK+2)+(GLRWIR(ID,JCHAIN)/2))+
     &                         ((GHIT(ID,JCHAIN)-1)*IQ(LKBANK+3))
        LRWIR(ID) = GLRWIR(ID,JCHAIN)
        RESID(ID) = GRESID(ID,JCHAIN)
  500 CONTINUE
      DO 505 ID = NHIT1+1,ITOT
        IHIT(ID) = IQ(LKBANKD1+4+IQ(LKBANKD1+2)+(GLRWIR(ID,JCHAIN)/2))+
     &                         ((GHIT(ID,JCHAIN)-1)*IQ(LKBANKD1+3))
        LRWIR(ID) = GLRWIR(ID,JCHAIN)
        RESID(ID) = GRESID(ID,JCHAIN)
  505 CONTINUE
      CALL LDPSEG(HALF,SECTDR,ITOT,LRWIR,IHIT,PHI,DR1,ZPOS,RESID,
     &  SLOPE,INTER,VARIANCE,AVE_ION,ERROR)
C
C  Reget and recheck link values.
C
      IF (LFLOC.LE.5) GOTO 999
      LLINKH = LQ(LFLOC-1)
      IF (LLINKH.LE.5) GOTO 999
      LLINKD = LQ(LFLOC-5)
      IF (LLINKD.LE.5) GOTO 999
      LCHAI = LQ(LFLOC-2)
      IF (LCHAI.LE.5) GOTO 999
C
C  Increment number of used hits in this sector
C
      IQ(LFLOC+2) = IQ(LFLOC+2)+NHUSED
C
C  Move on to next possible candidate after marking this one as used.
C
  350 CONTINUE
      GVAR(JCHAIN) = 600.
      GOTO 50
C
C----------------------------------------------------------------------
  999 RETURN
      END
