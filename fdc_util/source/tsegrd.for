      SUBROUTINE TSEGRD(HALF,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Form roads with hits within
C-                         Theta sector boundaries
C-
C-   Inputs  : HALF,QUAD,SECTOR - Identify which sector being used
C-   Outputs :
C-   Controls:
C-
C-   Created   7-NOV-1989   Jeffrey Bantly
C-   Updated  21-MAR-1990   Jeffrey Bantly  add residuals to segments
C-   Updated  16-APR-1990   Susan Blessing  use chi**2/dof for segment fitting
C-                                          rather than Cernlib PROB function
C-   Updated   3-MAY-1990   Jeffrey Bantly  tineff->theta only inefficiencies
C-   Updated   6-NOV-1990   Robert E. Avery  Add protection for too
C-                                           many combinations.
C-   Updated  12-DEC-1990   Susan K. Blessing  Change the size of arrays
C-                                             associated with the FBESEG call.
C-                                             Additional argument for FBESEG.
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS
C-   Updated  13-MAY-1991   Susan K. Blessing  Add NDELAY to FTSGPAR call, then
C-    pass it to LDFTSG.
C-   Updated  17-JUN-1991   Susan K. Blessing  Add SLOPE,INTER,BESCHI,AVE_ION
C-    information to LDTSEG call.
C-   Updated  20-JUN-1991   Susan K. Blessing   Turn off hits in segments
C-    rather than the entire segment after one segment has been loaded.
C-    Refit segments with sufficient hits and try again.
C-   Updated  28-JUN-1991   Susan K. Blessing  If a possible segment
C-    uses all of the hits of a second possible segment, the second
C-    segment has fewer hits, and the second segment has a better chisq,
C-    turn off the second segment because it is most likely to be the
C-    wrong side combination.
C-   Updated   1-JUL-1991   Susan K. Blessing  Save chisq from "wrong
C-    side" combination.  Turn "wrong side" back on before turning hits
C-    off after a successful segment, then do "wrong side" checking again.
C-   Updated  17-OCT-1991   Robert E. Avery  Add Position Error,
C-    Delay Line Position, and Delay Line Error to segement bank.
C-    Include slope dependent corrections to position and error.
C-   Updated  22-NOV-1991   Susan K. Blessing  Remove MIN(MX_HIT_WIRE,HOHITS())
C-    from inner wire loop.  Not only was it wrong, it's unncessary.
C-    Count number of missed wires and leave loop when gt INEFF.
C-   Updated  10-DEC-1991   Susan K. Blessing  Remove changes of 28-JUN-1991
C-    and 1-JUL-1991.  Now choose segments which point toward the IR first
C-    to help with the problem of getting wrong side segments.
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block.
C-   Updated  21-FEB-1992   Susan K. Blessing  Change call to THITAC
C-    to include beginning and ending wires of interest (0 to NBTSEN-1
C-    here).
C-   Updated  12-MAR-1992   Susan K. Blessing  Just throw out road if
C-    too many combinations rather than rest of sector.
C-   Updated  12-MAY-1992   Susan K. Blessing  Don't check loaded segment,
C-    just turn it off.
C-   Updated  29-MAY-1992   Susan K. Blessing  Use TOLDIST for tolerance
C-    to allow different values for theta and phi.
C-    Remove unnecessary VZERO calls.
C-   Updated  29-JUN-1992   Robert E. Avery  Fix bug. Direction of 
C-    slope dependent t0 correction was wrong.
C-   Updated   7-JUL-1992   Robert E. Avery  Include dependence of
C-    error on number hits in sector.
C-   Updated  24-AUG-1992   Susan K. Blessing  Subsequent lost hits have 
C-    to be moved down one place.
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER I,II,JJ,LL
      INTEGER HALF,QUAD,SECTOR
      INTEGER NHITS(0:NBTSEN-1),IPTR(0:NBTSEN-1)
      INTEGER IIWIRE(NBTSEN),LRWIR(NBTSEN),IIHIT(NBTSEN)
      INTEGER HITLOC(NBTSEN),NHIT,NHITS1
      INTEGER TIIWIRE(MXNSEG,NBTSEN),TLRWIR(MXNSEG,NBTSEN)
      INTEGER TIIHIT(MXNSEG,NBTSEN),THITLOC(MXNSEG,NBTSEN)
      INTEGER IWIRE,IHIT,JWIRE,JHIT,LR,LR1,NEL,NWORDS
      INTEGER ISTAT,ICALL,TMPI(NBTSEN),JH,IH,NHUSED,ID
      INTEGER LOC(0:NBTSEN-1,MX_HIT_WIRE*2,3),BESSEG,NTSEG
      INTEGER TLOC(NBTSEN,MX_HIT_WIRE*2,3),TLR1(MXNSEG)
      INTEGER TIMAX(NBTSEN),TNHIT(MXNSEG)
      INTEGER HOHITS(0:NBTSEN-1),MXWRBG,MXWREN
      INTEGER INEFF,IER,WIRBEG,WIREND,IHITST,IHITEN
      INTEGER NLVL,MAXLVL
      INTEGER ISEG
      INTEGER LKFTSC
      INTEGER NSEN
      INTEGER NDELAY
      INTEGER NUM_COMB
      INTEGER MAX_COMB
      INTEGER WIRMIN
      INTEGER GZFTSC
      INTEGER N_LOST_HITS(MXNSEG),LOST_HIT(MXNSEG,NBTSEN)
      INTEGER NMISS
C
      REAL    CONT(18)
      REAL    STAT,BESCHI,MAXCHI
      REAL    TBESCHI(MXNSEG),TRESID(MXNSEG,NBTSEN),TMPRES(NBTSEN)
      REAL    TSLOPE(MXNSEG),TINTER(MXNSEG)
      REAL    YL(0:NBTSEN-1,MX_HIT_WIRE*2),ZL(0:NBTSEN-1,MX_HIT_WIRE*2)
      REAL    TYL(NBTSEN,MX_HIT_WIRE*2),TZL(NBTSEN,MX_HIT_WIRE*2)
      REAL    TMPX(NBTSEN),TMPY(NBTSEN)
      REAL    XBEST(NBTSEN),YBEST(NBTSEN),RESID(NBTSEN)
      REAL    TX(MXNSEG,NBTSEN),TY(MXNSEG,NBTSEN)
      REAL    XC,YC,ZC,ENDY,ENDZ,SLOPEM,INTERB
      REAL    PHI,THETA,DISTANCE,LINEPOS
      REAL    TOLDIS,STAGGER,ANGLE
      REAL    TOLXSW,TOLERANCE
      REAL    SLOPE,INTER,AVE_ION
      REAL    DL_DIST, DL_ERROR
      REAL    SAVECHI(MXNSEG)
      REAL    ERROR
      REAL    ERROR_FACTOR, SHIFT(0:1)
      REAL    FDC_DRIFT_SLOPE
      REAL    FDC_ERROR_SLOPE
      REAL    AVE_HIT_WIRE 
      REAL    FDC_ERROR_NHIT
C
      EQUIVALENCE (ISTAT,STAT)
C
      LOGICAL ENOUGH, CALRES
      LOGICAL REQUIRE_SLOPE,TSEG_SLOPE_IR,OK
C
      SAVE ICALL,INEFF,TOLDIS,TOLXSW,CALRES
      SAVE MAX_COMB,NSEN,TSEG_SLOPE_IR
C
      DATA ICALL/0/
      DATA NSEN/NBTSEN/
      DATA TSEG_SLOPE_IR/.TRUE./
C
C----------------------------------------------------------------------
C
      IF (ICALL.EQ.0) THEN
        ICALL = 1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('TINEFF',INEFF,IER)
        CALL EZGET('TOLDIST',TOLDIS,IER)
        CALL EZGET('TOLXSW',TOLXSW,IER)
        CALL EZGET('CALRES',CALRES,IER)
        CALL EZGET('MAX_COMB',MAX_COMB,IER)
        CALL EZGET('MAX_TSEG_CHI',MAXCHI,IER)
        CALL EZGET('TSEG_SLOPE_IR',TSEG_SLOPE_IR,IER)
        IF (MAXCHI.LE.0.) MAXCHI = 10.
        CALL EZRSET
        IF ( TOLXSW .LE. 0) THEN
          TOLXSW = TOLDIS
        ENDIF
      END IF
C
C   Check to see if at least one hit available in current sector to
C   start road.
C
      CALL GTFTSC(HALF,QUAD,SECTOR,'WIR',0,NEL,NWORDS,CONT)
      CALL UCOPY(CONT,NHITS,NEL)
      CALL UCOPY(CONT(NEL+1),IPTR,NEL)
      DO 5 II = 0,INEFF
        NHITS1 = NHITS1 + NHITS(II)
    5 CONTINUE
      IF( NHITS1 .LE. 0 ) GOTO 999      ! no hits to start road
C
C  Accumulate unused hits.
C
      CALL THITAC(HALF,QUAD,SECTOR,0,NBTSEN-1,YL,ZL,LOC,HOHITS,ENOUGH)
      IF( .NOT.ENOUGH ) GOTO 999
C
C  Form segments by choosing beginning wire and ending wire and making a road.
C
      MXWRBG = NBTSEN
      MXWREN = -1
      DO 110 IWIRE = 0,INEFF
        IF(HOHITS(IWIRE) .GT. 0 ) THEN
          MXWRBG = IWIRE
          GOTO 115
        ENDIF
  110 CONTINUE
  115 CONTINUE
      DO 120 IWIRE = NBTSEN-1,NBTSEN-1-INEFF,-1
        IF(HOHITS(IWIRE) .GT. 0 ) THEN
          MXWREN = IWIRE
          GOTO 125
        ENDIF
  120 CONTINUE
  125 CONTINUE
      IF( MXWREN-MXWRBG+1 .LE. NBTSEN-1-INEFF ) GOTO 999
C
C  Loop over every allowable beginning wire up to wirbeg = ineff
C
      DO 200 WIRBEG = MXWRBG,INEFF
C
C  Loop over every allowable beginning wire up to wirbeg = ineff
C
        WIRMIN = NBTSEN-1-INEFF+WIRBEG
        DO 250 WIREND = MXWREN,WIRMIN,-1
          LKFTSC = GZFTSC(HALF,QUAD,SECTOR)
C
          REQUIRE_SLOPE = TSEG_SLOPE_IR
C
C  Loop over every unused hit in beginning wire, allowing only one possible
C  hit at this level.
C
          NTSEG = 0
          DO 300 IHITST = 1,HOHITS(WIRBEG)
C
C  Loop over every unused hit in ending wire, allowing only one possible
C  hit at this level.
C
            DO 400 IHITEN = 1,HOHITS(WIREND)
              JWIRE = 1
              JHIT = 1
              TYL(JWIRE,JHIT) = YL(WIRBEG,IHITST)
              TZL(JWIRE,JHIT) = ZL(WIRBEG,IHITST)
              TLOC(JWIRE,JHIT,1) = LOC(WIRBEG,IHITST,1)
              TLOC(JWIRE,JHIT,2) = LOC(WIRBEG,IHITST,2)
              TLOC(JWIRE,JHIT,3) = LOC(WIRBEG,IHITST,3)
              TIMAX(JWIRE) = JHIT
              ENDY = YL(WIREND,IHITEN)
              ENDZ = ZL(WIREND,IHITEN)
              IF( (ENDZ-TZL(1,1)) .EQ. 0. ) GOTO 400
              SLOPEM = (ENDY-TYL(1,1))/(ENDZ-TZL(1,1))
              INTERB = ENDY-(SLOPEM*ENDZ)
              ANGLE = ATAN2( (ENDY-TYL(1,1)),(ENDZ-TZL(1,1)) )
C
              IF ( SECTOR.EQ.1 ) THEN
                SHIFT(0) = -FDC_DRIFT_SLOPE(SLOPEM,0)
                SHIFT(1) = -FDC_DRIFT_SLOPE(SLOPEM,1)
              ELSE
                SHIFT(0) = FDC_DRIFT_SLOPE(SLOPEM,0)
                SHIFT(1) = FDC_DRIFT_SLOPE(SLOPEM,1)
              ENDIF
              TYL(JWIRE,JHIT) = TYL(JWIRE,JHIT)
     &          + SHIFT(LOC(WIRBEG,IHITST,3))
C
C  Loop over intervening wires
C
C Count number of missed/unused wires
              NMISS = NBTSEN - (WIREND-WIRBEG+1)
              DO 450 IWIRE = WIRBEG+1,WIREND-1
                IF( HOHITS(IWIRE) .GT. 0 )
     &                            LINEPOS = (SLOPEM*ZL(IWIRE,1))+INTERB
C
C  Wider tolerance for XSW segments (beware of time increase)
C
                IF ( LOC(WIRBEG,IHITST,3) .NE.
     &               LOC(WIREND,IHITEN,3) ) THEN
                  TOLERANCE = TOLXSW
                ELSE
                  TOLERANCE = TOLDIS
                ENDIF
C
C  Loop over all unused hits in the intervening wires in the home sector,
C  allowing as many hits as available at each level that are within
C  TOLDIS of the road center.
C
                JHIT = 0
                DO 475 IHIT = 1,HOHITS(IWIRE)
                  DISTANCE = ABS( (LINEPOS-YL(IWIRE,IHIT))*COS(ANGLE) )
                  IF(DISTANCE .LE. TOLERANCE ) THEN
                    IF (JHIT .EQ. 0) JWIRE = JWIRE+1
                    JHIT = JHIT+1
                    TYL(JWIRE,JHIT) = YL(IWIRE,IHIT)
     &                + SHIFT(LOC(IWIRE,IHIT,3))
                    TZL(JWIRE,JHIT)=ZL(IWIRE,IHIT)
                    TLOC(JWIRE,JHIT,1) = LOC(IWIRE,IHIT,1)
                    TLOC(JWIRE,JHIT,2) = LOC(IWIRE,IHIT,2)
                    TLOC(JWIRE,JHIT,3) = LOC(IWIRE,IHIT,3)
                  ENDIF
  475           CONTINUE                    ! End of loop over hits intervening
                IF (JHIT.GT.0) TIMAX(JWIRE) = JHIT ! Set num hits per level
C
C Check if have missed too many wires
                IF (JHIT.EQ.0) NMISS = NMISS + 1
                IF (NMISS.GT.INEFF) GO TO 400
C
  450         CONTINUE                      ! End of loop over intervening wires
C
              JWIRE = JWIRE+1                 ! Load end wire hit for this loop.
              JHIT = 1
              TYL(JWIRE,JHIT) = YL(WIREND,IHITEN)
     &          + SHIFT(LOC(WIREND,IHITEN,3))
              TZL(JWIRE,JHIT) = ZL(WIREND,IHITEN)
              TLOC(JWIRE,JHIT,1) = LOC(WIREND,IHITEN,1)
              TLOC(JWIRE,JHIT,2) = LOC(WIREND,IHITEN,2)
              TLOC(JWIRE,JHIT,3) = LOC(WIREND,IHITEN,3)
              TIMAX(JWIRE) = JHIT
C
C Check number of possible combinations
              IF (MAX_COMB.GT.0) THEN
C
                NUM_COMB = 1
                DO 11 II=2,JWIRE
                  NUM_COMB = NUM_COMB * TIMAX(II)
C If too many cominations, give up on this road
                  IF (NUM_COMB .GT. MAX_COMB) THEN
                    CALL ERRMSG('FDC-too-many-theta-combs','TSEGRD',
     &               ' Too many hit combinations in Theta segment road',
     &                'W')
                    GO TO 400
                  ENDIF
   11           CONTINUE
              END IF
C
              CALL FBESEG(NSEN,JWIRE,TZL,TYL,TIMAX,TMPI,TMPRES,
     &             TMPX,TMPY,BESCHI,SLOPE,INTER)
C                                           ! Store a segment if chisq<cut
              IF ( (BESCHI .GT. MAXCHI) .OR. (BESCHI.LE.0.) ) GOTO 400
C
C  TEMPORARILY Record as a segment.
C
              NTSEG = NTSEG+1
              NHIT = 0
              TBESCHI(NTSEG) = BESCHI
              TSLOPE(NTSEG) = SLOPE
              TINTER(NTSEG) = INTER
              DO 201 JJ = 1,JWIRE
                IF(TMPI(JJ) .GT. TIMAX(JJ)) GOTO 201
                NHIT = NHIT+1
                TIIWIRE(NTSEG,NHIT) = TLOC(JJ,TMPI(JJ),1)
                TLRWIR(NTSEG,NHIT)  =
     &                        TLOC(JJ,TMPI(JJ),1)*2+TLOC(JJ,TMPI(JJ),3)
                TIIHIT(NTSEG,NHIT)  = TLOC(JJ,TMPI(JJ),2)
                THITLOC(NTSEG,NHIT) = IQ(LKFTSC+4+IQ(LKFTSC+2)+
     &                                       TIIWIRE(NTSEG,NHIT))
                TRESID(NTSEG,NHIT)  = TMPRES(JJ)
                TX(NTSEG,NHIT)      = TMPX(JJ)
                TY(NTSEG,NHIT)      = TMPY(JJ)
                IF( NHIT .EQ. 1 ) TLR1(NTSEG) = TLOC(JJ,TMPI(JJ),3)
  201         CONTINUE
              TNHIT(NTSEG) = NHIT
C
              IF(NTSEG.GE.MXNSEG) GOTO 500
  400       CONTINUE                          ! End loop over end wire hits
  300     CONTINUE                          ! End loop over beginning wire hits
C
C  Compare all temporary segments, choose best one
C Choose segments which point toward the IR first to help with the problem
C of getting wrong side segments.
C
  500     CONTINUE
          BESCHI = MAXCHI
          BESSEG = 0
          DO 203 II = 1,NTSEG
            IF (TBESCHI(II).LT.BESCHI) THEN
              IF (REQUIRE_SLOPE) THEN
                OK = .FALSE.
                IF (HALF.EQ.0) THEN
C North side segments should have negative slope.
                  IF (TSLOPE(II).LE.0.) OK = .TRUE.
                ELSE
C South side segments should have postive slope.
                  IF (TSLOPE(II).GE.0.) OK = .TRUE.
                END IF
              ELSE
                OK = .TRUE.
              END IF
              IF (OK) THEN
                BESCHI = TBESCHI(II)
                BESSEG = II
              ELSE
                IF (BESSEG.EQ.0) BESSEG = -1
              END IF
            ENDIF
  203     CONTINUE
C
          IF (BESSEG.EQ.0) THEN
            GOTO 600
          ELSE IF (BESSEG.EQ.-1) THEN
            REQUIRE_SLOPE = .FALSE.
            GO TO 500
          END IF
C
          IF (TNHIT(BESSEG) .LT. NBTSEN-INEFF) THEN
            TBESCHI(BESSEG) = 999999.
            GOTO 500
          END IF
C
C  Fill best one for loading
C
          NHIT = 0
          BESCHI = TBESCHI(BESSEG)
          SLOPE = TSLOPE(BESSEG)
          INTER = TINTER(BESSEG)
          DO 202 JJ = 1,TNHIT(BESSEG)
            NHIT = NHIT+1
            IIWIRE(NHIT) = TIIWIRE(BESSEG,NHIT)
            LRWIR(NHIT) = TLRWIR(BESSEG,NHIT)
            IIHIT(NHIT) = TIIHIT(BESSEG,NHIT)
            HITLOC(NHIT) = THITLOC(BESSEG,NHIT)
            RESID(NHIT) = TRESID(BESSEG,NHIT)
            XBEST(NHIT) = TX(BESSEG,NHIT)
            YBEST(NHIT) = TY(BESSEG,NHIT)
            IF( NHIT .EQ. 1 ) LR1 = TLR1(BESSEG)
  202     CONTINUE
C
          OK = .FALSE.
C If wire 0 hit is missing, look in neighboring sector
C If OK=.TRUE., segment has been loaded in FSEGW0.
          IF (IIWIRE(1).NE.0) THEN
            CALL FSEGW0(HALF,QUAD,SECTOR,NHIT,IIWIRE,LRWIR,IIHIT,OK)
          END IF
C
          IF (.NOT.OK) THEN
C
C  Determine various segment parameters
C
            CALL FTSGPAR(HALF,QUAD,SECTOR,IIWIRE(1),LRWIR(1),IIHIT(1),
     &        THETA,PHI,NDELAY)
C
C  Calculate correct residuals if requested.
C
            IF (CALRES) THEN
              CALL FRESID(XBEST,YBEST,NHIT,RESID)
            ENDIF
C
C  Good track segment.
C  Flag hits on track segment and calculate AVE_ION.
C  and do angle corrections.
C
            LKFTSC = GZFTSC(HALF,QUAD,SECTOR)
            NHUSED = 0
            AVE_ION = 0.
C
            AVE_HIT_WIRE = IQ(LKFTSC+1)/8.
            ERROR_FACTOR = FDC_ERROR_NHIT(AVE_HIT_WIRE,0)
            BESCHI = BESCHI / ERROR_FACTOR**2.
            ERROR = FDC_ERROR_SLOPE(SLOPE,0) * ERROR_FACTOR 
C
            SHIFT(0) = FDC_DRIFT_SLOPE(SLOPE,0)
            SHIFT(1) = FDC_DRIFT_SLOPE(SLOPE,1)
            DL_DIST = 0.
            DL_ERROR = 9999.
            DO 401 ID = 1,NHIT
              JH = LKFTSC+HITLOC(ID)-1
              JH = JH+(IIHIT(ID)-1)*IQ(LKFTSC+3)
              IH = IQ(JH+9)
              AVE_ION = AVE_ION + Q(JH+7)
              Q(JH+5) = ERROR
              IF (.NOT.BTEST(IH,2)) THEN
                Q(JH+2) = Q(JH+2) + SHIFT(0)
                IF ( Q(JH+3) .NE. 0 ) THEN
                  Q(JH+3) = Q(JH+3) + SHIFT(1)
                ENDIF
                IQ(JH+9) = IBSET(IH,2)             ! Flag hit ID
                NHUSED = NHUSED+1
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
            DO 505 IHIT = 1,NHIT
              IIHIT(IHIT) = IQ(LKFTSC+4+IQ(LKFTSC+2)+(LRWIR(IHIT)/2))+
     &                         ((IIHIT(IHIT)-1)*IQ(LKFTSC+3))
  505       CONTINUE
            CALL LDTSEG(HALF,QUAD,SECTOR,NHIT,LRWIR,IIHIT,
     &        PHI,THETA,RESID,SLOPE,INTER,BESCHI,AVE_ION,NDELAY,
     &        ERROR, DL_DIST, DL_ERROR)
C
C  Increment number of used hits in this sector
C
            IQ(LFLOC+2) = IQ(LFLOC+2)+NHUSED
C
          END IF
C
C  Reform pool if enough hits left else done
C
          DO I = 1, NTSEG
            N_LOST_HITS(I) = 0
          END DO
C
          TBESCHI(BESSEG) = 999999.
C
          DO 501 ISEG = 1,NTSEG
            IF (ISEG.NE.BESSEG) THEN
              DO 502 II = 1,TNHIT(BESSEG)
                DO 503 JJ = 1,TNHIT(ISEG)
                  IF( TIIWIRE(BESSEG,II).EQ.TIIWIRE(ISEG,JJ)) THEN
                    IF( TIIHIT(BESSEG,II).EQ.TIIHIT(ISEG,JJ)) THEN
C
C Mark used hit.
                      N_LOST_HITS(ISEG) = N_LOST_HITS(ISEG) + 1
                      LOST_HIT(ISEG,N_LOST_HITS(ISEG)) = JJ
                      IF (TNHIT(ISEG)-N_LOST_HITS(ISEG).LT.NBTSEN-INEFF)
     &                  GO TO 501
C
                    ENDIF
                  ENDIF
  503           CONTINUE
  502         CONTINUE
            END IF
  501     CONTINUE
C
C Check leftover segments.  Refit those with sufficient hits.
          DO ISEG = 1, NTSEG
            IF (N_LOST_HITS(ISEG).GT.0
     &        .AND.TBESCHI(ISEG).LT.999999.) THEN
              IF (TNHIT(ISEG)-N_LOST_HITS(ISEG).LT.NBTSEN-INEFF) THEN
                TBESCHI(ISEG) = 999999.
              ELSE
C
C Move hits in segment arrays
C
                DO LL = 1, N_LOST_HITS(ISEG)
                  TNHIT(ISEG) = TNHIT(ISEG) - 1
                  DO II = 1, LOST_HIT(ISEG,LL)-1
                    TMPX(II) = TX(ISEG,II)
                    TMPY(II) = TY(ISEG,II)
                  END DO
                  DO II = LOST_HIT(ISEG,LL), TNHIT(ISEG)
                    TIIWIRE(ISEG,II) = TIIWIRE(ISEG,II+1)
                    TIIHIT(ISEG,II) = TIIHIT(ISEG,II+1)
                    TLRWIR(ISEG,II) = TLRWIR(ISEG,II+1)
                    THITLOC(ISEG,II) = THITLOC(ISEG,II+1)
                    TX(ISEG,II) = TX(ISEG,II+1)
                    TY(ISEG,II) = TY(ISEG,II+1)
                    TMPX(II) = TX(ISEG,II+1)
                    TMPY(II) = TY(ISEG,II+1)
                  END DO
                  TIIWIRE(ISEG,TNHIT(ISEG)+1) = 0
                  TIIHIT(ISEG,TNHIT(ISEG)+1) = 0
                  TLRWIR(ISEG,TNHIT(ISEG)+1) = 0
                  THITLOC(ISEG,TNHIT(ISEG)+1) = 0
                  TX(ISEG,TNHIT(ISEG)+1) = 0.
                  TY(ISEG,TNHIT(ISEG)+1) = 0.
C
C Subsequent lost hits have now moved down one place.
                  DO JJ = LL+1, N_LOST_HITS(ISEG)
                    LOST_HIT(ISEG,JJ) = LOST_HIT(ISEG,JJ) - 1
                  END DO
C
                END DO
C
C Refit segment
                CALL SEGFIT(TMPX,TMPY,TNHIT(ISEG),TSLOPE(ISEG),
     &            TINTER(ISEG),TBESCHI(ISEG),TMPRES)
                DO I = 1, TNHIT(ISEG)
                  TRESID(ISEG,I) = TMPRES(I)
                END DO
C
              END IF
            END IF
          END DO
C
          GOTO 500
C
  600     CONTINUE
C
C  Accumulate unused hits.
C
          CALL THITAC(HALF,QUAD,SECTOR,0,NBTSEN-1,
     &      YL,ZL,LOC,HOHITS,ENOUGH)
          IF( .NOT.ENOUGH ) GOTO 999
C
  250   CONTINUE                          ! End loop over possible end wires
  200 CONTINUE                          ! End loop over possible begin wires
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
