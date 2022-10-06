      SUBROUTINE PSEGRD(HALF,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Form roads with hits within
C-                         Phi sector boundaries
C-
C-   Inputs  : HALF,SECTOR - Identify which sector being used
C-   Outputs :
C-   Controls:
C-
C-   Created   7-NOV-1989   Jeffrey Bantly
C-   Updated  20-MAR-1990   Jeffrey Bantly  general cleanup
C-   Updated  16-APR-1990   Susan Blessing  use chi**2/dof for segment
C-                            fitting rather than Cernlib PROB function
C-   Updated   3-MAY-1990   Jeffrey Bantly  pineff->phi only inefficiencies
C-   Updated   6-NOV-1990   Robert E. Avery  Add protection for too
C-                              many combinations, allow smaller road
C-                              near sense wire plane.
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS files
C-   Updated  17-JUN-1991   Susan K. Blessing  Add SLOPE,INTER,CHISQ for
C-    segment fit.  Add AVE_ION for average ionization/hit for segment.
C-   Updated  18-JUN-1991   Susan K. Blessing  Change MAXCHI to an RCP
C-    parameter.
C-   Updated  21-JUN-1991   Susan K. Blessing   Turn off hits in segments
C-    rather than the entire segment after one segment has been loaded.
C-    Refit segments with enough hits and try again.
C-   Updated  17-OCT-1991   Robert E. Avery  Add Position Error
C-    to segement bank.
C-    Include slope dependent corrections to position and error.
C-   Updated  22-NOV-1991   Susan K. Blessing  Remove MIN(MX_HIT_WIRE,HOHITS())
C-    from inner wire loop.  Not only was it wrong, it's unncessary.
C-    Count number of missed wires and leave loop when gt INEFF.
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block.
C-   Updated  12-MAR-1992   Susan K. Blessing  Just throw out road if
C-    too many combinations rather than rest of sector.
C-   Updated  12-MAY-1992   Susan K. Blessing  Bug fix.  NBTSEN was being
C-    used rather than NBPSEN when checking turned off segments.  Don't check
C-    loaded segment, just turn it off.
C-   Updated  29-MAY-1992   Susan K. Blessing  Use TOLDISP for tolerance
C-    to allow different values for theta and phi.  
C-    Remove unnecessary VZERO calls.
C-   Updated   7-JUL-1992   Robert E. Avery  Include dependence of
C-    error on number hits in sector.
C-   Updated  24-AUG-1992   Susan K. Blessing  Subsequent lost hits have 
C-    to be moved down one place.
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C-   Updated  18-OCT-1993   Robert E. Avery  Eliminate unnecessary 
C-    QHIT array (~3000 words).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER II,JJ,LL
      INTEGER HALF,SECTOR
      INTEGER NHITS(0:NBPSEN-1),IPTR(0:NBPSEN-1)
      INTEGER IIWIRE(NBPSEN),LRWIR(NBPSEN),IIHIT(NBPSEN)
      INTEGER HITLOC(NBPSEN),NHIT,NHITS1,IDRIFT,DRIFT,ISECT
      INTEGER TIIWIRE(MXNSEG,NBPSEN),TLRWIR(MXNSEG,NBPSEN)
      INTEGER TIIHIT(MXNSEG,NBPSEN),THITLOC(MXNSEG,NBPSEN)
      INTEGER I1,I2,J1,J2,IWIRE,IHIT,JWIRE,JHIT,LHIT,LR,LR1,NEL,NWORDS
      INTEGER ISTAT,NN,ICALL,TMPI(NBPSEN),JH,IH,NHUSED,ID
      INTEGER LOC(0:NBPSEN-1,MX_HIT_WIRE*2,3),BESSEG,NOKSEG,IBLOC
      INTEGER TLOC(NBPSEN,MX_HIT_WIRE*2,3),TLR1(MXNSEG)
      INTEGER TIMAX(NBPSEN),TNHIT(MXNSEG)
      INTEGER HOHITS(0:NBPSEN-1),MXWRBG,MXWREN
      INTEGER INEFF,IER,NOFF,MIN,WIRBEG,WIREND,IHITST,IHITEN
      INTEGER NLVL,MAXLVL,I(NBPSEN),IMAX(NBPSEN),ISEG
      INTEGER LKFPSC
      INTEGER WIRMIN
      INTEGER NUM_COMB
      INTEGER MAX_COMB
      INTEGER GZFPSC
      INTEGER N_LOST_HITS(MXNSEG),LOST_HIT(MXNSEG,NBPSEN)
      INTEGER NMISS
C
      REAL CONT(36)
      REAL STAT,A,B,CHISQ,D,RESID(16),BESCHI,MAXCHI,DELAY,EDELAY
      REAL SLOPE,INTER,AVE_ION
      REAL TBESCHI(MXNSEG),TRESID(MXNSEG,NBPSEN)
      REAL TSLOPE(MXNSEG),TINTER(MXNSEG)
      REAL YL(0:NBPSEN-1,MX_HIT_WIRE*2),ZL(0:NBPSEN-1,MX_HIT_WIRE*2)
      REAL TYL(NBPSEN,MX_HIT_WIRE*2),TZL(NBPSEN,MX_HIT_WIRE*2)
      REAL TMPX(NBPSEN),TMPY(NBPSEN),TMPRES(NBPSEN)
      REAL XBEST(NBPSEN),YBEST(NBPSEN)
      REAL TX(MXNSEG,NBPSEN),TY(MXNSEG,NBPSEN)
      REAL X(16),Y(16),XC,YC,ZC,RC,RHIT,ENDY,ENDZ,SLOPEM,INTERB
      REAL PHI1,PHI2,PHI,THETA,DISTANCE,LINEPOS
      REAL TOLDIS,STAGGER,DR1,Z1,ANGLE,COSANGLE
      REAL TOLERANCE
      REAL TOLDIS_CLOSE
      REAL CLOSE_DIST
      PARAMETER(  CLOSE_DIST =  0.1)
      REAL    ERROR, SHIFT(0:1)
      REAL    FDC_DRIFT_SLOPE
      REAL    FDC_ERROR_SLOPE
      REAL    ERROR_FACTOR
      REAL    AVE_HIT_WIRE 
      REAL    FDC_ERROR_NHIT
C
      EQUIVALENCE (ISTAT,STAT)
C
      LOGICAL ENOUGH, CALRES
C
      SAVE ICALL,INEFF,TOLDIS,CALRES
      SAVE MAX_COMB,TOLDIS_CLOSE
C
      DATA ICALL/0/
      DATA MAXCHI/10./
C
C----------------------------------------------------------------------
C
      IF (ICALL.EQ.0) THEN
        ICALL=1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_i('PINEFF',INEFF,IER)
        CALL EZGET('TOLDISP',TOLDIS,IER)
        CALL EZGET('TOLDIS_CLOSE',TOLDIS_CLOSE,IER)
        IF ( TOLDIS_CLOSE .EQ. 0) THEN
          TOLDIS_CLOSE = TOLDIS
        ENDIF
        CALL EZGET_l('CALRES',CALRES,IER)
        CALL EZGET_i('MAX_COMB',MAX_COMB,IER)
        CALL EZGET('MAX_PSEG_CHI',MAXCHI,IER)
        IF (MAXCHI.LE.0.) MAXCHI = 10.
        CALL EZRSET
      END IF
C
C   Check to see if at least one hit available in current sector to
C   start road.
C
      CALL GTFPSC(HALF,SECTOR,'WIR',0,NEL,NWORDS,CONT)
      CALL UCOPY(CONT,NHITS,NEL)
      CALL UCOPY(CONT(NEL+1),IPTR,NEL)
      DO 5 II=0,INEFF
        NHITS1 = NHITS1 + NHITS(II)
    5 CONTINUE
      IF( NHITS1 .LE. 0 ) GOTO 999      ! no hits to start road
C
C  Accumulate unused hits.
C
      CALL PHITAC(HALF,SECTOR,YL,ZL,LOC,HOHITS,ENOUGH)
      IF( .NOT.ENOUGH ) GOTO 999
C
C  Form segments across sector boundaries by choosing beginning wire and
C  ending wire in different, adjacent sectors and making a road.
C
      MXWRBG = NBPSEN
      MXWREN = -1
      DO 110 IWIRE=0,INEFF
        IF(HOHITS(IWIRE) .GT. 0 ) THEN
          MXWRBG = IWIRE
          GOTO 115
        ENDIF
  110 CONTINUE
  115 CONTINUE
      DO 120 IWIRE=NBPSEN-1,NBPSEN-1-INEFF,-1
        IF(HOHITS(IWIRE) .GT. 0 ) THEN
          MXWREN = IWIRE
          GOTO 125
        ENDIF
  120 CONTINUE
  125 CONTINUE
      IF( MXWREN-MXWRBG+1 .LE. NBPSEN-1-INEFF ) GOTO 999
C
C  Loop over every allowable beginning wire up to wirbeg=ineff
C
      DO 200 WIRBEG=MXWRBG,INEFF
C
C  Loop over every allowable beginning wire up to wirbeg=ineff
C
        WIRMIN=NBPSEN-1-INEFF+WIRBEG
        DO 250 WIREND=MXWREN,WIRMIN,-1
          LKFPSC=GZFPSC(HALF,SECTOR)
C
C  Loop over every unused hit in beginning wire, allowing only one possible
C  hit at this level.
C
          NOKSEG=0
          DO 300 IHITST=1,HOHITS(WIRBEG)
C
C  Loop over every unused hit in ending wire, allowing only one possible
C  hit at this level.
C
            DO 400 IHITEN=1,HOHITS(WIREND)
              JWIRE=1
              JHIT =1
              TYL(JWIRE,JHIT)=YL(WIRBEG,IHITST)
              TZL(JWIRE,JHIT)=ZL(WIRBEG,IHITST)
              TLOC(JWIRE,JHIT,1)=LOC(WIRBEG,IHITST,1)
              TLOC(JWIRE,JHIT,2)=LOC(WIRBEG,IHITST,2)
              TLOC(JWIRE,JHIT,3)=LOC(WIRBEG,IHITST,3)
              TIMAX(JWIRE)=JHIT
              ENDY=YL(WIREND,IHITEN)
              ENDZ=ZL(WIREND,IHITEN)
              IF( (ENDZ-TZL(1,1)) .EQ. 0. ) GOTO 400
              SLOPEM=(ENDY-TYL(1,1))/(ENDZ-TZL(1,1))
              INTERB=ENDY-(SLOPEM*ENDZ)
              ANGLE=ATAN2( (ENDY-TYL(1,1)),(ENDZ-TZL(1,1)) )
              COSANGLE = COS(ANGLE)
C
              SHIFT(0) = FDC_DRIFT_SLOPE(SLOPEM,0)
              SHIFT(1) = FDC_DRIFT_SLOPE(SLOPEM,1)
              TYL(JWIRE,JHIT) = TYL(JWIRE,JHIT)
     &          + SHIFT(LOC(WIRBEG,IHITST,3))
C
C  Loop over intervening wires
C
C Count number of missed/unused wires
              NMISS = NBPSEN - (WIREND-WIRBEG+1)
              DO 450 IWIRE=WIRBEG+1,WIREND-1
                IF (  HOHITS(IWIRE) .GT. 0 ) THEN
                  LINEPOS=(SLOPEM*ZL(IWIRE,1))+INTERB
                  IF ( ABS(LINEPOS) .LT. CLOSE_DIST) THEN
                    TOLERANCE = TOLDIS_CLOSE
                  ELSE
                    TOLERANCE = TOLDIS
                  ENDIF
                ENDIF
C
C  Loop over all unused hits in the intervening wires in the home sector,
C  allowing as many hits as available at each level that are within
C  TOLDIS of the road center.
C
                JHIT=0
                DO 475 IHIT=1,HOHITS(IWIRE)
                  DISTANCE=ABS( (LINEPOS-YL(IWIRE,IHIT))*COSANGLE )
                  IF(DISTANCE .LE. TOLERANCE) THEN
                    IF (JHIT .EQ. 0) JWIRE=JWIRE+1
                    JHIT=JHIT+1
                    TYL(JWIRE,JHIT) = YL(IWIRE,IHIT)
     &                + SHIFT(LOC(IWIRE,IHIT,3))
                    TZL(JWIRE,JHIT)=ZL(IWIRE,IHIT)
                    TLOC(JWIRE,JHIT,1)=LOC(IWIRE,IHIT,1)
                    TLOC(JWIRE,JHIT,2)=LOC(IWIRE,IHIT,2)
                    TLOC(JWIRE,JHIT,3)=LOC(IWIRE,IHIT,3)
                  ENDIF
  475           CONTINUE                    ! End of loop over intervening hits
                IF (JHIT.GT.0) TIMAX(JWIRE)=JHIT ! Set num hits per lvl
C
C Check if have missed too many wires
                IF (JHIT.EQ.0) NMISS = NMISS + 1
                IF (NMISS.GT.INEFF) GO TO 400
C
  450         CONTINUE                      ! End of loop over intervening wires
C
              JWIRE=JWIRE+1                 ! Load end wire hit for this loop.
              JHIT =1
              TYL(JWIRE,JHIT) = YL(WIREND,IHITEN)
     &          + SHIFT(LOC(WIREND,IHITEN,3))
              TZL(JWIRE,JHIT)=ZL(WIREND,IHITEN)
              TLOC(JWIRE,JHIT,1)=LOC(WIREND,IHITEN,1)
              TLOC(JWIRE,JHIT,2)=LOC(WIREND,IHITEN,2)
              TLOC(JWIRE,JHIT,3)=LOC(WIREND,IHITEN,3)
              TIMAX(JWIRE)=JHIT
C
              BESCHI = 999999.0
              MAXLVL = JWIRE
C
C Check number of possible combinations
              IF (MAX_COMB.GT.0) THEN
C
                NUM_COMB = 1
                DO 11 II=2,JWIRE
                  I(II) = 1
                  NUM_COMB = NUM_COMB * TIMAX(II)
C If too many cominations, give up on this road
                  IF (NUM_COMB .GT. MAX_COMB) THEN
                    CALL ERRMSG('FDC-too-many-phi-combs','PSEGRD',
     &                ' Too many hit combinations in Phi segment road',
     &                'W')
                    GO TO 400
                  ENDIF
   11           CONTINUE
              END IF
C
              DO II=2,JWIRE
                I(II) = 1
              END DO
C
              I(1) = 0
              NLVL = 1
  100         CONTINUE                          ! Begin loop over possible roads
              I(NLVL) = I(NLVL) + 1
              IF( I(NLVL) .LE. TIMAX(NLVL) ) THEN
                NLVL = 1
                DO 12 JJ=1,JWIRE
                  Y(JJ) = TYL(JJ,I(JJ))
                  X(JJ) = TZL(JJ,I(JJ))
   12           CONTINUE
                CALL SEGFIT(X,Y,MAXLVL,A,B,CHISQ,RESID)
                IF( CHISQ .LT. BESCHI .AND. CHISQ .GT. 0.0 ) THEN
                  BESCHI = CHISQ
                  SLOPE = A
                  INTER = B
                  DO 13 JJ=1,JWIRE
                    TMPI(JJ)=I(JJ)
                    TMPRES(JJ)=RESID(JJ)
                    TMPX(JJ)=X(JJ)
                    TMPY(JJ)=Y(JJ)
   13             CONTINUE
                ENDIF
              ELSE
                NLVL = NLVL + 1
                I(NLVL-1) = 1
                IF( NLVL .GT. MAXLVL ) GOTO 150
              ENDIF
              GOTO 100
C
  150         CONTINUE                          ! Store a segment if chisq<cut
              IF ( (BESCHI .GT. MAXCHI) .OR. (BESCHI.LE.0.) ) GOTO 400
C
C  TEMPORARILY Record as a segment.
C
              NOKSEG=NOKSEG+1
              NHIT=0
              TBESCHI(NOKSEG)=BESCHI
              TSLOPE(NOKSEG) = SLOPE
              TINTER(NOKSEG) = INTER
              DO 201 JJ=1,JWIRE
                IF(TMPI(JJ) .GT. TIMAX(JJ)) GOTO 201
                NHIT=NHIT+1
                TIIWIRE(NOKSEG,NHIT) = TLOC(JJ,TMPI(JJ),1)
                TLRWIR(NOKSEG,NHIT)  =
     &                        TLOC(JJ,TMPI(JJ),1)*2+TLOC(JJ,TMPI(JJ),3)
                TIIHIT(NOKSEG,NHIT)  = TLOC(JJ,TMPI(JJ),2)
                THITLOC(NOKSEG,NHIT) = IQ(LKFPSC+4+IQ(LKFPSC+2)+
     &                                       TIIWIRE(NOKSEG,NHIT))
                TRESID(NOKSEG,NHIT)  = TMPRES(JJ)
                TX(NOKSEG,NHIT)      = TMPX(JJ)
                TY(NOKSEG,NHIT)      = TMPY(JJ)
                IF( NHIT .EQ. 1 ) TLR1(NOKSEG) = TLOC(JJ,TMPI(JJ),3)
  201         CONTINUE
              TNHIT(NOKSEG)=NHIT
C
              IF(NOKSEG.GE.MXNSEG) GOTO 500
  400       CONTINUE                          ! End loop over end wire hits
  300     CONTINUE                          ! End loop over beginning wire hits
C
C  Compare all temporary segments, choose best one
C
  500     CONTINUE
          LKFPSC=GZFPSC(HALF,SECTOR)
          BESCHI=999999.0
          BESSEG=0
          DO 203 II=1,NOKSEG
            IF( TBESCHI(II).LT.BESCHI .AND. TBESCHI(II).GT.0.0) THEN
              BESCHI=TBESCHI(II)
              BESSEG=II
            ENDIF
  203     CONTINUE
          IF(BESSEG.LE.0) GOTO 600
C
          IF (TNHIT(BESSEG) .LT. NBPSEN-INEFF) THEN
            TBESCHI(BESSEG) = 999999.
            GOTO 500
          END IF
C
C  Fill best one for loading
C
          NHIT=0
          BESCHI= TBESCHI(BESSEG)
          SLOPE = TSLOPE(BESSEG)
          INTER = TINTER(BESSEG)
          DO 202 JJ=1,TNHIT(BESSEG)
            NHIT=NHIT+1
            IIWIRE(NHIT)=TIIWIRE(BESSEG,NHIT)
            LRWIR(NHIT) =TLRWIR(BESSEG,NHIT)
            IIHIT(NHIT) =TIIHIT(BESSEG,NHIT)
            HITLOC(NHIT)=THITLOC(BESSEG,NHIT)
            RESID(NHIT) =TRESID(BESSEG,NHIT)
            XBEST(NHIT) =TX(BESSEG,NHIT)
            YBEST(NHIT) =TY(BESSEG,NHIT)
            IF( NHIT .EQ. 1 ) LR1 = TLR1(BESSEG)
  202     CONTINUE
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
          AVE_ION = 0.
          LKFPSC=GZFPSC(HALF,SECTOR)
          NHUSED=0
          DR1=0.
C
          AVE_HIT_WIRE = IQ(LKFPSC+1)/16.
          ERROR_FACTOR = FDC_ERROR_NHIT(AVE_HIT_WIRE,1)
          BESCHI = BESCHI / ERROR_FACTOR**2.
          ERROR = FDC_ERROR_SLOPE(SLOPE,1) * ERROR_FACTOR 
C
          SHIFT(0) = FDC_DRIFT_SLOPE(SLOPE,0)
          SHIFT(1) = FDC_DRIFT_SLOPE(SLOPE,1)
          DO 401 ID=1,NHIT
            JH=LKFPSC+HITLOC(ID)-1
            JH=JH+(IIHIT(ID)-1)*IQ(LKFPSC+3)
            IH=IQ(JH+9)
            AVE_ION = AVE_ION + Q(JH+7)
            Q(JH+5) = ERROR
            IF (.NOT.BTEST(IH,2)) THEN
              Q(JH+2) = Q(JH+2) + SHIFT(0)
              Q(JH+3) = Q(JH+3) + SHIFT(1)
              IQ(JH+9)=IBSET(IH,2)             ! Flag hit ID
              NHUSED=NHUSED+1
            END IF
            IF(ID.EQ.1) DR1=Q(JH+2+LR1)
  401     CONTINUE
          AVE_ION = AVE_ION/FLOAT(NHIT)
C
C  Store this segment in Zebra bank
C
          PHI=0.08727+0.17453*FLOAT(SECTOR)       ! add in drift %
          Z1=XBEST(1) 
          DO 505 IHIT=1,NHIT
            IIHIT(IHIT)=IQ(LKFPSC+4+IQ(LKFPSC+2)+(LRWIR(IHIT)/2))+
     &                         ((IIHIT(IHIT)-1)*IQ(LKFPSC+3))
  505     CONTINUE
          CALL LDPSEG(HALF,SECTOR,NHIT,LRWIR,IIHIT,PHI,DR1,Z1,RESID,
     &      SLOPE,INTER,BESCHI,AVE_ION,ERROR)
C
C  Increment number of used hits in this sector
C
          IQ(LFLOC+2)=IQ(LFLOC+2)+NHUSED
C
C       REFORM POOL IF ENOUGH HITS LEFT ELSE DONE
C
          DO II = 1, NOKSEG
            N_LOST_HITS(II) = 0
          END DO
C
          TBESCHI(BESSEG) = 999999.
C
          DO 501 ISEG=1,NOKSEG
            IF (ISEG.NE.BESSEG) THEN
              DO 502 II=1,TNHIT(BESSEG)
                DO 503 JJ=1,TNHIT(ISEG)
                  IF( TIIWIRE(BESSEG,II).EQ.TIIWIRE(ISEG,JJ)) THEN
                    IF( TIIHIT(BESSEG,II).EQ.TIIHIT(ISEG,JJ)) THEN
C
C Mark used hit.
                      N_LOST_HITS(ISEG) = N_LOST_HITS(ISEG) + 1
                      LOST_HIT(ISEG,N_LOST_HITS(ISEG)) = JJ
                      IF (TNHIT(ISEG)-N_LOST_HITS(ISEG).LT.NBPSEN-INEFF)
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
          DO ISEG = 1, NOKSEG
            IF (N_LOST_HITS(ISEG).GT.0
     &        .AND.TBESCHI(ISEG).LT.999999.) THEN
              IF (TNHIT(ISEG)-N_LOST_HITS(ISEG).LT.NBPSEN-INEFF) THEN
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
                DO II = 1, TNHIT(ISEG)
                  TRESID(ISEG,II) = TMPRES(II)
                END DO
C
              END IF
            END IF
          END DO
C
          GOTO 500
C
C  Accumulate unused hits.
C
  600     CONTINUE
          CALL PHITAC(HALF,SECTOR,YL,ZL,LOC,HOHITS,ENOUGH)
          IF( .NOT.ENOUGH ) GOTO 999
C
  250   CONTINUE                          ! End loop over possible end wires
  200 CONTINUE                          ! End loop over possible begin wires
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
