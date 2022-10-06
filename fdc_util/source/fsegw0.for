C VAX/DEC CMS REPLACEMENT HISTORY, Element FSEGW0.FOR
C *1     4-NOV-1993 10:58:41 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FSEGW0.FOR
      SUBROUTINE FSEGW0(HALF,QUAD,SECTOR,NH,
     &  WIRE_LIST,LRWIR_LIST,HIT_LIST,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Try to put wire 0 and wire 1 hits from
C-    the neighboring sector on segments
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-FEB-1992   Susan K. Blessing
C-   Updated  13-MAY-1993   Susan K. Blessing  Call FTSGPAR rather than
C-    calculating stuff here.  Bug fix.
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C-   Updated  18-OCT-1993   Robert E. Avery  Eliminate unnecessary 
C-    QHIT array (~3000 words).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER HALF,QUAD,SECTOR
      INTEGER NH,WIRE_LIST(NBTSEN),LRWIR_LIST(NBTSEN),HIT_LIST(NBTSEN)
      INTEGER SECTDR
      INTEGER IPTR(0:NBTSEN-1),DIR
      INTEGER NHIT,IIWIRE(NBTSEN),LRWIR(NBTSEN),IIHIT(NBTSEN)
      INTEGER HITLOC(NBTSEN),LHIT0,IDRIFT,NSECTOR
      INTEGER IWIRE,IHIT,JWIRE,JHIT,LHIT,LR,LR1,NEL,NWORDS
      INTEGER ISTAT,II,JJ,TMPI(NBTSEN),JH,IH,NHUSED,ID
      INTEGER LOC(0:NBTSEN-1,MX_HIT_WIRE*2,3),IHITD,JWIRED,JWIREH
      INTEGER NLOC(0:NBTSEN-1,MX_HIT_WIRE*2,3)
      INTEGER TLOC(NBTSEN,MX_HIT_WIRE*2,3)
      INTEGER TIMAX(NBTSEN),TIHMAX(NBTSEN),TIDMAX(NBTSEN)
      INTEGER NHITS(0:NBTSEN-1)
      INTEGER INEFF,IER,NOFF,WIRBEG,WIREND,IHITST,IHITEN
      INTEGER NLVL,MAXLVL,I(NBTSEN)
      INTEGER LBANK,LBANKN
      INTEGER NDELAY
      INTEGER GZFTSC
      INTEGER NMISS
C
      REAL    MAX_XT_SLOPE
      REAL    RESID(16),TMPRES(16)
      REAL    STAT,A,B,CHISQ,BESCHI,MAXCHI,DELAY,EDELAY
      REAL    SLOPE,INTER,AVE_ION
      REAL    YL(0:NBTSEN-1,MX_HIT_WIRE*2), ZL(0:NBTSEN-1,MX_HIT_WIRE*2)
      REAL    NYL(0:NBTSEN-1,MX_HIT_WIRE*2)
      REAL    NZL(0:NBTSEN-1,MX_HIT_WIRE*2)
      REAL    TYL(NBTSEN,MX_HIT_WIRE*2),TZL(NBTSEN,MX_HIT_WIRE*2)
      REAL    X(16),Y(16)
      REAL    XC,YC,ZC,RC,RHIT,ENDY,ENDZ,SLOPEM,INTERB,ANGLE
      REAL    PHI1,PHI2
      REAL    PHI,THETA
      REAL    DISTANCE
      REAL    LINEPOS,TOLXDI
      REAL    ERROR, SHIFT(0:1)
      REAL    FDC_DRIFT_SLOPE
      REAL    FDC_ERROR_SLOPE
      REAL    ERROR_FACTOR
      REAL    DL_DIST, DL_ERROR
C
      EQUIVALENCE (ISTAT,STAT)
C
      LOGICAL OK,FIRST
      LOGICAL ENOUGH
C
      SAVE FIRST,INEFF,TOLXDI,MAX_XT_SLOPE,MAXCHI,ERROR_FACTOR
C
      DATA FIRST/.TRUE./
      DATA MAX_XT_SLOPE/10./
      DATA MAXCHI/100000./
      DATA ERROR_FACTOR /1.0/
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_i('TINEFF',INEFF,IER)
        CALL EZGET('TOLXDI',TOLXDI,IER)
        CALL EZGET('MAX_XT_SLOPE',MAX_XT_SLOPE,IER)
        CALL EZGET('MAX_XTSEG_CHI',MAXCHI,IER)
        CALL EZGET('ERRF_XSECT_THETA',ERROR_FACTOR,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      OK = .FALSE.
C
C Look in +1 sector in inner theta, -1 sector in outer theta.
      IF (QUAD.LE.4) THEN
        DIR = 1
      ELSE
        DIR = -1
      END IF
C
      NSECTOR = SECTOR + DIR
      IF (NSECTOR.LT.0 .OR. NSECTOR.GT.5) GO TO 999
C
C  Accumulate unused hits from neighboring sector.  Only need hits from 
C  wires 0 through INEFF-1.
C
      CALL THITAC(HALF,QUAD,NSECTOR,0,INEFF-1,NYL,NZL,NLOC,NHITS,ENOUGH)
      IF (NHITS(0).EQ.0) GO TO 999
C
C Get hit location information for starting hits.
      CALL FHITYZ(HALF,QUAD,SECTOR,NH,
     &  WIRE_LIST,LRWIR_LIST,HIT_LIST,YL,ZL,LOC)
C
C  Form segments across sector boundaries by choosing beginning wire in
C  neighboring sector and making a road.
C
C Looking for a wire 0 hit, so want to begin there
      WIRBEG = 0
C
C Ending wire is last wire in original sector
      WIREND = WIRE_LIST(NH)
C
C  Loop over every unused hit in beginning wire, allowing only one possible
C  hit at this level.
C
      DO 300 IHITST = 1,NHITS(WIRBEG)
C
C Only interested in particular ending hit, numbered 1 in YL and ZL arrays
C
        CALL VZERO(TIMAX,NBTSEN)
        CALL VZERO(TIHMAX,NBTSEN)
        CALL VZERO(TIDMAX,NBTSEN)
        JWIRE = 1
        JHIT = 1
        TYL(JWIRE,JHIT) = NYL(WIRBEG,IHITST)
        TZL(JWIRE,JHIT) = NZL(WIRBEG,IHITST)
        TLOC(JWIRE,JHIT,1) = NLOC(WIRBEG,IHITST,1)
        TLOC(JWIRE,JHIT,2) = NLOC(WIRBEG,IHITST,2)
        TLOC(JWIRE,JHIT,3) = NLOC(WIRBEG,IHITST,3)
        TIHMAX(JWIRE) = JHIT
        TIMAX(JWIRE) = JHIT
        ENDY = YL(WIREND,1)
        ENDZ = ZL(WIREND,1)
        IF ((ENDZ-TZL(1,1)) .EQ. 0.) GOTO 300
        SLOPEM = (ENDY-TYL(1,1))/(ENDZ-TZL(1,1))
C
        IF (ABS(SLOPEM).GT.MAX_XT_SLOPE) GO TO 300
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
C
C  Loop over all unused hits in the intervening wires in the neighbor sector,
C  allowing as many hits as available at each level that are within
C  TOLXDI of the road center.
C
          JHIT = 0
          IF (IWIRE.LT.WIRE_LIST(1)) THEN
C
            IF (NHITS(IWIRE).GT.0) LINEPOS = (SLOPEM*ZL(IWIRE,1))+INTERB
            DO 475 IHIT = 1,NHITS(IWIRE)
              DISTANCE = ABS((LINEPOS-NYL(IWIRE,IHIT))*COS(ANGLE))
              IF (DISTANCE .LE. TOLXDI) THEN
                IF (JHIT .EQ. 0) JWIRE = JWIRE+1
                JHIT = JHIT+1
                TYL(JWIRE,JHIT) = NYL(IWIRE,IHIT)
                TZL(JWIRE,JHIT) = NZL(IWIRE,IHIT)
                TLOC(JWIRE,JHIT,1) = NLOC(IWIRE,IHIT,1)
                TLOC(JWIRE,JHIT,2) = NLOC(IWIRE,IHIT,2)
                TLOC(JWIRE,JHIT,3) = NLOC(IWIRE,IHIT,3)
              ENDIF
  475       CONTINUE                    ! End of loop over hits intervening
            IF (JHIT.GT.0) TIHMAX(JWIRE) = JHIT ! Set max num home hits per level
C
          ELSE
C
C Check if this wire was used in original segment
            IF (LOC(IWIRE,1,2).EQ.1) THEN
C
              LINEPOS = (SLOPEM*ZL(IWIRE,1))+INTERB
C
C Only one hit per wire in original sector arrays
              IHIT = 1
C
              DISTANCE = ABS((LINEPOS-YL(IWIRE,IHIT))*COS(ANGLE))
              IF (DISTANCE .LE. TOLXDI) THEN
                IF (JHIT .EQ. 0) JWIRE = JWIRE+1
                JHIT = JHIT+1
                TYL(JWIRE,JHIT) = YL(IWIRE,IHIT)
                TZL(JWIRE,JHIT) = ZL(IWIRE,IHIT)
                TLOC(JWIRE,JHIT,1) = LOC(IWIRE,IHIT,1)
                TLOC(JWIRE,JHIT,2) = LOC(IWIRE,IHIT,2)
                TLOC(JWIRE,JHIT,3) = LOC(IWIRE,IHIT,3)
              END IF
            END IF
          END IF
C
          IF (JHIT.GT.0) TIDMAX(JWIRE) = JHIT-TIHMAX(JWIRE) ! Num home hits/lvl
          IF (JHIT.GT.0) TIMAX(JWIRE) = JHIT ! Set max num all hits per level
C
C Check if have missed too many wires
          IF (JHIT.EQ.0) NMISS = NMISS + 1
          IF (NMISS.GT.INEFF) GO TO 300
C
  450   CONTINUE                      ! End of loop over intervening wires
C
        JWIRE = JWIRE+1                 ! Load end wire hit for this loop.
        JHIT = 1
        TYL(JWIRE,JHIT) = YL(WIREND,1)
        TZL(JWIRE,JHIT) = ZL(WIREND,1)
        TLOC(JWIRE,JHIT,1) = LOC(WIREND,1,1)
        TLOC(JWIRE,JHIT,2) = LOC(WIREND,1,2)
        TLOC(JWIRE,JHIT,3) = LOC(WIREND,1,3)
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
   11   CONTINUE
        I(1) = 0
        TMPI(1) = 0
        NLVL = 1
  100   CONTINUE                          ! Begin loop over possible roads
        I(NLVL) = I(NLVL) + 1
        IF (I(NLVL) .LE. TIMAX(NLVL)) THEN
          NLVL = 1
          DO 12 JJ = 1,JWIRE
            Y(JJ) = TYL(JJ,I(JJ)) + SHIFT(TLOC(JJ,I(JJ),3))
            X(JJ) = TZL(JJ,I(JJ))
   12     CONTINUE
          CALL SEGFIT(X,Y,MAXLVL,A,B,CHISQ,RESID)
          IF (CHISQ .LT. BESCHI .AND. CHISQ .GT. 0.0) THEN
            BESCHI = CHISQ
            SLOPE = A
            INTER = B
            DO 13 JJ = 1,JWIRE
              TMPI(JJ) = I(JJ)
              TMPRES(JJ) = RESID(JJ)
   13       CONTINUE
          ENDIF
        ELSE
          NLVL = NLVL + 1
          I(NLVL-1) = 1
          IF (NLVL .GT. MAXLVL) GOTO 200
        ENDIF
        GOTO 100
C
  200   CONTINUE                          ! Store a segment if chisq<cut
C
        IF ((BESCHI .GT. MAXCHI) .OR. (BESCHI.LE.0.)) GOTO 300
C
C  Record as a segment.
C
        NHIT = 0
        LBANK = GZFTSC(HALF,QUAD,NSECTOR)
        IHITD = 0
        DO 201 JJ = 1,JWIRE
          IF (TMPI(JJ) .GT. TIMAX(JJ)) GOTO 201
          NHIT = NHIT+1
          IF (TMPI(JJ) .GT. TIHMAX(JJ) .AND. IHITD.EQ.0) THEN
            LBANK = GZFTSC(HALF,QUAD,SECTOR)
            IHITD = NHIT                ! First hit in other sector
          ENDIF
          IF (IHITD.GT.0 .AND. TMPI(JJ).LE.TIHMAX(JJ)) THEN
C               !  Cross-back at hit JJ
            GOTO 300
          ENDIF
          IIWIRE(NHIT) = TLOC(JJ,TMPI(JJ),1)
          LRWIR(NHIT)  = TLOC(JJ,TMPI(JJ),1)*2+TLOC(JJ,TMPI(JJ),3)
          IIHIT(NHIT)  = TLOC(JJ,TMPI(JJ),2)
          HITLOC(NHIT) = IQ(LBANK+4+IQ(LBANK+2)+IIWIRE(NHIT))
          RESID(NHIT)  = TMPRES(JJ)
          IF (NHIT .EQ. 1) LR1 = TLOC(JJ,TMPI(JJ),3)
  201   CONTINUE
C
        IF (NHIT .LT. NBTSEN-INEFF) GOTO 300
C
        CALL FTSGPAR(HALF,QUAD,NSECTOR,IIWIRE(1),LRWIR(1),IIHIT(1),
     &    THETA,PHI,NDELAY)
C
C  Good track segment.
C  Flag hits on track segment and calculate AVE_ION,
C  and do angle correction.
C
        NHUSED = 0
        AVE_ION = 0.
        LBANKN  = GZFTSC(HALF,QUAD,NSECTOR)
        LBANK = GZFTSC(HALF,QUAD,SECTOR)
        IF (LBANKN.LE.0 .OR. LBANK.LE.0) THEN
          GOTO 300
        ENDIF
        ERROR = FDC_ERROR_SLOPE(SLOPE,0) * ERROR_FACTOR
        BESCHI = BESCHI / ERROR_FACTOR**2.
        SHIFT(0) = FDC_DRIFT_SLOPE(SLOPE,0)
        SHIFT(1) = FDC_DRIFT_SLOPE(SLOPE,1)
        DL_DIST = 0.0
        DL_ERROR = 9999.
        DO 401 ID = 1,NHIT
          JH = HITLOC(ID)-1
          IF (ID .GE. IHITD) THEN
            JH = LBANK+JH+(IIHIT(ID)-1)*IQ(LBANK+3)
          ELSE
            JH = LBANKN+JH+(IIHIT(ID)-1)*IQ(LBANKN+3)
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
            NHUSED = NHUSED+1
          END IF
          IF (BTEST(IH,0) .OR. BTEST(IH,1)) THEN
            DL_DIST = Q(JH+4)
            DL_ERROR = Q(JH+6)
          ENDIF
  401   CONTINUE
        AVE_ION = AVE_ION/FLOAT(NHIT)
C
C  Store this segment in Zebra bank
C
        SECTDR = -1 * (NSECTOR + 1000*IHITD)*DIR
        DO 505 ID = 1,IHITD-1
          IIHIT(ID) = IQ(LBANKN+4+IQ(LBANKN+2)+(IIWIRE(ID)))+
     &                         ((IIHIT(ID)-1)*IQ(LBANKN+3))
  505   CONTINUE
        DO 510 ID = IHITD,NHIT
          IIHIT(ID) = IQ(LBANK+4+IQ(LBANK+2)+(IIWIRE(ID)))+
     &                         ((IIHIT(ID)-1)*IQ(LBANK+3))
  510   CONTINUE
C
        CALL LDTSEG(HALF,QUAD,SECTDR,NHIT,LRWIR,IIHIT,
     &        PHI,THETA,RESID,SLOPE,INTER,BESCHI,AVE_ION,NDELAY,
     &        ERROR, DL_DIST, DL_ERROR)
C
C  Increment number of used hits in this sector
C
        IQ(LFLOC+2) = IQ(LFLOC+2)+NHUSED
C
        OK = .TRUE.
        GO TO 999
C
  300 CONTINUE                          ! End loop over hits on wire 0
C----------------------------------------------------------------------
C
  999 RETURN
      END
