      FUNCTION FDC_XSECT_HISTS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill FDC X-Sector histograms
C-      for determination of drift velocity.
C-
C-   Inputs  : none
C-   Outputs : histograms
C-
C-   Created  16-NOV-1990   Robert E. Avery
C-   Updated  24-MAR-1992   Robert E. Avery  Clean up for online use.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      LOGICAL FDC_XSECT_HISTS
C
      INTEGER IADD                      
      INTEGER MODULE,LAYER              
      INTEGER H
      INTEGER HALF,UNIT,QDRT,SCTR,WIRE,UBIT
      INTEGER SCTR_HOME
      INTEGER SCTR_XSECT
      INTEGER HIT_XSECT
      INTEGER WIRE_XSECT
      INTEGER NSEG,SEG
      INTEGER NHIT,HIT
      INTEGER NSEN
      INTEGER PTR_SC
      INTEGER NZBANK
      INTEGER IER,I,LR
      INTEGER NHTRK
      INTEGER SEG_TRK
      INTEGER IADD_TRK
      INTEGER HIT_TRK
      INTEGER IADD_SEG
      INTEGER HALF_TRK
      INTEGER ID,ID_QUAD
      INTEGER ITRK
      INTEGER PHI_OCT(0:35,0:1)        ! TRANSLATION FROM SECT TO HV OCTANT
      INTEGER LSGMT                     
      INTEGER LFSEG,LFXSC
      INTEGER LFDCT,LFDTH
      INTEGER GZFSEG,GZFXSC             
      INTEGER LZFIND
C
      INTEGER MAX_POINTS
      PARAMETER( MAX_POINTS = 16 )
C
      REAL    XC,YC,ZC
      REAL    XD,YD,ZD,Z0
      REAL    Z(MAX_POINTS ), X(MAX_POINTS )
      REAL    A,FIT_SLOPE,DISP,DISP_ERR
      REAL    CHISQ,RESID(MAX_POINTS)
      REAL    RADIUS
      REAL    FSTAGR,STAGGER
      REAL    MAX_DRDIST
      REAL    TH_DRDIST
      REAL    X_DRIFT
      REAL    ETZERO,ATZERO,VELOP,VELOM,VELOP_HOME
      REAL    VELOC
      REAL    MAXCHISQ_XSECT
      REAL    SLOPE_CUT
      REAL    SIN_5_DEG 
C
      LOGICAL FOUND_HIT
      LOGICAL SHORT_HISTS 
      LOGICAL FIRST 
C
      INTEGER ICONT(62)
      REAL CONT(62)                     ! CONTENTS OF FSGx BANKS
      EQUIVALENCE (CONT,ICONT)
C
      DATA FIRST /.TRUE./
      DATA MAXCHISQ_XSECT  /50./
      DATA SLOPE_CUT /0.40/
      DATA SHORT_HISTS /.TRUE./
      DATA PHI_OCT  /2,2,2,2,1,1,1,1,8,8,8,8,8,7,7,7,7,7,
     &               6,6,6,6,5,5,5,5,4,4,4,4,4,3,3,3,3,3,
     &               7,7,7,7,7,8,8,8,8,8,1,1,1,1,2,2,2,2,
     &               3,3,3,3,3,4,4,4,4,4,5,5,5,5,6,6,6,6/
C----------------------------------------------------------------------
      FDC_XSECT_HISTS = .TRUE.
      IF (FIRST) THEN
        CALL EZPICK('FDC_RCP')
        CALL EZGET('FB_MXCHSQ_XSECT ',MAXCHISQ_XSECT ,IER)
        CALL EZGET('FB_SLOPE_CUT',SLOPE_CUT,IER)
        CALL EZGET('FB_SHORT_HISTS',SHORT_HISTS,IER)
        CALL EZRSET
        CALL GTFALH(0,0,5,5,0,XC,YC,ZC)
        CALL GTFALH(0,0,5,4,0,XD,YD,ZD)
        TH_DRDIST = (YC-YD)/2.
        SIN_5_DEG = SIN(5.*RADIAN)
        FIRST = .FALSE.
      END IF
C
      CALL DHDIR('FDC_RCP','FDC_XSECT',IER,' ')
C
      DO HALF = 0, MXHALF
        DO LAYER = 0, 2
          IF (LAYER.LE.1) THEN
            NSEN = NBTSEN
          ELSE
            NSEN = NBPSEN
          END IF
          MODULE = 3*HALF + LAYER
          LFSEG = GZFSEG(HALF,LAYER)    ! LFSEG=0 MEANS NO SEGMENTS FOUND
          IF (LFSEG.GT.0) THEN
            NSEG = NZBANK(IXCOM,LFSEG)
            DO SEG = 1, NSEG
C
C Get segment info
C
              CALL GTFSEG(MODULE,SEG,CONT)
              IADD = ICONT(2)
              CALL FCODER(IADD,H,UNIT,QDRT,SCTR,WIRE,UBIT,1)
              NHIT = ICONT(3)
              HIT_XSECT = ICONT(1)/1000
              SCTR_HOME = SCTR
              SCTR_XSECT = SCTR + SIGN(1,HIT_XSECT)
C
C Only X-Sect segs, and don't use Theta cell 1/2 boundary.
C
              IF ( HIT_XSECT.EQ.0 ) GOTO 300
              IF ( UNIT .EQ. 0 ) THEN
                IF ( ((SCTR_HOME.EQ.1).AND.(SCTR_XSECT.EQ.2))
     &          .OR. ((SCTR_HOME.EQ.2).AND.(SCTR_XSECT.EQ.1)) ) THEN
                  GOTO 300
                ENDIF
              ENDIF
C
C Get pointers to hit banks, FXSC
C
              LFXSC = GZFXSC(HALF,UNIT,QDRT,SCTR)
              IF ( LFXSC .EQ. 0 )THEN
                CALL ERRMSG('NOLFXSC','FDC_XSECT_HISTS',
     &                ' NO hit bank for Segment','W')
                GOTO 300
              ENDIF
C
C Get pointer to fdct track (needed for Phi)
C
              NHTRK = 0
              LSGMT = LZFIND(IXCOM,LFSEG,SEG,-5)
              LFDCT = LQ(LSGMT-1)
              IF ( LFDCT.GT.0 ) THEN
                LFDTH=LQ(LFDCT-1)
                IF ( LFDTH.GT.0 ) THEN
                  ITRK = IQ(LFDCT-5)
                  NHTRK =  IQ(LFDCT+2)
                ENDIF
              ENDIF
              IF ( (UNIT.EQ.1) .AND. (NHTRK.LE.0) ) THEN
                GOTO 300
              ENDIF
C
C Accumulate hits on segment
C
              DO HIT = 1, NHIT
                WIRE = INT(CONT(3+HIT)/2.)
                LR = CONT(3+HIT) - WIRE*2
C
C Check if hit is in next sector:
C
                IF (HIT .EQ. ABS(HIT_XSECT) )  THEN
                  WIRE_XSECT = WIRE
                  SCTR = SCTR_XSECT
                  CALL FCODER(IADD,HALF,UNIT,QDRT,SCTR,0,0,2)
                  LFXSC = GZFXSC(HALF,UNIT,QDRT,SCTR)
                  IF ( LFXSC .EQ. 0 ) THEN
                    CALL ERRMSG('NOLFXSC','FDC_XSECT_HISTS',
     &                ' NO hit bank for X-sect hit','W')
                    GOTO 300
                  ENDIF
                ENDIF
C
C Save hit
C
                CALL GTFALH(HALF,UNIT,QDRT,SCTR,WIRE,XC,YC,ZC)
                Z(HIT) = ZC
                IF ( UNIT .EQ.0 ) THEN
                  PTR_SC = LFXSC + CONT(3+HIT+NSEN)        ! POINTER in FXSC
                  X_DRIFT = Q(PTR_SC+2+LR)
                  IF (SCTR .EQ. 1) X_DRIFT = -X_DRIFT
C
                  RADIUS=SQRT((XC)**2. + (YC)**2.)
                  STAGGER=FSTAGR(HALF,UNIT,QDRT,SCTR,WIRE)
                  X(HIT) = X_DRIFT +RADIUS-STAGGER
                ELSE
C
C for Phi fill with track residual
C
                  IADD_SEG = IADD*2 + CONT(3+HIT)
                  FOUND_HIT = .FALSE.
                  DO HIT_TRK =  1, NHTRK
                    IADD_TRK = IQ(LFDTH-2+HIT_TRK*3)
                    IF ( IADD_SEG .EQ. IADD_TRK ) THEN
                      FOUND_HIT = .TRUE.
                      GOTO 200
                    ENDIF
                  ENDDO
  200             CONTINUE
                  IF ( FOUND_HIT ) THEN
                    X(HIT) = Q(LFDTH+HIT_TRK*3)
                  ELSE
                    X(HIT) = 9999.
                    CALL ERRMSG('NOHIT','FDC_XSECT_HISTS',
     &                ' NO track hit corres. to sect seg. hit','W')
                  ENDIF
                ENDIF
              ENDDO
C
C Do cross sector fit to two segment peices:
C
              CALL FTWOSEGFIT(Z,X,NHIT,ABS(HIT_XSECT)-1,
     &                A,FIT_SLOPE,DISP,DISP_ERR,CHISQ,RESID)
              DISP = DISP * HIT_XSECT/ABS(HIT_XSECT)

C
C  Drift velocity determination:
C
C Get average velocity of two sectors from STP:
C
              CALL FGTLTM(HALF,UNIT,QDRT,SCTR_XSECT,0,
     &                        ETZERO,ATZERO,VELOP,VELOM)
              CALL FGTLTM(HALF,UNIT,QDRT,SCTR_HOME,0,
     &                        ETZERO,ATZERO,VELOP_HOME,VELOM)
              VELOC = (VELOP_HOME + VELOP)/2.
C
C Find maximum possible drift distance
C
              IF (UNIT .EQ. 0) THEN
                MAX_DRDIST = TH_DRDIST
              ELSEIF (UNIT .EQ. 1) THEN
C
C Use track radius to find max drift distance
                CALL FGETZ0(ITRK,Z0)
                ZD = Z( ABS(HIT_XSECT) ) - Z0
                XD = Q(LFDCT+4)+ Q(LFDCT+7)*ZD
                YD = Q(LFDCT+5)+ Q(LFDCT+8)*ZD
                RADIUS = SQRT(XD**2. + YD**2.)
                MAX_DRDIST = RADIUS*SIN_5_DEG 
              ENDIF
C
C Compute corrected velocity
C
              VELOC = VELOC * (1.0 + DISP/MAX_DRDIST/2.)
C
C Fill histograms
C
              IF ( SHORT_HISTS ) THEN
                ID_QUAD = 100*UNIT + HALF*10 
              ELSE
                IF ( UNIT.EQ.0 ) THEN
                  CALL HF1(7000+HALF*10,CHISQ,1.0)
                  ID_QUAD = 100*UNIT + HALF*10 + QDRT
                ELSE
                  CALL HF1(7200+HALF*10,CHISQ,1.0)
                  ID_QUAD = 100*UNIT + HALF*10 
     &                      + PHI_OCT(SCTR_HOME,HALF) 
                ENDIF
              ENDIF
C
              IF ( CHISQ .LE. MAXCHISQ_XSECT  ) THEN
                IF (    (WIRE_XSECT .GT. 1)
     &            .AND. (WIRE_XSECT .LT. NSEN-1) ) THEN
                  IF ( ABS(FIT_SLOPE) .LT. SLOPE_CUT ) THEN
C
                    CALL HF1(7100+ID_QUAD, VELOC, 1.0)
                  ENDIF
                ENDIF
              ENDIF
C
  300         CONTINUE
            ENDDO                       ! segment loop
          ENDIF
        ENDDO                           ! layer loop
      ENDDO                             ! half loop
C
  999 RETURN
      END
