      FUNCTION FDC_XSW_HISTS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill FDC X-Sense-wire-plane histograms
C-      for determination of t0.
C-
C-   Inputs  : none
C-   Outputs : histograms
C-
C-   Created   2-APR-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      LOGICAL FDC_XSW_HISTS
C
      INTEGER IADD                      
      INTEGER MODULE,LAYER              
      INTEGER H
      INTEGER HALF,UNIT,QDRT,SCTR,WIRE,UBIT
      INTEGER NSEG,SEG
      INTEGER NHIT,HIT
      INTEGER NSEN
      INTEGER PTR_SC
      INTEGER NZBANK
      INTEGER IER,I
      INTEGER HIT_XSW 
      INTEGER LR,LR_LAST 
      INTEGER NHTRK
      INTEGER ID,ID_QUAD
      INTEGER LSGMT                     
      INTEGER LFSEG,LFXSC
      INTEGER GZFSEG,GZFXSC             
      INTEGER WIRE_XSW 
C
      INTEGER MAX_POINTS
      PARAMETER( MAX_POINTS = 16 )
C
      REAL    XC,YC,ZC
      REAL    Z(MAX_POINTS ), X(MAX_POINTS )
      REAL    A,FIT_SLOPE,DISP,DISP_ERR
      REAL    CHISQ,RESID(MAX_POINTS)
      REAL    RADIUS
      REAL    ETZERO,ATZERO,VELOP,VELOM
      REAL    TIME 
      REAL    MAXCHISQ_XSW
      REAL    SLOPE_CUT
C
      LOGICAL SHORT_HISTS 
      LOGICAL FIRST 
C
      INTEGER ICONT(62)
      REAL CONT(62)                     ! CONTENTS OF FSGx BANKS
      EQUIVALENCE (CONT,ICONT)
C
      DATA FIRST /.TRUE./
      DATA MAXCHISQ_XSW  /10./
      DATA SLOPE_CUT /0.40/
      DATA SHORT_HISTS /.TRUE./
C----------------------------------------------------------------------
      FDC_XSW_HISTS = .TRUE.
      IF (FIRST) THEN
        CALL EZPICK('FDC_RCP')
        CALL EZGET('FB_MXCHSQ_XSW',MAXCHISQ_XSW ,IER)
        CALL EZGET('FB_SLOPE_CUT',SLOPE_CUT,IER)
        CALL EZGET('FB_SHORT_HISTS_XSW',SHORT_HISTS,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      CALL DHDIR('FDC_RCP','FDC_XSW',IER,' ')
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
C
C Only interested in cross sense wire segments:
C
              WIRE = INT(CONT(4)/2.)  ! FIRST HIT
              LR = INT(CONT(4)) - WIRE*2
              WIRE = INT(CONT(3+NHIT)/2.)  ! LAST HIT
              LR_LAST = INT(CONT(3+NHIT)) - WIRE*2
              IF ( LR.EQ.LR_LAST) GOTO 300
C
C No X-Sect segs, and don't use Theta half cells.
C
              IF ( ICONT(1)/1000 .NE. 0 ) GOTO 300
              IF ( UNIT .EQ. 0 ) THEN
                IF (SCTR .LT. 3) THEN
                  GOTO 300
                ENDIF
              ENDIF
C
C Get pointers to hit banks, FXSC
C
              LFXSC = GZFXSC(HALF,UNIT,QDRT,SCTR)
              IF ( LFXSC .EQ. 0 )THEN
                CALL ERRMSG('NOLFXSC','FDC_XSW_HISTS',
     &                ' NO hit bank for Segment','W')
                GOTO 300
              ENDIF
C
C Accumulate hits on segment
C
              HIT_XSW = 0
              DO HIT = 1, NHIT
C
C Check if hit crosses SW boundary:
C
                WIRE = INT(CONT(3+HIT)/2.)
                LR_LAST = CONT(3+HIT) - WIRE*2
                IF ( LR_LAST .NE. LR ) THEN
                  IF ( HIT_XSW .EQ. 0 ) THEN
                    HIT_XSW = HIT
                    WIRE_XSW = WIRE
                  ENDIF
                ELSE
C Reject if crosses plane twice:
                  IF ( HIT_XSW .NE. 0 ) THEN
                    GOTO 300
                  ENDIF
                ENDIF
C
C Save hit
C
                PTR_SC = LFXSC + CONT(3+HIT+NSEN)        ! POINTER in FXSC
                X(HIT) = Q(PTR_SC+2+LR_LAST)
                IF ( ( UNIT .EQ.0 ) .AND. (SCTR .EQ. 1) ) 
     &            X(HIT) = -X(HIT) 
C
                CALL GTFALH(HALF,UNIT,QDRT,SCTR,WIRE,XC,YC,ZC)
                Z(HIT) = ZC
              ENDDO
C
C Do cross sector fit to two segment peices:
C
              CALL FTWOSEGFIT(Z,X,NHIT,HIT_XSW-1,
     &              A,FIT_SLOPE,DISP,DISP_ERR,CHISQ,RESID)
              DISP = DISP * (-1)**LR
C
C Compute corrected offset time 
C
              CALL FGTLTM(HALF,UNIT,QDRT,SCTR,0,
     &                        ETZERO,ATZERO,VELOP,VELOM)
              TIME = 10000. * DISP / VELOP / 2.   ! In nsec
C
C Fill histograms
C
              IF ( SHORT_HISTS ) THEN
                ID_QUAD = 100*UNIT + HALF*10 
              ELSE
                IF ( UNIT.EQ.0 ) THEN
                  CALL HF1(8000+HALF*10,CHISQ,1.0)
                  ID_QUAD = 100*UNIT + HALF*10 + QDRT
                ELSE
                  CALL HF1(8200+HALF*10,CHISQ,1.0)
                  ID_QUAD = 100*UNIT + HALF*10 
     &                      + SCTR/9 + 1
                ENDIF
              ENDIF
C
              IF ( CHISQ .LE. MAXCHISQ_XSW  ) THEN
                IF (    (WIRE_XSW .GT. 1)
     &            .AND. (WIRE_XSW .LT. NSEN-1) ) THEN
                  IF ( ABS(FIT_SLOPE) .LT. SLOPE_CUT ) THEN
C
                    CALL HF1(8100+ID_QUAD, TIME, 1.0)
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
