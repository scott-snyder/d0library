      FUNCTION FDFHST_EXM()
C----------------------------------------------------------------------
C-
C-  Description:  Fill FDC histograms
C-
C-  Original Creation - July, 1986
C-   Updated   20-Sep-1988   Dave Buchholz
C-   Updated   17-MAY-1989   Susan Blessing
C-   Changed for use with D0USER  18-JAN-1990  Susan Blessing
C-   Updated  22-MAR-1991   Susan K. Blessing  Modified Rob's modifications
C-    for use with Cosmic Ray commissioning Examine2
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT, ICONT
C-    arrays.
C-   Updated  27-AUG-1991   Robert E. Avery  Half is now only bit 0 of
C-                              track status word.
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK
C-    to accomodate theta and phi errors and two spare words.
C-   Updated  22-OCT-1993   Susan K. Blessing  Split sectors 0 and 1 from
C-    sectors 2-5 in PH histograms.
C-   Updated  30-NOV-1993   Susan K. Blessing  Stop filling number of hits
C-    histograms with a weight.  Loop over hits instead.
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDEVNT.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL FDFHST_EXM
C
      INTEGER I,J,K
      INTEGER IADD                      ! PACKED ADDRESS
      INTEGER ID,ID1,ID2                ! HISTOGRAM ID
      INTEGER IER                       ! ERROR FOR EZGET
      INTEGER IPTR,DPTR                 ! POINTERS TO HITS IN SC, DA BANKS
      INTEGER LR                        ! LEFT OR RIGHT SOLUTION
      INTEGER HALF,UNIT,QDRT,SCTR,WIRE
      INTEGER H,QUAD                    ! DUMMY FOR HALF,QDRT
      INTEGER MODULE,LAYER              ! FOR TRACKING COORD, =HALF*3+LAYER
      INTEGER N_LAYER
      INTEGER NSECT,NQUAD,NWIRE
      INTEGER NSEG,SEG                  ! NUM OF SEGMENTS IN A PHI OR THETA
      INTEGER LFSEG,LFXSC,LFXDA         ! POINTERS TO FSEG, FxSC, FxDA BANKS
      INTEGER LFTRH,GZFTRH
      INTEGER GZFXSC,GZFXDA,GZFSEG      ! TO GET ABOVE POINTERS
      INTEGER LKFDCH,GZFDCH
      INTEGER LFDCT,GZFDCT
      INTEGER NHIT,HIT                  ! NUMBER OF HITS ON SEGMENT/TRACK
      INTEGER NSEN                      ! NUMBER OF SENSE WIRES IN A SECTOR
      INTEGER NTRACK,TRACK              ! NUMBER OF FDC TRACKS
      INTEGER NZBANK
      INTEGER NUM_BINS
      INTEGER NHTRK,HTRK                ! NUMBER OF HITS ON TRACK, COUNTER
      INTEGER SECT                      ! DUMMY TO REMEMBER SCTR FOR XSECT
      INTEGER SCTR1                     ! CROSS SECTOR SEG SECTOR NUMBER
      INTEGER UBIT                      ! USED/UNUSED CHANNEL
      INTEGER XHIT                      ! NUMBER OF HITS IN CROSS SECTOR
      INTEGER XWIRE                     ! WIRE OF FIRST HIT IN CROSS SECTOR
      INTEGER PHI_QDRT(0:35,0:1)        ! TRANSLATION FROM SECT TO HV OCTANT
      INTEGER LADDER(0:2)
C
      REAL CHI
      REAL INDEX                        ! FOR HISTOGRAM FILLING
      REAL SUMPH
C
      LOGICAL FIRST,OK
      LOGICAL FDBHST_EXM
      LOGICAL XSECT                     ! .TRUE. IF CROSS SECTOR SEGMENT
      LOGICAL FDC_XSECT_HISTS
C
      INTEGER IQTRAK(26),IQHTRK(3,34)   ! TRACK INFORMATION
      REAL QTRAK(26),QHTRK(3,34)
      EQUIVALENCE (QTRAK,IQTRAK)
      EQUIVALENCE (QHTRK,IQHTRK)
C
      INTEGER ICONT(62)
      REAL CONT(62)                     ! CONTENTS OF FSGx BANKS
      EQUIVALENCE (CONT,ICONT)
C
      DATA FIRST/.TRUE./
      DATA PHI_QDRT /2,2,2,2,
     &               1,1,1,1,1,
     &               8,8,8,8,
     &               7,7,7,7,
     &               6,6,6,6,6,
     &               5,5,5,5,5,
     &               4,4,4,4,
     &               3,3,3,3,2,
     &               2,2,2,2,
     &               1,1,1,1,1,
     &               8,8,8,8,
     &               7,7,7,7,
     &               6,6,6,6,6,
     &               5,5,5,5,5,
     &               4,4,4,4,
     &               3,3,3,3,2/
C
C      DATA PHI_QDRT /2,2,2,2,1,1,1,1,8,8,8,8,8,7,7,7,7,7,
C     &               6,6,6,6,5,5,5,5,4,4,4,4,4,3,3,3,3,3,
C     &               7,7,7,7,7,8,8,8,8,8,1,1,1,1,2,2,2,2,
C     &               3,3,3,3,3,4,4,4,4,4,5,5,5,5,6,6,6,6/
C
C----------------------------------------------------------------------
C
      FDFHST_EXM = .TRUE.
C
      IF (FIRST) THEN
        OK = FDBHST_EXM()
        FIRST = .FALSE.
      END IF
C
      IER = 0
      CALL DHDIR('FDC_RCP','HBOOK_FDC',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('FDC','FDFHST_EXM',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      CALL PATHST('RECO')
C
C VELOCITY HISTOGRAMS
      OK = FDC_XSECT_HISTS()
C
C  Number of hits in FDC
      LKFDCH=GZFDCH()
      NHIT=0
      IF(LKFDCH.GT.0) NHIT=IQ(LKFDCH+1)
      CALL HF1(90,FLOAT(NHIT),1.)
C
C HISTOGRAMS INVOLVING HITS ON SEGMENTS
C
      DO 100 HALF = 0, MXHALF
        N_LAYER = 0
        DO 200 LAYER = 0, 2
C
          IF (LAYER.LE.1) THEN
            NSEN = NBTSEN
            NWIRE = MXWIRT + 1
            NSECT = MXSECT + 1
          ELSE
            NSEN = NBPSEN
            NWIRE = MXWIRP + 1
            NSECT = MXSECP + 1
          END IF
C
          LFSEG = GZFSEG(HALF,LAYER)
C LFSEG=0 MEANS NO SEGMENTS FOUND
          IF (LFSEG.GT.0) THEN
            NSEG = NZBANK(0,LFSEG)
            N_LAYER = N_LAYER + 1
            CALL HF1(150+HALF*3+LAYER,FLOAT(NSEG),1.)
C
            DO 400 SEG = 1, NSEG
C
              MODULE = 3*HALF + LAYER
              CALL GTFSEG(MODULE,SEG,CONT)
              IADD = ICONT(2)
              NHIT = ICONT(3)
              CALL FCODER(IADD,H,UNIT,QDRT,SCTR,WIRE,UBIT,1)
C
C
C CHECK IF CROSS SECTOR SEGMENT
              XSECT = .FALSE.
              IF (ABS(ICONT(1)).GT.50) THEN
                SCTR1 = ICONT(1)/ABS(ICONT(1)) + SCTR
                XHIT = ABS(ICONT(1)/1000)
                XSECT = .TRUE.
                XWIRE = CONT(3+XHIT)/2.
              END IF
C
C HISTOGRAM HITS/SEGMENT IN A QUADRANT
              CALL HF1(13000+ID,FLOAT(NHIT),1.)
C
              DO 500 HIT = 1, NHIT
C IF HAVE A CROSS SECTOR SEGMENT AND HAVE CROSSED INTO THE NEXT SECTOR,
C CHANGE SECTORS
                IF (XSECT.AND.HIT.GE.XHIT) SCTR = SCTR1
C PHI HV QUADRANT:
                IF ( UNIT.EQ.0 ) THEN
                  QUAD = QDRT
                ELSE
                  QUAD = PHI_QDRT(SCTR,H) - 1
                ENDIF
C
                WIRE = INT(CONT(3+HIT)/2.)
C
                LR = INT(CONT(3+HIT)) - WIRE*2
C POINTER TO HIT IN PROCESS DATA BANK, FTSC OR FPSC
                IPTR = INT(CONT(3+HIT+NSEN))
C
C GET POINTER TO THIS HIT IN THE RAW DATA BANK, FTDA OR FPDA
                LFXSC = GZFXSC(H,UNIT,QDRT,SCTR)
                DPTR = IQ(LFXSC+IPTR+10)
                LFXDA = GZFXDA(H,UNIT,QDRT,SCTR)
                IF (LFXDA.NE.0) THEN
                  DPTR = LFXDA + DPTR
C
                  ID = 100*H + 10*UNIT
                  ID1 = 100000*H + 10000*UNIT + 1000*QDRT +
     &                100*SCTR + WIRE
                  ID2 = 100*H + 10*SCTR
C
C DELAY LINE STUFF
                  IF (UNIT.EQ.0.AND.WIRE.EQ.0)
     &                CALL FDLHST_EXM(H,UNIT,QDRT,SCTR,WIRE,IPTR)
C
C DRIFT TIME
                  CALL HF1(3000+ID,Q(DPTR+2),1.)
                  CALL HF1(4000000+ID1,Q(DPTR+2),1.)
C
C PEAK FADC
                  ID = 100*H + 10*UNIT +QUAD
                  IF (UNIT.EQ.0 .AND. WIRE.EQ.0) THEN
                    CALL HF1(21000+ID,Q(DPTR+5),1.)
                  ELSE
                    CALL HF1(20000+ID,Q(DPTR+5),1.)
                  END IF
                  CALL HF1(2000000+ID1,Q(DPTR+5),1.)
C
                  INDEX = FLOAT( NSECT*NWIRE*QDRT + NWIRE*SCTR + WIRE)
                  CALL HF1(100+H*10+UNIT*2,INDEX,Q(DPTR+5))
C
C PULSE HEIGHT
                  IF (UNIT.EQ.0) THEN
C THETA
                    IF (WIRE.EQ.0) THEN
                      IF (SCTR.LE.1) THEN
C SW0 S0-S1
                        CALL HF1(30000+ID,Q(DPTR+3),1.)
                      ELSE
C SW0 S2-S5
                        CALL HF1(33000+ID,Q(DPTR+3),1.)
                      END IF
                      CALL HF1(38000+ID2,Q(DPTR+3),1.)
                    ELSE IF (WIRE.NE.1) THEN
                      IF (SCTR.LE.1) THEN
C SW S0-S1
                        CALL HF1(31000+ID,Q(DPTR+3),1.)
                      ELSE
C SW S2-S5
                        CALL HF1(34000+ID,Q(DPTR+3),1.)
                      END IF
                      CALL HF1(37000+ID2,Q(DPTR+3),1.)
                    END IF
                  ELSE
C PHI
                    CALL HF1(36000+ID,Q(DPTR+3),1.)
                  END IF
                  CALL HF1(3000000+ID1,Q(DPTR+3),1.)
                END IF
C
  500         CONTINUE
  400       CONTINUE
          END IF
  200   CONTINUE

  100 CONTINUE
C
C HISTOGRAMS INVOLVING COMPLETE TRACKS
C
C GET NUMBER OF FDC TRACKS
      LFTRH = GZFTRH()
      NTRACK = IQ(LFTRH+2)
C
C TRACKS/EVENT
      CALL HF1(10,FLOAT(NTRACK),1.)
C
      DO TRACK = 1, NTRACK
        LFDCT=GZFDCT(TRACK)
        CALL GTFDCT_LINK(LFDCT,QTRAK,QHTRK,LADDER)
        NHTRK=IQTRAK(2)
        IF (NHTRK.GT.0) THEN
          HALF=IAND(1,IQTRAK(1))
C
C WIRES/TRACK
          CALL HF1(20,FLOAT(NHTRK),1.)
C
C CHI**2 - CALCULATION FROM FTRHIS
          CHI = SQRT(2*QTRAK(19))-SQRT(2*(FLOAT(NHTRK)-4.)-1.)
          CALL HF1(30,CHI,1.)
C
        END IF
      END DO
C
C FILL HISTOGRAMS WHICH BELONG IN THE CD DIRECTORY
C
      CALL DHDIR('FDC_RCP','HBOOK_CD',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('FDC','FDBHST_EXM',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      DO HALF = 0, MXHALF
        DO UNIT = 0, MXUNIT
C
          IF (UNIT.EQ.0) THEN
            NQUAD = MXQUAD + 1
            NSECT = MXSECT + 1
            NWIRE = MXWIRT + 1
          ELSE
            NQUAD = 1
            NSECT = MXSECP + 1
            NWIRE = MXWIRP + 1
          END IF
C
          DO QDRT = 0, NQUAD-1
            DO SCTR = 0, NSECT-1
C
              LFXDA = GZFXDA(HALF,UNIT,QDRT,SCTR)
              IF (LFXDA.NE.0) THEN
                DO WIRE = 0, NWIRE-1
                  NHIT = IQ(LFXDA+4+WIRE)
C
C TOTAL NUMBER OF HITS FOR THIS WIRE
                  IF (NHIT.GT.0) THEN
                    ID = 10*HALF + UNIT*2
                    INDEX = FLOAT( NSECT*NWIRE*QDRT
     &                           + NWIRE*SCTR + WIRE)
                    DO I = 1, NHIT
                      CALL HF1(400+ID,INDEX,1.)
                    END DO
C
                  END IF
                END DO
C Delay lines
                IF (UNIT.EQ.0) THEN
                  DO WIRE = 8,9
                    NHIT = IQ(LFXDA+4+WIRE)
C
C TOTAL NUMBER OF HITS FOR THIS WIRE
                    IF (NHIT.GT.0) THEN
                      ID = 10*HALF
                      INDEX = FLOAT( NSECT*2*QDRT + 2*SCTR + WIRE-8)
                      DO I = 1, NHIT
                        CALL HF1(401+ID,INDEX,1.)
                      END DO
C
C                      SUMPH = 0.
C                      DO HIT = 1, NHIT
C                        IPTR = IQ(LFXDA+4+IQ(LFXDA+2)+WIRE) +
C     &                    IQ(LFXDA+3) * (HIT-1) - 1
C                        SUMPH = SUMPH + Q(LFXDA+IPTR+3)
C                      END DO
C                      CALL HF1(404+ID,INDEX,SUMPH)
CC
                    END IF
                  END DO
                END IF
              END IF
            END DO
          END DO
        END DO
      END DO
C
      DO HALF = 0, MXHALF
        DO LAYER = 0, 2
C
          IF (LAYER.LE.1) THEN
            NSEN = NBTSEN
            NWIRE = MXWIRT + 1
            NSECT = MXSECT + 1
          ELSE
            NSEN = NBPSEN
            NWIRE = MXWIRP + 1
            NSECT = MXSECP + 1
          END IF
C
          LFSEG = GZFSEG(HALF,LAYER)
C LFSEG=0 MEANS NO SEGMENTS FOUND
          IF (LFSEG.GT.0) THEN
            NSEG = NZBANK(0,LFSEG)
C
            DO SEG = 1, NSEG
C
              MODULE = 3*HALF + LAYER
              CALL GTFSEG(MODULE,SEG,CONT)
              IADD = ICONT(2)
              NHIT = ICONT(3)
              CALL FCODER(IADD,H,UNIT,QDRT,SCTR,WIRE,UBIT,1)
C
C CHECK IF CROSS SECTOR SEGMENT
              XSECT = .FALSE.
              IF (ABS(ICONT(1)).GT.50) THEN
                SCTR1 = ICONT(1)/ABS(ICONT(1)) + SCTR
                XHIT = ABS(ICONT(1)/1000)
                XSECT = .TRUE.
                XWIRE = CONT(3+XHIT)/2.
              END IF
C
C TOTAL NUMBER OF SEGMENTS IN EACH SECTOR
              ID = 10*H + UNIT*2
              DO  WIRE =  0, NSEN-1
                IF (XSECT.AND.WIRE.GE.XWIRE) THEN
                  SECT = SCTR1
                ELSE
                  SECT = SCTR
                ENDIF
                INDEX = WIRE + NWIRE*SECT + NWIRE*NSECT*QDRT
                CALL HF1(420+ID,INDEX,1.)
              ENDDO
C
C DELAY LINES
              IF (UNIT.EQ.0) THEN
                ID = 10*H
                DO WIRE = 8,9
                  INDEX = FLOAT( NSECT*2*QDRT + 2*SCTR + WIRE-8)
                  CALL HF1(421+ID,INDEX,1.)
                END DO
              END IF
C
              DO HIT = 1, NHIT
C IF HAVE A CROSS SECTOR SEGMENT AND HAVE CROSSED INTO THE NEXT SECTOR,
C CHANGE SECTORS
                IF (XSECT.AND.HIT.GE.XHIT) SCTR = SCTR1
C
                WIRE = INT(CONT(3+HIT)/2.)
                IPTR = INT(CONT(3+HIT+NSEN))
C
C TOTAL NUMBER OF HITS ON SEGMENTS FOR THIS WIRE
                ID = 10*H + UNIT*2
                INDEX = WIRE + NWIRE*SCTR + NWIRE*NSECT*QDRT
                CALL HF1(403+ID,INDEX,1.)
C
                IF (UNIT.EQ.0.AND.WIRE.EQ.0)
     &                CALL FDLHST_CD_EXM(H,UNIT,QDRT,SCTR,WIRE,IPTR)
C
              END DO
            END DO
          END IF
        END DO
      END DO
C
C Average pulse height plots
      CALL FDC_AVE_PH
C
  999 RETURN
      END
