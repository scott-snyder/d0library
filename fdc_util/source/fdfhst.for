      FUNCTION FDFHST()
C----------------------------------------------------------------------
C-
C-  Description:  Fill FDC histograms
C-
C-  Original Creation - July, 1986
C-   Updated   20-Sep-1988   Dave Buchholz
C-   Updated   17-MAY-1989   Susan Blessing
C-   Changed for use with D0USER  18-JAN-1990  Susan Blessing
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT, ICONT
C-    arrays.
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of QTRAK
C-    to accomodate theta and phi errors and two spare words.
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDEVNT.INC'
C
      INTEGER LOUT,USUNIT
      INTEGER I,J,K
      INTEGER IADD                      ! PACKED ADDRESS
      INTEGER IADDLR                    ! PACKED ADDRESS, LR COMBINATION
      INTEGER ID                        ! HISTOGRAM ID
      INTEGER IER                       ! ERROR FOR EZGET
      INTEGER IPTR,DPTR                 ! POINTERS TO HITS IN SC, DA BANKS
      INTEGER LR                        ! LEFT OR RIGHT SOLUTION
      INTEGER HALF,UNIT,QDRT,SCTR,WIRE
      INTEGER H                         ! DUMMY FOR HALF
      INTEGER LADDER(0:2)
      INTEGER MODULE,LAYER              ! FOR TRACKING COORD, =HALF*3+LAYER
      INTEGER NSECT,NQUAD,NWIRE
      INTEGER NSEG,SEG                  ! NUM OF SEGMENTS IN A PHI OR THETA
      INTEGER LFDTH
      INTEGER LFSEG,LFXSC,LFXDA         ! POINTERS TO FSEG, FxSC, FxDA BANKS
      INTEGER LFTRH,GZFTRH
      INTEGER GZFXSC,GZFXDA,GZFSEG      ! TO GET ABOVE POINTERS
      INTEGER LFDCT,GZFDCT
      INTEGER MSEG(0:35,0:7,0:1)        ! NUMBER OF TRACKS IN A SECTOR
      INTEGER NHIT,HIT                  ! NUMBER OF HITS ON SEGMENT/TRACK
      INTEGER N50,N75                   ! 50%, 75% OF HITS
      INTEGER NSEN                      ! NUMBER OF SENSE WIRES IN A SECTOR
      INTEGER NSKIP                     ! WORDS TO SKIP TO GET SOMEWHERE
      INTEGER NTRACK,TRACK              ! NUMBER OF FDC TRACKS
      INTEGER NZBANK
      INTEGER NHTRK,HTRK                ! NUMBER OF HITS ON TRACK, COUNTER
      INTEGER PHPOINT(16)               ! POINTERS FOR ORDERED PH ARRAY
      INTEGER PRLVL                     ! PRINT LEVEL
      INTEGER UBIT                      ! USED/UNUSED CHANNEL
C
      INTEGER ICONT(62)
      REAL CONT(62)                     ! CONTENTS OF FSGx BANKS
      REAL CHI
      REAL INDEX                        ! FOR HISTOGRAM FILLING
      REAL PH(16)                       ! ARRAY OF PULSE HEIGHTS
      REAL PHSUM                        ! TO FIND TRUNC MEAN PH OF HITS
      ! ON SEGS
      REAL QTRAK(26),QHTRK(3,34)        ! TRACK INFORMATION
      REAL WEIGHT
      REAL XPOS,YPOS,ZPOS               ! POSITION ON CHAMBER
      REAL Z0(2)
C
      LOGICAL FDFHST
      LOGICAL FIRST
      LOGICAL DO_RISE                   ! .TRUE. IF WANT RISE, FALL TIME
      ! STUFF
      LOGICAL PASS                      ! .TRUE. IF PASS MEAN PH CUT
      LOGICAL DELAY_LINE                ! keep track of wire 0 trk hit
C
      EQUIVALENCE (CONT,ICONT)
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('DO_RISE',DO_RISE,IER)
        CALL EZGET('Z0',Z0,IER)
        CALL EZRSET
        CALL EZPICK('FDC_RCP')
        CALL EZGET('PRLVL',PRLVL,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
C      LOUT = USUNIT()
      CALL PATHST('RECO')
C      CALL PRFDC(LOUT,PRLVL)
C      CALL PRFDCT(LOUT,0,0,'ALL',PRLVL)
C      CALL PRFSG0(LOUT,0,0,'ALL',PRLVL)
C      CALL PRFSG1(LOUT,0,0,'ALL',PRLVL)
C      CALL PRFSG2(LOUT,0,0,'ALL',PRLVL)
C      CALL PRFSG3(LOUT,0,0,'ALL',PRLVL)
C      CALL PRFSG4(LOUT,0,0,'ALL',PRLVL)
C      CALL PRFSG5(LOUT,0,0,'ALL',PRLVL)
C
C TIMING PULSE BIN
      CALL HF1(29999,TMPUBN,1.)
C
C HISTOGRAMS INVOLVING ALL HITS
C
      DO HALF = 0, MXHALF
        DO UNIT = 0, MXUNIT
C
          IF (UNIT.EQ.0) THEN
            NQUAD = MXQUAD
            NSECT = MXSECT
            NWIRE = MXWIRT + 2
          ELSE
            NQUAD = 0
            NSECT = MXSECP
            NWIRE = MXWIRP
          END IF
C
          DO QDRT = 0, NQUAD
            DO SCTR = 0, NSECT
C
              LFXDA = GZFXDA(HALF,UNIT,QDRT,SCTR)
              IF (LFXDA.NE.0) THEN
                DO WIRE = 0, NWIRE
                  ID = 100000*HALF + 10000*UNIT + 1000*QDRT + 100*SCTR
                  NHIT = IQ(LFXDA+4+WIRE)
                  CALL HF1(1000000+ID+WIRE,FLOAT(NHIT),1.)
C
C TOTAL NUMBER OF HITS FOR THIS WIRE
                  IF (NHIT.GT.0) THEN
                    ID = 100000*HALF + 10000*UNIT + 1000*QDRT
                    INDEX = FLOAT(NWIRE*SCTR + WIRE)
                    WEIGHT = FLOAT(NHIT)
                    CALL HF1(52000000+ID,INDEX,WEIGHT)
                  END IF
C
                END DO
C
              END IF
            END DO
          END DO
        END DO
      END DO
C
C
C HISTOGRAMS INVOLVING HITS ON SEGMENTS
C
      DO 100 HALF = 0, MXHALF
C
        CALL VZERO(MSEG,576)
C
        DO 200 LAYER = 0, 2
C
          IF (LAYER.LE.1) THEN
            NSEN = NBTSEN
          ELSE
            NSEN = NBPSEN
          END IF
C
          LFSEG = GZFSEG(HALF,LAYER)
C LFSEG=0 MEANS NO SEGMENTS FOUND
          IF (LFSEG.GT.0) THEN
            NSEG = NZBANK(IXCOM,LFSEG)
C
            DO 400 SEG = 1, NSEG
C
              MODULE = 3*HALF + LAYER
              CALL GTFSEG(MODULE,SEG,CONT)
              IADD = ICONT(2)
              CALL FCODER(IADD,H,UNIT,QDRT,SCTR,WIRE,UBIT,1)
C
              NHIT = ICONT(3)
C
C TOTAL NUMBER OF SEGMENTS IN EACH SECTOR
              ID = 100000*H + 10000*UNIT + 1000*QDRT
              CALL HF1(51000000+ID,FLOAT(SCTR),1.)
C
C HISTOGRAM HITS/SEGMENT IN A QUADRANT
              CALL HF1(53000000+ID,FLOAT(NHIT),1.)
C
C COUNT NUMBER OF SEGMENTS PER SECTOR PER EVENT
              MSEG(SCTR,QDRT,UNIT) = MSEG(SCTR,QDRT,UNIT) + 1
C
              ID = 100000*H + 10000*UNIT + 1000*QDRT + 100*SCTR
C
C HISTOGRAM HITS/SEGMENT IN A SECTOR
              CALL HF1(30000000+ID,FLOAT(NHIT),1.)
C
              CALL VZERO(PH,16)
C
              DO 500 HIT = 1, NHIT
C
                WIRE = INT(CONT(3+HIT)/2.)
C
C TOTAL NUMBER OF HITS ON SEGMENTS FOR THIS WIRE
                ID = 100000*H + 10000*UNIT + 1000*QDRT
                IF (UNIT.EQ.0) THEN
                  INDEX = (NBTSEN+2*NBDELY)*SCTR + WIRE
                ELSE
                  INDEX = NBPSEN*SCTR + WIRE
                END IF
                CALL HF1(50000000+ID,INDEX,1.)
C
                LR = INT(CONT(3+HIT)) - WIRE*2
C POINTER TO HIT IN PROCESS DATA BANK, FTSC OR FPSC
                IPTR = INT(CONT(3+HIT+NSEN))
C
                ID = 100000*H + 10000*UNIT + 
     &              1000*QDRT + 100*SCTR + WIRE
C GET POINTER TO THIS HIT IN THE RAW DATA BANK, FTDA OR FPDA
                LFXSC = GZFXSC(H,UNIT,QDRT,SCTR)
                DPTR = IQ(LFXSC+IPTR+10)
                LFXDA = GZFXDA(H,UNIT,QDRT,SCTR)
                IF (LFXDA.NE.0) THEN
                  DPTR = LFXDA + DPTR
C
C DELAY LINE STUFF
                  IF (UNIT.EQ.0.AND.WIRE.EQ.0)
     &                CALL FDLHST(H,UNIT,QDRT,SCTR,WIRE,IPTR)
C
C DRIFT TIME
                  CALL HF1(2000000+ID,Q(DPTR+2),1.)
C
C DRIFT TIME VS TIMING PULSE
                  CALL HF2(200+UNIT*10,TMPUBN,Q(DPTR+2),1.)
C PEAK FADC
                  CALL HF1(8000000+ID,Q(DPTR+5),1.)
C PULSE HEIGHT
                  CALL HF1(12000000+ID,Q(DPTR+3),1.)
C
C PULSE HEIGHT VS DRIFT TIME, BY SECTOR
                  IF (WIRE.EQ.0) THEN
                    CALL HF2(36000000+ID-WIRE,Q(DPTR+2),Q(DPTR+3),1.)
                  ELSE IF ((UNIT.EQ.0.AND.WIRE.EQ.7).OR.
     &              (UNIT.EQ.1.AND.WIRE.EQ.15)) THEN
                    CALL HF2(38000000+ID-WIRE,Q(DPTR+2),Q(DPTR+3),1.)
                  ELSE
                    CALL HF2(37000000+ID-WIRE,Q(DPTR+2),Q(DPTR+3),1.)
                  END IF
C
C FILL LOCAL PULSE HEIGHT ARRAY
                  PH(HIT) = Q(DPTR+3)
C
C SEGMENT RESIDUAL
                  IF (UNIT.EQ.0) THEN
                    NSKIP = 2*(MXWIRT+1) + 2
                  ELSE
                    NSKIP = 2*(MXWIRP+1) + 3
                  END IF
                  CALL HF1(21000000+ID+LR*1000000,
     &              CONT(3+HIT+NSKIP),1.)
C
C RESIDUAL VS DRIFT TIME
                  CALL HF2(25000000+ID+LR*1000000,Q(DPTR+2),
     &                CONT(3+HIT+NSKIP),1.)
                END IF
  500         CONTINUE
C
C FIND TRUNCATED MEAN PULSE HEIGHT FOR THIS SEGMENT
C
C ORDER PULSE HEIGHTS - SRTFLT SORTS INTO ASCENDING ORDER
              CALL SRTFLT(PH,NHIT,PHPOINT)
C
              ID = 100000*H + 10000*UNIT + 1000*QDRT + 100*SCTR
C
C SUM ALL PULSE HEIGHTS
              PHSUM = 0.
              DO HIT = 1, NHIT
                PHSUM = PHSUM + PH(HIT)
              END DO
              CALL HF1(33000000+ID,PHSUM/FLOAT(NHIT),1.)
C
C SUM LOW 75%
              PHSUM = 0.
              N75 = NINT(FLOAT(NHIT)*.75)
              DO HIT = 1, N75
                PHSUM = PHSUM + PH(HIT)
              END DO
              CALL HF1(34000000+ID,PHSUM/FLOAT(N75),1.)
C
C SUM LOW 50%
              PHSUM = 0.
              N50 = NINT(FLOAT(NHIT)*.50)
              DO HIT = 1, N50
                PHSUM = PHSUM + PH(HIT)
              END DO
              CALL HF1(35000000+ID,PHSUM/FLOAT(N50),1.)
C
  400       CONTINUE
          END IF
C
  200   CONTINUE
C
C HISTOGRAM NUMBER OF SEGMENTS PER SECTOR PER EVENT
        DO UNIT = 0, MXUNIT
          IF (UNIT.EQ.0) THEN
            NSECT = MXSECT
            NQUAD = MXQUAD
          ELSE
            NSECT = MXSECP
            NQUAD = 0
          END IF
C
          DO QDRT = 0, NQUAD
            DO SCTR = 0, NSECT
              ID = 100000*HALF + 10000*UNIT + 1000*QDRT + 100*SCTR
              CALL HF1(31000000+ID,
     &            FLOAT(MSEG(SCTR,QDRT,UNIT)),1.)
            END DO
          END DO
        END DO
C
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
        CALL GTFDCT(TRACK,QTRAK,QHTRK,LADDER)
        CALL UCOPY(QTRAK(2),NHTRK,1)
C
C WIRES/TRACK
          CALL HF1(20,FLOAT(NHTRK),1.)
C
C CHI**2 - CALCULATION FROM FTRHIS
          CHI = SQRT(2*QTRAK(19))-SQRT(2*(FLOAT(NHTRK)-2.)-1.)
          CALL HF1(30,CHI,1.)
C
C X0 FOR TRACK
          CALL HF1(40,QTRAK(4),1.)
C
C Y0 FOR TRACK
          CALL HF1(50,QTRAK(5),1.)
C
C PHI
          CALL HF1(60,QTRAK(6),1.)
C
C DX/DZ
          CALL HF1(70,QTRAK(7),1.)
C
C DY/DZ
          CALL HF1(80,QTRAK(8),1.)
C
          LFDCT = GZFDCT(TRACK)
          LFDTH = LQ(LFDCT-1)
          IADD = (IQ(LFDTH+1))/2
          CALL FCODER(IADD,H,UNIT,QDRT,SCTR,WIRE,UBIT,1)
          ZPOS = 104. * ((-1.)**(H+1))
          XPOS = QTRAK(4) + QTRAK(7)*(ZPOS-Z0(H+1))
          YPOS = QTRAK(5) + QTRAK(8)*(ZPOS-Z0(H+1))
C
C X LOCATION ON CHAMBER
          CALL HF1(90,XPOS,1.)
C
C Y LOCATION ON CHAMBER
          CALL HF1(100,YPOS,1.)
C
C LOOP OVER HITS ON TRACK
          DELAY_LINE = .FALSE.
          DO HTRK = 1, NHTRK
C
C UNPACK ENCODED ADDRESS
            CALL UCOPY(QHTRK(1,HTRK),IADDLR,1)
            IADD = IADDLR/2
            LR = IADDLR - IADD*2
            CALL FCODER(IADD,H,UNIT,QDRT,SCTR,WIRE,UBIT,1)
C
            ID = 100000*H + 10000*UNIT + 1000*QDRT + 100*SCTR + WIRE
C
C LEFT RIGHT HISTOGRAMS
C RESIDUAL FOR THIS HIT
            IF (UNIT.EQ.0 .AND. WIRE.EQ.0) THEN         ! FOR DELAY LINE 
              IF (DELAY_LINE) THEN
                DELAY_LINE = .FALSE.
                ID = ID+8
              ELSE
                DELAY_LINE = .TRUE.
              ENDIF
            ENDIF
            CALL HF1(23000000+LR*1000000+ID,QHTRK(3,HTRK),1.)
C GET POINTER TO THIS HIT IN THE RAW DATA BANK, FTDA OR FPDA
            CALL UCOPY(QHTRK(2,HTRK),IPTR,1)
            LFXSC = GZFXSC(H,UNIT,QDRT,SCTR)
            DPTR = IQ(LFXSC+IPTR+10)
            LFXDA = GZFXDA(H,UNIT,QDRT,SCTR)
            IF (LFXDA.NE.0) THEN
              DPTR = LFXDA + DPTR
C RESIDUAL VS DRIFT TIME
              CALL HF2(27000000+ID+LR*1000000,Q(DPTR+2),
     &                QHTRK(3,HTRK),1.)
            END IF
          END DO
      END DO
C
C      CALL FDC_PWC_HISTS
C
      FDFHST = .TRUE.
C
  999 RETURN
      END
