      FUNCTION FDBHST()
C--------------------------------------------------------------
C
C  Book histograms for test purposes.
C
C  Histogram IDs are made the following way:
C      XXHUQS0W for theta chambers
C      XXHUSSWW for phi chambers
C   where  XX is the type of histogram,
C           H is the half,
C           U is the unit (0 for theta, 1 for phi),
C           Q is the theta quadrant (phis do not have quadrants, so always 0),
C          SS is the sector (0-5 for theta, 0-35 for phi, so overlaps Q for
C              phi)
C          WW is the wire number (0-9 for theta, 0-15 for phi)
C
C  UPDATED 05-17-89 SUSAN BLESSING
C  Modified for use with D0USER  17-JAN-1990  Susan Blessing
C-   Updated  26-APR-1991   Jeffrey Bantly  cleanup 
C
C--------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      LOGICAL FDBHST
      LOGICAL FDEXST
C
      INTEGER ID
      CHARACTER*40 TITLE
C
      INTEGER HALF,UNIT,QDRT,SCTR,WIRE
      INTEGER NQDRT,NSCTR,NWIRE
      INTEGER IER
C
      LOGICAL FIRST
      LOGICAL ZERO_SUP
      CHARACTER*2 H,U,QD,S,W
C
      DATA FIRST/.TRUE./
C
C-------------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL INTMSG(' FDBHST called, histograms being booked.')
C
C        CALL EZPICK('FTRAKS_RCP')
C        CALL EZRSET
C
C SET UP HISTOGRAMS
C
C Timing pulse location
        CALL HBOOK1(29999,'LOCATION OF TIMING PULSE',100,0.0,550.,0.)
C
C Track histograms
C FDC tracks/event
        CALL HBOOK1(10,'FDC TRACKS/EVENT',50,-.5,49.5,0.)
C
C Number of wires on FDC track
        CALL HBOOK1(20,'WIRES/FDC TRACK',35,-.5,34.5,0.)
C
C CHI**2
        CALL HBOOK1(30,'CHI**2, FDC TRACK',50,-10.,30.,0.)
C
C X location of vertex
        CALL HBOOK1(40,'X0 FOR TRACK',100,-100.,100.,0.)
C
C Y location of vertex
        CALL HBOOK1(50,'Y0 FOR TRACK',100,-100.,100.,0.)
C
C PHI for track
        CALL HBOOK1(60,'PHI FOR TRACK',100,0.,7.,0.)
C
C DX/DZ
        CALL HBOOK1(70,'DX/DZ FOR TRACK',100,-1.,1.,0.)
C
C DY/DZ
        CALL HBOOK1(80,'DY/DZ FOR TRACK',100,-1.,1.,0.)
C
C X LOCATION ON CHAMBER
        CALL HBOOK1(90,'X LOCATION ON CHAMBER',100,-70.,70.,0.)
C
C Y LOCATION ON CHAMBER
        CALL HBOOK1(100,'Y LOCATION ON CHAMBER',100,-70.,70.,0.)
C
C SINGLE HISTOGRAMS FOR ALL WIRES
C
C HISTOGRAMS INVOLVING ALL HITS
C
        DO 500 HALF = 0, MXHALF
          WRITE(H,'(I1)') HALF
C
          DO 400 UNIT = 0, MXUNIT
            WRITE(U,'(I1)') UNIT
C
            IF (UNIT.EQ.0) THEN
              NQDRT = MXQUAD
              NSCTR = MXSECT
              NWIRE = MXWIRT + 2
            ELSE IF (UNIT.EQ.1) THEN
              NQDRT = 0
              NSCTR = MXSECP
              NWIRE = MXWIRP
            END IF
C
            DO 300 QDRT = 0, NQDRT
              WRITE(QD,'(I1)') QDRT
C
C CHECK IF THIS HARDWARE IS BEING READ OUT IF THETA UNIT
C WILL ONLY GET HERE FOR PHI IF IT'S BEING READ OUT.  IF HAVE ONLY HALF=1,
C TWO EXTRA HISTOGRAMS WILL BE BOOKED.
C
              IF ((UNIT.EQ.0.AND.FDEXST(HALF,UNIT,QDRT,-1)).OR.
     &           UNIT.EQ.1) THEN
                ID = 100000*HALF + 10000*UNIT + 1000*QDRT
C
C BOOK HISTOGRAMS FOR CALCULATING SENSE WIRE EFFICIENCIES
C ONE FOR EACH QUADRANT
                TITLE = 'HITS/WIRE ON SEGMENTS -
     & H'//H//' U'//U//' Q'//QD
                CALL HBOOK1(50000000+ID,TITLE,576,0.0,576.,0.)
                TITLE = 'SEGMENTS/SCTR -
     & H'//H//' U'//U//' Q'//QD
                CALL HBOOK1(51000000+ID,TITLE,36,0.0,36.,0.)
                TITLE = 'HITS/WIRE TOTAL -
     & H'//H//' U'//U//' Q'//QD
                CALL HBOOK1(52000000+ID,TITLE,576,0.0,576.,0.)
                TITLE = 'HITS/SEGMENT -
     & H'//H//' U'//U//' Q'//QD
                CALL HBOOK1(53000000+ID,TITLE,17,-.5,16.5,0.)
C
              END IF
C
              DO 200 SCTR = 0, NSCTR
C
C CHECK IF THIS HARDWARE IS BEING READ OUT
                IF (FDEXST(HALF,UNIT,QDRT,SCTR)) THEN
                  WRITE(S,'(I2)') SCTR
C
                  ID = 100000*HALF + 10000*UNIT + 1000*QDRT + 100*SCTR
C
C HISTOGRAMS FOR EACH SCTR
C
                  TITLE = 'HITS PER SEGMENT -
     & H'//H//' U'//U//' Q'//QD//' S'//S
                  CALL HBOOK1(30000000+ID,TITLE,17,-.5,16.5,0.)
C
                  TITLE = 'SEGMENTS PER EVENT -
     & H'//H//' U'//U//' Q'//QD//' S'//S
                  CALL HBOOK1(31000000+ID,TITLE ,20,-.5,19.5,0.)
C
                  TITLE = 'TRUNC MEAN PH/HIT ON SEG (1)-
     & H'//H//' U'//U//' Q'//QD//' S'//S
                  CALL HBOOK1(33000000+ID,TITLE,100,-10.,1990.,0.)
C
                  TITLE = 'TRUNC MEAN PH/HIT ON SEG (.75)-
     & H'//H//' U'//U//' Q'//QD//' S'//S
                  CALL HBOOK1(34000000+ID,TITLE,100,-10.,1990.,0.)
C
                  TITLE = 'TRUNC MEAN PH/HIT ON SEG (.5)-
     & H'//H//' U'//U//' Q'//QD//' S'//S
                  CALL HBOOK1(35000000+ID,TITLE,100,-10.,1990.,0.)
C
                  IF (UNIT.EQ.1) THEN 
                    TITLE = 'PH VS DT, WIRE 0 -
     & H'//H//' U'//U//' Q'//QD//' S'//S
                    CALL HBOOK2(36000000+ID,TITLE,25,-40.,1660.,
     &                50,-10.,5990.,0.)
                  ELSE
                    TITLE = 'PH VS DT, WIRE 0 -
     & H'//H//' U'//U//' Q'//QD//' S'//S
                    CALL HBOOK2(36000000+ID,TITLE,25,-40.,1660.,
     &                50,-10.,3990.,0.)
                  END IF
C
                  TITLE = 'PH VS DT, INNER SW -
     & H'//H//' U'//U//' Q'//QD//' S'//S
                  CALL HBOOK2(37000000+ID,TITLE,25,-40.,1660.,
     &              50,-10.,3990.,0.)
C
                  TITLE = 'PH VS DT, WIRE 7/15 -
     & H'//H//' U'//U//' Q'//QD//' S'//S
                  CALL HBOOK2(38000000+ID,TITLE,25,-40.,1660.,
     &              50,-10.,3990.,0.)
C
C HISTOGRAMS FOR DELAY LINES
                  IF (UNIT.EQ.0) THEN
                    TITLE = 'DELAY (EAST + WEST) -
     & H'//H//' U'//U//' Q'//QD//' S'//S
                    CALL HBOOK1(40000000+ID,TITLE,100,0.0,600.,0.)
C
                    TITLE = 'DELAY Z (EAST - WEST) -
     & H'//H//' U'//U//' Q'//QD//' S'//S
                    CALL HBOOK1(41000000+ID,TITLE,100,-500.,500.,0.)
C
                    TITLE = 'DELAY (EAST VS WEST) -
     & H'//H//' U'//U//' Q'//QD//' S'//S
                    CALL HBOOK2(42000000+ID,TITLE,50,0.0,500.,50,0.0,
     &            500.,0.)
                  END IF
C
                  DO 100 WIRE = 0, NWIRE
                    WRITE(W,'(I2)') WIRE
C
C HISTOGRAMS FOR EACH WIRE
C
                    ID = 100000*HALF+10000*UNIT +1000*QDRT+100*SCTR+WIRE
C
                    TITLE = 'NUM HITS -
     & H'//H//' U'//U//' Q'//QD//' S'//S//' W'//W
                    CALL HBOOK1(1000000+ID,TITLE,12,-0.5,11.5,0.)
C
                    TITLE = 'DRIFT TIME -
     & H'//H//' U'//U//' Q'//QD//' S'//S//' W'//W
                    CALL HBOOK1(2000000+ID,TITLE,100,-100.0,2200.0,0.)
C
                    TITLE = 'PEAK FADC -
     & H'//H//' U'//U//' Q'//QD//' S'//S//' W'//W
                    CALL HBOOK1(8000000+ID,TITLE,70,0.,700.,0.)
C
                    IF (UNIT.EQ.0.AND.WIRE.EQ.0) THEN
                      TITLE = 'PULSE HEIGHT -
     & H'//H//' U'//U//' Q'//QD//' S'//S//' W'//W
                      CALL HBOOK1(12000000+ID,TITLE,100,-10.,7990.,0.)
                    ELSE
                      TITLE = 'PULSE HEIGHT -
     & H'//H//' U'//U//' Q'//QD//' S'//S//' W'//W
                      CALL HBOOK1(12000000+ID,TITLE,100,-10.,4990.,0.)
                    END IF
C
C DON'T BOOK RESIDUAL PLOTS FOR DELAY LINES
                    IF (.NOT.(UNIT.EQ.0.AND.WIRE.GE.8)) THEN
                      TITLE='SEG RESIDUAL-LEFT -
     & H'//H//' U'//U//' Q'//QD//' S'//S//' W'//W
                      CALL HBOOK1(21000000+ID,TITLE,50,-.2,.2,0.)
C
                      TITLE='SEG RESIDUAL-RIGHT -
     & H'//H//' U'//U//' Q'//QD//' S'//S//' W'//W
                      CALL HBOOK1(22000000+ID,TITLE,50,-.2,.2,0.)
C
C                      TITLE='SEG RESID LEFT VS DRIFT TIME-
C     & H'//H//' U'//U//' Q'//QD//' S'//S//' W'//W
C                      CALL HBOOK2(25000000+ID,TITLE,25,-40.,1660.,
C     &                  40,-.175,0.175,0.)
C
C                      TITLE='SEG RESID RIGHT VS DRIFT TIME-
C     & H'//H//' U'//U//' Q'//QD//' S'//S//' W'//W
C                      CALL HBOOK2(26000000+ID,TITLE,25,-40.,1660.,
C     &                  40,-.175,0.175,0.)
C
                      TITLE='TRK RESIDUAL-LEFT -
     & H'//H//' U'//U//' Q'//QD//' S'//S//' W'//W
                      CALL HBOOK1(23000000+ID,TITLE,50,-.2,.2,0.)
C
                      TITLE='TRK RESIDUAL-RIGHT -
     & H'//H//' U'//U//' Q'//QD//' S'//S//' W'//W
                      CALL HBOOK1(24000000+ID,TITLE,50,-.2,.2,0.)
C
C                      TITLE='TRK RESID LEFT VS DRIFT TIME-
C     & H'//H//' U'//U//' Q'//QD//' S'//S//' W'//W
C                      CALL HBOOK2(27000000+ID,TITLE,25,-40.,1660.,
C     &                  40,-.175,0.175,0.)
C
C                      TITLE='TRK RESID RIGHT VS DRIFT TIME-
C     & H'//H//' U'//U//' Q'//QD//' S'//S//' W'//W
C                      CALL HBOOK2(28000000+ID,TITLE,25,-40.,1660.,
C     &                  40,-.175,0.175,0.)
C
                    END IF
C  Book a resid. hist. for delay line hit for full track fit.
                    IF ((UNIT.EQ.0).AND.(WIRE.EQ.8)) THEN
                      TITLE='TRK RESIDUAL-LEFT DL-
     & H'//H//' U'//U//' Q'//QD//' S'//S//' W'//W
                      CALL HBOOK1(23000000+ID,TITLE,50,-2.,2.,0.)
C
                      TITLE='TRK RESIDUAL-RIGHT DL-
     & H'//H//' U'//U//' Q'//QD//' S'//S//' W'//W
                      CALL HBOOK1(24000000+ID,TITLE,50,-2.,2.,0.)
                    END IF
C
  100             CONTINUE
                END IF
  200         CONTINUE
  300       CONTINUE
  400     CONTINUE
  500   CONTINUE
C
C        CALL FDC_PWC_BOOK
C
        CALL INTMSG(' FDBHST FINISHED')
C
        FDBHST = .TRUE.
        FIRST = .FALSE.
      ELSE
        CALL INTMSG(' FDBHST called, histograms not changed.')
      END IF
C
      RETURN
      END
