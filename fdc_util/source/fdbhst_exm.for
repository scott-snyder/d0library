      FUNCTION FDBHST_EXM()
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
C    UPDATED 05-17-89 SUSAN BLESSING
C    Modified for use with D0USER  17-JAN-1990  Susan Blessing
C-   Updated   1-FEB-1991   Robert E. Avery   Change histograms for
C-                                      D0 Cosmic Ray Commissioning
C-   Updated  22-MAR-1991   Susan K. Blessing  Modify for use with Examine2
C-   Updated  26-APR-1991   Susan K. Blessing  New FTRAKS.RCP and FDPARA.PARAMS
C-   Updated  22-OCT-1993   Susan K. Blessing  Split sectors 0 and 1 from
C-    sectors 2-5 in PH histograms
C
C--------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      LOGICAL FDBHST_EXM
C
      INTEGER ID
      INTEGER HALF,UNIT,QDRT,SCTR,WIRE
      INTEGER HA,UN,QU,SE,WI
      INTEGER LAYER
      INTEGER NQDRT,NSCTR,NWIRE,NBIN,NBINT,NBINDL,NBINP
      INTEGER IER
      INTEGER LRCP
C
      LOGICAL FIRST
      LOGICAL FDBHST_PH,FDBHST_DT,FDBHST_PF
      LOGICAL OK,FDC_XSECT_BOOK
      LOGICAL FDC_ONLY
C
      CHARACTER*1 QD
      CHARACTER*2 S,W,L
      CHARACTER*5 H,U
      CHARACTER*50 TITLE
C
      DATA FIRST/.TRUE./
      DATA FDC_ONLY/.FALSE./
C
C-------------------------------------------------------------------------
C
      IF (FIRST) THEN
C
        CALL EZLOC('FDC_RCP',LRCP)
        IF (LRCP.GT.0) THEN
          CALL EZPICK('FDC_RCP')
          CALL EZGET('FDC_ONLY',FDC_ONLY,IER)
          CALL EZRSET
        END IF
C FIRST BOOK HISTOGRAMS WHICH BELONG IN THE CD DIRECTORY
C
        CALL DHDIR('FDC_RCP','HBOOK_CD',IER,' ')
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('FDC','FDBHST_EXM',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
C
        DO HALF = 0, MXHALF
          IF (HALF.EQ.0) THEN
            H = 'North'
          ELSE
            H = 'South'
          END IF
C
          NBINT = (MXWIRT+1)*(MXSECT+1)*(MXQUAD+1)
          NBINDL = 2*(MXSECT+1)*(MXQUAD+1)
          NBINP = (MXWIRP+1)*(MXSECP+1)
C
          TITLE = 'FDC SW Hits/Wire Total-'//H//' Theta'
          CALL HBOOK1(400+HALF*10,TITLE,NBINT,-0.5,
     &      FLOAT(NBINT)-0.5,0.)
C
          TITLE = 'FDC DL Hits/Wire Total-'//H//' Theta'
          CALL HBOOK1(401+HALF*10,TITLE,NBINDL,-0.5,
     &      FLOAT(NBINDL)-0.5,0.)
C
          TITLE = 'FDC SW Hits/Wire Total-'//H//' Phi'
          CALL HBOOK1(402+HALF*10,TITLE,NBINP,-0.5,
     &      FLOAT(NBINP)-0.5,0.)
C
          TITLE = 'FDC SW Hits/Wire on Segs-'//H//' Theta'
          CALL HBOOK1(403+HALF*10,TITLE,NBINT,-0.5,
     &      FLOAT(NBINT)-0.5,0.)
C
          TITLE = 'FDC DL Hits/Wire on Segs-'//H//' Theta'
          CALL HBOOK1(404+HALF*10,TITLE,NBINDL,-0.5,
     &      FLOAT(NBINDL)-0.5,0.)
C
          TITLE = 'FDC SW Hits/Wire on Segs-'//H//' Phi'
          CALL HBOOK1(405+HALF*10,TITLE,NBINP,-0.5,
     &      FLOAT(NBINP)-0.5,0.)
C
          TITLE = 'FDC SW Segments/Wire-'//H//' Theta'
          CALL HBOOK1(420+HALF*10,TITLE,NBINT,-0.5,
     &      FLOAT(NBINT)-0.5,0.)
C
          TITLE = 'FDC DL Segments/Wire-'//H//' Theta'
          CALL HBOOK1(421+HALF*10,TITLE,NBINDL,-0.5,
     &      FLOAT(NBINDL)-0.5,0.)
C
          TITLE = 'FDC SW Segments/Wire-'//H//' Phi'
          CALL HBOOK1(422+HALF*10,TITLE,NBINP,-0.5,
     &      FLOAT(NBINP)-0.5,0.)
C
        END DO
C
          TITLE = 'FDC Ave PH by Supply-Theta SW0, S0-S1'
          CALL HBOOK1(440,TITLE,16,-0.5,15.5,0.)
C
          TITLE = 'FDC Ave PH by Supply-Theta SW, S0-S1'
          CALL HBOOK1(441,TITLE,16,-0.5,15.5,0.)
C
          TITLE = 'FDC Ave PH by Supply-Theta SW0, S2-S5'
          CALL HBOOK1(442,TITLE,16,-0.5,15.5,0.)
C
          TITLE = 'FDC Ave PH by Supply-Theta SW, S2-S5'
          CALL HBOOK1(443,TITLE,16,-0.5,15.5,0.)
C
          TITLE = 'FDC Ave PH by Supply-Phi'
          CALL HBOOK1(444,TITLE,16,-0.5,15.5,0.)
C
C
C HISTOGRAMS WHICH BELONG IN THE FDC DIRECTORY
        CALL DHDIR('FDC_RCP','HBOOK_FDC',IER,' ')
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('FDC','FDBHST_EXM',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
C
        OK = FDC_XSECT_BOOK()
C
        IF (FDC_ONLY) THEN
C Track histograms
C FDC tracks/event
          CALL HBOOK1(10,'FDC Tracks/Event',50,-.5,49.5,0.)
C
C Number of wires on FDC track
          CALL HBOOK1(20,'Wires/FDC Track',35,-.5,34.5,0.)
C
C CHI**2
          CALL HBOOK1(30,'CHI**2, FDC Track',50,-10.,30.,0.)
C
        END IF
C
C Nhits per event:
        CALL HBOOK1(90,'Number of Hits/Event',10,0.,10000.,0.)
C
        DO HALF = 0, MXHALF
          IF (HALF.EQ.0) THEN
            H = 'North'
          ELSE
            H = 'South'
          END IF
C
          NBINT = (MXWIRT+1)*(MXSECT+1)*(MXQUAD+1)
          NBINDL = 2*(MXSECT+1)*(MXQUAD+1)
          NBINP = (MXWIRP+1)*(MXSECP+1)
C
          TITLE = 'FDC SW Sum of Peak Height, Seg Hits-'//H//' Theta'
          CALL HBOOK1(100+HALF*10,TITLE,NBINT,-0.5,FLOAT(NBINT)-0.5,0)
C
          TITLE = 'FDC DL Sum of Peak Height, Seg Hits-'//H//' Theta'
          CALL HBOOK1(101+HALF*10,TITLE,NBINDL,-0.5,FLOAT(NBINDL)-0.5,0)
C
          TITLE = 'FDC SW Sum of Peak Height, Seg Hits-'//H//' Phi'
          CALL HBOOK1(102+HALF*10,TITLE,NBINP,-0.5,FLOAT(NBINP)-0.5,0)
C
        END DO
C
C HISTOGRAMS INVOLVING ALL HITS
C
        DO HALF = 0, MXHALF
          IF (HALF.EQ.0) THEN
            H = 'North'
          ELSE
            H = 'South'
          END IF
C
C DRIFT TIME
          TITLE = 'FDC Drift Time, on Segs, '//H//' Theta'
          CALL HBOOK1(3000+100*HALF,TITLE,
     &      100,50.,150.,0.)

          TITLE = 'FDC Drift Time, on Segs, '//H//' Phi'
          CALL HBOOK1(3010+100*HALF,TITLE,
     &      100,50.,150.,0.)
C
C PULSE HEIGHT BY SECTOR FOR THETA
          DO SCTR = 0, MXSECT
            WRITE(S,'(I2)') SCTR
            ID = 100*HALF + 10*SCTR
C
            TITLE = 'FDC Pulse Height-'//H//' Theta S'
     &            //S//' SW2-7'
            CALL HBOOK1(37000+ID,TITLE,75,0.,4000.,0.)
            CALL HIDOPT(37000+ID,'STAT')
C
            TITLE = 'FDC Pulse Height-'//H//' Theta S'
     &            //S//' SW0'
            CALL HBOOK1(38000+ID,TITLE,75,0.,5000.,0.)
            CALL HIDOPT(38000+ID,'STAT')
C
          END DO
C
          DO UNIT = 0, MXUNIT
            IF (UNIT.EQ.0) THEN
              U = 'Theta'
            ELSE
              U = 'Phi'
            END IF
C
            IF (UNIT.EQ.0) THEN
              NQDRT = MXQUAD
              NWIRE = MXWIRT + 2
              NBIN = (MXSECT+1)*(NWIRE+1)*NQDRT
     &             + (NWIRE+1)*MXSECT + NWIRE + 1
            ELSE IF (UNIT.EQ.1) THEN
              NQDRT = MXQUAD            ! 8 HV "quadrants"
              NWIRE = MXWIRP
              NBIN = (NWIRE+1)*MXSECP + MXWIRP + 1
            END IF
C
            IF (FDC_ONLY) THEN
              ID = 100*HALF + 10*UNIT
C
              TITLE = 'FDC Hits/Segment-'//H//' '//U
              CALL HBOOK1(13000+ID,TITLE,17,-.5,16.5,0.)
            END IF
C
            DO QDRT = 0, NQDRT
C
C HISTOGRAMS FOR EACH QDRT
C
C THETA
              ID = 100*HALF + 10*UNIT + QDRT
              IF ( UNIT .EQ. 0 ) THEN
                WRITE(QD,'(I1)') QDRT
C
C SW0 Sectors 0 and 1
                TITLE = 'FDC Pulse Height-'//H//' '//U//' Q'
     &            //QD//' S0-S1, SW0'
                CALL HBOOK1(30000+ID,TITLE,75,0.,5000.,0.)
                CALL HIDOPT(30000+ID,'STAT')
C
C SW Sectors 0 and 1
                TITLE = 'FDC Pulse Height-'//H//' '//U//' Q'
     &            //QD//' S0-S1, SW2-7'
                CALL HBOOK1(31000+ID,TITLE,75,0.,4000.,0.)
                CALL HIDOPT(31000+ID,'STAT')
C
C DL Sectors 0 and 1
                TITLE = 'FDC Pulse Height-'//H//' '//U//' Q'
     &            //QD//' S0-S1, DL'
                CALL HBOOK1(32000+ID,TITLE,75,0.,4000.,0.)
                CALL HIDOPT(32000+ID,'STAT')
C
C SW0 Sectors 2-5
                TITLE = 'FDC Pulse Height-'//H//' '//U//' Q'
     &            //QD//' S2-S5, SW0'
                CALL HBOOK1(33000+ID,TITLE,75,0.,4000.,0.)
                CALL HIDOPT(33000+ID,'STAT')
C
C SW Sectors 2-5
                TITLE = 'FDC Pulse Height-'//H//' '//U//' Q'
     &            //QD//' S2-S5, SW2-7'
                CALL HBOOK1(34000+ID,TITLE,75,0.,4000.,0.)
                CALL HIDOPT(34000+ID,'STAT')
C
C DL Sectors 2-5
                TITLE = 'FDC Pulse Height-'//H//' '//U//' Q'
     &            //QD//' S2-S5, DL'
                CALL HBOOK1(35000+ID,TITLE,75,0.,4000.,0.)
                CALL HIDOPT(35000+ID,'STAT')
C
              ELSE
C PHI
                TITLE = 'FDC Pulse Height-'//H//' '//U//'HVQ'//QD
                CALL HBOOK1(36000+ID,TITLE,75,0.,4000.,0.)
                CALL HIDOPT(36000+ID,'STAT')
C
              END IF
C
            END DO
          END DO
        END DO
C
        FDBHST_EXM = .TRUE.
        FIRST = .FALSE.
      END IF
C
      GO TO 999
C
C---------------------------------------------------------------------------
C
      ENTRY FDBHST_PH(HA,UN,QU,SE,WI)
C
      CALL DHDIR('FDC_RCP','HBOOK_FDC',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('FDC','FDBHST_EXM',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
C BOOK PULSE HEIGHT HISTOGRAM FOR INDIVIDUAL WIRE
C
      HALF = HA
      UNIT = UN
      QDRT = QU
      SCTR = SE
      WIRE = WI
C
      IF (HALF.EQ.0) THEN
        H = 'North'
      ELSE
        H = 'South'
      END IF
C
      IF (UNIT.EQ.0) THEN
        U = 'Theta'
      ELSE
        U = 'Phi'
      END IF
      WRITE(QD,'(I1)') QDRT
      WRITE(S,'(I2)') SCTR
      WRITE(W,'(I2)') WIRE
C
      ID = 100000*HALF+10000*UNIT +1000*QDRT+100*SCTR+WIRE
C
      IF (UNIT.EQ.0.AND.WIRE.EQ.0) THEN
        TITLE = 'FDC Pulse Height-'
     &    //H//' '//U//' Q'//QD//' S'//S//' W'//W
        CALL HBOOK1(6000000+ID,TITLE,100,-10.,7990.,0.)
      ELSE
        TITLE = 'FDC Pulse Height-'
     &    //H//' '//U//' Q'//QD//' S'//S//' W'//W
        CALL HBOOK1(3000000+ID,TITLE,100,-10.,5990.,0.)
      END IF
C
      FDBHST_PH = .TRUE.
C
      GO TO 999
C
C------------------------------------------------------------------------
C
      ENTRY FDBHST_DT(HA,UN,QU,SE,WI)
C
      CALL DHDIR('FDC_RCP','HBOOK_FDC',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('FDC','FDBHST_EXM',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
C BOOK HISTOGRAM FOR DRIFT TIME FOR INDIVIDUAL WIRE
C
      HALF = HA
      UNIT = UN
      QDRT = QU
      SCTR = SE
      WIRE = WI
C
      IF (HALF.EQ.0) THEN
        H = 'North'
      ELSE
        H = 'South'
      END IF
C
      IF (UNIT.EQ.0) THEN
        U = 'Theta'
      ELSE
        U = 'Phi'
      END IF
      WRITE(QD,'(I1)') QDRT
      WRITE(S,'(I2)') SCTR
      WRITE(W,'(I2)') WIRE
C
      ID = 100000*HALF+10000*UNIT +1000*QDRT+100*SCTR+WIRE
C
      TITLE = 'FDC Drift Time-'
     &  //H//' '//U//' Q'//QD//' S'//S//' W'//W
      CALL HBOOK1(4000000+ID,TITLE,100,-100.0,4900.0,0.)
C
      FDBHST_DT = .TRUE.
C
      GO TO 999
C
C-------------------------------------------------------------------------
C
      ENTRY FDBHST_PF(HA,UN,QU,SE,WI)
C
      CALL DHDIR('FDC_RCP','HBOOK_FDC',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('FDC','FDBHST_EXM',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
C BOOK HISTOGRAM FOR PEAK FADC FOR INDIVIDUAL WIRE
C
      HALF = HA
      UNIT = UN
      QDRT = QU
      SCTR = SE
      WIRE = WI
C
      IF (HALF.EQ.0) THEN
        H = 'North'
      ELSE
        H = 'South'
      END IF
C
      IF (UNIT.EQ.0) THEN
        U = 'Theta'
      ELSE
        U = 'Phi'
      END IF
      WRITE(QD,'(I1)') QDRT
      WRITE(S,'(I2)') SCTR
      WRITE(W,'(I2)') WIRE
C
      ID = 100000*HALF+10000*UNIT +1000*QDRT+100*SCTR+WIRE
C
      TITLE = 'FDC Peak FADC-'
     &  //H//' '//U//' Q'//QD//' S'//S//' W'//W
      CALL HBOOK1(2000000+ID,TITLE,70,0.,700.,0.)
C
      FDBHST_PF = .TRUE.
C
      GO TO 999
C
C-------------------------------------------------------------------------
C
  999 RETURN
      END
