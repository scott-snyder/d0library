      FUNCTION FDC_PWC_BOOK()
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
C-   Updated  19-JUN-1990   Robert E. Avery  (From fdbhst)
C--------------------------------------------------------------
C
      IMPLICIT NONE
C
      LOGICAL FDC_PWC_BOOK
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL FDEXST
C
      INTEGER ID
      CHARACTER*40 TITLE
C
      INTEGER HALF,UNIT,QDRT,SCTR,WIRE
      INTEGER NQDRT,NSCTR,NWIRE
      INTEGER IER
      REAL    X_BEAM,Y_BEAM
      REAL    X_FDC,Y_FDC
      REAL    X_ANGLE,Y_ANGLE
C
      LOGICAL FIRST
      CHARACTER*17 WIRE_NAME
C
      DATA FIRST/.TRUE./
C
C-------------------------------------------------------------------------
C
      FDC_PWC_BOOK = .TRUE.
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('FDC_RCP')
        CALL EZGET('X_FDC',X_FDC,IER)
        CALL EZGET('Y_FDC',Y_FDC,IER)
        CALL EZGET('X_ANGLE',X_ANGLE,IER)
        CALL EZGET('Y_ANGLE',Y_ANGLE,IER)
        CALL EZRSET

C
C  Statistics
C
        CALL HBOOK1(1100,'STATISTICS',10,-0.5,9.5,0.)
C
C  LOCATION ON CHAMBER vs pwc location
C
        CALL HBOOK2(1110,'X FDC vs X PWC (cm)',
     &                  50,-10.0,10.0,
     &                  50,-10.0,10.0,0.)
        CALL HBOOK2(1120,'Y FDC vs Y PWC (cm)',
     &                  50,-10.0,10.0,
     &                  50,-10.0,10.0,0.)
C
C  Beam rel. to FDC:
C
        CALL HBOOK1(1150,'BEAM X POSITION (cm)',
     &                  200,-X_FDC-5.,-X_FDC+5.,0.)
        CALL HBOOK1(1160,'BEAM Y POSITION (cm)',
     &                  200,-Y_FDC-5.,-Y_FDC+5.,0.)
        CALL HBOOK1(1170,'BEAM X ANGLE',
     &                  100,X_ANGLE-0.04,X_ANGLE+0.04,0.)
        CALL HBOOK1(1180,'BEAM Y ANGLE',
     &                  100,Y_ANGLE-0.04,Y_ANGLE+0.04,0.)
C
        CALL HBOOK1(1250,'X resid (cm)',
     &                  300,-2.5,2.5,0.)
        CALL HBOOK1(1260,'Y resid (cm)',
     &                  300,-2.5,2.5,0.)
C
C HISTOGRAMS INVOLVING ALL HITS
C
        DO 500 HALF = 0, MXHALF
          DO 400 UNIT = 0, MXUNIT
            IF (UNIT.EQ.0) THEN
              NQDRT = MXQUAD
              NSCTR = MXSECT
              NWIRE = MXWIRT
            ELSE IF (UNIT.EQ.1) THEN
              NQDRT = 0
              NSCTR = MXSECP
              NWIRE = MXWIRP
            END IF
            DO 300 QDRT = 0, NQDRT
              DO 200 SCTR = 0, NSCTR
                IF (FDEXST(HALF,UNIT,QDRT,SCTR)) THEN
C
                  DO 100 WIRE = 0,NWIRE
C
                    ID = 100000*HALF+10000*UNIT
     &                +1000*QDRT+100*SCTR+WIRE
                    WRITE(WIRE_NAME,'(A,I1,A,I1,A,I1,A,I2,A,I2)')
     &                ' H',HALF,' U',UNIT,' Q',QDRT,
     &                ' S',SCTR,' W',WIRE
C
                    TITLE = 'DT (nsec) VS X-PWC (cm)'//WIRE_NAME
                    CALL HBOOK2(60000000+ID,TITLE,
     &                120,-6.0,6.0,
     &                100,-50.0,1800.0,200.)
C                  CALL HBOOK2(61000000+ID,TITLE,
C     &                100,-1.0,1.0,
C     &                50,-50.0,500.0,5000.)
                    TITLE = 'X-RESID, wire'//WIRE_NAME
                    CALL HBOOK1(71000000+ID,TITLE,
     &                100,-1.,1.,0.)
C                  TITLE = 'DX (cm) VS X-PWC (cm)'//WIRE_NAME
C                  CALL HBOOK2(72000000+ID,TITLE,
C     &                100,-1.0,1.0,
C     &                100,-1.0,1.0,200.)
C
  100             CONTINUE
                  ID = 100000*HALF+10000*UNIT +1000*QDRT+100*SCTR
                  WRITE(WIRE_NAME,'(A,I1,A,I1,A,I1,A,I2)')
     &                ' H',HALF,' U',UNIT,' Q',QDRT,' S',SCTR

                  TITLE = 'X-RESID (cm), segm '//WIRE_NAME
                  CALL HBOOK1(62000000+ID,TITLE,
     &                60,-1.5,1.5,
     &                5000.)

                  TITLE = 'X-ANGLE-RESID'//WIRE_NAME
                  CALL HBOOK1(63000000+ID,TITLE,
     &                60,-0.1,0.1,
     &                5000.)

                  TITLE = 'X-ANGLE (PWC)'//WIRE_NAME
                  CALL HBOOK1(70000000+ID,TITLE,
     &                100,-1.0,1.0,
     &                5000.)

C   Get position of BEAM W.R.T sector center:
                  CALL FBEAM_CELL(HALF,UNIT,QDRT,SCTR,X_BEAM,Y_BEAM)

                  IF (  UNIT .EQ. 0) THEN ! DL HISOGRAMS
                    TITLE = 'DL-resid (cm)'//WIRE_NAME
                    CALL HBOOK1(64000000+ID,TITLE,
     &                50,-2.5,2.5,5000.)

                    TITLE = 'DL-resid (cm) vs sw0 ph'//WIRE_NAME
                    CALL HBOOK2(65000000+ID,TITLE,
     &                25,0.,2500.,
     &                50,-5.,5.,200.)

                    TITLE = 'DL VS Y-PWC (cm)'//WIRE_NAME
                    CALL HBOOK2(66000000+ID,TITLE,
     &                25,Y_BEAM-5.,Y_BEAM+5.,
     &                25,Y_BEAM-5.,Y_BEAM+5.,
     &                200.)

                    TITLE = 'DL-diff(ns)'//WIRE_NAME
                    CALL HBOOK1(67000000+ID,TITLE,
     &                200,-50.,50.,5000.)

                    TITLE = 'DL-diff(ns) vs sw0 ph'//WIRE_NAME
                    CALL HBOOK2(68000000+ID,TITLE,
     &                25,0.,2500.,
     &                50,-50.,50.,200.)

                  ENDIF
                END IF
  200         CONTINUE
  300       CONTINUE
  400     CONTINUE
  500   CONTINUE
      END IF
C
C--------------------------------------------------------------
      RETURN
      END
