      SUBROUTINE VTX_AREA_GAIN(N_VTX_TRACK,THETA,USAGE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : VTX offline WIRE GAIN calibration.  Called
C-                         for each track.
C-
C-   Inputs  :
C-        N_VTX_TRACK: Matched track number with CDC or FDC
C-        THETA: Theta angle of the track provided from CDC or FDC
C-        USAGE: 0 If it's called during event processing
C-               1 If it's called at the end of job
C-   Outputs :
C-   Controls:
C-
C-   Created   3-JUN-1992   Myungyun Pang
C-   Updated   1-MAY-1993   Ed Oltman  ACCOMADATE CHANGE TO VTTH 
C-   Updated   5-MAR-1994   Roy Thatcher - fix spurious line in D0ENTRY
C-       Changed local variable ENTRY to ENTRY1
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VTXCOFF.INC/LIST'
      INTEGER LVTXT,LVTTH,N_VTX_TRACK
      INTEGER IER,MXLAY,MXWIRE,NSEC(3),GZVTXT
      INTEGER AREA_BIN,DRIFT_BIN
      INTEGER N_EVENT,MXWORD,GZVSEC
      INTEGER LAYER,SECTOR,WIRE,ICONT(16)
      INTEGER MAXBIN,NHIT,I_TRAK,N_TRAK
      INTEGER IHIT,HIT_NUM,LABEL,IPOINT,NEL
      INTEGER NWORDS,STATUS,LR,LVSEC,ITRAK(21)
      INTEGER CATG,TRACK
      INTEGER IAREA,ICATG
      INTEGER IDRIFT,IAREA_BIN
      INTEGER EVONUM,K_AREA,USAGE
      INTEGER HID,IDH,AV_METHODE
      PARAMETER ( MAXBIN = 500 )
      REAL HEIGHT0,HEIGHT1,AREA,DRIFT
      REAL GAIN_HST(0:MAXBIN,0:7,0:31,0:2)
      REAL AREA_DRIFT(0:MAXBIN,-20:20,0:2,0:2)
C               ! LAYER    WIRE 0, 1-6, 7   DRIFT IN mm    AREA
      REAL GAIN,QTRAK(21),DEDX,THETA
      REAL MIN_DRFT_0,MAX_DRFT_0
      REAL MIN_DRFT_1,MAX_DRFT_1
      REAL MIN_DRFT_2,MAX_DRFT_2
      REAL ERROR0(3),ERROR3(3),CONST,AVERAGE,SIGMA,CHI2
      REAL X_AREA,DX,WEIGHT,DISTANCE,ENTRY1,AVERAGE3
      REAL A_NMIN,G_NMIN,HSUM
      REAL MEAN,RMS,HSTATI
      REAL CONT(16),ERROR(0:600)
      REAL F_NORMALIZE(0:10)
      REAL SUM,SUM_SQR,INCLUDE,CONTENT(0:600)
      REAL SUM_ENTRY,DIVIDE,X,MEAN_ERR,DEL_AREA
      REAL TRUNCATION
      EQUIVALENCE (QTRAK,ITRAK)
      EQUIVALENCE (CONT,ICONT)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C-------------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZPICK( 'VTRAKS_RCP' )
        CALL EZGET( 'MXLAY', MXLAY, IER )
        CALL EZGET( 'MXWIRE', MXWIRE, IER )
        CALL EZGET( 'NSEC', NSEC, IER )
        CALL EZRSET
        CALL EZPICK( 'VTXCOFF_RCP' )
        CALL EZGET( 'G_NMIN', G_NMIN, IER )
        CALL EZGET( 'A_NMIN', A_NMIN, IER )
        CALL EZGET( 'AREA_BIN', IAREA_BIN, IER )
        CALL EZGET( 'MIN_DRFT_0', MIN_DRFT_0, IER)
        CALL EZGET( 'MAX_DRFT_0', MAX_DRFT_0, IER)
        CALL EZGET( 'MIN_DRFT_1', MIN_DRFT_1, IER)
        CALL EZGET( 'MAX_DRFT_1', MAX_DRFT_1, IER)
        CALL EZGET( 'MIN_DRFT_2', MIN_DRFT_2, IER)
        CALL EZGET( 'MAX_DRFT_2', MAX_DRFT_2, IER)
        CALL EZGET('DEL_AREA',DEL_AREA,IER)
        CALL EZGET('AV_METHODE',AV_METHODE,IER)
        CALL EZGET('TRUNCATION',TRUNCATION,IER)
        CALL EZRSET
        CALL HBOOK1(127,'AREA',IAREA_BIN,-5.,7.,0.)
        CALL HBOOK1(227,'AREA DIST. SQRT.',IAREA_BIN,-5.,7.,0.)
        CALL HBOOK1(327,'TRUNC. AREA DIST',IAREA_BIN,-5.,7.,0.)
        CALL HBOOK1(130,'ALL CHANNELS',IAREA_BIN,-1000.,1400.,0.)
        CALL HBOOK1(131,'L0-WIRE0',IAREA_BIN,-1000.,1400.,0.)
        CALL HBOOK1(132,'L0-WIRE1-6',IAREA_BIN,-1000.,1400.,0.)
        CALL HBOOK1(133,'L0-WIRE7',IAREA_BIN,-1000.,1400.,0.)
        CALL HBOOK1(134,'L1-WIRE0',IAREA_BIN,-1000.,1400.,0.)
        CALL HBOOK1(135,'L1-WIRE1-6',IAREA_BIN,-1000.,1400.,0.)
        CALL HBOOK1(136,'L1-WIRE7',IAREA_BIN,-1000.,1400.,0.)
        CALL HBOOK1(137,'L2-WIRE0',IAREA_BIN,-1000.,1400.,0.)
        CALL HBOOK1(138,'L2-WIRE1-6',IAREA_BIN,-1000.,1400.,0.)
        CALL HBOOK1(139,'L2-WIRE7',IAREA_BIN,-1000.,1400.,0.)
        MXWORD = 3 * 32 * 8 * (MAXBIN+1)
        CALL VZERO(GAIN_HST,MXWORD)
        CALL VZERO(AREA_DRIFT,(MAXBIN+1)*41*3*3)
        FIRST = .FALSE.
        GOTO 1000
      END IF
C
      IF (USAGE .EQ. 0) THEN                ! PROCESSING DATA
C
        LVTXT = GZVTXT(N_VTX_TRACK)
C
        NHIT = IQ(LVTXT+2)
        CALL UCOPY(Q(LVTXT+1),QTRAK,21)
        DEDX = QTRAK(20)
        LVTTH = LQ(LVTXT-1)
        DO IHIT = 0,NHIT-1
          LABEL = IQ(LVTTH+4*IHIT+6)
          HIT_NUM = IQ(LVTTH+4*IHIT+7)
          LAYER = IBITS(LABEL, 9, 3)
          SECTOR = IBITS(LABEL, 4, 5)
          WIRE = IBITS(LABEL, 1, 3)
          LR = IBITS(LABEL, 0, 1)
          LVSEC = GZVSEC(LAYER,SECTOR)
          CALL GTVSEC(LAYER,SECTOR,'WIR',0,NEL,NWORDS,CONT)
          IPOINT = LVSEC + ICONT(NEL+WIRE+1)  ! Pointer to the first hit
          IPOINT = IPOINT + NWORDS*(HIT_NUM-1) - 1 ! Pointer to the Nth hit
          STATUS = IBITS(IQ(IPOINT+10),0,2)
          DRIFT = Q(IPOINT+2+LR)                  ! DRIFT DISTANCE
          AREA = Q(IPOINT+7) * SIN(THETA)         ! THETA CORRECTED AREA
          IF ( STATUS.EQ.3 ) THEN
C Accumulate areas for gain calibration
            AREA_BIN = NINT(AREA/2.8)
            IF ( AREA_BIN.GT.0 .AND.
     &                    AREA_BIN.LT.MAXBIN ) THEN
              IF (WIRE .EQ. 0) CATG = 0
              IF (WIRE .GE. 1 .AND. WIRE .LE. 6) CATG = 1
              IF (WIRE .EQ. 7) CATG = 2
              IF (( LAYER.EQ.0 .AND.
     &          (ABS(DRIFT).GT.MIN_DRFT_0.AND.ABS(DRIFT).LT.MAX_DRFT_0))
     &          .OR. ( LAYER.EQ.1 .AND.
     &          (ABS(DRIFT).GT.MIN_DRFT_1.AND.ABS(DRIFT).LT.MAX_DRFT_1))
     &          .OR. ( LAYER.EQ.2 .AND.
     &          (ABS(DRIFT).GT.MIN_DRFT_2.AND.ABS(DRIFT).LT.MAX_DRFT_2))
     &          )THEN
                GAIN_HST(AREA_BIN,WIRE,SECTOR,LAYER) =
     &              GAIN_HST(AREA_BIN,WIRE,SECTOR,LAYER) + 1.
                HID = 130 + LAYER*3 + CATG + 1
                CALL HFILL(HID,AREA,0.,1.)
                CALL HFILL(130,AREA,0.,1.)
              END IF
C Accumulate areas for area correction
              DRIFT_BIN = NINT(DRIFT/DEL_AREA)
              IF ( DRIFT_BIN .GT. -20 .AND. DRIFT_BIN .LT. 20 ) THEN
                AREA_DRIFT(AREA_BIN,DRIFT_BIN,CATG,LAYER) =
     &             AREA_DRIFT(AREA_BIN,DRIFT_BIN,CATG,LAYER) + 1.
              END IF
            END IF
          END IF
        END DO
      END IF
C
      IF ( USAGE .EQ. 1 ) THEN             ! END OF JOB, PROCESSING HISTO
C FIND NORMALIZATION CONSTANT FOR GAIN CONSTANTS FOR ALL CHANNELS
C AND ALS0 FOR EACH CATAGORY OF AREA CORRECTION FACTORS
        DO IDH = 130, 139
          CALL HOPERA(IDH,'*',IDH,IDH,.1,.1)
          CALL HUNPAK(IDH,CONTENT,' ',0)
          DO K_AREA = 0,IAREA_BIN
            IF(CONTENT(K_AREA).NE.0.) THEN
              ERROR(K_AREA) = 1.
            ELSE
              ERROR(K_AREA) = 1.
            END IF
          END DO
          CALL HPAKE(IDH,ERROR)
          CALL HFITGA(IDH,CONST,AVERAGE,SIGMA,CHI2,100,ERROR0)
          F_NORMALIZE(IDH-130) = AVERAGE
        END DO
        CALL VZERO(AREA_DIST,41*3*3)
        CALL VZERO(AREA_GAIN,8*32*3)
        CALL VZERO(AREA_DIST_ERR,41*3*3)
        CALL VZERO(AREA_GAIN_ERR,8*32*3)
C Find gain calibration constants
        DO LAYER = 0,MXLAY
          DO SECTOR = 0,NSEC(LAYER+1)
            DO WIRE = 0,MXWIRE
              CALL HRESET(127,' ')
              DX = 2.8/F_NORMALIZE(0)
              X_AREA = - DX
              DO IAREA = 0,MAXBIN
                X_AREA = X_AREA + DX
                WEIGHT = GAIN_HST(IAREA,WIRE,SECTOR,LAYER)
                CALL HF1(127,X_AREA,WEIGHT)
              END DO
              ENTRY1 = HSUM(127)
              IF ( ENTRY1 .GT. G_NMIN ) THEN
                CALL HUNPAK(127,CONTENT,' ',0)
                INCLUDE = ENTRY1 * TRUNCATION            ! TRUNCATION
                SUM = 0.
                SUM_SQR = 0.
                SUM_ENTRY = 0.
                DO K_AREA = 0,IAREA_BIN
                  IF(CONTENT(K_AREA).NE.0.) THEN
                    ERROR(K_AREA) = 1.
                  ELSE
                    ERROR(K_AREA) = 1.
                  END IF
                  SUM_ENTRY = SUM_ENTRY + CONTENT(K_AREA)
                  IF ( SUM_ENTRY .LT. INCLUDE ) THEN
                    CALL HIX(127,K_AREA,X)
                    DIVIDE = SUM_ENTRY
                    SUM = SUM + X * CONTENT(K_AREA)
                    SUM_SQR = SUM_SQR + (X**2) * CONTENT(K_AREA)
                  ELSE
                    CONTENT(K_AREA) = 0.        ! TRUNCATION
                  END IF
                END DO
                IF ( DIVIDE .NE. 0. ) THEN
                  IF ( AV_METHODE .EQ. 1 ) THEN
                    CALL HRESET(327,' ')
                    CALL HPAK(327,CONTENT)
                    CALL HFITGA(327,CONST,AVERAGE3,SIGMA,CHI2,100,
     &                ERROR3)
                    AREA_GAIN(WIRE,SECTOR,LAYER) = AVERAGE3
                    AREA_GAIN_ERR(WIRE,SECTOR,LAYER) = ERROR3(2)
                  ELSE IF ( AV_METHODE .EQ. 2 ) THEN
                    MEAN = SUM / DIVIDE
                    RMS = SQRT( SUM_SQR/DIVIDE - MEAN**2 )
                    MEAN_ERR = RMS / SQRT(DIVIDE)
                    AREA_GAIN(WIRE,SECTOR,LAYER) = MEAN
                    AREA_GAIN_ERR(WIRE,SECTOR,LAYER) = MEAN_ERR
                  ELSE IF ( AV_METHODE .EQ. 3 ) THEN
                    CALL HRESET(227,' ')
                    CALL HOPERA(127,'*',127,227,1.,1.)
                    CALL HPAKE(227,ERROR)
                    CALL HFITGA(227,CONST,AVERAGE,SIGMA,CHI2,0,ERROR0)
                    AREA_GAIN(WIRE,SECTOR,LAYER) = AVERAGE
                    AREA_GAIN_ERR(WIRE,SECTOR,LAYER) = ERROR0(2)
                  END IF
                END IF
              END IF
            END DO
          END DO
        END DO
C
C Find area correction factor
C
        DO LAYER = 0,MXLAY
          DO ICATG = 0,2
            DO IDRIFT = -20,20
              CALL HRESET(127,' ')
              DX = 2.8/F_NORMALIZE(LAYER*3+ICATG+1)
              X_AREA = - DX
              DO IAREA = 0,MAXBIN
                X_AREA = X_AREA + DX 
                WEIGHT = AREA_DRIFT(IAREA,IDRIFT,ICATG,LAYER)
                CALL HF1(127,X_AREA,WEIGHT)
              END DO
              ENTRY1 = HSUM(127)
              IF ( ENTRY1 .GT. A_NMIN ) THEN
                CALL HUNPAK(127,CONTENT,' ',0)
                INCLUDE = ENTRY1 * TRUNCATION            !  TRUNCATION
                SUM = 0.
                SUM_SQR = 0.
                SUM_ENTRY = 0.
                DO K_AREA = 0,IAREA_BIN
                  IF(CONTENT(K_AREA).NE.0.) THEN
                    ERROR(K_AREA) = 1.
                  ELSE
                    ERROR(K_AREA) = 1.
                  END IF
                  SUM_ENTRY = SUM_ENTRY + CONTENT(K_AREA)
                  IF ( SUM_ENTRY .LT. INCLUDE ) THEN
                    CALL HIX(127,K_AREA,X)
                    SUM = SUM + X * CONTENT(K_AREA)
                    SUM_SQR = SUM_SQR + (X**2) * CONTENT(K_AREA)
                  ELSE
                    CONTENT(K_AREA) = 0.        ! TRUNCATION
                  END IF
                END DO
                IF ( SUM_ENTRY .NE. 0. ) THEN
                  IF ( AV_METHODE .EQ. 1 ) THEN
                    CALL HRESET(327,' ')
                    CALL HPAK(327,CONTENT)
                    CALL HFITGA(327,CONST,AVERAGE3,SIGMA,CHI2,100
     &                ,ERROR3)
                    AREA_DIST(IDRIFT,ICATG,LAYER) = AVERAGE3
                    AREA_DIST_ERR(IDRIFT,ICATG,LAYER) = ERROR3(2)
                  ELSE IF ( AV_METHODE .EQ. 2 ) THEN
                    MEAN = SUM / SUM_ENTRY
                    RMS = SQRT( SUM_SQR/SUM_ENTRY - MEAN**2 )
                    MEAN_ERR = RMS / SQRT(SUM_ENTRY)
                    AREA_DIST(IDRIFT,ICATG,LAYER) = MEAN
                    AREA_DIST_ERR(IDRIFT,ICATG,LAYER) = MEAN_ERR
                  ELSE IF ( AV_METHODE .EQ. 3 ) THEN
                    CALL HRESET(227,' ')
                    CALL HOPERA(127,'*',127,227,1.,1.)
                    CALL HPAKE(227,ERROR)
                    CALL HFITGA(227,CONST,AVERAGE,SIGMA,CHI2,0,ERROR0)
                    AREA_DIST(IDRIFT,ICATG,LAYER) = AVERAGE
                    AREA_DIST_ERR(IDRIFT,ICATG,LAYER) = ERROR0(2)
                  END IF
                END IF
              END IF
            END DO
          END DO
        END DO
C
      END IF
C
 1000 RETURN
      END
