      FUNCTION VTX_EXM_HST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : defines and fills VTX histograms
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls:
C-
C-   Created  23-JAN-1992   Susan K. Blessing
C-   Modified 10-APR-1992   Liang-ping Chen, define and fill VTX histograms
C-   Modified 04-JUN-1992   Liang-ping Chen, add Histograms in //PAWC/VTX
C-   Updated  16-DEC-1993   Ed Oltman   REDO PULSE AREA VS. HV GROUP -- NOW USES
C-                               HITS ON TRACKS
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      LOGICAL FIRST/.TRUE./
C
      INTEGER ID
      PARAMETER(ID=100)       ! assigned first ID for VTX in CD_EXAMINE
      INTEGER LVWDA,GZVWDA,LVTXT,GZVTXT,LVTTH,OLDLAY,OLDSEC,IH,IADD,LR
      INTEGER LAY,SEC,WIRE,ENDS,HITNUM,LVALS,GZVALS,NWVSEC
      INTEGER NEL,NWORDS,NHITS,PNT,K,NSEC(0:2),TWIRE, NEV
      INTEGER GRPID, GRP, LOSEC, HISEC, LVSEC, GZVSEC, NWIRE
C     INTEGER HVID
      REAL    DRIT,AREA,DZDR,ZVTX,DRFT,X,Y,ZEXT
      REAL    TRGTIM,CH
      REAL    STAGGR,SIGMA,MINDRFT,THETA,DTHETA
C     REAL    ARRAY(2,24), X0, Y0, IMPACT, SINTHE
      REAL    TRIPXY(-1:1,0:2),TRIPRZ(-1:1,0:2),RES,FACT
      REAL    XMI(0:2),XMA(0:2)
      REAL    TMIN(0:2), TMAX(0:2)
      INTEGER GOOD,LVGNL,GZVGNL,IX,NCH(0:2),RUN,RUNNO
      CHARACTER*40 TITLE
      CHARACTER*9 SECT
      CHARACTER*6 INOUT
      INTEGER STATUS
      INTEGER IER
      LOGICAL VTX_EXM_HST
      DATA NSEC   /15,31,31/
      DATA NCH    /  256 ,    512  ,    512/
      DATA XMI    / -0.5 ,  511.5  , 1023.5/
      DATA XMA    /255.5 , 1023.5  , 1535.5/
      DATA STAGGR /.01/      ! WIRE STAGGER IN CM
      DATA SIGMA  /15.0/     ! MAXIMUM EXTERNAL RESIDUAL FOR PH HISTOS
      DATA MINDRFT/0.1/      ! MINIMUM DRIFT DISTANCE FOR PH HISTOS
      DATA TWIRE  /4/        ! TRIPLET WIRE NUMBER
      DATA NEV/0/
      DATA TMIN / 200., 200., 200. /
      DATA TMAX / 1600., 900., 1600. /
C------------------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST=.FALSE.
C
C   Book Histograms which belong in the directory //PAWC/VTX
C
        CALL DHDIR(' ','//PAWC/VTX',IER,' ')
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('VTRAKS','VTX_EXM_HST',
     &        ' ERROR SETTING HBOOK DIRECTORY: //PAWC/VTX,','W')
        ENDIF
        TITLE='VTX Trigger Time (ns)'
        CALL HBOOK1(ID+33, TITLE,100, 200.,  600.,0.)
C
C ****  Area histograms - one for each sense hv channel
C
        DO LAY = 0, 2
          DO GRP = 0, 31  ! 32 sense hv channels per lay
            IF ( LAY .EQ. 0 ) THEN
              WRITE(SECT,'(A,I2)') 'Sector ', GRP/2
            ELSE
              LOSEC = 2*(GRP/2) + MOD(LAY,2)
              HISEC = MOD(LOSEC+1,32)
              WRITE(SECT,'(A,I2,A,I2)') 'Sec ', LOSEC, '-', HISEC
            ENDIF
            INOUT = ' Inner'
            IF ( GRP .GE. 16  ) INOUT = ' Outer'
            TITLE = ' Area: '//SECT//INOUT
            CALL HBOOK1(1000+100*LAY+GRP,TITLE,40,0.,400.,0.)
          ENDDO
        ENDDO
C
C ****  HISTOS RELATED TO VTXTS
C
        FACT = SQRT(2./3.)
        CALL HBOOK1(1500,'VTXT PHI',64,0.,6.2832,0.)
        CALL HBOOK1(1504,'THETA OF ROAD VTXT FOUND IN',64,0.,3.1415,0.)
        CALL HBOOK1(1505,'VTXT THETA RES AT PI/2 (NZ>5)',100,-2.,2.,0.)
        CALL HBOOK1(1506,'VTXT DEDX (NZ>5)',100,0.,5.,0.)
        DO LAY = 0,2
          WRITE(TITLE,'(A,I1)') 'VTXT EXTERNAL Z-RESOL: LAY ',LAY
          CALL HBOOK1(1600+10*LAY,TITLE,50,-20.,20.,0.)
          WRITE(TITLE,'(A,I1)') 'VTX R-PHI TRIPLET RESID: LAY ',LAY
          CALL HBOOK1(1600+10*LAY+1,TITLE,50,-.1,.1,0.)
          WRITE(TITLE,'(A,I1)') 'VTX R-Z TRIPLET RESID: LAY ',LAY
          CALL HBOOK1(1600+10*LAY+2,TITLE,50,-20.,20.,0.)
        ENDDO
C
C   Book Histograms which belong in the directory //PAWC/CD
C
        CALL DHDIR(' ','//PAWC/CD',IER,' ')
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('VTRAKS','VTX_EXM_HST',
     &        ' ERROR SETTING HBOOK DIRECTORY://PAWC/CD,','W')
        END IF
        DO LAY = 0,2
          WRITE(TITLE,'(A,I1,A)') 'VTX L',LAY,', VWDA Time (ns)'
          CALL HBOOK1(ID+LAY+0,TITLE,100,200.,2400.,0.)
          WRITE(TITLE,'(A,I1,A)') 'VTX L',LAy,', Hits vs Ch Number'
          CALL HBOOK1(ID+LAY+3,TITLE,NCH(LAY),XMI(LAY),XMA(LAY),0.)
          WRITE(TITLE,'(A,I1,A)') 'VTX L',LAY,', Sum(Area) vs VWDA Time'
          CALL HBOOK1(ID+LAY+6,TITLE,100,200.,2400.,0.)
          WRITE(TITLE,'(A,I1,A)') 'VTX L',LAY,', Ave(Area) vs VWDA Time'
          CALL HBOOK1(ID+LAY+9,TITLE,100, 200., 2400.,0.)
          WRITE(TITLE,'(A,I1,A)')'Area vs HV group, L',LAY,' in16-out16'
          CALL HBOOK1(ID+LAY+12,TITLE,32,-0.5,31.5,0.)
        ENDDO
C
C ****  We'll take care of beam centers at the end
C
        RUN = RUNNO()
C       CALL VXY_BEAM(RUN,X0,DX,Y0,DY,STATUS)
      END IF
C
C FILL HISTOGRAMS
C
      VTX_EXM_HST = .TRUE.
C
C                 fill the histograms which belong in //PAWC/CD
C
      DO  LAY=0,2
        DO  SEC=0,NSEC(LAY)
          LVWDA=GZVWDA(LAY,SEC)
          IF (LVWDA.GT.0) THEN
            NEL=IQ(LVWDA+2)
            NWORDS=IQ(LVWDA+3)
            LVSEC = GZVSEC(LAY,SEC) ! Need VSEC for area vs HV histos
            NWIRE = IQ(LVSEC+2)
C
C ****  Find ID for AREA vs HV histos
C
            IF ( LAY .EQ. 0 ) THEN
              GRPID = 1000 + SEC + 16  ! Start with an outer wire
            ELSEIF ( LAY .EQ. 1 ) THEN
              GRPID = 1100 + MOD(SEC+31,32)/2 + 16
            ELSE  ! LAY 2
              GRPID = 1200 + SEC/2 + 16
            ENDIF
            DO  WIRE=0,7
              IF ( WIRE .EQ. 1 ) GRPID = GRPID - 16 ! The 16 inner wire chans
              IF ( WIRE .EQ. 7 ) GRPID = GRPID + 16 ! are before the 16 outers
              CALL DHDIR(' ','//PAWC/CD',IER,' ')
              IF ( IER.NE.0 ) THEN
                CALL ERRMSG('VTRAKS','VTX_EXM_HST',
     &            ' ERROR SETTING HBOOK DIRECTORY://PAWC/CD,','W')
              END IF
              DO  ENDS=0,1
                CH = 512*LAY + 16*SEC + 2*WIRE + ENDS
                NHITS=IQ(LVWDA+WIRE*2+ENDS+4)
                IF (NHITS.GE.1)THEN
                  PNT=IQ(LVWDA+NEL+WIRE*2+ENDS+4)
                  DO K=1,NHITS 
                    DRIT=Q(LVWDA+PNT+(K-1)*NWORDS+1)!  Drift time
                    AREA=Q(LVWDA+PNT+(K-1)*NWORDS+2)!  pulse area
                    CALL HF1(ID+0+LAY,DRIT,1.)
                    CALL HF1(ID+3+LAY,CH,1.)
                    CALL HF1(ID+6+LAY,DRIT,AREA)
                  ENDDO
                ENDIF
              ENDDO
C
C ****  Now handle AREA vs HV group histos.  Use VSEC, and for each wire, only
C ****  use first hits, and furthermore demand that the drift time be less than
C ****  100 ns. (~2mm drift)
C
              CALL DHDIR(' ','//PAWC/VTX',IER,' ')
              PNT = IQ(LVSEC + NWIRE + WIRE + 4) ! Pointer to 1st hit on wire
              IF ( PNT .GT. 0 ) THEN             ! There are hits on this wire
                DRIT = Q(LVSEC + PNT + 8)
                IF ( DRIT .GE. TMIN(LAY) .AND.
     &               DRIT .LE. TMAX(LAY) ) THEN
                  AREA = Q(LVSEC + PNT + 6)
                  CALL HF1(GRPID,AREA,1.)
                ENDIF
              ENDIF
            ENDDO  ! Wire from 0 to 7
          ENDIF    ! LVWDA > 0
        ENDDO      ! Loop over sectors
      ENDDO        ! Loop over layers
C
C       fill the histogram which belong in //PAWC/VTX
C
      CALL DHDIR(' ','//PAWC/VTX',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('VTRAKS','VTX_EXM_HST',
     &        ' ERROR SETTING HBOOK DIRECTORY://PAWC/VTX,','W')
      END IF
      CALL VTRGTM(TRGTIM)
      CALL HF1(ID+33,TRGTIM,1.)
C
C ****  NOW, DO HV HISTOS
C
      LVTXT = GZVTXT(0)
      DO WHILE (LVTXT .GT. 0)
C       SINTHE = Q(LVTXT+21)
        DZDR =   Q(LVTXT+14)
        ZVTX =   Q(LVTXT+15)
        LVTTH = LQ(LVTXT-1)
        CALL HF1(1500,Q(LVTXT+6),1.)                    ! PHI
C       IMPACT = (Y0-Q(LVTXT+8))*COS(Q(LVTXT+6))
C    &         - (X0-Q(LVTXT+7))*SIN(Q(LVTXT+6))
C       CALL HF1(1501,IMPACT,1.)                        ! IMPACT
        THETA = DZDR/SQRT(1.+DZDR**2)
        THETA = ACOS(THETA)
        CALL HF1(1504,THETA,1.)                         ! ROAD THETA
        DTHETA = (Q(LVTXT+9)-THETA)/Q(LVTXT+21)**2
        IF (IQ(LVTXT+5) .GT. 5) THEN
          CALL HF1(1505,DTHETA,1.)                      ! DEL(THETA) AT PI/2
          CALL HF1(1506,Q(LVTXT+20),1.)                 ! DE/DX
        ENDIF
        OLDLAY = 3
        OLDSEC = 32
        GOOD = 0
C       CALL VZERO(ARRAY,48)
        CALL VFILL(TRIPXY(1,0),9,9999.)
        CALL VFILL(TRIPRZ(1,0),9,9999.)
        DO IH   = 0,IQ(LVTXT+2)-1                       ! LOOP OVER VTXT HITS
          IADD  = IQ(LVTTH+6+4*IH)
          LAY = IBITS(IADD,9,2)
          SEC= IBITS(IADD,4,5)
          WIRE  = IBITS(IADD,1,3)
          LR    = IBITS(IADD,0,1)
          HITNUM = IQ(LVTTH+7+4*IH)
          IF ( LAY .NE. OLDLAY .OR. SEC .NE. OLDSEC ) THEN
            OLDLAY = LAY
            OLDSEC = SEC
            LVGNL = GZVGNL(LAY)
            LVSEC = GZVSEC(LAY,SEC)
            LVALS = GZVALS(LAY,SEC)
            NWVSEC= IQ(LVSEC+3)
C           IF ( LAY .EQ. 0 ) THEN
C             GRPID = 1000 + SEC
C           ELSEIF ( LAY .EQ. 1 ) THEN
C             GRPID = 1100 + MOD(SEC+31,32)/2
C           ELSE
C             GRPID = 1200 + SEC/2
C           ENDIF
          ENDIF
C         HVID = GRPID
C         IF (WIRE .EQ. 0 .OR. WIRE .EQ. 7) HVID = HVID + 16
          PNT = LVSEC + IQ(LVSEC+12+WIRE) + NWVSEC*(HITNUM-1)
          STATUS = IBITS(IQ(PNT+9),0,2)
          IF (STATUS .NE. 3) GO TO 50                       ! UNMATCHED HIT
          DRFT = Q(PNT+1+LR) - STAGGR*(1-2*MOD(WIRE,2))*(1-2*MOD(SEC,2))
          IX = C(LVGNL+6)*ABS(DRFT)
          IF (IX .GT. IC(LVGNL+8+WIRE)) GO TO 50            ! HIT IN PWC REGION
          IF (IABS(WIRE-TWIRE) .LE. 1) THEN
            TRIPXY(WIRE-TWIRE,LAY) = Q(PNT+1+LR)
            TRIPRZ(WIRE-TWIRE,LAY) = Q(PNT+3)
          ENDIF
          IF (ABS(DRFT) .LT. MINDRFT) GO TO 50              ! HIT IN ANDODE
          X = C(LVALS+7+7*WIRE) + C(LVALS+3)*DRFT
          Y = C(LVALS+8+7*WIRE) + C(LVALS+4)*DRFT
          ZEXT = ZVTX + DZDR*SQRT(X**2 + Y**2)
          CALL HF1(1600+10*LAY,ZEXT-Q(PNT+3),1.)
          IF (ABS(ZEXT-Q(PNT+3)) .GT. SIGMA) GO TO 50     ! Z-RES TOO BIG
C         IF (WIRE .EQ. 0 .OR. WIRE .EQ. 7) THEN
C           AREA = Q(PNT+6)/C(LVGNL+24+IX)
C         ELSE
C           AREA = Q(PNT+6)/C(LVGNL+24+IX+IC(LVGNL+15)+1)
C         ENDIF
C         AREA = AREA*SINTHE
          GOOD = GOOD + 1
C         ARRAY(1,GOOD) = AREA
C         ARRAY(2,GOOD) = HVID
   50   ENDDO
C
C ****  INCREMENT TRIPLE HISTOS
C
        DO LAY = 0,2
          DO WIRE = -1,1
            IF (TRIPXY(WIRE,LAY) .GT. 999.) GO TO 60
          ENDDO
          RES = TRIPXY(0,LAY)-0.5*(TRIPXY(-1,LAY)+TRIPXY(1,LAY))
          CALL HF1(1600+10*LAY+1,FACT*RES,1.)
          DO WIRE = -1,1
            IF (TRIPRZ(WIRE,LAY) .GT. 999.) GO TO 60
          ENDDO
          RES = TRIPRZ(0,LAY)-0.5*(TRIPRZ(-1,LAY)+TRIPRZ(1,LAY))
          CALL HF1(1600+10*LAY+2,FACT*RES,1.)
   60   ENDDO
C
C ****  NOW DO HIT TRUNCATION
C
C        IF (GOOD .EQ. 0) GO TO 100
C        DROP = NINT(0.2*FLOAT(GOOD))
C        IF (DROP .GE. 1) CALL SORTR(ARRAY,2,GOOD,1)
C       DROP = 0
C       DO IH = 1,GOOD-DROP
C         AREA = ARRAY(1,IH)
C         HVID = ARRAY(2,IH)
C         CALL HF1(HVID,AREA,1.)
C       ENDDO
  100   LVTXT = LQ(LVTXT)
      ENDDO
C
      NEV = NEV + 1
      IF ( MOD(NEV,25) .EQ. 0 ) THEN
        CALL DHDIR(' ','//PAWC/CD',IER,' ')
        IF ( IER.EQ.0 ) THEN
          DO 500 LAY = 0, 2
            CALL HOPERA(106+LAY,'/',100+LAY,109+LAY,1.,1.)
  500     CONTINUE
          CALL VTX_AVE_PH
        ELSE
          CALL ERRMSG('VTRAKS','VTX_EXM_HST',
     &      ' ERROR SETTING HBOOK DIRECTORY://PAWC/CD,','W')
        END IF
      ENDIF
C
  999 RETURN
      END
