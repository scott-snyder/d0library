      SUBROUTINE VTXC_QDIV(EVENT,N_VTX,THETA,THE_ERR,PHI,
     &  XG,YG,ZG,ZG_ERR,ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find residuals for VTX tracks from ZTRK 
C-
C-   Inputs  : 
C-         EVENT: Event Number
C-         N_VTX: Matched VTX track number with CDC or FDC
C-         THETA: Theta angle of the track provided from CDC or FDC
C-         THE_ERR: Error in theta from CDC or FDC
C-         PHI: PHI angle of the track from CDC or FDC
C-         XG: X of CG from CDC or FDC
C-         YG: Y of CG from CDC or FDC
C-         ZG: Z of CG from CDC or FDC
C-         ZG_ERR: Error on ZG
C-         ID: 3 IF THE TRACK IS FROM CDC
C-             4 IF THE TRACK IF FROM FDC
C-   Outputs : 
C-   Controls:
C_
C-  ENTRY VTXC_QDIV_CLOSE(FILENAME):  Close output file and return the file name
C-
C-   Created   3-JUN-1992   Myungyung Pang
C-   Updated   1-MAY-1993   Ed Oltman  ACCOMADATE NEW VTTH BANK 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER LVTXT,LVTTH,LUN,EVENT,N_VTX
      INTEGER IER,MXLAY,MXWIRE,NSEC(3),GZVTXT
      INTEGER N_EVENT,MXWORD,GZVSEC
      INTEGER LAYER,SECTOR,WIRE,ICONT(16)
      INTEGER NHIT,I_TRAK,N_TRAK
      INTEGER IHIT,HIT_NUM,LABEL,IPOINT,NEL
      INTEGER NWORDS,STATUS,LR,LVSEC,ITRAK(21)
      INTEGER LVWDA,IP0,IP1,GZVWDA,NVWDA0,NVWDA1
      INTEGER IDRIFT,LVALS,ID_MATCH,IPAL,ID
      INTEGER RUNNO, LENGTH, RUN
      REAL HEIGHT0,HEIGHT1,AREA,DRIFT,IUSER,IERR
      REAL QTRAK(21),THETA
      REAL CONT(16),ZG_ERR
      REAL X,Y,Z,XG,YG,ZG,PHI,Z_ZTR
      REAL XY_RES,RZ_RES,AREA0,AREA1
      REAL R,RG,DRIFT_W,THE_ERR
      REAL Z_ZTR_ERR,Z_LENGTH(0:2)
      REAL WIDTH0,WIDTH1
      EQUIVALENCE (QTRAK,ITRAK)
      EQUIVALENCE (CONT,ICONT)
      CHARACTER*6 RUNNUM
      CHARACTER*50 OUTPUT_AREA
      CHARACTER*75 QDIVFILE
      CHARACTER*(*) FILENAME
      SAVE QDIVFILE
      LOGICAL FIRST,SATURATED,NTP2,OPENED
      SAVE LUN
      SAVE IUSER
      DATA FIRST /.TRUE./
      DATA Z_LENGTH / 48.3, 53.3, 58.4 /
      DATA IUSER / 777 /
C-------------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZPICK( 'VTRAKS_RCP' )
        CALL EZGET( 'MXLAY', MXLAY, IER )
        CALL EZGET( 'MXWIRE', MXWIRE, IER )
        CALL EZGET( 'NSEC', NSEC, IER )
        CALL EZRSET
        CALL EZPICK('VTXCOFF_RCP')
        CALL EZGETS('OUTPUT_AREA',1,OUTPUT_AREA,LENGTH,IER)
        CALL EZRSET
        RUN = RUNNO()
        WRITE(RUNNUM,'(I6)') RUN
        QDIVFILE = OUTPUT_AREA//'ZTR_Z_'//RUNNUM//'.DAT'
        CALL GTUNIT(IUSER,LUN,IERR)
        CALL D0OPEN(LUN,QDIVFILE,'OU',OPENED)
        IF ( .NOT. OPENED ) THEN
          CALL ERRMSG('FILE OPEN ERROR','VTXC_QDIV',
     &      'Error opening charge division file','F')
        ENDIF
        FIRST = .FALSE.
      END IF
c
      IF (THETA .EQ. 0. ) GOTO 1000
c
      LVTXT = GZVTXT(N_VTX)
c
      NHIT = IQ(LVTXT+2)
      CALL UCOPY(Q(LVTXT+1),QTRAK,21)
      LVTTH = LQ(LVTXT-1)
      DO IHIT = 0,NHIT-1
        LABEL = IQ(LVTTH+4*IHIT+6)
        HIT_NUM = IQ(LVTTH+4*IHIT+7)
        LAYER = IBITS(LABEL, 9, 3)
        SECTOR = IBITS(LABEL, 4, 5)
        WIRE = IBITS(LABEL, 1, 3)
        LR =   IBITS(LABEL, 0, 1)
        LVSEC = GZVSEC(LAYER,SECTOR)
        CALL GTVSEC(LAYER,SECTOR,'WIR',0,NEL,NWORDS,CONT)
        IPOINT = LVSEC + ICONT(NEL+WIRE+1)           ! Pointer to the first hit
        IPOINT = IPOINT + NWORDS*(HIT_NUM-1) - 1     ! Pointer to the Nth hit
        STATUS = IBITS(IQ(IPOINT+10),0,2)
        SATURATED = .FALSE.
        IF ( IBITS(IQ(IPOINT+10),8,1).EQ.1 .OR.
     &             IBITS(IQ(IPOINT+10),12,1).EQ.1 ) THEN
          SATURATED = .TRUE.
        END IF
        DRIFT = Q(IPOINT+2+LR)                  ! DRIFT DISTANCE
        AREA = Q(IPOINT+7) * SIN(THETA)         ! THETA CORRECTED AREA
        DRIFT_W=DRIFT-(C(LC(LVGEH-3)+31+WIRE))*(-1.)**SECTOR
        LVALS=LC(LC(LC(LSVTX-5)-(LAYER+1))-(SECTOR+1))
        IPAL=LVALS+6+IC(LVALS+6)*WIRE
        X = C(IPAL+1)+DRIFT_W*C(LVALS+3) ! x coordinate in D0 frame
        Y = C(IPAL+2)+DRIFT_W*C(LVALS+4) ! y coordinate in D0 frame
        Z = Q(IPOINT+4)
        R = SQRT(X**2+Y**2)
        RG = SQRT(XG**2+YG**2)
        XY_RES = (Y-YG)*COS(PHI)-(X-XG)*SIN(PHI)
        RZ_RES = (Z-ZG) - (RG-R) / TAN(THETA)
        Z_ZTR = ZG - (RG-R) / TAN(THETA)
        Z_ZTR_ERR = ZG_ERR + THE_ERR * SQRT((Z-ZG)**2+(R-RG)**2)
     &    /SIN(THETA)
        NVWDA0 = IBITS(IQ(IPOINT+10),16,8)      ! Hit # of VWDA for end 0
        NVWDA1 = IBITS(IQ(IPOINT+10),24,8)      !           "           1
        IF ( STATUS.EQ.3 ) THEN
          LVWDA = GZVWDA(LAYER,SECTOR)
          NEL = IQ(LVWDA+2)
          NWORDS = IQ(LVWDA+3)
          IP0 = LVWDA+IQ(LVWDA+NEL+4+WIRE*2)            ! VWDA Pointer
          IP1 = LVWDA+IQ(LVWDA+NEL+4+WIRE*2+1)
          IP0 = IP0 + NWORDS*(NVWDA0-1) - 1
          IP1 = IP1 + NWORDS*(NVWDA1-1) - 1
          HEIGHT0 = Q(IP0+5)
          HEIGHT1 = Q(IP1+5)
          AREA0 = Q(IP0+3)
          AREA1 = Q(IP1+3)
          WIDTH0 = Q(IP0+4)
          WIDTH1 = Q(IP1+4)
          IF ( .NOT. SATURATED ) THEN
            IF ( ABS(Z_ZTR) .LT. Z_LENGTH(LAYER) ) THEN
              WRITE(LUN) LAYER,SECTOR,WIRE,
     &              Z_ZTR,Z_ZTR_ERR,AREA0,AREA1,ID
            END IF
          END IF
        END IF
      END DO
c
 1234 FORMAT(1X,I1,2X,I2,2X,I1,
     &           2(2X,F7.2),2(2X,F8.1),2X,I1)
C
 1000 CONTINUE
      RETURN
C
      ENTRY VTXC_QDIV_CLOSE(FILENAME)
C
      CLOSE(LUN)
      CALL RLUNIT(IUSER,LUN,IERR)
      FILENAME = QDIVFILE
C
      RETURN
      END
