      SUBROUTINE TCHAIN(HALF,QUAD,SECTOR,ICHAIN,NHIT)
C--------------------------------------------------------------------
C
C    Purpose and Methods : Find theta and phi for a chain from drift 
C                          and delay line measurement for wire 0. 
C                          Store the chain as a track segment in 
C                          Zebra bank FSGx.
C
C    Input : HALF,QUAD,SECTOR = Logical location of sector
C            ICHAIN = chain number
C            NHIT   = number of hits on the chain
C
C-   Created  xx-DEC-1988   Daria Zieminska
C-   Updated  16-MAR-1990   Jeffrey Bantly  use logical format
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS 
C-   Updated  16-MAY-1991   Susan K. Blessing  Add NDELAY for number of
C-    delay lines hits.
C-   Updated  17-JUN-1991   Susan K. Blessing  Add SLOPE,INTER and CHISQ
C-    of segment fit and AVE_ION to LDTSEG call.
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated  15-APR-1993   Robert E. Avery  Add errors to LDTSEG call.
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C
C--------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FLOCAL.INC'
C
      INTEGER HALF,QUAD,SECTOR,WIRE
      INTEGER ICHAIN,NHIT,LRWIR(NBTSEN),IHIT(NBTSEN)
      INTEGER LCHAI,ILINK,LLINK,LWIRE,NEL,NWORDS
      INTEGER LOC,LH,ID,J1,IH,JH,LR,NHUSED
      INTEGER II,LKFTSC
      INTEGER IQHIT(18)
      INTEGER ICALL,ILAYER,IWIRE(NBTSEN),JBIT
      INTEGER GZFTSC,LZFIND
      INTEGER NDELAY
C
      REAL CONT(18),QHIT(18),PHI1,PHI2
      REAL XC,YC,ZC,PHI,RC,RHIT,DELAY,EDELAY,THETA
      REAL RESID(NBPSEN),DRIFTD(NBPSEN),ZLOC(NBPSEN)
      REAL SLOPE,INTER,VARIANCE
      REAL AVE_ION,ERROR
      REAL DL_DIST, DL_ERROR
C
      EQUIVALENCE (IQHIT,QHIT)
C
C---------------------------------------------------------------------
C
C  Get and check link values.
C
      IF (LFLOC.LE.5) GOTO 999
      LLINK = LQ(LFLOC-1)
      IF (LLINK.LE.5) GOTO 999
      LCHAI = LQ(LFLOC-2)
      IF (LCHAI.LE.5) GOTO 999
C
C  Loop over hits on chain and find their coordinates
C
      DO 200 ID = 1,NHIT
        IF (ID.LT.NHIT) THEN
          ILINK = IQ(LCHAI+1+(ICHAIN-1)*8+1+ID)
          LOC = LZFIND(IXCOM,LLINK,ILINK,-5)
          IF (LOC.LE.0) GO TO 999         ! link deleted
          J1 = 1
        END IF
        IF (ID.EQ.NHIT) J1 = 2
        IHIT(ID) = IQ(LOC+J1)/2
        LR = JBIT(IQ(LOC+J1),1)
        WIRE = IQ(LOC+J1+2)
        LRWIR(ID) = WIRE*2+LR
        IWIRE(ID) = WIRE
        CALL GTFTSC(HALF,QUAD,SECTOR,'HIT',IHIT(ID),NEL,NWORDS,QHIT)
        DRIFTD(ID) = QHIT(2+LR)
        CALL GTFALH(HALF,0,QUAD,SECTOR,WIRE,XC,YC,ZC)
        IF (ZC.EQ. 0.0) GOTO 999
        ZLOC(ID) = ZC
        IF (WIRE.EQ.0) THEN
          DELAY = QHIT(4)
          EDELAY = QHIT(6)
C Find number of delay hits used
          IF (IQHIT(11).GT.0.AND.IQHIT(12).GT.0) THEN
            NDELAY = 2
          ELSE IF (IQHIT(11).GT.0.OR.IQHIT(12).GT.0) THEN
            NDELAY = 1
          ELSE
            NDELAY = 0
          END IF
          IF (EDELAY.GT.10.) EDELAY = 9.9
          RC = SQRT(XC**2+YC**2)
          RHIT = SQRT(XC**2+YC**2+DELAY**2)
          PHI1 = ATAN2(YC,XC)
          PHI2 = ATAN2(DELAY,RC)
          PHI = PHI1 + PHI2
          IF (YC.LT.0.) PHI = PHI+TWOPI
          IF (PHI.LT.0.) PHI = PHI+TWOPI
          THETA = ATAN2(RHIT,ZC)
        END IF
  200 CONTINUE
C
C  Fit segment to get the residuals.
C
      CALL SEGFIT(ZLOC,DRIFTD,NHIT,SLOPE,INTER,VARIANCE,RESID)
C
C  Good track segment.
C  Flag hits on track segment and 
C Find average ionization/hit
      AVE_ION = 0.
      NHUSED = 0
      LKFTSC = GZFTSC(HALF,QUAD,SECTOR)
      IF (LKFTSC.LE.5) GOTO 999
      DL_DIST = 0.
      DL_ERROR = 9999.
      DO 400 ID = 1,NHIT
        JH = LKFTSC+IHIT(ID)-1
        IH = IQ(JH+9)
        AVE_ION = AVE_ION + Q(JH+7)
        IF (.NOT.BTEST(IH,IUSED)) THEN
          IQ(JH+9) = IBSET(IH,IUSED)             ! Flag hit ID
          NHUSED = NHUSED+1
        END IF
        IF (BTEST(IH,0) .OR. BTEST(IH,1)) THEN
          DL_DIST = Q(JH+4)
          DL_ERROR = Q(JH+6)
        ENDIF
  400 CONTINUE
      ERROR = Q(JH+5) 
      AVE_ION = AVE_ION/FLOAT(NHIT)
C
C  Store this segment in Zebra bank
C
      CALL LDTSEG(HALF,QUAD,SECTOR,NHIT,LRWIR,IHIT,
     &  PHI,THETA,RESID,SLOPE,INTER,VARIANCE,AVE_ION,NDELAY,
     &  ERROR, DL_DIST, DL_ERROR)
C
C  Increment number of used hits in this sector
C
      IF (LFLOC.LE. 5) GOTO 999
      IQ(LFLOC+2) = IQ(LFLOC+2)+NHUSED
C----------------------------------------------------------------------
  999 RETURN
      END
