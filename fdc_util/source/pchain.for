      SUBROUTINE PCHAIN(HALF,SECTOR,ICHAIN,NHIT)
C--------------------------------------------------------------------
C
C    Purpose and Methods : Store chain from a  PHI sector as a 
C                          track segment in Zebra bank FSGx.
C
C    Input : HALF,SECTOR = Logical location of sector
C            ICHAIN = chain to be stored
C            NHIT   = number of hits on the chain
C
C-   Created  xx-DEC-1988   Daria Zieminska
C-   Updated  16-MAR-1990   Jeffrey Bantly  general cleanup 
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS 
C-   Updated  17-JUN-1991   Susan K. Blessing  Add SLOPE,INTER,CHISQ for
C-    segment fit.  Add AVE_ION for average ionization/hit for segment.
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated  15-APR-1993   Robert E. Avery  Add errors to LDPSEG call.
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C
C--------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER HALF,SECTOR,ICHAIN,NHIT,LRWIR(NBPSEN),IHIT(NBPSEN),WIRE
      INTEGER LCHAI,ILINK,LLINK,LFASC,LWIRE,NEL,NWORDS
      INTEGER LOC,LH,ID,J1,IH,JH,LR,LR1,NHUSED
      INTEGER LKFPSC
      INTEGER GZFPSC,LZFIND
      INTEGER IWIRE(NBPSEN),JBIT
      INTEGER IQHIT(18),II
C
      REAL CONT(18),QHIT(18),PHI,XC,YC,ZC,DR1,Z1,DRIFTD(NBPSEN),DRIFT
      REAL RESID(NBPSEN),ZLOC(NBPSEN),SLOPE,INTER,VARIANCE
      REAL AVE_ION,ERROR
C
      EQUIVALENCE (IQHIT,QHIT)
C
      DATA RESID/16*0.0/
C----------------------------------------------------------------------
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
          ILINK = IQ(LCHAI+1+(ICHAIN-1)*NBPSEN+1+ID)
          LOC = LZFIND(IXCOM,LLINK,ILINK,-5)
          IF (LOC.LE.0) GO TO 999         ! link deleted
          J1 = 1
        END IF
        IF (ID.EQ.NHIT) J1 = 2
        IHIT(ID) = IQ(LOC+J1)/2
        LR = JBIT(IQ(LOC+J1),1)
        WIRE = IQ(LOC+J1+2)
        LRWIR(ID) = WIRE*2+LR
        IF (ID.EQ.1) LR1 = LR
        IWIRE(ID) = WIRE
        CALL GTFPSC(HALF,SECTOR,'HIT',IHIT(ID),NEL,NWORDS,QHIT)
        DRIFTD(ID) = QHIT(2+LR)
        CALL GTFALH(HALF,1,0,SECTOR,WIRE,XC,YC,ZC)
        IF (ZC.EQ. 0.0) GOTO 999
        ZLOC(ID) = ZC
  200 CONTINUE
      CALL SEGFIT(ZLOC,DRIFTD,NHIT,SLOPE,INTER,VARIANCE,RESID)
C
C  Good track segment.
C  Flag hits on track segment and calculate AVE_ION.
C
      AVE_ION = 0.
      NHUSED = 0
      LKFPSC = GZFPSC(HALF,SECTOR)
      IF (LKFPSC.LE.5) GOTO 999
      DO 400 ID = 1,NHIT
        JH = LKFPSC+IHIT(ID)-1
        IH = IQ(JH+9)
        AVE_ION = AVE_ION + Q(JH+7)
        IF (.NOT.BTEST(IH,IUSED)) THEN
          IQ(JH+9) = IBSET(IH,IUSED)             ! Flag hit ID
          NHUSED = NHUSED+1
        END IF
        IF (ID.EQ.1) DR1 = Q(JH+2+LR1)
  400 CONTINUE
      ERROR = Q(JH+5) 
      AVE_ION = AVE_ION/FLOAT(NHIT)
C
C  Store this segment in Zebra bank
C
      PHI = 0.08727+0.17453*FLOAT(SECTOR)
      CALL LDPSEG(HALF,SECTOR,NHIT,LRWIR,IHIT,PHI,DR1,ZLOC(1),RESID,
     &  SLOPE,INTER,VARIANCE,AVE_ION,ERROR)
C
C  Increment number of used hits in this sector
C
      IF (LFLOC.LE.5) GOTO 999
      IQ(LFLOC+2) = IQ(LFLOC+2)+NHUSED
C----------------------------------------------------------------------
  999 RETURN
      END
