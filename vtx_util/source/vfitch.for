      SUBROUTINE VFITCH(LAYER,SECTOR,ICHAIN,NHIT)
C--------------------------------------------------------------------
C
C  Fit a straight line to hits on chain.
C  Store the chain as a track segment in Zebra bank SEGM.
C
C  INPUT:  LAYER,SECTOR
C          ICHAIN = chain to be fitted
C          NHIT   = number of hits on the chain
C
C  Daria Zieminska Feb. 1987
C                  Oct. 1988: use VTX_STPFILE.DAT
C-   Updated  28-OCT-1992   Peter M. Grudberg  Change VSGn bank definition
C-   Updated  12-NOV-1992   Peter M. Grudberg  Fix phi bug 
C-   Updated  23-APR-1993   Al Clark Correct the calculation of SXY for the rz
C-                          fit
C
C--------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LAYER,SECTOR,ICHAIN,NHIT,LRWIR(8),IHIT(8),WIRE,NWIRES
      INTEGER LCHAI,ILINK,LLINK,LVALS,IPAL,NEL,NWORDS
      INTEGER LOC,LH,ID,J1,IH,JH,LR,LZFIND,NHUSED,IBSET
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'
      INTEGER LUSER
      PARAMETER (NWIRES=7)
      REAL DRIFT,AL,XG,YG,ALZ,SG,ZG,CHISQ,CHISQZ,CHISQ_OLD,AL_OLD
      REAL COSAL, SINAL
      REAL XHIT(8),YHIT(8),ZHIT(8),SXY(8),WT(8),WTZ(8),VAL,VALZ,VG
      REAL CONT(18),QHIT(18)
      INTEGER IQHIT(18)
      EQUIVALENCE (IQHIT,QHIT)
      LOGICAL NEW
      INTEGER ILAYER,IWIRE,JBIT
      REAL PI,PI2
      REAL CHIMAX,CHIMXZ
      REAL VZGTHETA, FACTOR, VZG
      REAL PHIG, PHIDIFF
      SAVE ICALL,CHIMAX,CHIMXZ,CHISQ_OLD,AL_OLD
      INTEGER IER,ICALL
      DATA PI,PI2/3.141593,6.283185/
      DATA ICALL/0/
C----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('CHIMAX',CHIMAX,IER)
        CALL EZGET('CHIMXZ',CHIMXZ,IER)
        CALL EZRSET
        CHISQ_OLD=CHIMAX
        AL_OLD=100.
        ICALL=1
      END IF
C
      LUSER=LQ(LHEAD-IZUSER)
      LLINK=LQ(LUSER-1)
      LCHAI=LQ(LUSER-2)
      LVALS=LC(LC(LC(LSVTX-5)-(LAYER+1))-(SECTOR+1))
C
C  Loop over hits on chain and find their coordinates
C
      DO 200 ID=1,NHIT
        IF (ID.LT.NHIT) THEN
          ILINK=IQ(LCHAI+1+(ICHAIN-1)*8+1+ID)
          LOC=LZFIND(0,LLINK,ILINK,-5)
          IF (LOC.LE.0) GO TO 1000         ! link deleted
          J1=1
        END IF
        IF (ID.EQ.NHIT) J1=2
        IHIT(ID)=IQ(LOC+J1)/2
        LR=JBIT(IQ(LOC+J1),1)
        WIRE=IQ(LOC+J1+2)
        LRWIR(ID)=WIRE*2+LR
        CALL GTVSEC(LAYER,SECTOR,'HIT',IHIT(ID),NEL,NWORDS,QHIT)
        DRIFT=QHIT(2+LR)-(C(LC(LVGEH-3)+31+WIRE))*(-1.)**SECTOR
        IPAL=LVALS+6+IC(LVALS+6)*WIRE
        XHIT(ID) = C(IPAL+1)+DRIFT*C(LVALS+3)  ! hit coordinates in D0 frame
        YHIT(ID) = C(IPAL+2)+DRIFT*C(LVALS+4)
        ZHIT(ID) = QHIT(4)
        WT(ID)   = 1./(QHIT(5))**2
        WTZ(ID)  = 1./(QHIT(6))**2
  200 CONTINUE
C
C  Fit in x-y plane
C
      CALL FITLIN(XHIT,YHIT,WT,NHIT,AL,XG,YG,VG,VAL,CHISQ)
      IF (CHISQ.LT.CHIMAX) THEN
        SINAL = SIN(AL)
        COSAL = COS(AL)
        DO 300 ID=1,NHIT
          SXY(ID)=(XHIT(ID)-XG)*COSAL + (YHIT(ID)-YG)*SINAL
  300   CONTINUE
C
C  Fit in s-z plane
C
        CALL FITLOC(SXY,ZHIT,WTZ,NHIT,ALZ,SG,ZG,VZG,VALZ,CHISQZ)
        IF (CHISQZ.LT.CHIMXZ) THEN
C
C  Good track segment.
C  Flag hits on track segment.
C
          NHUSED=0
          DO 400 ID=1,NHIT
            JH=LVSEC(SECTOR,LAYER)+IHIT(ID)-1
            IH=IQ(JH+10)
            IF (.NOT.BTEST(IH,2)) THEN
              IQ(JH+10)=IBSET(IH,2)             ! Flag hit ID
              NHUSED=NHUSED+1
            END IF
  400     CONTINUE

          NEW=.TRUE.
          IF (NHIT-NHUSED.GT.1) THEN
            IF (ABS(AL-AL_OLD).LT.0.01.AND.CHISQ.GT.CHISQ_OLD) THEN
              GO TO 1000
            ELSE
              NEW=.FALSE.
            END IF
          END IF
C
C ****  Make sure the phi angle is not off by PI - it should be very close to
C ****  the phi of the center of gravity.  Then force it into the range from 0
C ****  to twopi.
C
          PHIG = ATAN2(YG,XG)
          PHIDIFF = PHIG - AL
          PHIDIFF = PHIDIFF - NINT(PHIDIFF/PI)*PI
          AL = PHIG - PHIDIFF
          IF (AL.LT.0.) AL=AL+PI2
          IF (AL.GT.PI2) AL=AL-PI2
C
C ****  Convert to D0 frame and calculate errors accordingly
C
          ZG = ZG - SG*TAN(ALZ)
          ALZ=1.570796-ALZ
          FACTOR = SG/(SIN(ALZ)**2)
          VZG = VZG + VALZ*FACTOR**2
          VZGTHETA = VALZ*FACTOR
C
C  Store this segment in Zebra bank, adding matching z-strip clusters.
C
          CALL LDVSEG(LAYER,SECTOR,NHIT,LRWIR,IHIT,AL,XG,YG,VG,VAL,
     1      CHISQ,ALZ,VZGTHETA,ZG,VZG,VALZ,CHISQZ,NEW)
C
C  Increment number of used hits in this sector
C
          IQ(LUSER+2)=IQ(LUSER+2)+NHUSED
          CHISQ_OLD=CHISQ
          AL_OLD=AL
        END IF
      END IF
 1000 RETURN
      END
