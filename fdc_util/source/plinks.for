      SUBROUTINE PLINKS(HALF,SECTOR,NACTV,IPASS)
C------------------------------------------------------------------------
C
C  Make two-hit links in one phi sector of Forward Drift Chamber
C  Loop over active hits & build links. Active hits are hits in current
C  sector which have not been flagged in routine FCHEKH and have not
C  been used (i.e. assigned to successfully fitted track segment).
C  For the current IPASS make links spanning gap = IPASS.
C  In general, due to the left-right ambiguity:  2 hits <--> 4 links.
C  In order to keep the number of links within reasonable limits, for high
C  multiplicity events allow only for approximately  central tracks.
C  Use the fact that for tracks coming from the origin d(drift)/dz=const*drift
C  where const is approx. 0.01.
C  The unused hits may be picked up later (not implemented).
C
C  Create ZEBRA banks LINK (link banks)
C
C  Inputs:
C          HALF,SECTOR
C          NACTV         = number of active hits
C          IPASS
C
C-   Created  xx-DEC-1988   Daria Zieminska 
C-   Updated  16-MAR-1990   Jeffrey Bantly  general cleanup 
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine blocks. 
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C
C------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER NACTV
      INTEGER HALF,SECTOR,IPASS,LAY,WIRE,NEL,NWORDS,NOFF
      INTEGER LLINK,ILINK,NLINK,ICALL,LVRFT
      INTEGER NHITS(0:NBPSEN-1),IPTR(0:NBPSEN-1),IXLINK
      INTEGER I1,I2,J1,J2,ISTAT,WIRE1,WIRE2,IHIT1,IHIT2,LR1,LR2
      INTEGER ISTAT1,ISTAT2,LR2MIN,LR2MAX,TOLCUT,NN
      INTEGER IER
C
      REAL CONT(32)
      REAL XWIRE(0:1,0:NBPSEN-1)
      REAL DELTAZ(4),SL,AVSL,TOLSL,Y1,Y2,DELY,TOLDLY(3),STAT1,STAT2
      REAL YBIG(4)
      REAL QHIT(18*MX_HIT_PSEC)
      EQUIVALENCE (ISTAT1,STAT1),(ISTAT2,STAT2)
C
      LOGICAL BIGT
C
      SAVE ICALL,DELTAZ,YBIG,TOLDLY,TOLSL,IXLINK
      DATA ICALL/0/
C----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL MZFORM('LINK','4I 1F',IXLINK)
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('DELTAZ',DELTAZ,IER)
        CALL EZGET('YBIG',YBIG,IER)
        CALL EZGET('TOLDLY',TOLDLY,IER)
        CALL EZGET('TOLSL',TOLSL,IER)
        CALL EZRSET
        ICALL=1
      END IF
C
      IF (IPASS.EQ.1) THEN
        ILINK=0
        CALL GTFPSC(HALF,SECTOR,'WIR',0,NEL,NWORDS,CONT)
        CALL UCOPY(CONT,NHITS,NEL)
        CALL UCOPY(CONT(NEL+1),IPTR,NEL)
        NOFF=2*NEL+4
        IF (NACTV.LE.NEL) THEN
          TOLCUT=1
        ELSE IF (NACTV.LE.40) THEN
          TOLCUT=2
        ELSE
          TOLCUT=3
        END IF
      END IF
      CALL GTFPSC(HALF,SECTOR,'ALL',IPTR(0),NEL,NWORDS,QHIT)
      DO 200 WIRE1=0,NBPSEN-1-IPASS
        WIRE2=WIRE1+IPASS               ! put back later
        IF (NHITS(WIRE1).EQ.0) GO TO 200
        IF (NHITS(WIRE2).EQ.0) GO TO 200
        DO 300 IHIT1=1,NHITS(WIRE1)
          I1=IPTR(WIRE1)-NOFF+NWORDS*(IHIT1-1)
          STAT1=QHIT(I1+9)
          IF (BTEST(ISTAT1,2)) GO TO 300              ! Skip used hit
          DO 400 LR1=0,1
            Y1=QHIT(I1+2+LR1)
            AVSL=0.01*ABS(Y1)
            IF (ABS(Y1).GT.YBIG(IPASS)) THEN
              BIGT=.TRUE.
            ELSE
              BIGT=.FALSE.
            END IF
            LR2MIN=0
            LR2MAX=1
            IF (BIGT) THEN
              IF (LR1.EQ.0) THEN
                LR2MAX=0
              ELSE
                LR2MIN=1
              END IF
            END IF
            DO 500 IHIT2=1,NHITS(WIRE2)
              I2=IPTR(WIRE2)-NOFF+NWORDS*(IHIT2-1)
              STAT2=QHIT(I2+9)
              IF (BTEST(ISTAT2,2)) GO TO 500            ! Skip used hit
              DO 600 LR2=LR2MIN,LR2MAX
                Y2=QHIT(I2+2+LR2)
                DELY=Y2-Y1
                SL=DELY/DELTAZ(IPASS)
                IF ((ABS(SL)-AVSL).LT.TOLSL) THEN
C
C  Create ZEBRA bank LINK (link bank)
C
                  ILINK=ILINK+1
                  CALL MZBOOK(IXMAIN,LLINK,LFLOC,-1,
     &                                  'LINK',0,0,5,IXLINK,0)
                  IQ(LLINK-5) =ILINK
                  IQ(LLINK+1) =(I1+NOFF)*2+LR1
                  IQ(LLINK+2) =(I2+NOFF)*2+LR2
                  IQ(LLINK+3) =WIRE1
                  IQ(LLINK+4) =WIRE2
                  Q(LLINK+5)  =SL           ! slope=dy/dx
                END IF
                IF (BIGT) THEN
                  IF (LR1.EQ.0.AND.DELY.GT.TOLDLY(TOLCUT))  GO TO 400
                  IF (LR1.EQ.1.AND.DELY.LT.-TOLDLY(TOLCUT)) GO TO 400
                END IF
  600         CONTINUE
              IF (.NOT.BIGT.AND.ABS(DELY).GT.TOLDLY(TOLCUT)) GO TO 400
  500       CONTINUE
  400     CONTINUE
  300   CONTINUE
  200 CONTINUE
C
      IQ(LFLOC+1)=ILINK
C----------------------------------------------------------------------
 1000 CONTINUE
      RETURN
      END
