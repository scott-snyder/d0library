      SUBROUTINE TLINK2(HALF,QUAD,SECTOR,NACTV,IPASS)
C------------------------------------------------------------------------
C
C  Make two-hit links in one theta sector (SECTOR>2) of Forward Drift Chamber
C  Loop over active hits & build links. Active hits are hits in current
C  sector which have not been flagged in routine TCHEKH and have not
C  been used (i.e. assigned to successfully fitted track segment).
C  For the current IPASS make links spanning gap = IPASS.
C  In general, due to the left-right ambiguity:  2 hits <--> 4 links.
C  In order to keep the number of links withing reasonable limits, for high
C  multiplicity events a limit (TOLDLY) is set on delta_y, allowing only
C  for approximately  central tracks.
C  The unused hits may be picked up later (not implemented).
C
C  Create ZEBRA banks LINK (link banks)
C
C  Inputs: HALF,QUAD,SECTOR
C          NACTV         = number of active hits
C          IPASS
C
C-   Created  xx-DEC-1988   Daria Zieminska 
C-   Updated  20-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C
C------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:FLOCAL.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER NACTV
      INTEGER HALF,QUAD,SECTOR,IPASS,LAY,WIRE,NEL,NWORDS
      INTEGER LLINK,ILINK,NLINK,ICALL,IER,LVRFT,NOFF
      INTEGER NHITS(0:NBTSEN-1),IPTR(0:NBTSEN-1),IXLINK
      INTEGER I1,I2,J1,J2,ISTAT,WIRE1,WIRE2,IHIT1,IHIT2,LR1,LR2
      INTEGER ISTAT1,ISTAT2,LR2MIN,LR2MAX,TOLCUT,NN
C
      REAL CONT(18)
      REAL TOLDLY(3),DELTAZ(4),SL,Y1,Y2,AVDELY,DELTAY,STAT1,STAT2
      REAL YBIG(4),T1,TANTH(0:5)
      REAL QHIT(18*MX_HIT_TSEC)
      EQUIVALENCE (ISTAT1,STAT1),(ISTAT2,STAT2)
C
      LOGICAL BIGT
C
      SAVE ICALL,DELTAZ,TOLDLY,YBIG,TANTH
      DATA ICALL/0/
C-----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL MZFORM('LINK','4I 1F',IXLINK)
        ICALL=1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('DELTAZ',DELTAZ,IER)
        CALL EZGET('TOLDLY',TOLDLY,IER)
        CALL EZGET('YBIG',YBIG,IER)
        CALL EZGET('TANTH',TANTH,IER)
        CALL EZRSET
      END IF
C
      AVDELY=TANTH(SECTOR)*DELTAZ(IPASS)
      IF (QUAD.LT.4) AVDELY=-AVDELY
      IF (IPASS.EQ.1) THEN
        ILINK=0
        CALL GTFTSC(HALF,QUAD,SECTOR,'WIR',0,NEL,NWORDS,CONT)
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
      CALL GTFTSC(HALF,QUAD,SECTOR,'ALL',IPTR(0),NEL,NWORDS,QHIT)
      DO 200 WIRE1=0,NBTSEN-1-IPASS
        WIRE2=WIRE1+IPASS               ! put back later
        IF (NHITS(WIRE1).EQ.0) GO TO 200
        IF (NHITS(WIRE2).EQ.0) GO TO 200
        DO 300 IHIT1=1,NHITS(WIRE1)
          I1=IPTR(WIRE1)-NOFF+NWORDS*(IHIT1-1)
          STAT1=QHIT(I1+9)
          IF (BTEST(ISTAT1,2)) GO TO 300              ! Skip used hit
          DO 400 LR1=0,1
            Y1=QHIT(I1+2+LR1)
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
                DELTAY=Y2-Y1
                IF (ABS(DELTAY-AVDELY).LT.TOLDLY(TOLCUT)) THEN
                  SL=DELTAY/DELTAZ(IPASS)
C
C  Create ZEBRA bank LINK (link bank)
C
                  ILINK=ILINK+1
                  CALL MZBOOK(IXMAIN,LLINK,LFLOC,-1,
     &                            'LINK',0,0,5,IXLINK,0)
                  IQ(LLINK-5) =ILINK
                  IQ(LLINK+1) =(I1+NOFF)*2+LR1
                  IQ(LLINK+2) =(I2+NOFF)*2+LR2
                  IQ(LLINK+3) =WIRE1
                  IQ(LLINK+4) =WIRE2
                  Q(LLINK+5)  =SL           ! slope=dy/dx
                END IF
                IF (BIGT) THEN
                  IF (LR1.EQ.0.AND.(DELTAY-AVDELY).GT.
     &                                       TOLDLY(TOLCUT))  GOTO 400
                  IF (LR1.EQ.1.AND.(DELTAY-AVDELY).LT.
     &                                       -TOLDLY(TOLCUT)) GOTO 400
                END IF
  600         CONTINUE
              IF (.NOT.BIGT.AND.ABS(DELTAY-AVDELY).GT.
     &                                      TOLDLY(TOLCUT)) GO TO 400
  500       CONTINUE
  400     CONTINUE
  300   CONTINUE
  200 CONTINUE
C
      IQ(LFLOC+1)=ILINK
C---------------------------------------------------------------------
 1000 CONTINUE
      RETURN
      END
