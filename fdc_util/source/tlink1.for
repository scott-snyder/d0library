      SUBROUTINE TLINK1(HALF,QUAD,SECTOR,NACTV,IPASS)
C------------------------------------------------------------------------  
C
C  Make two-hit links in one theta sector (SECTOR<3) of Forward Drift Chamber
C  Loop over active hits & build links. Active hits are hits in current 
C  sector which have not been flagged in routine VCHEKH and have not 
C  been used (i.e. assigned to successfully fitted track segment).
C  For the current IPASS make links spanning gap = IPASS.
C  In order to keep the number of links withing reasonable limits, for high 
C  multiplicity events a limit (TOLDLY) is set on delta_phi, allowing only 
C  for approximately  central tracks.
C  The unused hits may be picked up later (not implemented).
C  
C  Create ZEBRA banks LINK (link banks)                     
C
C  Inputs: HALF,QUAD,SECTOR
C          NACTV         = number of active hits
C          IPASS
C
C-   Created  xx-DEC-1990   Daria Zieminska 
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
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FLOCAL.INC'
C
      INTEGER NACTV
      INTEGER HALF,LAYER,QUAD,SECTOR,IPASS,LAY,WIRE
      INTEGER NEL,NWORDS,NOFF 
      INTEGER LLINK,ILINK,NLINK,ICALL,IER,LVRFT
      INTEGER NHITS(0:NBTSEN-1),IPTR(0:NBTSEN-1),IXLINK
      INTEGER I1,I2,J1,J2,ISTAT,WIRE1,WIRE2,IHIT1,IHIT2 
      INTEGER ISTAT1,ISTAT2,TOLCUT,NN
C
      REAL CONT(18)
      REAL TOLDLY(3),DELTAZ(4),SL,Y1,Y2,DELTAY,AVDELY,STAT1,STAT2 
      REAL T1,TANTH(0:5)
      REAL QHIT(18*MX_HIT_TSEC)
      EQUIVALENCE (ISTAT1,STAT1),(ISTAT2,STAT2)
C
      LOGICAL BIGT
C                                     
      SAVE ICALL,DELTAZ,TOLDLY,TANTH
      DATA ICALL/0/
C--------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL MZFORM('LINK','4I 1F',IXLINK)
        ICALL=1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('DELTAZ',DELTAZ,IER)
        CALL EZGET('TOLDLY',TOLDLY,IER)
        CALL EZGET('TANTH',TANTH,IER)
        CALL EZRSET
      END IF
C
      LAYER=0
      IF(QUAD.GE.4) LAYER=1
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
          Y1=QHIT(I1+2)
          DO 500 IHIT2=1,NHITS(WIRE2)
            I2=IPTR(WIRE2)-NOFF+NWORDS*(IHIT2-1)  
            STAT2=QHIT(I2+9)
            IF (BTEST(ISTAT2,2)) GO TO 500            ! Skip used hit
            Y2=QHIT(I2+2)
            DELTAY=Y2-Y1
            AVDELY=TANTH(SECTOR)*DELTAZ(IPASS)*(-1.)**(SECTOR+LAYER+1)
            IF (ABS(DELTAY-AVDELY).LT.TOLDLY(TOLCUT)) THEN
              SL=(Y2-Y1)/DELTAZ(IPASS)
C
C  Create ZEBRA bank LINK (link bank)
C             
              ILINK=ILINK+1
              CALL MZBOOK(IXMAIN,LLINK,LFLOC,-1,'LINK',0,0,5,IXLINK,0)  
              IQ(LLINK-5) =ILINK                                  
              IQ(LLINK+1) =(I1+NOFF)*2 
              IQ(LLINK+2) =(I2+NOFF)*2 
              IQ(LLINK+3) =WIRE1 
              IQ(LLINK+4) =WIRE2 
              Q(LLINK+5)  =SL           ! slope=dy/dx
            END IF              
  500     CONTINUE
  300   CONTINUE
  200 CONTINUE
C
      IQ(LFLOC+1)=ILINK
C------------------------------------------------------------------------
 1000 CONTINUE
      RETURN                  
      END
