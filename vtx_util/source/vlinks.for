      SUBROUTINE VLINKS(LAYER,SECTOR,NACTV,IPASS)
C------------------------------------------------------------------------  
C
C  Loop over active hits & build links. Active hits are hits in current 
C  LAYER,SECTOR which have not been flagged in routine VCHEKH and have not 
C  been used (i.e. assigned to successfully fitted track segment).
C  For the current IPASS make links spanning gap = IPASS.
C  In general, due to the left-right ambiguity:  2 hits <--> 4 links.
C  In order to keep the number of links withing reasonable limits, for high 
C  multiplicity events a limit (TOLPHI) is set on delta_phi, allowing only 
C  for approximately  central tracks.
C  The unused hits may be picked up later (not implemented).
C  
C  Create ZEBRA banks LINK (link banks)                     
C
C  Inputs: 
C          LAYER,SECTOR
C          NACTV         = number of active hits
C          IPASS
C
C  Daria Zieminska Feb. 1987
C                  Oct. 1988: use VTX_STPFILE.DAT
C                 
C------------------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER MMBOOK(5),IBOOK,NACTV
      INTEGER LAYER,SECTOR,IPASS,IBSET,LAY,WIRE,NEL,NWORDS,NOFF 
      INTEGER LLINK,ILINK,NLINK,ICALL,LVRFT
      INTEGER NWIRES,NLAYER
      INTEGER IWIRE,ILAYER
      PARAMETER (NWIRES=7)      ! maximum wire# (counting from 0)
      PARAMETER (NLAYER=2)      ! maximum layer# (counting from 0)
      REAL CONT(18)
      INTEGER NHITS(0:NWIRES),IPTR(0:NWIRES),IXLINK,MAXHIT
      INTEGER I1,I2,J1,J2,ISTAT,WIRE1,WIRE2,IHIT1,IHIT2,LR1,LR2
      INTEGER ISTAT1,ISTAT2,LR2MIN,LR2MAX
      REAL RCELL(0:2),XWIRE(0:NLAYER,0:NWIRES) 
      REAL TOLPHI,TOLPHI_LT(3),DELTAX(4),SL,Y1,Y2,DELPHI,STAT1,STAT2 
      REAL TIMEBG(4),Y0,Y0MAX,T1
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'                             
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'                             
      INTEGER LUSER 
      PARAMETER (MAXHIT=200)
      REAL QHIT(18*MAXHIT)
      LOGICAL BIGT
      EQUIVALENCE (ISTAT1,STAT1),(ISTAT2,STAT2)
C                                     
      DATA IBOOK/4HLINK/
      DATA ICALL/0/
C
      INTEGER IER 
C----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL MZFORM('LINK','4I 1F',IXLINK)
        ICALL=1
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('TOLPHI_LT',TOLPHI_LT,IER)
        CALL EZGET('DELTAX',DELTAX,IER)
        CALL EZGET('TIMEBG',TIMEBG,IER)
        CALL EZRSET
        MMBOOK(1)=IBOOK
        MMBOOK(2)=0 
        MMBOOK(3)=0 
        MMBOOK(4)=5 
        MMBOOK(5)=IXLINK 
        DO 1 LAY=0,NLAYER
          LVRFT=LC(LVGEH-3)
          RCELL(LAY)=C(LVRFT+7*(LAY+1))
          DO 1 WIRE=0,NWIRES
            XWIRE(LAY,WIRE)=RCELL(LAY)+C(LVRFT+23+WIRE)
    1   CONTINUE
      END IF
      LUSER=LQ(LHEAD-IZUSER)
      IF (IPASS.EQ.1) THEN 
        ILINK=0
        CALL GTVSEC(LAYER,SECTOR,'WIR',0,NEL,NWORDS,CONT)
        CALL UCOPY(CONT,NHITS,NEL)
        CALL UCOPY(CONT(NEL+1),IPTR,NEL)
        NOFF=2*NEL+4
        IF (NACTV.LE.NEL) THEN
          TOLPHI=TOLPHI_LT(1)
        ELSE IF (NACTV.LE.40) THEN
          TOLPHI=TOLPHI_LT(2)
        ELSE
          TOLPHI=TOLPHI_LT(3)
        END IF
      END IF
      CALL GTVSEC(LAYER,SECTOR,'ALL',IPTR(0),NEL,NWORDS,QHIT)
      DO 200 WIRE1=0,NWIRES-IPASS
        IF (NHITS(WIRE1).EQ.0) GO TO 200
        WIRE2=WIRE1+IPASS
        IF (NHITS(WIRE2).EQ.0) GO TO 200
        DO 300 IHIT1=1,NHITS(WIRE1)        
          I1=IPTR(WIRE1)-NOFF+NWORDS*(IHIT1-1)
          STAT1=QHIT(I1+10)
          IF (BTEST(ISTAT1,2)) GO TO 300              ! Skip used hit
          DO 400 LR1=0,1
            Y1=QHIT(I1+2+LR1)
            T1=QHIT(I1+9)
            IF (T1.GT.TIMEBG(IPASS)) THEN
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
              STAT2=QHIT(I2+10)
              IF (BTEST(ISTAT2,2)) GO TO 500            ! Skip used hit
              DO 600 LR2=LR2MIN,LR2MAX 
                Y2=QHIT(I2+2+LR2)
                DELPHI=Y2/XWIRE(LAYER,WIRE2)-Y1/XWIRE(LAYER,WIRE1)
                IF (ABS(DELPHI).LT.TOLPHI) THEN
                  SL=(Y2-Y1)/DELTAX(IPASS)
C
C  Create ZEBRA bank LINK (link bank)
C             
                  ILINK=ILINK+1
                  CALL MZLIFT(0,LLINK,LUSER,-1,MMBOOK,0)  
                  IQ(LLINK-5) =ILINK                                  
                  IQ(LLINK+1) =(I1+NOFF)*2+LR1 
                  IQ(LLINK+2) =(I2+NOFF)*2+LR2
                  IQ(LLINK+3) =WIRE1 
                  IQ(LLINK+4) =WIRE2 
                  Q(LLINK+5)  =SL           ! slope=dy/dx
                END IF              
                IF (BIGT) THEN
                  IF (LR1.EQ.0.AND.DELPHI.GT.TOLPHI)  GO TO 400 
                  IF (LR1.EQ.1.AND.DELPHI.LT.-TOLPHI) GO TO 400 
                END IF 
  600         CONTINUE
              IF (.NOT.BIGT.AND.ABS(DELPHI).GT.TOLPHI) GO TO 400
  500       CONTINUE
  400     CONTINUE
  300   CONTINUE
  200 CONTINUE
      LUSER=LQ(LHEAD-IZUSER)
      IQ(LUSER+1)=ILINK
 1000 CONTINUE
      RETURN                  
      END
