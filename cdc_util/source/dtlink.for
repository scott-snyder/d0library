      SUBROUTINE DTLINK(NACTV,IPASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-  Loop over active hits & build links. Active hits are hits in current 
C-  LAYER,SECTOR which have not been flagged in routine VCHEKH and have not 
C-  been used (i.e. assigned to successfully fitted track segment).
C-  For the current IPASS make links spanning gap = IPASS.
C-  In general, due to the left-right ambiguity:  2 hits <--> 4 links.
C-  In order to keep the number of links within reasonable limits, for high 
C-  multiplicity events a limit (TOLPHI) is set on delta_phi, allowing only 
C-  for approximately  central tracks.
C-  The unused hits may be picked up later (not implemented).
C-
C-   Inputs  : NACTV = the number of currently active hits
C-             IPASS = the pass number
C-   Outputs : none
C-
C-   Created   6-MAR-1992   Qizhong Li-Demarteau  renamed from J.Thomson's
C                           DLINKS, which conflicts with other library routine
C-
C------------------------------------------------------------------------
      IMPLICIT NONE           
      INCLUDE 'D0$INC:ZEBCOM.INC'                             
      INCLUDE 'D0$INC:CDCPAR.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'                             
      INCLUDE 'D0$INC:DLTPAR.INC'
      INCLUDE 'D0$LINKS:IZUSER.LINK'                             
      INTEGER MMBOOK(5),IBOOK,NACTV
      INTEGER IPASS,IBSET,LAY,IDWIRE,NWORDS 
      INTEGER LLINK,ILINK,NLINK,ICALL,LDRFT,LNKLOC
      INTEGER NWIRES,NLAYER
      INTEGER NHITS(0:NCDCEL-1),IPTR(0:NCDCEL-1),IXLINK
      INTEGER I1,I2,WIRE1,WIRE2,IHIT1,IHIT2,LR1,LR2
      INTEGER ISTAT1,ISTAT2,LR2MIN,LR2MAX
      REAL RCELL(0:2),XGLOBL(0:NCDLYR-1,0:NCDCEL-1),XLOCAL(0:NCDCEL-1) 
      REAL TOLPHI,SL,Y1,Y2,DELPHI,STAT1,STAT2 
      REAL DISTBG(4),D1,BIGSLP,MEDSLP,LILSLP
      INTEGER LUSER,GZDRFT 
      INTEGER IER 
      LOGICAL BIGD
      LOGICAL EZERROR
      EQUIVALENCE (ISTAT1,STAT1),(ISTAT2,STAT2)
C                                     
      SAVE ICALL
      DATA IBOOK/4HLINK/
      DATA ICALL/0/
C
      NWIRES = (NCDCEL - 1)       ! maximum wire# (counting from 0)
      NLAYER = (NCDLYR - 1)      ! maximum layer# (counting from 0)
      IF (ICALL.EQ.0) THEN
        CALL MZFORM('LINK','/ 4I 1F 1I',IXLINK)
        ICALL=1
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DTLINK',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('DISTBG',DISTBG,IER)
        CALL EZGET('BIGSLP',BIGSLP,IER)
        CALL EZGET('MEDSLP',MEDSLP,IER)
        CALL EZGET('LILSLP',LILSLP,IER)
        CALL EZRSET
        MMBOOK(1)=IBOOK
        MMBOOK(2)=0 
        MMBOOK(3)=0 
        MMBOOK(4)=(6*MAXLNK)
        MMBOOK(5)=IXLINK 
        LDRFT=GZDRFT()
        DO 3 IDWIRE=0,NWIRES
          XLOCAL(IDWIRE) = C(LDRFT+19+IDWIRE)
    3   CONTINUE
        DO 1 LAY=0,NLAYER
          LDRFT=LC(LDGEH-3)
          RCELL(LAY)=C(LDRFT+11+2*(LAY))
          DO 2 IDWIRE=0,NWIRES
            XGLOBL(LAY,IDWIRE)=RCELL(LAY)+XLOCAL(IDWIRE)
    2     CONTINUE
    1   CONTINUE
      END IF
      LUSER=LQ(LHEAD-IZUSER)
      IF (IPASS.EQ.1) THEN 
        ILINK=0
        NWORDS=IQ(LDSEC(SECTOR,LAYER)+3)
        CALL UCOPY(IQ(LDSEC(SECTOR,LAYER)+4),NHITS,NCDCEL)
        CALL UCOPY(IQ(LDSEC(SECTOR,LAYER)+4+NCDCEL),IPTR,NCDCEL)
        IF (NACTV.LE.NCDCEL) THEN
          TOLPHI=BIGSLP
        ELSE IF (NACTV.LE.40) THEN
          TOLPHI=MEDSLP
        ELSE
          TOLPHI=LILSLP
        END IF
      END IF
      DO 200 WIRE1=0,NWIRES-IPASS
        IF (NHITS(WIRE1).EQ.0) GO TO 200
        WIRE2=WIRE1+IPASS
        IF (NHITS(WIRE2).EQ.0) GO TO 200
        DO 300 IHIT1=NHITS(WIRE1),1,-1        
          I1=LDSEC(SECTOR,LAYER)+IPTR(WIRE1)+NWORDS*(IHIT1-1)
          STAT1=IQ(I1+9)
          IF (BTEST(ISTAT1,2)) GO TO 300              ! Skip used hit
          DO 400 LR1=0,1
            Y1=Q(I1+2+LR1)
            D1=ABS(Q(I1+2))
            IF (D1.GT.DISTBG(IPASS)) THEN
              BIGD=.TRUE.
            ELSE
              BIGD=.FALSE.
            END IF
            LR2MIN=0
            LR2MAX=1
            IF (BIGD) THEN
              IF (LR1.EQ.0) THEN
                LR2MAX=0
              ELSE
                LR2MIN=1
              END IF
            END IF
            DO 500 IHIT2=NHITS(WIRE2),1,-1
              I2=LDSEC(SECTOR,LAYER)+IPTR(WIRE2)+NWORDS*(IHIT2-1)
              STAT2=Q(I2+9)
              IF (BTEST(ISTAT2,2)) GO TO 500            ! Skip used hit
              DO 600 LR2=LR2MIN,LR2MAX 
                Y2=Q(I2+2+LR2)
                DELPHI=Y2/XGLOBL(LAYER,WIRE2)-Y1/XGLOBL(LAYER,WIRE1)
                IF (ABS(DELPHI).LT.TOLPHI) THEN
                  SL=(Y2-Y1)/(XLOCAL(WIRE2)-XLOCAL(WIRE1))
C
C  Create ZEBRA bank LINK (link bank)
C             
                  ILINK=ILINK+1
                  LLINK=LQ(LUSER-1)
                  IF (LLINK .LE. 0) THEN
                     CALL MZLIFT(0,LLINK,LUSER,-1,MMBOOK,0)  
                  END IF
                  LLINK=LQ(LUSER-1)
                  LNKLOC = LLINK + (ILINK-1)*6
                  IF((LLINK+IQ(LLINK-1)-6) .LT. LNKLOC) THEN
                    CALL MZPUSH(IXCOM,LLINK,0,20*6,'I')
                    LLINK=LQ(LUSER-1)
                    LNKLOC = LLINK + (ILINK-1)*6
                  END IF
                  IQ(LNKLOC+1) = (I1)*2+LR1 
                  IQ(LNKLOC+2) = (I2)*2+LR2
                  IQ(LNKLOC+3) = WIRE1 
                  IQ(LNKLOC+4) = WIRE2 
                  Q(LNKLOC+5)  = SL           ! slope=dy/dx
                  IQ(LNKLOC+6) = 0            ! Identifies used links...
                END IF              
                IF (BIGD) THEN
                  IF (LR1.EQ.0.AND.DELPHI.GT.TOLPHI)  GO TO 400 
                  IF (LR1.EQ.1.AND.DELPHI.LT.-TOLPHI) GO TO 400 
                END IF 
  600         CONTINUE
              IF (.NOT.BIGD.AND.ABS(DELPHI).GT.TOLPHI) GO TO 400
  500       CONTINUE
  400     CONTINUE
  300   CONTINUE
  200 CONTINUE
      LUSER=LQ(LHEAD-IZUSER)
      IQ(LUSER+1)=ILINK
 1000 CONTINUE
  999 RETURN                  
      END
