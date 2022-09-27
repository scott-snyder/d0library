      SUBROUTINE VERTE1(LAYER,SECTOR,Z0AVR,NENTRY)
C------------------------------------------------------------------------
C
C  Get pairs of hits on first and last wire in cell (LAYER,SECTOR) in VTX
C  and check if they point to (x,y)=(0,0) within tolerance. Find z(r=0)
C  and fill a histogram of z(r=0)
C
C  D.Zieminska  Nov. 1988
C-   Updated  28-AUG-1992   Daria Zieminska  correct HBOOK call
C-   Updated   2-OCT-1992   Qizhong Li-Demarteau  fix the bug for the
C-                                        case of NHITS(WIRE2).GT.CROWD
C-   Updated   7-DEC-1992   Qizhong Li-Demarteau  added EZRSET and EZERROR,
C-                                                also added SAVE statement
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INTEGER LAYER,SECTOR
      INTEGER NENTRY,NWIRES,NEL,NWORDS
      PARAMETER (NWIRES=7)      ! maximum wire# (counting from 0)
      REAL CONT(18)
      INTEGER NHITS(0:NWIRES),IPTR(0:NWIRES),MXHSEC,MXHWIR
      PARAMETER (MXHSEC=200)
      PARAMETER (MXHWIR=50)
      INTEGER  I10,I20,I1,WIRE1,I2,WIRE2,LR, IHIT,IHIT1,NHIT,NMAX
      INTEGER  LVALS,IPAL1,IPAL2,IZBIT1,IZBIT2,SIGN,NID
      REAL DRIFT1,DRIFT2,TOL,Y0,Z0,STAG1,STAG2,STAG,SLOPE,IMPACT,Z0AVR
      REAL DELTAZ,MINPUL,ZLOW,ZHI,ZLIMIT(0:2)
      REAL     XHIT1,YHIT1,RHIT1,ZHIT1,PHI1,ZBIT1
      REAL     XHIT2,YHIT2,RHIT2,ZHIT2,PHI2,ZBIT2
      EQUIVALENCE (IZBIT1,ZBIT1)
      EQUIVALENCE (IZBIT2,ZBIT2)
      REAL PI
      PARAMETER   ( PI=3.141593)
C
      REAL QHIT(18*MXHSEC)
C
      INTEGER IER,ICALL,NBIN,CROWD,MAXWIRE1
      CHARACTER*4 PATH
      LOGICAL EZERROR
      SAVE ICALL
      DATA ICALL/0/
      DATA WIRE2/7/
      DATA ZLIMIT/50.,55.,60/  ! don't use hits with z > ZLIMIT
C
      WIRE1=0
      IF (ICALL.EQ.0) THEN
        STAG1=C(LC(LVGEH-3)+31+WIRE1)
        STAG2=C(LC(LVGEH-3)+31+WIRE2)
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('VERTEX','VERTE1',
     &       'Unable to find bank VERTEX_RCP','W')
          GOTO 1000
        ENDIF
        CALL EZGET('TOL',TOL,IER)
        CALL EZGET('MINPUL',MINPUL,IER)
        CALL EZGET_i('CROWD',CROWD,IER)
        CALL EZGET_i('MAXWIRE1',MAXWIRE1,IER)
        CALL EZGET_i('NBIN',NBIN,IER)
        CALL EZGET('ZHI',ZHI,IER)
        CALL EZRSET
        ZLOW=-ZHI
        DELTAZ=(ZHI-ZLOW)/FLOAT(NBIN)
        CALL PATHGT(PATH)
        ICALL=1
      END IF
      STAG=STAG1
      CALL GTVSEC(LAYER,SECTOR,'WIR',0,NEL,NWORDS,CONT)
      CALL UCOPY(CONT,NHITS,NEL)
      IF (NHITS(WIRE2).GT.CROWD) GO TO 1000
      CALL UCOPY(CONT(NEL+1),IPTR,NEL)
      CALL GTVSEC(LAYER,SECTOR,'ALL',IPTR(0),NEL,NWORDS,QHIT)
      LVALS=LC(LC(LC(LSVTX-5)-(LAYER+1))-(SECTOR+1))
      SIGN=(-1)**SECTOR
      IPAL2=LVALS+6+IC(LVALS+6)*WIRE2
      XHIT2 = C(IPAL2+1)
      YHIT2 = C(IPAL2+2)
      RHIT2=SQRT(XHIT2**2+YHIT2**2)
      I20=IPTR(WIRE2)-2*NEL-4-NWORDS
  400 CONTINUE
      IPAL1=LVALS+6+IC(LVALS+6)*WIRE1
      XHIT1 = C(IPAL1+1)
      YHIT1 = C(IPAL1+2)
      RHIT1=SQRT(XHIT1**2+YHIT1**2)
      I10=IPTR(WIRE1)-2*NEL-4-NWORDS
      IF (NHITS(WIRE2).EQ.0.AND.NHITS(WIRE1).EQ.0) GO TO 1000
      DO 100 IHIT=1,NHITS(WIRE2)
        I2=I20+IHIT*NWORDS
        ZBIT2=QHIT(I2+10)
        IZBIT2=IBITS(IZBIT2,0,2)
        IF (IZBIT2.NE.3) GO TO 100
        ZHIT2=QHIT(I2+4)
        IF (ABS(ZHIT2).GT.ZLIMIT(LAYER)) GO TO 100
        DO 200 IHIT1=1,NHITS(WIRE1)
        I1=I10+IHIT1*NWORDS
        ZBIT1=QHIT(I1+10)
        IZBIT1=IBITS(IZBIT1,0,2)
        IF (IZBIT1.NE.3) GO TO 200
        ZHIT1=QHIT(I1+4)
        IF (ABS(ZHIT1).GT.ZLIMIT(LAYER)) GO TO 200
        LR=0
  300   CONTINUE
        DRIFT2=QHIT(I2+2+LR)-STAG2*SIGN
        DRIFT1=QHIT(I1+2+LR)-STAG*SIGN
c        IF (ABS(DRIFT1).GT.ABS(DRIFT2)) GO TO 100
        XHIT1 = C(IPAL1+1)+DRIFT1*C(LVALS+3) ! hit coordinates in D0 frame
        YHIT1 = C(IPAL1+2)+DRIFT1*C(LVALS+4)
        XHIT2 = C(IPAL2+1)+DRIFT2*C(LVALS+3)
        YHIT2 = C(IPAL2+2)+DRIFT2*C(LVALS+4)
        SLOPE=(YHIT2-YHIT1)/(XHIT2-XHIT1)
        Y0=(YHIT1+YHIT2-(XHIT1+XHIT2)*SLOPE)/2.
        IMPACT=ABS(Y0)/SQRT(1.+SLOPE**2)
        IF (IMPACT.LT.TOL) THEN
          Z0=(ZHIT2+ZHIT1-(RHIT2+RHIT1)*(ZHIT2-ZHIT1)/
     &          (RHIT2-RHIT1))/2.
          IF (ABS(Z0).LT.ZHI) THEN
C
C  Don't use weak pulses if they give Z0 inconsistent with Z0AVR
C
            IF (NENTRY.GT.0.AND.ABS(Z0-Z0AVR).GT.12.) THEN
              IF (QHIT(I1+7).LT.MINPUL.OR.QHIT(I2+7).LT.MINPUL) THEN
                GO TO 200
              END IF
            END IF
            Z0AVR=(Z0AVR*FLOAT(NENTRY)+Z0)/FLOAT(NENTRY+1)
            IF (NENTRY.EQ.0) THEN
              CALL HBOOK1(1,' z(r=0) from VTX hits $',NBIN,ZLOW,ZHI,0.)
C              CALL HIDOPT(1,'STAT')
            END IF
C            CALL HFILL(1,Z0,0.,1.)
            CALL HFF1(1,NID,Z0,1.)
            NENTRY=NENTRY+1
          END IF
        ELSE
          IF (LR.EQ.0) THEN
            LR=1
            GO TO 300
          END IF
        END IF
  200   CONTINUE
  100 CONTINUE
      IF (WIRE1.LT.MAXWIRE1) THEN
        WIRE1=WIRE1+1
        STAG=-STAG
        GO TO 400
      END IF
 1000 RETURN
      END
