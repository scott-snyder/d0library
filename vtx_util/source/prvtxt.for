      SUBROUTINE PRVTXT(PRUNIT,LOC,ITRA,FLAG,IFLAG)
C------------------------------------------------------------------
C
C     Print out banks VTXT (VTX tracks)
C     IF FLAG='ALL'  all tracks
C     IF FLAG='ONE'  only track ITRA and track-hit assignments (VTTH)
C     IF FLAG='SUM'  only statistics
C
C     D.Z. May 1987
C     Updated 9-Mar-1992  Herbert Greenlee
C         Got rid of machine block.
C-   Updated  20-JAN-1994   Al Clark  Fix bug related to redefinition of 
C-        words 14 and 15 in VTXT bank; remove reference to non-existent
C-        z-strips; Clean up unused variables.
C
C------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT
      INTEGER ICONT(10),ITRA,NTRACK,ITRACK,IWORD,IB0,IB1,IB2,IB3
      INTEGER LAYER,SECTOR,IHSEC,ISATRA(24)
      INTEGER IADD(24),IHIT(24) 
      INTEGER IQTRAK(21),IBITS,NGOODT,SUMHIT,NEL,NWORDS,LOC,IFLAG
      REAL QTRAK(21),CONT(21),QHSEC(4,24),QHZLA(3,6),QHIT(18)
      REAL RESXY(24),RESRZ(24),MULTI,CHIDF,CHISUM
      CHARACTER*3 FLAG
      CHARACTER*4 PATH,VPATH
      EQUIVALENCE(QTRAK,IQTRAK)
      INTEGER IER,ICALL,IPATH
      INTEGER TDF
C
      EQUIVALENCE (IPATH,VPATH)
      LOGICAL TEST
      DATA TEST/.FALSE./
      DATA ICALL/0/
C--------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('VPATH',IPATH,IER)
        CALL EZRSET
        ICALL=1
      END IF
      PATH=VPATH
      IF (FLAG.EQ.'ONE') THEN
        CALL GTVTXT(ITRA,CONT,QHSEC,QHZLA)
        CALL UCOPY(CONT,QTRAK,21)
        CHIDF=QTRAK(12)/FLOAT(IQTRAK(2)-2)
        TDF=IQTRAK(2)+IQTRAK(5)-4
        IB0=IBITS(IQTRAK(3),0,8)
        IB1=IBITS(IQTRAK(3),8,8)
        IB2=IBITS(IQTRAK(3),16,8)
        IB3=IBITS(IQTRAK(3),24,8)
        WRITE (PRUNIT,101)
        WRITE (PRUNIT,102) ITRA,IB3,IB2,IB1,IB0,IQTRAK(2),IQTRAK(5)
     X    ,QTRAK(6),QTRAK(16),QTRAK(7),QTRAK(8),QTRAK(9),QTRAK(18),
     X    QTRAK(10),(QTRAK(IWORD),IWORD=11,13),TDF 
        DO 20 IHSEC=1,IQTRAK(2)
          CALL UCOPY_i(QHSEC(1,IHSEC),IADD(IHSEC),1)
          CALL UCOPY_i(QHSEC(2,IHSEC),IHIT(IHSEC),1)
          CALL UCOPY(QHSEC(3,IHSEC),RESXY(IHSEC),1)
          CALL UCOPY(QHSEC(4,IHSEC),RESRZ(IHSEC),1)
          LAYER =IBITS(IADD(IHSEC),9,3)
          SECTOR=IBITS(IADD(IHSEC),4,5)
          CALL GTVSEC(LAYER,SECTOR,'HIT',IHIT(IHSEC),NEL,NWORDS,QHIT)
          IF (PATH.EQ.'GEAN') CALL UCOPY_i(QHIT(11),ISATRA(IHSEC),1)
   20   CONTINUE
        IF (PATH.EQ.'GEAN'.AND.TEST)
     X    WRITE (PRUNIT,105) (ISATRA(IHSEC),IHSEC=1,IQTRAK(2))
        WRITE (PRUNIT,108) (IADD(IHSEC),IHSEC=1,IQTRAK(2))
        WRITE (PRUNIT,109) (IHIT(IHSEC),IHSEC=1,IQTRAK(2))
        WRITE (PRUNIT,110) (RESXY(IHSEC),IHSEC=1,IQTRAK(2))
        WRITE (PRUNIT,111) (RESRZ(IHSEC),IHSEC=1,IQTRAK(2))
        GO TO 1000
      END IF
      CALL GTVTRH(ICONT)
      NTRACK=ICONT(2)
      NGOODT=0
      SUMHIT=0
      CHISUM=0.
      IF (NTRACK.LE.0) GO TO 1000
      IF (FLAG.EQ.'ALL') WRITE (PRUNIT,101)
      DO 100 ITRACK=1,NTRACK
        CALL GTVTXT(ITRACK,CONT,QHSEC,QHZLA)
        CALL UCOPY(CONT,QTRAK,21)
        IF (IQTRAK(2).EQ.0) GO TO 100
        NGOODT=NGOODT+1
        CHIDF=QTRAK(12)/FLOAT(IQTRAK(2)-2)
        TDF=IQTRAK(2)+IQTRAK(5)-4
        CHISUM=CHISUM+CHIDF
        SUMHIT=SUMHIT+IQTRAK(2)
        IF (FLAG.EQ.'SUM') GO TO 100
        IB0=IBITS(IQTRAK(3),0,8)
        IB1=IBITS(IQTRAK(3),8,8)
        IB2=IBITS(IQTRAK(3),16,8)
        IB3=IBITS(IQTRAK(3),24,8)
        IF (FLAG.EQ.'ALL')
     X    WRITE (PRUNIT,102) ITRACK,IB3,IB2,IB1,IB0,IQTRAK(2),IQTRAK(5)
     X    ,QTRAK(6),QTRAK(16),QTRAK(7),QTRAK(8),QTRAK(9),QTRAK(18),
     X    QTRAK(10),(QTRAK(IWORD),IWORD=11,13),TDF
        IF (PATH.EQ.'GEAN'.AND.TEST) THEN
          DO 200 IHSEC=1,IQTRAK(2)
            CALL UCOPY_i(QHSEC(1,IHSEC),IADD(IHSEC),1)
            CALL UCOPY_i(QHSEC(2,IHSEC),IHIT(IHSEC),1)
            LAYER =IBITS(IADD(IHSEC),9,3)
            SECTOR=IBITS(IADD(IHSEC),4,5)
            CALL GTVSEC(LAYER,SECTOR,'HIT',IHIT(IHSEC),NEL,NWORDS,QHIT)
            CALL UCOPY_i(QHIT(11),ISATRA(IHSEC),1)  ! track ID
  200     CONTINUE
          IF (FLAG.EQ.'ALL')
     X      WRITE (PRUNIT,105) (ISATRA(IHSEC),IHSEC=1,IQTRAK(2))
        END IF
  100 CONTINUE
      IF (NGOODT.EQ.0) THEN
        MULTI=0.
        CHIDF=0.
      ELSE
        MULTI=FLOAT(SUMHIT)/FLOAT(NGOODT)
        CHIDF=CHISUM/FLOAT(NGOODT)
      END IF
  101 FORMAT(/,'  VTXT    b3 b2 b1 b0  NHIT  NZTOT         phi',
     X       '          xg       yg        theta      vzgthe   zg ',
     X       '  CHISQ  CHISQZ',
     X       ' D.F.')
  102 FORMAT(1X,I4,4X,4I3,2I6,2X,F9.4,'+-',F6.4,2F9.4,F8.3,'+-',F5.3,
     X       4F7.1,I4)
  105 FORMAT(' Isajet ID ',24I4)
C  300 WRITE (PRUNIT,106)
  106 FORMAT(/' #hits/track        chisq/d.f.')
C      WRITE (PRUNIT,107) MULTI,CHIDF
  107 FORMAT(1X,F7.1,10X,F7.1)
  108 FORMAT(//' wire  hit addresses: LAYER*512+SECTOR*16+WIRE*2+SIDE',
     X       //,(8I10))
  109 FORMAT(//' pointers to wire hits in VSEC  ',//,(8I10))
  110 FORMAT(//' wire  hit residuals in x-y',//,(8F10.4))
  111 FORMAT(//' wire  hit residuals in r-z',//,(8F10.3))
 1000 CONTINUE
      RETURN
      END
