      SUBROUTINE PRFDCT(PRUNIT,LOC,ITRA,CFL,IFL)
C------------------------------------------------------------------
C
C     Purpose and Methods : Print out banks FDCT (FDC tracks)
C
C     Inputs  : PRUNIT = output unit
C               LOC    = (not used)
C               ITRA   = Track number to be printed if CFL='ONE'
C               CFL    = Type of printout desired
C                        IF CFL='ALL'  all tracks
C                        IF CFL='ONE'  only track ITRA
C                        IF CFL='SUM'  only statistics
C               IFL    = print level
C                        IFL = 0   no printout
C                        IFL = 1   track parameters only
C                        IFL = 2   1 and specific sectors involved in track
C                        IFL = 3   1,2 and full printout of associated hit info
C
C-   Created  xx-JAN-1989   Daria Zieminska
C-   Updated   1-MAR-1990   Jeffrey Bantly  clean up, path indep.
C-   Updated  23-JAN-1991   Jeffrey Bantly  add track ladder to printout 
C-   Updated  17-SEP-1991   Susan K. Blessing  Add theta and phi errors 
C-    to printout
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated  20-AUG-1992   Robert E. Avery  Add FDC half for each track. 
C
C------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER PRUNIT
      INTEGER ICONT(10),ITRA,NTRACK,ITRACK,IWORD,IB0,IB1,IB2,IB3
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UB
      INTEGER IHSEC,LOC,IFL,LADDER(0:2)
      INTEGER ID(34),IDPRI(34),IDSEC(34),ISATRA(34)
      INTEGER IADD(34),IHIT(34),IADDS(6),IHITS(6)
      INTEGER IQTRAK(26),NGOODT,SUMHIT,NEL,NWORDS
      INTEGER NHIT,LKFDCT
      INTEGER GZFDCT
C
      REAL QTRAK(26),CONT(26),QHSEC(3,34),QHIT(18)
      REAL RESID(34),MULTI,CHIDF,CHISUM
      REAL HITX(34),HITY(34),HITZ(34),WR(34),WZ(34)
C
      CHARACTER*4 PATH
      CHARACTER*3 CFL
      CHARACTER*1 CHALF(0:1)
      EQUIVALENCE(QTRAK,IQTRAK)
C
      DATA CHALF/'N','S'/
C
C---------------------------------------------------------------------
      IF (IFL.EQ.0) GOTO 999
      CALL PATHGT(PATH)
      IF (CFL.EQ.'ONE') THEN
        CALL GTFDCT(ITRA,CONT,QHSEC,LADDER)
        CALL UCOPY(CONT,QTRAK,26)
        CHIDF=QTRAK(19)/FLOAT(IQTRAK(2)-4)
        IB0=IBITS(IQTRAK(3),0,8)
        IB1=IBITS(IQTRAK(3),8,8)
        IB2=IBITS(IQTRAK(3),16,8)
        IB3=IBITS(IQTRAK(3),24,8)
        CALL FGETLDR2(ITRA,LADDER)
        HALF = IAND(1,IQTRAK(1))
        WRITE (PRUNIT,101)
        WRITE (PRUNIT,102) ITRA,IB3,IB2,IB1,IB0,IQTRAK(2),
     X      (QTRAK(IWORD),IWORD=4,6),QTRAK(23),QTRAK(22),QTRAK(24),
     X      QTRAK(7),QTRAK(8),QTRAK(19),CHALF(HALF),
     X      LADDER(0),LADDER(1),LADDER(2)
        CALL UCOPY(QHSEC(1,1),IADD(1),1)
        IF(IADD(1).LE.0) GOTO 999
        DO 20 IHSEC=1,IQTRAK(2)
          CALL UCOPY(QHSEC(1,IHSEC),IADD(IHSEC),1)
          CALL UCOPY(QHSEC(2,IHSEC),IHIT(IHSEC),1)
          CALL UCOPY(QHSEC(3,IHSEC),RESID(IHSEC),1)
          IF (PATH.EQ.'GEAN') THEN
            CALL FCODER(IADD(IHSEC),HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
            IF (UNIT.LE.0) THEN
              CALL GTFTSC(HALF,QUAD,SECTOR,'HIT',IHIT(IHSEC),NEL,
     X           NWORDS,QHIT)
            ELSE
              CALL GTFPSC(HALF,SECTOR,'HIT',IHIT(IHSEC),NEL,NWORDS,QHIT)
            END IF
            CALL UCOPY(QHIT(10),ISATRA(IHSEC),1)
          ENDIF
   20   CONTINUE
        IF (PATH.EQ.'GEAN')
     X  WRITE (PRUNIT,103) (ISATRA(IHSEC),IHSEC=1,IQTRAK(2))
        WRITE (PRUNIT,104) (IADD(IHSEC),IHSEC=1,IQTRAK(2))
        WRITE (PRUNIT,105) (IHIT(IHSEC),IHSEC=1,IQTRAK(2))
        WRITE (PRUNIT,106) (RESID(IHSEC),IHSEC=1,IQTRAK(2))
        GO TO 999
      END IF
      CALL GTFTRH(ICONT)
      NTRACK=ICONT(2)
      NGOODT=0
      SUMHIT=0
      CHISUM=0.
      IF (NTRACK.LE.0) GO TO 999
      IF (CFL.EQ.'ALL') WRITE (PRUNIT,101)
      DO 100 ITRACK=1,NTRACK
        CALL GTFDCT(ITRACK,CONT,QHSEC,LADDER)
        CALL UCOPY(CONT,QTRAK,26)
        IF (IQTRAK(2).EQ.0) GO TO 100
        NGOODT=NGOODT+1
        CHIDF=QTRAK(19)/FLOAT(IQTRAK(2)-4)
        CHISUM=CHISUM+CHIDF
        SUMHIT=SUMHIT+IQTRAK(2)
        IF (CFL.EQ.'SUM') GO TO 100
        IB0=IBITS(IQTRAK(3),0,8)
        IB1=IBITS(IQTRAK(3),8,8)
        IB2=IBITS(IQTRAK(3),16,8)
        IB3=IBITS(IQTRAK(3),24,8)
        CALL FGETLDR2(ITRACK,LADDER)
        HALF = IAND(1,IQTRAK(1))
        IF (CFL.EQ.'ALL')
     X      WRITE (PRUNIT,102) ITRACK,IB3,IB2,IB1,IB0,IQTRAK(2),
     X      (QTRAK(IWORD),IWORD=4,6),QTRAK(23),QTRAK(22),QTRAK(24),
     X      QTRAK(7),QTRAK(8),QTRAK(19),CHALF(HALF),
     X      LADDER(0),LADDER(1),LADDER(2)
        CALL UCOPY(QHSEC(1,1),IADD(1),1)
        IF(IADD(1).LE.0) GOTO 100
        DO 200 IHSEC=1,IQTRAK(2)
          CALL UCOPY(QHSEC(1,IHSEC),IADD(IHSEC),1)
          CALL UCOPY(QHSEC(2,IHSEC),IHIT(IHSEC),1)
          CALL UCOPY(QHSEC(3,IHSEC),RESID(IHSEC),1)
          IF (PATH.EQ.'GEAN') THEN
            CALL FCODER((IADD(IHSEC)/2),HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
            IF (UNIT.LE.0) THEN
              CALL GTFTSC(HALF,QUAD,SECTOR,'HIT',IHIT(IHSEC),NEL,
     X           NWORDS,QHIT)
            ELSE
              CALL GTFPSC(HALF,SECTOR,'HIT',IHIT(IHSEC),NEL,NWORDS,QHIT)
            END IF
            CALL UCOPY(QHIT(10),ISATRA(IHSEC),1)  ! track ID
          ENDIF
  200   CONTINUE
        IF (CFL.EQ.'ALL'.AND.PATH.EQ.'GEAN')
     X      WRITE (PRUNIT,103) (ISATRA(IHSEC),IHSEC=1,IQTRAK(2))
        CALL FCODER((IADD(1)/2),HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        IF (IFL.EQ.3) THEN
          WRITE (PRUNIT,104) (IADD(IHSEC),IHSEC=1,IQTRAK(2))
          WRITE (PRUNIT,105) (IHIT(IHSEC),IHSEC=1,IQTRAK(2))
          WRITE (PRUNIT,106) (RESID(IHSEC),IHSEC=1,IQTRAK(2))
        ENDIF
  100 CONTINUE
C
  101 FORMAT(/,'  FDCT    b3 b2 b1 b0   NHIT    x0       y0',
     X   '       phi    phi err   theta  theta err  dx/dz    dy/dz',
     X   '  chisq   half   ladder')
  102 FORMAT(1X,I4,4X,4I3,I6,8F9.3,E9.2,A4,3I4)
  103 FORMAT((1X,20I4))
  104 FORMAT(//' wire  hit addresses: LOGCHA+SIDE',
     X       //,(8I10))
  105 FORMAT(//' pointers to wire hits in FxDA  ',//,(8I10))
  106 FORMAT(//' wire  hit residuals ',//,(8F10.4))
C-----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
