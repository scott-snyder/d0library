      SUBROUTINE TSEGLT(HALF,QUAD,SECTOR)
C------------------------------------------------------------------------
C
C  Routine for finding track segments in x-y plane in one THETA cell
C  (HALF,QUAD,SECTOR) of Forward Drift Chamber, using the 'link_and_tree'
C  method.
C  Uses 'active' hits, selected earlier. Depending on application, 'active'
C  hits are all hits in the cell or hits along a road.
C
C  Inputs:
C          HALF,QUAD,SECTOR
C
C-   Created  xx-DEC-1990   Daria Zieminska
C-   Updated  20-MAR-1990   Jeffrey Bantly  use logical format, any path
C-   Updated   3-MAY-1990   Jeffrey Bantly  tineff->theta only inefficiencies
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS 
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C
C------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:FLOCAL.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,QUAD,SECTOR
      INTEGER ICALL,IER
      INTEGER LLINK,NLINK,NNEXT(MAXLNK),INEXT(MXNEXT,MAXLNK)
      INTEGER IPASS,MXPASS,INEFF,ILINK,LCHAI,NACTIV,NHUSED
      INTEGER NCHAIN,ICHAIN,NHIT
      INTEGER IGAP1,IGAP2,IWRB1,LOCL,IBRNCH,IGAP(MAXLNK),IWRB(MAXLNK)
      INTEGER IE1,IWRE1,IB2,IWRB2,IWRE2,LOC2,LINK2,NLINK2
      INTEGER ISTART,NSTART,IWIRE
      INTEGER LZFIND,NZFIND
C
      REAL SL1,SL2,DSL,DSLMAX(4)
C
      DATA ICALL/0/
C------------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_i('TINEFF',INEFF,IER)
        CALL EZGET('DSLMAX',DSLMAX,IER)
        MXPASS=INEFF+1
        CALL EZRSET
        ICALL=1
      END IF
C
C  Do the work in 'passes'.
C  For IPASS=1 find track segments with no missing hits,
C      .......
C      IPASS=4                          3  missing hits.
C
      IQ(LFLOC+1)=0
      IQ(LFLOC+2)=0
      CALL VZERO(NNEXT,MAXLNK)
      CALL VZERO(INEXT,MAXLNK*MXNEXT)
      NACTIV=IQ(LFLOC+3)               ! number of 'active' hits
      NCHAIN=0
      DO 100 IPASS=1,MXPASS
        IF (IPASS.GT.1)   CALL TLKDEL(HALF,QUAD,SECTOR) ! delete used links
C
C  Build links with gap=IPASS
C
        IF (SECTOR.LT.3) THEN          ! no wire stagger
          CALL TLINK1(HALF,QUAD,SECTOR,NACTIV,IPASS)
        ELSE                           ! wire stagger
          CALL TLINK2(HALF,QUAD,SECTOR,NACTIV,IPASS)
        END IF
C
        LLINK=LQ(LFLOC-1)
        NLINK=IQ(LFLOC+1)              ! number of links
        IF (NLINK.LT.3) GO TO 100
        IF (NLINK.GT.MAXLNK) GO TO 999
C
C  Build elementary trees for all links.
C  A tree has to satisfy the local cut:
C  ABS(SL1-SL2).LT.DSLMAX, where SL1,SL2 are slopes of the two links
C  (defined by Y=SL*X+Y0).
C
        DO 200 ILINK=1,NLINK
          LOCL=LZFIND(IXCOM,LLINK,ILINK,-5)
          IF (IPASS.GT.1) THEN
            IF (LOCL.LE.0) THEN                ! used link
              IF (NNEXT(ILINK).GT.0) THEN
                DO 201 IBRNCH=1,NNEXT(ILINK)
                  INEXT(IBRNCH,ILINK)=0        ! cancel its branches
  201           CONTINUE
                NNEXT(ILINK)=0
                GO TO 200                      ! skip used link
              END IF
            END IF
          END IF
          SL1=Q(LOCL+5)             ! slope of link ILINK
          IE1=IQ(LOCL+2)            ! 2-nd hit on link ILINK
          IWRB1=IQ(LOCL+3)
          IWRE1=IQ(LOCL+4)
          IGAP1=IWRE1-IWRB1
          IGAP(ILINK)=IGAP1
          IWRB(ILINK)=IWRB1
C
C  Find all potential branches for link ILINK.
C  Check if slopes agree. If they do, the links make an elementary tree.
C
          LLINK=LQ(LFLOC-1)
          NLINK2=NZFIND(IXCOM,LLINK,IE1,1)
          IF (NLINK2.EQ.0) GO TO 200
          DO 300 LINK2=1,NLINK2
            LOC2=IQUEST(LINK2)
            IWRB2=IQ(LOC2+3)
            IWRE2=IQ(LOC2+4)
            IGAP2=IWRE2-IWRB2
            IF (IGAP1.EQ.IPASS.OR.IGAP2.EQ.IPASS) THEN
              SL2=Q(LOC2+5)
              DSL=ABS(SL1-SL2)
              IF (DSL.LT.DSLMAX(IPASS)) THEN
                NNEXT(ILINK)=NNEXT(ILINK)+1
                INEXT(NNEXT(ILINK),ILINK)=IQ(LOC2-5)
                IF (NNEXT(ILINK).EQ.MXNEXT) GO TO 200
              END IF
            END IF
  300     CONTINUE
  200   CONTINUE
C
C  Build chains starting from wire 0 (require wire 0 on chain for
C  delay line information)
C  Chains are stored in Zebra bank 'CHAI'.
C
        DO 400 ILINK=1,NLINK
C        IF (IWRB(ILINK).EQ.0.AND.NNEXT(ILINK).GT.0)
C     X      CALL TCLIMB(ILINK,NNEXT,INEXT,IPASS)
          IF (IWRB(ILINK).LT.IPASS.AND.NNEXT(ILINK).GT.0)
     X      CALL TCLIMB(ILINK,NNEXT,INEXT,IPASS)
          LCHAI=LQ(LFLOC-2)
          NCHAIN=IQ(LCHAI+1)
          IF (NCHAIN.GE.MXNSEG) GO TO 500   ! don't make more chains
  400   CONTINUE
  500   CONTINUE
C
C  Store track segments
C
        NHUSED=IQ(LFLOC+2)
        DO 600 ICHAIN=1,MIN(NCHAIN,MXNSEG)
          NHIT=IQ(LCHAI+1+NBTSEN*(ICHAIN-1)+1)  ! number of hits on chain
          IF ((NHIT.EQ.9-IPASS).OR.(NHIT.EQ.7.AND.IPASS.EQ.1).OR.(NHIT
     &   .EQ.NACTIV-NHUSED)) CALL TCHAIN(HALF,QUAD,SECTOR,ICHAIN,NHIT)
  600   CONTINUE
C
        NHUSED=IQ(LFLOC+2)
        IF (NACTIV-NHUSED.LT.NBTSEN-INEFF) GO TO 999
        IF (NCHAIN.GE.MXNSEG) GO TO 900
C
  100 CONTINUE
C
C  Try to use up remaining hits in roads and save them as chains
C
  900 CONTINUE
      IF (NACTIV-NHUSED.LT.NBTSEN-INEFF) GO TO 999
C
C-------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
