      SUBROUTINE PSEGLT(HALF,SECTOR)
C------------------------------------------------------------------------
C
C  Routine for finding track segments in one sector of PHI unit
C  of Forward Drift Chamber, using the 'link_and_tree' method.
C  Uses 'active' hits, selected earlier. Depending on application, 'active'
C  hits are all hits in the cell or hits along a road.
C
C  Inputs: HALF,SECTOR
C
C-   Created  xx-DEC-1988   Daria Zieminska 
C-   Updated  20-MAR-1990   Jeffrey Bantly  use logical format,any path  
C-   Updated   3-MAY-1990   Jeffrey Bantly  ineff->pineff, set phi only ineff 
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS files 
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C
C------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FLOCAL.INC'
C
      INTEGER HALF,SECTOR
      INTEGER LLINK,NLINK,NNEXT(MAXLNK),INEXT(MXNEXT,MAXLNK)
      INTEGER IPASS,MXPASS,INEFF,ILINK,LCHAI,NACTIV,NHUSED
      INTEGER NCHAIN,ICHAIN,NHIT,IER
      INTEGER IGAP1,IGAP2,IWRB1,LOCL,IBRNCH,IGAP(MAXLNK),IWRB(MAXLNK)
      INTEGER IE1,IWRE1,IB2,IWRB2,IWRE2,LOC2,LINK2,NLINK2
      INTEGER ISTART,NSTART,IWIRE,ICALL
      INTEGER LZFIND,NZFIND
      REAL SL1,SL2,DSL,DSLMAX(4)
      SAVE ICALL,INEFF,DSLMAX,MXPASS
      DATA ICALL/0/
C------------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('PINEFF',INEFF,IER)
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
        IF (IPASS.GT.1)   CALL PLKDEL(HALF,SECTOR) ! delete used links
C
C  Build links with gap=IPASS
C
        CALL PLINKS(HALF,SECTOR,NACTIV,IPASS)
C
        LLINK=LQ(LFLOC-1)
        NLINK=IQ(LFLOC+1)                    ! number of links
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
C  Build chains
C  Chains are stored in Zebra bank 'CHAI'.
C
        DO 400 ILINK=1,NLINK
          IF (NNEXT(ILINK).EQ.0) GO TO 400
          IF (ILINK.GT.1.AND.IWRB(ILINK).EQ.IPASS.AND.IGAP(ILINK).EQ.
     &          IPASS) GO TO 500
          IF ((ILINK.EQ.1.AND.IPASS.EQ.1).OR.IWRB(ILINK).LT.IPASS)
     &          CALL PCLIMB(ILINK,NNEXT,INEXT,IPASS)
          LCHAI=LQ(LFLOC-2)
          NCHAIN=IQ(LCHAI+1)
          IF (NCHAIN.GE.MXNSEG) GO TO 500
  400   CONTINUE
  500   CONTINUE
C
C  Store track segments
C
        NHUSED=IQ(LFLOC+2)
        DO 600 ICHAIN=1,MIN(NCHAIN,MXNSEG)
          NHIT=IQ(LCHAI+1+NBPSEN*(ICHAIN-1)+1)  ! number of hits on chain
          IF ((NHIT.EQ.17-IPASS).OR.(NHIT.EQ.15.AND.IPASS.EQ.1).OR.
     &    (NHIT.EQ.NACTIV-NHUSED)) CALL PCHAIN(HALF,SECTOR,ICHAIN,NHIT)
          NHUSED=IQ(LFLOC+2)
  600   CONTINUE
        NHUSED=IQ(LFLOC+2)
        IF (NCHAIN.GE.MXNSEG) GO TO 900
        IF (NACTIV-NHUSED.LT.NBPSEN-INEFF) GO TO 999
C
  100 CONTINUE
C
C  Try to use up remaining hits in roads and save them as chains
C
  900 CONTINUE
      IF (NACTIV-NHUSED.LT.NBPSEN-INEFF) GO TO 999
C
C-----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
