      SUBROUTINE FXPSEG(HALF,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Form roads with remaining hits across
C-                         Phi sector boundaries.
C-
C-   Inputs  : HALF,SECTOR - Identify which Phi sector being checked
C-
C-   Created  20-MAY-1990   Jeffrey Bantly
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS files
C-   Updated  13-JUN-1991   Jeffrey Bantly  comments and cleanup
C-   Updated  27-NOV-1991   Robert E. Avery  Don't let MXPASS be greater 
C-                              than 4.
C-   Updated  16-MAR-1992   Susan K. Blessing  Restrict MXPASS to 2. 
C-   Updated  29-MAY-1992   Susan K. Blessing  Add NLK and LK to 
C-    PXLINK and PXTREE calls.
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER HALF,SECTOR               ! Logical address of home sector
      INTEGER SECTD1                    ! Adjacent sector being checked
      INTEGER BCHAIN                    ! Number of chains in home sector
      INTEGER D1HITS(0:NBPSEN-1)        ! unused hits in adjac sector
      INTEGER DIR                       ! direction of adjacent sector
      INTEGER DLOC(0:NBPSEN-1,MX_HIT_WIRE*2,3) ! adjac sector unused hit info
      INTEGER HOHITS(0:NBPSEN-1)        ! unused hits in home sector
      INTEGER II                        ! loop counter
      INTEGER ILINK                     ! link loop counter
      INTEGER INEFF                     ! max allowed missing wires
      INTEGER IOFSET                    ! LINK bank offset, see LLINKH,D
      INTEGER IPASS                     ! pass loop counter
      INTEGER ITOT                      ! counter of wires with hits
      INTEGER IWIRE                     ! wire loop counter
      INTEGER JWIREH,JWIRED             ! num of wires w hits, home,adjac
      INTEGER LOC(0:NBPSEN-1,MX_HIT_WIRE*2,3) ! home sector unused hit info
      INTEGER LCHAI                     ! CHAI bank link
      INTEGER LBANK,LBANKD1             ! FPSC bank links for home,adj sectors
      INTEGER LLINKH                    ! home LINK bank link     (LFLOC-1)
      INTEGER LLINKD                    ! adjacent LINK bank link (LFLOC-5)
      INTEGER MXPASS                    ! max allowed wire gap size
      INTEGER NEL,NWORDS                ! dummies used in GTFPSC calls
      INTEGER NHITS(0:NBPSEN-1)         ! Number of hits by wire home sector
      INTEGER NHITS1                    ! sum of hits avail to start/end road
      INTEGER NLINK                     ! number of links in cur LINK bank
      INTEGER WIRBEG,WIREND             ! begin,end wire in home sector hits
      INTEGER WIRBND,WIREEG             ! begin,end wire in adja sector hits
      INTEGER ICALL                     ! initialization check
      INTEGER IER                       ! error check
      INTEGER STAT1 
      INTEGER GZFPSC                    ! function fetches link of FPSC bank
      EXTERNAL GZFPSC
      INTEGER NLK(0:NBPSEN-1,2),LK(0:NBPSEN-1,2)
C
      REAL    CONT(36)                  ! contents of Phi sector hit bank
      REAL    YL(0:NBPSEN-1,MX_HIT_WIRE*2)  ! Drift dists of home sector hits
      REAL    ZL(0:NBPSEN-1,MX_HIT_WIRE*2)  ! Wire Z locations of home sec hits
      REAL    DYL(0:NBPSEN-1,MX_HIT_WIRE*2) ! Drift dists of adjac sector hits
      REAL    DZL(0:NBPSEN-1,MX_HIT_WIRE*2) ! Wire Z locations of adjac sec hits
C
      LOGICAL ENOUGH                    ! TRUE if enough hits to form chain
C
      SAVE ICALL,INEFF
      DATA ICALL/0/
C----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        ICALL = 1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('PINEFF',INEFF,IER)
        CALL EZRSET
      END IF
C
C   Loop over the two possiblities, matching with the inner sector(-1)
C   and matching with the outer sector(+1).   The outer sector for sector
C   35 is sector 0 and the inner sector for sector 0 is sector 35.
C
      DO 10 DIR = -1,1,2
        IF (LFLOC.LE.0) GOTO 999
        LLINKH = LQ(LFLOC-1)
        IF (LLINKH.GT.0) CALL MZDROP(IXCOM,LLINKH,' ')
        LCHAI = LQ(LFLOC-2)
        IF (LCHAI.GT.0) CALL MZDROP(IXCOM,LCHAI,' ')
        LLINKD = LQ(LFLOC-5)
        IF (LLINKD.GT.0) CALL MZDROP(IXCOM,LLINKD,' ')
        SECTD1 = SECTOR + DIR
        IF (SECTD1.EQ.36) SECTD1 = 0
        IF (SECTD1.EQ.-1) SECTD1 = 35
C
C   Check to see if the sector near the current one has hits
C   for cross-sector segments.
C
        IF (SECTD1.LT.0 .OR. SECTD1.GT.MXSECP) THEN
          GOTO 10
        ELSE
          LBANK = GZFPSC(HALF,SECTD1)
          STAT1 = IQ(LBANK)
          IF (.NOT.BTEST(STAT1,ION)) GOTO 10
          NHITS1 = 0
          IF (LBANK.GT. 5) NHITS1 = IQ(LBANK+1)
          IF (NHITS1.LT.2) GOTO 10
        END IF
C
C   Check to see if at least one hit available in current sector to
C   start road.
C
        CALL GTFPSC(HALF,SECTOR,'WIR',0,NEL,NWORDS,CONT)
        CALL UCOPY(CONT,NHITS,NEL)
        NHITS1 = 0
        DO 5 II = 0,INEFF
          NHITS1 = NHITS1 + NHITS(II)
    5   CONTINUE
        IF (NHITS1 .LE. 0) GOTO 999      ! no hits to start road
C
C  Accumulate unused hits from beginning 'home' sector.
C
        CALL PHITAC(HALF,SECTOR,YL,ZL,LOC,HOHITS,ENOUGH)
C
C  Accumulate all unused hits from neighboring sector
C
        CALL GTFPSC(HALF,SECTD1,'WIR',0,NEL,NWORDS,CONT)
        CALL UCOPY(CONT,NHITS,NEL)
        NHITS1 = 0
        DO 205 II = NBPSEN-1,NBPSEN-1-INEFF,-1
          NHITS1 = NHITS1 + NHITS(II)
  205   CONTINUE
        IF (NHITS1 .LE. 0) GOTO 10      ! no hits to end road
C
C  Accumulate unused hits from 'minus 1' sector.
C
        CALL PHITAC(HALF,SECTD1,DYL,DZL,DLOC,D1HITS,ENOUGH)
C
C  Form segments across sector boundaries by choosing beginning wire and
C  ending wire in different, adjacent sectors and making two partial segments
C  come together at the inter-sector wall.
C
        JWIREH = 0
        JWIRED = 0
        WIRBEG = NBPSEN
        WIREND = -1
        DO 110 IWIRE = NBPSEN-1,0,-1
          IF (HOHITS(IWIRE) .GT. 0) THEN
            JWIREH = JWIREH + 1
            IF (IWIRE .LE. INEFF) WIRBEG = IWIRE
          END IF
  110   CONTINUE
        ITOT = 0
        DO 115 IWIRE = WIRBEG,NBPSEN-1
          IF (IWIRE-ITOT+1 .LE. INEFF) THEN
            IF (HOHITS(IWIRE) .GT. 0) THEN
              ITOT = ITOT + 1
              WIRBND = IWIRE
            END IF
          END IF
  115   CONTINUE
        DO 120 IWIRE = 0,NBPSEN-1
          IF (D1HITS(IWIRE) .GT. 0) THEN
            JWIRED = JWIRED + 1
            IF (IWIRE.GE.NBPSEN-1-INEFF) WIREND = IWIRE
          END IF
  120   CONTINUE
        ITOT = 0
        DO 125 IWIRE = WIREND,0,-1
          IF ((NBPSEN-(IWIRE+ITOT+1)) .LE. INEFF) THEN
            IF (D1HITS(IWIRE) .GT. 0) THEN
              ITOT = ITOT + 1
              WIREEG = IWIRE
            END IF
          END IF
  125   CONTINUE
        IF (WIREND-WIRBEG+1 .LE. NBPSEN-1-INEFF) GOTO 10
        IF (JWIREH+JWIRED .LT. NBPSEN-1-INEFF) GOTO 10
        IF (WIREEG-WIRBND .GT. INEFF) GOTO 10
        MXPASS = INEFF - (NBPSEN - (WIREND - WIRBEG + 1))
        IF (MXPASS .GT. INEFF) GOTO 10
        IF (MXPASS .LT. 1) GOTO 10
C        IF (MXPASS .GT. 4) MXPASS = 4
        IF (MXPASS .GT. 2) MXPASS = 2
C
C  Create links between hits starting with the beginning wire in home sector.
C
        IOFSET = 1*DIR
        CALL PXLINK(IOFSET,YL,ZL,HOHITS,WIRBEG,WIRBND,MXPASS,NLK,LK)
        IOFSET = ABS(IOFSET)
C
C  For each pass,
C    Form trees from the links storing branch info with each link.
C    Climb trees to form chains that will be used as partial segments.
C
        DO 200 IPASS = 1,MXPASS
          CALL PXTREE(IOFSET,IPASS,NLK,LK)
          NLINK = 0
          IF (LFLOC.GT.0) NLINK = IQ(LFLOC+IOFSET)
          DO 250 ILINK = 1,NLINK
            CALL PXCLIMB(IOFSET,ILINK,IPASS)
  250     CONTINUE
          LCHAI = LQ(LFLOC-2)
          IF (LCHAI.LE.0) GOTO 10
  200   CONTINUE
        LCHAI = LQ(LFLOC-2)
        IF (LCHAI.LE.0) GOTO 10
        BCHAIN = IQ(LCHAI+1)                ! Store beginning number of chains
C
C  Repeat procedure for adjacent sector.
C
        IOFSET = 5*DIR
        CALL PXLINK(IOFSET,DYL,DZL,D1HITS,WIREND,WIREEG,MXPASS,NLK,LK)
        IOFSET = ABS(IOFSET)
        DO 300 IPASS = 1,MXPASS
          CALL PXTREE(IOFSET,IPASS,NLK,LK)
          NLINK = 0
          IF (LFLOC.GT.0) NLINK = IQ(LFLOC+IOFSET)
          DO 350 ILINK = 1,NLINK
            CALL PXCLIMB(IOFSET,ILINK,IPASS)
  350     CONTINUE
          LCHAI = LQ(LFLOC-2)
          IF (LCHAI.LE.0) GOTO 10
  300   CONTINUE
C
C  Make chains using the trees available combining one beginning tree
C  and one ending tree.
C
        CALL PXCHAIN(HALF,SECTOR,SECTD1,BCHAIN,YL,ZL,LOC,DYL,DZL,DLOC)
C
   10 CONTINUE                          ! End loop over neigboring sectors
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
