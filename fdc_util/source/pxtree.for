      SUBROUTINE PXTREE(IOFSET,IPASS,NLK,LK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build a tree of links.
C-
C-   Inputs  : IOFSET = Link offset in USER bank structure.
C-             IPASS = Allowed gap size.
C-             NLK = Number of links for each wire for each pass.
C-             LK = Location of first link for each wire for each pass.
C-   Outputs : fill Links with valid next links, ie make branches
C-
C-   Created   6-AUG-1990   Jeffrey Bantly
C-   Updated  13-JUN-1991   Jeffrey Bantly  limit loop to links with gap
C-                                          < = current pass, saves time
C-   Updated  23-JUL-1991   Susan K. Blessing   Reorder IF statements
C-    after DO 300 line and change calculation of LOC2 to speed up.
C-   Updated  29-MAY-1992   Susan K. Blessing  Add NLK and LK to call
C-    so that number of times around DO 300 loop can be drastically
C-    reduced.
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
      INTEGER IOFSET                    ! Offset of LINK bank in USER
      INTEGER IPASS                     ! Current PASS or gapsize
C
      INTEGER LLINK                     ! LINK bank link
      INTEGER ILINK                     ! current link counter
      INTEGER NLINK                     ! Number of links in first LINK bank
      INTEGER LINK2                     ! current second link counter
      INTEGER IWRB1,IWRE1               ! begin,end wires of first link
      INTEGER IWRB2,IWRE2               ! begin,end wires of second link
      INTEGER IE1                       ! end hit first link
      INTEGER IGAP1,IGAP2               ! gaps of first and second link
      INTEGER LOCL                      ! LINK bank location of first link
      INTEGER LOC2                      ! LINK bank location of second link
      INTEGER IBRNCH                    ! current branch counter
      INTEGER ICALL                     ! initialization check
      INTEGER IER                       ! error check
      INTEGER NLK(0:NBPSEN-1,2),LK(0:NBPSEN-1,2)
      INTEGER LL,IP
C
      REAL    SL1,SL2                   ! slope of first and second link
      REAL    DSL                       ! difference in slopes
      REAL    DSLMAX(4)                 ! max allowed slope differences
C
      LOGICAL NEW                       ! marks a new branch
C
      SAVE ICALL,DSLMAX
      DATA ICALL/0/
C------------------------------------------------------------------------
C
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('DSLMAX',DSLMAX,IER)
        CALL EZRSET
        ICALL = 1
      END IF
C
C  Initialization of links and bank locations
C
      LLINK = LQ(LFLOC-IOFSET)
      NLINK = IQ(LFLOC+IOFSET)                    ! number of links
      IF (NLINK.LT.2) GO TO 999
C
C  Build elementary trees for all links. A tree has to satisfy the local cut:
C  ABS(SL1-SL2).LT.DSLMAX, where SL1,SL2 are slopes of the two links
C  (defined by Y = SL*X+Y0).
C
C  Loop over all links in first LINK bank.
C
      DO 200 ILINK = 1,NLINK
        LOCL = LLINK + (ILINK-1)*10
        IF (IPASS.GT.1) THEN
          IF (IQ(LOCL+6).LT.0) GO TO 201      ! skip used link
          IF (IQ(LOCL+6).GE.4) GO TO 201      ! skip filled link
        END IF
        SL1 = Q(LOCL+5)                   ! slope of link ILINK
        IE1 = IQ(LOCL+2)                  ! 2-nd, ending hit on link ILINK
        IWRB1 = IQ(LOCL+3)                ! beginning wire
        IWRE1 = IQ(LOCL+4)                ! ending wire
        IGAP1 = IWRE1-IWRB1               ! gap size
        IF(ABS(IGAP1).GT.IPASS) GOTO 101  ! end loop if gap beyond cur PASS
C
C  Find all potential branches for link ILINK.
C  Loop over all other links and look for a link with similar slope.
C
        LLINK = LQ(LFLOC-IOFSET)
C
        DO IP = 1, IPASS
          LOC2 = LLINK + 10 * (LK(IWRE1,IP)-1) - 10
          LINK2 = LK(IWRE1,IP) - 1
C
          DO 300 LL = 1, NLK(IWRE1,IP)
            LOC2 = LOC2 + 10
            LINK2 = LINK2 + 1
C
            IF(IQ(LOC2+1) .NE. IE1) GOTO 300       ! hits don't match
            IF(IQ(LOC2+6) .LT. 0) GOTO 300         ! link used
C
            IWRB2 = IQ(LOC2+3)              ! beginning wire
            IWRE2 = IQ(LOC2+4)              ! ending wire
            IGAP2 = IWRE2-IWRB2             ! gap size
C
            IF (ABS(IGAP1).EQ.IPASS.OR.ABS(IGAP2).EQ.IPASS) THEN
              SL2 = Q(LOC2+5)               ! slope of second link
C
C  Check if slopes agree. If they do, the links make an elementary tree.
C
              DSL = ABS(SL1-SL2)                  ! difference in slopes
              IF (DSL.LT.DSLMAX(IPASS)) THEN    ! cut to minimize diff in slopes
                IF(IQ(LOCL+6).GE.4) THEN
                  GOTO 201                      ! no more branch slots avail.
                END IF
C
                NEW = .TRUE.
                DO IBRNCH = 1,IQ(LOCL+6)    ! make sure it's a new branch
                  IF( IQ(LOCL+6+IBRNCH).EQ.LINK2) NEW = .FALSE.
                END DO
                IF(NEW) THEN              ! fill new branch info into cur link
                  IQ(LOCL+6) = IQ(LOCL+6)+1
                  IQ(LOCL+6+IQ(LOCL+6)) = LINK2
                END IF
              END IF
            END IF
C
  300     CONTINUE                        ! End loop over LINK2
        END DO
  201   CONTINUE
C
  200 CONTINUE                          ! End loop over ILINK
  101 CONTINUE
C-----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
