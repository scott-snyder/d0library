      SUBROUTINE PXCLIMB(IOFSET,IROOT,IPASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Climb chains away from the interaction point 
C-                         starting from link IROOT. Chains are saved by 
C-                         the routine SAVXPCH
C-
C-   Inputs  : IOFSET= Link bank to be used offset
C-             IROOT = First link of tree
C-             IPASS = Maximum difference in wires for a link
C-
C-   Created   6-AUG-1990   Jeffrey Bantly
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS files 
C-   Updated  13-JUN-1991   Jeffrey Bantly  fetch links once, store only
C-                                          longest chains,reset MAXWIR 
C-                                          for new sectors only, store
C-                                          chains once.
C-   Updated  12-JUL-1991   Susan K. Blessing  Replace VZERO calls with 
C-    (slightly faster) DO loop.
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER IOFSET                    ! Offset in USER of LINK bank
      INTEGER IROOT                     ! first link to be used
      INTEGER IPASS                     ! current max wire gap allowed in link
C
      INTEGER I
      INTEGER LLINK                     ! LINK bank link
      INTEGER LOCLNK                    ! location in LINK of current link
      INTEGER ILINK                     ! current link
      INTEGER JLINK(NBPSEN-1)           ! array of links in current branch
      INTEGER IBRNCH                    ! current branch
      INTEGER NBRNCH(NBPSEN-1)          ! array of cur branch in JLINK links
      INTEGER MAXDEP                    ! depth of longest branch so far
      INTEGER TMAXDEP                   ! depth of current pass longest branch
      INTEGER IDEPTH                    ! current depth in current branch
      INTEGER MAXWIR                    ! max number of wires in cur branch
      INTEGER MXLINK(NBPSEN-1)          ! array of links of longest branch
      INTEGER INEFF                     ! (RCP) max allowed missing hits
      INTEGER MINDEP                    ! minimum depth of branch allowed
      INTEGER IDEP                      ! brach depth loop var
      INTEGER ICALL                     ! initialization check
      INTEGER IER                       ! error check
C
      LOGICAL NEW_BRANCH                ! set TRUE for new branch
C
      SAVE ICALL,INEFF,MINDEP
      DATA ICALL/0/
C--------------------------------------------------------------------
C
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('PINEFF',INEFF,IER)
        CALL EZRSET
        MINDEP=2
        MAXDEP=0
        TMAXDEP=0
        ICALL=1
      END IF
C
C  Initialize variables for this pass.
C
      IDEPTH=0
      ILINK=IROOT
      TMAXDEP=0
      IF(MAXDEP.GT.2) TMAXDEP = MAXDEP - 2
      IF(MAXDEP.LT.0 .OR. IROOT.EQ.1) MAXDEP = 0
      IF(MAXDEP.LT.0 .OR. IROOT.EQ.1) TMAXDEP = 0
      IF(IPASS.EQ.1) MAXWIR = -1
      IBRNCH = 0
C
      DO I = 1, NBPSEN-1
        NBRNCH(I) = 0
        JLINK(I) = 0
        MXLINK(I) = 0
      END DO
C
      LLINK=LQ(LFLOC-IOFSET)
      IF(LLINK.LE.0) GOTO 999
C
C  Start of tree climbing.
C
   10 CONTINUE                          ! climb up ILINK
C
      IDEPTH=IDEPTH+1
      NBRNCH(IDEPTH)=0
      JLINK(IDEPTH)=ILINK
      LOCLNK=LLINK+(ILINK-1)*10
C
   20 CONTINUE                          ! climb up next branch
C
      NBRNCH(IDEPTH)=NBRNCH(IDEPTH)+1   ! increment cur branch at cur depth
      IBRNCH=NBRNCH(IDEPTH)                     ! set cur branch
      IF (IBRNCH.GT.IQ(LOCLNK+6)) GO TO 30      ! cur branch > link max branch
      IF (IDEPTH.GE.NBPSEN-1) GOTO 30           ! regular full segment
      ILINK=IQ(LOCLNK+6+IBRNCH)         ! set cur link to end link cur branch
      IF(ILINK.LE.0) GOTO 30            ! if no link, at end of cur branch
      GO TO 10                          ! climb up new link
C
   30 CONTINUE     ! climbing stopped at the top  of a chain - save it
C
      NEW_BRANCH=.FALSE.
      IF (IDEPTH.GE.MINDEP) THEN
        IF (IDEPTH.GT.TMAXDEP) THEN     ! cur branch depth > max best depth
          NEW_BRANCH=.TRUE.
        ELSEIF (IDEPTH.EQ.TMAXDEP) THEN   ! cur branch depth = max depth 
          IF((IOFSET.EQ.1 .AND. IQ(LOCLNK+4).GT.MAXWIR) .OR.   ! more wires
     &       (IOFSET.GT.1 .AND. IQ(LOCLNK+4).LT.MAXWIR)) THEN  ! more wires
            NEW_BRANCH=.TRUE.
          ENDIF
        ENDIF
      ENDIF
      IF (NEW_BRANCH) THEN              ! store cur branch if it is longest
        TMAXDEP=IDEPTH
        MAXWIR=IQ(LOCLNK+4)
        IF(IOFSET.GT.1) MAXWIR=IQ(LOCLNK+3)
        DO 40 IDEP=1,TMAXDEP
          MXLINK(IDEP)=JLINK(IDEP)
   40   CONTINUE
      ENDIF
C
C  Climb down tree one link and try to go up the next branch.
C
   50 CONTINUE
C
      IDEPTH=IDEPTH-1                   ! climb down ILINK
      IF (IDEPTH.EQ.0) GO TO 100        ! no more possible branches
      ILINK=JLINK(IDEPTH)               ! reset ilink
      IF (NBRNCH(IDEPTH).LT.IQ(LOCLNK+6)) GO TO 20      ! more branches
      GO TO 50                          ! no more branches at this depth
C
C  Save chain if it is currently the longest.
C
  100 CONTINUE
C
      IF(TMAXDEP.GE.2 .AND. TMAXDEP.GE.MAXDEP) THEN
        MAXDEP=TMAXDEP
        CALL SAVXPCH(IOFSET,MAXDEP,MXLINK,IPASS)
      ENDIF
C
C------------------------------------------------------------------------
  999 RETURN
      END
