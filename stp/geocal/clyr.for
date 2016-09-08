      SUBROUTINE CLYR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To Create and Fill 'CLYR' banks.  These
C-           describe the shape, position, and orientation of a 
C-           Calorimeter Cell.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-   Zebra Banks Lifted:    CLYR
C-
C-   Created   5-DEC-1988   Stephen Kahn, Esq.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$PARAMS:CEDP.PARAMS'
      INCLUDE 'D0$PARAMS:CETA.PARAMS'
      INCLUDE 'D0$LINKS:IZCETA.LINK'
      INCLUDE 'D0$LINKS:IZCLAY.LINK'
      INCLUDE 'D0$LINKS:IZCLYR.LINK'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
      INCLUDE 'D0$LINKS:IZCLGA.LINK'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:LCLYR.INC'
C
      INTEGER  JBYT, JCLGA
      INTEGER  JDEPTH, IPHI, LCLAY, NGROUP, IGROUP, MCLYR(5), IDEPTH
      INTEGER  NDEPTH, JCLAY, LZFIND, MPHI, IETA, NETA, IOCLYR
      INTEGER  LUNIT, KETA
      LOGICAL SBCLEX, FIRST_PASS
C
      DATA LUNIT / 0 /, KETA / 0 /
      CHARACTER*4 CHAR4
      EQUIVALENCE (CHAR4,MCLYR(1))
      DATA MCLYR / 4HCLYR, 3, 1, 24, 9 /
      DATA CHAR4 / 'CLYR'/
C
      CALL MZLINT(IXSTP,'/LCLYR/', LCLYR, JQCLYR, KSCLYR)   ! temporary link
      CALL MZFORM('CLYR','1B6F1H1I-F',IOCLYR)        ! form definition
      MCLYR(5) = IOCLYR
      NETA = IC(LQCEDP + IGNETA)       ! number of eta's
      NDEPTH = IC(LQCEDP + IGNDEP)     ! total number of depths
C
      DO 400 IETA = 1, NETA
      LQCETA = LC(LQCEDP - IETA - IZCETA + 1)        ! obtain link
      MPHI = IC(LQCETA + IGMPHI)       ! number phi's required
C
      DO 300 IDEPTH = 1, NDEPTH
      JCLAY = JDEPTH(IDEPTH)           ! layer ID code
      IF ( JCLAY .EQ. 0 ) GO TO 300
        JREGN = (JCLAY/1000) * 1000    ! region code
        LQCREG = LZFIND(IDVSTP, LC(LCGEH-IZCREG), JREGN, IGREGN)
        IF( LQCREG .EQ. 0) THEN
           WRITE (6, *) 'CLYR: BAD REGION IDENT', JREGN
           STOP 911
        END IF
      JCLGA = (JCLAY/100) * 100 + 100  ! module code
      IF( JCLGA .EQ. 6000) JCLGA = ICOCHA        ! special case
      IF( MOD(JCLAY,100) .EQ. 85) JCLGA = JCLAY  ! MG special case
      IF( ABS(JREGN) .EQ. ISICD) JCLGA = JCLAY   ! ICD special case
      LQCLGA = LZFIND(IDVSTP, LC(LQCREG - IZCLGA), JCLGA, IGIDEN)
      IF(LQCLGA .EQ. 0) THEN
        WRITE (6,*) 'CLYR: BAD MODULE IDENT ',JCLGA
        STOP 912
      END IF
C
C ... GET 1st LAYER BANK TO FIND HOW MANY GROUPS FOR THIS DEPTH
      LQCLAY = LZFIND(IDVSTP, LC(LQCLGA-IZCLAY), JCLAY, ILIDEN)
      IF( LQCLAY .EQ. 0) GO TO 300
      NGROUP = JBYT(C(LQCLAY), JBNGRP, NBNGRP)   ! total # of groups
      IF( NGROUP .EQ. 0) NGROUP = 1              ! patch to protect
C                                        ! against CC not being set
C
C ... LIFT 'CLYR' BANK
      CALL MZLIFT(IDVSTP, LQCLYR, LQCETA, -IZCLYR-IDEPTH+1, MCLYR, 0)
      JQCLYR = LQCLYR
      FIRST_PASS = .TRUE.
C
C ... CYCLE THROUGH PHI'S
      DO 200 IPHI = 1, MPHI            ! 1 for EC, 4 for CC
C
C ... CYCLE THROUGH GROUPS
      DO 100 IGROUP = 1, NGROUP
      LCLAY = JCLAY + (IGROUP - 1) * ICLINC      ! ID code for group
      LQCLAY = LZFIND(IDVSTP, LC(LQCLGA-IZCLAY), LCLAY, ILIDEN)
C
C ... CHECK TO SEE IF THIS PARTICULAR SUB-CELL EXISTS
      IF( .NOT. SBCLEX( IETA, IPHI, IDEPTH, IGROUP)) GO TO 100
C
      IF (.NOT. FIRST_PASS) 
     +        CALL MZLIFT(IDVSTP,LQCLYR,LQCLYR,0,MCLYR,0)
      FIRST_PASS = .FALSE.
      CALL SBYT(IGROUP, C(LQCLYR), JBSBCL, NBSBCL)
      CALL SBYT(NGROUP, C(LQCLYR), JBNSBC, NBNSBC)
      CALL SBYT(IPHI, C(LQCLYR+ICELID), JBIPHI, NBIPHI)
      CALL SBYT(IDEPTH, C(LQCLYR+ICELID), JBIDEP, NBIDEP)
      CALL SBYT(IETA, C(LQCLYR+ICELID), JBIETA, NBIETA)
      LC(LQCLYR - IXCLAY) = LQCLAY
C
C ... SPECIAL HANDLING FOR ICD
      CALL CELICD(IETA, IPHI, IDEPTH, IGROUP)
      IF( ABS(JREGN) .EQ. ISICD) GO TO 100
C
C ... POSITION AND ORIENTATION OF SUB CELL
      CALL CELPOS(IETA, IPHI, IDEPTH, IGROUP)
C
C ... SHAPE OF SUB CELL
      CALL CELSHA(IETA, IPHI, IDEPTH, IGROUP)
      CALL MZPUSH(IXSTP, LQCLYR, 0, ICNPAR + IC(LQCLYR+ICNPAR) -
     &  MCLYR(4), ' ')       ! correct for variable length shape banks
C
      IF(LUNIT .NE. 0 .AND. IETA .EQ. KETA) CALL PRCLYR(LUNIT, LQCLYR)
  100 CONTINUE
      CALL CRSCEL( IETA, IPHI, IDEPTH) ! make single course description
C                                      ! of cell when more than 1 subcell
      CALL GANG_CELLS( IETA, IPHI, IDEPTH)  ! gang together cells that
C                                      ! are suppposed to be
      IF(LUNIT .NE. 0 .AND. IETA .EQ. KETA) CALL PRCLYR(LUNIT, LQCLYR)
  200 CONTINUE
  300 CONTINUE
  400 CONTINUE
      LCLYR(1) = 0
C
C----------------------------------------------------------------------
  999 RETURN
      END
