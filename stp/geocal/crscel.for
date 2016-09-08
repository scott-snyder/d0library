      SUBROUTINE CRSCEL(IETA, IPHI, IDEPTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To create and fill a CLYR bank corresponding
C-                 to a single coarse representation of a cell for the
C-                 end design where there are several sub cells.
C-
C-   Inputs  :     IETA      physics variable ETA
C-                 IPHI      physics variable PHI
C-                 IDEPTH    physics variable LAYER
C-   Outputs : 
C-   Controls: 
C-   Zebra Banks Created:    CLYR
C-
C-   Created   3-APR-1989   Stephen Kahn
C-   Revised   14-OCT-1989  Stephen Kahn  -- separated into subroutines
C-                            CRSCEL_EH, and CRSCEL_OH
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:LCLYR.INC'
C
      INTEGER NGROUP, JBYT, NSC, LERR, I
      INTEGER IETA, IPHI, IDEPTH, LUNIT, MCLYR(5), IOCLYR
      LOGICAL FIRST
      SAVE FIRST
C
      CHARACTER*4 CHAR4
      INTEGER ICHARR,ICHARS
      CHARACTER*4 CHARR,CHARS
      EQUIVALENCE (ICHARR,CHARR)
      EQUIVALENCE (ICHARS,CHARS)
      EQUIVALENCE (CHAR4,MCLYR(1))
      DATA LUNIT / 0 /, LERR / 6 /
      DATA FIRST /.TRUE./
      DATA MCLYR / 4HCLYR, 2, 1, 16, 9 /
      DATA CHAR4 / 'CLYR'/
C
      IF (FIRST) THEN
        CALL MZFORM('CLYR','1B6F1H1I-F',IOCLYR)
        FIRST = .FALSE.
      END IF
C
      NGROUP = JBYT( C(JQCLYR), JBNSBC, NBNSBC)
      CHARR= 'TRAP'
      IF(IC(JQCLYR+ICSHAP) .EQ. ICHARR) RETURN   ! don't process CC
      IF( IDEPTH .GE. MNLYMG .AND. IDEPTH .LE. MXLYMG) RETURN   ! don't
C                                       ! process MG or ICD
C
      MCLYR(5) = IOCLYR
      LSCLYR = LQCLYR
      CALL MZLIFT( IDVSTP, LQCLYR, LQCLYR, 0, MCLYR, 0)
      CALL SBYT(NGROUP, C(LQCLYR), JBNSBC, NBNSBC)
      IC(LQCLYR + ICELID) = IC(JQCLYR + ICELID)  ! Cell ID word
      CALL SBYT(IPHI, C(LQCLYR+ICELID), JBIPHI, NBIPHI)
      CALL UCOPY( C(JQCLYR + ICTHTE), C(LQCLYR + ICTHTE), 3)
C
      CHARR='TUBS'
      CHARS='PCON'
      IF( IC(JQCLYR + ICSHAP) .EQ. ICHARR) THEN  !  EE, MH, IH
        CALL CRSCEL_EH( IETA, IPHI, IDEPTH)
      ELSE IF (IC(JQCLYR + ICSHAP) .EQ. ICHARS) THEN      ! OH
        CALL CRSCEL_OH( IETA, IPHI, IDEPTH)
      END IF
C
C----------------------------------------------------------------------
  999 RETURN
      END
