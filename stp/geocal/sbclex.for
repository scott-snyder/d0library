      LOGICAL FUNCTION SBCLEX( IETA, IPHI, IDEPTH, IGROUP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CHECK TO SEE IF A PARTICULAR SUB-CELL EXISTS
C-
C-   Returned value  : SBCLEX     TRUE if sub-cell exists
C-   Inputs  :         IETA       physics ETA
C-                     IPHI       physics PHI
C-                     IDEPTH     physics LAYER
C-                     IGROUP     sub-cell index
C-   Outputs : 
C-   Controls: 
C-
C-   Created  14-FEB-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER IETA, IPHI, IDEPTH, IGROUP, META, ISGN, IETA0
C
      SBCLEX = .FALSE.

      IF (IDEPTH .GE. MNLYMG .AND. IDEPTH .LE. MXLYMG) THEN
        SBCLEX = .TRUE.                  ! allow ICD and MG
        GO TO 999
      END IF
C
      ISGN = SIGN(1., C(LQCLAY + ILDETA))        ! dirn of increasing
C                                        ! eta
      IETA0 = 10 * C(LQCLAY + ILETA0) + 1        ! nominal eta for 1st
C                                        ! pad
      IF( ISGN*(IETA-IETA0) .LT. 0) GO TO 999    ! sub-cell out of lower
C                                        ! range
      IF (IC(LQCLAY + ILETPH) .EQ. 1) THEN       ! one eta/phi zone
        META = IC(LQCLAY + ILNETA)     ! number of etas on plate
      ELSE IF (IC(LQCLAY + ILETPH) .EQ. 2) THEN  ! two eta/phi zones
        META = IC(LQCLAY + ILNETA) + IC(LQCLAY+ILNETA+NWZONE)
      ELSE IF (IC(LQCLAY + ILETPH) .EQ. 3) THEN  ! three eta/phi zones
        META = IC(LQCLAY+ILNETA) + IC(LQCLAY+ILNETA+NWZONE) +
     &    IC(LQCLAY+ILNETA+2*NWZONE)
      END IF
C
      IF ( ISGN*(IETA-IETA0) .GT. META-1 ) GO TO 999
C
      SBCLEX = .TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
