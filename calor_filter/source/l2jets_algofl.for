      FUNCTION L2JETS_ALGOFL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize L2JETS_ALGO common
C-
C-   Returned value  : .TRUE. if no error
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-MAY-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L2JETS_ALGOFL
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$INC:L2JETS_ALGO.INC'          ! L2JETS_ALGO common
      INCLUDE 'D0$INC:L2JETS_CONT.INC'          ! JETS control common
      INCLUDE 'D0$INC:L2JETS_CHARS.INC'         ! Character variables
C----------------------------------------------------------------------
      L2JETS_ALGOFL = .TRUE.            ! set to true initially
      IF (NOWEVT .EQ. IEVT_ALGO) RETURN ! already done

C---Update event number
      IEVT_ALGO = NOWEVT
  999 RETURN
      END
