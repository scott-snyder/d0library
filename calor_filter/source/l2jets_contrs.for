      FUNCTION L2JETS_CONTRS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reset some variables in L2JETS_CONT that keep
C-                         track of JETS results throughout this event.
C-
C-   Returned value  : .TRUE. if worked okay
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   1-JUN-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L2JETS_CONTRS
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$INC:L2JETS_CONT.INC'  ! JETS control variables
      INCLUDE 'D0$INC:L2JETS_CHARS.INC'         ! Character variables
      INCLUDE 'D0$INC:L2JETS_HOT.INC'   ! Candidate towers
      INTEGER I
      LOGICAL RET_COND
      SAVE RET_COND
      DATA RET_COND /.FALSE./
C----------------------------------------------------------------------
      L2JETS_CONTRS = RET_COND
      IF ( NOWEVT .EQ. IEVT_CONT) RETURN   ! Shouldn't be called!
      L2JETS_CONTRS = .FALSE.
      IEVT_CONT = NOWEVT                ! See that it isnt called again
C                                       ! during this event
      CALL VZERO(BIT_PASSED,NPAR_MAX)   ! Set bits tried to 0
      CALL VZERO(TRIED_BIT,NPAR_IND_MAX) ! Set bits passed to 0
      CALL VZERO(MAKE_CUTS_BIT,NPAR_MAX) ! Set bits cut to 0
      CALL VZERO(JET_MASK, NLIST_MAX )  ! Trigger Mask of each jet candidate
      L2JETS_CONTRS = .TRUE.            ! Reseting went okay
  999 RET_COND = L2JETS_CONTRS
      RETURN
      END
