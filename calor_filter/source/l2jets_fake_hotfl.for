      FUNCTION L2JETS_FAKE_HOTFL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : In the event TRGR cannot give us hot tower
C-                         information. The control flag TRGR_READY
C-                         can be set .FALSE. and this routine will
C-                         fill the hot tower common blocks instead.
C-                          *NOTE* - Must still set LTRGR link if possible
C-                          as L2JETS_HOTFL usually does it.
C-   Returned value  : .TRUE. is no error.
C-   Inputs  :
C-   Outputs :
C-   Controls: TRGR_READY in JETS.PARAMS file.
C-
C-   Created   5-AUG-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L2JETS_FAKE_HOTFL
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'               ! zebra common
      INCLUDE 'D0$INC:L2LINK.INC'               ! zebra link area
      INCLUDE 'D0$INC:L2JETS_CONT.INC'          ! L2jets control common
      INCLUDE 'D0$INC:L2JETS_CHARS.INC'         ! Character variables
      INCLUDE 'D0$INC:L2JETS_HOT.INC'           ! Real hot tower common
      INCLUDE 'D0$INC:L2JETS_FAKE_HOT.INC'      ! Fake hot tower values
      INTEGER I, GZFIND_CRATE, GZTRGR
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      LTRGR = GZFIND_CRATE( 'TRGR', GZTRGR(), 11) ! Get link
      LTRGR = LTRGR - 1
      NOWERRID    = 'L2JETS_FAKE_HOTFL' ! Name of this routine
      L2JETS_FAKE_HOTFL = .TRUE.        ! Initialize as true

C---We only have to run this routine if we have not done it before.
      IF (IEVT_HOT .EQ. NOWEVT ) GOTO 900

C---Fill with FAKE hot towers that we get from
      NJTHOT = NJTHOT_FAKE
      JT_COMP= JT_COMP_FAKE
      DO I = 1,NJTHOT
        IHOT_JT(2*I-1) = IHOT_MSK_JT_FAKE(I)
        IHOT_JT(2*I) = IHOT_ADR_JT_FAKE(I)
      END DO

C---Flag event number so we dont do this again this event.
      IEVT_HOT = NOWEVT                 ! Update event number

      GOTO 900
C  800 CONTINUE    !   *** ERROR CONDITION ***
C      L2JETS_FAKE_HOTFL = .FALSE.            ! error
  900 CONTINUE    !   *** NORMAL COMPLETION ****
  999 RETURN
      END
