       SUBROUTINE L2JETS(PARAM_SET_NUMBER,HARDWARE,RESULT_FLAG,
     &  FORCE_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TOOL L2JETS : FIND JETS IN LEVEL 2
C-
C-   Inputs  : PARAM_SET_NUMBER : # of parameter set to use
C-             HARDWARE: mask of set bits for LV1 trigger which started
C-                       this filter.
C-   Outputs : RESULT_FLAG : Flag set to TRUE when we want to pass tool
C-                           under this PARAM_SET_NUMBER
C-             FORCE_FLAG  : Set to TRUE when we want to pass filter
C-             regardless. Set to .FALSE. for right now.
C-   Controls:
C-
C-   Created  26-JAN-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'               ! zebra main store
      INCLUDE 'D0$INC:L2LINK.INC'               ! zebra link area
      INCLUDE 'D0$INC:L2JETS_CONT.INC'          ! Control variables
      INCLUDE 'D0$INC:L2JETS_CHARS.INC'         ! Character variables
      INCLUDE 'D0$INC:L2JETS_HOT.INC'           ! L2JETS hot tower list
      INCLUDE 'D0$INC:L2JETS_ALGO.INC'          ! Algorithm common
      INCLUDE 'D0$INC:L2JETS_PAR.INC'           ! L2JETS cut parameters
      INTEGER PARAM_SET_NUMBER,HARDWARE
      INTEGER GZJAUX
      LOGICAL FORCE_FLAG,RESULT_FLAG
      LOGICAL VETO_WARNING_GIVEN
      SAVE    VETO_WARNING_GIVEN
C
      LOGICAL L2JETS_HOTFL                      ! logical function
      LOGICAL L2JETS_FAKE_HOTFL                 ! "        "
      LOGICAL L2JETS_ALGOFL                     ! "        "
      LOGICAL L2JETS_CONTRS                     ! "        "
      LOGICAL L2J_MAKE_JAUX                     ! "        "
      LOGICAL OK,FIRST
      CHARACTER*4 OLD_PATH              ! Path
C
      INTEGER IOR,IAND,IBCLR,I,J
      LOGICAL BTEST
      INTEGER IEVT_DUMP
      SAVE    IEVT_DUMP
C&IF VAXVMS,VAXELN
C&ELSE
C&      EXTERNAL IOR,IAND,IBCLR,BTEST
C&ENDIF
      DATA FIRST /.TRUE./
      DATA VETO_WARNING_GIVEN /.FALSE./
C----------------------------------------------------------------------
C---Start timer if in VMS_FILTER
C      IF (.NOT. RUN_ELN) CALL TIMELOG('L2JETS ','START')
C---We may use some of the zebra structure. Everything should be done
C---underneath the FILT header. Make sure the path is defined for this
C---call.
      CALL PATHGT(OLD_PATH)
      CALL PATHST('FILT')
C
C---If this is our first call, initialize some event counters
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        IEVT_DUMP= -9000
        IEVT_ALGO= -9000
        IEVT_CONT= -9000
        IEVT_JAUX= -9000
      END IF
C
C---If our tool initialization has errored, we quit immediately.
C
      IF (.NOT. INIT_OK ) THEN
        ERR_FLAG = 'F'                  ! This affects the entire RUN!!!
        GOTO 900
      ELSE
        NOWERR = ERR_NONE               ! Reset error flags
        NOWSMESS = 'NO ERROR'
        NOWERRMESS = 'NO ERROR'
      END IF
C
C---Have new parameter sets been downloaded?
C
      IF ( NOW_NEW_PARAMS ) THEN
        NOW_NEW_PARAMS = .FALSE.
      ENDIF
C
C---Let's load some of what the FILTER has passed us into a common block
C---so that we can always refer to it.
      NOWPARAM = PARAM_SET_NUMBER       ! store current param # in
C                                       !L2JETS control common block
C
C#### Temporary fix for Level 2 passing PARAM_SET_NUMBER = 0
C
      IF (NOWPARAM .LE. 0 .OR. NOWPARAM .GT. NUM_PARAMS) THEN
        NOWERRMESS = 'Illegal passed parameter number'
        NOWSMESS   = 'Bad parameter #'
        NOWERR     = ERR_BAD_PARAM_VALUE
        ERR_FLAG   = 'F'                ! Run is in question
      ENDIF

      NOW_IND_PARAM = IND_PARAM( NOWPARAM ) ! Independent param set
C
C#### I am currently concerned about the integrity of the HARDWARE mask,
C#### especially as it controls how we save results. It is only legal
C#### for HARDWARE to have 1 bit set.   Anything else
C#### is incorrect.
C
      J = 0
      DO I = 0, 31
        IF ( BTEST( HARDWARE, I ) ) J = J + 1
      ENDDO
      IF ( J .NE. 1 ) THEN
        NOWERRMESS = 'Illegal HARDWARE mask passed'
        NOWSMESS   = 'Illegal Mask'
        NOWERR     = ERR_HARD_MASK
        ERR_FLAG   = 'E'              ! This is bad. But maybe not fatal
        GOTO 900
      ENDIF
C
      NOWL1BIT = IOR(HARDWARE , DO_ALL_CAND) ! hardware bit that caused us to
C---be called. Note if DO_ALL_CAND is set to -1, we will process all bits.

C---Whether this tool pass/fails the event under this particular cut
C---will be held in a L2JETS_CONT common by NOWRESULT.  So at the end
C---of this Tool we will set RESULT_FLAG = NOWRESULT. But for now we
C---set them both equal to .FALSE. This means that if for some reason
C---this routine does not run (e.g. no candidates match mask or an error
C---condition occurrs) then this routine exits with a .FALSE.
C
      RESULT_FLAG = .FALSE.
      NOWRESULT   = .FALSE.             ! Default result is FAIL
      FORCE_FLAG  = .FALSE.             ! Leave this false
C
C---We will store the current event number here. This is important
C---as the event number is what we use to decide when we have a new
C---event.
C ##### %%% This is temporary. We must still find out %%%%%% #######
C ##### %%% how the event number will be stored       %%%%%% #######
C 9-16-90 Jan Hoftun says use the lower half of the Level1 beam crossing
C number. This number will change quickly!!! So we will test for .NE.
C
      NOWEVT      = IQ( LHEAD + 7)
C
C---On every call we must redeclare our temporary link area. After this
C---we need only get a link once. After that ZEBRA keeps track.
C---( make this an all-structural link area)
C
      CALL MZLINT(IXCOM,'/L2LINK/',DUM,L2LINK(NLNK),DUM)
C
C---Fill the Hot tower common block, if this is a new event. In most
C---cases we get this information from TRGR. But take it from a common
C---block if we are told TRGR cant do it.
C---The following routines are also responsible for defining LTRGR for
C---the rest of the program.
C
      IF (NOWEVT .NE. IEVT_HOT)  THEN
        IF (TRGR_READY) THEN
          OK = L2JETS_HOTFL()
        ELSE
          OK = L2JETS_FAKE_HOTFL()
        END IF
        IF (.NOT. OK) GOTO 900
      END IF

C
C---If there are no candidates, we leave after dispatching an
C---informational error to Level 2.
C
      IF (NJTHOT .LE. 0) THEN
        NOWSMESS   = 'No candidates'
        NOWERRMESS = ' No Jet candidates present in TRGR '
        ERR_FLAG   = 'W'
        CALL ERRMSG(NOWSMESS,'L2JETS',NOWERRMESS,ERR_FLAG)
      END IF
C
C---If we reach this point, we are pretty sure we are going to store
C---some results. We must do a ONE time reset of some of the variables in
C---our JETS control common block.
C
      IF (NOWEVT .NE. IEVT_CONT) THEN
        OK = L2JETS_CONTRS()
        IF (.NOT. OK) GOTO 900
      END IF
C
C---We are now ready to apply different jet-finding algorithms. Some of
C---these algorithms may use variables that must be reset before a new
C---event. Do this here.
C
      IF (NOWEVT .NE. IEVT_ALGO) THEN
        CALL L2J_COPY_JPAR
        OK = L2JETS_ALGOFL()
        IF (.NOT. OK) GOTO 900
      END IF
C
C---We will store all our results for Jet finding for each candidate and
C---under each independent parameter set in a zebra bank called JAUX.
C---We must Book this bank and initialize it if we havent already.
C
      LJAUX = GZJAUX()
      IF (NOWEVT .NE. IEVT_JAUX) THEN
        OK = L2J_MAKE_JAUX()
        IF (.NOT. OK) GOTO 900
      END IF
C
C---At this point, we wonder if we have been called before under the same
C---trigger bit and under an identical set of parameters. If so, skip
C---further processing and go straight to post-reconstruction routines.
C
      IF (IAND(TRIED_BIT(NOW_IND_PARAM),NOWL1BIT) .EQ. NOWL1BIT) THEN
        GOTO 1000                       ! Already processed
      END IF

C---    ! ****** START JET FINDING *******

C
C---Find position and ET of Jet 'Core'
C
      IF ( USE_CL2) THEN
        CALL L2JETS_CEN_CL2             ! Fast CADX unpack used
      ELSE
        CALL L2JETS_CEN                 ! TRGR FADC values used
      END IF
C
C---Find total ET of Jet
C
C      CALL L2JETS_ENG                   ! TRGR FADC values used

C
C---Adjust ET of Jets with the appropriate calibration factor:
C
      CALL L2JETS_CALIB
C
C---Dump JAUX if requested.
C
      IF (.NOT. RUN_ELN .AND. DUMP_JAUX .AND. NOWEVT .NE. IEVT_DUMP)
     &  THEN
        IEVT_DUMP = NOWEVT
C        CALL L2JETS_DUMP
      END IF

C
C------------------------------------------------------------------------
C---At this point we have done all the desired reconstruction for each of
C---the jet candidates who caused the specified trigger bits to fire.
C
      TRIED_BIT(NOW_IND_PARAM) = IOR( TRIED_BIT(NOW_IND_PARAM),NOWL1BIT)
 1000 CONTINUE

C
C---It is possible that this trigger bit has been passed before under
C---this parameter set.
C
      IF (IAND(MAKE_CUTS_BIT(NOWPARAM),NOWL1BIT) .EQ. NOWL1BIT ) THEN
        IF (NOWL1BIT .EQ. -1)  THEN     ! All bits asked for
          NOWRESULT = (BIT_PASSED(NOWPARAM) .EQ. -1)
        ELSE
          NOWRESULT =(IAND(BIT_PASSED(NOWPARAM),NOWL1BIT) .EQ. NOWL1BIT)
        END IF
        GOTO 2000
      END IF
C
C---Check cuts to see if this event passes the jet tool under the current
C---selecion of parameters.
C
      CALL L2JETS_CUTS                 ! this routine sets NOWRESULT
 2000 RESULT_FLAG = NOWRESULT          ! set result flag for level 2
      MAKE_CUTS_BIT(NOWPARAM) = IOR(MAKE_CUTS_BIT(NOWPARAM),NOWL1BIT)
      IF (RESULT_FLAG) BIT_PASSED(NOWPARAM) = IOR(BIT_PASSED(
     &  NOWPARAM), NOWL1BIT)


C---Write results if we passed.
C
C      IF ( RESULT_FLAG) CALL L2JETSFL   


      GOTO 998
  900 CONTINUE    ! ****** RETURN ON ERROR *******
      IF ( NOWERR .NE. ERR_UTILITY)
     &   CALL ERRMSG(NOWSMESS,'L2JETS',NOWERRMESS,ERR_FLAG)
  998 CONTINUE    ! ****** NORMAL COMPLETION *****

C---Deactivate our zebra link area
      DUM(1) = 0                       ! Deactivate
C---Reset the path to what it was before we ran
      CALL PATHST(OLD_PATH)
C---Set result flag
      RESULT_FLAG = NOWRESULT
C---Check VETO flag
      IF ( .NOT. VETO_WARNING_GIVEN .AND. VETO_THIS(NOWPARAM) ) THEN
        VETO_WARNING_GIVEN = .TRUE.
        CALL ERRMSG('Veto used','L2JETS','A veto has been  used','W')
      ENDIF
C
      IF ( VETO_THIS(NOWPARAM) ) RESULT_FLAG = ( .NOT. RESULT_FLAG )
C
C      IF (.NOT. RUN_ELN) CALL TIMELOG('L2JETS ','STOP ')

  999 CONTINUE
      RETURN
      END
