      LOGICAL FUNCTION run_select()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Applies run range and bad run cuts to current
C-                         run (based on current event in /ZEBCOM/).
C-                         Returns .TRUE./.FALSE. for Good/Bad.
C-                         Always return .TRUE. for Monte Carlo.
C-                         Adapted from TOP_LEPTONS_GOOD_RUN.
C-
C-   Inputs  :  none
C-   Outputs :  none
C-   Controls:  RUN_SELECT_RCP, BAD_RUN_RCP
C-
C-   Entry points:  RUN_SELECT_INI - Initialization.
C-
C-   Created  27-Dec-1993   Herbert Greenlee
C-   Updated  Nov-02-1994   Bob Kehoe  -- allow multiple run ranges, allow more
C-                                        bad runs
C-   Updated  Apr-02-1996   Bob Kehoe  -- add clean_run_mask, keep backward
C-                                        compatible
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL run_select_ini, run_select_end
      INCLUDE 'D0$INC:ZEBCOM.INC'
c-
C- Variables from RUN_SELECT_RCP.
C-
      LOGICAL do_run_select,do_range_select(5)
      INTEGER first_runs(5),last_runs(5)
C-
C- Bad run list from BAD_RUN_RCP
C-
      INTEGER max_bad_runs, no_bad_runs,max_array
      PARAMETER (max_bad_runs = 1000)
      PARAMETER (max_array = 2*max_bad_runs)
      INTEGER bad_run_nos(max_bad_runs),bad_list(max_array)
      INTEGER narray,nruns,clean_run_mask
      LOGICAL found_bad,new_bad_run_list
C-
C- Statistics
C-
      INTEGER num_run_input, num_run_kept, num_run_rejected
      INTEGER num_event_input, num_event_output
C-
C- Other variables and functions.
C-
      INTEGER lun, ssunit
      INTEGER run, prev_run, runno
      LOGICAL prev_run_select, print_runs
      INTEGER record_type,ier_nonfatal1,ier_nonfatal2
      INTEGER i,ier,k,nflag,nfrst,nlast,num_nosel
      LOGICAL mcdata,first
      DATA first/.true./

C----------------------------------------------------------------------
      run_select = .false.
      found_bad = .false.
      num_event_input = num_event_input + 1
      run = runno()
C-
C- Need to test this run?
C-
      IF(run.EQ.prev_run)THEN
        run_select = prev_run_select
        GOTO 998
      ENDIF
      num_run_input = num_run_input + 1
C-
C- Is this Monte Carlo?  Return .TRUE. if yes.
C-
      record_type = iq(lhead+1)
      mcdata = record_type.GE.1005
      IF(mcdata)THEN
        run_select = .true.
        IF(print_runs)THEN
          PRINT 10,run
   10     FORMAT(' RUN_SELECT: Processing Monte Carlo run',i6)
        ENDIF
        go to 997
      ENDIF
C-
C- Data.  Test to see if runs lie in requested ranges
C-    if none of run ranges enabled, then accept event and allow search thru
C-    bad_run_rcp if do_run_select set.
C-
      IF (num_nosel.LT.nflag) THEN
        DO k = 1,nflag
          IF (do_range_select(k)) THEN
            IF (run.GE.first_runs(k).AND.run.LE.last_runs(k)) run_select
     &          = .true.
          ENDIF
        ENDDO
      ELSE
        run_select = .true.
      ENDIF
C-
C- if run is in one of desired ranges, Test against known Bad Run List.
C-
      IF(do_run_select.AND.run_select)THEN
        DO i = 1,no_bad_runs
          IF (run.EQ.bad_run_nos(i)) THEN
            run_select = .false.
            found_bad = .true.
          ENDIF
        ENDDO
      ENDIF
      IF(print_runs)THEN
        IF(run_select)THEN
          PRINT 20,run
   20     FORMAT(' RUN_SELECT: Processing run',i6)
        ELSEIF (found_bad.AND.do_run_select) THEN
          PRINT 21,run
   21     FORMAT(' RUN_SELECT: Skipping bad run',i6)
        ELSE
          PRINT 30,run
   30     FORMAT(' RUN_SELECT: Skipping out-of-range run',i6)
        ENDIF
      ENDIF
C-
C- Run statistics
C-
  997 CONTINUE
      prev_run = run
      prev_run_select = run_select
      IF(run_select)THEN
        num_run_kept = num_run_kept + 1
      ELSE
        num_run_rejected = num_run_rejected + 1
      ENDIF
C-
C- Event statistics
C-
  998 CONTINUE
      IF(run_select)num_event_output = num_event_output + 1
      go to 999

      ENTRY run_select_ini()
C-
C- Initialization entry point
C-
      run_select_ini = .true.
      IF(first) THEN
        prev_run = run - 1                ! To insure run number mismatch
        prev_run_select = .false.
C-
C- Zero statistics
C-
        num_run_input = 0
        num_run_kept = 0
        num_run_rejected = 0
        num_event_input = 0
        num_event_output = 0
C-
C- Read RCP parameters.  First read from RUN_SELECT_RCP.
C-
        CALL ezpick_nomsg('RUN_SELECT_RCP', ier)
        IF(ier.NE.0)THEN
          CALL inrcp('RUN_SELECT_RCP', ier)
          CALL ezpick_nomsg('RUN_SELECT_RCP', ier)
        ENDIF
        IF (ier.EQ.0) CALL ezgeta_i('DO_RUN_RANGE_SELECT',0,0,0,nflag
     &       ,ier)
        IF (ier.EQ.0) CALL ezgeta_l('DO_RUN_RANGE_SELECT',1,nflag,1,
     &    do_range_select,ier)
        IF (ier.EQ.0) CALL ezgeta_i('FIRST_RUN',0,0,0,nfrst,ier)
        IF (ier.EQ.0) CALL ezgeta_iarr('FIRST_RUN',1,nfrst,1,
     &    first_runs,ier)
        IF (ier.EQ.0) CALL ezgeta_i('LAST_RUN',0,0,0,nlast,ier)
        IF (ier.EQ.0) CALL ezgeta_iarr('LAST_RUN',1,nlast,1,
     &    last_runs,ier)
        IF (ier.NE.0.OR.nflag.NE.nfrst.OR.nflag.NE.nlast)
     &    CALL errmsg('error in run_select_rcp','run_select',
     &    'range number mismatch','F')
        num_nosel = 0
        DO k = 1,nflag
          IF (.NOT.do_range_select(k)) num_nosel = num_nosel + 1
        ENDDO
        IF(ier.EQ.0) CALL ezget_l('DO_GOOD_RUN_SELECT',do_run_select
     &       ,ier)
        IF(ier.EQ.0) CALL ezget_l('PRINT_RUNS',print_runs,ier)
        IF(ier.EQ.0) CALL ezget_i('CLEAN_RUN_MASK',clean_run_mask,
     &        ier_nonfatal1)
        CALL ezrset
        IF (ier.NE.0) CALL errmsg('Error in RUN_SELECT_RCP',
     &    'RUN_SELECT',' ','F')
c-
C- Now Get Bad Run List
C-
        IF(do_run_select) THEN
          CALL ezpick_nomsg('BAD_RUN_RCP', ier)
          IF(ier.NE.0)THEN
            CALL inrcp('BAD_RUN_RCP', ier)
            CALL ezpick_nomsg('BAD_RUN_RCP', ier)
          ENDIF
          IF (ier.NE.0) CALL errmsg('Error in BAD_RUN_RCP',
     &      'RUN_SELECT',' ','F')
          IF (ier.EQ.0) CALL ezget_l('NEW_BAD_RUN_LIST',new_bad_run_list
     &         ,ier_nonfatal2)
          IF (ier_nonfatal1.EQ.0.AND.ier_nonfatal2.NE.0) ier = -1
          IF (ier_nonfatal1.NE.0.AND.ier_nonfatal2.EQ.0) ier = -1
          IF (ier.EQ.-1) call errmsg('BAD_RUN with wrong RUN_SELECT',
     &      'RUN_SELECT','mismatched RCPs','F')
          IF (ier.EQ.0) CALL ezgeta_i('BAD_RUN_NOS',0,0,0,narray,ier)
          IF (ier.EQ.0) THEN
            IF (ier_nonfatal1.NE.0.AND.ier_nonfatal2.NE.0) THEN ! *** old mode
              CALL errmsg('OLD MODE-take all runs','RUN_SELECT',
     &            'get new RCPs','W')
              IF (narray.LE.max_bad_runs) THEN
                CALL ezget_iarr('BAD_RUN_NOS',bad_run_nos,ier)
                no_bad_runs = narray
              ELSE
                IF(ier.EQ.0)CALL errmsg('Too Many Bad Runs To Store',
     &              'RUN_SELECT',' ','F')
              ENDIF
            ELSEIF (new_bad_run_list) THEN                ! *** new RCP files
              IF (narray.LE.max_array) THEN
                CALL errmsg('NEW MODE-select on mask','RUN_SELECT',' ',
     &                'W')
                CALL ezget('BAD_RUN_NOS',bad_list,ier)
                nruns = narray/2
                no_bad_runs = 0
                DO k = 1,nruns
                  IF (iand(bad_list(2*k),clean_run_mask).ne.0) then
                    no_bad_runs = no_bad_runs + 1
                    bad_run_nos(no_bad_runs) = bad_list(2*k-1)
                  ENDIF
                ENDDO
              ELSE
                IF(ier.EQ.0)CALL errmsg('Too Many Bad Runs To Store',
     &              'RUN_SELECT',' ','F')
              ENDIF
            ENDIF
          ENDIF
          CALL ezrset
          IF (ier.NE.0) CALL errmsg('Error in BAD_RUN_RCP',
     &      'RUN_SELECT',' ','F')
        ENDIF
        first=.false.
      ENDIF
      go to 999

      ENTRY run_select_end()
C-
C- Job summary entry point
C-
      lun = ssunit()
      PRINT 500, num_run_input, num_run_kept, num_run_rejected,
     &  num_event_input, num_event_output
      WRITE(lun,500)num_run_input, num_run_kept, num_run_rejected,
     &  num_event_input, num_event_output
  500 FORMAT(/' RUN_SELECT package statistics'/
     &  /1x,i8,' Runs processed'
     &  /1x,i8,' Runs selected'
     &  /1x,i8,' Runs rejected'/
     &  /1x,i8,' Events processed'
     &  /1x,i8,' Events selected'/)
  999 RETURN
      END
