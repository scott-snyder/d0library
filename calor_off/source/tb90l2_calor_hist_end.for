      FUNCTION TB90L2_CALOR_HIST_END
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Writes out histos of TB90L2_CALOR_HIST
C-   Called form EXM_END hook
C-
C-   Returned value  : TRUE
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  20-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL tb90l2_calor_hist_end
      INTEGER l,irun,runnum,runno
      CHARACTER*80 hrout_file,run,k
      INTEGER ier,i,j
C----------------------------------------------------------------------
C
      tb90l2_calor_hist_end = .true.
C
      CALL ezpick('TB90L2_CALOR_HIST_RCP')
      CALL ez_get_chars('HROUT_FILE',l,hrout_file,ier)
      CALL ezrset
      runnum = runno()
      irun = runnum/10000
      irun = irun * 10000
      irun = runnum - irun
      WRITE(run,'(I4.4)') irun
      CALL swords (hrout_file,i,j,k)
      l = len(hrout_file)
C
C ****  PICK YOUR FAVORITE MARKER TO STICK RUN AT
C
      irun = index(hrout_file,'*')
c
      IF (irun.EQ.0 )THEN
        hrout_file = hrout_file(i:j)//run
      ELSE
        hrout_file = hrout_file(i:irun-1)
     &    //run//hrout_file(irun+1:j)
      END IF
C
      CALL dhdir('TB90L2_CALOR_HIST_RCP','HBOOK_DIRECTORY',ier,' ')
c
C **** If HROUT_FILE exists then create a new one to write over
C
C
      CALL d0h_bump_version(hrout_file)
C
      CALL hrput(0,hrout_file,'NT')     ! Store all HISTOGRAMS
      END
