      LOGICAL FUNCTION l2em_ntuple()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make ntuple of L2EM bank
C-   Called from EVENT hook
C-
C-   Returned value  : true
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  10-SEP-1992   James T. McKinley
C-   Updated   8-FEB-1993   James T. McKinley - Add controls for progress
C-                          report from RCP file.
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'd0$params:l2em_ntuple.def'
      INCLUDE 'd0$inc:zebcom.inc'
C
      LOGICAL l2em_ntuple_init
      LOGICAL l2em_ntuple_reset
      LOGICAL reset_evtctr/.TRUE./,first/.TRUE./
C
      CHARACTER*80 msg                  ! holds error messages
C
      INTEGER runno                     ! gets last run number
      INTEGER runnum/0/                 ! run number
      INTEGER num_of_events             ! total number of events processed
      INTEGER ier                       ! error flag
      INTEGER i,gzesum,lesum,repfreq
      DATA repfreq/0/
c
      COMMON/nevt/num_of_events
C----------------------------------------------------------------------

C
C ****  set flag to true
C
      l2em_ntuple = .true.
C
      num_of_events = num_of_events + 1       ! increment event count
      IF(mod(num_of_events,repfreq).eq.0)THEN ! progress report every repfreq 
        WRITE(msg,1301)num_of_events,runnum   ! events
        CALL intmsg(msg)
      END IF
C
      CALL l2em_ntuple_fill()
C
C ****  Format statements
C
 1301 FORMAT(' L2EM_NTUPLE > ',i5,' EVENTS, ',
     &    ' RUN ',i7)
      RETURN
C#######################################################################
      ENTRY l2em_ntuple_reset
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reset event counter and call booking routine
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  10-SEP-1992   James T. McKinley
C-
C----------------------------------------------------------------------
      l2em_ntuple_reset = .TRUE.
      CALL l2em_ntuple_book()
      runnum=runno()
      IF(reset_evtctr)THEN
        num_of_events = 0
      ENDIF
      IF(first)THEN
        CALL ezpick('L2EM_NTUPLE_RCP')
        CALL ezget('RESET_EVTCTR',reset_evtctr,ier)
        CALL ezget('REPFREQ',repfreq,ier)
        first = .FALSE.
        CALL ezrset
      ENDIF
      RETURN
      END
