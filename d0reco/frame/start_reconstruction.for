      SUBROUTINE START_RECONSTRUCTION(INUNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-    read input ZEBRA records, handle runs and events
C-
C-    Input:  INUNIT= unit for reading events
C-
C-   Created   7-SEP-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER INUNIT,IOS
      LOGICAL STOP_D0RECO,SKIP_D0RECO,FLGVAL
      INTEGER NUM_EVTS,NPROC
      DATA NPROC/0/
C----------------------------------------------------------------------
C
   10 CALL EVTRD(INUNIT,IOS)  ! read ZEBRA record
C
C           event record
      IF(IOS.EQ.0) THEN
        IF(STOP_D0RECO()) THEN
          CALL END_RECO_RUN
          GOTO 999
        ENDIF
        IF(SKIP_D0RECO()) GOTO 10
        IF(FLGVAL('WRITE_E_FILE')) CALL WRITE_E_FILE   ! write one event file
        CALL NEW_RECO_RUN(0)
        CALL RECONSTRUCT_EVENT
        CALL D0RECO_STATUS         ! inform about status during production
        IF(FLGVAL('WRITE_E_FILE')) CALL CLOSE_E_FILE   ! delete one event file
        NPROC=NPROC+1
        CALL D0RECO_EVTS_FILE(NUM_EVTS)
        IF(NPROC.GE.NUM_EVTS) THEN     ! processed as many as requested
          NPROC=0
          GOTO 999
        ENDIF
C
C           Begin run record
      ELSE IF(IOS.EQ.1) THEN
        IF(SKIP_D0RECO()) GOTO 10
        CALL NEW_RECO_RUN(1)
C                                       
C           End run record
      ELSE IF(IOS.EQ.2) THEN 
        CALL END_RECO_RUN
        IF(STOP_D0RECO()) GOTO 999
C                             
C         end-of-file
      ELSE IF(IOS.GT.2) THEN
        GOTO 999      ! done
      ENDIF
      GOTO 10         ! get another record
C
  999 RETURN
      END
