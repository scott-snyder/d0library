      SUBROUTINE UGLAST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End Of Run, Print Histograms, CLOSE files,.....
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: NOne
C-
C-   Created   ??
C-   Updated   6-DEC-1988   A.M.Jonckheere   Added LV0, made into standard
C-                                              form.
C-   Updated   5-JUN-1989   Harrison B. Prosper  
C-   Added user hook LULAST.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:GCLIST.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$PARAMS:ACPDEF.PARAMS'
      INCLUDE 'D0$INC:ACP_USER.INC'
C
      INTEGER IER
C
C&IF UFFARM
C&      INCLUDE 'MLT$INCLUDE:MLT_CODES.INC'
C&      INTEGER MLT_SEE_TALK
C&      INTEGER STATUS
C&ENDIF
C----------------------------------------------------------------------
C
      CALL GLAST
      CALL ZEBFIN
      IF(NGET.GT.0 .OR. NSAVE .GT. 0) CALL GCLOSE(0,IER)
C
C ***********************
C ****  USER HOOK LULAST: End-Of-Run processing
C ***********************
      CALL LULAST
C
C
C&IF UFFARM
C&      CALL MAP_SEND(1,1,PRQ_TALK,STATUS)             ! Multi
C&      CALL MAP_WAITLOOP(1.)                          ! Multi
C&ENDIF
C
      CALL MZEND
      CALL ACP_QUEUE_PROCESS(ACP$THIS_PROCESS,DONEQ)
      CALL ACP_SYNC(ACP$ALL_PROCESSES,RUNDOWN)
      CALL ACP_STOP_PROCESS
C&IF UFFARM
C&      CALL MAP_DISCON                                ! Multi
C&      CALL MLB_TIME_SUMMARY(6)                       ! Multi
C&ENDIF
C
  999 RETURN
      END
