      SUBROUTINE DB_SERVER_CLEANUP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called in the beginning for cleanup
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Modified   9-AUG-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE       'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE       'D0$INC:DB_SRVR_UNITS.INC'
      INCLUDE       'D0$INC:DB_SRVR.INC'
      INCLUDE       'D0$INC:DB_SRVR_PARAM.INC'
C&IF VAXVMS
      INCLUDE       '($CLIDEF)'
C&ELSE
C&
C&ENDIF
      CHARACTER*27  CLEANUP_FILE
      CHARACTER*27  CLEANUP_LOG
      LOGICAL       FILE_THERE
      INTEGER*4     STATUS,LIB$SPAWN
C
      CLEANUP_FILE = 'DBSERV$'//DB_CURRENT_LNAME(1:3)//':'//
     &               DB_CURRENT_SNAME//'_JFCLEANUP.COM'
      INQUIRE (FILE=CLEANUP_FILE, EXIST=FILE_THERE)
      IF(.NOT.FILE_THERE) THEN
        PRINT *,'Server**ERROR** Cleanup file missing: ',cleanup_file
        GO TO 999
      ENDIF
      CLEANUP_LOG  = 'DBSERV$'//DB_CURRENT_LNAME(1:3)//':'//
     +               DB_CURRENT_SNAME//'_JFCLEANUP.LOG'
D     PRINT *,'Server- cleanup com file ',cleanup_file(1:27)
D     PRINT *,'Server- cleanup log file ',cleanup_log(1:27)
      STATUS = LIB$SPAWN (,CLEANUP_FILE(1:27)
     +                    ,CLEANUP_LOG(1:27)
     +                    ,,DB_CURRENT_SNAME//'_SRV_CLEANUP')
      IF(.NOT.STATUS) CALL LIB$STOP(%VAL(STATUS))
  999 CALL LIB$DATE_TIME(DB_DATETIME)
      PRINT *,'Server- back from cleanup on ',db_datetime
      RETURN
      END
