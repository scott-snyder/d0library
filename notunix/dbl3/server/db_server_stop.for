      SUBROUTINE DB_SERVER_STOP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To stop the DBL3 calibration database server.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-AUG-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE       'D0$INC:DRCCOM.INC'
      INCLUDE       'D0$INC:QUEST.INC'
      INCLUDE       'D0$INC:DB_SRVR_UNITS.INC'
      INCLUDE       'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE       'D0$INC:DB_SRVR.INC'
      INCLUDE       'D0$INC:DB_SRVR_PARAM.INC'
C&IF VAXVMS
      INCLUDE       '($CLIDEF)'
      INCLUDE       '($SSDEF)'
C&ELSE
C&
C&ENDIF
C      INCLUDE       'D0$INC:DB_EFS.INC'
      INTEGER*4     EFMASK
      BYTE          EFX(0:31)
      INTEGER*4     EF_I_SET,EFMASK_NOW,EF_SET
      INTEGER       TIME_INTERVAL(2)
C
      LOGICAL       FOREVER
      INTEGER       STATUS,SYS$SETAST,LIB$DELETE_FILE
      CHARACTER*30  FILENAME_FZ1_LAST
      BYTE          ENABLE_FLAG
      CHARACTER*25  BACKUP_FILE
      CHARACTER*29  BACKUP_LOG
      LOGICAL       FILE_THERE
      INTEGER       IERR,NEXT_JFILE
C
C           Disable AST delivery
      ENABLE_FLAG = 0
      STATUS = SYS$SETAST (%VAL(ENABLE_FLAG) )
      IF(STATUS.EQ.SS$_WASSET)
     &    PRINT *,'Server- AST delivery now disabled'
      CALL LIB$DATE_TIME(DB_DATETIME)
      PRINT *,'Server - Stopping by request at ',db_datetime
      CALL FZENDO(LUNFZ1,'T')
      CLOSE (LUNFZ1)
      CALL  DBCLB_END
      CLOSE (UNIT=LUNRZ)
      IF(CLIENT_DATA) THEN
        FILENAME_FZ1_LAST = FILENAME_FZ1(1:22)//'DBFZCLOS'
        CALL LIB$RENAME_FILE (FILENAME_FZ1, FILENAME_FZ1_LAST)
        PRINT *,'Server- file ',filename_fz1,' renamed to '
     &             ,filename_fz1_last
        PRINT *,'Server- ',jf_transactions
     &             ,' transactions in this journal file'
      ELSE
        STATUS = LIB$DELETE_FILE (FILENAME_FZ1//';')
        IF(.NOT.STATUS) THEN
          PRINT *,'Server**DURING STOP** file ',filename_fz1,
     &                ' not deleted/not found'
        ELSE
          PRINT *,'Server- file ',filename_fz1,' EMPTY deleted'
        ENDIF
        NEXT_JFILE = LAST_JFILE - 1
        PRINT *,'Server- last valid journal file was ',next_jfile
        WRITE (NEXT_JFILE_STR,'(I6.6)') NEXT_JFILE
        CALL DB_SERVER_WRTLJF(IERR)
      ENDIF
C           Reenable AST delivery
      ENABLE_FLAG = 1
      STATUS = SYS$SETAST (%VAL(ENABLE_FLAG) )
      IF(STATUS.EQ.SS$_WASCLR)
     +    PRINT *,'Server- AST delivery now enabled'
      CALL  IC_DROP_LOCK(CACHE_RESLCK,' ',IERR)
      CALL  CPC_FINISH
C----------------------------------------------------------------------
  999 RETURN
      END
