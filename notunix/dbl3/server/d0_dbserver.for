      PROGRAM D0_DBSERVER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main program for DBL3 calibration database server.
C-                         Uses INFOCACHE & CLUSCOM features of CPC
C-                         package to communicate between clients and server.
C-
C          Each server books the following caches:
C          xx_DB_CACHE   for client <--> server communications
C          xx_DB_RESLK   for exclusive use of server
C          xx_STOP_CACHE for exclusive use of server
C                        xx stands for detector label
C
C-
C-   Created   7-AUG-1991   SHAHRIAR ABACHI
C-   Modified 24-MAY-1992   SHAHRIAR ABACHI  Disk update feature added
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE       'D0$INC:ZEBSTP.INC'
      INCLUDE       'D0$INC:DBSTP.INC'
      INCLUDE       'D0$INC:DRCCOM.INC'
      INCLUDE       'D0$INC:QUEST.INC'
      INCLUDE       'D0$INC:DB_SRVR_UNITS.INC'
      INCLUDE       'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE       'D0$INC:D0DB_IDS.INC'
      INCLUDE       'D0$INC:DB_SRVR.INC'
      INCLUDE       'D0$INC:DB_SRVR_PARAM.INC'
C&IF VAXVMS
      INCLUDE       '($JPIDEF)'
      INCLUDE       '($CLIDEF)'
      INCLUDE       '($SSDEF)'
C&ELSE
C&
C&ENDIF
C      INCLUDE       'D0$INC:DB_EFS.INC'
      INTEGER*4     EFMASK
      BYTE          EFX(0:31)
      INTEGER*4     EF_I_SET,EFMASK_NOW,EF_SET
C
      INTEGER       TIME_INTERVAL(2)
C
      LOGICAL       FOREVER
      INTEGER       STATUS,STAT,SYS$SETAST,IOK
      CHARACTER*30  FILENAME_FZ1_LAST
      CHARACTER*9   CMD
      BYTE          ENABLE_FLAG
      CHARACTER*25  BACKUP_FILE
      CHARACTER*29  BACKUP_LOG
      CHARACTER*33  UPDATE_STOP
      CHARACTER*33  UPDATE_STOP_LOG
      CHARACTER*34  UPDATE_FROM_DISK
      CHARACTER*34  UPDATE_FROM_DISK_LOG
      CHARACTER*80  FOUND_FILE
      CHARACTER*13  PRC_NAM
C
      CHARACTER*1   DAY
      CHARACTER*2   HOUR
      CHARACTER*2   MINT
      CHARACTER*2   SEC
      CHARACTER*2   DSEC
      CHARACTER*3   DECT
      INTEGER       NDAY,NHOUR,NMINT,NSEC,NDSEC,LIB$GETJPI
      INTEGER       CTOI,LIB$SPAWN,CONT,CONT1,CONT2,CONT3
      INTEGER       LIB$FIND_FILE,LIB$FIND_FILE_END,ILEN
      LOGICAL       FILE_THERE,TIMELEFT
      INTEGER       IERR,I,ITIM,NDT,NFILES,IFLAG,ITEM
      REAL          DELT,DT
      CHARACTER*1   AC
      DATA AC, JF_TRANSACTIONS_B /'C',0/
      DATA DT /30.0/
      CHARACTER*1 EXT(10)
      DATA EXT /'1','2','3','4','5','6','7','8','9','0'/
      DATA CONT,CONT1,CONT2,CONT3 /0,0,0,0/
C
      FOREVER     = .TRUE.
      CLIENT_DATA = .FALSE.
C
      CALL GTUNIT(171,LUNFZ1,IERR)
      CALL GTUNIT(171,LUNFZ2,IERR)
      CALL GTUNIT(171,LUNPAR,IERR)
      CALL GTUNIT(171,LUNLJF,IERR)
      CALL GTUNIT(171,LUNJT1,IERR)
      CALL GTUNIT(171,LUNJT3,IERR)
C
      PRINT *,'  ------>   Database Server Version 2.0 <------- '
C           Parse the names
      CALL DB_SERVER_NAMES
C           Lock the resource
      CALL DB_SERVER_LCKRES
C           Clean up files
      CALL DB_SERVER_CLEANUP
C           Initialize
      CALL DB_SERVER_INIT(IOK)
      IF(IOK .EQ. 1) THEN
        PRINT *,
     &    'Problem with initialization in DB_SERVER_INIT. Will stop.'
        GOTO 998
      ENDIF
C
      TIMELEFT = .TRUE.
C
      DAY       = DTIME(1:1)
      HOUR(1:2) = DTIME(3:4)
      MINT(1:2) = DTIME(6:7)
      SEC(1:2)  = DTIME(9:10)
      DSEC(1:2) = DTIME(12:13)
      NDAY  = CTOI(DAY)
      NHOUR = CTOI(HOUR)
      NMINT = CTOI(MINT)
      NSEC  = CTOI(SEC)
      NDSEC = CTOI(DSEC)
C
      ITIM = NDAY*24*60*60 + NHOUR*60*60 + NMINT*60 + NSEC
      DELT = FLOAT(ITIM) + FLOAT(NDSEC)/100.
      NDT = DELT / DT
C
      UPDATE_FROM_DISK = 'DBSERV$'//DB_CURRENT_LNAME(1:3)
     &//':'//DB_CURRENT_SNAME//'_UPDATE_FROM_DISK.COM'
      UPDATE_FROM_DISK_LOG = 'DBSERV$'//DB_CURRENT_LNAME(1:3)
     &//':'//DB_CURRENT_SNAME//'_UPDATE_FROM_DISK.LOG'
      UPDATE_STOP = 'DBSERV$'//DB_CURRENT_LNAME(1:3)
     &//':'//DB_CURRENT_SNAME//'_DISKUPDATE_FAIL.COM'
      UPDATE_STOP_LOG = 'DBSERV$'//DB_CURRENT_LNAME(1:3)
     &//':'//DB_CURRENT_SNAME//'_DISKUPDATE_FAIL.LOG'
      PRC_NAM = DB_CURRENT_SNAME//'_DISKUPDATE'
      ITEM = JPI$_PRCNAM
C
      ENABLE_FLAG = 1
      STATUS = SYS$SETAST (%VAL(ENABLE_FLAG) )
      IF(STATUS.EQ.SS$_WASCLR)
     +          PRINT *,'Server- AST delivery now enabled'
C
      DO WHILE(TIMELEFT)
        DO I=1,NDT
          CALL DB_SERVER_READPAR(IERR)
          IF(DISK_UPDATE .GT. 0) THEN
            STATUS = LIB$GETJPI(ITEM,,PRC_NAM,,PRC_NAM,ILEN)
            IF(.NOT. STATUS) THEN
              STATUS = LIB$FIND_FILE('DBSERV$'//DB_CURRENT_LNAME(1:3)//
     &'$TODO:'//DB_CURRENT_SNAME//'_*.*',FOUND_FILE,CONT1)
              IF(STATUS) THEN
                STAT = LIB$FIND_FILE_END(CONT1)
                CALL LIB$DATE_TIME(DB_DATETIME)
                PRINT *,'Server- Disk file found, Will spawn a process
     +            to update at : ',DB_DATETIME
                STATUS = LIB$SPAWN (,UPDATE_FROM_DISK
     +                   ,UPDATE_FROM_DISK_LOG
     +                   ,CLI$M_NOWAIT
     +                   ,DB_CURRENT_SNAME//'_DISKUPDATE')
                IF(STATUS) THEN
                  CALL LIB$DATE_TIME(DB_DATETIME)
                  PRINT *,'Server- Disk update successfuly completed
     +              at: ', DB_DATETIME
                ELSE
                  CALL LIB$DATE_TIME(DB_DATETIME)
                  PRINT *, 'Server- Will terminate due to
     &              update from disk problem at ',DB_DATETIME
                  CALL LIB$STOP(%VAL(STATUS))
                  STATUS = LIB$SPAWN (,UPDATE_STOP
     +                    ,UPDATE_STOP_LOG
     +                    ,CLI$M_NOWAIT
     +                    ,DB_CURRENT_SNAME//'_DISKUPDATE_FAIL')
                  IF(.NOT.STATUS) CALL LIB$STOP(%VAL(STATUS))
                  TIMELEFT = .FALSE.
                  GOTO 998
                ENDIF
              ENDIF
            ENDIF
            IF(IQUEST(100) .EQ. -9) THEN    !Check for stop
              TIMELEFT = .FALSE.
              GOTO 998
            ENDIF
          ENDIF
          CALL L3_WAIT(DT)
        ENDDO
        ENABLE_FLAG = 0
        STATUS = SYS$SETAST (%VAL(ENABLE_FLAG) )
        IF(STATUS.EQ.SS$_WASSET)
     +    PRINT *,'Server- AST delivery now disabled'
        CALL LIB$DATE_TIME(DB_DATETIME)
        PRINT *,'Server- waking up on ',db_datetime
C
   77   STATUS = LIB$GETJPI(ITEM,,PRC_NAM,,PRC_NAM,ILEN)
        IF(STATUS) THEN
          CALL L3_WAIT(DT)
          GOTO 77
        ELSE
          CALL DB_SERVER_NXTJFILE(IOK)
          IF(IOK .EQ. 1) THEN
            TIMELEFT = .FALSE.
            PRINT *,'Server- problem in DB_SERVER_NXTJFILE. Will stop'
            GOTO 998
          ENDIF
        ENDIF
C
        CALL LIB$DATE_TIME(DB_DATETIME)
        PRINT *,'Server- back to sleep on ',db_datetime
C -  Reenable AST delivery
        ENABLE_FLAG = 1
        STATUS = SYS$SETAST (%VAL(ENABLE_FLAG) )
        IF(STATUS.EQ.SS$_WASCLR)
     +    PRINT *,'Server- AST delivery now enabled'
        PRINT *, ' ------------ '
C - Stop requested
        IF(IQUEST(100) .EQ. -9) THEN
          TIMELEFT = .FALSE.
        ENDIF
      ENDDO
C
  998 CONTINUE
   78 STATUS = LIB$GETJPI(ITEM,,PRC_NAM,,PRC_NAM,ILEN)
      IF(STATUS) THEN
        CALL L3_WAIT(DT)
        GOTO 78
      ENDIF
        ENABLE_FLAG = 1
        STATUS = SYS$SETAST (%VAL(ENABLE_FLAG) )
        IF(STATUS.EQ.SS$_WASCLR)
     +    PRINT *,'Server- AST delivery now enabled'
        CALL DB_SERVER_STOP
C
  999   STOP
        END
