      PROGRAM DBOFF_SERVER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main program for calorimeter calibrartion
C-                         DBL3 database server.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-SEP-1991   Shahriar Abachi
C-   Modified 19-FEB-1992   Modified for unix Lee Lueking
C-   Modified 05-JUN-1992   S. Abachi & H. Greenlee  Final mods for Unix
C-   Modified 08-JUL-1992   S. Abachi  More modifications for unix
C-   Modified 10-JUL-1992   S. Abachi  Stopping procedure modification
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:D0DB_IDS.INC'
      INCLUDE 'D0$INC:DB_SRVR_UNITS.INC'
      INCLUDE 'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE 'D0$INC:DB_SRVR_PARAM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C&IF VAXVMS
      INCLUDE '($CLIDEF)'
      INCLUDE '($SSDEF)'
C&ELSE
C&ENDIF
      CHARACTER*9  DB_CLUSCOM_NAME
      CHARACTER*80 PATH
      CHARACTER*80 UPDATE_FILE
      CHARACTER*80 UPDATE_LOG
      CHARACTER*80 DBFILE
      CHARACTER*80 FILENAME_DAT
      CHARACTER*80 FOUND_FILE
      CHARACTER*80 RENFILE
      INTEGER IERR,ENABLE_FLAG,CONT,CLEN,DOT,TRULEN
      LOGICAL LIB$SPAWN,SYS$SETAST
      LOGICAL LIB$FIND_FILE,LIB$RENAME_FILE,STATUS
      CHARACTER*1   DAY
      CHARACTER*2   HOUR
      CHARACTER*2   MINT
      CHARACTER*2   SEC
      CHARACTER*2   DSEC
      INTEGER       NDAY,NHOUR,NMINT,NSEC,NDSEC
      INTEGER       CTOI
      LOGICAL       FILE_THERE,TIMELEFT
      INTEGER       I,IDO,ITIM,NF,NT,NDT,MRK
      REAL          DELT,DT
      LOGICAL IOK,FOREVER,OK
      CHARACTER*13  CACHENAME_S
      INTEGER*4     ICACHE(4)
      EXTERNAL      AST_STOP
      INTEGER NSQNC,OLD_SQNC,DS,LU,CL
      CHARACTER*1 MSND
      CHARACTER*80 COMMAND
      CHARACTER*6 SQNC
      CHARACTER*80  ALLDBOFF_UPDATE,ALLDBOFF_UPDATE_LOG,PAR(4)
      DATA FOREVER, IOK /.TRUE., .TRUE./
      DATA DT /10.0/
C
      IRECL = 1024
      OLD_SQNC = 0
      CALL LIB$DATE_TIME(DB_DATETIME)
      PRINT *, 'Update - Server Started at ', DB_DATETIME
      CALL DB_CHK_CLIENT(DB_CURRENT_SNAME,DB_CURRENT_LNAME,
     &                   DB_CLUSCOM_NAME, IERR)
C
C&IF VAXVMS
C      CACHENAME_S =  DB_CURRENT_SNAME(1:2)//'_STOP_CACHE'
C      CALL IC_BOOK (CACHENAME_S, ICACHE,'M',IERR)
C      IF(IERR.NE.0) THEN
C        PRINT *,'Server**ERROR** error in booking cache',CACHENAME_S
C      ENDIF
C      CALL IC_ASSOCIATE (CACHENAME_S, ICACHE, AST_STOP, ' ', IERR)
C      IF(IERR.NE.0) THEN
C        PRINT *,'Server**ERROR** error in associating cache',CACHENAME_S
C      ENDIF
C
      DBFILE = 'DBCALIB$'//DB_CURRENT_LNAME(1:3)
      UPDATE_FILE = 'DBL3SERV$'//DB_CURRENT_SNAME//':'
     &     //DB_CURRENT_SNAME//'OFFLINE_UPDATE.DAT'
      UPDATE_LOG  = DB_CURRENT_SNAME//'OFFLINE_UPDATE.LOG'
      ALLDBOFF_UPDATE = 'DBL3SERV$'//DB_CURRENT_LNAME(1:3)//':'//
     &                      DB_CURRENT_SNAME//'_ALLDBOFFUPD.COM'
      ALLDBOFF_UPDATE_LOG = DB_CURRENT_SNAME//'_ALLDBOFFUPD.LOG'
C      FILE_THERE = LIB$FIND_FILE(ALLDBOFF_UPDATE,ALLDBOFF_UPDATE,CONT)
C      CALL LIB$FIND_FILE_END(CONT)
C      IF(.NOT.FILE_THERE) THEN
C        PRINT *,'Update**ERROR** ALLUPDATE file missing: '
C     &                                            ,ALLDBOFF_UPDATE
C        GO TO 999
C      ENDIF
C&ELSE
C&      DBFILE = 'DBCALIB$'//DB_CURRENT_LNAME(1:3)
C&ENDIF
C
C ***     Now look for parameter file
C
      FILENAME_DAT = 'DBL3SERV$'//DB_CURRENT_LNAME(1:3)//':'//
     &        DB_CURRENT_SNAME//'_PARAM.DAT'
C
      CALL GTUNIT(15,LUNPAR,IERR)
C
      CONT = 0
      FILE_THERE = LIB$FIND_FILE(FILENAME_DAT,FILENAME_DAT,CONT)
      CALL LIB$FIND_FILE_END(CONT)

      IF(FILE_THERE) THEN
        MRK = INDEX(FILENAME_DAT, ';')
        IF(MRK .NE. 0) THEN
          FILENAME_DAT = FILENAME_DAT(1:MRK-1)//' '
        ENDIF
        CALL DBOFF_SERVER_READPAR (FILENAME_DAT,IERR)
      ELSE
        PRINT *, ' No parameter file found. Will stop.'
        GOTO 999
      ENDIF
C      IF(IERR.NE.0.OR..NOT.FILE_THERE) THEN
C - Define few parameters with default values
C        PRINT *, 'Server - Parameter file ',FILENAME_DAT, 'not found.'
C        DTIME = '0 01:00:00.00'                  ! wakeup time d hh:mm:ss.cc
C        ISTOP = 0
C        DISK_UPDATE = 0
C      ENDIF
C
      IF(ISTOP .GT. 0) THEN
        PRINT *, 'ISTOP flag was on. Will set to off.'
        CALL GTUNIT(171,LU,IERR)
        CALL D0OPEN (LU,FILENAME_DAT,'IOF', FILE_THERE)
        IF(.NOT. FILE_THERE) GOTO 20
        DO I=1,4
          READ(LU,10) PAR(I)
        ENDDO
        PAR(3)(1:2) = '0 '
        REWIND(UNIT=LU)
        DO I=1,4
          WRITE(LU,10) PAR(I)
        ENDDO
        CLOSE(UNIT=LU)
        ISTOP = 0
        CALL RLUNIT(171,LU,IERR)
   10   FORMAT(A)
      ENDIF
C
   20 CONTINUE
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
      ITIM = NDAY*24*60*60 + NHOUR*60*60 + NMINT*60 + NSEC
      DELT = FLOAT(ITIM) + FLOAT(NDSEC)/100.
      NDT = DELT / DT
C
      DO WHILE(FOREVER)
        CALL LIB$DATE_TIME(DB_DATETIME)
        PRINT *, 'Update - Back from sleep', DB_DATETIME
C&IF VAXVMS
C        ENABLE_FLAG = 0
C        STATUS = SYS$SETAST(%VAL(ENABLE_FLAG))
C        IF(STATUS .EQ. SS$_WASSET)
C     &      PRINT *, 'Update - AST delivery disabled'
C&ELSE
C&ENDIF
        NT = 0
    2   CONTINUE
        CALL DBOFF_SERVER_INIT(DBFILE,IOK)
        IF(.NOT. IOK) THEN
          CALL ERRDB('Update - error in initializing database')
          NT = NT + 1
          IF(NT .LT. 2) GOTO 2
          CALL DBCLB_FINISH
          CALL LIB$DATE_TIME(DB_DATETIME)
          PRINT *, 'Update - Second try for initialization failed'
          PRINT *, 'Update - exiting at ', DB_DATETIME
C&IF VAXVMS
          STATUS = LIB$SPAWN (,'FAILED_COM'
     &                    ,'FAILED_LOG'
     &                    ,CLI$M_NOWAIT
     &                    ,DB_CURRENT_SNAME//'_FAILED_PROC')
C&ELSE
C&          CALL SYSTEM('mail denisenk@fnal <
C&     &/usr/people/dzero/dbl3/FAILED.COM')
C&
C&ENDIF
          GOTO 999
        ENDIF
        CONT = 0
C
    1   CONTINUE
        FOUND_FILE = 'DBL3SERV$'//DB_CURRENT_LNAME(1:3)//
     &':*.*CLOS'
        STATUS = LIB$FIND_FILE(FOUND_FILE,FOUND_FILE,CONT)
        CALL LIB$FIND_FILE_END(CONT)
        IF(STATUS) THEN
          NF = NF + 1
          CLEN=TRULEN(FOUND_FILE)
C
          PRINT *, 'Update - file found ; ', FOUND_FILE(1:CLEN)
          DOT = 0
          DOT = INDEX(FOUND_FILE, ']')
          RENFILE = FOUND_FILE(DOT+1:)
          DOT = INDEX(RENFILE, '.')
          RENFILE = RENFILE(1:DOT)//'dbfzupdated'
C
          SQNC(1:6) = RENFILE(DOT-6:DOT-1)
          NSQNC = CTOI(SQNC)
          IF(OLD_SQNC .NE. 0) THEN
            DS = NSQNC - OLD_SQNC
            IF(DS .NE. 1) THEN
              CALL LIB$DATE_TIME(DB_DATETIME)
              PRINT *,'Server- Sequence number error on D0 FZ files '
              PRINT *,'NEW_SQNC=',NSQNC,'    OLD_SQNC=',OLD_SQNC
              PRINT *,'Will terminate at ', db_datetime
C&IF VAXVMS
              STATUS = LIB$SPAWN (,'FAILED_COM'
     &                    ,'FAILED_LOG'
     &                    ,CLI$M_NOWAIT
     &                    ,DB_CURRENT_SNAME//'_FAILED_PROC')
C&ELSE
C&              CALL SYSTEM('mail denisenk@fnal <
C&     &/usr/people/dzero/dbl3/FAILED.COM')
C&ENDIF
              GOTO 999
            ENDIF
          ENDIF
          OLD_SQNC = NSQNC
C
C - Update database
C
          PATH = ' '
          CALL D0DB_UPDATE(PATH,FOUND_FILE(1:CLEN),'JAI',OK)
          IF(OK) THEN
            STATUS = LIB$RENAME_FILE(FOUND_FILE,RENFILE)
            IF(STATUS) PRINT *, 'Update - successful update.',
     &          'file renamed to: ', RENFILE
          ELSE
            RENFILE = FOUND_FILE(DOT-8:DOT)//'dbfzfailed'
            STATUS = LIB$RENAME_FILE(FOUND_FILE,RENFILE)
            PRINT *,  'Update - update failed for file: ',
     &                                      FOUND_FILE(1:CLEN)
            PRINT *,  'Update - update server will terminate '
C&IF VAXVMS
            STATUS = LIB$SPAWN (,'FAILED_COM'
     &                    ,'FAILED_LOG'
     &                    ,CLI$M_NOWAIT
     &                    ,DB_CURRENT_SNAME//'_FAILED_PROC')
C&ELSE
C&          CALL SYSTEM('mail denisenk@fnal <
C&     &/usr/people/dzero/dbl3/FAILED.COM')
C&ENDIF
            GOTO 999
          ENDIF
          GOTO 1
        ELSE
          PRINT *, 'Update - Number of update files done = ', NF
          NF = 0
        ENDIF
C
C - copy journal files to other offline nodes (only if on Main offline)
C
C&IF VAXVMS
        CALL DBOFF_SERVER_READPAR(FILENAME_DAT, IERR)
        IF(DISK_UPDATE .GT. 0) THEN
          MSND = 'Y'
          COMMAND = 
     &      '@'//'TOFFLN_COM'//' '//DB_CURRENT_SNAME//' '//MSND
          CL = LEN(COMMAND)
          STATUS = LIB$SPAWN(COMMAND(1:CL)
     &                     ,,'TOFFLN_LOG'
     +                     ,,DB_CURRENT_SNAME//'_ALLDBOFFUPD')
          IF(.NOT.STATUS) CALL LIB$STOP(%VAL(STATUS))
        ENDIF
C&ELSE
C&ENDIF
C
C - Close database before going to sleep
C
        CALL DBCLB_FINISH
C&IF VAXVMS
C        ENABLE_FLAG = 1
C        STATUS = SYS$SETAST(%VAL(ENABLE_FLAG))
C        IF(STATUS .EQ. SS$_WASCLR)
C     &      PRINT *, 'Update - AST delivery enabled'
C&ELSE
C&ENDIF
C
        CALL LIB$DATE_TIME(DB_DATETIME)
        PRINT *, 'Update - Back to sleep at ', DB_DATETIME
        DO I=1,NDT
          CALL LIB$WAIT(DT)
C          PRINT *, ' Update - TEST'
          CALL LIB$DATE_TIME(DB_DATETIME)
C&IF VAXVMS
C          IF(IQUEST(100) .EQ. -9) THEN
C            FOREVER = .FALSE.
C            PRINT *, 'Update - Stop requested at ', DB_DATETIME
C            GOTO 999
C          ENDIF
C&ELSE
C&ENDIF
          CALL DBOFF_SERVER_READPAR(FILENAME_DAT, IERR)
          IF(ISTOP .NE. 0) THEN
            FOREVER = .FALSE.
C&IF VAXVMS
            PRINT *, 'Update - Stop requested at ', DB_DATETIME
            STATUS = LIB$SPAWN (,'STOPED_COM'
     &                        ,'STOPED_LOG'
     &                        ,CLI$M_NOWAIT
     &                        ,DB_CURRENT_SNAME//'_STOPED_PROC')
C
C&ELSE
C&            CALL SYSTEM('mail denisenk@fnal <
C&     &/usr/people/dzero/dbl3/STOPED.COM')
C&ENDIF
            GOTO 999
          ENDIF
          FOUND_FILE = 'DBL3SERV$'//DB_CURRENT_LNAME(1:3)//
     &':*.*CLOS'
          STATUS = LIB$FIND_FILE(FOUND_FILE,FOUND_FILE,CONT)
          CALL LIB$FIND_FILE_END(CONT)
          IF(STATUS) GOTO 3
        ENDDO
    3   PRINT *, '----------------------------------------------'
      ENDDO
C
C----------------------------------------------------------------------
  999 CONTINUE
      CALL DBCLB_FINISH
C&IF VAXVMS
C      CALL CPC_FINISH
C&ELSE
C&ENDIF
      END
