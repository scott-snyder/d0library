      SUBROUTINE DB_SERVER_INIT(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To initialize the DBL3 calibrartion database sever
C-
C-   Inputs  :
C-   Outputs : 
C-   Controls: OK  if > 0 trouble
C-
C-   Created   9-AUG-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE       'D0$INC:DRCCOM.INC'
      INCLUDE       'D0$INC:QUEST.INC'
      INCLUDE       'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE       'D0$INC:DB_SRVR_UNITS.INC'
      INCLUDE       'D0$INC:DB_SRVR.INC'
      INCLUDE       'D0$INC:DB_SRVR_PARAM.INC'
C&IF VAXVMS
      INCLUDE       '($SSDEF)'
C&ELSE
C&
C&ENDIF
      CHARACTER*11  CACHENAME_DB
      CHARACTER*13  CACHENAME_S
      CHARACTER*11  DBFILE
      CHARACTER*80  RESULT
      CHARACTER*2   JFORM
      INTEGER*4     ICACHE(4)
      INCLUDE '($LNMDEF)'
      LOGICAL       DB_RECOVER_TRY
      LOGICAL       FILE_THERE
      INTEGER*4     STATUS,SYS$SETAST
      BYTE          ENABLE_FLAG
      INTEGER       IERR,NEXT_JFILE,LTOP,IOERR,IOERR1,IOERR2
      INTEGER  SYS$TRNLNM,LIB$SET_LOGICAL
      EXTERNAL SYS$TRNLNM,LIB$SET_LOGICAL,STR$TRIM
      INTEGER ITMLST(4), ISTAT, RETLEN
      INTEGER*2 LENCOD(2)
      EQUIVALENCE (ITMLST,LENCOD)
      LOGICAL       IOK
      INTEGER OK
      EXTERNAL      AST_DB
      EXTERNAL      AST_STOP
      DATA JFORM /'AO'/                 ! ASCII Exchange
C      DATA JFORM /'X0'/                ! Exchange
C      DATA JFORM /'O '/                ! Local
C----------------------------------------------------------------
C
      OK = 0
      LENCOD(1) = 80
      LENCOD(2) = LNM$_STRING
      ITMLST(2) = %LOC(RESULT)
      ITMLST(3) = %LOC(RETLEN)
      ITMLST(4) = 0
C
      DB_RECOVER_TRY = .FALSE.
      JF_TRANSACTIONS = 0
C
C -     Define cache name for current server
C -     Example: CA_DB_CACHE.
C
      CACHENAME_DB = DB_CURRENT_SNAME(1:2)//'_DB_CACHE'
D     PRINT *,'Server- db cache ',cachename_db
C
      CALL IC_BOOK (CACHENAME_DB, ICACHE,'M',IERR)
      IF(IERR.NE.0) THEN
        PRINT *,'Server**ERROR** error in booking cache ',cachename_db
      ENDIF
      CALL IC_ASSOCIATE (CACHENAME_DB, ICACHE,AST_DB,' ',IERR)
      IF(IERR.NE.0) THEN
        PRINT *,'Server**ERROR** error in associating cache '
     &          ,cachename_db
      ENDIF
C
      CACHENAME_S =  DB_CURRENT_SNAME(1:2)//'_STOP_CACHE'
      CALL IC_BOOK (CACHENAME_S, ICACHE,'M',IERR)
      IF(IERR.NE.0) THEN
        PRINT *,'Server**ERROR** error in booking cache',CACHENAME_S
      ENDIF
      CALL IC_ASSOCIATE (CACHENAME_S, ICACHE, AST_STOP, ' ', IERR)
      IF(IERR.NE.0) THEN
        PRINT *,'Server**ERROR** error in associating cache',CACHENAME_S
      ENDIF
C -         Disable AST delivery
      ENABLE_FLAG = 0
      STATUS = SYS$SETAST (%VAL(ENABLE_FLAG) )
      IF(status.EQ.ss$_wasset)
     +print *,'Server- AST delivery now disabled'
C
C -     Now look for parameter file
C
      FILENAME_PAR = 'DBSERV$'//DB_CURRENT_LNAME(1:3)//':'//
     &        DB_CURRENT_SNAME//'_PARAM.DAT'
C
      INQUIRE (FILE=FILENAME_PAR, EXIST=FILE_THERE)
      IF(FILE_THERE) THEN
        CALL DB_SERVER_READPAR (IERR)
      ENDIF
      IF(IERR.NE.0.OR..NOT.FILE_THERE) THEN
C - Define few parameters with default values
        PRINT *, 'Server - Parameter file ',FILENAME_PAR, 'not found.'
        IRECL = 1024                          ! record length in words
        DTIME = '0 01:00:00.00'               ! wakeup time d hh:mm:ss.cc
        DISK_UPDATE = 1                       ! envoke disk update
        ISTOP = 0                             ! do not stop
      ENDIF
C
C
C -     Now look for last journal file number
      FILENAME_JF  = 'DBSERV$'//DB_CURRENT_LNAME(1:3)//':'//
     &                 DB_CURRENT_SNAME//'_JF_LASTNUM.DAT'
      INQUIRE (FILE=FILENAME_JF , EXIST=FILE_THERE)
      IF(FILE_THERE) THEN
        PRINT *,'Server- last num file found'
        CALL DB_SERVER_READLJF (IERR)
        PRINT *,'Server- last num from file is',last_jfile
      ENDIF
      IF(IERR.NE.0.OR..NOT.FILE_THERE) THEN
C -       Define default journal file number
        PRINT *,'Server- last journal file number undefined'
        LAST_JFILE = 0
      ENDIF
C
C -     Define new journal file name
C -     Example: DBSPOOL$CAL:CA000023.DBFZCURR
C
      IF(LAST_JFILE.EQ.0) THEN
        FILENAME_FZ1 = 'DBSPOOL$'//DB_CURRENT_LNAME(1:3)//':'
     +                 //DB_CURRENT_SNAME//'000001.DBFZCURR'
        NEXT_JFILE = LAST_JFILE+1
        LAST_JFILE = NEXT_JFILE
        WRITE (NEXT_JFILE_STR,'(I6.6)') NEXT_JFILE
D       PRINT *,'Server- next jfile string ',next_jfile_str
      ELSE
        NEXT_JFILE = LAST_JFILE+1
        LAST_JFILE = NEXT_JFILE
C         Get the character from the number in 6 digits form
        WRITE (NEXT_JFILE_STR,'(I6.6)') NEXT_JFILE
D       PRINT *,'Server- next jfile string ',next_jfile_str
        FILENAME_FZ1 = 'DBSPOOL$'//DB_CURRENT_LNAME(1:3)//':'
     +                 //DB_CURRENT_SNAME//NEXT_JFILE_STR//'.DBFZCURR'
      ENDIF
D     PRINT *,'Server- journal file ',filename_fz1
C
C -     Define proper TopDir name for this server
C
CC      TOPDIR_DB   = 'DB'//DB_CURRENT_SNAME
      TOPDIR_DB   = 'D0STP'
D     PRINT *,'Server- TopDir ',topdir_db
C
C -     Define database (logical) file name for current server
C -     Example: DBCALIB$CAL
C
      DBFILE = 'DBCALIB$'//DB_CURRENT_LNAME(1:3)
C
C -  Initialize Zebra and open the RZ unit
C
    1 CONTINUE
      INQUIRE (FILE=DBFILE, EXIST=FILE_THERE)
      IF(.NOT.FILE_THERE) THEN
        PRINT *,'Server**ERROR** database file ',filename_db
     +         ,' not existing'
        GO TO 999
      ENDIF
C
      CALL DBCLB_INITIALIZE(DBFILE,'UOS',IOK)
C
      IF(.NOT.IOK .AND. .NOT.DB_RECOVER_TRY) THEN
        PRINT *,'Server- DBINIT FIRST TRY, iquest(1) ',iquest(1)
        CALL DBEND
        CLOSE(UNIT=LUNRZ)
        DB_RECOVER_TRY = .TRUE.
        GO TO 1
      ENDIF
      CALL DBLOGL(LUNRZ,2)
      IF(.NOT. IOK) THEN
        OK = 1
        GOTO 999
      ENDIF
C
      IF(JFORM(1:2) .EQ. 'AO') THEN
        OPEN (UNIT=LUNFZ1,FILE=FILENAME_FZ1,ACCESS='SEQUENTIAL',
     +      FORM='FORMATTED',RECORDTYPE='FIXED',CARRIAGECONTROL='LIST',
     +      RECL=80,STATUS='UNKNOWN',IOSTAT=IOERR1)
      ELSE
        OPEN (UNIT=LUNFZ1,FILE=FILENAME_FZ1, FORM='UNFORMATTED',
     +      STATUS='UNKNOWN',IOSTAT=IOERR1)
      ENDIF
      IF(IOERR1.NE.0) THEN
        PRINT *,'Server**ERROR** opening error on FZ1',ioerr1
        GO TO 999
      ENDIF
      CALL DB_SERVER_WRTLJF(IERR)
      IF(IERR.NE.0) THEN
        PRINT *,'Server**ERROR** Error in updating lastnum file'
      ENDIF
      CALL FZFILE(LUNFZ1,0,JFORM)
      CALL FZLOGL(LUNFZ1,2)
      CALL DBFZOP(LUNFZ1,TOPDIR_DB,' ')
C
C      CALL FZFILE(LUNFZ2,0,JFORM)
C      CALL FZLOGL(LUNFZ2,2)
C      CALL DBFZOP(LUNFZ2,TOPDIR_DB,'B')
C
  999 CONTINUE
C           Reenable AST delivery
      ENABLE_FLAG = 1
      STATUS = SYS$SETAST (%VAL(ENABLE_FLAG) )
      IF(status.EQ.ss$_wasclr)
     +print *,'Server- AST delivery now enabled'
      RETURN
      END
