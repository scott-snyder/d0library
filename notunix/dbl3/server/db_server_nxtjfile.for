      SUBROUTINE DB_SERVER_NXTJFILE(IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prepares the next update journal file
C-
C-   Inputs  :
C-   Outputs : IOK  If > 0 problem
C-   Controls:
C-
C-   Modified  20-SEP-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IOK
      INCLUDE       'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE       'D0$INC:DB_SRVR_UNITS.INC'
      INCLUDE       'D0$INC:DB_SRVR_PARAM.INC'
      INCLUDE       'D0$INC:QUEST.INC'
C&IF VAXVMS
      INCLUDE       '($CLIDEF)'
C&ELSE
C&
C&ENDIF
      CHARACTER*11  CACHENAME_DB
      CHARACTER*11  DBFILE
      LOGICAL       DB_RECOVER_TRY
      CHARACTER*30  FILENAME_FZ1_LAST
      CHARACTER*27  FILENAME_SPA
      CHARACTER*27  FILENAME_LOG
      LOGICAL       FILE_THERE,OK
      INTEGER       LIB$DELETE_FILE,LIB$SPAWN
      INTEGER       STATUS,IOERR1,ERR,NEXT_JFILE,IERR,IOS
      REAL          WAIT_TIME
C
      IOK = 0
C
      IF(.NOT.CLIENT_DATA) THEN
        IF(JF_TRANSACTIONS_B.GT.0) THEN
          PRINT *,'Server- ',jf_transactions_b,
     &            ' transactions with option B'
        ELSE
          PRINT *,'Server- NO DATA received since last wakeup'
        ENDIF
        JF_TRANSACTIONS_B = 0
        PRINT *,'Server- file ',filename_fz1,' EMPTY going to be killed'
        CALL FZENDO(LUNFZ1,'T')
        CLOSE (LUNFZ1,ERR=110)
        STATUS = LIB$DELETE_FILE (FILENAME_FZ1//';')
        IF(.NOT.STATUS) THEN
          PRINT *,'Server**ERROR** file ',filename_fz1,' not deleted'
          GO TO 999
        ENDIF
        OPEN (UNIT=LUNFZ1,FILE=FILENAME_FZ1,ACCESS='SEQUENTIAL',
     &        FORM='FORMATTED',RECORDTYPE='FIXED',
     &        CARRIAGECONTROL='LIST',
     &        RECL=80,STATUS='NEW',IOSTAT=IOERR1)
        IF(IOERR1.NE.0) THEN
          PRINT *,'Server**ERROR** opening error on FZ1',ioerr1
          GO TO 999
        ENDIF
        CALL LIB$DATE_TIME(DB_DATETIME)
        PRINT *,'Server- file ',filename_fz1,' reopened on ',db_datetime
        CALL FZFILE(LUNFZ1,0,'AO')
        CALL FZLOGL(LUNFZ1,2)
        CALL DBFZOP(LUNFZ1,TOPDIR_DB,' ')
        GO TO 999
      ENDIF
C
      WAIT_TIME = 3.
      CALL FZENDO(LUNFZ1,'T')
      CLOSE (LUNFZ1,ERR=100,IOSTAT=IOS)
      CALL LIB$WAIT(WAIT_TIME)
      FILENAME_FZ1_LAST = FILENAME_FZ1(1:21)//'DBFZCLOS'
      CALL LIB$RENAME_FILE (FILENAME_FZ1, FILENAME_FZ1_LAST)
      CALL LIB$DATE_TIME (DB_DATETIME)
      PRINT *,'Server- file ',filename_fz1,' renamed to '
     +       ,filename_fz1_last,' on ',db_datetime
      PRINT *,'Server- ',jf_transactions
     +       ,' transactions in this journal file'
      IF(JF_TRANSACTIONS_B.GT.0) THEN
        PRINT *,'Server- ',jf_transactions_b,
     &          ' transactions with option B'
      ENDIF
      JF_TRANSACTIONS   = 0
      JF_TRANSACTIONS_B = 0
      NEXT_JFILE = LAST_JFILE+1
      LAST_JFILE = NEXT_JFILE
      WRITE (NEXT_JFILE_STR,'(I6.6)') NEXT_JFILE
*      get the character from the number in 6 digits form
      FILENAME_FZ1 = 'DBSPOOL$'//DB_CURRENT_LNAME(1:3)//':'
     +               //DB_CURRENT_SNAME//NEXT_JFILE_STR//'.DBFZCURR'
      PRINT *,'Server- Open new CURRENT file ',filename_fz1
      OPEN (UNIT=LUNFZ1,FILE=FILENAME_FZ1,ACCESS='SEQUENTIAL',
     +      FORM='FORMATTED',RECORDTYPE='FIXED',CARRIAGECONTROL='LIST',
     +      RECL=80,STATUS='NEW',IOSTAT=IOERR1)
      IF(IOERR1.NE.0) THEN
        PRINT *,'Server**ERROR** opening error on FZ1',ioerr1
        GO TO 999
      ENDIF
      CALL DB_SERVER_WRTLJF(IERR)
      IF(IERR.NE.0) THEN
        PRINT *,'Server**ERROR** Error in updating lastnum file'
      ENDIF
C
      DB_RECOVER_TRY = .FALSE.
      DBFILE = 'DBCALIB$'//DB_CURRENT_LNAME(1:3)
   77 CALL DBEND
      CLOSE(UNIT=LUNRZ)
      CALL DBCLB_INITIALIZE(DBFILE,'UOS',OK)
      IF(.NOT.OK .AND. .NOT.DB_RECOVER_TRY) THEN
        PRINT *,'Server- DBINIT FIRST TRY, iquest(1) ',iquest(1)
        DB_RECOVER_TRY = .TRUE.
        GO TO 77
      ENDIF
      IF(.NOT. OK) THEN
        IOK = 1
        GOTO 998
      ENDIF
C
      CLIENT_DATA = .FALSE.
      CALL FZFILE(LUNFZ1,0,'AO')
      CALL FZLOGL(LUNFZ1,2)
      CALL DBFZOP(LUNFZ1,TOPDIR_DB,' ')
      FILENAME_LOG = 'DBSPOOL$'//DB_CURRENT_LNAME(1:3)//':'//
     +               DB_CURRENT_SNAME//'_TOFFLINE.LOG'
      FILENAME_SPA = 'DBSPOOL$'//DB_CURRENT_LNAME(1:3)//':'//
     +       DB_CURRENT_SNAME//'_TOFFLINE.COM'
      INQUIRE (FILE=FILENAME_SPA, EXIST=FILE_THERE)
      IF(.NOT.FILE_THERE) THEN
        PRINT *,'Server**ERROR** TOFFLINE file missing: ',filename_spa
        GO TO 999
      ENDIF
      STATUS = LIB$SPAWN (,FILENAME_SPA(1:27)
     +                    ,FILENAME_LOG(1:27)
     +                    ,CLI$M_NOWAIT
     +                    ,DB_CURRENT_SNAME//'_TOFFLINE')
      IF(.NOT.STATUS) CALL LIB$STOP(%VAL(STATUS))
C
  999 CLIENT_DATA = .FALSE.
      RETURN
  100 PRINT *,'Server**ERROR** Error closing journal file'
      RETURN
  110 PRINT *,'Server**ERROR** Error closing empty journal file'
      RETURN
  200 PRINT *,'Server**ERROR** Error in opening journal file '
     +       ,filename_fz1
  998 CONTINUE
C----------------------------------------------------------------------
      RETURN
      END
