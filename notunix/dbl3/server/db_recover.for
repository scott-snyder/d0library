      PROGRAM DB_RECOVER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To recover calibrartion DBL3 database
C-                         from journal filesin case of corruption.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  08-FEB-1992   Shahriar Abachi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:D0DB_IDS.INC'
      INCLUDE 'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE 'D0$INC:DB_SRVR_PARAM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C&IF VAXVMS
      INCLUDE '($SSDEF)'
C&ELSE
C&
C&ENDIF
      CHARACTER*9  DB_CLUSCOM_NAME, PATH
      CHARACTER*35 UPDATE_FILE
      CHARACTER*35 UPDATE_LOG
      CHARACTER*11 DBFILE
      CHARACTER*2  CHOP
      CHARACTER*25 FILENAME_DAT
      CHARACTER*80 FOUND_FILE
      CHARACTER*21 RENFILE
      INTEGER IERR,ENABLE_FLAG,CONT,CLEN,DOT
      INTEGER STATUS,LIB$WAIT,LIB$SPAWN,SYS$SETAST,LIB$DATE_TIME
      INTEGER LIB$FIND_FILE,LIB$RENAME_FILE
      CHARACTER*1   DAY
      CHARACTER*2   HOUR
      CHARACTER*2   MINT
      CHARACTER*2   SEC
      CHARACTER*2   DSEC
      INTEGER       NDAY,NHOUR,NMINT,NSEC,NDSEC
      INTEGER       CTOI
      LOGICAL       FILE_THERE,TIMELEFT
      INTEGER       I,IDO,ITIM,NF
      REAL          DELT
      LOGICAL IOK,FOREVER,OK
      CHARACTER*13  CACHENAME_S
      INTEGER*4     ICACHE(4)
      EXTERNAL      AST_STOP
      DATA FOREVER, IOK /.TRUE., .TRUE./
C
      CALL LIB$DATE_TIME(DB_DATETIME)
      PRINT *, 'Update - Recovery Started at ', DB_DATETIME
C
      CALL DB_CHK_CLIENT(DB_CURRENT_SNAME,DB_CURRENT_LNAME,
     &                   DB_CLUSCOM_NAME, IERR)
C
C ***     Now look for parameter file
C
      IRECL = 1024                             ! record length in words
C
      DBFILE = 'DBCALIB$'//DB_CURRENT_LNAME(1:3)
      CHOP(1:2) = 'SU'
      CALL DBCLB_INITIALIZE(DBFILE,CHOP,IOK)
      IF(.NOT. IOK) THEN
        CALL ERRDB('Update - error in initializing database')
        CALL DBCLB_FINISH
        GOTO 999
      ENDIF
C
      CALL LIB$DATE_TIME(DB_DATETIME)
      CONT = 0
    1 CONTINUE
      STATUS = LIB$FIND_FILE('DBSERV$'//DB_CURRENT_LNAME(1:3)//':'//
     &DB_CURRENT_SNAME(1:2)//'*.DBRECOVER', FOUND_FILE, CONT)
      IF(STATUS) THEN
        NF = NF + 1
        CALL STR$TRIM(FOUND_FILE,FOUND_FILE,CLEN)
        PRINT *, 'Update - file found ; ', FOUND_FILE(1:CLEN)
        DOT = 0
        DOT = INDEX(FOUND_FILE, ']')
        IF(DOT .NE. 0) THEN
          DOT = DOT + 9
        ELSE
          DOT = INDEX(FOUND_FILE, '.')
        ENDIF
        PATH = ' '
        CALL D0DB_UPDATE(PATH,FOUND_FILE(1:CLEN),'JAI',OK)
        IF(OK) THEN
          PRINT *, 'For ',FOUND_FILE(DOT-8:DOT),
     &                            ' file recovery succeeded'
          GOTO 1
        ELSE
          PRINT *, '*** Failour for ',FOUND_FILE(DOT-8:DOT),
     &                            ' file. Recovery failed'
          PRINT *, '*** ONLY ',NF, 'files were done. Recovery STOPPED '
          GOTO 999
        ENDIF
      ELSE
        PRINT *, 'Update - Number of update files done = ', NF
        NF = 0
      ENDIF
C
      CALL LIB$DATE_TIME(DB_DATETIME)
      PRINT *, 'Recovery completed at ', DB_DATETIME
C
C----------------------------------------------------------------------
  999 CONTINUE
      CALL DBCLB_FINISH
      END
