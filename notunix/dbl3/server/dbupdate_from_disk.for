      PROGRAM DBUPDATE_FROM_DISK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To get FZ files from disk and insert in database.
C-                         (part of online dbl3 server)
C-
C-   Inputs  :
C-   Outputs :
C-             IFLAG   > 0 Trouble
C-             NFILES  Number of files updated
C-
C-   Controls:
C-
C-   Created  20-MAY-1992   Shahriar Abachi
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*3 DECT
      INTEGER NFILES,IFLAG
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:DB_SRVR_UNITS.INC'
      INCLUDE 'D0$INC:D0DB_IDS.INC'
      INCLUDE 'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE 'D0$INC:DB_SRVR_PARAM.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:LKCALIB.INC'
      EQUIVALENCE(CALIB_LNK(1),LBANK)
      INCLUDE 'D0$INC:QUEST.INC'
C&IF VAXVMS
      INCLUDE '($SSDEF)'
C&ELSE
C&
C&ENDIF
      CHARACTER*80 FOUND_FILE,FOUND_FILE2
      CHARACTER*80 DONEFILE,TODOFILE,FAILFILE
      INTEGER IERR,ENABLE_FLAG,CONT,CLEN,CLEN2,CLEN3
      INTEGER CLEN4,CLEN5,DOT,TRULEN
      INTEGER STATUS,LIB$WAIT,SYS$SETAST
      INTEGER LIB$DATE_TIME,LIB$FIND_FILE_END
      INTEGER LIB$FIND_FILE,LIB$RENAME_FILE,LIB$DELETE_FILE
      INTEGER NF,LBANK,LUND,INTEGER
      LOGICAL FILE_THERE
      LOGICAL OK
      CHARACTER*27  TEMP_FILE
      CHARACTER*13  CACHENAME_S
      CHARACTER*11  DBFILE
      CHARACTER*40 PATH
      CHARACTER*17 CALTYPE
      INTEGER*4     ICACHE(4)
      EXTERNAL      AST_STOP
      CHARACTER*31  ALLDBOFF_UPDATE,ALLDBOFF_UPDATE_LOG
      DATA LUND /0/
C--------------------------------------------------------------
C
      CALL DB_SERVER_NAMES
C
      DECT = DB_CURRENT_LNAME(1:3)
      DBFILE = 'DBCALIB$'//DB_CURRENT_LNAME(1:3)
      IFLAG = 0
      NF = 0
      CONT = 0
C
      CALL DBCLB_INITIALIZE(DBFILE, 'SP', OK)
      IF(.NOT. OK) THEN
        PRINT *, 'update - Initialization for update_from_disk failed.
     &    Will terminate.'
        GOTO 998
      ENDIF
C
    1 CONTINUE
      STATUS = LIB$FIND_FILE('DBSERV$'//DB_CURRENT_LNAME(1:3)//
     &'$TODO:'//DB_CURRENT_SNAME//'_*.*',FOUND_FILE,CONT)
      IF(STATUS) THEN
        CALL STR$TRIM(FOUND_FILE,FOUND_FILE,CLEN)
        PRINT *, 'Update - file found ; ', FOUND_FILE(1:CLEN)
        NF = NF + 1
        DOT = 0
        DOT = INDEX(FOUND_FILE, ']')
        IF(DOT .EQ. 0) DOT = INDEX(FOUND_FILE, ':')
        FOUND_FILE2 = FOUND_FILE(DOT+1:)
        CALL STR$TRIM(FOUND_FILE2,FOUND_FILE2,CLEN2)
        DOT = INDEX(FOUND_FILE2, '.')
        CALTYPE = FOUND_FILE2(DOT+1:DOT+4)
        IF(CALTYPE(1:4) .EQ. 'PEDS') CALTYPE = 'PEDESTAL'
        IF(CALTYPE(1:4) .EQ. 'GAIN') CALTYPE = 'GAINS'
        IF(CALTYPE(1:4) .EQ. 'TIME') CALTYPE = 'TIMES'
        IF(CALTYPE(1:4) .EQ. 'DTIM') CALTYPE = 'DTIME'
        IF(CALTYPE(1:3) .EQ. 'HST') THEN
          IF(CALTYPE(4:4) .EQ. 'P') CALTYPE = 'PEDESTAL........H'
          IF(CALTYPE(4:4) .EQ. 'G') CALTYPE = 'GAINS...........H'
          IF(CALTYPE(4:4) .EQ. 'T') CALTYPE = 'TIMES...........H'
          IF(CALTYPE(4:4) .EQ. 'D') CALTYPE = 'DTIME...........H'
        ENDIF
C
C - Update database
C
        CALL DBCLB_PATH(CALTYPE,DECT,PATH)
        CALL STR$TRIM(PATH,PATH,CLEN3)
        IF(LBANK.NE.0) CALL MZDROP(IXSTP,LBANK,' ')
        CALL D0DB_UPDATE(PATH,FOUND_FILE(1:CLEN),'I',OK)
        IF(OK) THEN
          TODOFILE = 'DBSERV$'//DB_CURRENT_LNAME(1:3)//
     &'$TODO:'//FOUND_FILE2(1:CLEN2)
          DONEFILE = 'DBSERV$'//DB_CURRENT_LNAME(1:3)//'$DONE:*'
          CALL STR$TRIM(TODOFILE,TODOFILE,CLEN4)
          STATUS = LIB$RENAME_FILE(TODOFILE(1:CLEN4),DONEFILE)
          IF(STATUS) THEN
            PRINT *, 'Update - successful update.',
     &          'file ', TODOFILE, ' moved to DONE area.'
          ELSE
            PRINT *, 'Update - Renaming updated file ',
     &         FOUND_FILE(1:CLEN), 'failed. Will terminate'
            IFLAG = 1
            GOTO 998
          ENDIF
        ELSE
          FAILFILE = 'DBSERV$'//DB_CURRENT_LNAME(1:3)//
     &'$FAIL:'//FOUND_FILE2(1:CLEN2)
          CALL STR$TRIM(FAILFILE,FAILFILE,CLEN5)
          STATUS = LIB$RENAME_FILE(FOUND_FILE(1:CLEN),FAILFILE(1:CLEN5))
          PRINT *,  'Update - update failed for file: ',
     &            FOUND_FILE(1:CLEN), ' Will terminate.'
          IF(STATUS) THEN
            PRINT *,  'Update - Failed file was moved to: ',
     &            FAILFILE(1:CLEN5), '. Will terminate.'
          ELSE
            PRINT *, 'Update - Renaming the failed file ',
     &            FOUND_FILE(1:CLEN), 'failed. Will terminate'
          ENDIF
C
          IFLAG = 1
          GOTO 998
        ENDIF
        GOTO 1
      ELSE
        STATUS = LIB$FIND_FILE_END(CONT)
        PRINT *, 'Update - Number of update files done = ', NF
        NFILES = NF
        NF = 0
      ENDIF
C
  998 CONTINUE
      CALL DBCLB_FINISH
C
C----------------------------------------------------------------------
  999 CONTINUE
      END
