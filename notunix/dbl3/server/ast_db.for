      SUBROUTINE AST_DB (NUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Activated by a broadcast cache peforms the updating
C-                         of database
C-
C-   Inputs  :
C-   Outputs : NUM    Number associated with cache
C-   Controls:
C-
C-   Modified   9-AUG-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C      INCLUDE       'D0$INC:DB_EFS.INC'
C
      INCLUDE       'D0$PARAMS:INFOCACHE.PARAMS'
      INCLUDE       'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE       'D0$INC:DB_SRVR.INC'
      INCLUDE       'D0$INC:DB_SRVR_PARAM.INC'
      INCLUDE       'D0$INC:DRCCOM.INC'
      INCLUDE       'D0$INC:QUEST.INC'
C
      CHARACTER*11 CACHENAME
      CHARACTER*27 FILENAME
      CHARACTER*11 DIRNAME
      INTEGER       MODTRANS,IERR,NUM,NTRANSACT
      INTEGER       IQUEST_SAVE_1,IQUEST_SAVE_2
      PARAMETER    (MODTRANS=1)   !changed to print everytime
      INTEGER*4    B_OPTION
      CHARACTER*40 ACTN(8)
      DATA ACTN /'Data updated', 'New directory created',
     &           'Data objects deleted', 'Entire tree deleted',
     &           'Keys renamed', 'Name or help information eneterd',
     &           'Alias name of directory renamed',
     &           'Few partitions deleted'/
C
      INTEGER*4    ICACHE(4)
      BYTE         BCACHE(16)
      EQUIVALENCE  (BCACHE,ICACHE)
      INTEGER      STATUS
C
      CALL IC_NUM_ID(NUM,CACHENAME,' ',IERR)
      IF(IERR.NE.0)  THEN
        PRINT *,'Server- AST_DB: error from IC_NUM_ID, cachename '
     +         ,cachename,' ',ierr
        GO TO 999
      ENDIF
      IF(CACHENAME(1:2).NE.DB_CURRENT_SNAME(1:2) ) THEN
        PRINT *,'Server- cache inconsistent with server'
        PRINT *,'      - cachename, dbname ',cachename
     +         ,' ',db_current_sname
        GO TO 999
      ENDIF
      CALL IC_DELIVER(CACHENAME,ICACHE,' ',IERR)
      IF(IERR.NE.0) THEN
        PRINT *,'Server- error in delivery from cache '
     +         ,cachename,' ',ierr
        GO TO 999
      ENDIF
C
C -    The cluscom file name is defined thru the client name
C -     Example:  DBMCOM$CAL:CA_DBC_SA.DBMCOM
C
      DIRNAME         = 'DBMCOM$'//DB_CURRENT_LNAME(1:3)//':'
      FILENAME(1:11)  = DIRNAME
      FILENAME(12:14) = DB_CURRENT_SNAME//'_'
      FILENAME(15:20) = CHAR(BCACHE(1))//CHAR(BCACHE(2))//
     &                  CHAR(BCACHE(3))//CHAR(BCACHE(4))//
     &                  CHAR(BCACHE(5))//CHAR(BCACHE(6))
      FILENAME(21:)   = '.DBMCOM'
C
      CALL CC_SETUP('DRCCOM',FILENAME,IFRSDR,ILSTDR,'RO',IERR)
      IF(IERR.NE.0) THEN
        PRINT *,'Server- Error from CC_SETUP'
        CALL CC_CLOSE('DRCCOM',' ',IERR)
        GO TO 999
      ENDIF
      IF(ICACHE(4).EQ.-1) THEN
        B_OPTION = ICACHE(3)
        CALL CC_UPDATE('DRCCOM',' ',IERR)
        IF(IERR.NE.0) THEN
          CALL LIB$DATE_TIME (DB_DATETIME)
          PRINT *,'Server- error in CC_UPDATE on ',db_datetime
          CALL CC_CLOSE('DRCCOM',' ',IERR)
          GO TO 999
        ENDIF
D       PRINT *,'Server- CC_UPDATE done on ',db_datetime
        CALL CC_CLOSE('DRCCOM',' ',IERR)
        IF(ierr.NE.0) STOP 667
C
C -       Here call DBFZUP. This automatically performs
C -       DBENTR, DBPURG and DBENTB operations.
C
        CALL DBFZUP(999,' ')
C
C -       Fill ICACHE with the answer
C
        IF(iquest(1).NE.0)PRINT*,'Server back from DBFZUP iquest(1)'
     +                          ,iquest(1)
        IQUEST_SAVE_1 = IQUEST(1)
        IQUEST_SAVE_2 = IQUEST(2)
        CALL RZSAVE
        CALL LIB$DATE_TIME (DB_DATETIME)
        IF(B_OPTION.EQ.1) THEN
          PRINT *,'Server- DBFZUP ',db_datetime
     +           ,' path ',dbfzup_pathn(1:dbfzup_pathn_l)
          PRINT *,'        option B action: ',ACTN(dbfzup_iact)
          JF_TRANSACTIONS_B = JF_TRANSACTIONS_B + 1
        ELSE
          PRINT *,'Server- DBFZUP ',db_datetime
     +         ,' path ',dbfzup_pathn(1:dbfzup_pathn_l)
          PRINT *,'        Action: ',ACTN(dbfzup_iact)
          JF_TRANSACTIONS = JF_TRANSACTIONS+1
        ENDIF
        ICACHE(2) = IQUEST_SAVE_2
        ICACHE(3) = IQUEST_SAVE_1
        ICACHE(4) = 1
        CALL IC_SEND(CACHENAME,ICACHE,' ',IERR)
        IF(IERR.NE.0) THEN
          CALL LIB$DATE_TIME (DB_DATETIME)
          PRINT *,'Server- Error in handshaking with client on '
     +           ,db_datetime,' ',ierr
          GO TO 999
        ENDIF
        CLIENT_DATA = .TRUE.
        IF(B_OPTION.EQ.1.AND.JF_TRANSACTIONS.EQ.0) CLIENT_DATA = .FALSE.
        NTRANSACT = NTRANSACT + 1
        IF(MOD(NTRANSACT,MODTRANS).EQ.0) THEN !changed to print everytime
          PRINT *,'        Client was ', filename(15:19), '.....'
          PRINT *,'        # of Transactions so far : ' ,ntransact
        ENDIF
      ELSE
        PRINT *,'Server- AST_DB wrong flag set in the cache',icache(4)
      ENDIF
  999 RETURN
      END
