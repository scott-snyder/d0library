      PROGRAM D0DBL3_SERVER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : A program to serve most D0 dbl3 databases in
C-                         online platform and to offline machines.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   10-JUN-1992   SHAHRIAR ABACHI
C-   Modified  31-AUG-1992   SHAHRIAR ABACHI  Created common server for online
C-                                            and offline including Unix.
C-   Updated   15-SEP-1992   Chip Stewart     Added Heartbeat
C-   Modified  02-NOV-1992   SHAHRIAR ABACHI  New version with FZ update option
C-   Modified  24-NOV-1992   SHAHRIAR ABACHI  On unix delete updated file
C-   Modified  25-NOV-1992   SHAHRIAR ABACHI  Possible to update ASCII FZ
C-   Modified  27-NOV-1992   SHAHRIAR ABACHI  FZ file mode can be changed
C-   Modified  14-JAN-1993   SHAHRIAR ABACHI  Quicker stop + more
C-   Modified  08-MAR-1993   SHAHRIAR ABACHI  More printouts
C-   Modified  18-NOV-1993   SHAHRIAR ABACHI  Compact database updates
C-                                            implemented.
C-   Modified  16-DEC-1993   SHAHRIAR ABACHI  Compact database can now
C-                                            be modified as well
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NFILES
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      INCLUDE 'D0$INC:D0DBL3_LNK.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C&IF VAXVMS
      INCLUDE '($CLIDEF)'
C&ELSE
C&
C&ENDIF
      CHARACTER*120 FOUND_FILE,FOUND_FILE2,FOUND_FILE3,DBFILE2,FFILE
      CHARACTER*120 RENFILE,DONEFILE,TODOFILE,FAILFILE,UPDFILE,RNFILE
      INTEGER IERR,ENABLE_FLAG,CONT,CL,CLEN1,CLEN,CLEN2,CLEN3,CLEN6
      INTEGER CLEN4,CLEN5,DOT,MRK,TRULEN,CLEN7,CLEN8,LEN1
      INTEGER LIB$DATE_TIME,IDF,IDF2
      LOGICAL STATUS,LIR,XTOA,ILG,D0DBL3_GTHVEC
      LOGICAL LIB$FIND_FILE,LIB$RENAME_FILE,LIB$DELETE_FILE
      LOGICAL LIB$FIND_FILE_END,UPDCALL
      INTEGER NF,LBANK,LUND,IACT,IR
      INTEGER NKEY,NKEY1,KEY(50),I,COL
      CHARACTER*50 PATH,PATH2,PATH3
      CHARACTER*80 FZFILE
      CHARACTER*30 COPT
      CHARACTER*10 CHOP
      LOGICAL FILE_THERE,LIB$SPAWN
      LOGICAL OKO,OKC,OK,OK2,FOREVER,FJOFF
      CHARACTER*27  TEMP_FILE
      CHARACTER*23 DB_DATETIME
      CHARACTER*2 DAY,HOUR,MINT,SEC,DSEC
      INTEGER NDAY,NHOUR,NMINT,NSEC,NDSEC,ITIM,CTOI,IOP
      CHARACTER*31  ALLDBOFF_UPDATE,ALLDBOFF_UPDATE_LOG
      CHARACTER*6 SQNC
      CHARACTER*1 MSND,CHP,CHP2
      CHARACTER*80 COMMAND
      CHARACTER*2 SNAME
      CHARACTER*3 SRVRDIR
      CHARACTER*70 CMAIL1
      INTEGER NSQNC,DS,IER,IER1,NW,NWP,NWT,OLD_SQNC,IOK,ICT
      INTEGER LDUM,CL1,CL2,CL3,CL4,CL5,IRET,IRT,IRET1,CLT,MRK1
      LOGICAL CLOS,D0DBL3_OPENFZ,D0DBL3_CLOSEFZ
      LOGICAL D0DBL3_INFZ,D0DBL3_OUTFZ,COMP,FIRST_SEND
      REAL DELT,TDELT
      INTEGER MFREE,IOPN,IFLG,IK,NHVEC,HVEC(200),IOCK
      EQUIVALENCE (LBANK,D0DBLNK)
      CHARACTER*80 TODO_AREA,DONE_AREA,FAIL_AREA,SRVR_AREA,RAREA
      CHARACTER*1 SC,CHRC(9)
      CHARACTER*9 TOFFLPR
      CHARACTER*80 DATABASE(2)
      DATA LUND,NWP /0,40/
      DATA FIRST_SEND,FOREVER /.TRUE.,.TRUE./
C
      CMAIL1 = ' '
      FOUND_FILE2 = ' '
      FOUND_FILE3 = ' '
      SRVRNM = ' '
      PATH = ' '
      PATH2 = ' '
      PATH3 = ' '
C&IF VAXVMS
      SC = ':'
      TODO_AREA = 'TODO_AREA'
      DONE_AREA = 'DONE_AREA'
      FAIL_AREA = 'FAIL_AREA'
      SRVR_AREA = 'SRVR_AREA'
C&ELSE
C&      SC = '/'
C&      CALL GETENV('TODO_AREA',TODO_AREA)
C&      CALL GETENV('DONE_AREA',DONE_AREA)
C&      CALL GETENV('FAIL_AREA',FAIL_AREA)
C&      CALL GETENV('SRVR_AREA',SRVR_AREA)
C&ENDIF
      CL1 = TRULEN(TODO_AREA)
      CL2 = TRULEN(DONE_AREA)
      CL3 = TRULEN(FAIL_AREA)
      CL4 = TRULEN(SRVR_AREA)
C
      CALL TRNLNM('DBFILE',DATABASE(1),LEN1)
      CALL TRNLNM('DBFILC',DATABASE(2),LEN1)
C      DATABASE(1) = 'DBFILE'
C      DATABASE(2) = 'DBFILC'
      IOK = 0
      OLD_SQNC = 0
      NF = 0
      IK = 0
      NFILES = 0
      CONT = 0
      NW = 0
      CLOS = .FALSE.
      NWT = 0
      TDELT = 0.0
      DBINI = .FALSE.
      OPTJ = .FALSE.
      FJOFF = .FALSE.
      DBFILE2 = ' '
      CHOP = ' '
      CHP = ' '
      XTOA = .FALSE.
      COMP = .FALSE.
C
      CALL D0DBL3_READPAR(IER)
      IF(IER .NE. 0) THEN
        PRINT *, 'Server- Problem in reading parameter file in
     &    D0DBL3_READPAR. Will terminate.'
        CALL D0DBL3_FAIL(IOK)
        GOTO 998
      ENDIF
      IF(CHOPJ(3:3) .NE. ' ' .AND. (CHOPJ(2:2) .NE.
     &            CHOPJ(3:3))) XTOA = .TRUE.
      IF(CHOPJ(2:2) .EQ. 'G' .OR. CHOPJ(2:2) .EQ. 'X') THEN
        CHP = 'X'
      ELSE
        CHP = CHOPJ(2:2)
      ENDIF
      IF(CHP .EQ. 'X') CHP = ' '
      CHP2 = CHOPJ(3:3)
      IF(ICOMP .GT. 0) COMP = .TRUE.
C
      ICT = 0
      IF(D0DBLNK .GT. 0) THEN
        CALL MZDROP(IXSTP,D0DBLNK,'L')
        D0DBLNK = 0
      ENDIF
      CALL MZWIPE(2)
C
   10 CALL D0DBL3_INIT(DATABASE,OK)
C&IF VAXVMS
      CALL D0DBL3_HEARTBEAT_INIT
C&ELSE
C&
C&ENDIF
      IF(.NOT. OK) THEN
        ICT = ICT + 1
        IF(ICT .LT. 2) THEN
          PRINT *, 'Server- Problem in initializatn. Will try again.'
          CALL LIB$WAIT(30.)
          GOTO 10
        ENDIF
        PRINT *,
     &    'Server- Problem in dbl3 initialization. Will terminate'
        CALL D0DBL3_FAIL(IOK)
        GOTO 998
      ENDIF
C
      CALL D0DBL3_SERVER_CHK(IER)
      IF(IER .NE. 0) THEN
        PRINT *, 'Server- Problem in parameter and/or bookeeping files.
     &    Will terminate.'
        CALL D0DBL3_FAIL(IOK)
        GOTO 998
      ELSE
        SNAME = SRVRNM(1:2)
        IF(SNAME .EQ. 'GL') SNAME = 'DM'
      ENDIF
C
      SRVRDIR = SRVRNM
      IF(SRVRNM(1:3) .EQ. 'GLB' .OR. SRVRNM(1:3) .EQ. 'glb')
     &      SRVRDIR = 'DBM'
      CALL CUTOL(SRVRDIR)
      CMAIL1 = '/usr/sbin/Mail dbl3mail < /dbl3/'
     &            //SRVRDIR(1:3)// '/server/STOPED.COM'
      IF(SRVRNM(1:3) .EQ. ' ' .OR. SRVRNM(1:3) .EQ. 'UNK' .OR.
     &  SRVRNM(1:3) .EQ. 'unk') CMAIL1 =
     &  '/usr/sbin/Mail dbl3mail < /dbl3/STOPED.COM'
C
C-----------------------------------------------------------------------------
C
C-- Server main loop:
C
      DO WHILE(FOREVER)
C
  200   CONTINUE
C&IF VAXVMS
        CALL D0DBL3_HEARTBEAT
        IF(OPTJ .AND. IONLINE .GT. 0 .AND. NFRSH .GT. 0) THEN
          CALL D0DBL3_END_JOURNAL
          STATUS = LIB$FIND_FILE('JFILE',FOUND_FILE,CONT)
          IF(STATUS) THEN
            STATUS = LIB$FIND_FILE_END(CONT)
            CONT = 0
            CLEN = TRULEN(FOUND_FILE)
            STATUS = LIB$RENAME_FILE(FOUND_FILE(1:CLEN),'JFILEC')
            IF(.NOT. STATUS) THEN
              PRINT *, 'Server- Problem in journal file renaming.',
     &                 ' Will terminate.'
              IOK = -1
              CALL D0DBL3_FAIL(IOK)
              GOTO 998
            ENDIF
            FJOFF = .TRUE.
          ENDIF
        ENDIF
        IF(IONLINE .EQ. 0) FJOFF = .TRUE.
C&ELSE
C&
C&ENDIF
        IF(DBINI) THEN
          CALL D0DBL3_FINISH
        ENDIF
        IF(CLOS) THEN
          CLOS = .FALSE.
          GOTO 300
        ENDIF
C
C - Begin FZ search loop
C
    1   CONTINUE
C&IF VAXVMS
        CALL D0DBL3_HEARTBEAT
C&ELSE
C&
C&ENDIF
        IDF = 0
        IDF2 = 0
        IF ( IONLINE .EQ. 1 .OR.
     &            (IONLINE .EQ. 0 .AND. NFRSH .EQ. 0) ) THEN
          STATUS = LIB$FIND_FILE(TODO_AREA(1:CL1)//SC//'SEQ*.D0DBL3FZ'
     &              ,FOUND_FILE,CONT)
        ELSE
          STATUS = LIB$FIND_FILE(TODO_AREA(1:CL1)//SC//'JOFF*.DBFZCLOS'
     &              ,FOUND_FILE,CONT)
        ENDIF

        IF(STATUS) THEN
          OKO = .TRUE.
          OK = .TRUE.
          IF(.NOT. DBINI) THEN
            ICT = 0
            IF(D0DBLNK .GT. 0) THEN
              CALL MZDROP(IXSTP,D0DBLNK,'L')
              D0DBLNK = 0
            ENDIF
            CALL MZWIPE(IDVSTP)
            CALL MZNEED(IDVSTP,0,'G')
            MFREE = IQUEST(11)
            PRINT *, 'Server- ZEBSTP free space in memory = ', MFREE
            IF(MFREE .LT. 100000) THEN
              PRINT *, 'Server - ZEBSTP free space very low. Will stop.'
              CALL D0DBL3_FAIL(IOK)
              GOTO 998
            ENDIF
   20       CALL D0DBL3_INIT(DATABASE,OK)
            IF(.NOT. OK) THEN
              ICT = ICT + 1
              IF(ICT .LT. 2) THEN
                PRINT *, 'Server- Problem in initializatn. Will try.'
                CALL LIB$WAIT(30.)
                GOTO 20
              ENDIF
              PRINT *,
     &          'Server- Problem in dbl3 initialization. Will terminate'
              CALL D0DBL3_FAIL(IOK)
              GOTO 998
            ENDIF
C&IF VAXVMS
            IF(.NOT. OPTJ .AND. IONLINE .GT. 0 .AND. NFRSH .NE. 0)THEN
              CALL D0DBL3_SET_JOURNAL(OK)
              IF(.NOT. OK) THEN
                PRINT *, 'Server- Problem in journal file setting.',
     &                     ' Will terminate.'
                IOK = -1
                CALL D0DBL3_FAIL(IOK)
                GOTO 998
              ENDIF
            ENDIF
C&ELSE
C&
C&ENDIF
          ENDIF
          CLEN = TRULEN(FOUND_FILE)
          PRINT *, 'Server- file found ; ', FOUND_FILE(1:CLEN)
          NF = NF + 1
          MRK = 0
C&IF VAXVMS
          MRK = INDEX(FOUND_FILE, ']')
          IF(MRK .EQ. 0) MRK = INDEX(FOUND_FILE, ':')
          FOUND_FILE2 = FOUND_FILE(MRK+1:)
C&ELSE
C&          DBFILE2 = FOUND_FILE
C&50        CONTINUE
C&          MRK = INDEX(DBFILE2, '/')
C&          IF(MRK.EQ.0)GO TO 51
C&          DBFILE2 = DBFILE2(MRK+1:)
C&          GO TO 50
C&51        CONTINUE
C&          FOUND_FILE2 = DBFILE2
C&ENDIF
          DOT = INDEX(FOUND_FILE2, '.')
          FOUND_FILE3 = FOUND_FILE2(1:DOT)//'D0DBL3FZUPDATED'
          FAILFILE = FOUND_FILE2(1:DOT)//'FZOUTFAILED'
C
          CLEN2 = TRULEN(FOUND_FILE2)
          CLEN6 = TRULEN(FOUND_FILE3)
          MRK = INDEX(FOUND_FILE2, '.')
          SQNC(1:6) = FOUND_FILE2(MRK-6:MRK-1)
          NSQNC = CTOI(SQNC)
          IF(OLD_SQNC .NE. 0) THEN
            DS = NSQNC - OLD_SQNC
            IF(DS .NE. 1) THEN
              CALL LIB$DATE_TIME(DB_DATETIME)
              PRINT *,'Server- Sequence number error on D0 FZ files '
              PRINT *,'NEW_SQNC=',NSQNC,'    OLD_SQNC=',OLD_SQNC
              PRINT *,'Will terminate at ', DB_DATETIME
              IOK = -1
              CALL D0DBL3_FAIL(IOK)
              GOTO 998
            ENDIF
          ENDIF
          OLD_SQNC = NSQNC
          IF(LBANK.NE.0) CALL MZDROP(IXSTP,LBANK,' ')
          TODOFILE = TODO_AREA(1:CL1)//SC//FOUND_FILE2(1:CLEN2)
          CLEN4 = TRULEN(TODOFILE)
          UPDFILE = TODO_AREA(1:CL2)//SC//FOUND_FILE3(1:CLEN6)
          DONEFILE = DONE_AREA(1:CL2)//SC//FOUND_FILE2(1:CLEN2)
          CLEN1 = TRULEN(DONEFILE)
          RENFILE = ' '
C&IF VAXVMS
          IF(NFRSH .NE. 1) THEN
            RAREA = TODO_AREA
            RENFILE = UPDFILE
          ELSE
            RAREA = DONE_AREA
            RENFILE = DONEFILE
          ENDIF
C&ELSE
C&          RAREA = DONE_AREA
C&          RENFILE = DONEFILE
C&ENDIF
          CLEN7 = TRULEN(RENFILE)
          CLEN8 = TRULEN(RAREA)
          FAILFILE = RAREA(1:CLEN8)//FAILFILE
          CLEN5 = TRULEN(FAILFILE)
C
C - FZ file update
C
          IF(IONLINE .GT. 0 .OR. NFRSH .NE. 1) THEN
            NKEY = IABS(NKEY)
            NKEY1 = NKEY
            IF(NKEY1 .LE. 0) NKEY1 = 30
            DO I=1,NKEY1
              KEY(I) = 0
            ENDDO
            NKEY = 0
            IOPN = 0
    7       CONTINUE
C
C - Open input FZ file
C
            OK = D0DBL3_OPENFZ(FOUND_FILE(1:CLEN),IDVSTP,CHP,IDF,IRET)
            CALL LIB$DATE_TIME(DB_DATETIME)
            IF(.NOT. OK) THEN
              IOPN = IOPN + 1
              IF(IOPN .LT. 2) THEN
                PRINT *, 'Server- Error opening FZ. Will try again.'
                CALL LIB$WAIT(30.)
                GOTO 7
              ENDIF
              PRINT *, 'Server- Error opening FZ file a 2nd time.',
     &               ' Will terminate at ', DB_DATETIME
              CALL D0DBL3_FAIL(IOK)
              GOTO 998
            ELSE
              PRINT *, 'Server- FZ file successfully opened at ',
     &                DB_DATETIME
            ENDIF
C
C - Loop over contents of each FZ
C
            IOP = 0
            OK = .FALSE.
            UPDCALL = .FALSE.
            IFLG = 0
            DO WHILE ( D0DBL3_INFZ(IDF,CHOP,IACT,PATH,NKEY,KEY,COPT,
     &        LBANK,IRET1) )
              PATH(4:4) = TOPN(1)(2:2)
              OK = .FALSE.
              UPDCALL = .FALSE.
              IFLG = IFLG + 1
              CALL LIB$DATE_TIME(DB_DATETIME)
              PRINT *, 'Server- FZ file read in at ', DB_DATETIME
              IF(NKEY .EQ. 0) THEN
                PRINT *, 'Server- Corrupted or empty FZ file.',
     &               ' Will terminate at ', DB_DATETIME
                IF(IDF2 .NE. 0) THEN
                  STATUS = LIB$RENAME_FILE(RENFILE(1:CLEN7),
     &              FAILFILE(1:CLEN5))
                ENDIF
                IOK = -1
                CALL D0DBL3_FAIL(IOK)
                GOTO 998
              ENDIF
C&IF VAXVMS
              CALL D0DBL3_HEARTBEAT
C&ELSE
C&
C&ENDIF
              COL = TRULEN(COPT)
              CLEN3 = TRULEN(PATH)
              CALL LIB$DATE_TIME(DB_DATETIME)
              IF(IRET1 .NE. 0) THEN
                PRINT *, 'Server- Error reading FZ header in ',
     &               'D0DBL3_INFZ. Will terminate at ', DB_DATETIME
                IF(IDF2 .NE. 0) THEN
                  STATUS = LIB$RENAME_FILE(RENFILE(1:CLEN7),
     &              FAILFILE(1:CLEN5))
                ENDIF
                CALL D0DBL3_FAIL(IOK)
                GOTO 998
              ELSE
                PRINT *,
     &            'Server- Header successfully read for SQNC ',SQNC,
     &            ' at ', DB_DATETIME
                PRINT *, ' PATH is: ', PATH(1:CLEN3)
                PRINT *,
     &            ' DIV, IACT, CHOPT are: ', IDVSTP,IACT,'     ',COPT(1:
     &COL)
                PRINT *, ' KEYS are: '
                PRINT *, (KEY(I), I=1,IABS(NKEY))
              ENDIF
C
              IF(NKEY .LT. 0) THEN
                PRINT *,
     &            'Server- The above entry was from an empty compact '
                PRINT *, '    FZ file (or subfile).  Will be ignored.'
                GOTO 111
              ENDIF
C
C - Insert in database
C
              CALL LIB$DATE_TIME(DB_DATETIME)
              PRINT *,
     &          'Server- update of database via FZ file starts at :',
     &          DB_DATETIME
              IF(KEY(3) .GT. 0 .AND. KEY(4) .GT. 0) THEN
                CALL D0DBL3_UPDATE(IACT,PATH(1:CLEN3),NKEY,KEY,
     &                        COPT(1:COL),LBANK,OK)
                UPDCALL = .TRUE.
                IF(.NOT. OK) GOTO 101
C
C - Produce compact FZ and update compact database if required
C
                PATH2 = PATH
                IF(COMP) THEN
                  CALL LIB$DATE_TIME(DB_DATETIME)
                  IER1 = 0
                  IF(IACT .EQ. 10 .OR. IACT .EQ. 11) THEN
                    PRINT *,
     &                'Server- Making a compact calib data file at '
     &                ,DB_DATETIME
                    ILG = D0DBL3_GTHVEC(HVEC,NHVEC)
                    CALL DBL3_COMPRESS(LBANK,HVEC,NHVEC,IER1)
                  ELSE
                    PRINT *,
     &'Server- Modifying the compact database starts at '
     &                ,DB_DATETIME
                    PATH3 = ' '
                    PATH3 = PATH
                    PATH3(4:4) = TOPN(2)(2:2)
                    CALL D0DBL3_CHK_CONT(PATH3,NKEY,KEY,COPT,IOCK)
                    IF(IOCK .GT. 0) THEN
                      PRINT *,
     &'Server- No corresponding data in compressed database.'
                      PRINT *, 'Will ignore modification.'
                      NKEY = - NKEY
                      GOTO 111
                    ELSEIF(IOCK .LT. 0) THEN
                      PRINT *,
     &'Server- Error while reading compact database for modification.'
                      IER1 = -999
                    ENDIF
                  ENDIF
                  IF(IER1 .GT. 0) THEN
                    PRINT *, 'Server- No data left after compression.'
                    NKEY = - NKEY
                    GOTO 111
                  ENDIF
C
                  IF(IER1 .LT. 0) THEN
                    IF(IOCK .NE. -999) PRINT *,
     &                'Server- Problem with compression. IER=', IER1
                    OK = .FALSE.
                    UPDCALL = .FALSE.
                    GOTO 101
                  ELSE
                    PATH2(4:4) = TOPN(2)(2:2)
                    PRINT *,
     &'Server- Begin updating or modifying compact database.'
                  ENDIF
                  CALL D0DBL3_UPDATE(IACT,PATH2(1:CLEN3),NKEY,KEY,
     &                        COPT(1:COL),LBANK,OK)
                  UPDCALL = .TRUE.
                  IF(.NOT. OK) GOTO 101
                ENDIF
              ELSE
                PRINT *, 'Server-**WARNING** :'
                PRINT *, 'Key arrays are invalid. Empty FZ file ?.'
              ENDIF
C
C - Produce output mode-changed FZ files if required
C
  111         CONTINUE
              IF(XTOA .OR. COMP) THEN
                IF(IOP .EQ. 0) THEN
                  IOP = 1
                  OKO = D0DBL3_OPENFZ (
     &                RENFILE(1:CLEN7),IDVSTP,'O'//CHP2,IDF2,IRET)
                  IF(.NOT. OKO) THEN
                    PRINT *,
     &                'Server- Output FZ file could not be opened'
                    GOTO 102
                  ELSE
                    PRINT *,
     &                'Server- Mode change output file created.'
                  ENDIF
                ENDIF
                OK2 = D0DBL3_OUTFZ (IDF2,
     &               CHOP,IACT,PATH2,NKEY,KEY,COPT,LBANK,IRET)
                IF(.NOT. OK2) THEN
                  PRINT *,
     &              'Server- Problem in output FZ file. Will terminate.'
                  LIR = D0DBL3_CLOSEFZ(IDF2,IRT)
                  STATUS = LIB$RENAME_FILE(RENFILE(1:CLEN7),
     &                        FAILFILE(1:CLEN5))
                  IOK = -1
                  CALL D0DBL3_FAIL(IOK)
                  GOTO 998
                ELSE
                  PRINT *, 'Server- After mode change (',CHOPJ(2:3),
     &                '), contents were inserted in output file; ',
     &                RENFILE(1:CLEN7)
                ENDIF
              ENDIF
              PATH = ' '
              PATH2 = ' '
              NKEY = IABS(NKEY)
              NKEY1 = NKEY
              IF(NKEY1 .LE. 0) NKEY1 = 30
              DO I=1,NKEY1
                KEY(I) = 0
              ENDDO
              NKEY = 0
            ENDDO     !End-loop for reading contents of each FZ
C
C - Close up all open files
C
            IF(IDF .GT. 0) LIR = D0DBL3_CLOSEFZ(IDF,IRT)
            CALL LIB$DATE_TIME(DB_DATETIME)
            IF(IRET1 .LT. 0) THEN
              PRINT *, 'Server- Error reading last FZ header ',
     &            'in D0DBL3_INFZ. Will terminate at ', DB_DATETIME
  102         IOK = -1
              IF(IDF .GT. 0) LIR = D0DBL3_CLOSEFZ(IDF,IRT)
              IF(IDF2 .NE. 0) THEN
                LIR = D0DBL3_CLOSEFZ(IDF2,IRT)
                STATUS = LIB$RENAME_FILE(RENFILE(1:CLEN7),
     &                FAILFILE(1:CLEN5))
              ENDIF
              CALL D0DBL3_FAIL(IOK)
              GOTO 998
            ENDIF
            IF(IDF2 .GT. 0) THEN
              LIR = D0DBL3_CLOSEFZ(IDF2,IRT)
              IF(.NOT. LIR .OR. IRT .NE. 0) THEN
                PRINT *, 'Server- Closing of FZ file failed.'
                PRINT *, 'Server- File renamed'
                STATUS = LIB$RENAME_FILE(RENFILE(1:CLEN7),
     &              FAILFILE(1:CLEN5))
                IOK = -1
                CALL D0DBL3_FAIL(IOK)
                GOTO 998
              ENDIF
            ENDIF
C
          ELSE                                 ! Offline & Journal
            CALL LIB$DATE_TIME(DB_DATETIME)
            PRINT *, 'Server- update of database via journal file
     &            starts at :', DB_DATETIME
            PATH = ' '
            CALL LIB$DATE_TIME(DB_DATETIME)
            CALL D0DB_UPDATE(PATH,FOUND_FILE(1:CLEN),'JAI',OK)
            UPDCALL = .TRUE.
            IF(.NOT. OK) THEN
              GOTO 101
            ENDIF
          ENDIF
C
  101     CONTINUE
          IF(IDF .GT. 0) LIR = D0DBL3_CLOSEFZ(IDF,IRT)
C
C - Things to do after a successful update:
C
          IF(OK .OR. .NOT. UPDCALL) THEN
            PRINT *, 'Server- successful update at ',
     &                  DB_DATETIME
            PRINT *,
     &        'Server- Number of files in this FZ file was ',IFLG
C&IF VAXVMS
            IF(XTOA .OR. COMP) THEN    !Mode change or compression required
              STATUS = LIB$DELETE_FILE(TODOFILE(1:CLEN4))
              IF(STATUS) THEN
                PRINT *, 'Server- FILE: '
     &              ,TODOFILE(1:CLEN4), ' was deleted.'
              ELSE
                PRINT *, 'Server- Deletion of updated file ',
     &              FOUND_FILE(1:CLEN), 'failed. Will terminate'
                IOK = -1
                CALL D0DBL3_FAIL(IOK)
                GOTO 998
              ENDIF
            ELSE
              STATUS =
     &            LIB$RENAME_FILE(TODOFILE(1:CLEN4),RENFILE(1:CLEN7))
              IF(STATUS) THEN
                PRINT *, 'Server- FILE: '
     &              ,TODOFILE(1:CLEN4), ' was renamed to '
     &              ,RENFILE(1:CLEN7)
              ELSE
                PRINT *, 'Server- Renaming updated file ',
     &              FOUND_FILE(1:CLEN), 'failed. Will terminate'
                IOK = -1
                CALL D0DBL3_FAIL(IOK)
                GOTO 998
              ENDIF
            ENDIF
C
C - On Unix delete FZ after update
C
C&ELSE
C&           STATUS = LIB$DELETE_FILE(TODOFILE(1:CLEN4))
C&            IF(STATUS) THEN
C&              PRINT *, 'Server- FILE: '
C&     &            ,TODOFILE(1:CLEN4), ' was deleted.'
C&            ELSE
C&              PRINT *, 'Server- Deletion of updated file ',
C&     &            FOUND_FILE(1:CLEN), 'failed. Will terminate'
C&              IOK = -1
C&              CALL D0DBL3_FAIL(IOK)
C&              GOTO 998
C&            ENDIF
C&ENDIF
          ELSE     ! Exit with error if update fails
            PRINT *,
     &        'Server- D0DBL3_UPDATE failed for subfile No. ', IFLG
            PRINT *, 'Update did not succeed for',
     &            ' file ', TODOFILE(1:CLEN4)
            IOK = -1
            IF(IABS(IQUEST(1)) .EQ. 30) IOK = 0
            CALL D0DBL3_FAIL(IOK)
            GOTO 998
          ENDIF
C
          CALL D0DBL3_READPAR(IER)
          IF(IER .NE. 0) THEN
            PRINT *, 'Server- Problem in reading parameter file in',
     &          ' D0DBL3_READPAR. Will terminate.'
            CALL D0DBL3_FAIL(IOK)
            GOTO 998
          ENDIF
          IF(ISTOP .GT. 0) GOTO 250
C
C - After each FZ update check if should close & open + count updated files.
C
          IF ( NCLOS .GT. 0 .AND. MOD(NF,NCLOS) .EQ. 0 ) THEN
            CLOS = .TRUE.
            PRINT *, 'Server- ', NCLOS,
     &          ' files are done. will close and open database.'
            GOTO 200  !Close and reopen database. Go to main loop. Start fresh
          ENDIF
          GOTO 1
        ELSE
          STATUS = LIB$FIND_FILE_END(CONT)
          CONT = 0
          IF(NF .GT. 0) THEN
            PRINT *, 'Server- Number of update files done = ', NF
          ENDIF
          NFILES = NFILES + NF
          NF = 0
        ENDIF      ! End of FZ search loop
C
        CALL D0DBL3_READPAR(IER)
        IF(IER .NE. 0) THEN
          PRINT *, 'Server- Problem in reading parameter file in',
     &        ' D0DBL3_READPAR. Will terminate.'
          CALL D0DBL3_FAIL(IOK)
          GOTO 998
        ENDIF
  250   CONTINUE
        IF(ISTOP .GT. 0) THEN
          CALL LIB$DATE_TIME(DB_DATETIME)
          PRINT *,'Server- Stop requested at ',DB_DATETIME
          PRINT *,'Will terminate.'
C&IF VAXVMS
          IF(ISHIP .NE. -1) THEN
            STATUS = LIB$SPAWN (,'STOPED_COM'
     &                        ,'STOPED_LOG'
     &                        ,CLI$M_NOWAIT
     &                        ,SNAME//'_STOPED_PROC')
          ENDIF
C
C&ELSE
C&            CALL SYSTEM(CMAIL1)
C&ENDIF
          GOTO 998
        ENDIF
        NW = NW + 1
        NWT = NWT + 1
        IF(NWT .EQ. 1 .OR. NW .EQ. NWP) THEN
          CALL LIB$DATE_TIME(DB_DATETIME)
          IF(NWT .EQ. 1) THEN
            PRINT *,'Server- ',NWT,'st sleep on ',DB_DATETIME
          ELSE
            PRINT *,'Server- ',NWT,'th sleep on ',DB_DATETIME
          ENDIF
        ENDIF
C&IF VAXVMS
        CALL D0DBL3_HEARTBEAT
C&ELSE
C&
C&ENDIF
        CALL LIB$WAIT(TSEC)
        IF(NWT .EQ. 1 .OR. NW .EQ. NWP) THEN
          IF(NW .NE. 1) NW = 0
          CALL LIB$DATE_TIME(DB_DATETIME)
          IF(NWT .EQ. 1) THEN
            PRINT *,'Server- ',NWT,'st wakeup on ',DB_DATETIME
          ELSE
            PRINT *,'Server- ',NWT,'th wakeup on ',DB_DATETIME
          ENDIF
        ENDIF
        TDELT = TDELT + TSEC
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
        ITIM = NDAY*24*60*60 + NHOUR*60*60 + NMINT*60 + NSEC
        DELT = FLOAT(ITIM) + FLOAT(NDSEC)/100.
C
C - Send updates to offline
C
        IF(FIRST_SEND .OR. TDELT .GT. DELT) THEN
          TDELT = 0.0
C
  300     CONTINUE
C&IF VAXVMS
          IF(FJOFF .OR. NFRSH .NE. 1) THEN
            CONT = 0
            IF(FIRST_SEND) THEN
              STATUS = LIB$FIND_FILE(
     &            TODO_AREA//SC//'SEQ*.D0DBL3FZUPDATED',FFILE,CONT)
            ELSE
              STATUS = LIB$FIND_FILE(RENFILE(1:CLEN7),FFILE,CONT)
            ENDIF
            IF(STATUS) THEN
              IF(FJOFF .OR. NFRSH .NE. 1) THEN
                FJOFF = .FALSE.
                CALL LIB$DATE_TIME(DB_DATETIME)
                IF(ISHIP .GT. 0) THEN
                  PRINT *,
     &                'Server- Transfer of FZ-Journal files starts at ',
     &                DB_DATETIME
                  IF(NFRSH .EQ. 1) THEN
                    MSND = 'J'
                  ELSE
                    MSND = 'F'
                  ENDIF
                ELSE
                  PRINT *,
     &'Server- Transfer of FZ-Journal files not requested.',
     &' Copying and/or renaming starts at ', DB_DATETIME
                  IF(NFRSH .NE. 1) THEN
                    MSND = 'C'
                  ELSE
                    MSND = 'N'
                  ENDIF
                ENDIF
                COMMAND = '@'//'TOFFLN_COM'//' '//SNAME//' '//MSND
C
                IF(ISHIP .NE. -1) THEN
                  IK = MOD(IK,9) + 1
                  TOFFLPR = SNAME//'_TOFF_'//CHRC(IK)
                  CL = LEN(COMMAND)
                  STATUS = LIB$SPAWN(COMMAND(1:CL)
     &                     ,,'TOFFLN_LOG'
     &                     ,,TOFFLPR)
                  IF(.NOT.STATUS) THEN
                    CALL LIB$DATE_TIME(DB_DATETIME)
                    PRINT *,
     &                  'Server- Transfer of Journal files failed at ',
     &                  DB_DATETIME
                    CALL LIB$STOP(%VAL(STATUS))
                  ELSE
                    CALL LIB$DATE_TIME(DB_DATETIME)
                    PRINT *,'Server- Transfer completed at ',
     &                  DB_DATETIME
                  ENDIF
                ELSEIF(MSND(1:1) .EQ. 'C') THEN
                  RNFILE = ' '
                  CLT = TRULEN(FFILE)
                  MRK1 = INDEX(FFILE, '.')
                  MRK = INDEX(RNFILE, ']')
                  IF(MRK .EQ. 0) MRK = INDEX(FOUND_FILE, ':')
                  RNFILE = DONE_AREA(1:CL2)//':'
     &//FFILE(MRK+1:MRK1)//'D0DBL3FZ'
C                  STATUS = LIB$RENAME_FILE(FFILE(1:CLT),RNFILE)
                  STATUS = LIB$RENAME_FILE(
     &TODO_AREA//':*.D0DBL3FZUPDATED',DONE_AREA//':*.D0DBL3FZ')
                  PRINT *, 'Server- Files copied to done area.'
                ENDIF
C
              ENDIF
            ELSE
              PRINT *,'Server- No FZ-journal files found at ',
     &              DB_DATETIME
            ENDIF
            STATUS = LIB$FIND_FILE_END(CONT)
          ENDIF
C
          FIRST_SEND = .FALSE.
C&ELSE
C&
C&ENDIF
C
          CALL D0DBL3_READPAR(IER)
          IF(IER .NE. 0) THEN
            PRINT *, 'Server- Problem in reading parameter file in',
     &          ' D0DBL3_READPAR. Will terminate.'
            CALL D0DBL3_FAIL(IOK)
            GOTO 998
          ENDIF
          IF(ISTOP .GT. 0) THEN
            CALL LIB$DATE_TIME(DB_DATETIME)
            PRINT *,'Server- Stop requested at ',DB_DATETIME
            PRINT *,'Will terminate.'
C&IF VAXVMS
            IF(ISHIP .NE. -1) THEN
              STATUS = LIB$SPAWN (,'STOPED_COM'
     &                        ,'STOPED_LOG'
     &                        ,CLI$M_NOWAIT
     &                        ,SNAME//'_STOPED_PROC')
            ENDIF
C
C&ELSE
C&            CALL SYSTEM(CMAIL1)
C&ENDIF
            GOTO 998
          ENDIF
C
        ENDIF
      ENDDO     !End-loop for server main loop
C
  998 CONTINUE
C&IF VAXVMS
      CALL D0DBL3_HEARTBEAT_END
C&ELSE
C&
C&ENDIF
C
      PRINT *,'Server- Grand total number of files processed was: ',
     &           NFILES
C
      IF(DBINI) THEN
        CALL D0DBL3_FINISH
      ENDIF
C----------------------------------------------------------------------
  999 CONTINUE
      IF(IABS(IQUEST(1)) .GT. 0) THEN
        PRINT *, 'Server- ',
     &'*** See ',SRVRNM(1:2), '_DBERROR.LOG for more info on error.'
      ENDIF
      STOP
      END
