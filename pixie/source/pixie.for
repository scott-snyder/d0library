      PROGRAM PIXIE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Super-simple program to run PXDISPLAY. This
C-   merely reads events from logical file PIXIE$DATAFILE. The number of
C-   events to read should be supplied in logical PIXIE$COUNT as follows
C-
C-      DEFINE PIXIE$COUNT      "100"
C-
C-   This program has only 2 hooks:
C-
C-      PXINIT          -       Initialize PIXIE packages
C-      PXEXEC          -       Execute PIXIE packages
C-
C-   Specify exchange ('X') or native mode (' ') with the logical
C-   PIXIE$MODE
C-
C-
C-   Controls: PIXIE$DATAFILE
C-             PIXIE$COUNT
C-             PIXIE$MODE       ' ' - Native   mode
C-                              'X' - Exchange mode
C-
C-   Created  23-SEP-1990   Harrison B. Prosper
C-   Updated   1-OCT-1990   Harrison B. Prosper
C-      Fixed exit condition
C-   Updated  10-DEC-1990   Nobu. Oshima( Fill PTCAEP array )
C-   Updated  18-AUG-1991   Nobu. Oshima( Add CAEHFL/CATEFL for STA )
C-   Updated  14-MAY-1992   Lupe Howell  Fix machine code for nonvax
C-   Updated  15-MAY-1992   Nobuaki Oshima
C-      Back to PXINIT
C-   Updated   8-JUN-1992   Krzysztof L. Genser  Enable usage of FATMEN generic
C-      names
C-   Updated  22-SEP-1992   Nobuaki Oshima and Jasbir Singh
C-      Enable selection of particular Run and Event Number via EVTIN
C-   Updated  15-DEC-1992   Vipin Bhatnagar( Add Write Event function )
C-   Updated  13-JAN-1993   Vipin Bhatnagar
C-    Check to write an unscanned event or the scanned events (in the file
C-   SCAN_EVENT.DAT calling PX_EVWRIT)
C-   Modified  5-FEB-1993   Vipin Bhatnagar
C-      Enable to display events picked by PICK_EVENT util. by just
C-   specifying the DIR path, STREAM type, RUN & EVENT #
C-   Updated  25-FEB-1993   Vipin Bhatnagar
C-     Added Control PIXIE$DIRNAME for Single file case
C-   Modified 17-MAY-1993   Vipin Bhatnagar
C-     For Scanned output file,for both the single event & merge events
C-     case, the original filename is kept
C-   Updated  24-MAY-1993   Lupe Howell   Fix pick option for SGI, cleanup
C-   Updated  12-JUL-1993   Vipin Bhatnagar  Autodisplay of the first file
C-     for the single event case & cleanup of redundent code
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:FATCOM.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$INC:PXCOMK.INC'
C
      INTEGER PIXIE_ID
      PARAMETER( PIXIE_ID  = 1919 )
      INTEGER BIG_COUNT
      PARAMETER( BIG_COUNT = 1000000 )
C
      INTEGER I,J,K,GZCAEP,GZCAEH,RUNNO,LAST_RUN,RUN,IUW,IUS,N
      INTEGER EVNUM,LAST_EVN,EVONUM,INDEX,IBAKA, LANLS
      INTEGER TRNLNM,STATUS,LENGTH,LOOP,COUNT,LUN,IER,IOS
      INTEGER ISTAT,LIB$FIND_FILE,LIB$FIND_FILE_END,CONTXT
      REAL    VALUE

      INTEGER IDR,NDR,LDR,IFN,NFN,DLN,IFD,NFD
      INTEGER LFN,IDF,NDF,LDF,LST,FMODE,LFD
      INTEGER NM,MAXITM,ITM,ITMOLD,IUT,ITRY,IK,NC,KF,IV
      INTEGER IXS,NXS,LXS,ML,PKCNT
      INTEGER SPN_LIB,LIB$SPAWN
      INTEGER IVERSION,IPASS
C
      LOGICAL ONOFF,OPEN
      LOGICAL OK,EVENT,EOF,EOD,FLGVAL,FINE,FOUND,ACTIVE
      LOGICAL DO_MUODIS,DO_ZTRAKSDIS,SINGLE_FILE,NFLLST
      LOGICAL ZTRINI, CALOR_INI, CHTINI, MUONLY_INI, PMEVT_INI
      LOGICAL FIRST,ODD,PICK_NEXT,SECOND,MERGE_FILE,FTFLG
      LOGICAL UDST_TO_DST
C
      CHARACTER*160 DATAFILE,STRING,DATFILNM,STRINGDF,SCANFILE
      CHARACTER*132 DIRNAME,FILENAME,STRINGD,STRINGFN,OLDDIR,TMPDIR
      CHARACTER*60 MMSSG,FILENEXT(512),STRINGFL(512)
      CHARACTER*60 OLDFILE,STRINGI,STRINGO
      CHARACTER*45 STRVER
      CHARACTER*26 MODE,FAILMSG,DEFMSG
      CHARACTER*24 EXTS,EXT1,EXTSV
      CHARACTER*5  BLNK
      CHARACTER*3  STREAM
      CHARACTER    ANS,NANS,V,SOPT
C
      PARAMETER (MMSSG = '  No such file exists ')
      PARAMETER (FAILMSG = ' Sorry,try later ')
C
      DATA OPEN /.TRUE./
      DATA FIRST,SECOND,FTFLG /.TRUE.,.TRUE.,.FALSE./
      DATA MAXITM,ITMOLD /512,512/
      DATA N,ISTAT,CONTXT,PKCNT /10,1,0,0/
      DATA EXT1,ITRY,BLNK /'.*',0,'.....'/
C----------------------------------------------------------------------
C
C ****  Defining the ODD function
C
      ODD(NM) = 2*INT (NM/2).NE.NM
C
C ****  Setup zebra
C
      CALL MZEBRA(0)                    ! INITIALIZE ZEBRA
      CALL INZCOM(2)                    ! Initialize /ZEBCOM/
      CALL INZLNK                       ! Initialize ZLINKA
      CALL INZSTP                       ! Initialize /ZEBSTP/
      CALL INPAWC                       ! Initialize HBOOK4
C
C ****  Read in RCP control file/ Turn on or off
C
      CALL INRCP('PIXIE_RCP',STATUS)
      IF ( STATUS .NE. 0 ) THEN
        CALL ERRMSG('BAD_READ','INRCP',' Cannot read PIXIE_RCP','I')
        GOTO 999
      ENDIF
C
      CALL EZPICK('PIXIE_RCP')
      DO_MUODIS = .FALSE.
      DO_ZTRAKSDIS = .FALSE.
      CALL EZGET('PIXIE$INPTYPE',FMODE,STATUS)
      IF (FMODE.EQ.1) THEN
        SINGLE_FILE = .TRUE.
      ELSE
        MERGE_FILE  = .TRUE.
      ENDIF
      CALL EZGET('DO_MUODIS',DO_MUODIS,STATUS)
      CALL EZGET('DO_ZTRAKSDIS',DO_ZTRAKSDIS,STATUS)
      CALL EZRSET
C
      CALL PBDINI                       ! Initialize PBD packages
C
C ****  Do global initialization
C
      FINE= .TRUE.
      IF ( DO_ZTRAKSDIS ) THEN
        OK = ZTRINI()
        IF ( .NOT. OK ) THEN
          CALL ERRMSG('BAD_INIT','ZTRINI',' Failed','I')
        ENDIF
        FINE = FINE .AND. OK
      ENDIF
C
      OK = CALOR_INI()
      IF ( .NOT. OK ) THEN
        CALL ERRMSG('BAD_INIT','CALOR_INI',' Failed','I')
      ENDIF
      FINE = FINE .AND. OK
C
      OK = CHTINI()
      IF ( .NOT. OK ) THEN
        CALL ERRMSG('BAD_INIT','CHTINI',' Failed','I')
      ENDIF
      FINE = FINE .AND. OK
C
      IF ( DO_MUODIS ) THEN
        OK = MUONLY_INI()
        IF ( .NOT. OK ) THEN
          CALL ERRMSG('BAD_INIT','MUONLY_INI',' Failed','I')
        ENDIF
        FINE = FINE .AND. OK
      ENDIF
C
      IF ( .NOT. FINE ) THEN
        GOTO 999
      ENDIF
C
C ****  Turn ON/OFF PIXIE packages
C
      IF ( STATUS .EQ. 0 ) THEN
        CALL SET_SWITCHES ('PIXIE_RCP','DO_',' ')
      ELSE
        CALL ERRMSG
     &    ('PIXIE_RCP',
     &    'INRCP',' Please define the logical PIXIE_RCP','I')
        GOTO 999
      ENDIF
C
C ****  HOOK to initialize PIXIE packages
C
      CALL PXINIT
C-
      CALL MENDEF
C
C
C ****  Pick Control RCP bank
C
      CALL EZPICK('PIXIE_RCP')
C
C ****  Get data from logical names or from RCP bank if the logical
C ****  names are NOT defined.
C
C ****  Get datafile name
C
      IF ( MERGE_FILE ) THEN
C&IF VAXVMS
        STATUS = TRNLNM('PIXIE$DATAFILE',DATAFILE,LENGTH)
        IF ( .NOT. STATUS ) THEN
          CALL EZGETS('PIXIE$DATAFILE',1,DATAFILE,LENGTH,STATUS)
        ENDIF
        DATAFILE = DATAFILE(1:LENGTH)
C&ELSE
C&      DATAFILE = 'pixie_datafile'
C&      LENGTH   = 14
C&ENDIF
C
      ELSEIF ( SINGLE_FILE )THEN


C&IF VAXVMS
        CALL EZGETS('PIXIE$DIRNAME',1,DIRNAME,DLN,STATUS)
        DIRNAME = DIRNAME(1:DLN)
        TMPDIR = DIRNAME
        CALL INTMSG(' Default Dir is '//DIRNAME(1:DLN))
        STRINGD = DIRNAME
        CALL SWORDS(STRINGD,IDR,NDR,LDR)
C&ELSE
C&         STRINGD = '$d0fs_pick/'
C&         DIRNAME = STRINGD
C&         DLN     = 11
C&         LDR     = 11
C&         IDR     = 1
C&         NDR     = 11
C&ENDIF
C
C***Auto displaying of first file in the directed area
C
        IF (OPEN) THEN
          OPEN = .FALSE.
          CALL GETPAR (1,' Enter Stream Name>','C',STREAM)
          CALL UPCASE (STREAM,STREAM1)
          CALL PIKNEXFL(DIRNAME,MAXITM,ITM,FILENEXT)
          STRINGO = FILENEXT(1)
          IF(STRINGO(1:5).EQ.BLNK) GOTO 1001
          CALL SWORDS(STRINGO,IFN,NFN,LFN)
          FILENAME = STRINGO(IFN:NFN)
          CALL SWORDS(FILENAME,IFN,NFN,LFN)
          STRINGDF=DATFILNM
          CALL SWORDS(STRINGDF,IDF,NDF,LDF)
          DATFILNM=DIRNAME(1:LDR)//FILENAME(1:LFN)
          GOTO 777
        ENDIF
C
  777   IF (FILENAME(1:4) .EQ. '   ') GOTO 1001
C
C ****  Search for the file using the extension .*SC0(9-1)
C
        CALL PX_FIND_FILE_EXT(FILENAME,DIRNAME,LFN,LDR,'SC0',FOUND)
C
C ****  If the file with the extension .*SC0(9-1) is NOT found search for
C ****  the file with .* extension
C
        IF ( .NOT. FOUND ) THEN
          EXT1 = '.* '
          DATFILNM=DIRNAME(1:LDR)//FILENAME(1:LFN)//EXT1
          STRINGDF=DATFILNM
          CALL SWORDS(STRINGDF,IDF,NDF,LDF)
          CONTXT = 0
          DATAFILE = ' '
          ISTAT = LIB$FIND_FILE
     &          (DATFILNM(IDF:NDF),DATAFILE,CONTXT)
C
C ****  File end
C
          ISTAT  = LIB$FIND_FILE_END(CONTXT)
          EVTCNT = -1
        ENDIF
C
      ENDIF
C
C ****  Get number of events to read
C
  100 CONTINUE
C
C&IF VAXVMS
      STATUS = TRNLNM('PIXIE$COUNT',STRING,LENGTH)
      IF ( STATUS ) THEN
        COUNT = VALUE(STRING(1:LENGTH),I,J,K)
      ELSE
        CALL EZGET('PIXIE$COUNT',COUNT,STATUS)
      ENDIF
C&ELSE
C&      COUNT = 0
C&ENDIF
C
      IF ( COUNT .LE. 0 ) THEN
        COUNT = BIG_COUNT
      ENDIF
C
C **** If the data file is 'normal' (non-generic)
C
      IF ( DATAFILE(1:9) .NE. D0TRNK  ) THEN
C
C ****  Get Mode of the input file
C
C&IF VAXVMS
        STATUS = TRNLNM('PIXIE$MODE',STRING,LENGTH)
        IF ( .NOT. STATUS ) THEN
          CALL EZGETS('PIXIE$MODE',1,STRING,LENGTH,STATUS)
        ENDIF
C&ELSE
C&      STRING = 'X'
C&      LENGTH = 1
C&ENDIF
C
        MODE = STRING(1:LENGTH)
C
C ****************************************************************
C ****  Open 'normal' input file and declare its existence to ZEBRA
C *****************************************************************
C
        CALL EVOPIN(DATAFILE,MODE,LUN,OK)
      ELSE
C
C ****  Generic name
C ****  Get available unit number
C
        CALL GTUNIT(87,LUN,IER)
        IF(IER.NE.0) THEN
          STRING = ' Unable to open data file: '//DATAFILE
          CALL ERRMSG('BAD_GTUNIT','EVOPIN',STRING,'I')
          OK = .FALSE.
          GOTO 999
        ENDIF
C
C ****  Use D0OPEN to open the file via FATMEN
C ****  options for FMOPEN after / (the slash)  Read,
C ****  V update file size after staging to disk, F call FZFILE
C
        CALL D0OPEN(LUN,DATAFILE,'/RVF',OK)
      ENDIF
C
C ****  If data can NOT be open EXIT after 4 times asking...
C
      IF ( OK ) THEN
        IBAKA = 0
        OLDFILE = STRINGO
      ELSE
        IBAKA = IBAKA + 1
        IF (IBAKA .LE. 7) THEN
          CALL INTMSG(' %TYPO-W-NOFILES, no files found !!!')
          GO TO 1001
        ELSE
          STRING = ' Unable to open data file: '//DATAFILE
          CALL ERRMSG('BAD_OPEN','EVOPIN',STRING,'I')
          GOTO 999
        ENDIF
      ENDIF
C
C ****  Reset bank
C
      CALL EZRSET
C
C *** SET ZERO PTCAEP ARRAY
C
      PTZFLG=.FALSE.
      CALL CPTCAZ
C
C ****  Loop over events in file
C
      IF ( SINGLE_FILE )THEN
        PICK_FILE = .TRUE.
      ENDIF
      LAST_RUN = 0
      LOOP    = 0
      DO WHILE ( LOOP .LT. COUNT )
        LOOP = LOOP + 1
C
C ****  Read event
C
        IOS = 0
        CALL EVTIN_HEADER(.TRUE.)         ! For GO TO EVENT Menu
        CALL EVTIN (LUN,IOS)
        CALL FLGSET('GOTO_EVENT',.FALSE.)
        EVENT = IOS .EQ. 0                ! Set Event flag
        EOF   = IOS .EQ. 3                ! Set End-of-File flag
        EOD   = IOS .GE. 4                ! Set End-of-Data flag
C-
C--- Check for Micro-DST
C-
        LANLS = LQ(LHEAD-11)
        IF (LANLS.GT.0) THEN   
          IF (.NOT. UDST_TO_DST() ) GOTO 1001
        ENDIF
C
C ****  Check for run number change
C
        RUN   = RUNNO()
        EVNUM = EVONUM()
C--- This is for the stupid MUON Initialization !!!
        IF (RUN.NE.LAST_RUN .OR. LOOP.EQ.2 ) THEN
          IF ( DO_MUODIS ) THEN
            OK = PMEVT_INI()
          ENDIF
C--- This is for the stupid MUON Initialization !!!
          LAST_RUN = RUN
          LAST_EVN = EVNUM
        ENDIF
C
C ****  Display event
C
        IF ( EVENT ) THEN
C
C *** FILL THE PTCAEP ARRAY
C
          IF (GZCAEP() .GT. 0) THEN
            CALL CPTCAF
          ENDIF
C
C *** FILL CAEH BANK for CAL. Display using STA file
C
          IF ( (GZCAEP().GT.0) .AND. (GZCAEH().LE.0) ) THEN
            CALL CAEHFL
            CALL CATEFL
          ENDIF
C
C *** Execute the Event Display
C
          CALL RECO_VERSION(IVERSION,IPASS)
          WRITE(STRVER,1111) IVERSION,IPASS
 1111     FORMAT(' >>> D0RECO Version of your Data is',I3,'.'
     &           ,I2,' <<<')
          CALL INTMSG(STRVER)
C-
          CALL PXDISPLAY
C
C ****
C
          IF((.NOT.FLGVAL('GOTO_EVENT')).AND.(PICK_FILE)) THEN
            PICK_NEXT = .TRUE.
          ELSE
            PICK_NEXT = .FALSE.
          ENDIF
C
C *** Constructing the filelist for NEXT EVENT single event case
C
          IF ( PICK_FILE ) THEN
            IF (STRINGI(1:3).NE.STREAM1(1:3)) THEN
              DO I=1,ITMOLD
                STRINGFL(I) = ' '
              ENDDO
              CALL PIKNEXFL(DIRNAME,MAXITM,ITM,FILENEXT)
              ITMOLD = ITM
              DO LST = 1,ITM
                STRINGFL(LST) = FILENEXT(LST)
              ENDDO
              NFLLST =.TRUE.
            ENDIF
          ENDIF
C
C *** WRITE EVENT
C
          IF (FLGVAL('PX_WRITE_EVENT')) THEN
            CALL PX_EVWRIT(IUW,FTFLG,DATAFILE,EXTS)
          ENDIF
C
C *** Writing a scanned event
C
          IF (FLGVAL('PX_WRITE_SCAN')) THEN
            IF (PICK_NEXT.OR.PICK_FILE) THEN
              FTFLG = .TRUE.
            ENDIF
C
C *** Truncating the datafile name
C
            CALL SWORDS(DATAFILE,IFD,NFD,LFD)
            DO IK = IFD,NFD
              IF (DATAFILE(IK:IK).EQ.']') THEN
                DATAFILE = DATAFILE(IK+1:LFD)
              ENDIF
            END DO
            CALL PX_EVWRIT(IUS,FTFLG,DATAFILE,EXTS)
          ENDIF
C
C *** For the single event/file case
C
          IF ( .NOT. FLGVAL('CALL_PXMAIN') ) THEN
            CALL CPTCAZ
            LOOP = COUNT                  ! Exit loop merge case
            IF (PICK_FILE) THEN           !   FOR EXITING
              PICK_FILE = .FALSE.         !
              GO TO 888                   ! Exit loop single
            ENDIF
C
C ****  If PICK_FILE AND FLAG CALL_PXMAIN ON Zero PTCAEP
C ****  pointers using information in CAEP bank ans exit the loop
C
          ELSEIF ( PICK_FILE ) THEN
            CALL CPTCAZ
            LOOP = COUNT                  ! Exit loop
          ELSEIF ( MERGE_FILE ) THEN
            CALL CPTCAZ
          ENDIF
C
C ****  If EOF or end of data exit loop
C
        ELSEIF ( EOF .OR. EOD ) THEN
          LOOP = COUNT                    ! Exit loop
        ENDIF
      ENDDO
C
C *** Close Scan_event file (merge case)
C
      IF ( MERGE_FILE )THEN
        IF ( IUS .NE. 0) THEN
          CALL FZENDO(IUS,'T')
          CLOSE(IUS)
          CALL SWORDS(DATAFILE,IFD,NFD,LFD)
          DO NC = IFD,NFD
            IF (DATAFILE(NC:NC).EQ.';') THEN
              LFD = NC-1
            ENDIF
          END DO
          IF ((DATAFILE(LFD:LFD).EQ.'1'.OR.DATAFILE(LFD:LFD).EQ.'2').
     &        AND.( DATAFILE(LFD-3:LFD-3).EQ.'S' )) THEN
            LFD = LFD - 5
          ENDIF
          SCANFILE = DATAFILE(1:LFD)//EXTS
          CALL INTMSG(' FILE '//SCANFILE//'
     &        WRITTEN.')
        ENDIF
      ENDIF
C
C ****  Close datafile
C
  888 CALL FZENDI (LUN,'TU')
      CLOSE(UNIT=LUN,ERR=900)
C
C ****  Getting File name using pick mode
C
      IF ( PICK_FILE ) THEN
        IF (PICK_NEXT) THEN
C-
C *** Getting latest file name (NEXT EVENT case)
C-
C.N.O     CALL PFILE(RUN,EVNUM,STREAM1,OLDFILE)
          IF ( NFLLST ) THEN
            NFLLST = .FALSE.
            LST = 1
          ELSE
            LST = LST + 1
          ENDIF
          IF (LST.GT.ITM) THEN
            CALL INTMSG(MMSSG)
            CALL GETPAR(1,'WANT TO [Q]UIT OR CHANGE [S]TREAM >','C',
     &        SOPT)
            IF (SOPT(1:1).EQ.'Q'.OR.SOPT(1:1).EQ.'Q') THEN
              GOTO 1001
            ELSEIF (SOPT(1:1).EQ.'S'.OR.SOPT(1:1).EQ.'S') THEN
              PICK_FILE = .FALSE.
              PKCNT     = 1
              GOTO 222
            ENDIF
          ENDIF
          DO ML = LST,ITM
            IF (OLDFILE.NE.STRINGFL(ML)) GOTO 405
          ENDDO
  405     CONTINUE
          STRINGI = STRINGFL(ML)
          OLDFILE = STRINGI
          LST = ML
C
C *** If filename returned is empty
C
          IF(STRINGI(1:5).EQ.BLNK) GOTO 1001
          CALL SWORDS(STRINGI,IFN,NFN,LFN)
          FILENAME = STRINGI(IFN:NFN)
          CALL SWORDS(FILENAME,IFN,NFN,LFN)
          STRINGDF=DATFILNM
          CALL SWORDS(STRINGDF,IDF,NDF,LDF)
          DATFILNM=DIRNAME(1:LDR)//FILENAME(1:LFN)
C
C ****  Search for the file using the extension .*SC0(9-1)
C
          CALL PX_FIND_FILE_EXT(FILENAME,DIRNAME,LFN,LDR,'SC0',FOUND)
C
C *** If no scanned version found
C
          IF ( .NOT. FOUND ) THEN
            EXT1 = '.*'
            DATFILNM=DIRNAME(1:LDR)//FILENAME(1:LFN)//EXT1
            STRINGDF=DATFILNM
            CALL SWORDS(STRINGDF,IDF,NDF,LDF)
            CONTXT = 0
            DATAFILE = ' '
            ISTAT = LIB$FIND_FILE(DATFILNM(IDF:NDF),DATAFILE,CONTXT)
C
C ****  If the file is NOT found let user try again
C
            IF (.NOT.ODD(ISTAT)) THEN
              CALL INTMSG(MMSSG)
              CALL GETPAR(1,'WANT TO [Q]UIT OR CHANGE [S]TREAM >','C',
     &          SOPT)
              IF (SOPT(1:1).EQ.'Q'.OR.SOPT(1:1).EQ.'Q') THEN
                GOTO 1001
              ELSEIF (SOPT(1:1).EQ.'S'.OR.SOPT(1:1).EQ.'S') THEN
                PICK_FILE = .FALSE.
                IF (PKCNT.EQ.0) PKCNT = 1
                GOTO 222
              ENDIF
            ENDIF
C
C ****  File end
C
            ISTAT = LIB$FIND_FILE_END(CONTXT)
          ENDIF
C
C *** Goto event case
C
        ELSE
  222     CALL PIKFILE(FILENAME,LFN)
          PKCNT = PKCNT + 1
C
C *** If filename returned is blank Quit or enter file name
C
          IF (FILENAME(4:8) .EQ. '     ') GOTO 1001
C
C ****  Search for the file using the extension .*SC0(9-1)
C
          CALL PX_FIND_FILE_EXT(FILENAME,DIRNAME,LFN,LDR,'SC0',FOUND)
C
C *** If no scan version exists
C
          IF ( .NOT. FOUND ) THEN
            EXT1 = '.* '
            DATFILNM=DIRNAME(1:LDR)//FILENAME(1:LFN)//EXT1
            STRINGDF=DATFILNM
            CALL SWORDS(STRINGDF,IDF,NDF,LDF)
            DATFILNM=DATFILNM(1:LDF)
            ISTAT = LIB$FIND_FILE(DATFILNM,DATAFILE,CONTXT)
            IF (.NOT.ODD(ISTAT)) THEN
              CALL INTMSG(MMSSG)
              IF (PKCNT.GT.3) GOTO 1001
              PICK_FILE =.FALSE.
              GOTO 222
            ENDIF
            ISTAT = LIB$FIND_FILE_END(CONTXT)
          ENDIF
          PICK_FILE = .FALSE.
        ENDIF
        IF (DATAFILE(1:4).EQ.'    '.OR.DATAFILE(1:5).EQ.BLNK) GOTO 222
        CALL FLGSET('CALL_PXMAIN',.TRUE.)
        EVTCNT = -1
        GO TO 100
      ENDIF
C
C *** Get another File or Quit?
C
 1001 CONTINUE
      CALL INTMSG(
     &  '  PIXIE - ENTER INPUT FILE NAME OR RETURN FOR QUIT ?')
      CALL GETPAR (1,' PIXIE >','C',DATAFILE)
      IF (DATAFILE(1:1) .EQ. ' ') THEN
        GOTO 999
      ELSE
        CALL FLGSET('CALL_PXMAIN',.TRUE.)
        MERGE_FILE = .TRUE.
        CALL ERASE
        GOTO 100
      ENDIF
  900 CONTINUE
      CALL ERRMSG ('PIXIE','CLOSE',' ERROR CLOSING DATAFILE','I')
  999 CONTINUE
C-
C--- Special for TOPSCAN project...
C-
C&IF VAXVMS
      IF ( TMPDIR(1:19) .EQ. 'EVT_SCAN$HROOT:[TOP' ) THEN
        ONOFF = .FALSE.
C
C***turn off broadcast trapping
C
        CALL BROADC(ONOFF)
        SPN_LIB=LIB$SPAWN('@USR$ROOT4:[TOPSCAN.COM]MOVE_FILES.COM')
      ENDIF
C&ENDIF
C-
      END
