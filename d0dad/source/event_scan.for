      PROGRAM EVENT_SCAN
C-------------------------------------------------------------------------
C  Header scan Zebra file for trigger and filter information.
C
C  Author:   Herbert Greenlee
C  Date:     13-DEC-1994
C
C  Mods:     29-DEC-1994 JDH Change format of output files to more 
C              easily integrate with D0DAD's TEXT_STREAM mode.
C              Allow file names on command line.  
C            10-MAY-1995 JDH Make it work with compressed zebra files
C
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,J,K
C     
      INCLUDE  'D0$INC:zebcom.inc'
      INCLUDE  'D0$INC:quest.inc'
C     
      INTEGER  ILEN,IFUNIT,IXWIPE,IDADZB,IERR,IERR_RETURN
      INTEGER  NHMAX,NEVTOT,NEV
      PARAMETER(NHMAX=100)
      INTEGER  NHEAD,IUHEAD(NHMAX),ICNTXT
      LOGICAL  LFLOOP,OPENOK,LEXIST
      CHARACTER*128 INPUT_FILE,CINP,CFNAME
      CHARACTER CHOPT*8,TAG*5,BLANK*37
      CHARACTER CMDLIN*256
      INTEGER RUN, OLDRUN, EVENT
C- Trigger/filter "database"
      CHARACTER*32 CTRIG_RUN(0:31), CFILT_RUN(0:127)
C- Trigger/filter flags for this event
      LOGICAL NEED_TSUM
      INTEGER NTRIG, TRIG(32)
      INTEGER NFILT, FILT(128)
      CHARACTER*32 CTRIG(32), CFILT(128)
C- Trigger/filter database file
      INTEGER ITUNIT
      CHARACTER*255 TRIGFILE
C- Event file
      INTEGER IEUNIT
      CHARACTER*255 EVENTFILE
C- Zebra unit
      CHARACTER*(*) FDUMMY
C&IF VAXVMS
      PARAMETER(FDUMMY='NL:')
C&ELSE
C&      PARAMETER(FDUMMY='/dev/null')
C&ENDIF
C- Functions
      INTEGER LENOCC
      LOGICAL LIB$FIND_FILE
C-------------------------------------------------------------------------
C
C  Locals...
C
      IERR=0
      IERR_RETURN=0
      LFLOOP=.FALSE.
      NEVTOT=0
      IDADZB=21
      IFUNIT=22
      ITUNIT=23
      IEUNIT=24
      OLDRUN=0
      TRIGFILE = ' '
      EVENTFILE = ' '
      BLANK=' '
C
      INPUT_FILE = ' ' 
      CALL LIB$GET_FOREIGN(CMDLIN,%VAL(0),%VAL(0),NEVTOT)
      IF( LENOCC(CMDLIN).GT.0 ) INPUT_FILE=CMDLIN
      NEVTOT=0
C
C  Zebra...
C
      OPEN(3,FILE=FDUMMY,STATUS='UNKNOWN')
      CALL MZEBRA(0)
      CALL MZSTOR(IXCOM,'D0DADTST','Q',FENCE,LQ(1),LREF(1),ZSTOR(1),
     &   ZSTOR(40000),ENDZS)
      CALL MZDIV(IXCOM,IXDVR,'RUN DIV',100,40000,'L')
      IXMAIN=IXCOM+1
      IXWIPE=IXCOM+IXMAIN
C
C  Setup input file(s)
C
      IF( INPUT_FILE.EQ.' ' ) THEN
        WRITE(*,1010)
 1010   FORMAT('Enter input file name: ',$)
        READ(*,'(A)') INPUT_FILE
      ENDIF
      CINP=INPUT_FILE
      CALL CUTOL(CINP)
      IF( CINP.EQ.'file_names' ) THEN
        LFLOOP=.TRUE.
        CALL D0OPEN(IFUNIT,'file_names','IF',OPENOK)
        IF(.NOT.OPENOK)STOP 'Can''t open FILE_NAMES'
      ENDIF
C     
 20   CONTINUE
C
      IF( LFLOOP ) THEN
        IERR=0
        READ(IFUNIT,'(A)',END=999) INPUT_FILE
      ENDIF
C
C  Translate to true file name.
C
      CFNAME=INPUT_FILE
      IF( .NOT.LIB$FIND_FILE(CFNAME,INPUT_FILE,ICNTXT) ) THEN
        CALL LIB$FIND_FILE_END(ICNTXT)
        ICNTXT=0
        GOTO 903
      ENDIF
      CALL LIB$FIND_FILE_END(ICNTXT)
      ICNTXT=0
C
      NEV=0
C      WRITE(*,*) ' About to open: ',INPUT_FILE
      CALL D0OPEN(IDADZB, INPUT_FILE, 'IX', OPENOK)
      IF(.NOT.OPENOK)GOTO 903
      CALL XZRECL(ILEN,CHOPT)
      CALL FZFILE(IDADZB,ILEN,CHOPT)
      IF( IQUEST(1).NE.0 .AND. IQUEST(1).NE.1 ) THEN
        WRITE(*,8002) IQUEST(1)
 8002   FORMAT(' EVENT_SCAN: Error ',I4,'returned from FZFILE')
        IERR = -2
        GOTO 999
      ENDIF
C
CJDH      WRITE(*,1031) CFNAME(1:LENOCC(CFNAME))
 1031 FORMAT('  New File: ',A)
C
C     Scan the file...
C
 10   CONTINUE
      NHEAD=NHMAX
      CALL MZWIPE(IXWIPE)
      CALL FZIN(IDADZB,IXMAIN,0,0,'S',NHEAD,IUHEAD)
C
      IF( IQUEST(1).eq.-2 .OR. IQUEST(1).EQ.-3 ) THEN
        WRITE(*,8003) IQUEST(1)
 8003   FORMAT(' EVENT_SCAN: Error',I4,' from FZIN. Skip event.')
        GOTO 10
      ENDIF
      IF( IQUEST(1).EQ.0 ) THEN
        IF( NHEAD.LE.0 ) THEN
          WRITE(*,*) ' Error.  Header size=',nhead
          GOTO 10
        ENDIF
C-
C- Got a good event
C-
        NEV = NEV + 1
        RUN = IUHEAD(6)
        EVENT = IUHEAD(9)
C- See if run number has changed
        IF(RUN.NE.OLDRUN)THEN
          OLDRUN = RUN
C- Open new trigger and event files.
          IF(TRIGFILE.NE.' ')THEN
            DO I=0,31
              IF( LENOCC(CTRIG_RUN(I)).GT.0 ) THEN
                WRITE(ITUNIT,'(A37)') 'TRIG_'//CTRIG_RUN(I)
              ELSE 
                WRITE(ITUNIT,'(A37)') BLANK
              ENDIF
            ENDDO
            DO I=0,127
              IF( LENOCC(CFILT_RUN(I)).GT.0 ) THEN
                WRITE(ITUNIT,'(A37)') 'FILT_'//CFILT_RUN(I)
              ELSE 
                WRITE(ITUNIT,'(A37)') BLANK
              ENDIF
            ENDDO
            CALL D0CLOSE(ITUNIT, ' ', OPENOK)
          ENDIF
C-    Blank out trigger/filter names
          DO I = 0,31
            CTRIG_RUN(I) = ' '
          ENDDO
          DO I = 0,127
            CFILT_RUN(I) = ' '
          ENDDO
          WRITE(TRIGFILE,'(''trig_filt_'',I6.6,''.lis'')')RUN
          INQUIRE(FILE=TRIGFILE,EXIST=LEXIST)
          IF( LEXIST ) THEN
            WRITE(*,*) ' Reading in existing trigger file: '//
     >           TRIGFILE(1:LENOCC(TRIGFILE))
            CALL D0OPEN(ITUNIT, TRIGFILE, 'IFL', OPENOK)
            IF( OPENOK ) THEN
              READ(ITUNIT,'(A5,A32)')(TAG,CTRIG_RUN(I),I=0,31),
     &          (TAG,CFILT_RUN(I),I=0,127)
              CALL D0CLOSE(ITUNIT, ' ', OPENOK)
            ENDIF
          ENDIF
          CALL D0OPEN(ITUNIT, TRIGFILE, 'OFL', OPENOK)
          IF(.NOT.OPENOK)THEN
            PRINT *,'Error opening '//TRIGFILE
            STOP
          ENDIF
CJDH 94/12/29          WRITE(ITUNIT,'(I6)')RUN
          IF(EVENTFILE.NE.' ')CALL D0CLOSE(IEUNIT, ' ', OPENOK)
          WRITE(EVENTFILE,'(''events_'',I6.6,''.lis'')')RUN
          INQUIRE(FILE=EVENTFILE,EXIST=LEXIST)
          IF(LEXIST) THEN
            WRITE(*,*) ' Opening existing file '//
     >          EVENTFILE(1:LENOCC(EVENTFILE))
C&IF IBMAIX
C&            CALL ERRMSG('NOT_ON_IBMAIX','EVENT_SCAN,'Cannot'//
C&      >       ' append in AIX','F')
C&ELSEIF VAXVMS, ULTRIX
            OPEN(IEUNIT,FILE=EVENTFILE(1:LENOCC(EVENTFILE)),
     >           STATUS='UNKNOWN',ACCESS='APPEND',
     >           ERR=8888,CARRIAGECONTROL='LIST')
C&ELSE
C&            OPEN(IEUNIT,FILE=EVENTFILE(1:LENOCC(EVENTFILE)),
C&     >           STATUS='OLD',ACCESS='APPEND',ERR=8888)
C&ENDIF
          ELSE
            CALL D0OPEN(IEUNIT, EVENTFILE, 'OFL', OPENOK)
            IF(.NOT.OPENOK)THEN
 8888         CONTINUE
              PRINT *,'Error opening '//EVENTFILE
              STOP
            ENDIF
          ENDIF
CJDH 94/12/29          WRITE(IEUNIT,'(I6)')RUN
        ENDIF
C- Get a list of trigger and filter bit numbers for this event from the header.
        NEED_TSUM = .FALSE.
        NTRIG = 0
        NFILT = 0
        DO I = 0,31
          IF(BTEST(IUHEAD(11),I))THEN
            NTRIG = NTRIG + 1
            TRIG(NTRIG) = I
            IF(CTRIG_RUN(I).EQ.' ')THEN
              NEED_TSUM = .TRUE.
            ELSE
              CTRIG(NTRIG) = CTRIG_RUN(I)
            ENDIF
          ENDIF
          DO J = 0,3
            IF(BTEST(IUHEAD(15+J),I))THEN
              NFILT = NFILT + 1
              K = I + 32*J
              FILT(NFILT) = K
              IF(CFILT_RUN(K).EQ.' ')THEN
                NEED_TSUM = .TRUE.
              ELSE
                CFILT(NFILT) = CFILT_RUN(K)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C- See if we need to read in the TSUM bank.
        IF(NEED_TSUM)THEN
          CALL FZIN(IDADZB,IXMAIN,LHEAD,1,'A',NHEAD,IUHEAD)
          CALL UNCOMPRESS_ZEBRA
          IF( IQUEST(1).eq.-2 .OR. IQUEST(1).EQ.-3 ) THEN
            WRITE(*,8501) IQUEST(1)
 8501       FORMAT(' EVENT_SCAN: Error',I4,
     +        ' from FZIN reading full event. Skip event.')
            GOTO 10
          ENDIF
          CALL GTTSUM(NTRIG, TRIG, CTRIG, NFILT, FILT, CFILT)
          IF(NTRIG.EQ.0.AND.NFILT.EQ.0)THEN
            WRITE(*,* ) ' EVENT_SCAN: Error - TSUM bank not found'
            GO TO 10
          ENDIF
C- Update trigger/filter database
          DO I = 1,NTRIG
            K = TRIG(I)
            IF(K.LT.0.OR.K.GT.31)THEN
              WRITE(*,8502)RUN, EVENT, K
 8502         FORMAT(' EVENT_SCAN: Error - Run', I6, ', Event', I8, 
     +          ', Bad trigger number',I10,' -- skipping event')
              GO TO 10
            ENDIF
            IF(CTRIG_RUN(K).EQ.' ')THEN
              CTRIG_RUN(K) = CTRIG(I)
            ELSE
              IF(CTRIG_RUN(K).NE.CTRIG(I))WRITE(*,8503)RUN, EVENT
 8503         FORMAT(' EVENT_SCAN: Error - Run', I6, ', Event', I8,
     +          ', Trigger name mismatch')
            ENDIF
          ENDDO
          DO I = 1,NFILT
            K = FILT(I)
            IF(K.LT.0.OR.K.GT.127)THEN
              WRITE(*,8504)RUN, EVENT, K
 8504         FORMAT(' EVENT_SCAN: Error - Run', I6, ', Event', I8, 
     +          ', Bad filter number',I10,' -- skipping event')
              GO TO 10
            ENDIF
            IF(CFILT_RUN(K).EQ.' ')THEN
              CFILT_RUN(K) = CFILT(I)
            ELSE
              IF(CFILT_RUN(K).NE.CFILT(I))WRITE(*,8505)RUN, EVENT
 8505         FORMAT(' EVENT_SCAN: Error - Run', I6, ', Event', I8,
     +          ', Filter name mismatch')
            ENDIF
          ENDDO
        ENDIF
C- Done getting trigger/filter information from TSUM or header vector.
        WRITE(IEUNIT,8600)RUN,EVENT,(IUHEAD(I),I=18,15,-1),IUHEAD(11)
 8600   FORMAT(I8,1X,I8,1X,5(Z8.8))
C- Done processing good event
C& IF VAXVMS
C& ELSE
C&        CALL FLUSH_STD
C& ENDIF
        GOTO 10
      ENDIF
C
      IF( IQUEST(1).EQ.1 ) THEN
        WRITE(*,9001) 
 9001   FORMAT('     Found start of run.  Continueing...')
        GOTO 10
      ENDIF
      IF( IQUEST(1).EQ.2 ) THEN
        WRITE(*,9002) 
 9002   FORMAT('     Found end of run.  Continueing...')
        GOTO 10
      ENDIF
      IF( IQUEST(1).EQ.3 ) THEN
        NEVTOT = NEVTOT + NEV
        WRITE(*,9003) NEV, NEVTOT
 9003   FORMAT('     Found ZEBRA EOF -- Close file and go on'/
     +    20X,I6,' Events,         ',I8,' Total events.')
        IF(TRIGFILE.NE.' ')THEN
          DO I=0,31
            IF( LENOCC(CTRIG_RUN(I)).GT.0 ) THEN
              WRITE(ITUNIT,'(A37)') 'TRIG_'//CTRIG_RUN(I)
            ELSE 
              WRITE(ITUNIT,'(A37)') BLANK
            ENDIF
          ENDDO
          DO I=0,127
            IF( LENOCC(CFILT_RUN(I)).GT.0 ) THEN
              WRITE(ITUNIT,'(A37)') 'FILT_'//CFILT_RUN(I)
            ELSE 
              WRITE(ITUNIT,'(A37)') BLANK
            ENDIF
          ENDDO
          CALL D0CLOSE(ITUNIT, ' ', OPENOK)
          TRIGFILE = ' '
        ENDIF
        IF(EVENTFILE.NE.' ')THEN
          CALL D0CLOSE(IEUNIT, ' ', OPENOK)
          EVENTFILE = ' '
        ENDIF
        NEV = 0
        GOTO 30
      ENDIF
      IF( IQUEST(1).EQ.4 ) THEN
        WRITE(*,9004) 
 9004   FORMAT('     SYSEOF: Can continue...')
        GOTO 30
      ENDIF
      IF( IQUEST(1).EQ.5 ) THEN
        WRITE(*,9005)
 9005   FORMAT('     SYSEOF: Cannot continue with this file...')
        GOTO 30
      ENDIF
      IF( IQUEST(1).EQ.6 ) THEN
        WRITE(*,9006) 
 9006   FORMAT('     Attempted to read beyond EOD...')
        GOTO 30
      ENDIF
      IF( IQUEST(1).LT.0 ) THEN
        WRITE(*,9007) IQUEST(1)
 9007   FORMAT(' Error',I10,' from FZIN. Skip file')
        GOTO 30
      ENDIF
      WRITE(*,9008) IQUEST(1)
 9008 FORMAT('     FZIN: Unknown return code=',I10,'. Stop')
      GOTO 999
 30   CONTINUE
      CALL FZENDI(IDADZB,'TX')
      CALL D0CLOSE(IDADZB,' ',OPENOK)
      IF( LFLOOP ) GOTO 20
C
C     Done.  Clean up and exit.
C
 999  CONTINUE
C&IF VAXVMS
      IF( IERR_RETURN.NE.0 ) THEN
        IERR_RETURN = 2**16+1
      ELSE
        IERR_RETURN = 1
      ENDIF
C&ENDIF
      CALL EXIT(IERR_RETURN)
      STOP

 903  CONTINUE
      IERR_RETURN=1
      WRITE(*,9903) IERR,INPUT_FILE(1:LENOCC(INPUT_FILE))
 9903 FORMAT(' EVENT_SCAN: Error ',I4,' opening ',A,/,
     +       '    skip this file...')
      IF( LFLOOP ) GOTO 20
      GOTO 999
C
      END
