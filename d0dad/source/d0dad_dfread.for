      SUBROUTINE D0DAD_DFREAD(IDADDF,IER)
C--------------------------------------------------------------------
C  Read the next event from a D0Dad file...
C
C     Author:        John D. Hobbs
C     Creation Date: 1-NOV-1993
C     Modified:      20-May-1994 JDH Move details into D0DAD_READEVENT
C     Modified:      07-Jul-1994 JDH Add RCP steering parameters.
C                    02-Dec-1994 JDH Add more RCP parameters...
C                    11-OCT-1996 JDH Safety check for 'deleted' events.
C
C--------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER IER,IDFID,IR,IE,ICNTXT,IDADDF,IDATE(2),I
      CHARACTER*50 ERRTXT
      LOGICAL ISOK
      LOGICAL LIB$FIND_FILE
      EXTERNAL LIB$FIND_FILE
C
      INTEGER NBAD_MAX,NSEL_MAX,LENOCC
      PARAMETER(NBAD_MAX=100,NSEL_MAX=1000)
C
      LOGICAL FIRST
      INTEGER IUNIT,IDADFC,NLEN,IOLDFID,RECO,OMNI
      INTEGER NSKIP,NEVENTS,NBAD_RUNS,START_TIME(2),NSEL_RUNS
      INTEGER BAD_RUNS(NBAD_MAX),N,FIRST_RUN,GOOD_RUNS(NSEL_MAX)
      INTEGER LAST_RUN,NSEL_EVTS,GOOD_EVTS(3,NSEL_MAX)     
      INTEGER ERR_LIST_UNIT
      LOGICAL FILE_ERR_FATAL,LFILE_RCP,FILE_FAIL_RCP
      CHARACTER*256 ERR_LIST_FILE,FNTMP
      INTEGER FILE_MDS_VER(2),FILE_RECO_VER(2),FILE_OMNI_VER(2)
      SAVE    IDADFC,FIRST,NSKIP,NEVENTS,NBAD_RUNS,BAD_RUNS
      SAVE    START_TIME,N,FIRST_RUN,LAST_RUN,FILE_FAIL_RCP
      SAVE    NSEL_EVTS,GOOD_EVTS,LFILE_RCP,IOLDFID
      SAVE    FILE_ERR_FATAL,ERR_LIST_FILE,ERR_LIST_UNIT
      SAVE    FILE_MDS_VER,FILE_RECO_VER,FILE_OMNI_VER
      DATA FIRST/.TRUE./
C
C--------------------------------------------------------------------
C
      IF(FIRST) THEN

        FIRST=.FALSE.
        N=0
        NSKIP=0
        NEVENTS=0
        NBAD_RUNS=0
        NSEL_RUNS=0
        NSEL_EVTS=0
        CALL VZERO(START_TIME,2)
        FIRST_RUN=0
        LAST_RUN=0
        ERR_LIST_UNIT=0
C&IF VAXVMS
        ERR_LIST_FILE='bad_events_d0dadf'
C&ELSE
C&        ERR_LIST_FILE='bad_events.d0dadf'
C&ENDIF
        FILE_ERR_FATAL=.FALSE.
        LFILE_RCP=.FALSE.
        CALL VZERO(FILE_MDS_VER,2)
        CALL VZERO(FILE_RECO_VER,2)
        CALL VZERO(FILE_OMNI_VER,2)
        IOLDFID=0
        FILE_FAIL_RCP=.FALSE.

        CALL INRCP('D0DAD_RCP',IER)
        IF(IER.NE.0) THEN
          ERRTXT='Optional RCP file D0DAD_RCP not found'
          CALL ERRMSG('D0DAD_RCP','D0DAD_DFREAD',ERRTXT,'W')
          GOTO 25
        ENDIF

        ERRTXT='Reading D0DAD_RCP'
        CALL ERRMSG('D0DAD_RCP','D0DAD_DFREAD',ERRTXT,'I')
C
C  D0DAD file based RCP parameters...
C
        CALL EZGET('NSKIP',NSKIP,IER)
        CALL EZGET('NEVENTS',NEVENTS,IER)
        CALL EZGETA('START_TIME',1,2,1,START_TIME,IER)
        CALL EZGET('START_RUN',FIRST_RUN,IER)
        CALL EZGET('STOP_RUN',LAST_RUN,IER)
        CALL EZGETA('VETO_RUNS',0,NBAD_RUNS,0,NBAD_RUNS,IER)
        IF( IER.EQ.0 .AND. NBAD_RUNS.GT.NBAD_MAX ) THEN
          WRITE(ERRTXT,1001) NBAD_RUNS,NBAD_MAX
 1001     FORMAT('Number of bad runs in RCP=',I5,' Maximum=',I5)
          CALL ERRMSG('BADRUNS_EXCEEDED','D0DAD_DFREAD',ERRTXT,'F')
        ELSEIF(IER.EQ.0) THEN 
          CALL EZGETA('VETO_RUNS',1,NBAD_RUNS,1,BAD_RUNS,IER)
          NBAD_RUNS=NBAD_RUNS
        ENDIF
        CALL EZGETA('SELECT_RUNS',0,NSEL_RUNS,0,NSEL_RUNS,IER)
       IF( IER.EQ.0 .AND. NSEL_RUNS.GT.NSEL_MAX ) THEN
          WRITE(ERRTXT,1002) NSEL_RUNS,NSEL_MAX
 1002     FORMAT('Number of selected runs in RCP=',I5,' Maximum=',I5)
          CALL ERRMSG('SELRUNS_EXCEEDED','D0DAD_DFREAD',ERRTXT,'F')
        ELSEIF(IER.EQ.0) THEN 
          CALL EZGETA('SELECT_RUNS',1,NSEL_RUNS,1,GOOD_RUNS,IER)
        ENDIF
        CALL EZGETA('SELECT_EVENTS',0,NSEL_EVTS,0,NSEL_EVTS,IER)
        IF( IER.EQ.0 .AND. NSEL_EVTS.GT.NSEL_MAX ) THEN
          WRITE(ERRTXT,1003) NSEL_EVTS,NSEL_MAX
 1003     FORMAT('Number of selected events in RCP=',I5,' Max=',I5)
          CALL ERRMSG('SELEVTS_EXCEEDED','D0DAD_DFREAD',ERRTXT,'F')
        ELSEIF(IER.EQ.0) THEN 
          CALL EZGETA('SELECT_EVENTS',1,NSEL_EVTS,1,GOOD_EVTS,IER)
        ENDIF
C
C  File-name based RCP parameters (Can't get MDS version yet...)
C
        CALL EZGETA('RECO_VERSION',1,2,1,FILE_RECO_VER,IER)
        CALL EZGETA('OMNI_VERSION',1,2,1,FILE_OMNI_VER,IER)
CJDH        CALL EZGETA('MDS_VERSION',1,2,1,FILE_MDS_VER,IER)
        IF( FILE_RECO_VER(1).NE.0 ) LFILE_RCP=.TRUE.
        IF( FILE_OMNI_VER(1).NE.0 ) LFILE_RCP=.TRUE.
CJDH        IF( FILE_MDS_VER(1).NE.0  ) LFILE_RCP=.TRUE.
        IF( FILE_RECO_VER(2).NE.0 ) LFILE_RCP=.TRUE.
        IF( FILE_OMNI_VER(2).NE.0 ) LFILE_RCP=.TRUE.
CJDH        IF( FILE_MDS_VER(2).NE.0  ) LFILE_RCP=.TRUE.
C
C  Open/Read error control parameters
C
        CALL EZGET('FILE_ERROR_IS_FATAL',FILE_ERR_FATAL,IER)
        CALL EZGETS('ERROR_LIST_FILE',1,ERR_LIST_FILE,NLEN,IER)
C
C  Even if no file was found, check the error file (default is write it)
C
 25     CONTINUE
        IF( LENOCC(ERR_LIST_FILE).GT.0 ) THEN
          CALL CUTOL(ERR_LIST_FILE)
          CALL D0DAD_OPEN(JFDF,ERR_LIST_FILE,'W',ERR_LIST_UNIT,IER)
          IF( IER.NE.0 .AND. FILE_ERR_FATAL ) THEN
            ERRTXT='Cannot OPEN error record D0DAD file'
            CALL ERRMSG('OPEN_ERR_FILE','D0DAD_READ_EVENT',ERRTXT,'F')
          ENDIF
        ENDIF

      ENDIF
C
C  Event reading...
C
 20   CONTINUE
C
C   Read the event record from the d0dad file...
C
      CALL DFGET(IDADDF,IR,IE,IDFID,IDZRN,IDZBO,IDATE,IER)
      IF( IER.EQ.1 ) GOTO 998
      IF( IER.NE.0 ) GOTO 903
      IF( IR.EQ.0.AND.IE.EQ.0 ) GOTO 20   ! This shouldn't be necessary
      IF( IDFID.LE.0 ) GOTO 20            ! Deleted events should never get
                                          !   here, but...
C
C   ---------------------------------------------------------
C   Check for valid timestamp/run/event (rcp parameters)...
C   ---------------------------------------------------------
C
C   Timestamp check

      IF( IDATE(1).LT.START_TIME(1) ) GOTO 20
      IF(IDATE(1).EQ.START_TIME(1).AND.IDATE(2).LT.START_TIME(2))GOTO 20

C   Checking for starting/stopping runs

      IF( IR.LT.FIRST_RUN ) GOTO 20
      IF( IR.GT.LAST_RUN .AND. LAST_RUN.GT.0 ) GOTO 998

C   Checking for bad runs...

      DO I=1,NBAD_RUNS
        IF( IR.EQ.BAD_RUNS(I) ) GOTO 20
      ENDDO

C   Checking for selected run...

      DO I=1,NSEL_RUNS
        IF( IR.EQ.GOOD_RUNS(I) ) GOTO 10
      ENDDO
      IF(NSEL_RUNS.GT.0) GOTO 20
 10   CONTINUE

C   Checking for selected events...

      DO I=1,NSEL_EVTS
        IF( IR.EQ.GOOD_EVTS(1,I) .AND.
     >      IE.GE.GOOD_EVTS(2,I) .AND.
     >      IE.LE.GOOD_EVTS(3,I)
     >    ) GOTO 30
      ENDDO
      IF(NSEL_EVTS.GT.0) GOTO 20
 30   CONTINUE
C
C  -----------------------------------------------
C  Check the filename based RCP parameters...
C  -----------------------------------------------
C
      IF( LFILE_RCP ) THEN
        IF( IDFID.NE.IOLDFID ) THEN
          CALL FCGET(IDADFC,IDFID,CFNAME,CGNAME,CTAPE,CFCCOM,IER)
          CALL FILENAME_PARSE(CFNAME,'EXT',FNTMP,NLEN)
          READ(FNTMP,8001) RECO,OMNI
 8001     FORMAT(11X,I4,4X,I2)
          IOLDFID=IDFID
          FILE_FAIL_RCP=.FALSE.
          IF( FILE_RECO_VER(1).NE.0 .AND. FILE_RECO_VER(2).NE.0 .AND.
     >      ( FILE_RECO_VER(1).GT.RECO .OR. FILE_RECO_VER(2).LT.RECO )
     >    )  FILE_FAIL_RCP=.TRUE.
          IF( FILE_OMNI_VER(1).NE.0 .AND. FILE_OMNI_VER(2).NE.0 .AND.
     >      ( FILE_OMNI_VER(1).GT.OMNI .OR. FILE_OMNI_VER(2).LT.OMNI )
     >    )  FILE_FAIL_RCP=.TRUE.
        ENDIF
        IF( FILE_FAIL_RCP ) GOTO 20
      ENDIF

C   Skipping a fixed number of events

      N=N+1
      IF( N.LE.NSKIP ) GOTO 20
      IF( N.GT.(NEVENTS+NSKIP) .AND. NEVENTS.GT.0 ) GOTO 998
C
C    ---------------------------------
C    Read the event from Zebra file...
C    ---------------------------------
C
      CALL D0DAD_READ_EVENT(IR,IE,IDZRN,IDZBO,IDFID,IER)
      IF( IER.NE.0 ) THEN
C    Write record to d0dad file if requested
        IF(ERR_LIST_UNIT.GT.0) THEN
          CALL DFPUT(ERR_LIST_UNIT,IR,IE,IDFID,IDZRN,IDZBO,IDATE,IER)
          IF(IER.NE.0 .AND. FILE_ERR_FATAL ) THEN
            WRITE(ERRTXT,9001) IER
 9001       FORMAT('Error ',I3,' calling DFPUT for BAD EVENT')
            CALL ERRMSG('DFPUT_ERROR','D0DAD_READ_EVENT',ERRTXT,'F')
          ENDIF
        ENDIF
C    Fatal end if requested on read error
        IF( FILE_ERR_FATAL ) THEN
          WRITE(ERRTXT,9002) IDFID
 9002     FORMAT('FILE OPEN/READ ERROR for FID=',I7)
          CALL ERRMSG('READERROR','D0DAD_DFREAD',ERRTXT,'F')
        ENDIF
        GOTO 20
      ENDIF
C
 999  CONTINUE
      IER=0
      RETURN
C
C  End-of-d0dadfile...
C
 998  CONTINUE
      CALL LIB$FIND_FILE_END(ICNTXT)
      CALL D0DAD_CLOSE(IDADDF,IER)
      IQUEST(1)=3
      IER = 3
      RETURN
C
 903  CONTINUE
      IER = -3
      RETURN

 904  CONTINUE
      IER = -4
      RETURN
C-----------------------------------------------------------------------
      ENTRY D0DAD_DFREAD_SETX(IUNIT)
C-----------------------------------------------------------------------
C
C  Let DFREAD know the unit on which the FC has been opened.
C
C-----------------------------------------------------------------------
      IDADFC=IUNIT
      CALL D0DAD_READEVENT_SET(IUNIT)
      RETURN
      END
