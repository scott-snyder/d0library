      SUBROUTINE D0RECO_NEW_FILE(INUNIT,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Initialize input and output files for D0RECO
C-
C-   Outputs : 
C-     INUNIT = unit for reading events
C-     OK     = new input file opened
C-
C-   ENTRY D0RECO_EVTS_FILE(NUM_EVTS)
C-     Output:  
C-     NUM_EVTS= number of events to be read from current input file
C-                                
C-   Created   3-DEC-1991   Serban D. Protopopescu
C-   Updated   8-MAR-1993   Hailin Li and Kirill Denisenko
C-                          Added Parallel RECO flag
C-   Updated  10-FEB-1993   Kirill Denisenko
C-                          Added DBL3SERVER flag
C-   Updated  19-OCT-1994   Qizhong Li-Demarteau  added MXDUMPS_RAW and
C-                                         changed MXDUMPS from 8 to 80
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER INUNIT,NUM_EVTS
      LOGICAL OK
      INCLUDE 'D0$INC:FATCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FATPARA.DEF/LIST'
      CHARACTER*255 TMPFNAME
      CHARACTER*4 CFLF
      INTEGER LENOCC
      CHARACTER*32 TAPE_LABEL
      CHARACTER*16 MSG
      CHARACTER*256 FILE_NAME
      INTEGER MXDUMPS, MXDUMPS_RAW
      PARAMETER( MXDUMPS = 80 )
      PARAMETER( MXDUMPS_RAW = 8 )
      CHARACTER*4 DUMPS(MXDUMPS),DROPS(40),PATH
      INTEGER NUMBER_DUMPS,NUMRAW,EVENT_ID(2,MXDUMPS),NUMDRP
      INTEGER I,SKIP_DUMPS,IER,I1,I2,N,SSUNIT,SUM_UNIT
      INTEGER GIVE,ISTAT,OUTLEN,TRANS_LOG
      INTEGER D0_TIME,OFFTIM
      INTEGER NFILES,FILE_NUM,TRULEN
      INTEGER MAX_NO_FILES
      PARAMETER (MAX_NO_FILES=200)
      CHARACTER*256 INPUT_FILES(MAX_NO_FILES),STA_FILES(MAX_NO_FILES)
     &  ,DST_FILES(MAX_NO_FILES)
      INTEGER NO_EVENTS(MAX_NO_FILES)
      CHARACTER*256 INPUT_FILE,STA_FILE,DST_FILE
      CHARACTER*26 TIMSTR
      CHARACTER*1 XOPT
      LOGICAL INP_XMODE,STA_XMODE,DST_XMODE
      LOGICAL WRITE_STA,WRITE_DST,OPEN_STA,OPEN_DST
      LOGICAL FIRST,RCP
      SAVE FILE_NUM,STA_FILE,DST_FILE,RCP,SUM_UNIT
      DATA FIRST/.TRUE./
      DATA FILE_NUM/0/
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        STA_FILE=' '
        DST_FILE=' '
        CALL D0RECO_FILES(NFILES,INPUT_FILES,STA_FILES,DST_FILES,
     &    NO_EVENTS,TAPE_LABEL,RCP)
        IF(NFILES.GT.MAX_NO_FILES) THEN
          WRITE(INPUT_FILE,2001) NFILES, MAX_NO_FILES
          CALL ERRMSG(' Too many files','D0RECO_NEW_FILE',
     &      INPUT_FILE,'F')
        ENDIF
C
        CALL INRCP('D0RECO_RCP',IER)  ! read control parameters
        IF(IER.NE.0) THEN
          CALL ERRMSG(' Cannot open file','D0RECO_NEW_FILE',
     &      ' cannot open D0RECO_RCP','F')
        ENDIF
C
        CALL EZPICK('D0RECO_RCP')
C
C           read flag settings
        OK=.FALSE.
        CALL EZGET_l('WRITE_E_FILE',OK,IER)
        CALL FLGSET('WRITE_E_FILE',OK)
        CALL EZGET_l('VERIFY',OK,IER)
        CALL FLGSET('VERIFY',OK)
        CALL EZGET_l('REMOTE_STA',OK,IER)
        IF(IER.EQ.0)CALL FLGSET('REMOTE_STA',OK)
C-IF parallel RECO
        CALL EZGET('PARALLEL',OK,IER)
        IF(IER.EQ.0)CALL FLGSET('PARALLEL',OK)
        CALL EZGET('READ_TRIG_FILE',OK,IER)
        CALL FLGSET('READ_TRIG_FILE',OK)
C
C- IF reading DBL3 data using the farm server
        CALL EZGET('DBL3SERVER',OK,IER)
        IF(IER.EQ.0)CALL FLGSET('DBL3SERVER',OK)
C-ENDIF

        FILE_NAME=TAPE_LABEL
        IF ( RCP ) THEN
          OUTLEN=TRULEN(TAPE_LABEL)
          FILE_NAME=TAPE_LABEL(1:OUTLEN)//'.OUT'
        ENDIF
        CALL SSOPEN(FILE_NAME)     ! open summary file
        SUM_UNIT=SSUNIT()
        CALL GET_TIME(TIMSTR)
        WRITE(SUM_UNIT,1000) TIMSTR
        CALL D0RECO_LIBLIST
C
C ****  Tell which banks must be dropped from DST output
C
        CALL EZ_GET_CHARS('DROP_DST_BANKS',NUMDRP,DROPS,IER)   
C
        IF(NUMDRP.GT.0) THEN
          DO 200 I = 1,NUMDRP
            CALL EVDROP('DST',DROPS(I))       ! add bank to the drop list
  200     CONTINUE
          WRITE(SUM_UNIT,1004) (DROPS(I),I=1,NUMDRP)
        ENDIF
C
C            read in dump requests
C
        CALL EZGET_i('NUMBER_DUMPS',NUMBER_DUMPS,IER)
        CALL FLGSET('DUMP_PROCES',.FALSE.)
        IF(NUMBER_DUMPS.NE.0) THEN
C
          CALL FLGSET('DUMP_PROCES',.TRUE.)
          IF ( NUMBER_DUMPS.GT.MXDUMPS ) THEN
            NUMBER_DUMPS=MXDUMPS
            WRITE(MSG,2000) MXDUMPS
            CALL ERRMSG('Too many dumps requested','D0RECO_NEW_FILE',
     &        MSG,'W')
          ENDIF
C
          CALL EZGET_i('SKIP_DUMPS',SKIP_DUMPS,IER)
          IF(SKIP_DUMPS.LT.0) THEN           ! read list of events to dump
            CALL EZGET_iarr('EVENTS_TO_DUMP',EVENT_ID,IER)
            DO I=1,NUMBER_DUMPS
              CALL DMPLST(EVENT_ID(1,I),IER)
            ENDDO
          ENDIF
          CALL DMP_SETNUM(NUMBER_DUMPS,SKIP_DUMPS)
C
          CALL EZ_GET_CHARS('DROP_DST_BANKS',NUMRAW,DUMPS,IER)   
          IF ( NUMRAW.GT.0.AND.NUMRAW.LT.MXDUMPS_RAW) THEN
            CALL FLGSET('DUMP_NONE_H',.FALSE.)
            DO I = 1,NUMRAW
              CALL FLGSET('DUMP_'//DUMPS(I),.TRUE.)
            ENDDO
          ELSE
            CALL FLGSET('DUMP_NONE_H',.TRUE.)
          ENDIF
C
        ENDIF
C
C          set default path
        CALL EZ_GET_CHARS('PATH',N,PATH,IER)   
        IF(IER.EQ.0) CALL PATHDF(PATH)
C
C          read in file requests
        CALL EZGET('INPUT_XCHANGE_MODE',INP_XMODE,IER)
        CALL EZGET('STA_XCHANGE_MODE',STA_XMODE,IER)
        CALL EZGET('DST_XCHANGE_MODE',DST_XMODE,IER)
        CALL EZGET('WRITE_STA',WRITE_STA,IER)
        CALL EZGET('WRITE_DST',WRITE_DST,IER)
C
        CALL EZRSET
        CALL INITIALIZE_RECO_PBD      ! packages initialization
        CALL GET_TIME(TIMSTR)
        D0_TIME=OFFTIM()
        WRITE(SUM_UNIT,1010) TIMSTR,D0_TIME
        FIRST=.FALSE.
      ENDIF
C
      OK=.FALSE.
      FILE_NUM=FILE_NUM+1
      IF(FILE_NUM.GT.NFILES) GOTO 999   ! finished
C
      OPEN_STA=WRITE_STA.AND.STA_FILE.NE.STA_FILES(FILE_NUM)
      OPEN_DST=WRITE_DST.AND.DST_FILE.NE.DST_FILES(FILE_NUM)
      IF(.NOT.FIRST) THEN             ! close previous file
        CALL FZENDI(INUNIT,'T')
        CALL D0CLOSE(INUNIT,' ',IER)
        CALL RLUNIT(87,INUNIT,IER)   
        IF(OPEN_STA) CALL EVCLWO('STA')
        IF(OPEN_DST) CALL EVCLWO('DST')
      ENDIF
C
      IF ( RCP ) THEN
        OUTLEN=TRULEN(TAPE_LABEL)
        FILE_NAME=TAPE_LABEL(1:OUTLEN)
      ELSE
        ISTAT=TRANS_LOG(TAPE_LABEL,FILE_NAME,OUTLEN)  ! translate logical name
      ENDIF
      WRITE(SUM_UNIT,1005) FILE_NAME(1:OUTLEN)
C
      INPUT_FILE=INPUT_FILES(FILE_NUM)
      STA_FILE=STA_FILES(FILE_NUM)
      DST_FILE=DST_FILES(FILE_NUM)
      XOPT='T'
      IF(INP_XMODE) XOPT='XT'
      TMPFNAME = INPUT_FILE
      CALL CLTOU(TMPFNAME)
C
      IF ( TMPFNAME(1:9) .NE. D0TRNK  ) THEN
C
        CALL EVOPIN(INPUT_FILE,XOPT,INUNIT,OK)  ! open input file
        IF(.NOT.OK) THEN
          CALL ERRMSG(' Cannot open file','D0RECO_NEW_FILE',
     &      ' cannot open '//INPUT_FILE ,'F')
        ENDIF
c
      ELSE
C
C ****  this is a FATMEN generic name
C
        CALL GTUNIT(87,INUNIT,IER)
C
C ****  options for fmopen after '/' R read,  
C ****  V update file size after staging to disk, F call FZFILE
C
        CALL D0OPEN(INUNIT,INPUT_FILE,'/RVF',OK)
        IF(.NOT.OK) THEN
          CALL ERRMSG(' Cannot open file','D0RECO_NEW_FILE',
     &      ' Fatmen file '//INPUT_FILE ,'F')
        ENDIF
C
C ****  get the file name corresponding to the generic name
C
        CALL FMGETC (LFMINF,INPUT_FILE,MFQNFA,255,IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG(' Cannot open file','D0RECO_NEW_FILE',
     &      ' Fatmen file '//INPUT_FILE ,'F')
        ENDIF
C
C ****  get the file format
C
        CALL FMGETC (LFMINF,CFLF,MFLFFA,NFLFFA,IER)
        IF(IER.NE.0) THEN
          IF(.NOT.OK) THEN
            CALL ERRMSG(' Cannot get file format','D0RECO_NEW_FILE',
     &        ' Fatmen file '//INPUT_FILE ,'F')
          ENDIF
        ENDIF
        XOPT=' '
        IF ( CFLF(1:LENOCC(CFLF)) .EQ. 'FX' .OR.
     &         CFLF(1:LENOCC(CFLF)) .EQ. 'FFX' .OR.
     &         CFLF(1:LENOCC(CFLF)) .EQ. 'FXN' ) XOPT='X'
C
C
      ENDIF
      MSG='in native mode'
      IF(INP_XMODE) MSG= ' in exchange mode'
      IF ( RCP ) THEN
        FILE_NAME=INPUT_FILE
        OUTLEN=TRULEN(FILE_NAME)
      ELSE
        ISTAT=TRANS_LOG(INPUT_FILE,FILE_NAME,OUTLEN)  ! translate logical name
      ENDIF
      WRITE(SUM_UNIT,1001) MSG,FILE_NAME(1:OUTLEN)
C
C          STA output data file
      IF(OPEN_STA) THEN
        XOPT=' '
        IF(STA_XMODE) XOPT='X'
        CALL EVOPWO('STA',STA_FILE,XOPT,OK) ! open standard output file
        IF(.NOT.OK) THEN
          CALL ERRMSG(' Cannot open file','D0RECO_NEW_FILE',
     &      ' cannot open '//STA_FILE ,'F')
        ENDIF
        MSG='in native mode'
        IF(STA_XMODE) MSG= 'in exchange mode'
        IF ( RCP ) THEN
          FILE_NAME=STA_FILE
          OUTLEN=TRULEN(FILE_NAME)
        ELSE
          ISTAT=TRANS_LOG(STA_FILE,FILE_NAME,OUTLEN)  ! translate logical name
        ENDIF
        WRITE(SUM_UNIT,1002) MSG,FILE_NAME(1:OUTLEN)
      ENDIF
C
C         DST output data file
      IF(OPEN_DST) THEN
        XOPT=' '
        IF(DST_XMODE) XOPT='X'
        CALL EVOPWO('DST',DST_FILE,XOPT,OK)   ! open dst output file
        IF(.NOT.OK) THEN
          CALL ERRMSG(' Cannot open file','D0RECO_NEW_FILE',
     &      ' cannot open '//DST_FILE ,'F')
        ENDIF
        MSG=' in native mode '
        IF(DST_XMODE) MSG= ' in exchange mode '
        IF ( RCP ) THEN
          FILE_NAME=DST_FILE
          OUTLEN=TRULEN(FILE_NAME)
        ELSE
          ISTAT=TRANS_LOG(DST_FILE,FILE_NAME,OUTLEN)  ! translate logical name
        ENDIF
        WRITE(SUM_UNIT,1003) MSG,FILE_NAME(1:OUTLEN)
      ENDIF
      OK=.TRUE.
      GOTO 999
C
      ENTRY D0RECO_EVTS_FILE(NUM_EVTS)
      NUM_EVTS=NO_EVENTS(FILE_NUM)
      IF(NUM_EVTS.LE.0) NUM_EVTS=999999
C
  999 RETURN
C
 1000 FORMAT(//,' +',72('-'),'+',/,' +',72X,'+',/,
     &  ' +',15X,'SUMMARY FILE FOR D0 RECONSTRUCTION PROGRAM',15X,'+',
     &  /,' +',72X,'+',/,' +',72('-'),'+',//,
     &  '    INITIALIZATION STARTED AT ',A)
 1001 FORMAT(/,'  INPUT data file ',A16,2X,A,/)
 1002 FORMAT('  STA data file ',A16,4X,A,/)
 1003 FORMAT('  DST data file ',A16,4X,A,/)
 1004 FORMAT('  Banks dropped from DST by main program :',5A5)
 1005 FORMAT(/,'  Tape label (same as summary label)  ',A)
 1010 FORMAT(//,'    PROCESSING STARTED AT ',A,', D0 time=',I10)
 2000 FORMAT(' Maximum allowed is ',I4)
 2001 FORMAT(' Number of files',I4,' exceeds maximum allowed ',I4)
      END
