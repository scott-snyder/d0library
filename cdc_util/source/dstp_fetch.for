      SUBROUTINE DSTP_FETCH(CRUN,CALTYPE,DB_OPENED,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract the Processed STP structures from DBL3 into
C-                         memory
C-
C-   Inputs  : CRUN = Run number 
C-             CALTYPE = what calibration type to process
C-
C-   Input/
C-   Outputs : DB_OPENED = Indicated whether database is opened or closed
C-
C-   Controls: returns IOK = .true. if all goes well
C-
C-   Created  14-OCT-1992   Srini Rajagopalan
C-   Updated  30-JUN-1993   Qizhong Li-Demarteau  refill LDPDH, LDGNH and 
C-                                                LDTMH after ZSHUNT
C-   Updated  21-DEC-1993   Ed Oltman  fix DBCLB_FETCH_OFFLINE error detection 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDPDH.LINK'
      INCLUDE 'D0$LINKS:IZDGNH.LINK'
      INCLUDE 'D0$LINKS:IZDTMH.LINK'
C
      INTEGER CRUN,TYPE
      INTEGER LENGTH,IER
      INTEGER LDATA,LKEY
      INTEGER LORUN(3),HIRUN(3)
      INTEGER GZDPDH,GZDGNH,GZDTMH
C
      CHARACTER*(*) CALTYPE
      CHARACTER*25 PATH
      CHARACTER*40 MSG
      CHARACTER*80 DBCALIB
C
      LOGICAL IOK,DB_OPENED
      LOGICAL FIRST,BYPASS_DBL3_ERROR
      CHARACTER*1 SEVER
C
      SAVE FIRST,LORUN,HIRUN
C
      DATA FIRST /.TRUE./
      DATA LORUN /3*999999/
      DATA HIRUN /3*-1/
C
C----------------------------------------------------------------------
C
C  init...
C
      IOK = .TRUE.
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        CALL EZGETS('DBCALIB$CDC',1,DBCALIB,LENGTH,IER)
        CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
        CALL EZRSET
        SEVER = 'F'
        IF (BYPASS_DBL3_ERROR) SEVER = 'W'
      ENDIF
C
C  Check top level STP bank
C
      IF (LSCDC.LE.0) THEN
        CALL ERRMSG('DTRAKS','DSTP_FETCH',
     &    'CDC constants header bank does not exist',SEVER)
        IOK = .FALSE.
        GO TO 900
      ENDIF
C
C  Check calibration type
C
      IF (CALTYPE.EQ.'PEDESTAL') THEN
        TYPE = 1
      ELSE IF (CALTYPE.EQ.'GAINS') THEN
        TYPE = 2
      ELSE IF (CALTYPE.EQ.'TIMES') THEN
        TYPE = 3
      ELSE
        WRITE(MSG,10)CALTYPE
   10   FORMAT('Unsupported CALTYPE=',A,'specified as input')
        CALL ERRMSG('DTRAKS','DSTP_FETCH',MSG,SEVER)
        IOK = .FALSE.
        GO TO 900
      ENDIF
C
C  Check against validity range, if satisfactory return without doing anything
C
      IF (CRUN.GE.LORUN(TYPE) .AND. CRUN.LE.HIRUN(TYPE)) GO TO 900
C
C  Open DBL3 database if not already opened
C
      IF (.NOT.DB_OPENED) THEN
        CALL DBCLB_INITIALIZE(DBCALIB,'S',IOK)
        IF (.NOT.IOK) THEN
          CALL ERRMSG('DTRAKS','DSTP_FETCH',
     &      'Error initializing DBL3 database',SEVER)
          GO TO 900
        ELSE
          DB_OPENED = .TRUE.
        ENDIF
      ENDIF
C
C  Check if Pedestals have been requested, If so read structure under SCDC
C
      IF (TYPE.EQ.1) THEN
        CALL DBCLB_PATH('PROC_PEDESTALS','CDC',PATH)
        CALL DBCLB_FETCH_OFFLINE(PATH,CRUN,0,LDATA,LKEY)
        IF (LDATA .EQ. 0 .OR. LKEY .EQ. 0) THEN
          CALL ERRMSG('DTRAKS','DSTP_FETCH',
     &      'Error fetching Processed pedestals from DBL3',SEVER)
          IOK = .FALSE.
          GO TO 900
        ENDIF
C
C  Update Validity arrays
C
        LORUN(TYPE) = IC(LKEY + 3)
        HIRUN(TYPE) = IC(LKEY + 4)
C
C Check if DPDH already exists, If so drop structure. 
C Shunt new structure under SCDC.
C        
        IF (LDPDH.GT.0) CALL MZDROP(IXSTP,LDPDH,' ')
        CALL ZSHUNT(IXSTP,LDATA,LSCDC,-IZDPDH,0)
        LDPDH = LC( LSCDC - IZDPDH )
C
C  Check if Gains have been requested, If so read structure under SCDC
C
      ELSE IF (TYPE.EQ.2) THEN
        CALL DBCLB_PATH('PROC_GAINS','CDC',PATH)
        CALL DBCLB_FETCH_OFFLINE(PATH,CRUN,0,LDATA,LKEY)
        IF (LDATA .EQ. 0 .OR. LKEY .EQ. 0) THEN
          CALL ERRMSG('DTRAKS','DSTP_FETCH',
     &      'Error fetching Processed Gains from DBL3',SEVER)
          IOK = .FALSE.
          GO TO 900
        ENDIF
C
C  Update Validity arrays
C
        LORUN(TYPE) = IC(LKEY + 3)
        HIRUN(TYPE) = IC(LKEY + 4)
C
C Check if DGNH already exists, If so drop structure. 
C Shunt new structure under SCDC.
C        
        IF (LDGNH.GT.0) CALL MZDROP(IXSTP,LDGNH,' ')
        CALL ZSHUNT(IXSTP,LDATA,LSCDC,-IZDGNH,0)
        LDGNH = LC( LSCDC - IZDGNH )
C
C
C  Check if T0's have been requested, If so read structure under SCDC
C
      ELSE IF (TYPE.EQ.3) THEN
        CALL DBCLB_PATH('PROC_TIMES','CDC',PATH)
        CALL DBCLB_FETCH_OFFLINE(PATH,CRUN,0,LDATA,LKEY)
        IF (LDATA .EQ. 0 .OR. LKEY .EQ. 0) THEN
          CALL ERRMSG('DTRAKS','DSTP_FETCH',
     &      'Error fetching Processed Times from DBL3',SEVER)
          IOK = .FALSE.
          GO TO 900
        ENDIF
C
C  Update Validity arrays
C
        LORUN(TYPE) = IC(LKEY + 3)
        HIRUN(TYPE) = IC(LKEY + 4)
C
C Check if DTMH already exists, If so drop structure. 
C Shunt new structure under SCDC.
C        
        IF (LDTMH.GT.0) CALL MZDROP(IXSTP,LDTMH,' ')
        CALL ZSHUNT(IXSTP,LDATA,LSCDC,-IZDTMH,0)
        LDTMH = LC( LSCDC - IZDTMH )
      ENDIF
C
  900 CONTINUE
C
      IF (.NOT.IOK) THEN
        CALL ERRMSG('NO DATA','DSTP_FETCH',
     &    'No STP constants obtained from DBL3 database',SEVER)
      ENDIF
C
  999 RETURN
      END
