      SUBROUTINE VSTP_INSERT(CALTYPE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write out STP file into TODO area to be 
C-                         inserted into VTX DBL3 database.
C-
C-   Inputs  : CALTYPE = Calibration type to be enetered in offline database.
C-                     
C-   Outputs : IER     = 0 if all goes well
C-                     = -1 to -4 Error code returned from D0DBL3_WRITFZ
C-                     = -5 Top level bank not found
C-                     = -6 Invalid calibration type passed
C-                     = -7 Error initializing DBL3 database
C-   Controls: none
C-
C-   Created  10-SEP-1992   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER IER, LEN
      INTEGER I,J,K
      INTEGER JDATE,JTIME,DATTIM
      CHARACTER*8 STIME
C
      INTEGER NKYS
      PARAMETER (NKYS=15)
      INTEGER KEYS(NKYS)
      INTEGER WRITE_MODE
C
      CHARACTER*25 PATH,MSGSTR
      CHARACTER*48 TODO_AREA
      CHARACTER*80 DBCALIB
      CHARACTER*(*) CALTYPE
C
      LOGICAL DB_OPENED
      LOGICAL FIRST,IOK
      DATA FIRST,DB_OPENED /.TRUE.,.FALSE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTXCOFF_RCP')
        CALL EZGETS('TODO_AREA',1,TODO_AREA,LEN,IER)
        CALL EZGETS('DBCALIB$VTX',1,DBCALIB,LEN,IER)
        CALL EZGET('WRITE_MODE',WRITE_MODE,IER)
        CALL EZRSET
      ENDIF
C
C&IF VAXVMS
C
      CALL IDATE(I,J,K)           ! Output is MM,DD,YR
      JDATE=K*10000+J*100+I       ! Pack as YYMMDD for DBL3 compatibility
      CALL TIME(STIME)
      READ(STIME,'(I2,1X,I2,1X,I2)')I,J,K
      JTIME=I*10000+J*100+K             ! Pack as HrMnSc
      CALL DBPKTS(JDATE,JTIME,DATTIM)   ! Packed date and time
C
C&ENDIF
C
C  Open DBL3 database only if necessary.
C  Note:  NEED Write privileges in order to do this
C
      IF (WRITE_MODE.EQ.1.OR.WRITE_MODE.EQ.2) THEN
        IF (.NOT.DB_OPENED) THEN
          CALL DBCLB_INITIALIZE(DBCALIB,'SU',IOK)
          IF (.NOT.IOK) THEN
            IER = -7
            CALL ERRMSG('INIT_ERROR','VSTP_INSERT',
     &          'Error initializing DBL3 database, Aborting.','W')
          ELSE
            DB_OPENED = .TRUE.
          ENDIF
        ENDIF
      ENDIF
C
      CALL VZERO(KEYS(1),NKYS)          ! Zero all keys
C
C  - Pedestals under VPDH
C
      IF (CALTYPE(1:8).EQ.'PEDESTAL') THEN
        IF (LVPDH.LE.0) THEN
          CALL ERRMSG('VPDH bank not found','VSTP_INSERT',
     &      'STP structure not inserted in database','W')
          IER = -5
          GO TO 999
        ENDIF
C
        IC(LVPDH+3) = JDATE
        IC(LVPDH+4) = JTIME
        KEYS(3) = IC(LVPDH+1)                ! Start Validity
        KEYS(4) = IC(LVPDH+2)                ! End Validity
        KEYS(9) = DATTIM                     ! Date/Time of entry
C
        CALL DBCLB_PATH('PROC_PEDESTAL','VTX',PATH)
C - insert to server
        IF (WRITE_MODE.EQ.0.OR.WRITE_MODE.EQ.2) THEN
          CALL D0DBL3_WRITFZ(TODO_AREA,IDVSTP,10,PATH,NKYS,KEYS,
     &                       'R-KS348',LVPDH,IER)
        ENDIF
C - insert directly into DBL3
        IF (WRITE_MODE.EQ.1.OR.WRITE_MODE.EQ.2) THEN
          CALL ZDB_INSERT('VPDH',0,PATH,KEYS,IER)
        ENDIF
C
        IF (IER.NE.0) THEN
          WRITE(MSGSTR,10)IER
          CALL ERRMSG(MSGSTR,'VSTP_INSERT',
     &                       'Error writing out Proc Pedestals','W')
        ENDIF
C
      ELSE IF (CALTYPE(1:5).EQ.'GAINS') THEN
C
C Gains stuff under VGNH
C
        IF (LVGNH.LE.0) THEN
          CALL ERRMSG('VGNH bank not found','VSTP_INSERT',
     &      'STP structure not inserted in database','W')
          IER = -5
          GO TO 999
        ENDIF
C
        IC(LVGNH+3) = JDATE
        IC(LVGNH+4) = JTIME
        KEYS(3) = IC(LVGNH+1)                ! Start Validity
        KEYS(4) = IC(LVGNH+2)                ! End Validity
        KEYS(9) = DATTIM                     ! Date/Time of entry
C - insert into server
        CALL DBCLB_PATH('PROC_GAINS','VTX',PATH)
        IF (WRITE_MODE.EQ.0.OR.WRITE_MODE.EQ.2) THEN
          CALL D0DBL3_WRITFZ(TODO_AREA,IDVSTP,10,PATH,NKYS,KEYS,
     &                     'R-KS348',LVGNH,IER)
        ENDIF
C
C - insert directly into DBL3
C
        IF (WRITE_MODE.EQ.1.OR.WRITE_MODE.EQ.2) THEN
          CALL ZDB_INSERT('VGNH',0,PATH,KEYS,IER)
        ENDIF
        IF (IER.NE.0) THEN
          WRITE(MSGSTR,10)IER
          CALL ERRMSG(MSGSTR,'VSTP_INSERT',
     &                       'Error writing out Proc Gains','W')
        ENDIF
C
      ELSE IF (CALTYPE(1:5).EQ.'TIMES') THEN
C
C Times stuff under VTMH
C
        IF (LVTMH.LE.0) THEN
          CALL ERRMSG('VTMH bank not found','VSTP_INSERT',
     &      'STP structure not inserted in database','W')
          IER = -5
          GO TO 999
        ENDIF
C
        IC(LVTMH+3) = JDATE
        IC(LVTMH+4) = JTIME
        KEYS(3) = IC(LVTMH+1)                ! Start Validity
        KEYS(4) = IC(LVTMH+2)                ! End Validity
        KEYS(9) = DATTIM                     ! Date/Time of entry
C
        CALL DBCLB_PATH('PROC_TIMES','VTX',PATH)
C
        IF (WRITE_MODE.EQ.0.OR.WRITE_MODE.EQ.2) THEN
          CALL D0DBL3_WRITFZ(TODO_AREA,IDVSTP,10,PATH,NKYS,KEYS,
     &                     'R-KS348',LVTMH,IER)
        ENDIF
C - insert directly into DBL3
        IF (WRITE_MODE.EQ.1.OR.WRITE_MODE.EQ.2) THEN
          CALL ZDB_INSERT('VTMH',0,PATH,KEYS,IER)
        ENDIF
C
        IF (IER.NE.0) THEN
          WRITE(MSGSTR,10)IER
          CALL ERRMSG(MSGSTR,'VSTP_INSERT',
     &                       'Error writing out Proc Times','W')
        ENDIF
C
      ELSE 
        CALL ERRMSG('CALTYPE_ERROR','VSTP_INSERT',
     &    'Invalid calibration type requested','W')
        IER = -8
      ENDIF
C
C  Finally close database, if opened directly
C
      IF (WRITE_MODE.EQ.1.OR.WRITE_MODE.EQ.2) THEN
        IF (DB_OPENED) THEN
          CALL DBCLB_FINISH
          DB_OPENED = .FALSE.
        ENDIF
      ENDIF
C
   10 FORMAT('STP Insert Error = ',I3)
C
  999 RETURN
      END
