      SUBROUTINE FSTP_INSERT(CALTYPE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write out STP file into TODO area to be 
C-                         inserted into FDC DBL3 database.
C-
C-   Inputs  : CALTYPE = Calibration type to be written out
C-                     = 'PEDESTAL', 'GAINS' or 'TIMES'
C-   Outputs : IER     = 0 if all goes well
C-                     = -1 to -4 Error code returned from D0DBL3_WRITFZ
C-                     = -5 Top level bank not found
C-                     = -6 Invalid calibration type passed
C-                     = -7 Error initializing DBL3 database
C-   Controls: none
C-
C-   Created  10-SEP-1992   Srini Rajagopalan
C-   Updated   8-APR-1993   Robert E. Avery   Fill in KEYS(11) with run num. 
C-   Updated   3-JUN-1993   Susan K. Blessing  Set default values for
C-    JDATE, JTIME, DATTIM
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER IER
      INTEGER I,J,K
      INTEGER JDATE,JTIME,DATTIM
      CHARACTER*8 STIME
C
      INTEGER NKYS
      PARAMETER (NKYS=15)
      INTEGER KEYS(NKYS)
      INTEGER WRITE_MODE
      INTEGER LENGTH
C
      CHARACTER*(*) CALTYPE
      CHARACTER*25 PATH,MSGSTR
      CHARACTER*48 TODO_AREA
      CHARACTER*80 DBCALIB
C
      LOGICAL FIRST,IOK,DB_OPENED
      DATA FIRST,DB_OPENED /.TRUE.,.FALSE./
      DATA JDATE,JTIME,DATTIM/3*0/
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGETS('TODO_AREA',1,TODO_AREA,LENGTH,IER)
        CALL EZGETS('DBCALIB$FDC',1,DBCALIB,LENGTH,IER)
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
      CALL VZERO(KEYS(1),NKYS)          ! Zero all keys
C
C Open database if necessary
C
      IF (WRITE_MODE.EQ.1.OR.WRITE_MODE.EQ.2) THEN
        IF (.NOT.DB_OPENED) THEN
          CALL DBCLB_INITIALIZE(DBCALIB,'SU',IOK)
          IF (.NOT.IOK) THEN
            CALL ERRMSG('DBINIT_ERROR','FSTP_INSERT',
     &        'Error initializing DBL3 database','W')
            IER = -7
            GO TO 999
          ELSE
            DB_OPENED = .TRUE.
          ENDIF
        ENDIF
      ENDIF
C
C  Pedestals under FPDH...
C
      IF (CALTYPE(1:8).EQ.'PEDESTAL') THEN
        IF (LFPDH.LE.0) THEN
          CALL ERRMSG('FPDH bank not found','FSTP_INSERT',
     &      'STP structure not inserted in database','W')
          IER = -5
          GO TO 999
        ENDIF
C
        IC(LFPDH+3) = JDATE
        IC(LFPDH+4) = JTIME
        KEYS(3) = IC(LFPDH+1)                ! Start Validity
        KEYS(4) = IC(LFPDH+2)                ! End Validity
        KEYS(9) = DATTIM                     ! Date/Time of entry
        KEYS(11) = KEYS(3)                   ! Run no = start validity run
C
        CALL DBCLB_PATH('PROC_PEDESTAL','FDC',PATH)
C
        IF (WRITE_MODE.EQ.0.OR.WRITE_MODE.EQ.2) THEN
          CALL D0DBL3_WRITFZ(TODO_AREA,IDVSTP,10,PATH,NKYS,KEYS,
     &                     'R-KS348',LFPDH,IER)
        ENDIF
C
        IF (WRITE_MODE.EQ.1.OR.WRITE_MODE.EQ.2) THEN
          CALL ZDB_INSERT('FPDH',0,PATH,KEYS,IER)
        ENDIF
C
        IF (IER.NE.0) THEN
          WRITE(MSGSTR,10)IER
          CALL ERRMSG(MSGSTR,'FSTP_INSERT',
     &                       'Error writing out Proc Pedestals','W')
        ENDIF
C
C  Gains under FGNH
C
      ELSE IF (CALTYPE.EQ.'GAINS') THEN
        IF (LFGNH.LE.0) THEN
          CALL ERRMSG('FGNH bank not found','FSTP_INSERT',
     &      'STP structure not inserted in database','W')
          IER = -5
          GO TO 999
        ENDIF
C
        IC(LFGNH+3) = JDATE
        IC(LFGNH+4) = JTIME
        KEYS(3) = IC(LFGNH+1)                ! Start Validity
        KEYS(4) = IC(LFGNH+2)                ! End Validity
        KEYS(9) = DATTIM                     ! Date/Time of entry
        KEYS(11) = KEYS(3)                   ! Run no = start validity run
C
        CALL DBCLB_PATH('PROC_GAINS','FDC',PATH)
C
        IF (WRITE_MODE.EQ.0.OR.WRITE_MODE.EQ.2) THEN
          CALL D0DBL3_WRITFZ(TODO_AREA,IDVSTP,10,PATH,NKYS,KEYS,
     &                     'R-KS348',LFGNH,IER)
        ENDIF
C
        IF (WRITE_MODE.EQ.1.OR.WRITE_MODE.EQ.2) THEN
          CALL ZDB_INSERT('FGNH',0,PATH,KEYS,IER)
        ENDIF
C
        IF (IER.NE.0) THEN
          WRITE(MSGSTR,10)IER
          CALL ERRMSG(MSGSTR,'FSTP_INSERT',
     &                       'Error writing out Proc Gains','W')
        ENDIF
C
C  Times under FTMH...
C
      ELSE IF (CALTYPE.EQ.'TIMES') THEN
        IF (LFTMH.LE.0) THEN
          CALL ERRMSG('FTMH bank not found','FSTP_INSERT',
     &      'STP structure not inserted in database','W')
          IER = -5
          GO TO 999
        ENDIF
C
        IC(LFTMH+3) = JDATE
        IC(LFTMH+4) = JTIME
        KEYS(3) = IC(LFTMH+1)                ! Start Validity
        KEYS(4) = IC(LFTMH+2)                ! End Validity
        KEYS(9) = DATTIM                     ! Date/Time of entry
        KEYS(11) = KEYS(3)                   ! Run no = start validity run
C
        CALL DBCLB_PATH('PROC_TIMES','FDC',PATH)
C
        IF (WRITE_MODE.EQ.0.OR.WRITE_MODE.EQ.2) THEN
          CALL D0DBL3_WRITFZ(TODO_AREA,IDVSTP,10,PATH,NKYS,KEYS,
     &                     'R-KS348',LFTMH,IER)
        ENDIF
C
        IF (WRITE_MODE.EQ.1.OR.WRITE_MODE.EQ.2) THEN
          CALL ZDB_INSERT('FTMH',0,PATH,KEYS,IER)
        ENDIF
C
        IF (IER.NE.0) THEN
          WRITE(MSGSTR,10)IER
          CALL ERRMSG(MSGSTR,'FSTP_INSERT',
     &                       'Error writing out Proc Times','W')
        ENDIF
C
      ELSE
        CALL ERRMSG('Invalid Calibration Type','FSTP_INSERT',' ','W')
        IER = -6
      ENDIF
C
C  Close database if necessary
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
