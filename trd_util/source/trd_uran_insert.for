      PROGRAM TRD_URAN_INSERT
C----------------------------------------------------------------------
C-   Purpose and Methods : Write out STP file into TODO area to be 
C-                         inserted into TRD DBL3 database.
C-
C-   Inputs  : 
C-                     
C-   Outputs : IER     = 0 if all goes well
C-                     = -1 to -4 Error code returned from D0DBL3_WRITFZ
C-                     = -5 Top level bank not found
C-                     = -6 Invalid calibration type passed
C-                     = -7 Error initializing DBL3 database
C-   Controls: none
C-
C-   Created  20-JAN-1995   Lewis Taylor Goss   
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER IER,LEN,I,J,K,JDATE,JTIME,DATTIM,OLD_NKYS,NEW_NKYS,NSIZ
      PARAMETER (OLD_NKYS=15,NEW_NKYS=23,NSIZ=13)
      INTEGER KEYS(OLD_NKYS),NEW_KEYS(NEW_NKYS),WRITE_MODE
      INTEGER LTURA,IOH
C
      CHARACTER*8 STIME
      CHARACTER*25 PATH,MSGSTR
      CHARACTER*80 DBCALIB
C
      LOGICAL DB_OPENED,FIRST,IOK
      DATA FIRST,DB_OPENED /.TRUE.,.FALSE./
      COMMON /URLINK/ LTURA
C----------------------------------------------------------------------
      CALL MZEBRA(0)
      CALL INZSTP
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL INRCP ('TRD_RCP',IER)
        CALL EZPICK('TRD_RCP')
        CALL EZGETS('DBCALIB$TRD',1,DBCALIB,LEN,IER)
        CALL EZGET('WRITE_MODE',WRITE_MODE,IER)
        CALL EZRSET
        CALL MZFORM('TURA','1I 12F',IOH)
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
            CALL ERRMSG('INIT_ERROR','TRD_URAN_INSERT',
     &          'Error initializing DBL3 database, Aborting.','W')
          ELSE
            DB_OPENED = .TRUE.
          ENDIF
        ENDIF
      ENDIF
C
      CALL VZERO(KEYS(1),OLD_NKYS)          ! Zero all keys
C
      CALL MZLINK( IXSTP,'/URLINK/', LTURA, LTURA, LTURA)
      CALL MZBOOK( IDVSTP, LTURA, 0, 2, 'TURA', 0, 0, NSIZ, IOH, 0)
C
      CALL UPDATE_KEYS(NEW_KEYS,11)
      KEYS( 3) = NEW_KEYS(12)               ! Uranium Run Number  15000
      KEYS( 4) = 999999999
      KEYS( 9) = DATTIM                     ! Date/Time of entry
      KEYS(11) = NEW_KEYS(12)               ! Uranium Run Number 
      KEYS(12) = NEW_KEYS( 3)               ! DLB3 packed time of run
C
      IC(LTURA+1) = 1                       ! TURA bank version number
      C(LTURA+ 2) = FLOAT(NEW_KEYS(16))/10. ! Pressure in collision hall
      C(LTURA+ 3) = FLOAT(NEW_KEYS(17))/10. ! Temperature in collision hall
      C(LTURA+ 4) = FLOAT(NEW_KEYS(13))/10. ! Uranium value layer 1
      C(LTURA+ 5) = FLOAT(NEW_KEYS(14))/10. ! Uranium value layer 2
      C(LTURA+ 6) = FLOAT(NEW_KEYS(15))/10. ! Uranium value layer 3
      C(LTURA+ 7) = FLOAT(NEW_KEYS(18))/10. ! High Voltage layer 1
      C(LTURA+ 8) = FLOAT(NEW_KEYS(19))/10. ! High Voltage layer 2
      C(LTURA+ 9) = FLOAT(NEW_KEYS(20))/10. ! High Voltage layer 3
      C(LTURA+10) = FLOAT(NEW_KEYS(21))/10. ! Potential wire voltage layer 1
      C(LTURA+11) = FLOAT(NEW_KEYS(22))/10. ! Potential wire voltage layer 2
      C(LTURA+12) = FLOAT(NEW_KEYS(23))/10. ! Potential wire voltage layer 3
      C(LTURA+13) = 0.
C
      CALL DBCLB_PATH('TRD_URAN','TRD',PATH)
C - insert to server
      IF (WRITE_MODE.EQ.0.OR.WRITE_MODE.EQ.2) THEN
        CALL D0DBL3_WRITFZ('TODO_AREA',IDVSTP,10,PATH,OLD_NKYS,KEYS,
     &                     'R-KS348',LTURA,IER)
      ENDIF
C - insert directly into DBL3
      IF (WRITE_MODE.EQ.1.OR.WRITE_MODE.EQ.2) THEN
        CALL ZDB_INSERT('TURA',0,PATH,KEYS,IER)
      ENDIF
C
      IF (IER.NE.0) THEN
        WRITE(MSGSTR,10)IER
        CALL ERRMSG(MSGSTR,'TRD_URAN_INSERT',
     &                     'Error writing out Uranium Run values','W')
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
      END
