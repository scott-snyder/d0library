      PROGRAM FDC_OFFCAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create FZ file to be inserted into
C-   FDC DBL3 database. FZ file contains offline calibration constants,
C-   in logical format (FPDH, FTMH, FGNH).
C-
C-   File is formed as follows:
C-      1. Read in STP banks containing default values.
C-      2. Read in DBL3 online constants into logical structure
C-              (just as in FTRAKS reconstruction).
C-      3. Read in OFFLINE constants into logical structure.
C-   Then FZ file is output.
C-
C-   Inputs/Controls: FTRAKS_RCP, FDC_STPFILE, FDC online DBL3 database
C-   Outputs: dbl3 fz files written to "todo" area.
C-
C-   Created  23-AUG-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER IER
      INTEGER ILEN
      INTEGER MAX_FDCRT
      INTEGER DBL3_RUN,START_RUN,END_RUN
      INTEGER LFHEAD
C
      LOGICAL GN_INI,PD_INI,TM_INI
      CHARACTER*26 FILNAM
      CHARACTER*10 CALTYPE
      CHARACTER*9  RUN_STRING
      LOGICAL OK
C
      DATA MAX_FDCRT /11/
      DATA GN_INI,PD_INI,TM_INI /3*.FALSE./
C----------------------------------------------------------------------
C
C  Initialize the Zebra structure & initialize /ZEBSTP/ store
C
      CALL MZEBRA ( 0 )
      CALL INZSTP
      CALL FSPLNK               ! Create permanent link area for FDC STP
C
C  Initialize RCP file.
C
      CALL INRCP ('FTRAKS_RCP',IER)  ! Read parameter file into an SRCP bank
      IF (IER .NE. 0) THEN
        CALL INTMSG(' FDC_OFFCAL: can not open RCP file FTRAKS_RCP')
        GO TO 999
      END IF
C
C  Read in FDC STP file.
C
      CALL EZPICK('FTRAKS_RCP')
      CALL EZGETS('FDC_STPFILE',1,FILNAM,ILEN,IER)
      CALL EZRSET
C
      CALL INTMSG(' Reading FDC Static Parameter File '//FILNAM)
      CALL FDISTP(FILNAM, IER)
      IF(IER.NE.0) THEN
        CALL INTMSG(' FDC_OFFCAL: can not open file '//FILNAM)
        GOTO 999
      ENDIF
C
C  Make inquiries:
C
      CALL EZPICK('FTRAKS_RCP')
      CALL EZGET('START_RUN',START_RUN,IER)
      CALL EZGET('END_RUN',END_RUN,IER)
      CALL EZGET('DBL3_RUN',DBL3_RUN,IER)
      CALL EZGETS('CALTYPE',1,CALTYPE,ILEN,IER)
C
      CALL EZSET('GN_PROC_INI',.FALSE.,IER)
      CALL EZSET('PD_PROC_INI',.FALSE.,IER)
      CALL EZSET('TM_PROC_INI',.FALSE.,IER)
      CALL EZRSET
C
      IF ( CALTYPE.EQ.'PEDESTAL') THEN
        LFHEAD=LFPDH
      ELSEIF ( CALTYPE.EQ.'TIMES' ) THEN
        LFHEAD=LFTMH
      ELSEIF ( CALTYPE .EQ. 'GAINS' ) THEN
        LFHEAD=LFGNH
      ELSE
        CALL INTMSG(' Invalid CALIB type name:'//CALTYPE)
        GOTO 999
      ENDIF
      IC(LFHEAD+1) = START_RUN   ! Start Validity
      IC(LFHEAD+2) = END_RUN     ! End Validity
C
C  Read in values from Online DBL3 database.
C
      WRITE(RUN_STRING,100) DBL3_RUN
  100 FORMAT(I9)
      CALL INTMSG(' Reading FDC online DBL3 database, using run'
     &          //RUN_STRING)
      CALL DBCLB_INITIALIZE_FORCE_RUN(DBL3_RUN)
      CALL FDBINI(DBL3_RUN,OK)
      IF (.NOT.OK) THEN
        CALL INTMSG(' FDC_OFFCAL:cannot open FDC DBL3 online database')
        GO TO 999
      END IF
C
C  Insert new Offline constants (from RCP) into STP banks
C
      CALL FDC_FILL_STP
C
C  Write out FZ file
C
      CALL INTMSG(' Output Offline DBL3 Banks:'//CALTYPE)
      CALL DBCLB_INITIALIZE_FORCE_RUN(0)
      CALL FSTP_INSERT(CALTYPE,IER)
      IF ( IER .NE. 0 ) THEN
        CALL INTMSG(' FDC_OFFCAL: error on writing DBL3 output')
        GO TO 999
      ENDIF
C
  999 CONTINUE
      END
