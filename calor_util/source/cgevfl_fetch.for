      SUBROUTINE CGEVFL_FETCH(RCP_BANK,ICRATE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FETCH PED and/or GNS one crate at a time
C-                         from DBL3.
C-
C-   Inputs  :  RCP_FILE [C] RCP bank to fetch DO_GNSCOR,DO_PEDSUB
C-              ICRATE   [I] crate number
C-   Outputs :  IER      [I]
C-   Controls:
C-
C-   Created  17-JUN-1992   Chip Stewart
C-   Updated  26-APR-1993   Jan Guida  Add Bypass DBL3 error switch
C-   Updated  23-JUN-1993   Jan Guida  Fill LORUN and HIRUN variables
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCP_BANK
      INTEGER ICRATE,IER,IER1
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'     ! CAD bank params
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'    ! CAL UNPACKING params
      INCLUDE 'D0$INC:ZEBSTP.INC'
C CHARACTER STRINGS
      CHARACTER*80 GAIN_FILE,PED_FILE,FILENAME,MSG
      CHARACTER DBCALIB*132,SRCPNAME*12
C INTEGERS
      INTEGER I,J,K,N
      INTEGER LENF,NRUN,LRUN,GRUN,PRUN,CADVSN,TASK,RUNNO,LORUN,HIRUN
      INTEGER GZCPDH,GZCGNH,LZFIND ! PED/GAIN bank addresses
      INTEGER TRULEN,INDEX,IBSET  ! VAX functions
      INTEGER ICRT,ICABLE,IETA,IPHI,ILYR,ICR
      INTEGER LORUN_PED(12),HIRUN_PED(12),LORUN_GNS(12),HIRUN_GNS(12)
      INTEGER KRATE(2),KRUN(2),KKRUN(2)
C LOGICALS
      LOGICAL FIRST,DO_PED,DO_GNSCOR,OK,EZERR,DBINIT
      LOGICAL BYPASS_DBL3_ERROR,LPED,LGNS,VFIRST
C REAL
      REAL VSN
      SAVE FIRST,DBINIT,DO_PED,DO_GNSCOR
      DATA FIRST /.TRUE./,DBINIT/.TRUE./,VFIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        IF (LSCAL.EQ.0) CALL BKSCAL('STPC',LSCAL)
        CALL EZPICK(RCP_BANK)
        IF( .NOT. EZERR(IER) ) THEN
          CALL EZGET('DO_GNSCOR',DO_GNSCOR,IER)
          IF(DO_GNSCOR)THEN
            CALL EZ_GET_CHARS('GAINS_FILE',N,FILENAME,IER)
            IF (IER.NE.0) THEN
              FILENAME = 'DBL3'
              CALL ERRMSG('NO GAIN FILE in RCP ','CAEPFL_PEDGNS',
     &          ' USE NOMINAL DBL3 ','W')
            END IF
            CALL UPCASE(FILENAME,GAIN_FILE)
          END IF
          CALL EZGET('DO_PEDSUB',DO_PED,IER)
          IF(.NOT.DO_PED)CALL EZGET('PEDESTAL_SIGMAS',DO_PED,IER)
          IF(DO_PED)THEN
            CALL EZ_GET_CHARS('PEDESTAL_FILE',N,FILENAME,IER)
            IF (IER.NE.0) THEN
              FILENAME = 'DBL3'
              CALL ERRMSG('NO PED FILES in RCP ','CAEPFL_PEDGNS',
     &          ' USE NOMINAL DBL3 ','W')
            END IF
            CALL UPCASE(FILENAME,PED_FILE)
          END IF
          CALL EZGETS('DBCALIB$CAL',1,DBCALIB,LENF,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('NO DBCALIB$CAL IN CAHITS_RCP','CGEVFL_FETCH',
     &        'USE DBCALIB$CAL AS LOGICAL','W')
            DBCALIB = 'DBCALIB$CAL'
          END IF
        ELSE
          CALL ERRMSG('NO RCP BANK','CGEVFL_FETCH',
     &        ' USE NOMINAL VALUES ','W')
          DO_PED = .TRUE.
          DO_GNSCOR = .TRUE.
          GAIN_FILE= 'DBL3'
          PED_FILE= 'DBL3'
          DBCALIB = 'DBCALIB$CAL'
        END IF
        BYPASS_DBL3_ERROR = .FALSE.
        IF (PED_FILE.EQ.'DBL3' .OR. GAIN_FILE.EQ.'DBL3') THEN
          CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
          IF(IER.NE.0) CALL ERRMSG('NO_ERR_SWITCH','READ_PEDGNS',
     &      'USE DBL3 BYPASS ERROR SWITCH  = FALSE AS DEFAULT','W')
        END IF
        CALL EZRSET
C
C ****  ZERO OUT CALIB VALIDITY RANGE ARRAYS
C
        CALL VZERO(LORUN_PED,12)
        CALL VZERO(LORUN_GNS,12)
        CALL VZERO(HIRUN_PED,12)
        CALL VZERO(HIRUN_GNS,12)
C
C ****  Get PEDS and GAINS
C
      END IF
      IER = 0
C
C ****  SET TASK
C
      TASK = -1
      IF( DO_GNSCOR) THEN
        TASK = 2
        FILENAME = GAIN_FILE
        IF ( DO_PED ) THEN
          TASK =0
          IF(FILENAME(1:TRULEN(FILENAME))
     &        .NE.(PED_FILE(1:TRULEN(PED_FILE)))) THEN
            CALL ERRMSG('PEDESTAL_NE_GAIN_FILE','CGEVFL_FETCH',
     &        ' USE GAIN_FILE','W')
          END IF
        END IF
      ELSE IF ( DO_PED ) THEN
        TASK =1
        FILENAME = PED_FILE
      END IF
      IF ( TASK .EQ.-1 ) GOTO 999
C
C ****  READ IN PEDESTALS AND/OR GAINS
C
      NRUN = RUNNO()
      LRUN = INDEX(FILENAME,'@')
      IF(LRUN.GT.0) THEN
        CALL SWORDS(FILENAME,I,J,K)
        IF ( (J-LRUN).EQ.4) THEN
          READ(FILENAME(LRUN+1:J),'(I4)')PRUN
          PRUN = PRUN + 1000000
        ELSE IF ( (J-LRUN).EQ.7) THEN
          READ(FILENAME(LRUN+1:J),'(I7)')PRUN
        ELSE
          PRUN=NRUN
          CALL INTMSG(' No DBL3 gains run given, default taken')
        ENDIF
        NRUN = PRUN
      END IF
C
C ****  GET CRATE INDEX FOR VALIDITY RANGE TESTS
C
      ICABLE = 0
      IF(MOD(ICRATE,10).EQ.8) ICABLE = 1
      ICRT = ICRATE/10 + 1 + ICABLE*6     ! Get crate number 1-12
C
C ****  CHECK EXISTING CDPH,CGNH BANKS FOR VALIDITY RANGE
C
      IF(TASK.NE.2) THEN
        LORUN = LORUN_PED(ICRT) ! Start Validity
        HIRUN = HIRUN_PED(ICRT) ! End validity
C        LCPDH = GZCPDH ()
C        IF(LCPDH.GT.0) LCPDH = LZFIND(IXSTP,LCPDH,ICRATE,9)
C        IF(LCPDH.GT.0) THEN
C          LORUN = IC(LCPDH +4) ! Start Validity
C          HIRUN = IC(LCPDH +5) ! End validity
        IF((NRUN.GE.LORUN).AND.(NRUN.LE.HIRUN)) THEN
          WRITE(MSG,'('' CPDH CRATE '',I4,'' RUN '',I7.7)')ICRATE,NRUN
          CALL ERRMSG('CALIB BANKS ALREADY IN PLACE','CGEVFL_FETCH',
     &        MSG,'W')
          IF(TASK.EQ.1) THEN
            GOTO 999
          ELSE
            TASK = 2
          END IF
        ELSE
          LCPDH = GZCPDH ()
          IF(LCPDH.GT.0) LCPDH = LZFIND(IXSTP,LCPDH,ICRATE,9)
          IF(LCPDH.GT.0) THEN
            CALL MZDROP(IXSTP,LCPDH,' ')
          END IF
        END IF
C        END IF
      END IF
      IF(TASK.NE.1) THEN
        LORUN = LORUN_GNS(ICRT) ! Start Validity
        HIRUN = HIRUN_GNS(ICRT) ! End validity
C        LCGNH = GZCGNH ()
C        IF(LCGNH.GT.0) LCGNH = LZFIND(IXSTP,LCGNH,ICRATE,9)
C        IF(LCGNH.GT.0) THEN
C          LORUN = IC(LCGNH +4) ! Start Validity
C          HIRUN = IC(LCGNH +5) ! End validity
        IF((NRUN.GE.LORUN).AND.(NRUN.LE.HIRUN)) THEN
          WRITE(MSG,'('' CGNH CRATE '',I4,'' RUN '',I7.7)')ICRATE,NRUN
          CALL ERRMSG('CALIB BANKS ALREADY IN PLACE','CGEVFL_FETCH',
     &        MSG,'W')
          IF(TASK.EQ.2) THEN
            GOTO 999
          ELSE
            TASK = 1
          END IF
        ELSE
          LCGNH = GZCGNH ()
          IF(LCGNH.GT.0) LCGNH = LZFIND(IXSTP,LCGNH,ICRATE,9)
          IF(LCGNH.GT.0) THEN
            CALL MZDROP(IXSTP,LCGNH,' ')
          END IF
        END IF
C        END IF
      END IF
C
      IF(DBINIT)THEN
        DBINIT = .FALSE.
        WRITE(MSG,1001)NRUN
 1001   FORMAT(' CAL FETCH GAIN/PEDS FOR RUN =',I10)
        CALL INTMSG(MSG)
        CALL DBCLB_INITIALIZE(DBCALIB,'S',OK)
        IF (.NOT.OK) THEN
          IF (BYPASS_DBL3_ERROR) THEN
            CALL ERRDB(' Error in DBL3 intialization ')
          ELSE
            CALL ERRMSG('CALORIMETER','CGEVFL_FETCH',
     &        ' failed in initializating DBL3','F')
            GOTO 999
          ENDIF
        END IF
      END IF
C
      IF(TASK.NE.2 .AND. OK) THEN
        KRATE(1) = ICRATE
        CALL CPDINI(NRUN,1,KRATE,OK)
        IF (.NOT.OK) THEN
          CALL ERRMSG('CALORIMETER','CGEVFL_FETCH',
     &       ' failed in reading CALIB pedestals','W')
          IER = -1
        ELSE
          WRITE(MSG,'('' Reading new peds for crate '',I3)') ICRATE
          CALL ERRMSG('CALORIMETER','CGEVFL_FETCH',MSG,'I')
          IER = IBSET(IER,0)
          LORUN_PED(ICRT) = IC(LCPDH+4)
          HIRUN_PED(ICRT) = IC(LCPDH+5)
        ENDIF
      ENDIF
      IF(TASK.NE.1 .AND. OK) THEN
        KRATE(1) = ICRATE
        CALL CGNINI(NRUN,1,KRATE,OK)
        IF (.NOT.OK) THEN
          CALL ERRMSG('CALORIMETER','CGEVFL_FETCH',
     &       ' failed in reading CALIB gains','W')
          IER = -1
        ELSE
          WRITE(MSG,'('' Reading new gains for crate '',I3)') ICRATE
          CALL ERRMSG('CALORIMETER','CGEVFL_FETCH',MSG,'I')
          IER = IBSET(IER,1)
          LORUN_GNS(ICRT) = IC(LCGNH+4)
          HIRUN_GNS(ICRT) = IC(LCGNH+5)
        ENDIF
      ENDIF
      IF (OK) THEN
        CALL EZGET('CALIB_VERSION',VSN,IER1)  !FROM SRCP BANK LAST PICKED
        CALL EZGET('CAD_VERSION',CADVSN,IER1)
        C(LSCAL+9)=VSN
        IC(LSCAL+10)=CADVSN
        IF(TASK.EQ.1) THEN
          WRITE(SRCPNAME,'(''STPC/CALP'',I2.2)')ICRATE
          CALL EZRSET
          CALL EZDROP(SRCPNAME)
        ELSE
          WRITE(SRCPNAME,'(''STPC/CALG'',I2.2)')ICRATE
          CALL EZRSET
          CALL EZDROP(SRCPNAME)
        END IF
      ELSE
        IER = -1
      ENDIF
      CALL EZRSET
      KRATE(1) = ICRATE
      CALL CLBRUN(1,KRATE,KRUN,KKRUN)
      PRUN = KRUN(1) 
      GRUN = KKRUN(1) 
      IF(TASK .EQ. 0 .OR. TASK.EQ. 2) THEN
        WRITE(MSG,1002)ICRATE,' GAINS ',GRUN
        CALL INTMSG(MSG)
      ENDIF
      IF(TASK .EQ. 0 .OR. TASK.EQ. 1) THEN
        WRITE(MSG,1002)ICRATE,' PEDS  ',PRUN
        CALL INTMSG(MSG)
      ENDIF
 1002 FORMAT('   CAL CRATE ',   I5,      A8,' RUN =',I10)
C------------------------------------------------------------------------
 2999 RETURN
  999 CALL EZRSET
      RETURN
C#######################################################################
      ENTRY CGEVFL_DBFINISH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CLOSE DBL3
C-
C----------------------------------------------------------------------

      IF(.NOT.DBINIT) CALL DBCLB_FINISH
      DBINIT = .TRUE.
      RETURN
C**********************************************************************
C
      ENTRY CGEVFL_VALIDITY(IETA,IPHI,ILYR,LPED,LGNS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURNS LXXX = TRUE IF RANGE STILL VALID
C-
C----------------------------------------------------------------------
      IF (VFIRST.AND.FIRST) THEN
        CALL VZERO(LORUN_PED,12)
        CALL VZERO(LORUN_GNS,12)
        CALL VZERO(HIRUN_PED,12)
        CALL VZERO(HIRUN_GNS,12)
        VFIRST = .FALSE.
      ENDIF
      CALL CPHCRT(IETA,IPHI,ILYR,ICR)
      NRUN = RUNNO()
      ICABLE = 0
      IF(MOD(ICR,10).EQ.8) ICABLE = 1
      ICRT = ICR/10 + 1 + ICABLE*6     ! Get crate number 1-12
C
      LORUN = LORUN_GNS(ICRT) ! Start Validity
      HIRUN = HIRUN_GNS(ICRT) ! End validity
      LGNS = (NRUN.GE.LORUN).AND.(NRUN.LE.HIRUN)
      LORUN = LORUN_PED(ICRT) ! Start Validity
      HIRUN = HIRUN_PED(ICRT) ! End validity
      LPED = (NRUN.GE.LORUN).AND.(NRUN.LE.HIRUN)
 1999 RETURN
      END
