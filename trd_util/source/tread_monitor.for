      SUBROUTINE TREAD_MONITOR(DB_OPENED,DO_URANIUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads the monitoring data into the TROP
C-                         bank.
C-   Inputs  :       DO_URANIUM - whether or not to access CALIB DB
C-   Input/Output :  DB_OPENED  - .TRUE. if CALIB DB is open
C-   Controls:
C-
C-   Created  16-JUN-1992   Jean-Francois Glicenstein
C-   Updated   2-JUL-1992   JFG added more canary results, changed the
C-                          number of high voltage devices to 32.
C-   Updated  10-JUL-1992   Mark A. Goforth  to correctly pick dbmon database
C-   Updated   8-OCT-1992   Lars Rasmussen to handle no devices found correctly
C-                          and try to prevent some possible pit fals.
C-                          (added lines are marked with %%%)
C-   Updated  27-OCT-1992   Lars Rasmussen, To read in the right DBMON dbl3
C-                          file (added DBMU_GTFILE, deletet EZPICK).
C-   Updated  30-DEC-1992   A. Zylberstejn
C-   Updated  15-APR-1993   LR, implemented by JFG Correct the bug
C-                          with IRET=0
C-   Updated  19-JUN-1993   JFG  Uses DBMU_GTDM to avoid reopening the
C-                          monitoring database.
C-   Updated  18-NOV-1993   J.Fr. Glicenstein  reads new gas devices for run 1b
C-   Updated  21-MAR-1994   A. Zylberstejn  :do not refer to data base if CDD4
C-                                           absent
C-   Updated  28-JAN-1995   Lewis Taylor Goss  read Uranium info. from CALIB DB
C-   Updated  17-JUL-1995   Lewis Taylor Goss  add DO_URANIUM flag
C-   Updated  11-DEC-1995   A. Zylberstejn:test on cdd4 and cad1 for raw data
C-   Updated  18-DEC-1995   A. ZYLBERSTEJN : take pressure and temperature from
C-                                           FDC data base
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:D3U.INC'
      INCLUDE 'D0$INC:DBMUKEY.INC'
      INCLUDE 'D0$LINKS:IZTROP.LINK'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INTEGER IRET,ENDVL_C,ENDVL_G
      INTEGER ENDVL_H,I,LTROP,GZCAD1
      REAL DELTATC,DELTATG,DELTATH
      INTEGER LOUT,TRUNIT
      LOGICAL FIRST,BYPASS_DBMON_ERROR,BYPASS_DBL3_ERROR,D3UUT,DUMMY
      LOGICAL DOPRINT,TRD_DO_PRINT,OK
      LOGICAL RUN1A,DB_OPENED
      EXTERNAL RUN1A
      CHARACTER*11 CLASS(4)
      DATA CLASS/'CANARY','DBM_TRD_GAS','DBM_CD','TRD_URAN'/
      INTEGER NDEV_CANARY,NDEV_GAS,IER,DBTIME
      PARAMETER (NDEV_CANARY = 6)
      CHARACTER*8 CANARY_DEV(NDEV_CANARY)
      PARAMETER (NDEV_GAS = 15)
      DATA CANARY_DEV/'TRD.GAIN','TRD.DGAI','TRD.ATTC','TRD.DATT',
     &  'TRD.ENRJ','TRD.SNRJ'/
      CHARACTER*17 GAS_DEV_RUN1A(NDEV_GAS)
      INTEGER *4 YE,MO,DA,HO,MI,SE,IDAT,ITIM
      INTEGER *2 NTIM(7)
      CHARACTER*17 FDC_DEV(5)
      REAL VAL_FDC(5)
      DATA FIRST/.TRUE./
      DATA GAS_DEV_RUN1A/'TRD_PT722.PRES','TRD_PT101.PRES',
     &  'TRD_TT615.TEMP','TRD_TT390.TEMP','TRD_OXT505.CONC',
     &  'TRD_MT503.CONC','TRD_PT209.PRES','TRD_PT266.PRES',
     &  'TRD_PT269.PRES','TRD_PT309.PRES','TRD_PT366.PRES',
     &  'TRD_PT369.PRES','TRD_PT409.PRES','TRD_PT466.PRES',
     &  'TRD_PT469.PRES'/
      CHARACTER*17 GAS_DEV(NDEV_GAS)
      DATA GAS_DEV/'TRD_GAS.PPAT','TRD_GAS.PRAT','TRD_GAS.TRM',
     &  'TRD_GAS.TPLT','TRD_GAS.AO2','TRD_GAS.PCAN',
     &  'TRD_GAS.PDT1','TRD_GAS.PRD1','TRD_GAS.PGP1',
     &  'TRD_GAS.PDT2','TRD_GAS.PRD2','TRD_GAS.PGP2',
     &  'TRD_GAS.PDT3','TRD_GAS.PRD3','TRD_GAS.PGP3'/
      INTEGER NDEV_URAN
      PARAMETER (NDEV_URAN = 14)
      CHARACTER*17 URAN_DEV(NDEV_URAN)
      DATA URAN_DEV/   'TRD_URA.RUN1','TRD_URA.RUN2','TRD_URA.TIME',
     &  'TRD_URA.PRES','TRD_URA.TEMP','TRD_URA.URL1','TRD_URA.URL2',
     &  'TRD_URA.URL3','TRD_URA.HVL1','TRD_URA.HVL2','TRD_URA.HVL3',
     &  'TRD_URA.PWL1','TRD_URA.PWL2','TRD_URA.PWL3' /
      INTEGER NDEV_HV,IDX,DATF,TIMF,RUN_NO1,RUN_NO2,RUNNO
      INTEGER TIME_OF_RUN(2),NTRIES,TIME_OF_RUNU(2)
      INTEGER TIME_OF_RUNC(2),TIME_OF_RUNH(2),TIME_OF_RUNG(2)
      CHARACTER*1 X1
      CHARACTER*2 X2
      INTEGER NMAX,ADD_CAN,ADD_HV,ADD_GAS
      LOGICAL SYS$NUMTIM,DO_URANIUM
      EXTERNAL SYS$NUMTIM
      PARAMETER (NDEV_HV = 48)
      PARAMETER (IDX=1)
      DATA NMAX/10/,ADD_CAN/3/,ADD_HV/3/,ADD_GAS/3/
      DATA FDC_DEV/'FDC_DET_TEMP.T1N','FDC_DET_TEMP.T2N',
     &  'FDC_DET_TEMP.T1S','FDC_DET_TEMP.T2S','FDC_DET_PRES.ATMO'/
C   6/12/93 added the Canary HV devices to all HV devices
      CHARACTER*17 HV_DEV(NDEV_HV+3)
      REAL VAL_GAS(NDEV_GAS),VAL_CANARY(NDEV_CANARY),VAL_HV(NDEV_HV+3)
      REAL VAL_URAN1(NDEV_URAN),VAL_URAN2(NDEV_URAN)
C-----------------------------------------------------------------------------
      IF (FIRST) THEN
        DO I = 1,MIN0(9,NDEV_HV)
          WRITE(X1,1001) I
          HV_DEV(I) = 'TRD_HV_00'//X1//'.VOLT'
        ENDDO
        DO I = 10,MAX0(9,NDEV_HV)
          WRITE(X2,1002) I
          HV_DEV(I) = 'TRD_HV_0'//X2//'.VOLT'
        ENDDO
        HV_DEV(NDEV_HV+1) = 'TRD_CNGRIDHV.VOLT'
        HV_DEV(NDEV_HV+2) = 'TRD_CNPOTHV.VOLT'
        HV_DEV(NDEV_HV+3) = 'TRD_CNWINDHV.VOLT'
C      If run1A, then gets the old gas devices
        IF (RUN1A()) THEN
          DO I = 1,NDEV_GAS
            GAS_DEV(I) = GAS_DEV_RUN1A(I)
          ENDDO
          CLASS(2) = 'DBM_CD'
        ENDIF
        CALL EZPICK('TRD_RCP')
        CALL EZGET('BYPASS_DBMON_ERROR',BYPASS_DBMON_ERROR,IER)
        IF(IER.NE.0)BYPASS_DBMON_ERROR=.FALSE.
        CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
        IF(IER.NE.0)BYPASS_DBL3_ERROR=.FALSE.
        BYPASS_DBMON_ERROR=BYPASS_DBMON_ERROR .OR. BYPASS_DBL3_ERROR
        CALL EZRSET
        FIRST = .FALSE.
        LOUT=TRUNIT()
        DOPRINT=TRD_DO_PRINT()
C Check the presence of raw data
        IF(LQ(LHEAD-IZCDD4).LE.0 .AND. GZCAD1().LE.0)THEN
          IF(DOPRINT) WRITE(LOUT,*)
     &      ' gas and H.V. constants taken from THIT'
          RETURN
        END IF
      ENDIF
C      IF(LQ(LHEAD-IZCDD4).LE.0)RETURN
C
C ****  get the run time
C
      IDAT=0
      ITIM=0
      IF ( SYS$NUMTIM( NTIM,TIME_OF_RUN ) ) THEN
        YE = ((NTIM(1) - (NTIM(1)/100)*100))
        MO = NTIM(2)
        DA = NTIM(3)
        HO = NTIM(4)
        MI = NTIM(5)
        SE = NTIM(6)
        IDAT = YE*10000 + MO*100 + DA
        ITIM = HO*10000 + MI*100 + SE
        IF(DOPRINT)WRITE(LOUT,*)' IDAT,ITIM',IDAT,ITIM
      END IF
      TIME_OF_RUN(1) = IQ(LHEAD+4)
      TIME_OF_RUN(2) = IQ(LHEAD+5)
      RUN_NO1        = RUNNO()
C
C **** First get canary data
C
      NTRIES = 0
      CALL UCOPY(TIME_OF_RUN,TIME_OF_RUNC,2)
  100 CONTINUE
      NTRIES = NTRIES + 1
      IF(NTRIES.GE.NMAX) THEN
        IF(BYPASS_DBMON_ERROR)THEN
          CALL INTMSG(' TREAD_MONITOR: incorrect values in dbmon ')
        ELSE
          LTROP=LC(LTGEN-IZTROP)
          IF (LTROP.LE.0) THEN
            CALL ERRMSG('TTRAKS','TREAD_MONITOR',' No TROP bank','F')
          ENDIF
          CALL INTMSG(' TREAD_MONITOR: incorrect values in dbmon ')
          C(LTROP+100) = 1.
        END IF
        GO TO 999
      ENDIF
      CALL DBMU_GETDM(CLASS(1),CANARY_DEV(1),NDEV_CANARY,TIME_OF_RUNC,
     &  1,' ',VAL_CANARY,DELTATC,IRET)
C
C ****  check if read error (no need to be after the "begin run")
C
      ENDVL_C = DBKEYS(4)
      IF (IRET.LE.0) THEN
        CALL DBINCT (ENDVL_C, ADD_CAN,ENDVL_C)
        DUMMY = D3UUT(TIME_OF_RUNC,ENDVL_C)
        GOTO 100
      ENDIF
C
C **** get the associated gas data
C
      NTRIES = 0              ! %%% Prevent infinite loop
      CALL UCOPY(TIME_OF_RUN,TIME_OF_RUNG,2)
  200 CONTINUE
      NTRIES = NTRIES + 1
      IF(NTRIES.GE.NMAX) THEN
        IF(BYPASS_DBMON_ERROR)THEN
          CALL INTMSG(' TREAD_MONITOR: incorrect values in dbmon ')
        ELSE
          LTROP=LC(LTGEN-IZTROP)
          IF (LTROP.LE.0) THEN
            CALL ERRMSG('TTRAKS','TREAD_MONITOR',' No TROP bank','F')
          ENDIF
          CALL INTMSG(' TREAD_MONITOR: incorrect values in dbmon ')
          C(LTROP+100) = 2.
        END IF
        GO TO 999
      ENDIF
      CALL DBMU_GETDM(CLASS(2),GAS_DEV(1),NDEV_GAS,
     &    TIME_OF_RUNG(1),1,' ',VAL_GAS,DELTATG,IRET)
C
C ****  check if read error (no need to check if after "begin run").
C
      ENDVL_G = DBKEYS(4)
      IF (IRET.LE.0) THEN
        CALL DBINCT (ENDVL_G, ADD_GAS,ENDVL_G)
        DUMMY = D3UUT(TIME_OF_RUNG,ENDVL_G)
        GOTO 200
      ENDIF
C
C **** get the associated HV data
C
      NTRIES = 0            ! %%% Prevent infinite loop
      CALL UCOPY(TIME_OF_RUN,TIME_OF_RUNH,2)
  300 CONTINUE
      NTRIES = NTRIES + 1
      IF(NTRIES.GE.NMAX) THEN
        IF(BYPASS_DBMON_ERROR)THEN
          CALL INTMSG(' TREAD_MONITOR: incorrect values in dbmon ')
        ELSE
          LTROP=LC(LTGEN-IZTROP)
          IF (LTROP.LE.0) THEN
            CALL ERRMSG('TTRAKS','TREAD_MONITOR',' No TROP bank','F')
          ENDIF
          CALL INTMSG(' TREAD_MONITOR: incorrect values in dbmon ')
          C(LTROP+100) = 3.
        END IF
        GOTO 999
      ENDIF
      CALL DBMU_GETDM(CLASS(3),HV_DEV(1),NDEV_HV+3,
     &    TIME_OF_RUNH(1),1,' ',VAL_HV,DELTATH,IRET)
C
C ****  check if read time is before begin run or read error
C     (this time, it is mandatory to check that it is after begin run)
C
      ENDVL_H = DBKEYS(4)
      IF (DELTATH.LE.0.OR.IRET.LE.0) THEN
        CALL DBINCT (ENDVL_H, ADD_HV,ENDVL_H)
        DUMMY = D3UUT(TIME_OF_RUNH,ENDVL_H)
        GOTO 300
      ENDIF
C
C **** get the end validity
C
      CALL DBUPTS (DATF,TIMF,ENDVL_H)
      IF(        IDAT .GT.951001)THEN ! take pressure and temperature from FDC

        NTRIES = 0            ! %%% Prevent infinite loop
        CALL UCOPY(TIME_OF_RUN,TIME_OF_RUNH,2)
  400   CONTINUE
        NTRIES = NTRIES + 1
        CALL DBMU_GETDM(CLASS(3),FDC_DEV,5,TIME_OF_RUNH,1,
     +         ' ',VAL_FDC, DELTATH,IRET)
C
C ****  check if read time is before begin run or read error
C     (this time, it is mandatory to check that it is after begin run)
C
        IF (DELTATH.LE.0.OR.IRET.LE.0) THEN! Something went wrong
          ENDVL_H = DBKEYS(4)
          IF(NTRIES.LT.NMAX) THEN
            CALL DBINCT (ENDVL_H, ADD_GAS,ENDVL_H)
            DUMMY = D3UUT(TIME_OF_RUNH,ENDVL_H)
            GO TO 400
          ELSE
            IF(BYPASS_DBMON_ERROR)THEN
                CALL ERRMSG('DBMON error','TREAD_MONITOR',' FDC ','W')
                go to 410
            ELSE
              LTROP=LC(LTGEN-IZTROP)
              IF (LTROP.LE.0) THEN
                CALL ERRMSG('DBMON error','TREAD_MONITOR',
     &            ' No TROP bank','F')
              ENDIF
              CALL INTMSG(' TREAD_MONITOR: incorrect values in dbmon ')
              C(LTROP+100) = 3.
            END IF
            GOTO 999
          ENDIF
        ENDIF
        IF(DOPRINT)WRITE(LOUT,*)' press,temp FDC',
     &    VAL_FDC(5),VAL_FDC(4)
        VAL_GAS(1)=VAL_FDC(5)
        VAL_GAS(4)=VAL_FDC(4)
C        pression=val_fdc(5)+val_gas(7)
C        temperature=val_fdc(4)
      END IF
C
C **** get the end validity
C
  410    CALL DBUPTS (DATF,TIMF,ENDVL_H)
C
C
C **** store the relevant data into the TROP bank.
C
      LTROP=LC(LTGEN-IZTROP)
      IF (LTROP.GT.0) THEN
        IC(LTROP+1) = DATF
        IC(LTROP+2) = TIMF
        C(LTROP+3) = INT(VAL_CANARY(1)) ! gain with 50% half max (ADC counts/10)
        C(LTROP+4) = INT(VAL_CANARY(3)) ! slope (%)
        C(LTROP+5) = INT(VAL_CANARY(5)) ! gain with 80% half max (ADC counts)
        DO I = 1,NDEV_HV
          C(LTROP+5+I) = INT(VAL_HV(I)*1000.)*0.001
        ENDDO
        DO I = 1,NDEV_GAS
          C(LTROP+5+NDEV_HV+I) = INT(VAL_GAS(I)*10.)*0.1
        ENDDO
C      6/12/93 to keep compatibility with previous LTROP structure (JFG)
        C(LTROP+NDEV_HV+NDEV_GAS+ 6) = VAL_HV(NDEV_HV+1)
        C(LTROP+NDEV_HV+NDEV_GAS+ 7) = VAL_HV(NDEV_HV+2)
        C(LTROP+NDEV_HV+NDEV_GAS+ 8) = VAL_HV(NDEV_HV+3)
C
        IF(DOPRINT)
     +      WRITE(LOUT,*)' in tread_monitor gain',C(LTROP+5),
     +      'C(LTROP+8+NDEV_HV+NDEV_GAS)', C(LTROP+8+NDEV_HV+NDEV_GAS)
        C(LTROP+100) = 0.
      ELSE
        IF(BYPASS_DBMON_ERROR)THEN
          CALL INTMSG(' TREAD_MONITOR: no TROP bank  dbmon ')
          GO TO 999
        ELSE
          CALL ERRMSG('TTRAKS','TREAD_MONITOR',' No TROP bank','F')
          GO TO 999
        END IF
      ENDIF
C
C **** get the TRD Uranium Noise Run info
C
      IF (DO_URANIUM) THEN
        NTRIES = 0
        CALL UCOPY(TIME_OF_RUN,TIME_OF_RUNU,2)
  420   CONTINUE
        NTRIES = NTRIES + 1
        IF (NTRIES.GE.NMAX) THEN
          IF (BYPASS_DBL3_ERROR) THEN
            CALL INTMSG(' TREAD_MONITOR: incorrect values in calib ')
          ELSE
            LTROP = LC(LTGEN-IZTROP)
            IF (LTROP.LE.0) THEN
              CALL ERRMSG('TTRAKS','TREAD_MONITOR',' No TROP bank',
     &            'F')
            ENDIF
            CALL INTMSG(' TREAD_MONITOR: incorrect values in calib ')
            C(LTROP+100) = 4.
          ENDIF
          GO TO 999
        ENDIF
C get the first uranium run
        CALL TRD_URAN_FETCH(RUN_NO1,TIME_OF_RUNU,CLASS(4),NDEV_URAN,
     &      VAL_URAN1,DB_OPENED,OK)
C get the second uranium run, if one exists.
        IF (VAL_URAN1(2).NE.999999.9) THEN
          RUN_NO2 = VAL_URAN1(2) + 1
          CALL D3UPT(TIME_OF_RUNU,DBTIME)
          CALL DBINCT(DBTIME,1,DBTIME)
          DUMMY = D3UUT(TIME_OF_RUNU,DBTIME)
          CALL TRD_URAN_FETCH(RUN_NO2,TIME_OF_RUNU,CLASS(4),NDEV_URAN,
     &        VAL_URAN2,DB_OPENED,OK)
        ELSE
          DO I = 1,NDEV_URAN
            VAL_URAN2(I) = VAL_URAN1(I)
          ENDDO
        ENDIF
C
C ****  check if read error (no need to check if after "begin run").
C
        IF (.NOT.OK) THEN
          RUN_NO1 = RUN_NO1 + 1
          GOTO 420
        ENDIF
        C(LTROP+NDEV_HV+NDEV_GAS+ 9) = VAL_URAN1(1)
        C(LTROP+NDEV_HV+NDEV_GAS+10) = VAL_URAN1(3)
        DO I = 1,NDEV_URAN-3
          C(LTROP+NDEV_HV+NDEV_GAS+10+I) = VAL_URAN1(I+3)
        ENDDO
        C(LTROP+NDEV_HV+NDEV_GAS+22) = VAL_URAN2(1)
        C(LTROP+NDEV_HV+NDEV_GAS+23) = VAL_URAN2(3)
        DO I = 1,NDEV_URAN-3
          C(LTROP+NDEV_HV+NDEV_GAS+23+I) = VAL_URAN2(I+3)
        ENDDO
      ENDIF
C
  999 RETURN
 1001 FORMAT(I1)
 1002 FORMAT(I2)
      END
