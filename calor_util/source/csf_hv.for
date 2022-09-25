      SUBROUTINE CSF_HV
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correct the CSFH and CSFW banks for the 
C-                         HV state of the calorimeter. This should be 
C-                         replaced by DB fetch at some point. 
C-                         ~ boost the A factor for data taken at 2KV
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: 
C-
C-   Created   5-NOV-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LCSFH,GZCSFH,HV_CSF,HV_TARGET,LOC,IER,NHV_COR
      INTEGER LCSFW,GZCSFW,ICSFW,I,J,K,IETA,IPHI,ILYR,NRUN,RUNNO,POWER
      INTEGER CAL_MODULE,IMOD
      LOGICAL MC,TB
      CHARACTER MSG*80,MODULE*4
      INTEGER FIRST_RUN/52470/
      INTEGER HV_OLD   /2500/
      INTEGER HV_NEW   /2000/
      REAL HV_COR(10) /1.015,1.023,1.016,1.000,1.019,! CCEM ECEM CCMG ICD ECMG
     &  1.016,2*1.019,1.016,1.019/      ! CCFH ECIH ECMH CCCH ECOH
C----------------------------------------------------------------------
C
C ****  Check data source
C
      MC = BTEST(D0VSN,5)         ! Check for Monte Carlo generated CAD bank 
      TB = BTEST(D0VSN,6)         ! Check for NWA data CAD1 bank 
C
C ****  CHECK HV correction in CSFH header banks (NV)
C
      LCSFH = GZCSFH ()
      HV_CSF = IC(LCSFH+1)
      IF(HV_CSF.EQ.1) THEN
        IF(MC.OR.TB) GOTO 999
        HV_CSF = 2500
      END IF
C
C ****  READ CSF_HV_RCP
C
      CALL EZLOC('CSF_HV_RCP',LOC)
      IF (LOC.EQ.0) THEN
        CALL INRCP('D0$CALOR_OFF:CSF_HV.RCP',IER)
      END IF
      IF(IER.NE.0) THEN
        CALL EZGET_ERROR_TEXT (IER,MSG)
        CALL ERRMSG('CAL','CSF_HV',MSG,'W')
        call ERRMSG('CAL','CSF_HV',
     &    'FAIL to read CSF_HV_RCP/Use Default','W')
        GOTO 999
      END IF
      CALL EZPICK('CSF_HV_RCP')
      CALL EZGET_i('FIRST_RUN_TO_CORRECT',FIRST_RUN,IER)
      IF(IER.NE.0) CALL ERRMSG('CAL','CSF_HV',
     &    'Fail to read FIRST_RUN - Use Default','W')
      CALL EZGET_i('HV_OLD',HV_OLD,IER)
      IF(IER.NE.0) CALL ERRMSG('CAL','CSF_HV',
     &    'Fail to read HV_OLD - use Default','W')
      CALL EZGET_i('HV_NEW',HV_NEW,IER)
      IF(IER.NE.0) CALL ERRMSG('CAL','CSF_HV',
     &    'Fail to read HV_NEW - use Default','W')
      CALL EZGET('HV_COR',HV_COR,IER)
      IF(IER.NE.0) CALL ERRMSG('CAL','CSF_HV',
     &    'Fail to read HV_COR - Use Default','W')
      CALL EZGET_SIZE('HV_COR',NHV_COR,IER)  !  NUMBER OF MODULES
      IF (NHV_COR.NE.10) THEN  
        CALL ERRMSG('CAL','CSF_HV','NEED 10 VALUES IN HV_COR','W')
        GOTO 999
      END IF
      call EZRSET
C
C ****  Check run number 
C
      NRUN = RUNNO()
      HV_TARGET = HV_OLD
      IF(NRUN.GE.FIRST_RUN) HV_TARGET = HV_NEW
      IF(HV_CSF.EQ.HV_TARGET) THEN
        GOTO 999
      ELSE IF(HV_CSF.EQ.HV_OLD.AND.HV_TARGET.EQ.HV_NEW) THEN
        POWER =  1
      ELSE IF(HV_CSF.EQ.HV_NEW.AND.HV_TARGET.EQ.HV_OLD) THEN
        POWER = -1
      ELSE
        WRITE (MSG,10)HV_CSF,HV_TARGET
   10   FORMAT('UNEXPECTED HV_CSF ',I5,' HV_TARGET ',I5)
        CALL ERRMSG('CAL','CSF_HV',MSG,'W')
        GOTO 999
      END IF
C
C ****  LOOP THROUGH CSFH
C
      WRITE (MSG,12)HV_CSF,HV_TARGET
   12 FORMAT('CORRECTING GAIN FROM HV=',I5,' TO ',I5)
      CALL ERRMSG('CAL','CSF_HV',MSG,'I')
      CALL ERRMSG('CAL','CSF_HV',MSG,'S')
      LCSFH = GZCSFH ()
      IC(LCSFH+1) = HV_TARGET
      DO IMOD = 1, 10
        C(LCSFH+1+IMOD) = C(LCSFH+1+IMOD) * HV_COR(IMOD)**POWER
      END DO
      LCSFW = GZCSFW ()
      DO ILYR = 1, NLYRL
        DO IETA = 1,NETAL
          IMOD = CAL_MODULE(IETA,ILYR,MODULE)
          IF(IMOD.eq.0) IMOD = CAL_MODULE(-IETA,ILYR,MODULE)
          IF(IMOD.GT.0) THEN
            ICSFW =ILYR+(IETA-1)*NLYRL  
            C(LCSFW+1+ICSFW) = C(LCSFW+1+ICSFW) * HV_COR(IMOD)**POWER
          END IF
        END DO
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
